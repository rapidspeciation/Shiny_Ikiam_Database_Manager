import { computed, ref, watch } from 'vue'
import { useSheet } from './useSheet'
import { api, requestId } from '../lib/api'
import { isBlank } from '../lib/cells'
import { isoToSerial, todayIso } from '../lib/dates'
import { noteDay } from '../lib/clutches'
import { ADULT } from '../lib/emerged'
import { initialsOf } from '../lib/rows'
import { errorText, notify } from '../lib/notice'
import { listColumn } from '../lib/options'
import { listProblem, verificationsFor } from '../lib/verifications'
import {
  COLUMNS,
  FATES,
  HEADERS,
  INSECTARY_ID,
  TUBE_ID,
  applies,
  insectarySex,
  isEmptyDraft,
  misfit,
  parseWeight,
  signNote,
  rankByRecency,
  type Column,
  type Draft,
  type Fate,
} from '../lib/collect'
import { parseBlock, parseCamTube, parseFate, parseSex, parseTime, stepId } from '../lib/paste'
import { persistentRef } from '../lib/persist'
import type { CellValue } from '../lib/types'
import { usePending } from '../stores/pending'
import { useSession } from '../stores/session'
import { type ServerRecord, useTables } from '../stores/tables'
import { t, tn } from '../lib/i18n'

/**
 * A day of field collection, shared by the Colecta tab's cards and its table
 * (CollectView): the outing's header (date, people, weather, place, medium),
 * the list of butterflies being typed (kept in this browser until saved), the
 * next free Insectary IDs, CAMs and tubes, the checks, and the one save that
 * writes Collection_data (and Insectary_data for the live ones). Butterflies
 * taken alive get the next Insectary ID (to write on the wings) and no CAM;
 * those preserved in the field a CAM and a tube.
 */
export const MODULE = 'Collection_data'

export function useCollect() {
  const module = ref(MODULE)
  const pending = usePending()
  const tables = useTables()
  const sheet = useSheet(module)
  const { table, lists, options, createFormulas } = sheet
  tables.load('Insectary_data').catch(() => {})

  const header = persistentRef('collect:header', {
    date: todayIso(),
    collector: '',
    identifier: '',
    location: '',
    rainfall: '',
    cloud: '',
    medium: 'Flash frozen',
    /** Who went out that day (the cards offer them as each butterfly's Collector). */
    team: [] as string[],
  })
  header.value.team ??= []
  // A day of field entries must survive a closed tab: kept in this browser, per person, until saved or emptied.
  // It is usually dry on collecting days: Rainfall starts as DY_(dry) (it can still be changed, per row too).
  if (!header.value.rainfall) header.value.rainfall = 'DY_(dry)'
  const drafts = persistentRef<Draft[]>(`collect:drafts:${useSession().user?.username}`, [], { lasting: true })
  // Lists kept from before Collector and Identifier were per row take the header's people.
  for (const d of drafts.value) {
    d.collector ??= header.value.collector
    d.identifier ??= header.value.identifier
    d.rainfall ??= header.value.rainfall
    d.cloud ??= header.value.cloud
  }
  /** The fate the next butterfly added gets (the last one chosen: a day is mostly one kind). */
  const addFate = persistentRef<Fate>('collect:addFate', 'insectario')

  type DayField = 'location' | 'identifier' | 'rainfall' | 'cloud'
  /**
   * Sets one of the day's values (cards): the butterflies that had the day's
   * value (or none) follow it; one given its own value keeps it.
   */
  function setDay(field: DayField, value: string) {
    const old = header.value[field]
    header.value[field] = value
    for (const d of drafts.value) if (!d[field] || d[field] === old) d[field] = value
  }
  /**
   * Who went out that day: the first is the default Collector; a butterfly whose
   * Collector is no longer one of them gets the first (with one person, all do).
   */
  function setTeam(team: string[]) {
    header.value.team = team
    header.value.collector = team[0] ?? ''
    if (!team.length) return
    for (const d of drafts.value) if (!team.includes(d.collector)) d.collector = team[0]
  }

  const observed = computed(() => table.value?.rows.filter(r => r.observed) || [])
  const latest = (field: string) => {
    const row = [...observed.value].reverse().find(r => !isBlank(r.values[field]))
    return row ? String(row.values[field]) : ''
  }
  watch(
    observed,
    rows => {
      if (!rows.length) return
      header.value.collector ||= latest('Collector')
      header.value.identifier ||= latest('Identifier')
    },
    { immediate: true },
  )

  /** A column's values, most used first. */
  const ranked = (field: string) => {
    const counts = new Map<string, number>()
    for (const row of observed.value)
      if (!isBlank(row.values[field])) counts.set(String(row.values[field]), (counts.get(String(row.values[field])) || 0) + 1)
    return [...counts].sort((a, b) => b[1] - a[1]).map(([v]) => v)
  }
  /** Species in Collection_data are "Genus species"; the most used come first. */
  const speciesList = computed(() => ranked('SPECIES'))
  /** The species of the latest collections (field trips, not monitoring walks), most recent first. */
  const recentSpecies = computed(() =>
    rankByRecency(observed.value.map(r => r.values.SPECIES), { window: 400 }).filter(s => !/^(NA|NOT_FOUND)$/i.test(s)),
  )
  /**
   * Subspecies or forms for each species: those used with it in Collection_data
   * (most used first), then those in Insectary_data's SPECIES ("Ithomia salapia
   * derasa") and in the Lists' Insectary_species. Taxonomy_v18Jun25 has a
   * subspecies column, but it is NA for every species, and the sheet does not
   * validate Subspecies_Form, so a new form is not marked.
   */
  const subspecies = computed(() => {
    void tables.versions.Insectary_data
    const counts = new Map<string, Map<string, number>>()
    const add = (species: string, sub: string, n = 1) => {
      // Not a form: NA, a doubt ("?", "deceptus?") or punctuation.
      if (!species || !sub || /^(NA|N\/A)$/i.test(sub) || /\?|^\W*$/.test(sub)) return
      const forms = counts.get(species) || new Map<string, number>()
      forms.set(sub, (forms.get(sub) || 0) + n)
      counts.set(species, forms)
    }
    for (const r of observed.value) add(String(r.values.SPECIES ?? '').trim(), String(r.values.Subspecies_Form ?? '').trim(), 1000)
    // "Genus species subspecies" (not hybrids: "… x …", "… VS …").
    const split = (name: string) => {
      const words = name.trim().split(/\s+/)
      if (words.length > 2 && !/ x |\bVS\b/i.test(name)) add(words.slice(0, 2).join(' '), words.slice(2).join(' '))
    }
    for (const r of tables.tables.Insectary_data?.rows || [])
      if (r.observed && !isBlank(r.values.SPECIES)) split(String(r.values.SPECIES))
    for (const name of listColumn(lists.value, 'Insectary_species')) split(name)
    return new Map([...counts].map(([species, forms]) => [species, [...forms].sort((a, b) => b[1] - a[1]).map(([f]) => f)]))
  })
  const subspeciesFor = (species: string) => subspecies.value.get(species.trim()) || []
  /** Every species the sheet accepts (Taxonomy), the ones used here first. */
  const allSpecies = computed(() => [...new Set([...speciesList.value, ...(options.value.SPECIES || [])])])
  const places = computed(() => [...new Set([...ranked('Collection_location'), ...(options.value.Collection_location || [])])])
  /** Places of the latest collections, most recent first (a trip goes back to the same places). */
  const recentPlaces = computed(() =>
    rankByRecency(observed.value.map(r => r.values.Collection_location), { window: 600 }).filter(p => !/^NA$/i.test(p)),
  )
  const people = computed(() => options.value.Collector || ranked('Collector'))
  const recentPeople = computed(() =>
    rankByRecency(
      observed.value.flatMap(r => [r.values.Collector, r.values.Identifier]),
      { window: 600 },
    ).filter(p => !/^NA\b/i.test(p) && (!options.value.Collector?.length || options.value.Collector.includes(p))),
  )
  const rainfalls = computed(() => options.value.Rainfall || ranked('Rainfall'))
  const clouds = computed(() => options.value.Cloud_cover || ranked('Cloud_cover'))
  const mediums = computed(() => options.value.Preservation_medium || ['Flash frozen'])
  /** The media of field-preserved butterflies, most used first (Flash frozen; Ethanol for pheromone males). */
  const fieldMediums = computed(() => {
    const used = rankByRecency(
      observed.value.filter(r => r.values.Release_Collect === 'Collected_Preserved').map(r => r.values.Preservation_medium),
      { window: 2000 },
    ).filter(m => mediums.value.includes(m))
    return [...new Set(['Flash frozen', ...used])].slice(0, 3)
  })
  const purposes = computed(() => options.value.Purpose || [])
  const session = useSession()
  /** Who signs the notes added here ("3/10/26 FCH: …"), as in Muertes and Clutches. */
  const initials = computed(() =>
    initialsOf(session.user?.displayName || '', sheet.listColumn('Abbr_name'), session.user?.username || ''),
  )

  // Insectary IDs: the free pre-made rows of Insectary_data, those after the last row used first,
  // then earlier empty rows (server/grid.mjs insectaryIds).
  const freeIds = ref<string[]>([])
  /** How many of freeIds come after the last row used; the rest are earlier empty rows. */
  const tailCount = ref(0)
  /** Free pre-made IDs with their rows, in sheet order (the fill handle continues in that order). */
  const premade = ref<{ value: string; row: number }[]>([])
  async function loadFreeIds() {
    try {
      const { sequence, rows, tail } = await api<{ sequence: string[]; rows: { value: string; row: number }[]; tail: number }>(
        'ids?kind=insectary&count=5000',
      )
      const pendingIds = new Set(pending.creates.filter(c => c.module === 'Insectary_data').map(c => String(c.values.Insectary_ID)))
      freeIds.value = sequence.filter(id => !pendingIds.has(id))
      tailCount.value = sequence.slice(0, tail).filter(id => !pendingIds.has(id)).length
      premade.value = rows.filter(r => !pendingIds.has(r.value)).sort((a, b) => a.row - b.row)
    } catch (e) {
      notify(errorText(e), 'error')
    }
  }
  watch(() => tables.tables.Insectary_data?.revision, loadFreeIds, { immediate: true })
  /** IDs already on butterflies in Collection_data (their Insectary_data row may not be filled in yet). */
  const collectedIds = computed(() => {
    const out = new Map<string, number>()
    for (const r of observed.value)
      if (!isBlank(r.values.Insectary_ID)) out.set(String(r.values.Insectary_ID).trim().toUpperCase(), r.row)
    return out
  })
  /**
   * An Insectary ID typed by hand must be a free pre-made row of Insectary_data
   * (the ID belongs to its row) and appear once in the list.
   */
  const idRows = computed(() => {
    void tables.versions.Insectary_data
    const out = new Map<string, { row: number; observed: boolean }>()
    for (const r of tables.tables.Insectary_data?.rows || [])
      if (!isBlank(r.values.Insectary_ID))
        out.set(String(r.values.Insectary_ID).trim().toUpperCase(), { row: r.row, observed: r.observed })
    return out
  })
  /**
   * The free pre-made ID `step` rows after `id` in Insectary_data (N9D + 1 → O0D),
   * for the fill handle; from an ID that is not free itself, the free rows after its row.
   */
  function nextId(id: string, step: number): string | null {
    const key = id.trim().toUpperCase()
    const at = premade.value.findIndex(r => r.value.toUpperCase() === key)
    if (at >= 0) return premade.value[at + step]?.value ?? null
    const row = idRows.value.get(key)?.row
    return row === undefined ? null : (premade.value.filter(r => r.row > row)[step - 1]?.value ?? null)
  }
  /** IDs of earlier empty rows given to the list: they may already be on the wings of a butterfly not typed in yet. */
  const earlierIds = computed(() => {
    const earlier = new Set(freeIds.value.slice(tailCount.value))
    return drafts.value.filter(d => d.fate === 'insectario' && earlier.has(d.insectaryId)).map(d => d.insectaryId)
  })
  const nextInsectaryId = () =>
    freeIds.value.find(id => !drafts.value.some(d => d.insectaryId === id) && !collectedIds.value.has(id.toUpperCase())) || ''

  // CAM IDs for field-preserved butterflies come from the Lists pool; tubes from the collection rack.
  const usedCams = computed(() => {
    void tables.version
    const used = new Set<string>()
    for (const [name, keys] of [
      [MODULE, ['CAM_ID', 'CAM_ID_insectary']],
      ['Insectary_data', ['CAM_ID', 'CAM_ID_CollData']],
    ] as const)
      for (const row of tables.tables[name]?.rows || [])
        for (const key of keys) if (!isBlank(row.values[key])) used.add(String(row.values[key]))
    return used
  })
  const camPool = computed(() => {
    const number = (id: string) => Number(/(\d+)$/.exec(id)?.[1] ?? -1)
    const pool = [...listColumn(lists.value, 'Wild_indv_CAMid')].sort((a, b) => number(a) - number(b))
    const at = pool.indexOf(latest('CAM_ID'))
    return [...pool.slice(at + 1), ...pool.slice(0, at + 1)].filter(id => !usedCams.value.has(id))
  })
  const usedTubes = computed(() => {
    void tables.version
    const used = new Map<string, string>()
    for (const [name, keys] of [
      [MODULE, ['Tube_1_id', 'Tube_2_id', 'Tube_3_id', 'Tube_4_id_LEGS']],
      ['Insectary_data', ['Tube_1_id', 'Tube_2_id', 'Tube_3_id', 'Tube_4_id']],
    ] as const)
      for (const row of tables.tables[name]?.rows || [])
        for (const key of keys)
          if (!isBlank(row.values[key]) && String(row.values[key]).trim() !== 'NA')
            used.set(String(row.values[key]).trim().toUpperCase(), t('{sheet} fila {row}', { sheet: name, row: row.row }))
    return used
  })
  const nextCam = () => camPool.value.find(id => !drafts.value.some(d => d.cam === id)) || ''
  const tubeRun = ref<string[]>([])
  async function loadTubes() {
    try {
      const { suggestions } = await api<{ suggestions: { value: string; context: string; medium: string }[] }>('ids?kind=tube')
      const run =
        suggestions.find(s => s.context === 'Colecta' && s.medium === header.value.medium) ||
        suggestions.find(s => s.context === 'Colecta')
      tubeRun.value = run ? (await api<{ sequence: string[] }>(`ids?kind=tube&start=${run.value}&count=60`)).sequence : []
    } catch (e) {
      notify(errorText(e), 'error')
    }
  }
  watch(() => header.value.medium, loadTubes, { immediate: true })
  const nextTube = () => tubeRun.value.find(id => !drafts.value.some(d => d.tube === id)) || ''

  /**
   * The CAM or tube after that of the preserved row above, when it is free: the
   * team's notebooks run consecutively (CAM079905, CAM079906…).
   */
  function following(d: Draft, column: 'cam' | 'tube'): string {
    const at = drafts.value.indexOf(d)
    const above = drafts.value
      .slice(0, at < 0 ? drafts.value.length : at)
      .reverse()
      .find(x => x.fate === 'preservada' && x[column])
    const next = above ? stepId(above[column].trim().toUpperCase(), 1) : null
    if (!next || drafts.value.some(x => x !== d && x[column].trim().toUpperCase() === next)) return ''
    if (column === 'cam') return camPool.value.includes(next) ? next : ''
    return TUBE_ID.test(next) && !usedTubes.value.has(next) ? next : ''
  }
  /**
   * What the next row added would get, as in Monitoreo's "Próxima marca": the
   * CAM and tube after the last preserved row of the list (else the next free
   * ones), the next free Insectary ID, and how many CAMs of the pool are left.
   */
  const upcoming = computed(() => {
    const none = {} as Draft
    const cams = camPool.value.filter(id => !drafts.value.some(d => d.cam.trim().toUpperCase() === id.toUpperCase()))
    return {
      cam: following(none, 'cam') || nextCam(),
      camsLeft: cams.length,
      tube: following(none, 'tube') || nextTube(),
      insectaryId: nextInsectaryId(),
    }
  })
  function setFate(draft: Draft, fate: Fate) {
    draft.fate = fate
    draft.insectaryId = fate === 'insectario' ? draft.insectaryId || nextInsectaryId() : ''
    draft.cam = fate === 'preservada' ? draft.cam || following(draft, 'cam') || nextCam() : ''
    draft.tube = fate === 'preservada' ? draft.tube || following(draft, 'tube') || nextTube() : ''
    draft.medium = fate === 'preservada' ? draft.medium || header.value.medium : ''
    // Insectary_data has no "female ?" / "male ?": a live butterfly's sex is definite.
    if (fate === 'insectario' && (draft.sex === 'female ?' || draft.sex === 'male ?')) draft.sex = insectarySex(draft.sex) as Draft['sex']
  }
  // Rows added before the free IDs, CAMs or tubes had arrived get them once they do.
  watch([freeIds, camPool, tubeRun], () => {
    for (const d of drafts.value) {
      if (d.fate === 'insectario' && !d.insectaryId) d.insectaryId = nextInsectaryId()
      if (d.fate === 'preservada' && !d.cam) d.cam = following(d, 'cam') || nextCam()
      if (d.fate === 'preservada' && !d.tube) d.tube = following(d, 'tube') || nextTube()
      // Lists kept from before the medium was a column of its own.
      if (d.fate === 'preservada' && !d.medium) d.medium = header.value.medium
    }
  })

  /** A butterfly with the header's place, people and weather, and the fate of the last one added. */
  function blankDraft(fate: Fate = addFate.value): Draft {
    return {
      key: crypto.randomUUID(),
      location: header.value.location,
      species: '',
      subspecies: '',
      sex: '',
      fate,
      time: '',
      purpose: '',
      notes: '',
      insectaryId: '',
      cam: '',
      tube: '',
      medium: '',
      weight: '',
      deadAlive: '',
      collector: header.value.collector,
      identifier: header.value.identifier,
      rainfall: header.value.rainfall,
      cloud: header.value.cloud,
    }
  }
  /** Adds `count` butterflies (all of one species if given); returns the index of the first. */
  function addDrafts(count: number, { fate = addFate.value, species = '', subspecies = '', sex = '' as Draft['sex'] } = {}) {
    const first = drafts.value.length
    for (let i = 0; i < count; i++) {
      drafts.value.push({ ...blankDraft(fate), species: species.trim(), subspecies, sex })
      setFate(drafts.value.at(-1)!, fate)
    }
    return first
  }
  /**
   * "Another like this": a new butterfly right after `d`, with its species, form,
   * sex, fate, place, people, weather, time and purpose; its own new ID, CAM and
   * tube (the next free ones); no note.
   */
  function duplicate(d: Draft): Draft {
    const at = drafts.value.indexOf(d)
    const copy: Draft = { ...d, key: crypto.randomUUID(), notes: '', insectaryId: '', cam: '', tube: '', weight: '', deadAlive: '' }
    drafts.value.splice(at < 0 ? drafts.value.length : at + 1, 0, copy)
    const placed = drafts.value[at < 0 ? drafts.value.length - 1 : at + 1]
    setFate(placed, d.fate)
    return placed
  }
  function remove(key: string) {
    drafts.value = drafts.value.filter(d => d.key !== key)
  }
  /** A row nothing was written in yet (its ID, CAM and tube are filled in by the app). */
  const emptyCount = computed(() => drafts.value.filter(isEmptyDraft).length)
  function removeEmpty() {
    const n = emptyCount.value
    drafts.value = drafts.value.filter(d => !isEmptyDraft(d))
    notify(t('Se quitaron {n} filas vacías: quedan {left}', { n, left: drafts.value.length }))
  }
  function clearAll() {
    const filled = drafts.value.length - emptyCount.value
    if (filled && !confirm(t('¿Vaciar la lista? Se pierden {n} filas con datos sin guardar.', { n: filled }))) return
    drafts.value = []
    notify(t('Lista vaciada'))
  }

  /** Writes a typed or pasted value in a row; says so when the row changes Release_Collect because of it. */
  function setColumn(d: Draft, column: Column, text: string): string | null {
    if (column === 'sex') d.sex = parseSex(text)
    else if (column === 'fate') {
      const fate = parseFate(text)
      if (fate) setFate(d, fate)
    } else if (column === 'time') d.time = parseTime(text)
    else if (column === 'cam') {
      // "CAM079895", or CAM and tube together ("CAM079895 · FS90415305 (Flash frozen)").
      const { cam, tube } = parseCamTube(text)
      if (!applies(d, column) && !cam) return null
      // A CAM given to a butterfly sent to the insectary (or released) means it was preserved: its Insectary ID is freed.
      const freed = !applies(d, column) && d.fate === 'insectario' ? d.insectaryId : ''
      const switched = !applies(d, column)
      if (switched) setFate(d, 'preservada')
      if (cam || !text.trim()) d.cam = cam
      if (tube) d.tube = tube
      const medium = mediums.value.find(m => text.includes(`(${m})`))
      if (medium) d.medium = medium
      if (switched)
        return freed
          ? t('pasa a Collected_Preserved por el CAM {cam} (queda libre {id})', { cam, id: freed })
          : t('pasa a Collected_Preserved por el CAM {cam}', { cam })
    } else if (!applies(d, column)) return null
    else if (column === 'insectaryId') {
      // The ID written on the wings, if it is not the one suggested (checked in `problems`).
      const id = text.trim().toUpperCase()
      if (INSECTARY_ID.test(id)) d.insectaryId = id
    } else if (column === 'tube') d.tube = /^(NA|N\/A)$/i.test(text.trim()) ? '' : text.trim().toUpperCase()
    else if (column === 'medium') {
      d.medium = text.trim()
      // The next rows added take the same medium (and its tubes).
      if (d.medium) header.value.medium = d.medium
    } else if (column === 'weight') {
      const w = parseWeight(text)
      d.weight = w === null ? text.trim() : String(w)
    } else if (column === 'deadAlive') {
      const v = text.trim()
      d.deadAlive = /^(dead|muerta)$/i.test(v) ? 'Dead' : /^(alive|viva)$/i.test(v) ? 'Alive' : ''
    } else d[column] = text === 'NA' && column === 'subspecies' ? '' : text
    return null
  }
  const shorten = (text: string) => (text.length > 24 ? `${text.slice(0, 22)}…` : text)
  /**
   * A block copied from a spreadsheet, pasted at a row and column: fills down and
   * across, adding rows if needed. False when the text is a single value (the
   * cell takes it as typed). Values that do not fit their column (a note in
   * Tube_1_id, a CAM in Insectary_ID: the block was pasted a column off) are
   * left out, and the notice says which.
   */
  function pasteText(text: string, index: number, column: Column): boolean {
    const block = parseBlock(text)
    if (!block) return false
    const start = COLUMNS.indexOf(column)
    let added = 0
    const skipped: string[] = []
    const switched: number[] = []
    block.forEach((cells, r) => {
      if (!drafts.value[index + r]) {
        drafts.value.push(blankDraft())
        setFate(drafts.value.at(-1)!, addFate.value)
        added++
      }
      const d = drafts.value[index + r]
      cells.forEach((text, c) => {
        const target = COLUMNS[start + c]
        if (!target) return
        const why = misfit(target, text)
        if (why)
          return void skipped.push(
            t('«{value}» en {column}, fila {row}: {problem}', {
              value: shorten(text.trim()),
              column: HEADERS[target],
              row: index + r + 1,
              problem: why,
            }),
          )
        if (setColumn(d, target, text)) switched.push(index + r + 1)
      })
    })
    const total = drafts.value.length
    const notes = [
      added
        ? t('Pegadas {n} filas ({added} nuevas): la lista tiene {total}', { n: block.length, added, total })
        : t('Pegadas {n} filas: la lista tiene {total}', { n: block.length, total }),
    ]
    if (switched.length) notes.push(t('filas {rows} pasan a Collected_Preserved por su CAM', { rows: switched.join(', ') }))
    if (skipped.length)
      notes.push(
        tn(
          skipped.length,
          'no se pegaron {n} valor que no encaja (¿columnas corridas?): {values}',
          'no se pegaron {n} valores que no encajan (¿columnas corridas?): {values}',
          { values: `${skipped.slice(0, 3).join('; ')}${skipped.length > 3 ? '…' : ''}` },
        ),
      )
    notify(notes.join('. '), skipped.length ? 'error' : undefined)
    return true
  }
  /** A cell edited in the grid (typed, pasted as one value, filled by dragging or Ctrl+D). */
  function editCell(key: string, column: Column, text: string) {
    const d = drafts.value.find(x => x.key === key)
    if (!d) return
    const why = misfit(column, text)
    // The cell goes back to what the list holds (CollectGrid).
    if (why)
      return notify(
        t('«{value}» {problem}: no se escribió en {column}', {
          value: shorten(text.trim()),
          problem: why,
          column: HEADERS[column],
        }),
      )
    const note = setColumn(d, column, text)
    if (note) notify(t('Fila {n} {change}', { n: drafts.value.indexOf(d) + 1, change: note }))
    // Outside a list the sheet does not enforce: kept (red corner), with a warning so a typo is noticed.
    const field = LISTED[column]
    const issue = field && !collectionRules.value?.lists[field]?.strict ? cellProblem(d, column) : null
    if (issue && !note) notify(t('{problem}: se guarda igual; corrígelo si es un error', { problem: issue }))
  }

  /** An Insectary ID, CAM or tube repeated in the list, or already used in the sheets. */
  function idProblem(d: Draft, column: Column = 'insectaryId'): string | null {
    if (column === 'cam' || column === 'tube') {
      const value = d[column].trim().toUpperCase()
      if (d.fate !== 'preservada' || !value) return null
      if (drafts.value.filter(x => x.fate === 'preservada' && x[column].trim().toUpperCase() === value).length > 1)
        return t('{id} está repetido en la lista', { id: value })
      if (column === 'cam' && usedCams.value.has(value)) return t('{id} ya está usado en las hojas', { id: value })
      const used = column === 'tube' && usedTubes.value.get(value)
      return used ? t('{id} ya está usado ({where})', { id: value, where: used }) : null
    }
    if (d.fate !== 'insectario' || !d.insectaryId) return null
    if (drafts.value.filter(x => x.fate === 'insectario' && x.insectaryId === d.insectaryId).length > 1)
      return t('{id} está repetido en la lista', { id: d.insectaryId })
    if (!idRows.value.size) return null
    const row = idRows.value.get(d.insectaryId)
    if (!row) return t('{id} no tiene fila preparada en Insectary_data', { id: d.insectaryId })
    if (row.observed) return t('{id} ya está registrado (Insectary_data fila {row})', { id: d.insectaryId, row: row.row })
    const collected = collectedIds.value.get(d.insectaryId)
    if (collected) return t('{id} ya está en Collection_data (fila {row})', { id: d.insectaryId, row: collected })
    return null
  }
  /**
   * The sheet's lists for the columns typed in the list (Collection_data): a
   * species not in Taxonomy, a place not in Location_data, a purpose or sex not
   * in Lists. They are strict in the sheet, so saving waits until they are fixed.
   */
  const collectionRules = computed(() => verificationsFor(MODULE))
  const LISTED: Partial<Record<Column, string>> = {
    location: 'Collection_location',
    species: 'SPECIES',
    sex: 'Sex',
    purpose: 'Purpose',
    medium: 'Preservation_medium',
    collector: 'Collector',
    identifier: 'Identifier',
    rainfall: 'Rainfall',
    cloud: 'Cloud_cover',
  }
  function cellProblem(d: Draft, column: Column): string | null {
    const field = LISTED[column]
    return field ? listProblem(collectionRules.value, field, d[column as 'species']) : null
  }
  /** What keeps one butterfly from being saved, by column (the cards show them in place). */
  function draftIssues(d: Draft): { column: Column; text: string }[] {
    if (isEmptyDraft(d)) return []
    const out: { column: Column; text: string }[] = []
    if (!d.species) out.push({ column: 'species', text: t('falta la especie') })
    if (!d.sex) out.push({ column: 'sex', text: t('falta el sexo') })
    if (!d.location) out.push({ column: 'location', text: t('falta el lugar') })
    if (!d.collector) out.push({ column: 'collector', text: t('falta quién la colectó (Collector)') })
    if (d.species && !d.identifier) out.push({ column: 'identifier', text: t('falta quién la identificó (Identifier)') })
    if (d.time && !/^\d{2}:\d{2}$/.test(d.time)) out.push({ column: 'time', text: t('la hora no es hh:mm') })
    if (d.fate === 'insectario' && /\?$/.test(d.sex))
      out.push({ column: 'sex', text: t('una mariposa viva va al insectario con sexo seguro (sin «?»)') })
    if (d.fate === 'insectario' && !d.insectaryId)
      out.push({
        column: 'insectaryId',
        text: t('no quedan Insectary IDs libres; crea más filas preasignadas en Insectary_data'),
      })
    for (const column of ['insectaryId', 'cam', 'tube'] as const) {
      const issue = idProblem(d, column)
      if (issue) out.push({ column, text: issue })
    }
    for (const column of Object.keys(LISTED) as Column[]) {
      const issue = column === 'location' && !d.location ? null : cellProblem(d, column)
      if (issue) out.push({ column, text: issue })
    }
    if (d.fate === 'preservada') {
      if (d.weight && parseWeight(d.weight) === null) out.push({ column: 'weight', text: t('no es un peso en gramos (p. ej. 0.152)') })
      if (!d.cam) out.push({ column: 'cam', text: t('falta el CAM_ID') })
      if (!d.tube) out.push({ column: 'tube', text: t('falta el Tube_1_id') })
      if (!d.medium) out.push({ column: 'medium', text: t('falta el Preservation_medium') })
    }
    return out
  }
  const problems = computed(() => [
    ...(emptyCount.value ? [t('{n} filas vacías', { n: emptyCount.value })] : []),
    ...drafts.value.flatMap((d, i) =>
      draftIssues(d).map(p => t('fila {n}: {problem}', { n: i + 1, problem: p.text })),
    ),
  ])

  const serial = (iso: string) => (iso ? isoToSerial(iso) : null)
  const dayFraction = (time: string) => {
    const m = /^(\d{1,2}):(\d{2})$/.exec(time.trim())
    return m ? (Number(m[1]) * 60 + Number(m[2])) / (24 * 60) : null
  }
  /** The same columns the team fills by hand (checked against the 23-Sep-26 rows). */
  function collectionRow(d: Draft): Record<string, CellValue> {
    const date = serial(header.value.date)
    const values: Record<string, CellValue> = {
      Release_Collect: FATES[d.fate].value,
      FieldMark_ID: 'NA',
      Insectary_ID: d.fate === 'insectario' ? d.insectaryId : 'NA',
      SPECIES: d.species,
      Subspecies_Form: d.subspecies || 'NA',
      Identifier: d.identifier || null,
      ID_status: 'COMPLETE',
      Sex: d.sex,
      Collection_location: d.location,
      Transect_section: 'NA',
      Bait: 'NA',
      Forest_stratum: 'NA',
      Collection_date: date,
      Collection_time: dayFraction(d.time) ?? 'NA',
      Collector: d.collector || null,
      Rainfall: d.rainfall || null,
      Cloud_cover: d.cloud || 'NA',
      Flight_height: 'NA',
      Purpose: d.purpose || 'NA',
      // Dated and signed as the team writes notes ("3/10/26 FCH: Sexed by genitalia").
      Notes_Collection_data: signNote(d.notes, noteDay(isoToSerial(todayIso())), initials.value) || null,
    }
    if (d.fate === 'preservada')
      Object.assign(values, {
        CAM_ID_insectary: 'NA',
        CAM_ID: d.cam,
        Tube_1_id: d.tube,
        Tube_1_tissue: 'WHOLE_ORGANISM',
        Tube_2_id: 'NA',
        Tube_2_tissue: 'NOT_COLLECTED',
        Tube_3_id: 'NA',
        Tube_3_tissue: 'NOT_PROVIDED',
        Tube_4_id_LEGS: 'NA',
        Butterfly_weight: parseWeight(d.weight ?? '') || 'NA',
        Death_date: date,
        Preservation_date: date,
        Preservation_medium: d.medium || header.value.medium,
        Preserved_dead_alive: d.deadAlive || 'Alive',
        Splitted_body: 'No',
        Location_Head: 'Ikiam',
        Location_Torax: 'Ikiam',
        Location_abdomen: 'Ikiam',
        Location_Legs: 'Ikiam',
        Location_wings: 'Ikiam',
      })
    for (const field of createFormulas.value) delete values[field]
    for (const [k, v] of Object.entries(values)) if (v === null || v === '') delete values[k]
    return values
  }
  /** The butterfly's row in Insectary_data (a pre-made row with that ID). */
  const insectaryRow = (d: Draft): Record<string, CellValue> => ({
    Insectary_ID: d.insectaryId,
    Wild_Reared: 'Wild-caught',
    'CLUTCH NUMBER': 'NA',
    Stock_of_origin: 'NA',
    SPECIES: [d.species, d.subspecies].filter(s => s && s !== 'NA').join(' '),
    Sex: insectarySex(d.sex),
    Collection_location: d.location,
    Intro2Insectary_date: serial(header.value.date),
    // Brought in as an adult: with a date there, LIFESTAGE Adult (team rule, 5 Oct 2026).
    ...(serial(header.value.date) !== null ? { LIFESTAGE: ADULT } : {}),
  })

  // --- Save (one save for the whole list), then Undo
  const saving = ref(false)
  /** The last save, to undo it: its Historial action, and the list as it was, to bring it back. */
  const lastSave = ref<null | { actionId: string; count: number; drafts: Draft[]; date: string }>(null)
  const undoing = ref(false)
  const waitIdle = async () => {
    for (let i = 0; i < 300 && pending.saving; i++) await new Promise(r => setTimeout(r, 100))
  }
  /** Saves the list (Collection_data, and Insectary_data for the live ones); true when everything was written. */
  async function save(): Promise<boolean> {
    if (!drafts.value.length || saving.value) return false
    if (problems.value.length) {
      notify(problems.value.slice(0, 3).join('; '), 'error')
      return false
    }
    saving.value = true
    const added: string[] = []
    const kept = JSON.parse(JSON.stringify(drafts.value)) as Draft[]
    for (const d of drafts.value) {
      added.push(pending.addCreate(MODULE, d.insectaryId || d.cam || t('nuevo'), collectionRow(d)).clientId)
      if (d.fate === 'insectario') added.push(pending.addCreate('Insectary_data', d.insectaryId, insectaryRow(d)).clientId)
    }
    pending.touch()
    try {
      // An automatic save already running would leave these rows for later (and give no Undo).
      await waitIdle()
      const result = await pending.save(`Colecta ${header.value.date}`)
      if (added.some(id => pending.creates.some(c => c.clientId === id))) {
        // What was refused stays pending (red in the table below), not in the list: typed again it would go twice.
        notify(t('Revisa los errores marcados en la tabla'), 'error')
        drafts.value = []
        return false
      }
      notify(tn(kept.length, 'Colecta guardada: {n} mariposa', 'Colecta guardada: {n} mariposas'), 'success')
      lastSave.value = result.actionId ? { actionId: result.actionId, count: kept.length, drafts: kept, date: header.value.date } : null
      drafts.value = []
      loadFreeIds()
      loadTubes()
      return true
    } catch (e) {
      notify(errorText(e), 'error')
      return false
    } finally {
      saving.value = false
    }
  }
  /** Undoes the last save (both sheets) and brings its butterflies back to the list, to correct and save again. */
  async function undo() {
    const last = lastSave.value
    if (!last || undoing.value) return
    undoing.value = true
    try {
      const result = await api<{ records?: ServerRecord[] }>('history/undo', {
        method: 'POST',
        body: { actionIds: [last.actionId], requestId: requestId(), reason: null },
      })
      if (result.records?.length) tables.merge(result.records)
      else await Promise.all([tables.load(MODULE, true), tables.load('Insectary_data', true)])
      drafts.value = [...last.drafts, ...drafts.value]
      header.value.date = last.date
      lastSave.value = null
      notify(tn(last.count, 'Deshecha la colecta de {n} mariposa', 'Deshecha la colecta de {n} mariposas'), 'success')
      loadFreeIds()
      loadTubes()
    } catch (e) {
      notify(errorText(e), 'error')
    } finally {
      undoing.value = false
    }
  }

  return {
    ...sheet,
    header,
    drafts,
    addFate,
    setDay,
    setTeam,
    initials,
    observed,
    speciesList,
    recentSpecies,
    allSpecies,
    subspeciesFor,
    places,
    recentPlaces,
    people,
    recentPeople,
    rainfalls,
    clouds,
    mediums,
    fieldMediums,
    purposes,
    upcoming,
    earlierIds,
    nextId,
    loadFreeIds,
    setFate,
    setColumn,
    pasteText,
    editCell,
    addDrafts,
    duplicate,
    remove,
    emptyCount,
    removeEmpty,
    clearAll,
    idProblem,
    cellProblem,
    draftIssues,
    problems,
    saving,
    save,
    lastSave,
    undoing,
    undo,
  }
}
export type CollectState = ReturnType<typeof useCollect>
