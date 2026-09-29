<script setup lang="ts">
import ChoiceField from '../components/ChoiceField.vue'
import DateField from '../components/DateField.vue'
import { computed, nextTick, onDeactivated, reactive, ref, watch } from 'vue'
import { CheckSquare, Copy, Eraser, Plus, Save, Trash2, X } from 'lucide-vue-next'
import CollectGrid from '../components/CollectGrid.vue'
import SheetGrid from '../components/SheetGrid.vue'
import InsectaryIdsWarning from '../components/InsectaryIdsWarning.vue'
import { useSheet } from '../composables/useSheet'
import { api } from '../lib/api'
import { isBlank } from '../lib/cells'
import { formatSerial, isoToSerial, todayIso, weekdayOf } from '../lib/dates'
import { errorText, notify } from '../lib/notice'
import { listColumn } from '../lib/options'
import { listProblem, verificationsFor } from '../lib/verifications'
import {
  COLUMNS,
  FATES,
  HEADERS,
  INSECTARY_ID,
  SEX_VALUES,
  TUBE_ID,
  applies,
  insectarySex,
  misfit,
  summarize,
  type Column,
  type Draft,
  type Fate,
} from '../lib/collect'
import { parseBlock, parseCamTube, parseFate, parseSex, parseTime, stepId } from '../lib/paste'
import { persistentRef } from '../lib/persist'
import { orderColumns } from '../lib/rows'
import type { CellValue } from '../lib/types'
import { usePending } from '../stores/pending'
import { useSession } from '../stores/session'
import { useTables } from '../stores/tables'
import { t, tn } from '../lib/i18n'

/**
 * A day of field collection, entered in bulk. The header holds what the whole
 * outing shares (date, people, weather, place); each butterfly gets species,
 * sex and fate. Butterflies taken alive to the insectary get the next
 * Insectary ID (to write on the wings) and no CAM ID; their Insectary_data row
 * is filled in the same save. Butterflies preserved in the field get a CAM ID
 * and a tube. A second place can be added by changing the place and adding more.
 */
const MODULE = 'Collection_data'
const module = ref(MODULE)
const pending = usePending()
const tables = useTables()
const { table, ready, lists, options, creates, createFormulas } = useSheet(module)
tables.load('Insectary_data').catch(() => {})

const header = persistentRef('collect:header', {
  date: todayIso(),
  collector: '',
  identifier: '',
  location: '',
  rainfall: '',
  cloud: '',
  medium: 'Flash frozen',
})
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
const addCount = ref(1)
const addFate = ref<Fate>('insectario')
/** Optional: the species of all the rows being added (e.g. five Mechanitis at once). */
const addSpecies = ref('')
/**
 * The list as a spreadsheet everywhere: ranges, copy/paste and the fill handle
 * on computers; tap, stretch and the action bar on phones (lib/gridKit.ts).
 * The form (big buttons, ticked rows and a bar to apply values) stays available.
 */
const view = persistentRef<'tabla' | 'formulario'>('collect:view2', 'tabla', { lasting: true })
const touchScreen = window.matchMedia('(pointer: coarse)').matches
/** Phones show the outing as one line ("23-Sep · Cavernas · PAS ✎") until tapped; open while no place is chosen. */
const headerOpen = ref(!touchScreen || !header.value.location)
const headerChip = computed(() => {
  const h = header.value
  const day = h.date ? formatSerial(isoToSerial(h.date)).replace(/-\d{2}$/, '') : t('sin fecha')
  return [day, h.location || t('sin lugar'), h.collector.split(' - ')[0]].filter(Boolean).join(' · ')
})
const grid = ref<InstanceType<typeof CollectGrid>>()
const saving = ref(false)
const recentCount = ref(10)

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

/** Species in Collection_data are "Genus species"; the most used come first. */
const ranked = (field: string, filter?: (values: Record<string, CellValue>) => boolean) => {
  const counts = new Map<string, number>()
  for (const row of observed.value)
    if (!isBlank(row.values[field]) && (!filter || filter(row.values)))
      counts.set(String(row.values[field]), (counts.get(String(row.values[field])) || 0) + 1)
  return [...counts].sort((a, b) => b[1] - a[1]).map(([v]) => v)
}
const speciesList = computed(() => ranked('SPECIES'))
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
    if (!species || !sub || /^(NA|N\/A)$/i.test(sub)) return
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
const places = computed(() => [...new Set([...ranked('Collection_location'), ...(options.value.Collection_location || [])])])
const people = computed(() => options.value.Collector || ranked('Collector'))
const rainfalls = computed(() => options.value.Rainfall || ranked('Rainfall'))
const clouds = computed(() => options.value.Cloud_cover || ranked('Cloud_cover'))
const mediums = computed(() => options.value.Preservation_medium || ['Flash frozen'])
const fateChoices = Object.entries(FATES).map(([value, f]) => ({ value, label: f.label }))

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
  tables.version
  const used = new Set<string>()
  for (const [sheet, keys] of [
    [MODULE, ['CAM_ID', 'CAM_ID_insectary']],
    ['Insectary_data', ['CAM_ID', 'CAM_ID_CollData']],
  ] as const)
    for (const row of tables.tables[sheet]?.rows || [])
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
  tables.version
  const used = new Map<string, string>()
  for (const [sheet, keys] of [
    [MODULE, ['Tube_1_id', 'Tube_2_id', 'Tube_3_id', 'Tube_4_id_LEGS']],
    ['Insectary_data', ['Tube_1_id', 'Tube_2_id', 'Tube_3_id', 'Tube_4_id']],
  ] as const)
    for (const row of tables.tables[sheet]?.rows || [])
      for (const key of keys)
        if (!isBlank(row.values[key]) && String(row.values[key]).trim() !== 'NA')
          used.set(String(row.values[key]).trim().toUpperCase(), t('{sheet} fila {row}', { sheet, row: row.row }))
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
async function add() {
  if (!header.value.location) {
    headerOpen.value = true
    return notify(t('Elige el lugar de colecta'))
  }
  const count = Math.min(60, Math.max(1, Math.round(addCount.value || 1)))
  const first = drafts.value.length
  for (let i = 0; i < count; i++) {
    drafts.value.push({ ...blankDraft(), species: addSpecies.value.trim() })
    setFate(drafts.value.at(-1)!, addFate.value)
  }
  notify(
    tn(count, 'Se añadieron {n} fila: la lista tiene {total}', 'Se añadieron {n} filas: la lista tiene {total}', {
      total: drafts.value.length,
    }),
  )
  // Straight to the first new row, ready to type its species.
  await nextTick()
  if (view.value === 'tabla') return void grid.value?.focusCell(first, 'species')
  const row = document.querySelector<HTMLElement>(`[data-draft="${drafts.value[first]?.key}"]`)
  row?.scrollIntoView({ block: 'center', behavior: 'smooth' })
  row?.querySelector<HTMLInputElement>('[data-col=species]')?.focus({ preventScroll: true })
}
function remove(key: string) {
  drafts.value = drafts.value.filter(d => d.key !== key)
}
/** A row nothing was written in yet (its ID, CAM and tube are filled in by the app). */
const isEmpty = (d: Draft) => !d.species && !d.subspecies && !d.sex && !d.time && !d.purpose && !d.notes
const emptyCount = computed(() => drafts.value.filter(isEmpty).length)
function removeEmpty() {
  const n = emptyCount.value
  drafts.value = drafts.value.filter(d => !isEmpty(d))
  notify(t('Se quitaron {n} filas vacías: quedan {left}', { n, left: drafts.value.length }))
}
/**
 * Spreadsheet habits in the list. The columns in the order they appear, which
 * is also the order pasted cells are spread over (from the cell pasted into).
 */
function blankDraft(): Draft {
  return {
    key: crypto.randomUUID(),
    location: header.value.location,
    species: '',
    subspecies: '',
    sex: '',
    fate: addFate.value,
    time: '',
    purpose: '',
    notes: '',
    insectaryId: '',
    cam: '',
    tube: '',
    medium: '',
    collector: header.value.collector,
    identifier: header.value.identifier,
    rainfall: header.value.rainfall,
    cloud: header.value.cloud,
  }
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
function onPaste(event: ClipboardEvent, index: number, column: Column) {
  if (pasteText(event.clipboardData?.getData('text/plain') || '', index, column)) event.preventDefault()
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
function focusCell(index: number, column: Column) {
  const key = drafts.value[index]?.key
  document.querySelector<HTMLElement>(`[data-draft="${key}"] [data-col="${column}"]`)?.focus()
}
/** Enter: same column, next row. Ctrl+D: copy from the row above (species with its subspecies) and move down. */
function onCellKey(event: KeyboardEvent, index: number, column: Column) {
  if ((event.ctrlKey || event.metaKey) && event.key.toLowerCase() === 'd') {
    event.preventDefault()
    const above = drafts.value[index - 1]
    const d = drafts.value[index]
    if (!above || !d) return
    if (column === 'species' || column === 'subspecies') {
      d.species = above.species
      d.subspecies = above.subspecies
    } else if (column === 'location' || column === 'time' || column === 'purpose' || column === 'notes') d[column] = above[column]
    focusCell(index + 1, column)
  } else if (event.key === 'Enter') {
    event.preventDefault()
    focusCell(index + 1, column)
  }
}
// Rows ticked in the form (phones), and what to apply to them.
const selected = ref<string[]>([])
const anchor = ref('')
const isSelected = (key: string) => selected.value.includes(key)
function toggleSelect(key: string) {
  selected.value = isSelected(key) ? selected.value.filter(k => k !== key) : [...selected.value, key]
  anchor.value = key
}
/** Ticks every row from the last one ticked to this one. */
function selectTo(key: string) {
  const keys = drafts.value.map(d => d.key)
  const [a, b] = [keys.indexOf(anchor.value), keys.indexOf(key)].sort((x, y) => x - y)
  if (a < 0) return toggleSelect(key)
  selected.value = [...new Set([...selected.value, ...keys.slice(a, b + 1)])]
  anchor.value = key
}
watch(
  () => drafts.value.map(d => d.key).join('|'),
  () => (selected.value = selected.value.filter(k => drafts.value.some(d => d.key === k))),
)
const bulk = reactive({ species: '', subspecies: '', sex: '' as Draft['sex'], fate: '' as '' | Fate })
/** Applies the values filled in the bar (the empty ones are left alone) to the rows ticked. */
function applyBulk() {
  const rows = drafts.value.filter(d => isSelected(d.key))
  for (const d of rows) {
    if (bulk.species) {
      d.species = bulk.species
      d.subspecies = bulk.subspecies
    } else if (bulk.subspecies) d.subspecies = bulk.subspecies
    if (bulk.sex) d.sex = bulk.sex
    if (bulk.fate) setFate(d, bulk.fate)
  }
  notify(tn(rows.length, 'Aplicado a {n} fila', 'Aplicado a {n} filas'))
}
/** Touch-screen copy: a row's species, subspecies, sex and fate go to the bar, to apply to the rows ticked. */
function copyRow(d: Draft, n: number) {
  Object.assign(bulk, { species: d.species, subspecies: d.subspecies, sex: d.sex, fate: d.fate })
  selected.value = selected.value.filter(k => k !== d.key)
  anchor.value = d.key
  notify(t('Copiada la fila {n}: marca las filas donde pegarla y pulsa Aplicar', { n }))
}
function removeSelected() {
  drafts.value = drafts.value.filter(d => !isSelected(d.key))
  selected.value = []
}
function clearAll() {
  const filled = drafts.value.length - emptyCount.value
  if (filled && !confirm(t('¿Vaciar la lista? Se pierden {n} filas con datos sin guardar.', { n: filled }))) return
  drafts.value = []
  notify(t('Lista vaciada'))
}
const groups = computed(() => {
  const out = new Map<string, Record<Fate, number>>()
  for (const d of drafts.value) {
    const g = out.get(d.location) || { insectario: 0, preservada: 0, liberada: 0 }
    g[d.fate]++
    out.set(d.location, g)
  }
  return [...out]
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
/** The people of the list's rows, for the summary before saving. */
const distinct = (field: 'collector' | 'identifier' | 'rainfall' | 'cloud') =>
  [
    ...new Set(
      drafts.value
        .filter(d => !isEmpty(d))
        .map(d => d[field])
        .filter(Boolean),
    ),
  ].join(', ') || '—'
function cellProblem(d: Draft, column: Column): string | null {
  const field = LISTED[column]
  return field ? listProblem(collectionRules.value, field, d[column as 'species']) : null
}
const problems = computed(() => [
  ...(emptyCount.value ? [t('{n} filas vacías', { n: emptyCount.value })] : []),
  ...drafts.value.flatMap((d, i) => {
    if (isEmpty(d)) return []
    const n = i + 1
    const out: string[] = []
    if (!d.species) out.push(t('fila {n}: falta la especie', { n }))
    if (!d.sex) out.push(t('fila {n}: falta el sexo', { n }))
    if (d.fate === 'insectario' && !d.insectaryId)
      out.push(t('fila {n}: no quedan Insectary IDs libres; crea más filas preasignadas en Insectary_data', { n }))
    for (const column of ['insectaryId', 'cam', 'tube'] as const) {
      const idIssue = idProblem(d, column)
      if (idIssue) out.push(t('fila {n}: {column} {problem}', { n, column: HEADERS[column], problem: idIssue }))
    }
    for (const column of Object.keys(LISTED) as Column[]) {
      const issue = cellProblem(d, column)
      if (issue) out.push(t('fila {n}: {column} {problem}', { n, column: LISTED[column], problem: issue }))
    }
    if (d.fate === 'preservada' && (!d.cam || !d.tube || !d.medium))
      out.push(t('fila {n}: falta CAM_ID, Tube_1_id o Preservation_medium', { n }))
    return out
  }),
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
    Collection_time: dayFraction(d.time),
    Collector: d.collector || null,
    Rainfall: d.rainfall || null,
    Cloud_cover: d.cloud || null,
    Flight_height: 'NA',
    Purpose: d.purpose || null,
    Notes_Collection_data: d.notes || null,
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
      Butterfly_weight: 'NA',
      Death_date: date,
      Preservation_date: date,
      Preservation_medium: d.medium || header.value.medium,
      Preserved_dead_alive: 'Alive',
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
})

/** Read over before saving: the day (a list typed days later was saved as today), places, sexes, CAMs. */
const confirming = ref(false)
// Escape closes the summary wherever the focus is (the dialog itself is not focused when it opens).
const closeOnEscape = (e: KeyboardEvent) => e.key === 'Escape' && (confirming.value = false)
watch(confirming, open => (open ? window.addEventListener : window.removeEventListener)('keydown', closeOnEscape))
onDeactivated(() => (confirming.value = false))
const summary = computed(() => summarize(drafts.value))
const isToday = computed(() => header.value.date === todayIso())
const longDate = computed(() =>
  header.value.date ? `${weekdayOf(header.value.date)} ${formatSerial(isoToSerial(header.value.date))}` : t('sin fecha'),
)
function askSave() {
  if (!drafts.value.length) return
  if (problems.value.length) return notify(problems.value.slice(0, 3).join('; '), 'error')
  confirming.value = true
}
function confirmSave() {
  confirming.value = false
  save()
}
async function save() {
  if (!drafts.value.length) return
  if (problems.value.length) return notify(problems.value.slice(0, 3).join('; '), 'error')
  saving.value = true
  const added: string[] = []
  for (const d of drafts.value) {
    added.push(pending.addCreate(MODULE, d.insectaryId || d.cam || t('nuevo'), collectionRow(d)).clientId)
    if (d.fate === 'insectario') added.push(pending.addCreate('Insectary_data', d.insectaryId, insectaryRow(d)).clientId)
  }
  pending.touch()
  try {
    await pending.save(`Colecta ${header.value.date}`)
    if (added.some(id => pending.creates.some(c => c.clientId === id)))
      throw new Error(t('Revisa los errores marcados en la tabla'))
    notify(t('Colecta guardada: {n} mariposas', { n: drafts.value.length }), 'success')
    drafts.value = []
    loadFreeIds()
    loadTubes()
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    saving.value = false
  }
}

const columns = computed(() =>
  table.value
    ? // Who the butterfly is first (Insectary_ID pinned), then what it is, then what happened, when, where and by whom.
      orderColumns(table.value.columns, [
        'Insectary_ID',
        'CAM_ID',
        'Tube_1_id',
        'SPECIES',
        'Subspecies_Form',
        'Sex',
        'Release_Collect',
        'Collection_date',
        'Collection_location',
        'Collector',
        'Identifier',
        'Rainfall',
        'Cloud_cover',
        'Preservation_medium',
        'Notes_Collection_data',
      ])
    : [],
)
const recent = computed(() => observed.value.slice(-recentCount.value))
</script>

<template>
  <div class="flex h-full flex-col overflow-y-auto">
    <!-- Phones: the outing folds into one line, so the list is in sight; a tap opens it. -->
    <button
      v-if="touchScreen"
      type="button"
      class="flex w-full items-center gap-2 border-b border-stone-200 bg-white px-3 py-2 text-left text-sm"
      :aria-expanded="headerOpen"
      @click="headerOpen = !headerOpen"
    >
      <span
        class="min-w-0 flex-1 truncate rounded-full bg-stone-100 px-3 py-1"
        :class="{ 'bg-amber-50 text-amber-900': isToday }"
      >
        {{ headerChip }}
      </span>
      <span class="shrink-0 text-brand-700">{{ headerOpen ? $t('Cerrar') : '✎' }}</span>
    </button>
    <div v-if="headerOpen" class="toolbar">
      <label>
        <span class="field-label"
          >Collection_date <span class="font-normal text-stone-500">{{ weekdayOf(header.date) }}</span></span
        >
        <DateField v-model="header.date" class="field-input" :class="{ 'border-amber-500 bg-amber-50': isToday }" />
        <span v-if="isToday" class="block text-xs text-amber-800">{{ $t('¿Es hoy la fecha de la colecta?') }}</span>
      </label>
      <label class="min-w-52">
        <span class="field-label"
          >Collector <span class="font-normal text-stone-500">{{ $t('(filas nuevas)') }}</span></span
        >
        <ChoiceField v-model="header.collector" class="field-input" :options="people" />
      </label>
      <label class="min-w-52">
        <span class="field-label"
          >Identifier <span class="font-normal text-stone-500">{{ $t('(filas nuevas)') }}</span></span
        >
        <ChoiceField v-model="header.identifier" class="field-input" :options="people" />
      </label>
      <label>
        <span class="field-label"
          >Rainfall <span class="font-normal text-stone-500">{{ $t('(filas nuevas)') }}</span></span
        >
        <ChoiceField v-model="header.rainfall" class="field-input" :options="rainfalls" :freetext="false" allow-empty />
      </label>
      <label>
        <span class="field-label"
          >Cloud_cover <span class="font-normal text-stone-500">{{ $t('(filas nuevas)') }}</span></span
        >
        <ChoiceField v-model="header.cloud" class="field-input" :options="clouds" :freetext="false" allow-empty />
      </label>
    </div>
    <div class="toolbar border-t-0" :class="{ 'gap-2 py-2': !headerOpen }">
      <label v-if="headerOpen" class="min-w-64">
        <span class="field-label">{{ $t('Collection_location (cámbialo para añadir mariposas de otro sitio)') }}</span>
        <ChoiceField v-model="header.location" class="field-input" :options="places" />
      </label>
      <label>
        <span class="field-label">{{ $t('Filas a añadir') }}</span>
        <input v-model.number="addCount" type="number" min="1" max="60" class="field-input w-20" />
      </label>
      <label v-if="headerOpen" class="min-w-48">
        <span class="field-label">{{ $t('SPECIES (opcional)') }}</span>
        <ChoiceField v-model="addSpecies" class="field-input" :options="speciesList" :placeholder="$t('la misma para todas')" />
      </label>
      <label>
        <span class="field-label">Release_Collect</span>
        <ChoiceField v-model="addFate" class="field-input" :options="fateChoices" :freetext="false" />
      </label>
      <button class="btn-primary" @click="add">
        <Plus :size="15" /> {{ $tn(Math.max(1, addCount || 1), 'Añadir {n} fila', 'Añadir {n} filas') }}
      </button>
      <div v-if="headerOpen" class="ml-auto flex gap-2">
        <div
          class="rounded-md border border-brand-600 bg-brand-50 px-3 py-1"
          :title="$t('Wild_indv_CAMid de Lists: quedan {n} sin usar', { n: upcoming.camsLeft })"
        >
          <p class="text-xs text-brand-700">{{ $t('Próximo CAM_ID') }}</p>
          <p class="font-mono text-lg font-semibold text-brand-700">{{ upcoming.cam || '—' }}</p>
          <p v-if="upcoming.camsLeft < 50" class="text-xs text-amber-800">{{ $t('quedan {n}', { n: upcoming.camsLeft }) }}</p>
        </div>
        <div
          class="rounded-md border border-stone-300 bg-stone-50 px-3 py-1"
          :title="$t('Tubo de la colecta ({medium})', { medium: header.medium })"
        >
          <p class="text-xs text-stone-600">{{ $t('Próximo tubo') }}</p>
          <p class="font-mono text-lg font-semibold">{{ upcoming.tube || '—' }}</p>
        </div>
        <div
          class="rounded-md border border-stone-300 bg-stone-50 px-3 py-1"
          :title="$t('Para las mariposas que van al insectario')"
        >
          <p class="text-xs text-stone-600">{{ $t('Próximo Insectary ID') }}</p>
          <p class="font-mono text-lg font-semibold">{{ upcoming.insectaryId || '—' }}</p>
        </div>
      </div>
    </div>
    <!-- Live butterflies take the next pre-made Insectary IDs. -->
    <InsectaryIdsWarning class="mx-3 my-2" :revision="tables.tables.Insectary_data?.revision" @extended="loadFreeIds" />

    <div v-if="drafts.length" class="border-b border-stone-200 bg-white px-3 pb-2">
      <!-- Always in sight while scrolling the list, and kept to one line: how long it is, how to trim it, the view. -->
      <div
        data-sticky-bar
        class="sticky top-0 z-10 -mx-3 flex items-center gap-2 border-b border-stone-200 bg-white px-3 py-1.5 text-sm"
      >
        <span class="font-semibold whitespace-nowrap">{{ $tn(drafts.length, '{n} fila', '{n} filas') }}</span>
        <span v-if="emptyCount" class="whitespace-nowrap text-amber-800">{{ $t('{n} vacías', { n: emptyCount }) }}</span>
        <button v-if="emptyCount" class="btn px-2 py-1" :title="$t('Quitar filas vacías')" @click="removeEmpty">
          <Eraser :size="15" /><span class="max-sm:hidden">{{ $t('Quitar vacías') }}</span>
        </button>
        <button class="btn px-2 py-1" :title="$t('Vaciar lista')" @click="clearAll">
          <Trash2 :size="15" /><span class="max-sm:hidden">{{ $t('Vaciar lista') }}</span>
        </button>
        <span class="ml-auto inline-flex shrink-0 overflow-hidden rounded-md border border-stone-300 text-xs">
          <button
            v-for="v in ['tabla', 'formulario'] as const"
            :key="v"
            class="px-2 py-1"
            :class="view === v ? 'bg-brand-700 text-white' : 'bg-white text-stone-700'"
            @click="view = v"
          >
            {{ v === 'tabla' ? $t('Tabla') : $t('Formulario') }}
          </button>
        </span>
      </div>
      <!-- How to use it: scrolls away with the page; folded on phones, where space is short. -->
      <details class="hint mt-1" :open="!touchScreen">
        <summary class="cursor-pointer select-none">{{ $t('Cómo se usa · la lista se guarda en este navegador') }}</summary>
        <p>{{ $t('Se guarda en este navegador, aunque recargues o cierres la página, hasta que la guardes o la vacíes.') }}</p>
        <p v-if="view === 'tabla' && touchScreen">
          {{
            $t(
              'Toca una celda para seleccionarla y dos veces para editarla · arrastra el círculo de la esquina para ampliar la selección · la barra de abajo copia, pega, rellena hacia abajo o borra lo seleccionado.',
            )
          }}
        </p>
        <p v-else-if="view === 'tabla'">
          {{
            $t(
              'Como en una hoja de cálculo: selecciona celdas y arrastra el cuadrito de la esquina hacia abajo para copiarlas (Insectary_ID, CAM_ID y Tube_1_id siguen la serie: O6D → O7D, CAM079895 → CAM079896) · pega celdas de Excel o Sheets (llena hacia abajo y a la derecha, y añade filas si faltan) · Ctrl+D copia la primera fila de la selección · escribe sobre una celda para reemplazarla, doble clic para editarla.',
            )
          }}
        </p>
        <p v-else>
          {{
            $t(
              'Marca filas (o «hasta aquí» para marcar varias seguidas) y aplica especie, sexo o destino a todas a la vez · en computador también se puede pegar desde Excel, Ctrl+D copia la fila de arriba y Enter baja.',
            )
          }}
        </p>
      </details>
      <CollectGrid
        v-if="view === 'tabla'"
        ref="grid"
        class="mt-2"
        :drafts="drafts"
        :places="places"
        :species="speciesList"
        :subspecies-for="subspeciesFor"
        :purposes="options.Purpose || []"
        :mediums="mediums"
        :people="people"
        :rainfalls="rainfalls"
        :clouds="clouds"
        :paste="pasteText"
        :id-problem="idProblem"
        :next-id="nextId"
        :cell-problem="cellProblem"
        @edit="editCell"
        @remove="remove"
        @notice="notify"
      />
      <div v-else class="overflow-x-auto">
        <table class="w-full text-sm">
          <thead class="text-left text-xs text-stone-600">
            <tr>
              <th class="w-8 px-1 py-1">
                <span class="sr-only">{{ $t('Marcar') }}</span>
              </th>
              <th class="px-1 py-1">#</th>
              <th v-for="c in COLUMNS" :key="c" class="px-1">{{ HEADERS[c] }}</th>
              <th></th>
            </tr>
          </thead>
          <tbody>
            <tr
              v-for="(d, i) in drafts"
              :key="d.key"
              :data-draft="d.key"
              class="border-t border-stone-100 align-middle"
              :class="{ 'bg-brand-50': isSelected(d.key) }"
            >
              <td class="px-1">
                <input
                  type="checkbox"
                  class="h-5 w-5 accent-brand-700"
                  :checked="isSelected(d.key)"
                  :aria-label="$t('Marcar fila {n}', { n: i + 1 })"
                  @change="toggleSelect(d.key)"
                />
              </td>
              <td class="px-1 whitespace-nowrap text-stone-500 tabular-nums">
                {{ i + 1 }}
                <button
                  class="btn-ghost align-middle"
                  :aria-label="$t('Copiar fila {n}', { n: i + 1 })"
                  :title="$t('Copiar esta fila (para aplicarla a las filas que marques)')"
                  @click="copyRow(d, i + 1)"
                >
                  <Copy :size="15" />
                </button>
                <button
                  v-if="selected.length && !isSelected(d.key)"
                  class="ml-1 rounded bg-stone-100 px-1.5 py-0.5 text-xs whitespace-nowrap text-brand-700"
                  @click="selectTo(d.key)"
                >
                  {{ $t('hasta aquí') }}
                </button>
              </td>
              <td class="px-1">
                <ChoiceField
                  v-model="d.location"
                  data-col="location"
                  :class="{ 'border-red-500 bg-red-50': cellProblem(d, 'location') }"
                  :title="cellProblem(d, 'location') || undefined"
                  class="field-input w-44"
                  :options="places"
                  @paste="onPaste($event, i, 'location')"
                  @keydown="onCellKey($event, i, 'location')"
                />
              </td>
              <td class="px-1">
                <ChoiceField
                  v-model="d.species"
                  data-col="species"
                  :class="{ 'border-red-500 bg-red-50': cellProblem(d, 'species') }"
                  :title="cellProblem(d, 'species') || undefined"
                  class="field-input w-48"
                  :options="speciesList"
                  @paste="onPaste($event, i, 'species')"
                  @keydown="onCellKey($event, i, 'species')"
                />
              </td>
              <td class="px-1">
                <ChoiceField
                  v-model="d.subspecies"
                  data-col="subspecies"
                  class="field-input w-36"
                  :options="subspeciesFor(d.species)"
                  placeholder="—"
                  @paste="onPaste($event, i, 'subspecies')"
                  @keydown="onCellKey($event, i, 'subspecies')"
                />
              </td>
              <td class="px-1 whitespace-nowrap">
                <button
                  v-for="value in SEX_VALUES"
                  :key="value"
                  type="button"
                  class="mr-0.5 h-8 rounded border px-2 text-sm"
                  :class="d.sex === value ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white'"
                  :aria-label="value"
                  @click="d.sex = value"
                >
                  {{ value }}
                </button>
              </td>
              <td class="px-1">
                <ChoiceField
                  :model-value="d.fate"
                  data-col="fate"
                  class="field-input w-56"
                  :options="fateChoices"
                  :freetext="false"
                  @paste="onPaste($event, i, 'fate')"
                  @update:model-value="setFate(d, $event as Fate)"
                />
              </td>
              <td class="px-1">
                <input
                  v-model="d.time"
                  data-col="time"
                  class="field-input w-20"
                  placeholder="hh:mm"
                  @paste="onPaste($event, i, 'time')"
                  @keydown="onCellKey($event, i, 'time')"
                  @change="d.time = parseTime(d.time)"
                />
              </td>
              <td class="px-1">
                <input
                  v-if="d.fate === 'insectario'"
                  :value="d.insectaryId"
                  data-col="insectaryId"
                  class="field-input w-24 font-semibold tracking-wide text-brand-800"
                  :class="{ 'border-amber-500 bg-amber-50': idProblem(d) }"
                  :title="idProblem(d) || $t('El ID escrito en las alas (se sugiere el siguiente libre)')"
                  @change="setColumn(d, 'insectaryId', ($event.target as HTMLInputElement).value)"
                  @paste="onPaste($event, i, 'insectaryId')"
                />
              </td>
              <template v-if="d.fate === 'preservada'">
                <td v-for="c in ['cam', 'tube'] as const" :key="c" class="px-1">
                  <input
                    :value="d[c]"
                    :data-col="c"
                    class="field-input w-32"
                    :class="{ 'border-red-500 bg-red-50': idProblem(d, c) }"
                    :title="idProblem(d, c) || undefined"
                    @change="setColumn(d, c, ($event.target as HTMLInputElement).value)"
                    @paste="onPaste($event, i, c)"
                  />
                </td>
                <td class="px-1">
                  <ChoiceField
                    :model-value="d.medium"
                    data-col="medium"
                    class="field-input w-48"
                    :options="mediums"
                    :freetext="false"
                    @update:model-value="setColumn(d, 'medium', $event)"
                  />
                </td>
              </template>
              <td v-else colspan="3"></td>
              <td class="px-1">
                <ChoiceField
                  v-model="d.purpose"
                  data-col="purpose"
                  :class="{ 'border-red-500 bg-red-50': cellProblem(d, 'purpose') }"
                  :title="cellProblem(d, 'purpose') || undefined"
                  class="field-input w-28"
                  :options="options.Purpose || []"
                  @paste="onPaste($event, i, 'purpose')"
                  @keydown="onCellKey($event, i, 'purpose')"
                />
              </td>
              <td class="px-1">
                <input
                  v-model="d.notes"
                  data-col="notes"
                  class="field-input w-48"
                  @paste="onPaste($event, i, 'notes')"
                  @keydown="onCellKey($event, i, 'notes')"
                />
              </td>
              <td v-for="c in ['collector', 'identifier'] as const" :key="c" class="px-1">
                <ChoiceField
                  :model-value="d[c]"
                  :data-col="c"
                  class="field-input w-48"
                  :options="people"
                  @update:model-value="setColumn(d, c, $event)"
                />
              </td>
              <td v-for="c in ['rainfall', 'cloud'] as const" :key="c" class="px-1">
                <ChoiceField
                  :model-value="d[c]"
                  :data-col="c"
                  class="field-input w-48"
                  :options="c === 'rainfall' ? rainfalls : clouds"
                  :freetext="false"
                  allow-empty
                  @update:model-value="setColumn(d, c, $event)"
                />
              </td>
              <td class="px-1 whitespace-nowrap">
                <button class="btn-ghost" :title="$t('Quitar')" @click="remove(d.key)"><Trash2 :size="14" /></button>
              </td>
            </tr>
          </tbody>
        </table>
      </div>
      <!-- Phones: what to apply to the rows ticked, pinned at the bottom of the screen. -->
      <div
        v-if="view === 'formulario' && selected.length"
        class="sticky bottom-0 z-10 -mx-3 mt-2 flex flex-wrap items-end gap-2 border-t border-brand-700 bg-brand-50 px-3 py-2 text-sm"
      >
        <span class="w-full font-semibold text-brand-800">{{
          $tn(selected.length, '{n} fila marcada: aplicar lo que llenes', '{n} filas marcadas: aplicar lo que llenes')
        }}</span>
        <label class="min-w-40 flex-1">
          <span class="field-label">SPECIES</span>
          <ChoiceField v-model="bulk.species" class="field-input" :options="speciesList" />
        </label>
        <label class="min-w-32 flex-1">
          <span class="field-label">Subspecies_Form</span>
          <ChoiceField v-model="bulk.subspecies" class="field-input" :options="subspeciesFor(bulk.species)" />
        </label>
        <span class="whitespace-nowrap">
          <button
            v-for="value in SEX_VALUES"
            :key="value"
            type="button"
            class="mr-0.5 h-9 rounded border px-2 text-sm"
            :class="bulk.sex === value ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white'"
            :aria-label="`Sex ${value}`"
            @click="bulk.sex = bulk.sex === value ? '' : value"
          >
            {{ value }}
          </button>
        </span>
        <ChoiceField
          v-model="bulk.fate"
          class="field-input w-52"
          aria-label="Release_Collect"
          placeholder="Release_Collect…"
          :options="fateChoices"
          :freetext="false"
          allow-empty
        />
        <button class="btn-primary" @click="applyBulk"><CheckSquare :size="15" /> {{ $t('Aplicar') }}</button>
        <button class="btn" @click="removeSelected"><Trash2 :size="15" /> {{ $t('Quitar') }}</button>
        <button class="btn" @click="selected = []"><X :size="15" /> {{ $t('Desmarcar') }}</button>
      </div>
      <div class="mt-2 flex flex-wrap items-center gap-3 text-sm">
        <span v-for="[place, g] in groups" :key="place" class="rounded bg-stone-100 px-2 py-0.5">
          {{ place }}: {{ g.insectario }} Collected_Sent2Insectary · {{ g.preservada }} Collected_Preserved<template
            v-if="g.liberada"
          >
            · {{ g.liberada }} Released_Unmarked</template
          >
        </span>
        <span v-if="problems.length" class="text-xs text-amber-800"
          >{{ problems[0]
          }}<template v-if="problems.length > 1"> {{ $t('(y {n} más)', { n: problems.length - 1 }) }}</template></span
        >
        <span v-if="earlierIds.length" class="w-full text-xs text-amber-800">
          {{
            $t(
              'Ya no quedan filas preasignadas al final de Insectary_data: {ids} son filas vacías anteriores. Comprueba que ningún ID esté ya escrito en otra mariposa, o crea más filas preasignadas en Insectary_data.',
              { ids: earlierIds.slice(0, 4).join(', ') + (earlierIds.length > 4 ? '…' : '') },
            )
          }}
        </span>
        <button v-if="emptyCount" class="btn" @click="removeEmpty"><Eraser :size="15" /> {{ $t('Quitar filas vacías') }}</button>
        <button class="btn-primary ml-auto" :disabled="saving || !!problems.length" @click="askSave">
          <Save :size="15" /> {{ saving ? $t('Guardando…') : $t('Guardar colecta ({n})', { n: drafts.length }) }}
        </button>
      </div>
      <p class="hint mt-1">
        {{
          $t(
            'Las que van al insectario reciben el Insectary ID para escribir en las alas, sin CAM ID (se da al hacer el wing clip o al preservar). Se guardan en Collection_data e Insectary_data a la vez.',
          )
        }}
      </p>
    </div>

    <p class="hint px-4 py-1">
      {{ $t('Últimos {n} registros de Collection_data.', { n: recentCount }) }}
      <button class="underline" @click="recentCount += 20">{{ $t('ver más') }}</button>
    </p>
    <div class="min-h-80 flex-1">
      <p v-if="!ready" class="p-6 text-stone-500">{{ $t('Cargando {sheet}…', { sheet: 'Collection_data' }) }}</p>
      <SheetGrid
        v-else
        :module="MODULE"
        :rows="recent"
        :creates="creates"
        :columns="columns"
        :options="options"
        :frozen="['Insectary_ID']"
        :create-formulas="createFormulas"
        label-field="Insectary_ID"
        @notice="notify"
        @remove-create="
          id => {
            pending.removeCreate(id)
            pending.touch()
          }
        "
      />
    </div>

    <div v-if="confirming" class="fixed inset-0 z-40 grid place-items-center bg-black/40 p-2" @click.self="confirming = false">
      <section
        class="flex max-h-[90vh] w-full max-w-lg flex-col rounded-lg bg-white shadow-xl"
        role="dialog"
        :aria-label="$t('Guardar colecta')"
      >
        <header class="flex items-center border-b border-stone-200 px-4 py-3">
          <h2 class="flex-1 text-lg font-semibold">{{ $t('Guardar {n} mariposas', { n: drafts.length }) }}</h2>
          <button class="btn-ghost" :aria-label="$t('Cerrar')" @click="confirming = false"><X :size="20" /></button>
        </header>
        <div class="flex-1 space-y-2 overflow-y-auto px-4 py-3 text-sm">
          <p class="text-base">
            <strong class="capitalize">{{ longDate }}</strong> · {{ summary.places.join(', ') }}
          </p>
          <p v-if="isToday" class="rounded bg-amber-50 px-2 py-1 text-amber-800">
            {{ $t('La fecha es hoy. Si pasas a limpio una colecta de otro día, cambia Collection_date antes de guardar.') }}
          </p>
          <p>
            {{ $t('Al insectario:') }} <strong>{{ summary.insectary.female }} ♀ · {{ summary.insectary.male }} ♂</strong
            ><template v-if="summary.insectary.other"> · {{ $t('{n} sin sexo', { n: summary.insectary.other }) }}</template>
            <br />
            {{ $t('Preservadas:') }} <strong>{{ summary.preserved }}</strong
            ><template v-if="summary.cams">
              ({{ summary.cams.first
              }}<template v-if="summary.cams.first !== summary.cams.last"> – {{ summary.cams.last }}</template
              ><template v-if="!summary.cams.consecutive">, {{ $t('con saltos') }}</template
              >)</template
            >
            <template v-if="summary.released"
              ><br />{{ $t('Liberadas:') }} <strong>{{ summary.released }}</strong></template
            >
          </p>
          <table class="w-full">
            <thead class="text-left text-xs text-stone-500">
              <tr>
                <th class="py-1">{{ $t('Especie') }}</th>
                <th class="w-10 text-right">♀</th>
                <th class="w-10 text-right">♂</th>
                <th class="w-10 text-right">?</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="s in summary.species" :key="s.name" class="border-t border-stone-100">
                <td class="py-1 pr-2">{{ s.name }}</td>
                <td class="text-right tabular-nums">{{ s.female || '' }}</td>
                <td class="text-right tabular-nums">{{ s.male || '' }}</td>
                <td class="text-right tabular-nums">{{ s.other || '' }}</td>
              </tr>
            </tbody>
          </table>
          <p class="text-stone-600">
            Collector: {{ distinct('collector') }} · Identifier: {{ distinct('identifier') }} · Rainfall:
            {{ distinct('rainfall') }} · Cloud_cover: {{ distinct('cloud') }}
          </p>
        </div>
        <footer class="flex justify-end gap-2 border-t border-stone-200 px-4 py-3">
          <button class="btn" @click="confirming = false">{{ $t('Volver') }}</button>
          <button class="btn-primary" :disabled="saving" @click="confirmSave">
            <Save :size="15" /> {{ $t('Guardar en la hoja') }}
          </button>
        </footer>
      </section>
    </div>
  </div>
</template>
