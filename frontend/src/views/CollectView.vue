<script setup lang="ts">
import { computed, nextTick, reactive, ref, watch } from 'vue'
import { CheckSquare, Copy, Eraser, Plus, Save, Trash2, X } from 'lucide-vue-next'
import CollectGrid from '../components/CollectGrid.vue'
import SheetGrid from '../components/SheetGrid.vue'
import { useSheet } from '../composables/useSheet'
import { api } from '../lib/api'
import { isBlank } from '../lib/cells'
import { isoToSerial, todayIso } from '../lib/dates'
import { errorText, notify } from '../lib/notice'
import { listColumn } from '../lib/options'
import { listProblem, verificationsFor } from '../lib/verifications'
import { COLUMNS, FATES, HEADERS, SEX_VALUES, applies, insectarySex, type Column, type Draft, type Fate } from '../lib/collect'
import { parseBlock, parseCamTube, parseFate, parseSex, parseTime } from '../lib/paste'
import { persistentRef } from '../lib/persist'
import { orderColumns } from '../lib/rows'
import type { CellValue } from '../lib/types'
import { usePending } from '../stores/pending'
import { useSession } from '../stores/session'
import { useTables } from '../stores/tables'

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
const drafts = persistentRef<Draft[]>(`collect:drafts:${useSession().user?.username}`, [], { lasting: true })
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
const subspeciesFor = (species: string) => ranked('Subspecies_Form', v => v.SPECIES === species)
const places = computed(() => [...new Set([...ranked('Collection_location'), ...(options.value.Collection_location || [])])])
const people = computed(() => options.value.Collector || ranked('Collector'))
const rainfalls = computed(() => options.value.Rainfall || ranked('Rainfall'))
const clouds = computed(() => options.value.Cloud_cover || ranked('Cloud_cover'))
const mediums = computed(() => options.value.Preservation_medium || ['Flash frozen'])

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
  for (const r of observed.value) if (!isBlank(r.values.Insectary_ID)) out.set(String(r.values.Insectary_ID).trim().toUpperCase(), r.row)
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
        if (!isBlank(row.values[key]) && String(row.values[key]).trim() !== 'NA') used.set(String(row.values[key]).trim().toUpperCase(), `${sheet} fila ${row.row}`)
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

function setFate(draft: Draft, fate: Fate) {
  draft.fate = fate
  draft.insectaryId = fate === 'insectario' ? draft.insectaryId || nextInsectaryId() : ''
  draft.cam = fate === 'preservada' ? draft.cam || nextCam() : ''
  draft.tube = fate === 'preservada' ? draft.tube || nextTube() : ''
  draft.medium = fate === 'preservada' ? draft.medium || header.value.medium : ''
}
// Rows added before the free IDs, CAMs or tubes had arrived get them once they do.
watch([freeIds, camPool, tubeRun], () => {
  for (const d of drafts.value) {
    if (d.fate === 'insectario' && !d.insectaryId) d.insectaryId = nextInsectaryId()
    if (d.fate === 'preservada' && !d.cam) d.cam = nextCam()
    if (d.fate === 'preservada' && !d.tube) d.tube = nextTube()
    // Lists kept from before the medium was a column of its own.
    if (d.fate === 'preservada' && !d.medium) d.medium = header.value.medium
  }
})
async function add() {
  if (!header.value.location) return notify('Elige el lugar de colecta')
  const count = Math.min(60, Math.max(1, Math.round(addCount.value || 1)))
  const first = drafts.value.length
  for (let i = 0; i < count; i++) {
    drafts.value.push({ ...blankDraft(), species: addSpecies.value.trim() })
    setFate(drafts.value.at(-1)!, addFate.value)
  }
  notify(`Se añadieron ${count} ${count === 1 ? 'fila' : 'filas'}: la lista tiene ${drafts.value.length}`)
  // Straight to the first new row, ready to type its species.
  await nextTick()
  if (view.value === 'tabla') return void grid.value?.focusCell(first, 'species')
  const row = document.querySelector<HTMLElement>(`[data-draft="${drafts.value[first]?.key}"]`)
  row?.scrollIntoView({ block: 'center', behavior: 'smooth' })
  row?.querySelector<HTMLInputElement>('input[list=collect-species]')?.focus({ preventScroll: true })
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
  notify(`Se quitaron ${n} filas vacías: quedan ${drafts.value.length}`)
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
  }
}
function setColumn(d: Draft, column: Column, text: string) {
  if (column === 'sex') d.sex = parseSex(text)
  else if (column === 'fate') {
    const fate = parseFate(text)
    if (fate) setFate(d, fate)
  } else if (column === 'time') d.time = parseTime(text)
  else if (!applies(d, column)) return
  else if (column === 'insectaryId') {
    // The ID written on the wings, if it is not the one suggested (checked in `problems`).
    const id = text.trim().toUpperCase()
    if (/^[0-9A-ZÑ]{2,6}$/.test(id)) d.insectaryId = id
  } else if (column === 'cam') {
    // "CAM079895", or CAM and tube together ("CAM079895 · FS90415305 (Flash frozen)").
    const { cam, tube } = parseCamTube(text)
    if (cam || !text.trim()) d.cam = cam
    if (tube) d.tube = tube
    const medium = mediums.value.find(m => text.includes(`(${m})`))
    if (medium) d.medium = medium
  } else if (column === 'tube') d.tube = text.trim().toUpperCase()
  else if (column === 'medium') {
    d.medium = text.trim()
    // The next rows added take the same medium (and its tubes).
    if (d.medium) header.value.medium = d.medium
  } else d[column] = text === 'NA' && column === 'subspecies' ? '' : text
}
/**
 * A block copied from a spreadsheet, pasted at a row and column: fills down and
 * across, adding rows if needed. False when the text is a single value (the
 * cell takes it as typed).
 */
function pasteText(text: string, index: number, column: Column): boolean {
  const block = parseBlock(text)
  if (!block) return false
  const start = COLUMNS.indexOf(column)
  let added = 0
  block.forEach((cells, r) => {
    if (!drafts.value[index + r]) {
      drafts.value.push(blankDraft())
      setFate(drafts.value.at(-1)!, addFate.value)
      added++
    }
    const d = drafts.value[index + r]
    cells.forEach((text, c) => {
      const target = COLUMNS[start + c]
      if (target) setColumn(d, target, text)
    })
  })
  notify(`Pegadas ${block.length} filas${added ? ` (${added} nuevas)` : ''}: la lista tiene ${drafts.value.length}`)
  return true
}
function onPaste(event: ClipboardEvent, index: number, column: Column) {
  if (pasteText(event.clipboardData?.getData('text/plain') || '', index, column)) event.preventDefault()
}
/** A cell edited in the grid (typed, pasted as one value, filled by dragging or Ctrl+D). */
function editCell(key: string, column: Column, text: string) {
  const d = drafts.value.find(x => x.key === key)
  if (!d) return
  if (column === 'insectaryId' && text.trim() && !/^[0-9A-ZÑ]{2,6}$/i.test(text.trim()))
    return notify(`«${text.trim()}» no parece un Insectary ID (p. ej. N9D); para CAM y tubo usa CAM_ID y Tube_1_id`)
  setColumn(d, column, text)
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
  notify(`Aplicado a ${rows.length} ${rows.length === 1 ? 'fila' : 'filas'}`)
}
/** Touch-screen copy: a row's species, subspecies, sex and fate go to the bar, to apply to the rows ticked. */
function copyRow(d: Draft, n: number) {
  Object.assign(bulk, { species: d.species, subspecies: d.subspecies, sex: d.sex, fate: d.fate })
  selected.value = selected.value.filter(k => k !== d.key)
  anchor.value = d.key
  notify(`Copiada la fila ${n}: marca las filas donde pegarla y pulsa Aplicar`)
}
function removeSelected() {
  drafts.value = drafts.value.filter(d => !isSelected(d.key))
  selected.value = []
}
function clearAll() {
  const filled = drafts.value.length - emptyCount.value
  if (filled && !confirm(`¿Vaciar la lista? Se pierden ${filled} filas con datos sin guardar.`)) return
  drafts.value = []
  notify('Lista vaciada')
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
    if (!isBlank(r.values.Insectary_ID)) out.set(String(r.values.Insectary_ID).trim().toUpperCase(), { row: r.row, observed: r.observed })
  return out
})
/** An Insectary ID, CAM or tube repeated in the list, or already used in the sheets. */
function idProblem(d: Draft, column: Column = 'insectaryId'): string | null {
  if (column === 'cam' || column === 'tube') {
    const value = d[column].trim().toUpperCase()
    if (d.fate !== 'preservada' || !value) return null
    if (drafts.value.filter(x => x.fate === 'preservada' && x[column].trim().toUpperCase() === value).length > 1)
      return `${value} está repetido en la lista`
    if (column === 'cam' && usedCams.value.has(value)) return `${value} ya está usado en las hojas`
    const used = column === 'tube' && usedTubes.value.get(value)
    return used ? `${value} ya está usado (${used})` : null
  }
  if (d.fate !== 'insectario' || !d.insectaryId) return null
  if (drafts.value.filter(x => x.fate === 'insectario' && x.insectaryId === d.insectaryId).length > 1)
    return `${d.insectaryId} está repetido en la lista`
  if (!idRows.value.size) return null
  const row = idRows.value.get(d.insectaryId)
  if (!row) return `${d.insectaryId} no tiene fila preparada en Insectary_data`
  if (row.observed) return `${d.insectaryId} ya está registrado (Insectary_data fila ${row.row})`
  const collected = collectedIds.value.get(d.insectaryId)
  if (collected) return `${d.insectaryId} ya está en Collection_data (fila ${collected})`
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
}
function cellProblem(d: Draft, column: Column): string | null {
  const field = LISTED[column]
  return field ? listProblem(collectionRules.value, field, d[column as 'species']) : null
}
const problems = computed(() => [
  ...(emptyCount.value ? [`${emptyCount.value} filas vacías`] : []),
  ...drafts.value.flatMap((d, i) => {
    if (isEmpty(d)) return []
    const n = i + 1
    const out: string[] = []
    if (!d.species) out.push(`fila ${n}: falta la especie`)
    if (!d.sex) out.push(`fila ${n}: falta el sexo`)
    if (d.fate === 'insectario' && !d.insectaryId)
      out.push(`fila ${n}: no quedan Insectary IDs libres; crea más filas preasignadas en Insectary_data`)
    for (const column of ['insectaryId', 'cam', 'tube'] as const) {
      const idIssue = idProblem(d, column)
      if (idIssue) out.push(`fila ${n}: ${HEADERS[column]} ${idIssue}`)
    }
    for (const column of Object.keys(LISTED) as Column[]) {
      const issue = cellProblem(d, column)
      if (issue) out.push(`fila ${n}: ${LISTED[column]} ${issue}`)
    }
    if (d.fate === 'preservada' && (!d.cam || !d.tube || !d.medium)) out.push(`fila ${n}: falta CAM_ID, Tube_1_id o Preservation_medium`)
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
    Identifier: header.value.identifier || null,
    ID_status: 'COMPLETE',
    Sex: d.sex,
    Collection_location: d.location,
    Transect_section: 'NA',
    Bait: 'NA',
    Forest_stratum: 'NA',
    Collection_date: date,
    Collection_time: dayFraction(d.time),
    Collector: header.value.collector || null,
    Rainfall: header.value.rainfall || null,
    Cloud_cover: header.value.cloud || null,
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

async function save() {
  if (!drafts.value.length) return
  if (problems.value.length) return notify(problems.value.slice(0, 3).join('; '), 'error')
  saving.value = true
  const added: string[] = []
  for (const d of drafts.value) {
    added.push(pending.addCreate(MODULE, d.insectaryId || d.cam || 'nuevo', collectionRow(d)).clientId)
    if (d.fate === 'insectario') added.push(pending.addCreate('Insectary_data', d.insectaryId, insectaryRow(d)).clientId)
  }
  pending.touch()
  try {
    await pending.save(`Colecta ${header.value.date}`)
    if (added.some(id => pending.creates.some(c => c.clientId === id))) throw new Error('Revisa los errores marcados en la tabla')
    notify(`Colecta guardada: ${drafts.value.length} mariposas`, 'success')
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
    ? orderColumns(table.value.columns, [
        'Collection_date',
        'Collection_location',
        'Release_Collect',
        'Insectary_ID',
        'CAM_ID',
        'Tube_1_id',
        'SPECIES',
        'Subspecies_Form',
        'Sex',
        'Collector',
        'Identifier',
        'Notes_Collection_data',
      ])
    : [],
)
const recent = computed(() => observed.value.slice(-recentCount.value))
</script>

<template>
  <div class="flex h-full flex-col overflow-y-auto">
    <div class="toolbar">
      <label>
        <span class="field-label">Collection_date</span>
        <input v-model="header.date" type="date" class="field-input" />
      </label>
      <label class="min-w-52">
        <span class="field-label">Collector</span>
        <input v-model="header.collector" class="field-input" list="collect-people" />
      </label>
      <label class="min-w-52">
        <span class="field-label">Identifier</span>
        <input v-model="header.identifier" class="field-input" list="collect-people" />
      </label>
      <label>
        <span class="field-label">Rainfall</span>
        <select v-model="header.rainfall" class="field-input">
          <option value="">—</option>
          <option v-for="r in rainfalls" :key="r" :value="r">{{ r }}</option>
        </select>
      </label>
      <label>
        <span class="field-label">Cloud_cover</span>
        <select v-model="header.cloud" class="field-input">
          <option value="">—</option>
          <option v-for="c in clouds" :key="c" :value="c">{{ c }}</option>
        </select>
      </label>
      <datalist id="collect-people">
        <option v-for="p in people" :key="p" :value="p" />
      </datalist>
      <datalist id="collect-species">
        <option v-for="s in speciesList" :key="s" :value="s" />
      </datalist>
    </div>
    <div class="toolbar border-t-0">
      <label class="min-w-64">
        <span class="field-label">Collection_location (cámbialo para añadir mariposas de otro sitio)</span>
        <input v-model="header.location" class="field-input" list="collect-places" />
        <datalist id="collect-places">
          <option v-for="p in places" :key="p" :value="p" />
        </datalist>
      </label>
      <label>
        <span class="field-label">Filas a añadir</span>
        <input v-model.number="addCount" type="number" min="1" max="60" class="field-input w-20" />
      </label>
      <label class="min-w-48">
        <span class="field-label">SPECIES (opcional)</span>
        <input v-model="addSpecies" class="field-input" list="collect-species" placeholder="la misma para todas" />
      </label>
      <label>
        <span class="field-label">Release_Collect</span>
        <select v-model="addFate" class="field-input">
          <option v-for="(f, key) in FATES" :key="key" :value="key">{{ f.label }}</option>
        </select>
      </label>
      <button class="btn-primary" @click="add">
        <Plus :size="15" /> Añadir {{ Math.max(1, addCount || 1) }} {{ Math.max(1, addCount || 1) === 1 ? 'fila' : 'filas' }}
      </button>
    </div>

    <div v-if="drafts.length" class="border-b border-stone-200 bg-white px-3 pb-2">
      <!-- Always in sight while scrolling the list, and kept to one line: how long it is, how to trim it, the view. -->
      <div
        data-sticky-bar
        class="sticky top-0 z-10 -mx-3 flex items-center gap-2 border-b border-stone-200 bg-white px-3 py-1.5 text-sm"
      >
        <span class="font-semibold whitespace-nowrap">{{ drafts.length }} {{ drafts.length === 1 ? 'fila' : 'filas' }}</span>
        <span v-if="emptyCount" class="whitespace-nowrap text-amber-800">{{ emptyCount }} vacías</span>
        <button v-if="emptyCount" class="btn px-2 py-1" title="Quitar filas vacías" @click="removeEmpty">
          <Eraser :size="15" /><span class="max-sm:hidden">Quitar vacías</span>
        </button>
        <button class="btn px-2 py-1" title="Vaciar lista" @click="clearAll">
          <Trash2 :size="15" /><span class="max-sm:hidden">Vaciar lista</span>
        </button>
        <span class="ml-auto inline-flex shrink-0 overflow-hidden rounded-md border border-stone-300 text-xs">
          <button
            v-for="v in ['tabla', 'formulario'] as const"
            :key="v"
            class="px-2 py-1"
            :class="view === v ? 'bg-brand-700 text-white' : 'bg-white text-stone-700'"
            @click="view = v"
          >
            {{ v === 'tabla' ? 'Tabla' : 'Formulario' }}
          </button>
        </span>
      </div>
      <!-- How to use it: scrolls away with the page; folded on phones, where space is short. -->
      <details class="hint mt-1" :open="!touchScreen">
        <summary class="cursor-pointer select-none">Cómo se usa · la lista se guarda en este navegador</summary>
        <p>Se guarda en este navegador, aunque recargues o cierres la página, hasta que la guardes o la vacíes.</p>
        <p v-if="view === 'tabla' && touchScreen">
          Toca una celda para seleccionarla y dos veces para editarla · arrastra el círculo de la esquina para ampliar la selección ·
          la barra de abajo copia, pega, rellena hacia abajo o borra lo seleccionado.
        </p>
        <p v-else-if="view === 'tabla'">
          Como en una hoja de cálculo: selecciona celdas y arrastra el cuadrito de la esquina hacia abajo para copiarlas (Insectary_ID, CAM_ID y
          Tube_1_id siguen la serie: O6D → O7D, CAM079895 → CAM079896) · pega
          celdas de Excel o Sheets (llena hacia abajo y a la derecha, y añade filas si faltan) · Ctrl+D copia la primera fila de la
          selección · escribe sobre una celda para reemplazarla, doble clic para editarla.
        </p>
        <p v-else>
          Marca filas (o «hasta aquí» para marcar varias seguidas) y aplica especie, sexo o destino a todas a la vez · en computador
          también se puede pegar desde Excel, Ctrl+D copia la fila de arriba y Enter baja.
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
              <th class="w-8 px-1 py-1"><span class="sr-only">Marcar</span></th>
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
                  :aria-label="`Marcar fila ${i + 1}`"
                  @change="toggleSelect(d.key)"
                />
              </td>
              <td class="px-1 whitespace-nowrap text-stone-500 tabular-nums">
                {{ i + 1 }}
                <button
                  class="btn-ghost align-middle"
                  :aria-label="`Copiar fila ${i + 1}`"
                  title="Copiar esta fila (para aplicarla a las filas que marques)"
                  @click="copyRow(d, i + 1)"
                >
                  <Copy :size="15" />
                </button>
                <button
                  v-if="selected.length && !isSelected(d.key)"
                  class="ml-1 rounded bg-stone-100 px-1.5 py-0.5 text-xs whitespace-nowrap text-brand-700"
                  @click="selectTo(d.key)"
                >
                  hasta aquí
                </button>
              </td>
              <td class="px-1">
                <input
                  v-model="d.location"
                  data-col="location"
                  :class="{ 'border-red-500 bg-red-50': cellProblem(d, 'location') }"
                  :title="cellProblem(d, 'location') || undefined"
                  class="field-input w-44"
                  list="collect-places"
                  @paste="onPaste($event, i, 'location')"
                  @keydown="onCellKey($event, i, 'location')"
                />
              </td>
              <td class="px-1">
                <input
                  v-model="d.species"
                  data-col="species"
                  :class="{ 'border-red-500 bg-red-50': cellProblem(d, 'species') }"
                  :title="cellProblem(d, 'species') || undefined"
                  class="field-input w-48"
                  list="collect-species"
                  @paste="onPaste($event, i, 'species')"
                  @keydown="onCellKey($event, i, 'species')"
                />
              </td>
              <td class="px-1">
                <input
                  v-model="d.subspecies"
                  data-col="subspecies"
                  class="field-input w-36"
                  :list="`collect-sub-${d.key}`"
                  placeholder="—"
                  @paste="onPaste($event, i, 'subspecies')"
                  @keydown="onCellKey($event, i, 'subspecies')"
                />
                <datalist :id="`collect-sub-${d.key}`">
                  <option v-for="sub in subspeciesFor(d.species)" :key="sub" :value="sub" />
                </datalist>
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
                <select
                  :value="d.fate"
                  data-col="fate"
                  class="field-input w-56"
                  @paste="onPaste($event, i, 'fate')"
                  @change="setFate(d, ($event.target as HTMLSelectElement).value as Fate)"
                >
                  <option v-for="(f, key) in FATES" :key="key" :value="key">{{ f.label }}</option>
                </select>
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
                  :title="idProblem(d) || 'El ID escrito en las alas (se sugiere el siguiente libre)'"
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
                  <select
                    :value="d.medium"
                    data-col="medium"
                    class="field-input w-48"
                    @change="setColumn(d, 'medium', ($event.target as HTMLSelectElement).value)"
                  >
                    <option v-for="m in mediums" :key="m" :value="m">{{ m }}</option>
                  </select>
                </td>
              </template>
              <td v-else colspan="3"></td>
              <td class="px-1">
                <input
                  v-model="d.purpose"
                  data-col="purpose"
                  :class="{ 'border-red-500 bg-red-50': cellProblem(d, 'purpose') }"
                  :title="cellProblem(d, 'purpose') || undefined"
                  class="field-input w-28"
                  list="collect-purposes"
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
              <td class="px-1 whitespace-nowrap">
                <button class="btn-ghost" title="Quitar" @click="remove(d.key)"><Trash2 :size="14" /></button>
              </td>
            </tr>
          </tbody>
        </table>
        <datalist id="collect-purposes">
          <option v-for="p in options.Purpose || []" :key="p" :value="p" />
        </datalist>
      </div>
      <!-- Phones: what to apply to the rows ticked, pinned at the bottom of the screen. -->
      <div
        v-if="view === 'formulario' && selected.length"
        class="sticky bottom-0 z-10 -mx-3 mt-2 flex flex-wrap items-end gap-2 border-t border-brand-700 bg-brand-50 px-3 py-2 text-sm"
      >
        <span class="w-full font-semibold text-brand-800"
          >{{ selected.length }} {{ selected.length === 1 ? 'fila marcada' : 'filas marcadas' }}: aplicar lo que llenes</span
        >
        <label class="min-w-40 flex-1">
          <span class="field-label">SPECIES</span>
          <input v-model="bulk.species" class="field-input" list="collect-species" />
        </label>
        <label class="min-w-32 flex-1">
          <span class="field-label">Subspecies_Form</span>
          <input v-model="bulk.subspecies" class="field-input" list="collect-bulk-sub" />
          <datalist id="collect-bulk-sub">
            <option v-for="sub in subspeciesFor(bulk.species)" :key="sub" :value="sub" />
          </datalist>
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
        <select v-model="bulk.fate" class="field-input w-52" aria-label="Release_Collect">
          <option value="">Release_Collect…</option>
          <option v-for="(f, key) in FATES" :key="key" :value="key">{{ f.label }}</option>
        </select>
        <button class="btn-primary" @click="applyBulk"><CheckSquare :size="15" /> Aplicar</button>
        <button class="btn" @click="removeSelected"><Trash2 :size="15" /> Quitar</button>
        <button class="btn" @click="selected = []"><X :size="15" /> Desmarcar</button>
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
          >{{ problems[0] }}<template v-if="problems.length > 1"> (y {{ problems.length - 1 }} más)</template></span
        >
        <span v-if="earlierIds.length" class="w-full text-xs text-amber-800">
          Ya no quedan filas preasignadas al final de Insectary_data: {{ earlierIds.slice(0, 4).join(', ')
          }}{{ earlierIds.length > 4 ? '…' : '' }} son filas vacías anteriores. Comprueba que ningún ID esté ya escrito en otra
          mariposa, o crea más filas preasignadas en Insectary_data.
        </span>
        <button v-if="emptyCount" class="btn" @click="removeEmpty"><Eraser :size="15" /> Quitar filas vacías</button>
        <button class="btn-primary ml-auto" :disabled="saving || !!problems.length" @click="save">
          <Save :size="15" /> {{ saving ? 'Guardando…' : `Guardar colecta (${drafts.length})` }}
        </button>
      </div>
      <p class="hint mt-1">
        Las que van al insectario reciben el Insectary ID para escribir en las alas, sin CAM ID (se da al hacer el wing clip o al
        preservar). Se guardan en Collection_data e Insectary_data a la vez.
      </p>
    </div>

    <p class="hint px-4 py-1">
      Últimos {{ recentCount }} registros de Collection_data.
      <button class="underline" @click="recentCount += 20">ver más</button>
    </p>
    <div class="min-h-80 flex-1">
      <p v-if="!ready" class="p-6 text-stone-500">Cargando Collection_data…</p>
      <SheetGrid
        v-else
        :module="MODULE"
        :rows="recent"
        :creates="creates"
        :columns="columns"
        :options="options"
        :frozen="['Collection_date']"
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
  </div>
</template>
