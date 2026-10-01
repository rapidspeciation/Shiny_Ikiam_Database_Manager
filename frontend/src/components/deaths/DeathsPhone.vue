<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, reactive, ref, toRaw, watch } from 'vue'
import { Check, Loader2, Search, Undo2, X } from 'lucide-vue-next'
import DateField from '../DateField.vue'
import RowDrawer from '../RowDrawer.vue'
import DeathEditor from './DeathEditor.vue'
import LifeBadge from './LifeBadge.vue'
import { useKeyboard } from '../../composables/usePhone'
import { api, requestId } from '../../lib/api'
import { isBlank } from '../../lib/cells'
import { dayLabel, formatSerial, isoToSerial, serialFromIso, serialToIso, todayIso } from '../../lib/dates'
import {
  KILLED,
  WHOLE,
  bestRack,
  buildIndex,
  deathCells,
  factsOf,
  firstEmptySlot,
  lifeOf,
  lookAlikes,
  rankCauses,
  searchKey,
  suggest,
  type DeathCell,
  type Entry,
  type Facts,
  type RackSuggestion,
} from '../../lib/deaths'
import { idTokens, resolveIds } from '../../lib/ids'
import { errorText, notify } from '../../lib/notice'
import { persistentRef } from '../../lib/persist'
import { verificationsFor } from '../../lib/verifications'
import { fillIfBlank } from '../../lib/rows'
import type { CellValue, Table, TableRow } from '../../lib/types'
import { usePending } from '../../stores/pending'
import { useSession } from '../../stores/session'
import { type ServerRecord, useTables } from '../../stores/tables'
import { t, tn, tx, type Msg } from '../../lib/i18n'

/**
 * Muertes on a phone, with one thumb in the insectary: a search box that finds
 * a butterfly by Insectary ID (or CAM or tube) and says at once whether it is
 * alive; the butterflies chosen as cards; the death date, the cause as big
 * buttons and preserved or not; then one "Save" that writes exactly what the
 * computer's «Escribir fecha y causa» writes (lib/deaths.ts) and saves it, with
 * an Undo. The latest deaths below, by day; any card opens the full-screen editor.
 */
const MODULE = 'Insectary_data'
const props = defineProps<{ table: Table | undefined; ready: boolean; options: Record<string, string[]> }>()

const pending = usePending()
const session = useSession()
const tables = useTables()
const keyboard = useKeyboard()

const picked = persistentRef<string[]>('deaths:phone-picked', [])
const query = ref('')
const date = ref(todayIso())
const cause = persistentRef('deaths:phone-cause', '')
const preserved = persistentRef('deaths:phone-preserved', false)
const medium = persistentRef('deaths:phone-medium', 'Flash frozen', { lasting: true })
const recentCount = ref(30)
const today = computed(() => isoToSerial(todayIso()))

// --- The butterflies, indexed once per version of the sheet (13,500 rows: typing must stay instant).
const index = computed(() => (props.table ? buildIndex(props.table.rows) : []))
const byKey = computed(() => new Map(index.value.map(e => [e.key, e])))
const known = computed(() => new Map(index.value.map(e => [e.key, e.id])))
/** Rows alive in the sheet; a row with unsaved edits is asked again (its death may be pending). */
const aliveSaved = computed(() => {
  const out = new Set<string>()
  for (const e of index.value) if (lifeOf(f => e.row.values[f] ?? null).state === 'alive') out.add(e.row.id)
  return out
})
const get = (row: TableRow) => (field: string) => pending.value(row, field)
const isAlive = (entry: Entry) =>
  toRaw(pending.edits)[entry.row.id] ? lifeOf(get(entry.row)).state === 'alive' : aliveSaved.value.has(entry.row.id)
const factsFor = (row: TableRow): Facts => factsOf(get(row), today.value)

const chosen = computed(() =>
  picked.value.map(id => byKey.value.get(searchKey(id))?.row).filter((r): r is TableRow => !!r),
)
/** Newest first: the card just added shows right under the search box. */
const cards = computed(() => [...chosen.value].reverse())

// --- Search
const searchInput = ref<HTMLInputElement>()
const focused = ref(false)
const suggestions = computed(() =>
  query.value.trim() ? suggest(index.value, query.value, { alive: isAlive, skip: new Set(picked.value) }) : [],
)
/** What was typed that is no ID, with IDs that look like it ("did you mean"). */
const missing = ref<string[]>([])
const typedMissing = computed(() => {
  const q = searchKey(query.value)
  if (q.length < 2 || suggestions.value.length || !props.ready) return null
  return { typed: query.value.trim(), alike: lookAlikes(q, known.value) }
})
const missingAlike = computed(() => (missing.value.length ? lookAlikes(missing.value[0], known.value) : []))
const alreadyChosen = computed(() => {
  const q = searchKey(query.value)
  return q && picked.value.some(id => searchKey(id) === q) ? query.value.trim().toUpperCase() : ''
})

function add(ids: string[]) {
  const fresh = ids.filter(id => !picked.value.includes(id))
  if (fresh.length) picked.value = [...picked.value, ...fresh]
  lastSave.value = null
}
function choose(id: string) {
  add([id])
  query.value = ''
  missing.value = []
}
/** Enter: the exact ID (or CAM/tube), several IDs, or a range in the sheet's pre-made order (B0D-B9D). */
function enter() {
  const text = query.value.trim()
  if (!text) return
  const tokens = idTokens(text)
  if (tokens.length > 1 || tokens[0]?.includes('-')) return addTokens(tokens)
  const exact = byKey.value.get(searchKey(text))
  const top = suggestions.value[0]
  if (exact) choose(exact.id)
  else if (top && top.via === searchKey(text)) choose(top.entry.id)
  else missing.value = [text]
}
function addTokens(tokens: string[]) {
  const { found, missing: none } = resolveIds(
    tokens,
    index.value.map(e => e.id),
  )
  add(found)
  missing.value = none
  query.value = ''
}
function onPaste(event: ClipboardEvent) {
  const tokens = idTokens(event.clipboardData?.getData('text') || '')
  if (tokens.length > 1 || tokens[0]?.includes('-')) {
    event.preventDefault()
    addTokens(tokens)
  }
}
function remove(id: string) {
  picked.value = picked.value.filter(p => p !== id)
}
function removeAll() {
  picked.value = []
  missing.value = []
}

// --- Register: date, cause, preserved or not
const canEdit = computed(() => session.canEdit)
/**
 * The cause buttons: the sheet's own dropdown list for Death_cause (its data
 * validation), most used first; until it arrives, the values the column uses.
 */
const causes = computed(() => {
  const list = verificationsFor(MODULE)?.lists.Death_cause?.values
  const values = list?.size ? [...list] : (props.options.Death_cause || []).filter(v => !/^\d+$/.test(v))
  return rankCauses(values, props.table?.rows || [], today.value)
})
const quickDates = computed(() => [
  { iso: todayIso(), name: t('Hoy') },
  { iso: serialToIso(today.value - 1), name: t('Ayer') },
])
const dateError = computed(() =>
  date.value && serialFromIso(date.value) === null ? t('Fecha no válida: el año debe estar entre 1990 y 2099') : '',
)
function pickCause(c: string) {
  cause.value = c
  // Killed to be preserved: the body goes in a tube.
  if (c === KILLED) preserved.value = true
}
const mediums = computed(() => [...new Set(['Flash frozen', 'Ethanol', 'DMSO'])])

/** Rows dying now (no death date yet): in "preserved" each gets its CAM and tube. */
const dying = (row: TableRow) => lifeOf(get(row)).state !== 'dead'
const toPreserve = computed(() => (preserved.value ? cards.value.filter(dying) : []))
/** The CAM and tube typed (or suggested) for each card, by Insectary ID. */
const samples = reactive<Record<string, { cam: string; tube: string }>>({})
/** What the app suggested, so a value the person typed is never replaced. */
const suggested = reactive<Record<string, { cam: string; tube: string }>>({})
const sampleOf = (id: string) => (samples[id] ??= { cam: '', tube: '' })

// The next free CAM IDs and tubes (server/grid.mjs idSuggestions), asked for only when preserving.
const camRun = ref<string[]>([])
const tubeRun = ref<string[]>([])
const racks = ref<(RackSuggestion & { label: string; labelMsg?: Msg })[]>([])
const rack = computed(() => bestRack(racks.value, chosen.value, medium.value))
let camStart = ''
async function loadSampleIds() {
  if (!preserved.value || !toPreserve.value.length) return
  try {
    if (!camStart || !racks.value.length) {
      const [cam, tube] = await Promise.all([
        api<{ suggestions: { value: string }[] }>('ids?kind=cam'),
        api<{ suggestions: (RackSuggestion & { label: string; labelMsg?: Msg })[] }>('ids?kind=tube'),
      ])
      camStart = cam.suggestions[0]?.value || ''
      racks.value = tube.suggestions
    }
    const count = toPreserve.value.length + 4
    const run = (kind: string, start: string) =>
      api<{ sequence: string[] }>(`ids?kind=${kind}&start=${encodeURIComponent(start)}&count=${count}`).then(r => r.sequence)
    const [cams, tubes] = await Promise.all([
      camStart ? run('cam', camStart) : [],
      rack.value ? run('tube', rack.value.value) : [],
    ])
    camRun.value = cams
    tubeRun.value = tubes
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
watch([preserved, () => toPreserve.value.length, () => rack.value?.value], loadSampleIds, { immediate: true })
/**
 * Gives each card being preserved the next free CAM and tube no other card has,
 * keeping what the person typed and what was suggested before (a suggestion
 * from another rack, after the medium changed, gives way to the new rack's).
 */
watch([toPreserve, camRun, tubeRun], () => {
  const ids = toPreserve.value.map(r => String(r.values.Insectary_ID))
  const taken = (kind: 'cam' | 'tube', value: string, owner: string) =>
    ids.some(other => other !== owner && samples[other]?.[kind] === value)
  for (const row of toPreserve.value) {
    const id = String(row.values.Insectary_ID)
    const s = sampleOf(id)
    const auto = (suggested[id] ??= { cam: '', tube: '' })
    if (s.cam && s.cam === auto.cam && camRun.value.length && !camRun.value.includes(s.cam)) s.cam = ''
    if (s.tube && s.tube === auto.tube && tubeRun.value.length && !tubeRun.value.includes(s.tube)) s.tube = ''
    if (!s.cam && isBlank(pending.value(row, 'CAM_ID'))) {
      const next = camRun.value.find(v => !taken('cam', v, id))
      if (next) s.cam = auto.cam = next
    }
    if (!s.tube) {
      const next = tubeRun.value.find(v => !taken('tube', v, id))
      if (next) s.tube = auto.tube = next
    }
  }
})

/** The cells "Save" would write in each chosen row (the same as the computer's «Escribir fecha y causa»). */
const plans = computed(() => {
  const serial = date.value ? serialFromIso(date.value) : null
  const out = new Map<string, DeathCell[]>()
  for (const row of cards.value) {
    const id = String(row.values.Insectary_ID)
    const s = samples[id]
    const keep = preserved.value && dying(row)
    out.set(
      row.id,
      deathCells(row, pending.value, {
        serial,
        cause: cause.value,
        notPreserved: !preserved.value,
        preserve: keep ? { cam: s?.cam.trim().toUpperCase() || '', tube: s?.tube.trim().toUpperCase() || '', medium: medium.value } : undefined,
      }),
    )
  }
  return out
})
const toSave = computed(() => cards.value.filter(r => plans.value.get(r.id)?.length))
/** A card's plan in words: the date and cause, the CAM and tube, and how many NA / NOT_COLLECTED cells. */
function planText(row: TableRow) {
  const cells = plans.value.get(row.id) || []
  if (!cells.length) return ''
  const main: string[] = []
  let rest = 0
  for (const c of cells) {
    if (c.field === 'Death_date' && typeof c.value === 'number') main.push(formatSerial(c.value))
    else if ((['Death_cause', 'CAM_ID'].includes(c.field) || /^Tube_\d_id$/.test(c.field)) && !isBlank(c.value))
      main.push(String(c.value))
    else rest++
  }
  return rest ? `${main.join(' · ')} ${tn(rest, '+{n} celda', '+{n} celdas')}` : main.join(' · ')
}

/** Why "Save" cannot run yet (shown above the button: there is no hover on a phone). */
const blocker = computed(() => {
  if (!chosen.value.length) return t('Añade al menos una mariposa')
  if (dateError.value) return dateError.value
  if (!date.value) return t('Elige la fecha de muerte')
  if (!cause.value) return t('Elige la causa')
  if (preserved.value) {
    const seen = new Map<string, string>()
    for (const row of toPreserve.value) {
      const id = String(row.values.Insectary_ID)
      const s = samples[id]
      if (firstEmptySlot(f => pending.value(row, f)) === null) return t('{id} no tiene sitio para otro tubo', { id })
      if (isBlank(pending.value(row, 'CAM_ID')) && !s?.cam.trim()) return t('Falta el CAM de {id}', { id })
      if (!s?.tube.trim()) return t('Falta el tubo de {id}', { id })
      for (const v of [s.cam, s.tube].map(x => x.trim().toUpperCase()).filter(Boolean)) {
        if (seen.has(v)) return t('{value} está en {a} y en {b}', { value: v, a: seen.get(v)!, b: id })
        seen.set(v, id)
      }
    }
  }
  if (!toSave.value.length) return t('Nada que escribir: esas filas ya tienen fecha y causa')
  return ''
})
/** Pending changes of other rows, which go to the sheet in the same save. */
const otherPending = computed(() => {
  const mine = new Set(chosen.value.map(r => r.id))
  let n = pending.creates.length
  for (const e of Object.values(pending.edits)) if (!mine.has(e.id)) n += Object.keys(e.values).length
  return n
})

// --- Save, then Undo
const saving = ref(false)
const lastSave = ref<null | { actionId: string; ids: string[]; count: number }>(null)
const undoing = ref(false)
const waitIdle = async () => {
  for (let i = 0; i < 300 && pending.saving; i++) await new Promise(r => setTimeout(r, 100))
}
async function save() {
  if (blocker.value || saving.value) return
  saving.value = true
  const rows = toSave.value
  const ids = rows.map(r => String(r.values.Insectary_ID))
  try {
    for (const row of rows)
      for (const c of plans.value.get(row.id) || [])
        fillIfBlank(MODULE, row, String(row.values.Insectary_ID), c.field, c.value, c.overwrite)
    pending.touch()
    // An automatic save already running would leave these cells for later (and give no Undo).
    await waitIdle()
    const result = await pending.save('')
    const refused = rows.filter(r => Object.keys(pending.issues).some(k => k.startsWith(`${r.id}:`)))
    if (refused.length) {
      notify(
        t('No se guardó {ids}: {reason}', {
          ids: refused.map(r => String(r.values.Insectary_ID)).join(', '),
          reason: Object.values(pending.issues)[0] ?? '',
        }),
        'error',
      )
      picked.value = picked.value.filter(id => refused.some(r => String(r.values.Insectary_ID) === id))
      return
    }
    lastSave.value = result.actionId ? { actionId: result.actionId, ids, count: rows.length } : null
    picked.value = []
    cause.value = ''
    preserved.value = false
    for (const id of ids) {
      delete samples[id]
      delete suggested[id]
    }
    if (!result.actionId) notify(tn(rows.length, '{n} muerte guardada en Google Sheets', '{n} muertes guardadas en Google Sheets'), 'success')
    // Back to the top: the search for the next ones, and today's deaths in the list.
    scroller.value?.scrollTo({ top: 0 })
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    saving.value = false
  }
}
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
    else await tables.load(MODULE, true)
    // The cards come back, to correct and save again.
    // (`ids` are in card order, newest first: added back oldest first, the cards look as before.)
    picked.value = [...new Set([...picked.value, ...[...last.ids].reverse()])]
    lastSave.value = null
    notify(tn(last.count, '{n} muerte deshecha en Google Sheets', '{n} muertes deshechas en Google Sheets'), 'success')
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    undoing.value = false
  }
}

// --- Latest deaths, by day
const recent = computed(() => {
  if (!props.table) return []
  return props.table.rows
    .filter(r => r.observed && typeof r.values.Death_date === 'number' && !isBlank(r.values.Insectary_ID))
    .sort((a, b) => (b.values.Death_date as number) - (a.values.Death_date as number) || b.row - a.row)
    .slice(0, recentCount.value)
})
const recentGroups = computed(() => {
  const groups: { label: string; rows: { row: TableRow; at: number }[] }[] = []
  recent.value.forEach((row, at) => {
    const day = row.values.Death_date as number
    const ago = today.value - day
    const label = ago === 0 ? t('Hoy') : ago === 1 ? t('Ayer') : dayLabel(serialToIso(day)).split(' · ')[0]
    if (groups.at(-1)?.label !== label) groups.push({ label, rows: [] })
    groups.at(-1)!.rows.push({ row, at })
  })
  return groups
})

// --- The full-screen editor, over the list it was opened from
const editing = ref<null | { list: 'cards' | 'recent'; ids: string[]; index: number }>(null)
const rowById = computed(() => new Map((props.table?.rows || []).map(r => [r.id, r])))
const editorRows = computed(() =>
  editing.value ? editing.value.ids.map(id => rowById.value.get(id)).filter((r): r is TableRow => !!r) : [],
)
function openEditor(list: 'cards' | 'recent', at: number) {
  const rows = list === 'cards' ? cards.value : recent.value
  editing.value = { list, ids: rows.map(r => r.id), index: at }
}
const drawerRow = ref<TableRow | null>(null)

// --- The keyboard: the save bar rides above it, and the box being typed in stays in view
const root = ref<HTMLElement>()
const scroller = ref<HTMLElement>()
const footer = ref<HTMLElement>()
const searchBar = ref<HTMLElement>()
const rootBottom = ref(0)
const footerHeight = ref(0)
const measure = () => {
  rootBottom.value = root.value?.getBoundingClientRect().bottom ?? 0
  footerHeight.value = footer.value?.offsetHeight ?? 0
}
/** How far the save bar is lifted: the part of the page the keyboard covers below it. */
const lift = computed(() => (keyboard.open.value ? Math.max(0, Math.round(rootBottom.value - keyboard.visibleBottom.value)) : 0))
/** The suggestions fill the space between the search box and the save bar. */
const listHeight = computed(() => {
  const top = searchBar.value?.getBoundingClientRect().bottom ?? 0
  const bottom = Math.min(rootBottom.value - lift.value, keyboard.visibleBottom.value) - footerHeight.value
  return Math.max(160, Math.round(bottom - top - 8))
})
function keepInView() {
  const el = document.activeElement as HTMLElement | null
  const box = scroller.value
  if (!el || !box || !box.contains(el) || el === searchInput.value) return
  const r = el.getBoundingClientRect()
  const top = (searchBar.value?.getBoundingClientRect().bottom ?? keyboard.visibleTop.value) + 8
  const bottom = Math.min(rootBottom.value - lift.value, keyboard.visibleBottom.value) - footerHeight.value - 8
  if (r.bottom > bottom) box.scrollTop += r.bottom - bottom
  else if (r.top < top) box.scrollTop -= top - r.top
}
watch([keyboard.visibleBottom, keyboard.visibleTop], () => {
  measure()
  requestAnimationFrame(keepInView)
})
const sizes = typeof ResizeObserver === 'undefined' ? null : new ResizeObserver(measure)
onMounted(() => {
  measure()
  if (root.value) sizes?.observe(root.value)
  if (footer.value) sizes?.observe(footer.value)
})
watch(footer, (el, old) => {
  if (old) sizes?.unobserve(old)
  if (el) sizes?.observe(el)
  nextTick(measure)
})
onBeforeUnmount(() => sizes?.disconnect())
function onFocusIn() {
  measure()
  setTimeout(keepInView, 350)
}

const summary = computed(() => {
  const parts = [
    date.value ? dayLabel(date.value).split(' · ')[0] : '',
    cause.value,
    preserved.value ? t('preservadas') : t('sin preservar'),
  ].filter(Boolean)
  return parts.join(' · ')
})
/** Sex, clutch and the day it entered the insectary, as one line under the species. */
const line = (f: Facts) =>
  [
    f.sex,
    f.clutch && t('clutch {c}', { c: f.clutch }),
    f.entered !== null &&
      (f.wild ? t('Capturada {date}', { date: formatSerial(f.entered) }) : t('Emergió {date}', { date: formatSerial(f.entered) })),
  ]
    .filter(Boolean)
    .join(' · ')
const cellText = (v: CellValue) => (v === null || v === undefined ? '' : String(v))
</script>

<template>
  <div ref="root" class="flex h-full flex-col bg-stone-50" @focusin="onFocusIn">
    <div ref="scroller" class="min-h-0 flex-1 overflow-y-auto" :style="lift ? { paddingBottom: `${lift}px` } : undefined">
      <!-- The search stays at the top while the cards scroll. -->
      <div ref="searchBar" class="sticky top-0 z-20 border-b border-stone-200 bg-white px-3 pt-3 pb-2">
        <div class="relative">
          <Search :size="20" class="pointer-events-none absolute top-1/2 left-3 -translate-y-1/2 text-stone-400" />
          <input
            ref="searchInput"
            v-model="query"
            class="h-13 w-full rounded-xl border border-stone-300 bg-white pr-12 pl-10 text-lg font-medium uppercase placeholder:text-base placeholder:font-normal placeholder:normal-case focus:border-brand-600 focus:ring-2 focus:ring-brand-100 focus:outline-none"
            :placeholder="$t('Insectary ID, CAM o tubo')"
            :aria-label="$t('Buscar una mariposa por Insectary ID, CAM o tubo')"
            type="text"
            inputmode="text"
            autocapitalize="characters"
            autocomplete="off"
            autocorrect="off"
            spellcheck="false"
            enterkeyhint="go"
            @focus="focused = true"
            @blur="focused = false"
            @input="missing = []"
            @keydown.enter.prevent="enter"
            @paste="onPaste"
          />
          <button
            v-if="query"
            class="absolute top-1/2 right-1 grid h-11 w-11 -translate-y-1/2 place-items-center text-stone-500"
            :aria-label="$t('Borrar búsqueda')"
            @mousedown.prevent
            @click="query = ''"
          >
            <X :size="20" />
          </button>
          <!-- Suggestions: tapping one adds its card and keeps the keyboard for the next ID. -->
          <ul
            v-if="focused && suggestions.length"
            class="absolute inset-x-0 top-full z-30 mt-1 divide-y divide-stone-100 overflow-y-auto rounded-xl border border-stone-200 bg-white shadow-lg"
            :style="{ maxHeight: `${listHeight}px` }"
            role="listbox"
          >
            <li v-for="s in suggestions" :key="s.entry.id">
              <button
                class="flex min-h-14 w-full items-center gap-3 px-3 py-2 text-left active:bg-brand-50"
                role="option"
                @mousedown.prevent
                @click="choose(s.entry.id)"
              >
                <span class="w-16 shrink-0 text-lg font-semibold">{{ s.entry.id }}</span>
                <span class="min-w-0 flex-1">
                  <span class="block truncate text-sm">{{ factsFor(s.entry.row).species || '—' }}</span>
                  <span class="block truncate text-xs text-stone-500">{{ line(factsFor(s.entry.row)) }}</span>
                  <span v-if="s.via" class="block truncate text-xs text-brand-700">{{ s.via }}</span>
                </span>
                <LifeBadge :facts="factsFor(s.entry.row)" />
              </button>
            </li>
          </ul>
        </div>
        <p v-if="!ready" class="mt-1.5 text-sm text-stone-500">{{ $t('Cargando {sheet}…', { sheet: MODULE }) }}</p>
        <p v-else-if="alreadyChosen" class="mt-1.5 text-sm text-stone-600">{{ $t('{id} ya está en las tarjetas', { id: alreadyChosen }) }}</p>
        <div v-else-if="typedMissing || missing.length" class="mt-1.5 text-sm">
          <p class="text-red-700">
            {{ $t('No encontrado: {ids}', { ids: typedMissing ? typedMissing.typed.toUpperCase() : missing.join(', ') }) }}
          </p>
          <div v-if="(typedMissing ? typedMissing.alike : missingAlike).length" class="mt-1 flex flex-wrap items-center gap-2">
            <span class="text-stone-600">{{ $t('¿Quisiste decir?') }}</span>
            <button
              v-for="id in typedMissing ? typedMissing.alike : missingAlike"
              :key="id"
              class="h-11 rounded-lg border border-brand-600 bg-white px-4 font-semibold text-brand-800"
              @mousedown.prevent
              @click="choose(id)"
            >
              {{ id }}
            </button>
          </div>
        </div>
        <p v-else-if="!picked.length && !query" class="mt-1.5 text-xs text-stone-500">
          {{ $t('Escribe un ID para ver si está viva; pega varios o un rango (B0D-B9D) para registrar muertes.') }}
        </p>
      </div>

      <!-- The chosen butterflies. -->
      <section v-if="cards.length" class="px-3 pt-3">
        <div class="flex items-center justify-between">
          <h2 class="text-sm font-semibold text-stone-700">{{ $t('Elegidas ({n})', { n: cards.length }) }}</h2>
          <button class="h-11 px-2 text-sm text-stone-600 underline" @click="removeAll">{{ $t('Quitar todas') }}</button>
        </div>
        <ul class="space-y-2">
          <li v-for="(row, i) in cards" :key="row.id" class="relative rounded-xl border border-stone-200 bg-white shadow-sm">
            <button class="block w-full px-3 pt-2.5 pb-2 pr-12 text-left" @click="openEditor('cards', i)">
              <span class="flex flex-wrap items-center gap-2">
                <span class="text-xl font-semibold">{{ row.values.Insectary_ID }}</span>
                <LifeBadge :facts="factsFor(row)" />
              </span>
              <span class="mt-0.5 block text-sm">{{ factsFor(row).species || '—' }}</span>
              <span class="block text-xs text-stone-600">{{ line(factsFor(row)) }}</span>
              <span v-if="factsFor(row).life.cause" class="block text-xs text-stone-700">Death_cause: {{ factsFor(row).life.cause }}</span>
              <span v-if="factsFor(row).notes" class="block truncate text-xs text-stone-500">{{ factsFor(row).notes }}</span>
            </button>
            <button
              class="absolute top-1 right-1 grid h-11 w-11 place-items-center text-stone-500"
              :aria-label="$t('Quitar {id}', { id: String(row.values.Insectary_ID) })"
              @click="remove(String(row.values.Insectary_ID))"
            >
              <X :size="20" />
            </button>
            <!-- Preserved: its CAM and tube, the next free ones suggested. -->
            <div v-if="canEdit && toPreserve.includes(row)" class="grid grid-cols-2 gap-2 border-t border-stone-100 px-3 py-2">
              <label v-if="isBlank(pending.value(row, 'CAM_ID'))">
                <span class="field-label">CAM_ID</span>
                <input
                  v-model="sampleOf(String(row.values.Insectary_ID)).cam"
                  class="field-input h-11 text-base uppercase"
                  autocapitalize="characters"
                  autocomplete="off"
                  spellcheck="false"
                  enterkeyhint="next"
                />
                <span
                  v-if="samples[String(row.values.Insectary_ID)]?.cam && samples[String(row.values.Insectary_ID)]?.cam === suggested[String(row.values.Insectary_ID)]?.cam"
                  class="text-xs text-stone-500"
                  >{{ $t('siguiente libre') }}</span
                >
              </label>
              <p v-else class="text-sm">
                <span class="field-label">CAM_ID</span>{{ cellText(pending.value(row, 'CAM_ID')) }}
                <span class="block text-xs text-stone-500">{{ $t('se conserva') }}</span>
              </p>
              <label>
                <span class="field-label">Tube_{{ firstEmptySlot(f => pending.value(row, f)) ?? '?' }}_id</span>
                <input
                  v-model="sampleOf(String(row.values.Insectary_ID)).tube"
                  class="field-input h-11 text-base uppercase"
                  autocapitalize="characters"
                  autocomplete="off"
                  spellcheck="false"
                  enterkeyhint="done"
                />
                <span
                  v-if="samples[String(row.values.Insectary_ID)]?.tube && samples[String(row.values.Insectary_ID)]?.tube === suggested[String(row.values.Insectary_ID)]?.tube"
                  class="text-xs text-stone-500"
                  >{{ $t('siguiente libre') }}</span
                >
              </label>
            </div>
            <p
              v-if="canEdit"
              class="border-t border-stone-100 px-3 py-1.5 text-xs"
              :class="cause && plans.get(row.id)?.length ? 'text-amber-900' : 'text-stone-500'"
            >
              <template v-if="!cause && factsFor(row).life.state !== 'dead'">{{ $t('Elige la causa para registrarla') }}</template>
              <template v-else-if="plans.get(row.id)?.length">{{ $t('Se escribirá: {what}', { what: planText(row) }) }}</template>
              <template v-else>{{ $t('Ya registrada: no se cambiará') }}</template>
            </p>
          </li>
        </ul>
      </section>

      <!-- How they died. -->
      <section v-if="canEdit && cards.length" class="space-y-4 px-3 pt-5">
        <div>
          <h2 class="mb-1.5 text-sm font-semibold text-stone-700">{{ $t('Fecha de muerte') }}</h2>
          <div class="grid grid-cols-2 gap-2">
            <button
              v-for="d in quickDates"
              :key="d.iso"
              class="h-12 rounded-lg border text-base font-medium"
              :class="date === d.iso ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800'"
              :aria-pressed="date === d.iso"
              @click="date = d.iso"
            >
              {{ d.name }}
            </button>
          </div>
          <DateField v-model="date" class="field-input mt-2 h-12 text-base" />
          <p v-if="dateError" class="mt-1 text-sm text-red-700">{{ dateError }}</p>
          <p v-else-if="date" class="mt-1 text-sm text-stone-600">{{ dayLabel(date) }}</p>
        </div>
        <div>
          <h2 class="mb-1.5 text-sm font-semibold text-stone-700">Death_cause</h2>
          <div class="grid grid-cols-2 gap-2">
            <button
              v-for="c in causes"
              :key="c"
              class="min-h-12 rounded-lg border px-2 py-2 text-base font-medium break-words"
              :class="cause === c ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800 active:bg-stone-100'"
              :aria-pressed="cause === c"
              @click="pickCause(c)"
            >
              {{ c }}
            </button>
          </div>
        </div>
        <div>
          <h2 class="mb-1.5 text-sm font-semibold text-stone-700">{{ $t('Preservación') }}</h2>
          <div class="grid grid-cols-2 gap-2">
            <button
              class="min-h-12 rounded-lg border px-2 text-base font-medium"
              :class="!preserved ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800'"
              :aria-pressed="!preserved"
              @click="preserved = false"
            >
              {{ $t('Sin preservar') }}
            </button>
            <button
              class="min-h-12 rounded-lg border px-2 text-base font-medium"
              :class="preserved ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800'"
              :aria-pressed="preserved"
              @click="preserved = true"
            >
              {{ $t('Preservada') }}
            </button>
          </div>
          <p v-if="!preserved" class="mt-1.5 text-sm text-stone-600">
            {{ $t('Sin preservar: CAM y tubos NA, medios NOT_COLLECTED') }}
          </p>
          <template v-else>
            <p class="mt-1.5 text-sm text-stone-600">
              {{ $t('Cuerpo entero ({tissue}) en un tubo; el CAM y el tubo de cada una van en su tarjeta.', { tissue: WHOLE }) }}
            </p>
            <span class="field-label mt-2">T1_Preservation_medium</span>
            <div class="grid grid-cols-3 gap-2">
              <button
                v-for="m in mediums"
                :key="m"
                class="min-h-11 rounded-lg border px-1 text-sm font-medium"
                :class="medium === m ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800'"
                :aria-pressed="medium === m"
                @click="medium = m"
              >
                {{ m }}
              </button>
            </div>
            <p v-if="rack" class="mt-1.5 text-xs text-stone-500">{{ $t('Tubos de la gradilla {rack}', { rack: tx(rack.label, rack.labelMsg) }) }}</p>
          </template>
        </div>
        <p v-if="otherPending" class="text-xs text-amber-900">
          {{ $tn(otherPending, 'Se guardará también {n} cambio pendiente de otras filas.', 'Se guardarán también {n} cambios pendientes de otras filas.') }}
        </p>
      </section>

      <!-- The latest deaths, by day. -->
      <section class="px-3 pt-6 pb-8">
        <h2 class="text-sm font-semibold text-stone-700">{{ $t('Últimas muertes registradas') }}</h2>
        <p v-if="ready && !recent.length" class="py-3 text-sm text-stone-500">{{ $t('No hay muertes registradas.') }}</p>
        <template v-for="group in recentGroups" :key="group.label">
          <h3 class="mt-3 mb-1 text-xs font-semibold tracking-wide text-stone-500 uppercase">{{ group.label }}</h3>
          <ul class="divide-y divide-stone-100 overflow-hidden rounded-xl border border-stone-200 bg-white">
            <li v-for="{ row, at } in group.rows" :key="row.id">
              <button class="flex min-h-12 w-full items-center gap-3 px-3 py-1.5 text-left active:bg-stone-50" @click="openEditor('recent', at)">
                <span class="w-14 shrink-0 font-semibold">{{ row.values.Insectary_ID }}</span>
                <span class="min-w-0 flex-1">
                  <span class="block truncate text-sm">{{ cellText(row.values.Death_cause) || '—' }}</span>
                  <span class="block truncate text-xs text-stone-500">
                    {{ [cellText(row.values.SPECIES), cellText(row.values.Sex)].filter(Boolean).join(' · ') }}
                  </span>
                </span>
                <span v-if="!isBlank(row.values.CAM_ID)" class="shrink-0 text-xs text-brand-700">{{ row.values.CAM_ID }}</span>
              </button>
            </li>
          </ul>
        </template>
        <button v-if="recent.length >= recentCount" class="btn mt-3 h-11 w-full" @click="recentCount += 30">
          {{ $t('ver más') }}
        </button>
      </section>
    </div>

    <!-- Save (or the last save, with Undo): above the keyboard when it is open. -->
    <footer
      v-if="lastSave || (canEdit && cards.length)"
      ref="footer"
      class="relative z-20 shrink-0 border-t border-stone-200 bg-white px-3 pt-2 pb-[calc(0.5rem+env(safe-area-inset-bottom))]"
      :style="lift ? { transform: `translateY(-${lift}px)` } : undefined"
    >
      <div v-if="lastSave && !cards.length" class="flex items-center gap-2" role="status">
        <Check :size="22" class="shrink-0 text-brand-700" />
        <p class="min-w-0 flex-1 text-sm">
          <span class="font-medium">{{
            $tn(lastSave.count, '{n} muerte guardada en Google Sheets', '{n} muertes guardadas en Google Sheets')
          }}</span>
          <span class="block truncate text-xs text-stone-500">{{ lastSave.ids.join(', ') }}</span>
        </p>
        <button class="btn h-12 px-4 text-base" :disabled="undoing" @click="undo">
          <Loader2 v-if="undoing" :size="18" class="animate-spin" /><Undo2 v-else :size="18" /> {{ $t('Deshacer') }}
        </button>
        <button class="btn h-12 px-3" :aria-label="$t('Cerrar')" @click="lastSave = null"><X :size="18" /></button>
      </div>
      <template v-else>
        <p class="mb-1.5 truncate text-xs" :class="blocker ? 'text-amber-900' : 'text-stone-600'">
          {{ blocker || summary }}
        </p>
        <button class="btn-primary h-13 w-full text-base" :disabled="!!blocker || saving" @click="save">
          <Loader2 v-if="saving" :size="18" class="animate-spin" />
          {{
            saving
              ? $t('Guardando…')
              : $tn(toSave.length, 'Guardar {n} muerte', 'Guardar {n} muertes')
          }}
        </button>
      </template>
    </footer>

    <DeathEditor
      v-if="editing && editorRows.length"
      v-model:index="editing.index"
      :rows="editorRows"
      :columns="table?.columns || []"
      :causes="causes"
      :options="options"
      :can-edit="canEdit"
      @close="editing = null"
      @more="drawerRow = $event"
    />
    <RowDrawer
      v-if="drawerRow && table"
      :module="MODULE"
      :row-id="drawerRow.id"
      :rows="table.rows"
      :creates="[]"
      :columns="table.columns"
      :options="options"
      :locked-fields="[]"
      :create-formulas="[]"
      label-field="Insectary_ID"
      @close="drawerRow = null"
      @changed="pending.touch()"
    />
  </div>
</template>
