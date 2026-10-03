<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, ref, watch } from 'vue'
import { AlertTriangle, Check, CircleHelp, Columns3, ListFilter, Plus, Sparkles, SquarePen, X } from 'lucide-vue-next'
import ProposalSheet, { type CellEdit } from './assistant/ProposalSheet.vue'
import { api } from '../lib/api'
import { displayValue } from '../lib/cells'
import { errorText, notify } from '../lib/notice'
import { persistentRef } from '../lib/persist'
import {
  cellId,
  changedCells,
  changedText,
  expandProposal,
  notApplied,
  photoSummaries,
  rowKey,
  rowsToWrite,
  sheetGroups,
  sampleWarnings,
  uncheckedDoubts,
  unfilledUnreadable,
  withLocal,
  type LocalCell,
  type Proposal,
  type ProposalChange,
} from '../lib/proposals'
import type { CellValue } from '../lib/types'
import { useSession } from '../stores/session'
import { t } from '../lib/i18n'

export type { Proposal, ProposalChange } from '../lib/proposals'

/**
 * A proposal of the assistant as an editable sheet (one table per sheet it
 * touches): the assistant's values in green, the person's in blue. The person
 * corrects cells as in Colecta and they are saved to the proposal at once
 * (the assistant sees them and does not overwrite them); what the assistant
 * changes meanwhile flashes. Selected cells go back to the sheet's value (the
 * assistant's kept aside, marked) or take the assistant's again; "Aplicar"
 * writes what the table shows (as does "aplica" in the chat). Doubtful cells
 * (amber, "?") are counted at the top; "Aplicar" with some still unreviewed
 * asks first: apply them anyway, only the sure cells, or go and review them.
 * Cells the assistant could not read (hatched red, "unreadable") are counted
 * apart; they are never written until the person types them, and "Aplicar"
 * says so before applying.
 * A notebook page's proposal follows the page: a header per photo (its
 * thumbnail, which opens it upright in a new tab, and how many of its lines
 * change), every line in the notebook's order ("solo cambios" hides the lines
 * that write nothing), the notebook's columns first and the template's NA /
 * NOT_COLLECTED columns folded.
 */
const props = defineProps<{ proposal: Proposal; busy?: boolean }>()
const emit = defineEmits<{
  /** doubtful: what to do with the unreviewed doubtful cells (the person chose it in the dialog). */
  apply: [indexes: number[], revision: number | undefined, doubtful?: 'confirm' | 'skip']
  discard: []
  replace: [proposal: Proposal]
}>()
const session = useSession()

const pending = computed(() => props.proposal.status === 'pending')
const editable = computed(() => pending.value && session.canEdit)

// ------------------------------------------------------------ the person's edits, saved to the proposal
/** Typed and not yet saved (laid over the server's copy). */
const local = ref(new Map<string, LocalCell>())
/** Doubtful cells marked (or unmarked) as reviewed and not yet saved. */
const localChecks = ref(new Map<string, boolean>())
/** The proposal as the server sends it (lean) made whole for the table. */
const full = computed(() => expandProposal(props.proposal))
const shown = computed(() => withLocal(full.value, local.value, localChecks.value))
const queue = new Map<string, CellEdit>()
const checkQueue = new Map<string, { key: string; field: string; checked: boolean }>()
let removes: string[] = []
let adds: { sheet: string }[] = []
/** What was sent lately: its echo from the server must not flash as the assistant's change. */
const sent = new Map<string, { value: CellValue; at: number }>()
const saving = ref(false)
const failed = ref(false)
let timer: number | undefined
let running: Promise<void> | null = null

function onEdit(cells: CellEdit[]) {
  const next = new Map(local.value)
  for (const c of cells) {
    const id = cellId(c.key, c.field)
    next.set(id, c.use ? { value: c.value, use: c.use } : { value: c.value })
    // The first "before" is what the person saw.
    queue.set(id, { ...c, before: queue.get(id)?.before ?? c.before })
  }
  local.value = next
  later()
}
/** «Marcar revisadas»: shown at once, saved with the next edits. */
function onCheck(cells: { key: string; field: string }[]) {
  const next = new Map(localChecks.value)
  for (const c of cells) {
    next.set(cellId(c.key, c.field), true)
    checkQueue.set(cellId(c.key, c.field), { ...c, checked: true })
  }
  localChecks.value = next
  later(0)
}
function later(ms = 500) {
  window.clearTimeout(timer)
  timer = window.setTimeout(() => void save(), ms)
}
/** Sends what waits, one request at a time. */
async function save(): Promise<void> {
  window.clearTimeout(timer)
  if (running) {
    await running
    return save()
  }
  if (!queue.size && !removes.length && !adds.length && !checkQueue.size) return
  const cells = [...queue.values()]
  const checks = [...checkQueue.values()]
  const body = { cells, remove: removes, add: adds, check: checks }
  checkQueue.clear()
  queue.clear()
  removes = []
  adds = []
  saving.value = true
  running = (async () => {
    try {
      const out = await api<{
        proposal: Proposal
        rejected: { label?: string; field?: string; message: string }[]
        overrode: { label?: string; field: string; ai: CellValue }[]
      }>(`chat/proposals/${props.proposal.id}/edit`, { method: 'POST', body })
      failed.value = false
      const now = Date.now()
      for (const c of cells) sent.set(cellId(c.key, c.field), { value: c.value, at: now })
      dropSaved(cells)
      dropChecks(checks)
      savedRevision = Math.max(savedRevision, out.proposal.revision ?? 0)
      emit('replace', out.proposal)
      for (const r of out.rejected.slice(0, 3)) {
        const what = [r.field, r.label ? `(${r.label})` : ''].filter(Boolean).join(' ')
        const message = t(r.message)
        notify(what ? t('No se guardó {what}: {message}', { what, message }) : t('No se guardó: {message}', { message }), 'error')
      }
      for (const o of out.overrode.slice(0, 3))
        notify(
          t('Escribiste encima de un cambio de la IA en {field}: proponía {value}', {
            field: `${o.field}${o.label ? ` (${o.label})` : ''}`,
            value: show(o.field, o.ai) || t('vacío'),
          }),
        )
    } catch (e) {
      const code = (e as { code?: string }).code
      if (code === 'OFFLINE' || (e as { status?: number }).status === 0) {
        // Kept, and tried again: nothing typed is lost.
        failed.value = true
        for (const c of cells) if (!queue.has(cellId(c.key, c.field))) queue.set(cellId(c.key, c.field), c)
        for (const c of checks) if (!checkQueue.has(cellId(c.key, c.field))) checkQueue.set(cellId(c.key, c.field), c)
        removes.push(...body.remove)
        adds.push(...body.add)
        later(5000)
      } else {
        dropSaved(cells)
        dropChecks(checks)
        notify(errorText(e), 'error')
      }
    } finally {
      saving.value = false
      running = null
    }
  })()
  return running
}
function dropSaved(cells: CellEdit[]) {
  const next = new Map(local.value)
  for (const c of cells) {
    const id = cellId(c.key, c.field)
    const mine = next.get(id)
    if (!queue.has(id) && mine?.use === c.use && JSON.stringify(mine?.value) === JSON.stringify(c.value)) next.delete(id)
  }
  local.value = next
}
function dropChecks(checks: { key: string; field: string }[]) {
  if (!checks.length) return
  const next = new Map(localChecks.value)
  for (const c of checks) if (!checkQueue.has(cellId(c.key, c.field))) next.delete(cellId(c.key, c.field))
  localChecks.value = next
}
onBeforeUnmount(() => {
  // Leaving the page (or the panel) still saves what was typed.
  if (queue.size || removes.length || adds.length || checkQueue.size) void save()
})

function removeRow(key: string) {
  removes.push(key)
  later(0)
}
function addRow(sheet: string) {
  adds.push({ sheet })
  later(0)
}

// ------------------------------------------------------------ what the assistant changes, live
const flash = ref(new Set<string>())
const flashText = ref('')
let flashTimer: number | undefined
watch(
  () => props.proposal,
  (next, prev) => {
    const now = Date.now()
    for (const [id, s] of sent) if (now - s.at > 30000) sent.delete(id)
    const valueOf = (id: string) => {
      const [key, field] = id.split('\u0000')
      const c = next.changes.find(x => rowKey(x) === key)
      return c && field in c.values ? c.values[field] : undefined
    }
    const cells = changedCells(prev, next).filter(id => {
      if (local.value.has(id)) return false
      const mine = sent.get(id)
      return !mine || JSON.stringify(mine.value ?? null) !== JSON.stringify(valueOf(id) ?? null)
    })
    if (!cells.length) return
    flash.value = new Set(cells)
    flashText.value = changedText(cells.length)
    window.clearTimeout(flashTimer)
    flashTimer = window.setTimeout(() => {
      flash.value = new Set()
      flashText.value = ''
    }, 2600)
  },
)
onBeforeUnmount(() => window.clearTimeout(flashTimer))

// ------------------------------------------------------------ columns, apply
/** The rows "Aplicar" writes: what the table shows (a row set back to the sheet in every cell is left out). */
const chosen = computed(() => rowsToWrite(shown.value))
const setAside = computed(() => notApplied(shown.value))

/** Columns the person added to a sheet's table (kept while the tab is open). */
const extra = persistentRef<Record<string, string[]>>(`proposal-columns:${props.proposal.id}`, {})
const fieldsOf = (sheet: string) => session.module(sheet)?.fields
const groups = computed(() => sheetGroups(shown.value, extra.value, sheet => fieldsOf(sheet)?.map(f => f.key)))

// ------------------------------------------------------------ a notebook page
/** "Solo cambios": the page's lines that write nothing hidden (a line to look at, not found or refused, stays). */
const changesOnly = persistentRef('proposal-changes-only', false)
const lookAt = (c: ProposalChange) => !!c.page?.error || (!!c.placeholder && c.page?.status !== 'crossed')
const rowsOf = (changes: ProposalChange[]) => (changesOnly.value ? changes.filter(c => !c.context || lookAt(c)) : changes)
const quietRows = (changes: ProposalChange[]) => changes.filter(c => c.context && !lookAt(c)).length
/** The template's columns (only NA / NOT_COLLECTED), folded unless opened. */
const templatesOpen = ref(false)
const columnsOf = (g: { fields: string[]; template: string[] }) =>
  templatesOpen.value ? g.fields : g.fields.filter(f => !g.template.includes(f))
const page = computed(() => props.proposal.page)
/** Per photo of the page: its lines, how many change, how many are as the sheet has them. */
function photosOf(changes: ProposalChange[]) {
  const summaries = photoSummaries(changes)
  for (let n = 0; n < (page.value?.photos ?? 0); n++)
    if (!summaries.some(s => s.photo === n)) summaries.push({ photo: n, from: 0, to: 0, change: 0, same: 0, other: 0 })
  return summaries.sort((a, b) => a.photo - b.photo)
}
/** Each sheet's table as shown: its rows ("solo cambios"), its columns (the template folded), its page's photos. */
const tables = computed(() =>
  groups.value.map(g => ({
    ...g,
    rows: rowsOf(g.changes),
    columns: columnsOf(g),
    quiet: quietRows(g.changes),
    photos: page.value?.sheet === g.sheet ? photosOf(g.changes) : [],
  })),
)
const photoUrl = (n: number, size: 'thumb' | 'view') => `api/proposals/${props.proposal.id}/photos/${n}?size=${size}`
/** A thumbnail that would not load (an old proposal's photo gone): hidden. */
const brokenPhotos = ref(new Set<number>())
const typesOf = (sheet: string) => ({
  ...Object.fromEntries((fieldsOf(sheet) ?? []).map(f => [f.key, f.type])),
  ...props.proposal.types,
})
/** Columns that can still be added to a sheet's table. */
const addable = (sheet: string, fields: string[]) =>
  (fieldsOf(sheet) ?? []).filter(f => !f.readonly && !f.unavailable && !fields.includes(f.key)).map(f => f.key)
function addColumn(sheet: string, event: Event) {
  const select = event.target as HTMLSelectElement
  if (select.value) extra.value = { ...extra.value, [sheet]: [...(extra.value[sheet] ?? []), select.value] }
  select.value = ''
}

/** The revision the last save of the person's edits produced (the list may not show it yet). */
let savedRevision = 0
/** Doubtful cells nobody reviewed yet (in the whole table, and in the rows "Aplicar" writes). */
const doubtful = computed(() => uncheckedDoubts(shown.value))
const doubtfulToWrite = computed(() => uncheckedDoubts(shown.value, chosen.value))
/** The CAM or tube a preserved butterfly would be left without: marked in the table until filled. */
const noSample = computed(() => sampleWarnings(shown.value))
/** Unreadable cells nobody filled yet: applying leaves them as the sheet has them. */
const unreadable = computed(() => unfilledUnreadable(shown.value))
/** The dialog "Aplicar" opens while doubtful cells are unreviewed, or unreadable ones empty. */
const asking = ref(false)
/**
 * how: what to do with unreviewed doubtful cells; `leave`: the person saw the
 * empty unreadable cells and applies anyway (they stay as the sheet has them).
 */
async function apply(how?: 'confirm' | 'skip', leave = false) {
  // What was just typed goes into the proposal first.
  await save()
  await nextTick()
  if ((!how && doubtfulToWrite.value.length) || (!how && !leave && unreadable.value.length)) {
    asking.value = true
    return
  }
  asking.value = false
  emit('apply', chosen.value, Math.max(props.proposal.revision ?? 1, savedRevision) || undefined, how)
}
/** The tables, to bring a doubtful cell into view. */
const sheets = new Map<string, { focusCell: (key: string, field: string) => boolean }>()
const sheetRef = (sheet: string) => (el: unknown) => {
  if (el) sheets.set(sheet, el as { focusCell: (key: string, field: string) => boolean })
  else sheets.delete(sheet)
}
/** Selects the next doubtful (or unreadable) cell to review (after the one selected last, then from the top). */
const lastReviewed = { doubtful: -1, unreadable: -1 }
function reviewNext(which: 'doubtful' | 'unreadable' = doubtful.value.length ? 'doubtful' : 'unreadable') {
  asking.value = false
  const list = which === 'doubtful' ? doubtful.value : unreadable.value
  if (!list.length) return
  lastReviewed[which] = (lastReviewed[which] + 1) % list.length
  const next = list[lastReviewed[which]]
  const sheet = shown.value.changes.find(c => rowKey(c) === next.key)?.sheet
  if (sheet) sheets.get(sheet)?.focusCell(next.key, next.field)
}

const show = (field: string, value: CellValue | undefined) =>
  displayValue(value, { key: field, type: (props.proposal.types[field] ?? 'text') as 'text' })
const created = computed(() => props.proposal.changes.filter(c => c.create).length)
const personCells = computed(() => shown.value.changes.reduce((n, c) => n + Object.keys(c.personEdits ?? {}).length, 0))
const statusText = computed(
  () =>
    ({
      applied: t('Aplicado en la hoja ({n} filas)', { n: props.proposal.applied?.length ?? props.proposal.changes.length }),
      needs_review: t('No se pudo aplicar: revisa las filas en la hoja'),
      discarded: t('Descartado'),
      applying: t('Aplicando…'),
    })[props.proposal.status as string] ?? '',
)
</script>

<template>
  <div class="mt-2 rounded-md border border-stone-300 bg-white text-stone-800">
    <p class="flex flex-wrap items-center gap-x-2 border-b border-stone-200 px-2 py-1.5 text-xs font-medium">
      <span>
        {{ $t('Cambios propuestos') }} · {{ (proposal.sheets ?? [proposal.changes[0]?.sheet]).join(', ') }}
        <template v-if="created"> · {{ $tn(created, '{n} fila nueva', '{n} filas nuevas') }}</template>
        <span class="font-normal text-stone-500">— {{ proposal.reason }}</span>
      </span>
      <span
        v-if="flashText"
        class="flex items-center gap-1 rounded bg-amber-100 px-1.5 py-0.5 font-normal text-amber-900"
        role="status"
      >
        <Sparkles :size="12" /> {{ flashText }}
      </span>
      <button
        v-if="pending && doubtful.length"
        type="button"
        class="doubt-count"
        :title="$t('La IA no está segura de estas celdas: revisa cada una (edítala, elige otra lectura en la barra de arriba o márcala revisada). Clic: ir a la siguiente')"
        @click="reviewNext('doubtful')"
      >
        <CircleHelp :size="12" />
        {{ $tn(doubtful.length, '{n} celda dudosa por revisar', '{n} celdas dudosas por revisar') }}
      </button>
      <button
        v-if="pending && unreadable.length"
        type="button"
        class="unread-count"
        :title="$t('La IA no pudo leer estas celdas: escribe su valor en la tabla (la barra de arriba dice por qué y lo que se leyó). Vacías no se escriben. Clic: ir a la siguiente')"
        @click="reviewNext('unreadable')"
      >
        <SquarePen :size="12" />
        {{ $tn(unreadable.length, '{n} celda ilegible por rellenar', '{n} celdas ilegibles por rellenar') }}
      </button>
      <span
        v-if="pending && noSample.length"
        class="warn-count"
        :title="$t('Estas filas dejan una mariposa preservada sin CAM_ID o Tube_1_id (celdas en ámbar): pregunta al equipo y escríbelos aquí')"
      >
        <AlertTriangle :size="12" />
        {{ $tn(new Set(noSample.map(w => w.key)).size, '{n} preservada sin CAM o tubo', '{n} preservadas sin CAM o tubo') }}
      </span>
    </p>
    <div v-for="g in tables" :key="g.sheet" class="border-b border-stone-100 last:border-b-0">
      <!-- A notebook page: per photo, its thumbnail (opens upright in a new tab) and how its lines compare with the sheet. -->
      <div v-if="page && g.photos.length" class="flex flex-wrap gap-2 px-2 pt-1.5">
        <div
          v-for="p in g.photos"
          :key="p.photo"
          class="flex items-center gap-2 rounded border border-stone-200 bg-stone-50 py-1 pr-2 pl-1 text-[11px] text-stone-600"
        >
          <a
            v-if="p.photo < page.photos && !brokenPhotos.has(p.photo)"
            :href="photoUrl(p.photo, 'view')"
            target="_blank"
            rel="noopener"
            class="shrink-0"
            :title="$t('Abrir la foto en una pestaña nueva')"
          >
            <img
              :src="photoUrl(p.photo, 'thumb')"
              :alt="$t('Foto {n} del cuaderno', { n: p.photo + 1 })"
              class="h-14 w-auto max-w-24 rounded border border-stone-300 bg-white object-contain"
              loading="lazy"
              @error="brokenPhotos = new Set([...brokenPhotos, p.photo])"
            />
          </a>
          <span>
            <b v-if="g.photos.length > 1" class="font-medium text-stone-700"
              >{{ $t('Foto {n}', { n: p.photo + 1 }) }} ·
            </b>
            <template v-if="p.to"
              >{{ $t('Líneas {from}–{to}', { from: p.from, to: p.to }) }} ·
              {{ $tn(p.change, '{n} cambia', '{n} cambian') }} · {{ $tn(p.same, '{n} igual', '{n} iguales') }}
              <template v-if="p.other"> · {{ $tn(p.other, '{n} sin escribir', '{n} sin escribir') }}</template>
            </template>
          </span>
        </div>
      </div>
      <!-- The table and its bar, where ProposalSheet adds the buttons for the selected cells (Valor de la hoja / de la IA). -->
      <ProposalSheet
        :ref="sheetRef(g.sheet)"
        :sheet="g.sheet"
        :changes="g.rows"
        :fields="g.columns"
        :types="typesOf(g.sheet)"
        :new-row-formulas="proposal.newRowFormulas?.[g.sheet] ?? []"
        :editable="editable"
        :applied="proposal.status === 'applied' ? (proposal.applied ?? []) : null"
        :flash="flash"
        @edit="onEdit"
        @remove="removeRow"
        @check="onCheck"
        @notice="m => notify(m)"
      >
        <template v-if="groups.length > 1 || editable || g.template.length || g.quiet" #default>
          <span v-if="groups.length > 1" class="font-medium text-stone-700">{{ g.sheet }}</span>
          <label
            v-if="g.quiet"
            class="flex cursor-pointer items-center gap-1"
            :title="$t('Oculta las líneas de la página que no escriben nada (iguales a la hoja o tachadas)')"
          >
            <input v-model="changesOnly" type="checkbox" class="h-3 w-3" />
            <ListFilter :size="12" /> {{ $t('Solo cambios') }}
          </label>
          <button
            v-if="g.template.length"
            type="button"
            class="flex items-center gap-0.5 hover:text-stone-800"
            :title="$t('Columnas que solo llevan NA o NOT_COLLECTED de la plantilla: se escriben igual, aunque estén plegadas')"
            @click="templatesOpen = !templatesOpen"
          >
            <Columns3 :size="12" />
            {{
              templatesOpen
                ? $t('Plegar la plantilla')
                : $tn(g.template.length, '+{n} columna de plantilla', '+{n} columnas de plantilla')
            }}
          </button>
          <button
            v-if="editable && g.changes.some(c => c.create)"
            class="flex items-center gap-0.5 hover:text-emerald-800"
            :title="$t('Añadir una fila nueva vacía a esta hoja')"
            @click="addRow(g.sheet)"
          >
            <Plus :size="12" /> {{ $t('Fila') }}
          </button>
          <select
            v-if="editable && addable(g.sheet, g.fields).length"
            class="rounded border border-stone-200 bg-white px-1 py-0.5 text-[11px]"
            :aria-label="$t('Añadir columna')"
            @change="addColumn(g.sheet, $event)"
          >
            <option value="">{{ $t('+ Columna…') }}</option>
            <option v-for="f in addable(g.sheet, g.fields)" :key="f" :value="f">{{ f }}</option>
          </select>
        </template>
        <template v-if="editable" #end>
          <span class="ml-auto flex items-center gap-2">
            <span class="legend is-proposed" :title="$t('Valor de la IA: se escribe al aplicar')">{{ $t('IA') }}</span>
            <span class="legend is-person" :title="$t('Escrito por ti: se escribe al aplicar')">{{ $t('tú') }}</span>
            <span class="legend is-sheet" :title="$t('Valor actual de la hoja: no cambia')">{{ $t('hoja') }}</span>
            <span class="legend is-reverted" :title="$t('Vuelto al valor de la hoja: la sugerencia de la IA no se aplica')">{{
              $t('IA sin aplicar')
            }}</span>
            <span
              v-if="g.changes.some(c => c.doubts)"
              class="legend is-doubtful"
              :title="$t('La IA no está segura: revísala antes de aplicar')"
              >{{ $t('dudosa') }}</span
            >
            <span
              v-if="g.changes.some(c => c.unreadable)"
              class="legend is-unreadable"
              :title="$t('La IA no pudo leerla: escribe el valor; vacía no se escribe')"
              >{{ $t('ilegible') }}</span
            >
            <span
              v-if="g.changes.some(c => c.inferred?.length)"
              class="legend is-inferred"
              :title="$t('No está escrito en la línea: sale de la página, de la nota o de lo que el equipo escribe siempre')"
              >{{ $t('deducida') }}</span
            >
            <span
              v-if="g.changes.some(c => c.formulaGives)"
              class="legend is-formula-gives"
              :title="$t('Lo dará la fórmula de la hoja (del clutch): no se escribe')"
              >{{ $t('fórmula') }}</span
            >
            <span
              v-if="g.changes.some(c => c.context && !c.page?.error)"
              class="legend is-context"
              :title="$t('Línea de la página que no escribe nada: solo para seguirla')"
              >{{ $t('sin cambios') }}</span
            >
            <span
              v-if="g.changes.some(c => c.page?.error)"
              class="legend is-line-error"
              :title="$t('La hoja no aceptaría esta línea: su nota dice por qué')"
              >{{ $t('rechazada') }}</span
            >
          </span>
        </template>
      </ProposalSheet>
    </div>
    <div class="flex flex-wrap items-center gap-2 px-2 py-1.5">
      <template v-if="pending">
        <button class="btn-primary bg-emerald-700 hover:bg-emerald-800" :disabled="busy || !chosen.length" @click="apply()">
          <Check :size="15" /> {{ $tn(chosen.length, 'Aplicar {n} fila', 'Aplicar {n} filas') }}
        </button>
        <button class="btn" :disabled="busy" @click="emit('discard')"><X :size="15" /> {{ $t('Descartar') }}</button>
        <span class="hint">
          <template v-if="saving">{{ $t('Guardando tus cambios…') }}</template>
          <template v-else-if="failed">{{ $t('Sin conexión: tus cambios se guardarán al volver') }}</template>
          <template v-else>
            <template v-if="personCells > setAside"
              >{{ $tn(personCells - setAside, '{n} celda editada por ti', '{n} celdas editadas por ti') }} ·
            </template>
            <template v-if="setAside"
              >{{ $tn(setAside, '{n} sugerencia de la IA sin aplicar', '{n} sugerencias de la IA sin aplicar') }} ·
            </template>
          </template>
          {{ $t('Corrige en la tabla o díselo al asistente; también puedes responder «sí, aplícalo» en el chat.') }}
        </span>
      </template>
      <span v-else class="text-xs" :class="proposal.status === 'applied' ? 'text-brand-700' : 'text-amber-800'">
        {{ statusText }}
      </span>
    </div>
    <!-- "Aplicar" with doubtful cells nobody reviewed, or unreadable ones still empty: the person decides what happens to them. -->
    <div
      v-if="asking && pending"
      class="doubt-ask"
      :class="{ 'is-unread': !doubtfulToWrite.length }"
      role="alertdialog"
      :aria-label="doubtfulToWrite.length ? $t('Celdas dudosas sin revisar') : $t('Celdas ilegibles sin rellenar')"
    >
      <template v-if="doubtfulToWrite.length">
        <p class="font-medium">
          {{
            $tn(
              doubtfulToWrite.length,
              '{n} celda dudosa sin revisar: ¿aplicarla como la leyó la IA?',
              '{n} celdas dudosas sin revisar: ¿aplicarlas como las leyó la IA?',
            )
          }}
        </p>
        <p class="text-stone-600">
          {{ $t('Revísalas en la tabla (bordes ámbar con «?»): edita, elige otra lectura en la barra de arriba o márcalas revisadas.') }}
        </p>
      </template>
      <p v-if="unreadable.length" :class="doubtfulToWrite.length ? 'unread-line' : 'font-medium'">
        {{
          $tn(
            unreadable.length,
            '{n} celda ilegible sigue vacía: al aplicar no se escribe (queda como está en la hoja).',
            '{n} celdas ilegibles siguen vacías: al aplicar no se escriben (quedan como están en la hoja).',
          )
        }}
      </p>
      <div class="mt-1.5 flex flex-wrap gap-2">
        <template v-if="doubtfulToWrite.length">
          <button class="btn" :disabled="busy" @click="reviewNext('doubtful')"><CircleHelp :size="14" /> {{ $t('Revisarlas') }}</button>
          <button class="btn" :disabled="busy" @click="apply('skip')">{{ $t('Aplicar sin las dudosas') }}</button>
          <button class="btn" :disabled="busy" @click="apply('confirm')">{{ $t('Aplicar todo igualmente') }}</button>
        </template>
        <template v-else>
          <button class="btn" :disabled="busy" @click="reviewNext('unreadable')"><SquarePen :size="14" /> {{ $t('Rellenarlas') }}</button>
          <button class="btn" :disabled="busy || !chosen.length" @click="apply(undefined, true)">{{ $t('Aplicar sin ellas') }}</button>
        </template>
        <button class="btn" @click="asking = false"><X :size="14" /> {{ $t('Cancelar') }}</button>
      </div>
    </div>
  </div>
</template>
