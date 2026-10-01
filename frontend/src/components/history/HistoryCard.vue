<script setup lang="ts">
import { computed, reactive, ref, watch } from 'vue'
import {
  ArrowRight,
  Binoculars,
  Bot,
  Bug,
  ChevronDown,
  ClipboardCheck,
  Egg,
  FileSpreadsheet,
  Fingerprint,
  History,
  Link2,
  Skull,
  Sparkles,
  Table2,
  TestTube,
  Undo2,
  Upload,
} from 'lucide-vue-next'
import { displayValue } from '../../lib/cells'
import { PURPOSES, formatWhen, rowsOf, timeRange } from '../../lib/history'
import { notify } from '../../lib/notice'
import type { HistoryAction, HistoryChange, HistoryGroup } from '../../lib/types'
import { useSession } from '../../stores/session'
import { t, tn, tx } from '../../lib/i18n'

/** One card of the Historial: a group of saves, and when open, every change grouped by row. */
const props = defineProps<{
  group: HistoryGroup
  detail: HistoryGroup | null
  open: boolean
  highlighted: boolean
  canEdit: boolean
  /** The text searched for: the cells it matches are marked. */
  search?: string
  /** Big touch targets (a tab's own history, on a phone or tablet). */
  touch?: boolean
}>()
const emit = defineEmits<{
  toggle: []
  /** Preview an undo of these groups, saves or changes; `title` says what in the dialog. */
  undo: [body: { groupIds?: string[]; actionIds?: string[]; changeIds?: string[] }, title: string]
}>()
const session = useSession()

const ICONS = {
  colecta: Bug,
  monitoreo: Binoculars,
  muertes: Skull,
  emergidos: Sparkles,
  clutches: Egg,
  tubos: TestTube,
  tablas: Table2,
  revision: ClipboardCheck,
  cambio_id: Fingerprint,
  asistente: Bot,
  deshacer: Undo2,
  sheets: FileSpreadsheet,
  importacion: Upload,
} as const
const icon = computed(() => ICONS[props.group.purpose as keyof typeof ICONS] ?? History)
const tone = computed(() => PURPOSES[props.group.purpose]?.tone ?? 'bg-stone-100 text-stone-700')
const who = computed(
  () => props.group.actorName || (props.group.actor === 'unknown' ? t('alguien en Google Sheets') : props.group.actor),
)
/** The purpose in the interface's language (the server's label for one the app does not know). */
const purpose = computed(() => {
  const known = PURPOSES[props.group.purpose]?.label
  return known ? t(known) : props.group.purposeLabel
})
/** "12 mariposas de colecta: A0D–A8D" in the interface's language (the server sends its descriptor). */
const summary = computed(() => tx(props.group.summary, props.group.summaryMsg))
/** Save reasons: the app's own (e.g. the assistant's) are translated; typed ones stay as written. */
const reasons = computed(() => props.group.reasons.map(r => t(r)))
const counts = computed(() => {
  const c = props.group.counts
  const parts = [tn(c.rows, '{n} fila', '{n} filas')]
  if (c.newRows) parts.push(tn(c.newRows, '{n} nueva', '{n} nuevas'))
  parts.push(tn(c.cells, '{n} celda', '{n} celdas'))
  if (c.actions > 1) parts.push(t('{n} guardados', { n: c.actions }))
  return parts.join(' · ')
})
/** A save's state (Spanish, the keys of lib/i18n.ts: shown through t()). */
const STATUS: Record<string, string> = {
  observed: 'Leído de Google Sheets',
  pending: 'En curso',
  uncertain: 'Sin confirmar',
  failed: 'No guardado',
}
/** Saves of the group that were not confirmed in the sheet. */
const trouble = computed(() =>
  Object.entries(props.group.statuses)
    .filter(([s]) => ['pending', 'uncertain', 'failed'].includes(s))
    .map(([s, n]) => `${n} ${t(STATUS[s]).toLowerCase()}`),
)

const fieldOf = (sheet: string, key: string) => session.module(sheet)?.fields.find(f => f.key === key)
function show(value: HistoryChange['before'], sheet: string, field: string) {
  if (value && typeof value === 'object') return t('fórmula {formula}', { formula: value.formula })
  return displayValue(value, fieldOf(sheet, field)) || t('vacío')
}
/** A cell whose row, field or values contain the searched text. */
function found(c: HistoryChange, label: string) {
  const text = props.search?.trim().toLowerCase()
  if (!text) return false
  const values = [c.before, c.after].map(v => (v && typeof v === 'object' ? v.formula : String(v ?? '')))
  return [label, c.field, ...values].some(v => v.toLowerCase().includes(text))
}
const isEmpty = (value: HistoryChange['before']) => value === null || value === undefined || value === ''
/** Old value struck out in red, new value in green; an empty cell is just "vacío" in grey. */
const beforeClass = (value: HistoryChange['before']) =>
  isEmpty(value) ? 'italic text-stone-400' : 'rounded bg-red-50 px-1 text-red-800 line-through decoration-red-300'
const afterClass = (value: HistoryChange['after']) =>
  isEmpty(value) ? 'italic text-stone-400' : 'rounded bg-emerald-50 px-1 font-medium text-emerald-900'
/** "deshecho" when every cell of a save was put back, "deshecho en parte" when some were. */
function undoneLabel(action: HistoryAction) {
  const undone = action.changes.filter(c => c.undone).length
  return !undone ? '' : undone === action.changes.length ? t('deshecho') : t('deshecho en parte')
}

/** Big saves (a whole sync) show their rows a hundred at a time. */
const PAGE = 100
const shown = ref(PAGE)
const actions = computed<HistoryAction[]>(() => props.detail?.actions ?? [])
const multi = computed(() => actions.value.length > 1)
const sections = computed(() => {
  let left = shown.value
  return actions.value.map(action => {
    const rows = rowsOf(action.changes)
    const visible = rows.slice(0, Math.max(left, 0))
    left -= visible.length
    return { action, rows: visible, hidden: rows.length - visible.length }
  })
})
const hidden = computed(() => sections.value.reduce((n, s) => n + s.hidden, 0))

/** Cells picked with the checkboxes, to undo together. */
const picked = reactive(new Set<string>())
watch(
  () => props.detail,
  () => picked.clear(),
)
const canUndo = (action: HistoryAction, c?: HistoryChange) => props.canEdit && action.undoable && (!c || !c.undone)
function togglePick(id: string) {
  if (picked.has(id)) picked.delete(id)
  else picked.add(id)
}
const pending = (changes: HistoryChange[]) => changes.filter(c => !c.undone).map(c => c.id)

async function copyLink() {
  const url = `${location.origin}${location.pathname}#/historial?grupo=${props.group.id}`
  try {
    await navigator.clipboard.writeText(url)
    notify(t('Enlace copiado'))
  } catch {
    notify(url)
  }
}
</script>

<template>
  <article
    :id="`grupo-${group.id}`"
    class="scroll-mt-2 rounded-lg border bg-white shadow-sm transition-shadow"
    :class="highlighted ? 'border-amber-400 ring-2 ring-amber-300' : 'border-stone-200'"
  >
    <header class="flex cursor-pointer items-start gap-3 p-3" @click="emit('toggle')">
      <span class="grid size-9 shrink-0 place-items-center rounded-full" :class="tone" :title="purpose">
        <component :is="icon" :size="18" />
      </span>
      <div class="min-w-0 flex-1">
        <div class="flex flex-wrap items-baseline gap-x-2 gap-y-0.5 text-sm">
          <strong>{{ purpose }}</strong>
          <span class="text-stone-700">{{ who }}</span>
          <span class="text-xs text-stone-500 tabular-nums">{{ timeRange(group.start, group.end) }}</span>
          <span v-if="group.undone === 'all'" class="rounded bg-stone-100 px-1.5 text-xs text-stone-600">{{
            $t('deshecho')
          }}</span>
          <span v-else-if="group.undone === 'some'" class="rounded bg-stone-100 px-1.5 text-xs text-stone-600">{{
            $t('deshecho en parte')
          }}</span>
          <span v-for="text in trouble" :key="text" class="rounded bg-amber-100 px-1.5 text-xs text-amber-900">{{ text }}</span>
        </div>
        <p class="mt-0.5 text-sm break-words text-stone-800">{{ summary }}</p>
        <p class="hint mt-0.5 break-words">
          {{ counts }} · {{ group.sheets.join(', ') }}
          <template v-if="reasons.length"> · «{{ reasons.join(' · ') }}»</template>
          <template v-if="group.matched?.length && group.matched.length < group.counts.actions">
            ·
            {{ $t('coincide en {n} de {total} guardados', { n: group.matched.length, total: group.counts.actions }) }}</template
          >
        </p>
      </div>
      <div class="flex shrink-0 items-center gap-1" @click.stop>
        <button
          v-if="canEdit && group.undoable"
          :class="touch ? 'btn h-11 px-3 text-sm' : 'btn px-2 py-1 text-xs'"
          :title="$t('Deshacer todos los cambios de este guardado (se revisa antes)')"
          @click="emit('undo', { groupIds: [group.id] }, $t('Deshacer todo: {summary}', { summary }))"
        >
          <Undo2 :size="touch ? 16 : 14" /> <span :class="touch ? '' : 'hidden sm:inline'">{{ $t('Deshacer todo') }}</span>
        </button>
        <button v-if="!touch" class="btn-ghost" :title="$t('Copiar el enlace a este guardado')" @click="copyLink"><Link2 :size="15" /></button>
        <button
          class="btn-ghost"
          :class="{ 'h-11 w-11 justify-center': touch }"
          :title="open ? $t('Cerrar') : $t('Ver los cambios')"
          :aria-label="open ? $t('Cerrar') : $t('Ver los cambios')"
          :aria-expanded="open"
          @click="emit('toggle')"
        >
          <ChevronDown :size="16" class="transition-transform" :class="{ 'rotate-180': open }" />
        </button>
      </div>
    </header>

    <div v-if="open" class="border-t border-stone-100 px-2 pb-3 sm:px-3">
      <p v-if="!detail" class="hint p-2">{{ $t('Cargando cambios…') }}</p>
      <section v-for="{ action, rows, hidden: rest } in sections" :key="action.id">
        <div v-if="multi" class="mt-3 flex flex-wrap items-center gap-x-2 gap-y-1 text-xs text-stone-600">
          <span class="font-medium tabular-nums text-stone-800">{{ formatWhen(action.createdAt) }}</span>
          <span>{{ $tn(action.changes.length, '{n} celda', '{n} celdas') }}</span>
          <span v-if="action.reason" class="break-words">«{{ action.reason }}»</span>
          <span v-if="STATUS[action.status] && action.status !== 'observed'" class="rounded bg-amber-100 px-1.5 text-amber-900">{{
            $t(STATUS[action.status])
          }}</span>
          <span v-if="undoneLabel(action)" class="rounded bg-stone-100 px-1.5">{{ undoneLabel(action) }}</span>
          <button
            v-if="canUndo(action) && pending(action.changes).length"
            class="ml-auto inline-flex items-center gap-1 text-brand-700 hover:underline"
            :class="{ 'min-h-11 px-1 text-sm': touch }"
            @click="
              emit(
                'undo',
                { actionIds: [action.id] },
                $t('Deshacer el guardado de {when}', { when: formatWhen(action.createdAt) }),
              )
            "
          >
            <Undo2 :size="12" /> {{ $t('Deshacer este guardado') }}
          </button>
        </div>
        <div v-for="row in rows" :key="row.recordId" class="mt-2 overflow-hidden rounded border border-stone-200">
          <div class="flex flex-wrap items-center gap-x-2 gap-y-0.5 bg-stone-50 px-2 py-1 text-xs text-stone-600">
            <strong class="text-sm text-stone-900">{{ row.label }}</strong>
            <span>{{ $t('{sheet} fila {row}', { sheet: row.sheet, row: row.row }) }}</span>
            <span v-if="row.isNew" class="rounded bg-emerald-100 px-1.5 text-emerald-800">{{ $t('fila nueva') }}</span>
            <button
              v-if="canUndo(action) && row.changes.length > 1 && pending(row.changes).length"
              class="ml-auto inline-flex items-center gap-1 text-brand-700 hover:underline"
              :class="{ 'min-h-11 px-1 text-sm': touch }"
              @click="
                emit('undo', { changeIds: pending(row.changes) }, $t('Deshacer los cambios de {label}', { label: row.label }))
              "
            >
              <Undo2 :size="12" /> {{ $t('Deshacer fila') }}
            </button>
          </div>
          <ul class="divide-y divide-stone-100 text-sm">
            <li
              v-for="c in row.changes"
              :key="c.id"
              class="grid gap-x-2 px-2 py-1"
              :class="{
                'grid-cols-[1.5rem_minmax(0,1fr)_auto] items-center': touch,
                'grid-cols-[1.25rem_minmax(0,1fr)_auto] items-start': !touch,
                'opacity-60': c.undone,
                'bg-amber-50': picked.has(c.id),
                'bg-yellow-50 ring-1 ring-yellow-300 ring-inset': found(c, row.label),
              }"
            >
              <input
                v-if="canUndo(action, c)"
                type="checkbox"
                :class="touch ? 'size-5' : 'mt-1'"
                :checked="picked.has(c.id)"
                :aria-label="$t('Elegir {field} de {label}', { field: c.field, label: row.label })"
                @change="togglePick(c.id)"
              />
              <span v-else></span>
              <div class="min-w-0 sm:flex sm:items-start sm:gap-2">
                <span class="block shrink-0 font-mono text-xs break-all text-stone-600 sm:w-44 sm:pt-0.5">{{ c.field }}</span>
                <span class="flex min-w-0 flex-wrap items-center gap-1">
                  <span class="break-all" :class="beforeClass(c.before)">{{ show(c.before, c.sheet, c.field) }}</span>
                  <ArrowRight :size="12" class="shrink-0 text-stone-400" />
                  <span class="break-all" :class="afterClass(c.after)">{{ show(c.after, c.sheet, c.field) }}</span>
                  <span v-if="c.undone" class="text-xs text-stone-500">{{ $t('deshecho') }}</span>
                </span>
              </div>
              <button
                v-if="canUndo(action, c)"
                class="btn-ghost"
                :class="touch ? 'h-11 w-11 justify-center' : 'p-1'"
                :aria-label="$t('Deshacer solo {field}', { field: c.field })"
                :title="$t('Deshacer solo {field}', { field: c.field })"
                @click="
                  emit('undo', { changeIds: [c.id] }, $t('Deshacer {field} de {label}', { field: c.field, label: row.label }))
                "
              >
                <Undo2 :size="14" />
              </button>
            </li>
          </ul>
        </div>
        <p v-if="rest" class="hint mt-1">{{ $t('y {n} filas más en este guardado', { n: rest }) }}</p>
      </section>
      <div v-if="hidden" class="mt-2 text-center">
        <button class="btn py-1 text-xs" @click="shown += PAGE">
          {{ $t('Mostrar {n} filas más', { n: Math.min(PAGE, hidden) }) }}
        </button>
      </div>
      <div
        v-if="picked.size"
        class="sticky bottom-0 mt-3 flex flex-wrap items-center gap-2 rounded-md border border-amber-300 bg-amber-50 px-3 py-2 text-sm"
      >
        <span>{{ $tn(picked.size, '{n} cambio elegido', '{n} cambios elegidos') }}</span>
        <button
          class="btn-primary ml-auto py-1"
          @click="emit('undo', { changeIds: [...picked] }, $t('Deshacer {n} cambios elegidos', { n: picked.size }))"
        >
          <Undo2 :size="14" /> {{ $t('Deshacer selección') }}
        </button>
        <button class="btn py-1" @click="picked.clear()">{{ $t('Quitar') }}</button>
      </div>
    </div>
  </article>
</template>
