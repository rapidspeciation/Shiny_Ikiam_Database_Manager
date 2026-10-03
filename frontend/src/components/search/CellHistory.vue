<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, ref, shallowRef, watch } from 'vue'
import { RouterLink } from 'vue-router'
import { ArrowRight, Eye, History, X } from 'lucide-vue-next'
import { displayValue } from '../../lib/cells'
import { errorText } from '../../lib/notice'
import {
  PURPOSES,
  editMoment,
  formatMoment,
  loadCellHistory,
  momentRange,
  type AsOfSide,
  type CellEdit,
  type CellHistory,
  type HistoryTarget,
  type Stored,
} from '../../lib/history'
import { useSession } from '../../stores/session'
import { t } from '../../lib/i18n'

/**
 * The history of the cell selected in the Buscador (or of its whole row):
 * every edit oldest first, with when, who, from where (a tab, Google Sheets,
 * the assistant, an undo) and before → after. A person's saves less than ten
 * minutes apart are one edit. From each edit the sheet opens as it was just
 * before or after it (`view`), and its save in the Historial.
 * On a phone a sheet over the bottom of the screen; on a computer a column on the right.
 */
const props = defineProps<{ target: HistoryTarget }>()
const emit = defineEmits<{
  close: []
  /** The sheet around this row as it was before or after one of these edits. */
  view: [moment: { action: string; side: AsOfSide }, field: string | null]
}>()
const session = useSession()

const wholeRow = ref(props.target.field === null)
watch(
  () => props.target,
  target => (wholeRow.value = target.field === null),
)
const field = computed(() => (wholeRow.value ? null : props.target.field))
const history = shallowRef<CellHistory | null>(null)
const error = ref('')
const list = ref<HTMLElement>()
let controller: AbortController | null = null

async function load() {
  controller?.abort()
  const mine = (controller = new AbortController())
  error.value = ''
  history.value = null
  try {
    const reply = await loadCellHistory(props.target.recordId, field.value, mine.signal)
    if (controller !== mine) return
    history.value = reply
    // The newest edit is the one wanted most often: the list opens at its end.
    await nextTick()
    if (list.value) list.value.scrollTop = list.value.scrollHeight
  } catch (e) {
    if (controller === mine && !mine.signal.aborted) error.value = errorText(e)
  }
}
watch([() => props.target.recordId, field], load, { immediate: true })
onBeforeUnmount(() => controller?.abort())

const fieldOf = (key: string) => session.module(props.target.module)?.fields.find(f => f.key === key)
function show(value: Stored, key: string) {
  if (value && typeof value === 'object') return t('fórmula {formula}', { formula: value.formula })
  return displayValue(value, fieldOf(key)) || t('vacío')
}
const empty = (value: Stored) => value === null || value === undefined || value === ''
const who = (edit: CellEdit) => edit.actorName || (edit.actor === 'unknown' ? t('alguien en Google Sheets') : edit.actor)
const purpose = (edit: CellEdit) => {
  const known = PURPOSES[edit.purpose]
  return { label: known ? t(known.label) : edit.purpose || t('Otro'), tone: known?.tone ?? 'bg-stone-100 text-stone-700' }
}
const saves = (edit: CellEdit) => edit.actionIds.length

/** What the log cannot know, said once under the list. */
const limits = computed(() => {
  const h = history.value
  if (!h?.since) return ''
  const since = formatMoment(h.since).slice(0, 8)
  const sheets = h.sheetsSince ? formatMoment(h.sheetsSince).slice(0, 8) : since
  return t(
    'El historial empieza el {since}; lo escrito directamente en Google Sheets se conoce desde el {sheets}, cuando la app lo leyó (sin saber quién ni los pasos intermedios).',
    { since, sheets },
  )
})
const title = computed(() =>
  wholeRow.value ? t('Historial de la fila') : t('Historial de {field}', { field: props.target.field ?? '' }),
)
</script>

<template>
  <aside
    class="fixed inset-x-0 bottom-0 z-40 flex max-h-[75vh] flex-col rounded-t-xl border-t border-stone-300 bg-white shadow-2xl sm:static sm:z-auto sm:max-h-none sm:w-[24rem] sm:flex-none sm:rounded-none sm:border-t-0 sm:border-l sm:shadow-none xl:w-[27rem]"
    :aria-label="title"
  >
    <header class="flex items-start gap-2 border-b border-stone-200 px-3 py-2">
      <History :size="18" class="mt-1 flex-none text-stone-500" />
      <div class="min-w-0 flex-1">
        <h2 class="truncate font-semibold">{{ title }}</h2>
        <p class="truncate text-xs text-stone-500">
          {{ target.label || '—' }} · {{ target.module }}<template v-if="target.row"> · {{ $t('fila {row}', { row: target.row }) }}</template>
        </p>
      </div>
      <div v-if="target.field" class="flex flex-none overflow-hidden rounded-md border border-stone-300 text-xs" role="group">
        <button
          type="button"
          class="px-2 py-1.5"
          :class="!wholeRow ? 'bg-stone-800 text-white' : 'hover:bg-stone-100'"
          :aria-pressed="!wholeRow"
          @click="wholeRow = false"
        >
          {{ $t('Celda') }}
        </button>
        <button
          type="button"
          class="border-l border-stone-300 px-2 py-1.5"
          :class="wholeRow ? 'bg-stone-800 text-white' : 'hover:bg-stone-100'"
          :aria-pressed="wholeRow"
          @click="wholeRow = true"
        >
          {{ $t('Fila') }}
        </button>
      </div>
      <button type="button" class="btn-ghost flex-none" :title="$t('Cerrar')" :aria-label="$t('Cerrar')" @click="emit('close')">
        <X :size="18" />
      </button>
    </header>

    <div ref="list" class="min-h-0 flex-1 overflow-y-auto overscroll-contain bg-stone-50 px-2 py-2">
      <p v-if="error" class="p-3 text-sm text-red-700">{{ error }}</p>
      <p v-else-if="!history" class="p-3 text-sm text-stone-500">{{ $t('Cargando…') }}</p>
      <template v-else>
        <p v-if="!history.edits.length" class="p-3 text-sm text-stone-600">
          {{ wholeRow ? $t('Ningún cambio de esta fila en el historial.') : $t('Ningún cambio de esta celda en el historial.') }}
        </p>
        <p v-if="history.more" class="hint px-1 pb-2">{{ $t('Se muestran los 500 guardados más antiguos.') }}</p>
        <ol class="space-y-2">
          <li
            v-for="(edit, i) in history.edits"
            :key="edit.first"
            class="rounded-md border border-stone-200 bg-white px-2.5 py-2 text-sm shadow-xs"
          >
            <div class="flex flex-wrap items-baseline gap-x-2 gap-y-0.5">
              <span class="text-xs font-medium text-stone-800 tabular-nums">{{ momentRange(edit.start, edit.end) }}</span>
              <span class="text-stone-700">{{ who(edit) }}</span>
              <span class="rounded px-1.5 text-xs" :class="purpose(edit).tone">{{ purpose(edit).label }}</span>
              <span v-if="saves(edit) > 1" class="text-xs text-stone-500">{{ $t('{n} guardados', { n: saves(edit) }) }}</span>
              <span v-if="edit.status === 'failed'" class="rounded bg-amber-100 px-1.5 text-xs text-amber-900">{{ $t('No guardado') }}</span>
              <span v-if="i === history.edits.length - 1" class="ml-auto text-xs text-stone-400">{{ $t('último') }}</span>
            </div>
            <ul class="mt-1 space-y-0.5">
              <li v-for="c in edit.cells" :key="c.field" class="flex min-w-0 flex-wrap items-center gap-1" :class="{ 'opacity-60': c.undone }">
                <span v-if="wholeRow" class="mr-1 font-mono text-xs break-all text-stone-600">{{ c.field }}</span>
                <span
                  class="break-all"
                  :class="empty(c.before) ? 'italic text-stone-400' : 'rounded bg-red-50 px-1 text-red-800 line-through decoration-red-300'"
                  >{{ show(c.before, c.field) }}</span
                >
                <ArrowRight :size="12" class="flex-none text-stone-400" />
                <span
                  class="break-all"
                  :class="empty(c.after) ? 'italic text-stone-400' : 'rounded bg-emerald-50 px-1 font-medium text-emerald-900'"
                  >{{ show(c.after, c.field) }}</span
                >
                <span v-if="c.edits > 1" class="text-xs text-stone-500">{{ $t('({n} cambios)', { n: c.edits }) }}</span>
                <span v-if="c.isNew" class="rounded bg-emerald-100 px-1.5 text-xs text-emerald-800">{{ $t('fila nueva') }}</span>
                <span v-if="c.undone" class="text-xs text-stone-500">{{ $t('deshecho') }}</span>
              </li>
            </ul>
            <p v-if="edit.reasons.length" class="mt-1 text-xs break-words text-stone-500">«{{ edit.reasons.map(r => $t(r)).join(' · ') }}»</p>
            <div class="mt-1.5 flex flex-wrap items-center gap-x-3 gap-y-1 text-xs">
              <button
                type="button"
                class="inline-flex min-h-8 items-center gap-1 text-brand-700 hover:underline"
                :title="$t('Ver la hoja como estaba justo antes de este cambio')"
                @click="emit('view', editMoment(edit, 'before'), field)"
              >
                <Eye :size="13" /> {{ $t('Hoja antes') }}
              </button>
              <button
                type="button"
                class="inline-flex min-h-8 items-center gap-1 text-brand-700 hover:underline"
                :title="$t('Ver la hoja como quedó justo después de este cambio')"
                @click="emit('view', editMoment(edit, 'after'), field)"
              >
                <Eye :size="13" /> {{ $t('Hoja después') }}
              </button>
              <RouterLink
                :to="edit.link.slice(1)"
                class="inline-flex min-h-8 items-center gap-1 text-brand-700 hover:underline"
                :title="$t('Abrir este guardado en el Historial (allí se puede deshacer)')"
              >
                <History :size="13" /> {{ $t('En el Historial') }}
              </RouterLink>
            </div>
          </li>
        </ol>
        <p v-if="limits" class="hint px-1 pt-3">{{ limits }}</p>
      </template>
    </div>
  </aside>
</template>
