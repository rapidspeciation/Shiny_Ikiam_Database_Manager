<script setup lang="ts">
import { computed, markRaw, onBeforeUnmount, ref, shallowRef, watch } from 'vue'
import { RouterLink } from 'vue-router'
import { ArrowLeft, ChevronDown, ChevronLeft, ChevronRight, ChevronUp, History, Info } from 'lucide-vue-next'
import SheetGrid from '../SheetGrid.vue'
import { errorText, notify } from '../../lib/notice'
import {
  formatMoment,
  loadAsOf,
  loadCellHistory,
  stepEdit,
  type AsOf,
  type AsOfSide,
  type CellEdit,
  type HistoryTarget,
} from '../../lib/history'
import type { Field, TableRow } from '../../lib/types'
import { rowFromWire } from '../../stores/tables'
import { useSession } from '../../stores/session'
import { t, tn, tx } from '../../lib/i18n'

/**
 * A sheet as it was just before (or after) one save: the rows around `row`,
 * read-only, each cell with the value it had then (the server undoes every
 * later change). Cells that differ from now are yellow, those the save
 * changed outlined, rows not created yet striped. ‹ › step to the previous or
 * next edit of one cell (`field`, else the first the save changed in that row).
 * Links: #/tablas?hoja=<sheet>&fila=<row>&antes=<save>[&campo=<column>] (despues= for after).
 */
const props = defineProps<{ module: string; row: number; action: string; side: AsOfSide; field: string | null }>()
const emit = defineEmits<{
  /** Back to the sheet as it is now, at the same row. */
  now: []
  move: [moment: { action: string; side: AsOfSide }]
  history: [target: HistoryTarget]
}>()
const session = useSession()
const mod = computed(() => session.module(props.module))
const keys = computed(() => mod.value?.fields.map(f => f.key) || [])
const columns = computed<Field[]>(() => (mod.value?.fields || []).filter((f, i, all) => all.findIndex(g => g.key === f.key) === i))
const frozen = computed(() => mod.value?.identityFields.slice(0, 1) || [])

const view = shallowRef<AsOf | null>(null)
const rows = shallowRef<TableRow[]>([])
const error = ref('')
let controller: AbortController | null = null
async function load() {
  controller?.abort()
  const mine = (controller = new AbortController())
  error.value = ''
  try {
    const reply = await loadAsOf(props.module, props.action, props.side, props.row, mine.signal)
    if (controller !== mine) return
    rows.value = reply.rows.map(r => markRaw(rowFromWire(keys.value, r)))
    view.value = reply
  } catch (e) {
    if (controller === mine && !mine.signal.aborted) error.value = errorText(e)
  }
}
watch(() => [props.module, props.row, props.action, props.side], load, { immediate: true })
onBeforeUnmount(() => controller?.abort())

const focus = computed(() => rows.value.find(r => r.row === props.row) ?? null)
/** The cell whose edits ‹ › step through: the one named, else the first the save changed in the focused row. */
const stepField = computed(() => props.field ?? (focus.value ? view.value?.touched[focus.value.id]?.[0] : undefined) ?? null)
const edits = shallowRef<CellEdit[]>([])
watch(
  () => [focus.value?.id, stepField.value] as const,
  async ([id, field], before) => {
    if (before && before[0] === id && before[1] === field) return
    edits.value = []
    if (!id || !field) return
    try {
      edits.value = (await loadCellHistory(id, field)).edits
    } catch (e) {
      notify(errorText(e), 'error')
    }
  },
)
const position = computed(() => edits.value.findIndex(e => e.actionIds.includes(props.action)))
const previous = computed(() => stepEdit(edits.value, props.action, props.side, -1))
const next = computed(() => stepEdit(edits.value, props.action, props.side, 1))

const save = computed(() => view.value?.action ?? null)
const who = computed(() => {
  const a = save.value
  if (!a) return ''
  return a.actorName || (a.actor === 'unknown' ? t('alguien en Google Sheets') : a.actor)
})
const summary = computed(() => (save.value ? tx(save.value.summary, save.value.summaryMsg) : ''))
const differing = computed(() => Object.values(view.value?.changed ?? {}).reduce((n, cells) => n + Object.keys(cells).length, 0))
const touchedCount = computed(() => Object.values(view.value?.touched ?? {}).reduce((n, cells) => n + cells.length, 0))
/** Edits typed in Google Sheets before the app read the sheet are not in the log. */
const unknownBefore = computed(() => {
  const v = view.value
  const since = v?.sheetsSince ?? v?.since
  return v && since && v.at < since ? formatMoment(since).slice(0, 8) : ''
})
const showLimits = ref(false)
const titleOpen = ref(false)
const gridKey = computed(() => `${props.action}:${props.side}:${props.row}`)
</script>

<template>
  <div class="flex min-h-0 flex-1 flex-col">
    <div class="border-b border-amber-300 bg-amber-50 px-3 py-1.5 text-sm text-amber-950 sm:px-4 sm:py-2">
      <div class="flex items-start gap-x-3">
        <button
          type="button"
          class="btn flex-none py-1 max-sm:px-2"
          :title="$t('Volver a la hoja como está ahora')"
          :aria-label="$t('Volver a la hoja como está ahora')"
          @click="emit('now')"
        >
          <ArrowLeft :size="15" /> <span class="max-sm:hidden">{{ $t('Volver a ahora') }}</span>
        </button>
        <!-- Two lines on a phone until tapped. -->
        <p class="min-w-0 flex-1 self-center" :class="{ 'max-sm:line-clamp-2': !titleOpen }" @click="titleOpen = !titleOpen">
          <template v-if="view">
            <b>{{ $t('{sheet} como estaba el {when}', { sheet: module, when: formatMoment(view.at) }) }}</b>{{ ' ' }}
            <span v-if="save" class="break-words">
              {{
                side === 'before'
                  ? $t('(antes del guardado de {who} «{summary}»)', { who, summary })
                  : $t('(después del guardado de {who} «{summary}»)', { who, summary })
              }}
            </span>
          </template>
          <template v-else-if="!error">{{ $t('Cargando cómo estaba {sheet}…', { sheet: module }) }}</template>
        </p>
      </div>
      <div class="mt-1.5 flex flex-wrap items-center gap-x-2 gap-y-1.5">
        <div class="flex overflow-hidden rounded-md border border-amber-300 bg-white text-xs" role="group">
          <button
            type="button"
            class="px-2 py-1.5"
            :class="side === 'before' ? 'bg-amber-800 text-white' : 'hover:bg-amber-100'"
            :aria-pressed="side === 'before'"
            @click="emit('move', { action: props.action, side: 'before' })"
          >
            {{ $t('Antes') }}
          </button>
          <button
            type="button"
            class="border-l border-amber-300 px-2 py-1.5"
            :class="side === 'after' ? 'bg-amber-800 text-white' : 'hover:bg-amber-100'"
            :aria-pressed="side === 'after'"
            @click="emit('move', { action: props.action, side: 'after' })"
          >
            {{ $t('Después') }}
          </button>
        </div>
        <span v-if="stepField && edits.length" class="flex items-center gap-1 text-xs">
          <button
            type="button"
            class="btn px-1.5 py-1"
            :disabled="!previous"
            :title="$t('Cambio anterior de {field}', { field: stepField })"
            @click="previous && emit('move', previous)"
          >
            <ChevronLeft :size="15" />
          </button>
          <span class="tabular-nums">{{
            $t('cambio {at} de {total} de {field}', { at: position + 1 || '–', total: edits.length, field: stepField })
          }}</span>
          <button
            type="button"
            class="btn px-1.5 py-1"
            :disabled="!next"
            :title="$t('Cambio siguiente de {field}', { field: stepField })"
            @click="next && emit('move', next)"
          >
            <ChevronRight :size="15" />
          </button>
        </span>
        <RouterLink
          v-if="save"
          :to="save.link.slice(1)"
          class="btn py-1 text-xs"
          :title="$t('Abrir este guardado en el Historial (allí se puede deshacer)')"
        >
          <History :size="14" /> <span class="max-sm:hidden">{{ $t('En el Historial') }}</span>
        </RouterLink>
      </div>
      <div v-if="view" class="mt-1.5 flex flex-wrap items-center gap-x-3 gap-y-1 text-xs">
        <span><span class="mr-1 inline-block size-3 rounded-sm bg-[#fdf0b8] align-[-2px] ring-1 ring-amber-300" />{{
          tn(differing, '{n} celda distinta de ahora', '{n} celdas distintas de ahora')
        }}</span>
        <span v-if="save && touchedCount"
          ><span class="mr-1 inline-block size-3 rounded-sm align-[-2px] ring-2 ring-[#b45309] ring-inset" />{{
            tn(touchedCount, '{n} celda de este guardado', '{n} celdas de este guardado')
          }}</span
        >
        <span v-if="view.absent.length"
          ><span class="mr-1 inline-block size-3 rounded-sm bg-stone-300 align-[-2px]" />{{
            tn(view.absent.length, '{n} fila aún no creada', '{n} filas aún no creadas')
          }}</span
        >
        <button
          type="button"
          class="btn-ghost -my-1 ml-auto p-0.5"
          :title="$t('Qué se ve aquí')"
          :aria-expanded="showLimits"
          @click="showLimits = !showLimits"
        >
          <Info :size="15" /> <component :is="showLimits ? ChevronUp : ChevronDown" :size="13" />
        </button>
      </div>
      <p v-if="unknownBefore" class="mt-1 text-xs font-medium text-amber-900">
        {{
          $t('El historial de lo escrito en Google Sheets empieza el {date}: lo cambiado allí antes no se conoce.', {
            date: unknownBefore,
          })
        }}
      </p>
      <p v-if="showLimits" class="mt-1 text-xs text-amber-900">
        {{
          $t(
            'Cada celda muestra el valor que tenía en ese momento, deshaciendo todos los cambios posteriores del historial. Las fórmulas muestran su valor de hoy si eran la misma fórmula. De Google Sheets solo se conoce lo que la app leyó (cada pocos minutos, sin quién); las filas escritas directamente allí se ven como están ahora.',
          )
        }}
      </p>
    </div>
    <div class="min-h-0 flex-1">
      <p v-if="error" class="p-6 text-red-700">{{ error }}</p>
      <p v-else-if="!view" class="p-6 text-stone-500">{{ $t('Cargando cómo estaba {sheet}…', { sheet: module }) }}</p>
      <p v-else-if="!rows.length" class="p-6 text-stone-500">{{ $t('Sin filas') }}</p>
      <SheetGrid
        v-else
        :key="gridKey"
        :module="module"
        :rows="rows"
        :columns="columns"
        :frozen="frozen"
        :header-filters="false"
        :newest-first="false"
        :focus-row="focus?.id ?? null"
        :focus-field="stepField"
        :compare="view.changed"
        :touched="view.touched"
        :absent="view.absent"
        readonly
        cell-history
        @notice="notify"
        @history="target => emit('history', target)"
      />
    </div>
  </div>
</template>
