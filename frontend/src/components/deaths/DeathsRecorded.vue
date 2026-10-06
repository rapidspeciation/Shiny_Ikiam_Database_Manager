<script setup lang="ts">
import { computed } from 'vue'
import { ArrowDown, ArrowUp, Loader2, Pencil, RefreshCw, Undo2 } from 'lucide-vue-next'
import SexBadge from '../SexBadge.vue'
import { isBlank } from '../../lib/cells'
import { formatSerial } from '../../lib/dates'
import { sortRecorded, type RecordedOrder, type RecordedSort } from '../../lib/deathsCart'
import type { CellValue, TableRow } from '../../lib/types'
import { usePending } from '../../stores/pending'
import { t } from '../../lib/i18n'

/** A death in «Registradas hoy»: saved (with its changes, to undo them), or not in Google Sheets yet. */
export interface RecordedItem {
  row: TableRow
  status: 'saved' | 'pending' | 'queued'
  /** Others who saved it (the person's own saves are not named). */
  others: string[]
  changeIds: string[]
}

/**
 * «Registradas hoy»: the deaths saved from Muertes today, in the order chosen
 * (Insectary ID, emergence date or sheet row, ↑/↓, kept in this browser) to
 * copy them into the paper notebook; those not in Google Sheets yet first. A
 * tap opens one in the panel to correct it; «Deshacer» undoes its death.
 */
const props = defineProps<{
  items: RecordedItem[]
  /** The one open in the panel. */
  editing: string | null
  canEdit: boolean
  loading: boolean
  /** A chip for a butterfly preserved without CAM or tube. */
  gapOf: (row: TableRow) => string
}>()
const order = defineModel<RecordedOrder>('order', { required: true })
const mineOnly = defineModel<boolean>('mineOnly', { required: true })
const emit = defineEmits<{ edit: [row: TableRow]; undo: [item: RecordedItem]; refresh: [] }>()
const pending = usePending()

const value = (row: TableRow, field: string) => pending.value(row, field)
const text = (v: CellValue) => (isBlank(v) ? '' : String(v))
const facts = (item: RecordedItem) => {
  const emergence = value(item.row, 'Intro2Insectary_date')
  return { id: text(value(item.row, 'Insectary_ID')), emergence: typeof emergence === 'number' ? emergence : null, row: item.row.row }
}
const waiting = computed(() => sortRecorded(props.items.filter(i => i.status !== 'saved'), order.value, facts))
const saved = computed(() => sortRecorded(props.items.filter(i => i.status === 'saved'), order.value, facts))

const sorts = computed<{ by: RecordedSort; label: string }[]>(() => [
  { by: 'id', label: 'Insectary ID' },
  { by: 'emergence', label: t('Emergencia') },
  { by: 'row', label: t('Fila') },
])
const sortBy = (by: RecordedSort) => (order.value = { ...order.value, by })
const flip = () => (order.value = { ...order.value, desc: !order.value.desc })

function line(item: RecordedItem) {
  const death = value(item.row, 'Death_date')
  return [text(value(item.row, 'Death_cause')) || '—', typeof death === 'number' ? formatSerial(death) : ''].filter(Boolean).join(' · ')
}
function details(item: RecordedItem) {
  const entered = value(item.row, 'Intro2Insectary_date')
  const wild = /wild/i.test(text(value(item.row, 'Wild_Reared')))
  return [
    typeof entered !== 'number'
      ? ''
      : wild
        ? t('Capturada {date}', { date: formatSerial(entered) })
        : t('Emergió {date}', { date: formatSerial(entered) }),
    t('fila {row}', { row: item.row.row }),
    item.others.length ? t('por {names}', { names: item.others.join(', ') }) : '',
  ]
    .filter(Boolean)
    .join(' · ')
}
const sample = (row: TableRow) => {
  const cam = text(value(row, 'CAM_ID'))
  return cam && cam !== 'NA' ? cam : ''
}
const status = (item: RecordedItem) =>
  item.status === 'queued' ? t('esperando a Google Sheets') : item.status === 'pending' ? t('por guardar') : ''
const undoTitle = (item: RecordedItem) =>
  item.status === 'queued'
    ? t('Esperando a Google Sheets: se podrá deshacer cuando se escriba')
    : t('Deshacer la muerte de {id}', { id: facts(item).id })
</script>

<template>
  <section class="px-3 pt-6 pb-8" data-recorded>
    <div class="flex flex-wrap items-center gap-x-3 gap-y-2">
      <h2 class="text-sm font-semibold text-stone-700">
        {{ $t('Registradas hoy') }} <span class="font-normal text-stone-500">({{ items.length }})</span>
      </h2>
      <button
        class="grid h-9 w-9 place-items-center rounded-md text-stone-500 active:bg-stone-100"
        :aria-label="$t('Actualizar')"
        :title="$t('Actualizar')"
        :disabled="loading"
        @click="emit('refresh')"
      >
        <RefreshCw :size="15" :class="{ 'animate-spin': loading }" />
      </button>
      <div class="ml-auto flex flex-wrap items-center gap-2">
        <div class="inline-flex overflow-hidden rounded-lg border border-stone-300 text-sm" role="group" :aria-label="$t('De quién')">
          <button class="h-9 px-2.5" :class="!mineOnly ? 'bg-brand-700 text-white' : 'bg-white'" :aria-pressed="!mineOnly" @click="mineOnly = false">
            {{ $t('De todos') }}
          </button>
          <button
            class="h-9 border-l border-stone-300 px-2.5"
            :class="mineOnly ? 'bg-brand-700 text-white' : 'bg-white'"
            :aria-pressed="mineOnly"
            @click="mineOnly = true"
          >
            {{ $t('Mías') }}
          </button>
        </div>
        <div class="inline-flex overflow-hidden rounded-lg border border-stone-300 text-sm" role="group" :aria-label="$t('Ordenar por')">
          <button
            v-for="(s, i) in sorts"
            :key="s.by"
            class="h-9 px-2.5"
            :class="[order.by === s.by ? 'bg-brand-700 text-white' : 'bg-white', i ? 'border-l border-stone-300' : '']"
            :aria-pressed="order.by === s.by"
            @click="sortBy(s.by)"
          >
            {{ s.label }}
          </button>
          <button
            class="grid h-9 w-9 place-items-center border-l border-stone-300 bg-white"
            :aria-label="order.desc ? $t('Descendente: cambiar a ascendente') : $t('Ascendente: cambiar a descendente')"
            :title="order.desc ? $t('Descendente') : $t('Ascendente')"
            @click="flip"
          >
            <ArrowDown v-if="order.desc" :size="16" /><ArrowUp v-else :size="16" />
          </button>
        </div>
      </div>
    </div>
    <p v-if="items.length" class="mt-1 text-xs text-stone-500">
      {{ $t('Las muertes guardadas hoy desde Muertes, para copiarlas al cuaderno en este orden. Tócala para corregirla.') }}
    </p>
    <p v-if="loading && !items.length" class="py-3 text-sm text-stone-500"><Loader2 :size="14" class="inline animate-spin" /> {{ $t('Cargando…') }}</p>
    <p v-else-if="!items.length" class="py-3 text-sm text-stone-500">
      {{ mineOnly ? $t('Ninguna muerte tuya registrada hoy.') : $t('Ninguna muerte registrada hoy.') }}
    </p>
    <template v-for="group in [waiting, saved]" :key="group === waiting ? 'waiting' : 'saved'">
      <ul
        v-if="group.length"
        class="mt-2 divide-y overflow-hidden rounded-xl border"
        :class="group === waiting ? 'divide-amber-100 border-amber-300 bg-amber-50' : 'divide-stone-100 border-stone-200 bg-white'"
        :data-unsaved="group === waiting ? '' : undefined"
      >
        <li
          v-for="item in group"
          :key="item.row.id"
          class="flex items-center"
          :class="editing === item.row.id ? 'bg-brand-50 ring-2 ring-brand-600 ring-inset' : ''"
          :data-recorded-id="facts(item).id"
        >
          <button
            class="flex min-h-12 min-w-0 flex-1 items-center gap-3 px-3 py-1.5 text-left active:bg-stone-50"
            :aria-label="$t('Corregir la muerte de {id}', { id: facts(item).id })"
            :disabled="!canEdit"
            @click="emit('edit', item.row)"
          >
            <span class="w-14 shrink-0 font-semibold">{{ facts(item).id }}</span>
            <span class="min-w-0 flex-1">
              <span class="block truncate text-sm">
                {{ line(item) }}
                <span v-if="status(item)" class="text-xs font-medium text-amber-900"> · {{ status(item) }}</span>
              </span>
              <span class="flex flex-wrap items-center gap-x-1 text-xs text-stone-500">
                <span>{{ text(value(item.row, 'SPECIES')) }}</span>
                <SexBadge :sex="text(value(item.row, 'Sex'))" />
                <span>{{ details(item) }}</span>
              </span>
            </span>
            <span v-if="gapOf(item.row)" class="shrink-0 rounded-md bg-amber-100 px-1.5 py-0.5 text-xs font-medium text-amber-900">{{
              gapOf(item.row)
            }}</span>
            <span v-else-if="sample(item.row)" class="hidden shrink-0 text-xs text-brand-700 sm:inline">{{ sample(item.row) }}</span>
            <Pencil v-if="canEdit" :size="15" class="shrink-0 text-stone-400" />
          </button>
          <button
            v-if="canEdit"
            class="grid h-12 w-12 shrink-0 place-items-center text-stone-600 active:bg-stone-100 disabled:opacity-30"
            :aria-label="undoTitle(item)"
            :title="undoTitle(item)"
            :disabled="item.status === 'queued'"
            @click="emit('undo', item)"
          >
            <Undo2 :size="18" />
          </button>
        </li>
      </ul>
    </template>
  </section>
</template>
