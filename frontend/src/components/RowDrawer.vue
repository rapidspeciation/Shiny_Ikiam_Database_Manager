<script setup lang="ts">
import ChoiceField from './ChoiceField.vue'
import { computed, ref } from 'vue'
import { X, Lock, ExternalLink, Trash2, ChevronLeft, ChevronRight, Search } from 'lucide-vue-next'
import InsectaryIdFix from './InsectaryIdFix.vue'
import { displayValue, normalizeInput } from '../lib/cells'
import { isSumField } from '../lib/sums'
import type { CellValue, Field, TableRow } from '../lib/types'
import { type PendingCreate, usePending } from '../stores/pending'
import { useSession } from '../stores/session'
import { t } from '../lib/i18n'

/** Vertical view of one row: the easiest way to edit a record on a phone. */
const props = defineProps<{
  module: string
  rowId: string
  rows: TableRow[]
  creates: PendingCreate[]
  columns: Field[]
  options: Record<string, string[]>
  lockedFields: string[]
  createFormulas: string[]
  labelField?: string
  /** Prototype (ui-rowform-proto): shown in place of the grid on phones, one row at a time. */
  inline?: boolean
  /** Prototype: rows in the grid's order, for previous / next and «Ir a…». */
  order?: { id: string; row: number; label: string; hint: string }[]
}>()
const emit = defineEmits<{ close: []; changed: []; removeCreate: [clientId: string]; go: [id: string] }>()

// Prototype navigation: where this row sits in the grid's order.
const at = computed(() => (props.order || []).findIndex(o => o.id === props.rowId))
const jumpOpen = ref(false)
const jumpText = ref('')
const jumpMatches = computed(() => {
  const q = jumpText.value.trim().toLowerCase()
  const all = props.order || []
  if (!q) return all.slice(0, 8)
  return all.filter(o => o.label.toLowerCase().startsWith(q) || String(o.row).startsWith(q)).slice(0, 8)
})
function go(step: number) {
  const next = (props.order || [])[at.value + step]
  if (next) emit('go', next.id)
}
function jump(id: string) {
  jumpOpen.value = false
  jumpText.value = ''
  emit('go', id)
}

const pending = usePending()
const session = useSession()
const message = ref('')

const row = computed(() => props.rows.find(r => r.id === props.rowId))
const created = computed(() => props.creates.find(c => c.clientId === props.rowId))
const formulas = computed(() => new Set(row.value ? row.value.formulas : props.createFormulas))
const title = computed(() => {
  const key = props.labelField || session.module(props.module)?.identityFields[0]
  const values = row.value?.values || created.value?.values || {}
  return String((key && values[key]) || (row.value ? t('Fila {row}', { row: row.value.row }) : t('Fila nueva')))
})
/** A saved butterfly's Insectary ID can be corrected from its Insectary_data or Collection_data row. */
const insectaryId = computed(() => {
  if (!row.value?.observed || !session.canEdit || !['Insectary_data', 'Collection_data'].includes(props.module)) return ''
  const id = String(row.value.values.Insectary_ID ?? '').trim()
  return /^(|NA|N\/A)$/i.test(id) ? '' : id
})
const sourceUrl = computed(() => {
  const mod = session.module(props.module)
  if (!row.value || !mod || !session.settings?.sheetUrl) return null
  return `${session.settings.sheetUrl}#gid=${mod.sheetId}&range=A${row.value.row}`
})

function current(field: Field): CellValue {
  if (created.value) return created.value.values[field.key] ?? null
  return row.value ? pending.value(row.value, field.key) : null
}
function editable(field: Field) {
  return (
    session.canEdit &&
    !field.readonly &&
    !props.lockedFields.includes(field.key) &&
    (!formulas.value.has(field.key) || isSumField(props.module, field.key))
  )
}
function change(field: Field, text: string) {
  const result = normalizeInput(text, field, props.module)
  if (!result.ok) {
    message.value = result.message
    return
  }
  message.value = ''
  if (created.value) pending.updateCreate(created.value.clientId, field.key, result.value)
  else if (row.value) {
    const key = props.labelField || session.module(props.module)?.identityFields[0]
    pending.setCell(props.module, row.value, String((key && row.value.values[key]) || ''), field.key, result.value)
  }
  emit('changed')
}
</script>

<template>
  <div
    :class="inline ? 'rowform-proto relative flex h-full flex-col' : 'fixed inset-0 z-40 flex justify-end bg-black/30'"
    @click.self="!inline && emit('close')"
  >
    <aside :class="inline ? 'flex min-h-0 flex-1 flex-col bg-surface' : 'flex h-full w-full max-w-md flex-col bg-surface shadow-xl'">
      <header class="flex items-center gap-2 border-b border-stone-200 px-4 py-3">
        <div class="min-w-0 flex-1">
          <h2 class="truncate text-lg font-semibold">{{ title }}</h2>
          <p class="text-xs text-stone-500">
            {{ module }}{{ row ? ` · ${$t('fila {row}', { row: row.row })}` : ` · ${$t('se creará al guardar')}` }}
            <template v-if="inline && order && at >= 0">
              · {{ (at + 1).toLocaleString('es') }} de {{ order.length.toLocaleString('es') }}</template
            >
          </p>
        </div>
        <a
          v-if="sourceUrl"
          :href="sourceUrl"
          target="_blank"
          rel="noopener"
          class="btn-ghost"
          :title="$t('Abrir en Google Sheets')"
        >
          <ExternalLink :size="18" />
        </a>
        <button
          v-if="created"
          class="btn-ghost text-red-700"
          :title="$t('Quitar fila nueva')"
          @click="emit('removeCreate', created.clientId)"
        >
          <Trash2 :size="18" />
        </button>
        <button v-if="!inline" class="btn-ghost" :title="$t('Cerrar')" @click="emit('close')"><X :size="20" /></button>
      </header>
      <p v-if="message" class="bg-red-50 px-4 py-2 text-sm text-red-800">{{ message }}</p>
      <InsectaryIdFix v-if="insectaryId" :id="insectaryId" @done="emit('close')" />
      <div class="rowform-fields flex-1 overflow-y-auto px-4 py-2">
        <label v-for="field in columns" :key="field.key" class="block border-b border-stone-100 py-2">
          <span class="flex items-center gap-1 text-xs font-medium text-stone-600">
            {{ field.key }}
            <Lock v-if="!editable(field)" :size="12" class="text-stone-400" />
            <span v-if="row && pending.isDirty(row.id, field.key)" class="ml-auto text-amber-700">
              {{ $t('antes: {value}', { value: displayValue(row.values[field.key], field) || $t('vacío') }) }}
            </span>
          </span>
          <!-- Columns with a list of values: the grid's dropdown (typed values are completed to the first suggestion). -->
          <div v-if="editable(field) && options[field.key]?.length" class="mt-1">
            <ChoiceField
              class="field-input"
              :class="{ 'is-dirty': row && pending.isDirty(row.id, field.key) }"
              :model-value="displayValue(current(field), field)"
              :options="options[field.key]"
              @update:model-value="change(field, $event)"
            />
          </div>
          <input
            v-else-if="editable(field)"
            class="field-input mt-1"
            :class="{ 'is-dirty': row && pending.isDirty(row.id, field.key) }"
            :value="displayValue(current(field), field)"
            :placeholder="field.type === 'date' ? '14-Aug-25' : ''"
            @change="change(field, ($event.target as HTMLInputElement).value)"
          />
          <p v-else class="mt-1 min-h-6 text-sm text-stone-500">{{ displayValue(current(field), field) || '—' }}</p>
        </label>
      </div>
      <!-- Prototype: previous / next and «Ir a…», at the bottom where a thumb reaches them. -->
      <nav v-if="inline" class="flex items-center gap-2 border-t border-stone-200 bg-surface px-3 py-2">
        <button type="button" class="btn min-h-11 px-3" :disabled="at <= 0" @click="go(-1)">
          <ChevronLeft :size="18" /> {{ $t('Anterior') }}
        </button>
        <button type="button" class="rowform-jump-open btn min-h-11 flex-1 justify-center" @click="jumpOpen = !jumpOpen">
          <Search :size="16" /> {{ $t('Ir a…') }}
        </button>
        <button type="button" class="btn min-h-11 px-3" :disabled="at < 0 || at >= (order?.length || 0) - 1" @click="go(1)">
          {{ $t('Siguiente') }} <ChevronRight :size="18" />
        </button>
      </nav>
      <div
        v-if="inline && jumpOpen"
        class="rowform-jump absolute inset-x-0 bottom-15 z-10 border-t border-stone-200 bg-surface px-3 pt-3 pb-2 shadow-xl"
      >
        <label class="block text-xs font-medium text-stone-600">
          {{ $t('Ir a la fila: escribe su ID o su número de fila') }}
          <input v-model="jumpText" class="field-input mt-1" inputmode="text" autocomplete="off" placeholder="R6E · 13698" />
        </label>
        <ul class="mt-2 max-h-72 overflow-y-auto">
          <li v-for="o in jumpMatches" :key="o.id">
            <button
              type="button"
              class="flex min-h-11 w-full items-center gap-3 border-b border-stone-100 px-1 text-left"
              @click="jump(o.id)"
            >
              <span class="w-14 shrink-0 font-semibold">{{ o.label }}</span>
              <span class="w-14 shrink-0 text-right text-xs text-stone-500 tabular-nums">{{ o.row }}</span>
              <span class="min-w-0 truncate text-sm text-stone-600">{{ o.hint }}</span>
            </button>
          </li>
          <li v-if="!jumpMatches.length" class="py-2 text-sm text-stone-500">{{ $t('Ninguna fila con ese ID o número.') }}</li>
        </ul>
      </div>
    </aside>
  </div>
</template>
