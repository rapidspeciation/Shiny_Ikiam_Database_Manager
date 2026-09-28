<script setup lang="ts">
import ChoiceField from './ChoiceField.vue'
import { computed, ref } from 'vue'
import { X, Lock, ExternalLink, Trash2 } from 'lucide-vue-next'
import InsectaryIdFix from './InsectaryIdFix.vue'
import { displayValue, normalizeInput } from '../lib/cells'
import { isSumField } from '../lib/sums'
import type { CellValue, Field, TableRow } from '../lib/types'
import { type PendingCreate, usePending } from '../stores/pending'
import { useSession } from '../stores/session'

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
}>()
const emit = defineEmits<{ close: []; changed: []; removeCreate: [clientId: string] }>()

const pending = usePending()
const session = useSession()
const message = ref('')

const row = computed(() => props.rows.find(r => r.id === props.rowId))
const created = computed(() => props.creates.find(c => c.clientId === props.rowId))
const formulas = computed(() => new Set(row.value ? row.value.formulas : props.createFormulas))
const title = computed(() => {
  const key = props.labelField || session.module(props.module)?.identityFields[0]
  const values = row.value?.values || created.value?.values || {}
  return String((key && values[key]) || (row.value ? `Fila ${row.value.row}` : 'Fila nueva'))
})
/** A saved butterfly's Insectary ID can be corrected from its Insectary_data or Collection_data row. */
const insectaryId = computed(() => {
  if (!row.value?.observed || !session.canEdit || !['Insectary_data', 'Collection_data'].includes(props.module)) return ''
  const id = String(row.value.values.Insectary_ID ?? '').trim()
  return /^(|NA|N\/A)$/i.test(id) ? '' : id
})
const sourceUrl = computed(() => {
  const mod = session.module(props.module)
  if (!row.value || !mod || !session.settings) return null
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
  <div class="fixed inset-0 z-40 flex justify-end bg-black/30" @click.self="emit('close')">
    <aside class="flex h-full w-full max-w-md flex-col bg-white shadow-xl">
      <header class="flex items-center gap-2 border-b border-stone-200 px-4 py-3">
        <div class="min-w-0 flex-1">
          <h2 class="truncate text-lg font-semibold">{{ title }}</h2>
          <p class="text-xs text-stone-500">{{ module }}{{ row ? ` · fila ${row.row}` : ' · se creará al guardar' }}</p>
        </div>
        <a v-if="sourceUrl" :href="sourceUrl" target="_blank" rel="noopener" class="btn-ghost" title="Abrir en Google Sheets">
          <ExternalLink :size="18" />
        </a>
        <button
          v-if="created"
          class="btn-ghost text-red-700"
          title="Quitar fila nueva"
          @click="emit('removeCreate', created.clientId)"
        >
          <Trash2 :size="18" />
        </button>
        <button class="btn-ghost" title="Cerrar" @click="emit('close')"><X :size="20" /></button>
      </header>
      <p v-if="message" class="bg-red-50 px-4 py-2 text-sm text-red-800">{{ message }}</p>
      <InsectaryIdFix v-if="insectaryId" :id="insectaryId" @done="emit('close')" />
      <div class="flex-1 overflow-y-auto px-4 py-2">
        <label v-for="field in columns" :key="field.key" class="block border-b border-stone-100 py-2">
          <span class="flex items-center gap-1 text-xs font-medium text-stone-600">
            {{ field.key }}
            <Lock v-if="!editable(field)" :size="12" class="text-stone-400" />
            <span v-if="row && pending.isDirty(row.id, field.key)" class="ml-auto text-amber-700">
              antes: {{ displayValue(row.values[field.key], field) || 'vacío' }}
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
    </aside>
  </div>
</template>
