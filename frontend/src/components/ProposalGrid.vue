<script setup lang="ts">
import { computed, ref } from 'vue'
import { RouterLink } from 'vue-router'
import { Check, X } from 'lucide-vue-next'
import { displayValue } from '../lib/cells'
import type { CellValue } from '../lib/types'

/**
 * Rows the assistant wants to change, shown like the sheet: changed cells are
 * green with the old value struck through; new rows (e.g. from a Wikiloc walk)
 * are all green. Unticked rows are left as they are.
 */
export interface ProposalChange {
  index: number
  /** null for a new row not written yet. */
  recordId: string | null
  sheet: string
  /** null for a new row not written yet. */
  row: number | null
  label: string
  create?: boolean
  before: Record<string, CellValue>
  values: Record<string, CellValue>
  current: Record<string, CellValue>
  replaceFormula?: string[]
  note?: string
}
export interface Proposal {
  id: string
  reason: string
  status: 'pending' | 'applying' | 'applied' | 'needs_review' | 'discarded'
  sheets?: string[]
  /** The conversation it comes from (T3 Code, Revisión de datos, a chat). */
  source?: string
  createdAt?: string
  fields: string[]
  types: Record<string, string>
  applied: number[] | null
  changes: ProposalChange[]
}

const props = defineProps<{ proposal: Proposal; busy?: boolean }>()
const emit = defineEmits<{ apply: [indexes: number[]]; discard: [] }>()

const chosen = ref(new Set(props.proposal.changes.map(c => c.index)))
const pending = computed(() => props.proposal.status === 'pending')
const allChosen = computed(() => chosen.value.size === props.proposal.changes.length)

function toggle(index: number) {
  const next = new Set(chosen.value)
  if (!next.delete(index)) next.add(index)
  chosen.value = next
}
function toggleAll() {
  chosen.value = allChosen.value ? new Set() : new Set(props.proposal.changes.map(c => c.index))
}
const show = (field: string, value: CellValue | undefined) =>
  displayValue(value, { key: field, type: (props.proposal.types[field] ?? 'text') as 'text' })
const created = computed(() => props.proposal.changes.filter(c => c.create).length)
const wasApplied = (index: number) => props.proposal.status === 'applied' && (props.proposal.applied ?? []).includes(index)
const statusText = computed(
  () =>
    ({
      applied: `Aplicado en la hoja (${props.proposal.applied?.length ?? props.proposal.changes.length} filas)`,
      needs_review: 'No se pudo aplicar: revisa las filas en la hoja',
      discarded: 'Descartado',
      applying: 'Aplicando…',
    })[props.proposal.status as string] ?? '',
)
</script>

<template>
  <div class="mt-2 rounded-md border border-stone-300 bg-white text-stone-800">
    <p class="border-b border-stone-200 px-2 py-1.5 text-xs font-medium">
      Cambios propuestos · {{ (proposal.sheets ?? [proposal.changes[0]?.sheet]).join(', ') }}
      <template v-if="created"> · {{ created }} {{ created === 1 ? 'fila nueva' : 'filas nuevas' }}</template>
      <span class="font-normal text-stone-500">— {{ proposal.reason }}</span>
    </p>
    <div class="max-h-96 overflow-auto">
      <table class="w-full border-collapse text-xs">
        <thead class="sticky top-0 bg-stone-100 text-left">
          <tr>
            <th class="w-7 border-b border-stone-200 px-1.5 py-1">
              <input
                v-if="pending"
                type="checkbox"
                :checked="allChosen"
                title="Elegir todas"
                aria-label="Elegir todas las filas"
                @change="toggleAll"
              />
            </th>
            <th class="border-b border-stone-200 px-1.5 py-1">Fila</th>
            <th class="border-b border-stone-200 px-1.5 py-1">ID</th>
            <th v-for="f in proposal.fields" :key="f" class="border-b border-stone-200 px-1.5 py-1">{{ f }}</th>
            <th class="border-b border-stone-200 px-1.5 py-1">Motivo</th>
          </tr>
        </thead>
        <tbody>
          <tr
            v-for="c in proposal.changes"
            :key="c.index"
            class="align-top"
            :class="{
              'opacity-45': (pending && !chosen.has(c.index)) || (proposal.status === 'applied' && !wasApplied(c.index)),
            }"
          >
            <td class="border-b border-stone-100 px-1.5 py-1">
              <input
                v-if="pending"
                type="checkbox"
                :checked="chosen.has(c.index)"
                :aria-label="`Aplicar ${c.label}`"
                @change="toggle(c.index)"
              />
              <Check v-else-if="wasApplied(c.index)" :size="14" class="text-brand-700" />
            </td>
            <td class="border-b border-stone-100 px-1.5 py-1 text-stone-500 tabular-nums">
              <span v-if="c.row">{{ c.row }}</span>
              <span v-else class="rounded bg-emerald-100 px-1 text-[11px] font-medium text-emerald-900">nueva</span>
            </td>
            <td class="border-b border-stone-100 px-1.5 py-1 font-medium whitespace-nowrap">
              <RouterLink
                v-if="c.recordId"
                :to="{ path: '/tablas', query: { hoja: c.sheet, buscar: c.label } }"
                class="hover:underline"
              >
                {{ c.label }}
              </RouterLink>
              <template v-else>{{ c.label }}</template>
            </td>
            <td
              v-for="f in proposal.fields"
              :key="f"
              class="border-b border-stone-100 px-1.5 py-1"
              :class="f in c.values ? 'bg-emerald-50 outline outline-1 -outline-offset-1 outline-emerald-300' : 'text-stone-500'"
            >
              <template v-if="f in c.values">
                <span class="font-medium text-emerald-900">{{ show(f, c.values[f]) || 'vacío' }}</span>
                <span v-if="!c.create" class="block text-[11px] text-stone-500">
                  <template v-if="c.replaceFormula?.includes(f)">fórmula: </template>
                  <span class="line-through">{{ show(f, c.before[f]) || 'vacío' }}</span>
                </span>
              </template>
              <template v-else>{{ show(f, c.current[f]) }}</template>
            </td>
            <td class="min-w-64 border-b border-stone-100 px-1.5 py-1 text-stone-600">{{ c.note }}</td>
          </tr>
        </tbody>
      </table>
    </div>
    <div class="flex flex-wrap items-center gap-2 px-2 py-1.5">
      <template v-if="pending">
        <button
          class="btn-primary bg-emerald-700 hover:bg-emerald-800"
          :disabled="busy || !chosen.size"
          @click="
            emit(
              'apply',
              [...chosen].sort((a, b) => a - b),
            )
          "
        >
          <Check :size="15" /> Aplicar {{ chosen.size }} {{ chosen.size === 1 ? 'fila' : 'filas' }}
        </button>
        <button class="btn" :disabled="busy" @click="emit('discard')"><X :size="15" /> Descartar</button>
        <span class="hint">También puedes responder "sí, aplícalo" en el chat.</span>
      </template>
      <span v-else class="text-xs" :class="proposal.status === 'applied' ? 'text-brand-700' : 'text-amber-800'">
        {{ statusText }}
      </span>
    </div>
  </div>
</template>
