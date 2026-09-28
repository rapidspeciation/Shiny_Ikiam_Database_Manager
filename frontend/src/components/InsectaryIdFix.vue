<script setup lang="ts">
import { computed, ref } from 'vue'
import { ArrowRightLeft, Pencil } from 'lucide-vue-next'
import { api, requestId } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import { usePending } from '../stores/pending'
import { useTables } from '../stores/tables'

/**
 * Correcting a saved Insectary ID (the wings carry another one). The ID
 * belongs to its row in Insectary_data, so the butterfly's data moves to the
 * pre-made row of the right ID, or is exchanged with the butterfly holding it;
 * Collection_data and the crosses sheets follow. Previewed, then one save that
 * Historial can undo.
 */
const props = defineProps<{ id: string }>()
const emit = defineEmits<{ done: [] }>()

interface Plan {
  mode: 'move' | 'swap'
  from: string
  to: string
  source: { row: number; species: string; sex: string }
  target: { row: number; species: string; sex: string }
  moved: string[]
  skipped: string[]
  references: { sheet: string; row: number; field: string; before: string; after: string }[]
}
const pending = usePending()
const tables = useTables()
const open = ref(false)
const to = ref('')
const plan = ref<Plan | null>(null)
const problem = ref('')
const busy = ref(false)
const unsaved = computed(() => pending.changeCount > 0)

async function preview() {
  problem.value = ''
  plan.value = null
  if (!to.value.trim()) return
  try {
    plan.value = await api<Plan>(
      `insectary-ids/plan?from=${encodeURIComponent(props.id)}&to=${encodeURIComponent(to.value.trim())}`,
    )
  } catch (e) {
    problem.value = errorText(e)
  }
}
function cancel() {
  open.value = false
  plan.value = null
  to.value = ''
}
async function apply() {
  if (!plan.value) return
  busy.value = true
  try {
    await api('insectary-ids/change', { method: 'POST', body: { from: plan.value.from, to: plan.value.to, requestId: requestId() } })
    const sheets = new Set(['Insectary_data', ...plan.value.references.map(r => r.sheet)])
    await Promise.all([...sheets].filter(s => tables.tables[s]).map(s => tables.load(s, true)))
    notify(
      plan.value.mode === 'move' ? `Insectary ID corregido: ${plan.value.from} → ${plan.value.to}` : `IDs intercambiados: ${plan.value.from} ↔ ${plan.value.to}`,
      'success',
    )
    emit('done')
  } catch (e) {
    problem.value = errorText(e)
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <section class="border-b border-stone-200 bg-stone-50 px-4 py-2 text-sm">
    <button v-if="!open" class="flex items-center gap-1.5 text-brand-700 hover:underline" @click="open = true">
      <Pencil :size="14" /> Corregir Insectary ID ({{ id }})
    </button>
    <template v-else>
      <p class="mb-2 text-xs text-stone-600">
        Si las alas llevan otro ID: los datos pasan a la fila de ese ID en Insectary_data (o se intercambian si ese ID ya es de
        otra mariposa), y el ID se corrige en Collection_data y en las hojas de cruces. Se puede deshacer en Historial.
      </p>
      <form class="flex items-end gap-2" @submit.prevent="preview">
        <label class="flex-1">
          <span class="field-label">ID correcto (el de las alas)</span>
          <input v-model="to" class="field-input uppercase" autocapitalize="characters" placeholder="p. ej. N9D" />
        </label>
        <button class="btn">Ver cambios</button>
      </form>
      <p v-if="problem" class="mt-2 rounded bg-red-50 px-2 py-1 text-red-800">{{ problem }}</p>
      <div v-if="plan" class="mt-2 space-y-1.5">
        <p v-if="plan.mode === 'move'">
          Los datos de <b>{{ plan.from }}</b> (fila {{ plan.source.row }}, <i>{{ plan.source.species }}</i> {{ plan.source.sex }})
          pasan a la fila preparada de <b>{{ plan.to }}</b> (fila {{ plan.target.row }}); {{ plan.from }} queda libre.
        </p>
        <p v-else class="flex gap-1.5">
          <ArrowRightLeft :size="15" class="mt-0.5 shrink-0" />
          <span
            ><b>{{ plan.from }}</b> (<i>{{ plan.source.species }}</i> {{ plan.source.sex }}) y <b>{{ plan.to }}</b> (<i>{{
              plan.target.species
            }}</i>
            {{ plan.target.sex }}) intercambian sus datos: cada mariposa queda con el otro ID.</span
          >
        </p>
        <p class="text-xs text-stone-600">{{ plan.moved.length }} columnas de Insectary_data cambian de fila.</p>
        <p v-if="plan.skipped.length" class="text-xs text-stone-600">
          Calculadas por fórmula en la fila de destino (se recalculan): {{ plan.skipped.join(', ') }}.
        </p>
        <div v-if="plan.references.length">
          <p class="text-xs font-medium text-stone-600">También se corrige:</p>
          <ul class="text-xs">
            <li v-for="r in plan.references" :key="`${r.sheet}:${r.row}:${r.field}`">
              {{ r.sheet }} fila {{ r.row }} · {{ r.field }}: {{ r.before }} → <b>{{ r.after }}</b>
            </li>
          </ul>
        </div>
        <p v-if="unsaved" class="rounded bg-amber-50 px-2 py-1 text-amber-900">
          Hay cambios sin guardar: guárdalos (o descártalos) antes de corregir el ID.
        </p>
        <div class="flex gap-2 pt-1">
          <button class="btn-primary" :disabled="busy || unsaved" @click="apply">
            {{ busy ? 'Guardando…' : 'Confirmar' }}
          </button>
          <button class="btn" @click="cancel">Cancelar</button>
        </div>
      </div>
    </template>
  </section>
</template>
