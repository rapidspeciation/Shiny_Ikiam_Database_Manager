<script setup lang="ts">
import { computed, ref } from 'vue'
import { ArrowRightLeft, Pencil } from 'lucide-vue-next'
import { api, requestId } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import { usePending } from '../stores/pending'
import { useTables } from '../stores/tables'
import { t } from '../lib/i18n'

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

type Piece = { text: string; tag?: 'b' | 'i' }
/** A translated sentence with its placeholders filled, keeping IDs bold and species in italics. */
function pieces(sentence: string, vars: Record<string, [string | number, ('b' | 'i')?]>): Piece[] {
  return sentence
    .split(/(\{\w+\})/)
    .filter(Boolean)
    .map(s => {
      const v = /^\{\w+\}$/.test(s) ? vars[s.slice(1, -1)] : undefined
      return v ? { text: String(v[0]), tag: v[1] } : { text: s }
    })
}
const planText = computed(() => {
  const p = plan.value
  if (!p) return []
  const vars: Record<string, [string | number, ('b' | 'i')?]> = {
    from: [p.from, 'b'],
    to: [p.to, 'b'],
    free: [p.from],
    row: [p.source.row],
    target: [p.target.row],
    species: [p.source.species, 'i'],
    sex: [p.source.sex],
    otherSpecies: [p.target.species, 'i'],
    otherSex: [p.target.sex],
  }
  return p.mode === 'move'
    ? pieces(
        t(
          'Los datos de {from} (fila {row}, {species} {sex}) pasan a la fila preparada de {to} (fila {target}); {free} queda libre.',
        ),
        vars,
      )
    : pieces(
        t(
          '{from} ({species} {sex}) y {to} ({otherSpecies} {otherSex}) intercambian sus datos: cada mariposa queda con el otro ID.',
        ),
        vars,
      )
})

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
    await api('insectary-ids/change', {
      method: 'POST',
      body: { from: plan.value.from, to: plan.value.to, requestId: requestId() },
    })
    const sheets = new Set(['Insectary_data', ...plan.value.references.map(r => r.sheet)])
    await Promise.all([...sheets].filter(s => tables.tables[s]).map(s => tables.load(s, true)))
    notify(
      plan.value.mode === 'move'
        ? t('Insectary ID corregido: {from} → {to}', { from: plan.value.from, to: plan.value.to })
        : t('IDs intercambiados: {from} ↔ {to}', { from: plan.value.from, to: plan.value.to }),
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
      <Pencil :size="14" /> {{ $t('Corregir Insectary ID ({id})', { id }) }}
    </button>
    <template v-else>
      <p class="mb-2 text-xs text-stone-600">
        {{
          $t(
            'Si las alas llevan otro ID: los datos pasan a la fila de ese ID en Insectary_data (o se intercambian si ese ID ya es de otra mariposa), y el ID se corrige en Collection_data y en las hojas de cruces. Se puede deshacer en Historial.',
          )
        }}
      </p>
      <form class="flex items-end gap-2" @submit.prevent="preview">
        <label class="flex-1">
          <span class="field-label">{{ $t('ID correcto (el de las alas)') }}</span>
          <input v-model="to" class="field-input uppercase" autocapitalize="characters" :placeholder="$t('p. ej. N9D')" />
        </label>
        <button class="btn">{{ $t('Ver cambios') }}</button>
      </form>
      <p v-if="problem" class="mt-2 rounded bg-red-50 px-2 py-1 text-red-800">{{ problem }}</p>
      <div v-if="plan" class="mt-2 space-y-1.5">
        <p :class="{ 'flex gap-1.5': plan.mode === 'swap' }">
          <ArrowRightLeft v-if="plan.mode === 'swap'" :size="15" class="mt-0.5 shrink-0" />
          <span
            ><template v-for="(p, i) in planText" :key="i"
              ><b v-if="p.tag === 'b'">{{ p.text }}</b
              ><i v-else-if="p.tag === 'i'">{{ p.text }}</i
              ><template v-else>{{ p.text }}</template></template
            ></span
          >
        </p>
        <p class="text-xs text-stone-600">
          {{ $t('{n} columnas de Insectary_data cambian de fila.', { n: plan.moved.length }) }}
        </p>
        <p v-if="plan.skipped.length" class="text-xs text-stone-600">
          {{ $t('Calculadas por fórmula en la fila de destino (se recalculan): {fields}.', { fields: plan.skipped.join(', ') }) }}
        </p>
        <div v-if="plan.references.length">
          <p class="text-xs font-medium text-stone-600">{{ $t('También se corrige:') }}</p>
          <ul class="text-xs">
            <li v-for="r in plan.references" :key="`${r.sheet}:${r.row}:${r.field}`">
              {{ $t('{sheet} fila {row}', { sheet: r.sheet, row: r.row }) }} · {{ r.field }}: {{ r.before }} →
              <b>{{ r.after }}</b>
            </li>
          </ul>
        </div>
        <p v-if="unsaved" class="rounded bg-amber-50 px-2 py-1 text-amber-900">
          {{ $t('Hay cambios sin guardar: guárdalos (o descártalos) antes de corregir el ID.') }}
        </p>
        <div class="flex gap-2 pt-1">
          <button class="btn-primary" :disabled="busy || unsaved" @click="apply">
            {{ busy ? $t('Guardando…') : $t('Confirmar') }}
          </button>
          <button class="btn" @click="cancel">{{ $t('Cancelar') }}</button>
        </div>
      </div>
    </template>
  </section>
</template>
