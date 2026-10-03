<script setup lang="ts">
import { computed, ref } from 'vue'
import { ChevronDown, ChevronUp } from 'lucide-vue-next'
import ChoiceField from '../ChoiceField.vue'
import { MEDIUMS, type YoungField } from '../../lib/emerged'
import type { RackSuggestion } from '../../lib/deaths'
import { normalizeId } from '../../lib/tubes'
import { t, tx, type Msg } from '../../lib/i18n'

/**
 * How the eggs and larvae on the cards are preserved, for all of them or for
 * the selected cards only: the medium (flash frozen in the dry shipper;
 * ethanol when it fails, which has its own rack), Research_purpose, the rack
 * the tubes come from (the app's for the medium, another one when a run of
 * tubes ends, or a first tube typed) and the first CAM. Folded to one line on
 * phones; the cards follow at once (lib/emerged youngSamples).
 */
export interface Rack extends RackSuggestion {
  label: string
  labelMsg?: Msg
}
const props = defineProps<{
  /** What is shown: the selected cards' shared value (undefined when they differ), else the batch's. */
  shown: { medium?: string; purpose?: string; tube?: string; cam?: string }
  /** The batch's (or the selected cards') rack and first CAM were chosen, not the app's. */
  chosen: { tube: boolean; cam: boolean }
  selected: string[]
  /** Cards with their own value of a field (shown with none selected). */
  own: Record<YoungField, string[]>
  racks: Rack[]
  camFirst: string
  purposes: string[]
  /** Open at first (wide screens); folded on phones. */
  startOpen: boolean
}>()
const emit = defineEmits<{ set: [field: YoungField, value: string]; done: [] }>()

const open = ref(props.startOpen)
const editingRack = ref(false)
const typedTube = ref('')
const typedCam = ref('')
const rackNow = computed(() => props.racks.find(r => r.value === props.shown.tube))
/** The insectary's racks first (crosses and insectary), those of the medium shown on top. */
const insectary = computed(() =>
  props.racks
    .filter(r => r.context === 'Cruces' || r.context === 'Insectario')
    .sort((a, b) => Number(b.medium === props.shown.medium) - Number(a.medium === props.shown.medium)),
)
const others = computed(() => props.racks.filter(r => !insectary.value.includes(r)))
function pickRack(value: string) {
  const rack = props.racks.find(r => r.value === value)
  // A rack of another medium brings its medium (flash frozen and ethanol tubes live in different racks).
  if (rack?.medium && MEDIUMS.includes(rack.medium) && rack.medium !== props.shown.medium) emit('set', 'medium', rack.medium)
  emit('set', 'tubeFrom', normalizeId(value))
  editingRack.value = false
  typedTube.value = ''
}
function useCam() {
  emit('set', 'camFrom', normalizeId(typedCam.value))
  typedCam.value = ''
}
const choice = (on: boolean) => (on ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800 active:bg-stone-100')
const summary = computed(() =>
  [
    props.shown.medium ?? t('varios medios'),
    props.shown.tube ? t('tubos desde {tube}', { tube: props.shown.tube }) : t('varias gradillas'),
    props.shown.cam ? t('CAM desde {cam}', { cam: props.shown.cam }) : t('varios CAM'),
    props.shown.purpose ?? t('varios propósitos'),
  ].join(' · '),
)
const ownText = (field: YoungField) => props.own[field].join(', ')
</script>

<template>
  <div class="rounded-xl border border-violet-200 bg-violet-50/50 text-sm">
    <button class="flex min-h-12 w-full items-center gap-2 px-3 py-1.5 text-left" :aria-expanded="open" @click="open = !open">
      <span class="min-w-0 flex-1">
        <span class="block font-semibold text-violet-950">
          {{ $t('Preservación') }} ·
          {{ selected.length ? $tn(selected.length, 'para {n} seleccionada', 'para {n} seleccionadas') : $t('para todas') }}
        </span>
        <span v-if="!open" class="block truncate text-xs text-stone-600">{{ summary }}</span>
      </span>
      <ChevronUp v-if="open" :size="18" class="shrink-0 text-stone-500" />
      <ChevronDown v-else :size="18" class="shrink-0 text-stone-500" />
    </button>
    <div v-if="open" class="space-y-3 px-3 pb-3">
      <p v-if="selected.length" class="flex items-center gap-2 rounded-lg bg-violet-100 px-2 py-1.5 text-violet-950">
        <span class="min-w-0 flex-1">{{ $t('Lo que elijas aquí va solo a las tarjetas seleccionadas.') }}</span>
        <button class="btn h-10 shrink-0" @click="emit('done')">{{ $t('Listo') }}</button>
      </p>
      <div>
        <h3 class="field-label">T1_Preservation_medium</h3>
        <div class="grid grid-cols-3 gap-1.5">
          <button
            v-for="m in MEDIUMS"
            :key="m"
            class="min-h-11 rounded-lg border px-1 text-sm font-medium"
            :class="choice(shown.medium === m)"
            :aria-pressed="shown.medium === m"
            @click="emit('set', 'medium', m)"
          >
            {{ m }}
          </button>
        </div>
        <p v-if="own.medium.length" class="mt-1 text-xs text-violet-800">{{ $t('Con medio propio: {ids}', { ids: ownText('medium') }) }}</p>
      </div>
      <label class="block">
        <span class="field-label">Research_purpose</span>
        <ChoiceField
          :model-value="shown.purpose ?? ''"
          class="field-input h-11 text-base"
          :options="purposes"
          :placeholder="$t('Distintos: elige uno para todas las seleccionadas')"
          @update:model-value="$event && emit('set', 'purpose', $event)"
        />
        <span v-if="own.purpose.length" class="mt-1 block text-xs text-violet-800">{{ $t('Con propósito propio: {ids}', { ids: ownText('purpose') }) }}</span>
      </label>
      <!-- Where the tubes and CAMs come from. -->
      <div class="rounded-lg border border-stone-200 bg-white p-2.5">
        <div class="flex items-start gap-2">
          <p class="min-w-0 flex-1">
            <span class="block">
              {{ $t('Tubos desde') }} <strong class="font-mono">{{ shown.tube ?? $t('varias gradillas') }}</strong>
              <span v-if="rackNow" class="text-stone-500"> · {{ tx(rackNow.label, rackNow.labelMsg).replace(`${rackNow.value} · `, "") }}</span>
              <span v-else-if="chosen.tube" class="text-stone-500"> · {{ $t('escrito') }}</span>
            </span>
            <span class="block">
              {{ $t('CAM desde') }} <strong class="font-mono">{{ shown.cam ?? $t('varios CAM') }}</strong>
              <span v-if="!chosen.cam" class="text-stone-500"> · {{ $t('el siguiente libre') }}</span>
            </span>
          </p>
          <button class="btn h-11 shrink-0" :aria-expanded="editingRack" @click="editingRack = !editingRack">{{ editingRack ? $t('Cerrar') : $t('Cambiar') }}</button>
        </div>
        <p v-if="own.tubeFrom.length" class="mt-1 text-xs text-violet-800">{{ $t('Con gradilla propia: {ids}', { ids: ownText('tubeFrom') }) }}</p>
        <p v-if="own.camFrom.length" class="mt-1 text-xs text-violet-800">{{ $t('Con CAM propio: {ids}', { ids: ownText('camFrom') }) }}</p>
        <div v-if="editingRack" class="mt-3 space-y-3">
          <div>
            <span class="field-label">{{ $t('Gradilla en uso (siguiente tubo libre)') }}</span>
            <div class="space-y-1.5">
              <button
                v-for="r in insectary"
                :key="r.value"
                class="block min-h-11 w-full rounded-lg border px-3 py-1.5 text-left text-sm"
                :class="choice(shown.tube === r.value)"
                @click="pickRack(r.value)"
              >
                {{ tx(r.label, r.labelMsg) }}
              </button>
              <details v-if="others.length">
                <summary class="min-h-11 py-2 text-xs font-semibold text-stone-500">{{ $t('Colectas y monitoreo') }}</summary>
                <button
                  v-for="r in others"
                  :key="r.value"
                  class="mt-1.5 block min-h-11 w-full rounded-lg border px-3 py-1.5 text-left text-sm"
                  :class="choice(shown.tube === r.value)"
                  @click="pickRack(r.value)"
                >
                  {{ tx(r.label, r.labelMsg) }}
                </button>
              </details>
            </div>
            <form class="mt-2 flex items-end gap-2" @submit.prevent="typedTube && pickRack(typedTube)">
              <label class="min-w-0 flex-1">
                <span class="field-label">{{ $t('o escribe el primer tubo') }}</span>
                <input
                  v-model="typedTube"
                  class="field-input h-11 font-mono text-base uppercase"
                  autocapitalize="characters"
                  autocomplete="off"
                  spellcheck="false"
                  placeholder="FS90415474"
                />
              </label>
              <button class="btn h-11" :disabled="!typedTube">{{ $t('Usar') }}</button>
            </form>
            <button v-if="chosen.tube" class="mt-2 min-h-10 text-sm underline" @click="emit('set', 'tubeFrom', '')">{{ $t('Que la app elija la gradilla') }}</button>
          </div>
          <form class="flex items-end gap-2" @submit.prevent="useCam">
            <label class="min-w-0 flex-1">
              <span class="field-label">{{ $t('Primer CAM') }}</span>
              <input
                v-model="typedCam"
                class="field-input h-11 font-mono text-base uppercase"
                autocapitalize="characters"
                autocomplete="off"
                spellcheck="false"
                :placeholder="shown.cam || camFirst"
              />
            </label>
            <button class="btn h-11" :disabled="!typedCam">{{ $t('Usar') }}</button>
          </form>
          <p class="text-xs text-stone-500">
            {{ $t('Vacío: el siguiente libre ({cam}).', { cam: camFirst }) }}
            <button v-if="chosen.cam" class="ml-1 underline" @click="emit('set', 'camFrom', '')">{{ $t('Volver al siguiente libre') }}</button>
          </p>
        </div>
      </div>
      <p class="text-xs text-stone-600">{{ $t('Un CAM o tubo escrito en una tarjeta hace que las siguientes sigan desde él. Marca tarjetas para darles su propio medio, gradilla o CAM.') }}</p>
    </div>
  </div>
</template>
