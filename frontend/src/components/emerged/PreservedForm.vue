<script setup lang="ts">
import { computed } from 'vue'
import { AlertTriangle, Plus } from 'lucide-vue-next'
import StagePicker from './StagePicker.vue'
import { youngNote } from '../../lib/emerged'

/**
 * «+ Preservados…»: eggs, larvae or pupae preserved from the clutch, each with
 * its Insectary ID, CAM and tube (Sex NOT_COLLECTED, flash frozen, F1/F2
 * mutation rate: the batch panel below). How many, their LIFESTAGE, alive or
 * found dead, and an optional note in English (saved dated and signed; empty:
 * «Preserved alive 3rd instar»). «+ 1 más igual» adds one more like the last
 * ones with the next ID. Opened from Clutches (`forAll`), the stage, fate and
 * note go to all its cards, and there is no count (Clutches gave it).
 */
const props = defineProps<{
  stage: string
  foundDead: boolean
  note: string
  /** How many «Añadir» adds (absent: opened from Clutches, only «+ 1 más igual»). */
  count?: number | null
  /** The IDs «Añadir» would give (as many as there are free). */
  ids?: string[]
  /** The ID «+ 1 más igual» gives. */
  next: string | null
  /** What «+ 1 más igual» repeats, short (L3 · found dead · note), or null before any was added. */
  same: string | null
  forAll?: boolean
  canEdit: boolean
}>()
const emit = defineEmits<{
  'update:stage': [stage: string]
  'update:foundDead': [dead: boolean]
  'update:note': [note: string]
  'update:count': [count: number | null]
  add: []
  same: []
}>()
const counting = computed(() => props.count !== undefined)
const n = computed(() => Math.max(1, Math.min(60, Math.floor(props.count ?? 1))))
const ids = computed(() => props.ids ?? [])
const choice = (on: boolean, color = 'border-brand-700 bg-brand-700 text-white') => (on ? color : 'border-stone-300 bg-white text-stone-800 active:bg-stone-100')
</script>

<template>
  <form class="space-y-2 rounded-lg border border-violet-200 bg-violet-50/60 p-2" @submit.prevent="counting ? emit('add') : emit('same')">
    <div class="flex items-center gap-2">
      <span class="min-w-0 flex-1 text-sm font-medium text-violet-950">
        {{ forAll ? $t('Para todas las de este clutch') : $t('Preservados del clutch') }}
      </span>
      <span v-if="counting" class="flex items-stretch overflow-hidden rounded-lg border border-stone-300 bg-white">
        <button type="button" class="h-11 w-11 text-xl active:bg-stone-100" :aria-label="$t('Una menos')" @click="emit('update:count', Math.max(1, n - 1))">−</button>
        <input
          :value="count"
          type="number"
          inputmode="numeric"
          min="1"
          max="60"
          class="h-11 w-14 border-x border-stone-300 text-center text-lg font-semibold tabular-nums outline-none"
          :aria-label="$t('Cuántas')"
          @input="emit('update:count', ($event.target as HTMLInputElement).value === '' ? null : Number(($event.target as HTMLInputElement).value))"
        />
        <button type="button" class="h-11 w-11 text-xl active:bg-stone-100" :aria-label="$t('Una más')" @click="emit('update:count', Math.min(60, n + 1))">+</button>
      </span>
    </div>
    <StagePicker :model-value="stage" :disabled="!canEdit" @update:model-value="emit('update:stage', $event)" />
    <div class="grid grid-cols-2 gap-1.5">
      <button type="button" class="min-h-11 rounded-lg border text-sm font-medium" :class="choice(!foundDead)" :aria-pressed="!foundDead" @click="emit('update:foundDead', false)">
        {{ $t('Vivas, preservadas') }}
      </button>
      <button
        type="button"
        class="min-h-11 rounded-lg border text-sm font-medium"
        :class="choice(foundDead, 'border-amber-700 bg-amber-700 text-white')"
        :aria-pressed="foundDead"
        @click="emit('update:foundDead', true)"
      >
        {{ $t('Encontradas muertas') }}
      </button>
    </div>
    <label class="block">
      <span class="field-label">{{ $t('Nota (opcional, en inglés)') }}</span>
      <input
        :value="note"
        class="field-input h-11 text-base"
        :placeholder="$t('Por defecto: {note}', { note: youngNote(stage, foundDead) })"
        enterkeyhint="done"
        autocomplete="off"
        @input="emit('update:note', ($event.target as HTMLInputElement).value)"
      />
    </label>
    <button v-if="counting" class="btn-primary flex h-12 w-full touch-manipulation flex-col justify-center text-base leading-tight select-none" :disabled="!ids.length">
      <span>{{ $tn(n, 'Añadir {n} preservado', 'Añadir {n} preservados') }}</span>
      <span v-if="ids.length" class="text-xs font-normal opacity-90">{{ ids.length > 1 ? `${ids[0]}–${ids.at(-1)}` : ids[0] }}</span>
    </button>
    <p v-if="counting && ids.length && ids.length < n" class="flex items-start gap-1.5 text-sm text-amber-900">
      <AlertTriangle :size="15" class="mt-0.5 shrink-0" />
      {{ $t('Solo hay {n} filas preasignadas libres desde {id}: crea más filas preasignadas en Insectary_data', { n: ids.length, id: ids[0] }) }}
    </p>
    <!-- One more like the last ones, with the next ID (as «Sin sexo» for adults): many larvae are a few taps. -->
    <button
      v-if="same || !counting"
      type="button"
      class="btn flex h-auto min-h-12 w-full touch-manipulation flex-col justify-center py-1 text-base leading-tight select-none"
      :disabled="!next"
      @click="emit('same')"
    >
      <span class="flex items-center gap-1"><Plus :size="16" /> {{ $t('1 más igual') }}</span>
      <span v-if="next" class="max-w-full truncate text-xs font-normal text-stone-600 tabular-nums">{{ same ? `${same} · ` : '' }}{{ next }}</span>
    </button>
  </form>
</template>
