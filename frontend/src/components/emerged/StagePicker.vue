<script setup lang="ts">
import { computed, ref } from 'vue'
import { PUPA_STAGES, stageGroup } from '../../lib/emerged'
import { t } from '../../lib/i18n'

/**
 * The LIFESTAGE of an egg, larva or pupa preserved, as the sheet's list writes
 * it: Egg, 1st–5th instar larva, Pre-pupa, Pupa day 1–12. One tap for the egg,
 * an instar or the prepupa; «Pupa» opens its days (nothing is chosen until a
 * day is tapped). Used by «+ Preservados…» and each card.
 */
const props = defineProps<{
  modelValue: string
  disabled?: boolean
  /** A card: a pupa's days stay folded under «Pupa dN» until it is tapped (the form keeps them open). */
  fold?: boolean
}>()
const emit = defineEmits<{ 'update:modelValue': [stage: string] }>()

const MAIN: { value: string; label: () => string }[] = [
  { value: 'Egg', label: () => t('Huevo') },
  { value: '1st instar larva', label: () => 'L1' },
  { value: '2nd instar larva', label: () => 'L2' },
  { value: '3rd instar larva', label: () => 'L3' },
  { value: '4th instar larva', label: () => 'L4' },
  { value: '5th instar larva', label: () => 'L5' },
  { value: 'Pre-pupa', label: () => 'Pre-pupa' },
]
const isPupa = computed(() => stageGroup(props.modelValue) === 'pupa')
const pupaOpen = ref(false)
const showDays = computed(() => pupaOpen.value || (isPupa.value && !props.fold))
const day = computed(() => (isPupa.value ? props.modelValue.replace('Pupa day ', '') : ''))
function pick(stage: string) {
  pupaOpen.value = false
  emit('update:modelValue', stage)
}
const choice = (on: boolean) => (on ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800 active:bg-stone-100')
</script>

<template>
  <div class="space-y-1">
    <div class="grid grid-cols-4 gap-1 sm:grid-cols-8" role="group" aria-label="LIFESTAGE">
      <button
        v-for="s in MAIN"
        :key="s.value"
        type="button"
        class="min-h-11 rounded-lg border px-1 text-sm font-medium"
        :class="choice(modelValue === s.value)"
        :aria-pressed="modelValue === s.value"
        :title="s.value"
        :disabled="disabled"
        @click="pick(s.value)"
      >
        {{ s.label() }}
      </button>
      <button
        type="button"
        class="min-h-11 rounded-lg border px-1 text-sm font-medium"
        :class="choice(isPupa)"
        :aria-pressed="isPupa"
        :aria-expanded="showDays"
        :title="$t('Pupa: elige su día')"
        :disabled="disabled"
        @click="pupaOpen = !pupaOpen"
      >
        {{ isPupa ? $t('Pupa d{n}', { n: day }) : $t('Pupa…') }}
      </button>
    </div>
    <div v-if="showDays" class="rounded-lg border border-stone-200 bg-stone-50 p-1">
      <p class="px-1 pb-1 text-xs text-stone-600">{{ $t('Día de pupa') }}</p>
      <div class="grid grid-cols-6 gap-1" role="group" :aria-label="$t('Día de pupa')">
        <button
          v-for="(p, i) in PUPA_STAGES"
          :key="p"
          type="button"
          class="min-h-10 rounded-lg border text-sm font-medium tabular-nums"
          :class="choice(modelValue === p)"
          :aria-pressed="modelValue === p"
          :title="p"
          :disabled="disabled"
          @click="pick(p)"
        >
          {{ i + 1 }}
        </button>
      </div>
    </div>
  </div>
</template>
