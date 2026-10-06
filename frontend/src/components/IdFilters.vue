<script setup lang="ts">
import { computed } from 'vue'
import type { SexFilter } from '../lib/idMatch'
import { t } from '../lib/i18n'

/**
 * What is seen on the butterfly in hand, to rank the IDs that fit what was
 * typed (lib/idMatch.ts): its sex (♀, ♂, ? or any) and, when a list is given,
 * its species. Big buttons for a thumb; they never take the focus from the ID
 * box (the keyboard stays open).
 */
const sex = defineModel<SexFilter>('sex', { required: true })
const species = defineModel<string>('species', { default: '' })
defineProps<{
  /** Species to choose from (Muertes); none: the species is fixed (Censo) or not asked. */
  speciesList?: { species: string; alive: number }[]
}>()
const sexes = computed<{ value: SexFilter; label: string; title: string }[]>(() => [
  { value: '', label: t('Todas'), title: t('Cualquier sexo') },
  { value: 'female', label: '♀', title: 'female' },
  { value: 'male', label: '♂', title: 'male' },
  { value: 'unknown', label: '?', title: t('Sexo desconocido') },
])
</script>

<template>
  <div class="flex flex-wrap items-center gap-2">
    <div class="flex overflow-hidden rounded-lg border border-stone-300 bg-white" role="group" :aria-label="$t('Sexo visto')">
      <button
        v-for="s in sexes"
        :key="s.value"
        type="button"
        class="h-10 min-w-11 border-l border-stone-200 px-2 font-semibold first:border-l-0"
        :class="[
          s.value === 'female' || s.value === 'male' ? 'text-xl leading-none' : 'text-sm',
          sex === s.value
            ? s.value === 'female'
              ? 'bg-pink-600 text-white'
              : s.value === 'male'
                ? 'bg-sky-600 text-white'
                : 'bg-brand-700 text-white'
            : 'text-stone-700 active:bg-stone-100',
        ]"
        :aria-pressed="sex === s.value"
        :title="s.title"
        @mousedown.prevent
        @click="sex = s.value"
      >
        {{ s.label }}
      </button>
    </div>
    <select
      v-if="speciesList"
      v-model="species"
      class="h-10 max-w-60 min-w-0 flex-1 rounded-lg border border-stone-300 bg-white px-2 text-sm"
      :aria-label="$t('Especie vista')"
    >
      <option value="">{{ $t('Cualquier especie') }}</option>
      <option v-for="s in speciesList" :key="s.species" :value="s.species">{{ s.species }} ({{ s.alive }})</option>
    </select>
  </div>
</template>
