<script setup lang="ts">
import { computed, onMounted } from 'vue'
import { Loader2 } from 'lucide-vue-next'
import ChoiceField from '../ChoiceField.vue'
import SexBadge from '../SexBadge.vue'
import type { ParentSex, useParents } from '../../composables/useParents'
import { formatSerial } from '../../lib/dates'
import { t } from '../../lib/i18n'

/**
 * The mother (♀) and father (♂) of a clutch by Insectary ID, female first as
 * the team writes them. Each list starts with the living butterflies of that
 * sex (with species and sex); one of the other sex or not in Insectary_data is
 * pointed out under its box.
 */
const props = defineProps<{
  parents: ReturnType<typeof useParents>
  /** The clutch's species: its butterflies come first in each list. */
  species?: string
}>()
const female = defineModel<string>('female', { required: true })
const male = defineModel<string>('male', { required: true })
onMounted(() => props.parents.load())
const lists = computed(() => ({ female: props.parents.choicesFor('female', props.species), male: props.parents.choicesFor('male', props.species) }))
const upper = (v: string) => v.trim().toUpperCase()
/** What is known of a chosen parent: species and alive or dead. */
function about(id: string): string {
  const p = id.trim() ? props.parents.find(id) : undefined
  if (!p) return ''
  const life = p.alive ? t('vive') : p.death !== null ? t('murió el {date}', { date: formatSerial(p.death) }) : t('murió')
  return [p.species, life].filter(Boolean).join(' · ')
}
const boxes: { sex: ParentSex; label: () => string }[] = [
  { sex: 'female', label: () => t('Madre') },
  { sex: 'male', label: () => t('Padre') },
]
</script>

<template>
  <div>
    <div class="grid grid-cols-2 gap-2">
      <div v-for="b in boxes" :key="b.sex" class="min-w-0">
        <label class="block min-w-0">
          <span class="field-label flex items-center gap-1.5">
            <SexBadge :sex="b.sex" /> {{ b.label() }} · Insectary_ID
          </span>
          <ChoiceField
            :model-value="b.sex === 'female' ? female : male"
            class="field-input h-12 text-base uppercase"
            :options="lists[b.sex]"
            autocapitalize="characters"
            @update:model-value="b.sex === 'female' ? (female = upper($event)) : (male = upper($event))"
          >
            <template #option="{ option }">
              <SexBadge :sex="parents.find(option.value)?.sex" />
              <span class="font-semibold tabular-nums">{{ option.value }}</span>
              <span class="min-w-0 truncate text-xs text-stone-600">{{ parents.find(option.value)?.species }}</span>
              <span v-if="parents.find(option.value)?.alive" class="ml-auto shrink-0 rounded-full bg-brand-50 px-1.5 text-[11px] text-brand-800">{{ $t('vive') }}</span>
            </template>
          </ChoiceField>
        </label>
        <p v-if="about(b.sex === 'female' ? female : male)" class="mt-1 flex items-center gap-1 text-xs text-stone-600">
          <SexBadge :sex="parents.find(b.sex === 'female' ? female : male)?.sex" />
          <span class="min-w-0">{{ about(b.sex === 'female' ? female : male) }}</span>
        </p>
        <p v-if="parents.warning(b.sex === 'female' ? female : male, b.sex)" class="mt-1 text-sm text-amber-900" role="alert">
          {{ parents.warning(b.sex === 'female' ? female : male, b.sex) }}
        </p>
      </div>
    </div>
    <p v-if="parents.loading.value" class="mt-1 text-xs text-stone-500">
      <Loader2 :size="12" class="inline animate-spin" /> {{ $t('Cargando {sheet}…', { sheet: 'Insectary_data' }) }}
    </p>
  </div>
</template>
