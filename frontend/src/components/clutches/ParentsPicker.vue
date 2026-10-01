<script setup lang="ts">
import { onMounted } from 'vue'
import { Loader2 } from 'lucide-vue-next'
import ChoiceField from '../ChoiceField.vue'
import type { useParents } from '../../composables/useParents'

/** The mother (♀) and father (♂) of a clutch by Insectary ID, female first as the team writes them. */
const props = defineProps<{ parents: ReturnType<typeof useParents> }>()
const female = defineModel<string>('female', { required: true })
const male = defineModel<string>('male', { required: true })
onMounted(() => props.parents.load())
const upper = (v: string) => v.trim().toUpperCase()
</script>

<template>
  <div>
    <div class="grid grid-cols-2 gap-2">
      <label class="block min-w-0">
        <span class="field-label">♀ Insectary_ID</span>
        <ChoiceField
          :model-value="female"
          class="field-input h-12 text-base uppercase"
          :options="parents.females.value"
          autocapitalize="characters"
          @update:model-value="female = upper($event)"
        />
      </label>
      <label class="block min-w-0">
        <span class="field-label">♂ Insectary_ID</span>
        <ChoiceField
          :model-value="male"
          class="field-input h-12 text-base uppercase"
          :options="parents.males.value"
          autocapitalize="characters"
          @update:model-value="male = upper($event)"
        />
      </label>
    </div>
    <p v-if="parents.loading.value" class="mt-1 text-xs text-stone-500">
      <Loader2 :size="12" class="inline animate-spin" /> {{ $t('Cargando {sheet}…', { sheet: 'Insectary_data' }) }}
    </p>
    <p v-for="w in [parents.warning(female, 'female'), parents.warning(male, 'male')].filter(Boolean)" :key="w" class="mt-1 text-sm text-amber-900">
      {{ w }}
    </p>
    <p v-if="female && parents.find(female)" class="mt-1 text-xs text-stone-600">♀ {{ parents.find(female)?.species }}</p>
    <p v-if="male && parents.find(male)" class="text-xs text-stone-600">♂ {{ parents.find(male)?.species }}</p>
  </div>
</template>
