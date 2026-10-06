<script setup lang="ts">
import { computed, ref } from 'vue'
import EmergedCards from './EmergedCards.vue'
import type { EntryMode } from '../../composables/useEntryMode'
import { useSheet } from '../../composables/useSheet'

/**
 * Larvae (or eggs) preserved from a clutch, registered from the Clutches tab:
 * Emergidos' own cards in focus (components/emerged/EmergedCards `focus`),
 * full screen, so both tabs give each one its Insectary ID, LIFESTAGE, CAM
 * and tube the same way (the batch panel: Flash frozen by default, the next
 * free CAM and tube, all editable). Insectary_data is loaded here when needed.
 */
const props = defineProps<{ clutch: string; count: number; stage: string; date: string }>()
const emit = defineEmits<{
  preserved: [result: { ids: string[]; stage: string; date: string; entryId: string | null }]
  close: []
}>()
const { table, stocks, ready, options, createFormulas, listColumn, marks } = useSheet(ref('Insectary_data'), ref(true), { staged: true })
const collectors = computed(() => listColumn('Abbr_name'))
const mode = ref<EntryMode>('cards')
const focus = computed(() => ({ clutch: props.clutch, count: props.count, stage: props.stage, date: props.date }))
</script>

<template>
  <div class="fixed inset-0 z-50 bg-stone-50" role="dialog" :aria-label="$t('Preservar del clutch {clutch}', { clutch })">
    <EmergedCards
      v-model:mode="mode"
      :table="table"
      :stocks="stocks"
      :ready="ready"
      :options="options"
      :collectors="collectors"
      :create-formulas="createFormulas"
      :staged-marks="marks"
      :focus="focus"
      @preserved="emit('preserved', $event)"
      @close="emit('close')"
    />
  </div>
</template>
