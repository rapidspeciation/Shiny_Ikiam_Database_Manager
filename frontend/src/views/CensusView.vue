<script setup lang="ts">
import { computed, onActivated, onMounted, ref, watch } from 'vue'
import StagedBar from '../components/StagedBar.vue'
import CensusStart from '../components/census/CensusStart.vue'
import CensusRun from '../components/census/CensusRun.vue'
import CensusReview from '../components/census/CensusReview.vue'
import CensusSummary from '../components/census/CensusSummary.vue'
import { useCensus } from '../composables/useCensus'
import { useSheet } from '../composables/useSheet'

/**
 * Censo: counting the butterflies of one species alive in the insectary. On
 * paper, they go into a small cage and are released one by one into the big
 * one; for each, someone reads the wing ID, finds it in the notebook and draws
 * a smiley; afterwards the IDs without one are marked disappeared, and the
 * database is updated row by row in the lab.
 *
 * Here: start (or join) a census of a species → mark each butterfly as it is
 * released, from one or several phones at once (CensusRun) → review what is
 * left (CensusReview) → the disappearances wait in the app for «Guardar en
 * Google Sheets» like Emergidos and Clutches, and the notebook's lines to copy
 * (CensusSummary). The sheet shown carries everyone's entries kept in the app
 * (a butterfly emerged today is on the list; a death kept in the app is not).
 */
const { table, ready } = useSheet(ref('Insectary_data'), ref(true), { staged: true })
const census = useCensus()
const reviewing = ref(false)
const screen = computed(() => {
  const d = census.detail.value
  if (!census.currentId.value) return 'start'
  if (!d) return 'loading'
  if (d.census.status !== 'open') return 'summary'
  return reviewing.value ? 'review' : 'run'
})
watch(
  () => census.detail.value?.census.status,
  status => {
    if (status !== 'open') reviewing.value = false
  },
)
onMounted(() => {
  void census.loadOverview()
  if (census.currentId.value) void census.loadDetail()
})
// Back to the tab (it stays alive in the background): what changed meanwhile.
onActivated(() => {
  if (census.currentId.value) void census.loadDetail()
})
function leave() {
  reviewing.value = false
  census.open(null)
}
</script>

<template>
  <div class="flex h-full flex-col bg-stone-50">
    <!-- The disappearances wait in the app with Emergidos and Clutches for «Guardar en Google Sheets». -->
    <StagedBar />
    <div class="min-h-0 flex-1">
      <CensusStart v-if="screen === 'start'" />
      <p v-else-if="screen === 'loading'" class="p-6 text-stone-500">{{ $t('Cargando el censo…') }}</p>
      <CensusRun v-else-if="screen === 'run'" :table="table" :ready="ready" @review="reviewing = true" @leave="leave" />
      <CensusReview v-else-if="screen === 'review'" :table="table" :ready="ready" @back="reviewing = false" />
      <CensusSummary v-else :table="table" @leave="leave" />
    </div>
  </div>
</template>
