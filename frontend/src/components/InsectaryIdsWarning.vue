<script setup lang="ts">
import { ref, watch } from 'vue'
import { api } from '../lib/api'
import ExtendRowsButton from './ExtendRowsButton.vue'

/**
 * Warns when few pre-made Insectary IDs are left after the last row used, so
 * more are made before an emergence or a field day needs them.
 * `revision` (of Insectary_data) re-checks when the sheet changes.
 */
const props = withDefaults(defineProps<{ revision?: string; below?: number }>(), { below: 60 })
const emit = defineEmits<{ extended: [] }>()
const free = ref<number | null>(null)
const last = ref<string | null>(null)

async function load() {
  try {
    const r = await api<{ freeAtEnd: number; last: string | null }>('ids?kind=insectary&count=1')
    free.value = r.freeAtEnd
    last.value = r.last
  } catch {
    free.value = null
  }
}
watch(() => props.revision, load, { immediate: true })
function done() {
  load()
  emit('extended')
}
</script>

<template>
  <div
    v-if="free !== null && free < below"
    class="flex flex-wrap items-center gap-x-3 gap-y-1 rounded-md border border-amber-300 bg-amber-50 px-3 py-2 text-sm text-amber-950"
  >
    <span>
      Quedan <b>{{ free }}</b> Insectary IDs preasignados<template v-if="last"> (hasta {{ last }})</template>.
    </span>
    <ExtendRowsButton sheet="Insectary_data" :count="200" @done="done" />
  </div>
</template>
