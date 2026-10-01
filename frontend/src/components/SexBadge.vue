<script setup lang="ts">
import { computed } from 'vue'

/**
 * A butterfly's sex at a glance: ♀ in pink, ♂ in blue (the sheet's value as the
 * label for screen readers and on hover); anything else (NA, NOT_COLLECTED, a
 * doubt) stays as written, in grey.
 */
const props = defineProps<{ sex: string | null | undefined }>()
const kind = computed(() => {
  const s = String(props.sex ?? '').trim().toLowerCase()
  return s === 'female' ? 'f' : s === 'male' ? 'm' : null
})
</script>

<template>
  <span
    v-if="kind === 'f'"
    class="inline-flex h-6 min-w-6 shrink-0 items-center justify-center rounded-full bg-pink-100 px-1 text-base leading-none font-bold text-pink-700"
    :title="sex ?? ''"
    :aria-label="sex ?? ''"
    >♀</span
  >
  <span
    v-else-if="kind === 'm'"
    class="inline-flex h-6 min-w-6 shrink-0 items-center justify-center rounded-full bg-sky-100 px-1 text-base leading-none font-bold text-sky-700"
    :title="sex ?? ''"
    :aria-label="sex ?? ''"
    >♂</span
  >
  <span v-else-if="sex" class="text-stone-500">{{ sex }}</span>
</template>
