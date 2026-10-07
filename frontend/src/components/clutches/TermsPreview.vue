<script setup lang="ts">
import { computed } from 'vue'
import { parseTerms } from '../../lib/clutchGroups'

/**
 * What a count box reads while it is typed: its terms as chips and what they
 * add up to («27 3 5» → 27 +3 +5 = 35; a space is a plus, «-» a minus).
 * Shown only when there is more than one term. `counts`: every term must be a
 * count (no minus).
 */
const props = defineProps<{ text: string; counts?: boolean }>()
const parsed = computed(() => parseTerms(props.text))
const terms = computed(() => (parsed.value.ok && (!props.counts || parsed.value.terms.every(n => n >= 0)) ? parsed.value.terms : null))
</script>

<template>
  <div v-if="terms && terms.length > 1" class="flex flex-wrap items-center gap-1 text-sm tabular-nums" role="status">
    <span v-for="(n, i) in terms" :key="i" class="rounded px-1.5 py-0.5" :class="n < 0 ? 'bg-red-50 text-red-800' : 'bg-white ring-1 ring-stone-200'">
      {{ n < 0 ? `−${-n}` : i ? `+${n}` : n }}
    </span>
    <span class="font-semibold text-brand-800">= {{ terms.reduce((a, b) => a + b, 0) }}</span>
  </div>
</template>
