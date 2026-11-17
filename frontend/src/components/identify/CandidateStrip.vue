<script setup lang="ts">
/**
 * The AI's most likely species under the comparison, as thumbnails with their %: tapping one shows its reference
 * photos in the comparison above, in place (IdentifyView's `compared`). The one compared is outlined; the person's
 * choice («Es esta») carries a check. Names as «M. messenoides», the genus written out when two genera in the strip share
 * the initial (stripNames).
 */
import { computed } from 'vue'
import { Check } from 'lucide-vue-next'
import PhotoThumb from './PhotoThumb.vue'
import { percent, stripNames, type Candidate } from '../../lib/identifyResult'

const props = defineProps<{ candidates: (Candidate & { thumb?: string })[]; showing: string; pick: string | null }>()
defineEmits<{ compare: [name: string] }>()
const names = computed(() => stripNames(props.candidates.map(c => c.name)))
</script>

<template>
  <div class="-mx-1 flex gap-2 overflow-x-auto px-1 pt-1 pb-1" role="group" :aria-label="$t('Otras posibles especies')" data-testid="candidates">
    <PhotoThumb
      v-for="c in candidates"
      :key="c.name"
      :src="c.thumb"
      :size="64"
      :width="72"
      :selected="c.name === showing"
      :label="`${c.name}, ${percent(c.p)}`"
      :data-candidate="c.name"
      @select="$emit('compare', c.name)"
    >
      <span v-if="pick === c.name" class="absolute top-0.5 right-0.5 grid size-5 place-items-center rounded-full bg-brand text-on-brand"><Check :size="13" /></span>
      <template #caption>
        <span
          class="block w-full leading-tight"
          :class="[c.name === showing ? 'font-semibold text-fg' : 'text-fg-muted', names.get(c.name) === c.name ? 'line-clamp-3 text-[11px]' : 'line-clamp-2 text-xs']"
          data-testid="candidate-name"
        >
          <i>{{ names.get(c.name) }}</i>
        </span>
        <span class="block text-xs leading-tight tabular-nums" :class="c.name === showing ? 'text-brand-text font-medium' : 'text-fg-muted'">{{ percent(c.p) }}</span>
      </template>
    </PhotoThumb>
  </div>
</template>
