<script setup lang="ts">
import { computed } from 'vue'
import { codeSpans, parseWordDiff, proseBlocks } from '../../lib/wordDiff'

/**
 * A section's change read as text (a proposed change, a change applied in the app): the body font, the source's
 * wrapped lines joined into paragraphs, removed words struck in red and added ones in green; `code` spans and fenced
 * blocks in mono.
 */
const props = defineProps<{ diff: string }>()
const blocks = computed(() => proseBlocks(parseWordDiff(props.diff, 2).flatMap(f => f.rows)))
</script>

<template>
  <div class="space-y-2 rounded-md border border-stone-200 bg-surface px-3 py-2 text-sm leading-6 text-stone-800">
    <template v-for="(b, i) in blocks" :key="i">
      <p v-if="b.type === 'skip'" class="text-xs text-stone-400 italic">⋯ {{ $tn(b.count, '{n} línea sin cambios', '{n} líneas sin cambios') }}</p>
      <p v-else-if="b.type === 'note'" class="text-xs text-stone-500">{{ b.text }}</p>
      <p
        v-else
        class="-ml-3 border-l-4 pl-2 break-words"
        :class="[b.changed ? 'border-amber-400' : 'border-transparent', b.type === 'code' ? 'font-mono text-xs whitespace-pre-wrap' : '']"
      >
        <template v-for="(s, k) in b.segments" :key="k"
          ><template v-if="s.kind === 'add' && b.segments[k - 1]?.kind === 'del'">{{ ' ' }}</template
          ><component :is="s.kind === 'del' ? 'del' : s.kind === 'add' ? 'ins' : 'span'" :class="s.kind === 'del' ? 'bg-red-100 text-red-800 decoration-red-400' : s.kind === 'add' ? 'bg-emerald-100 text-emerald-900 no-underline' : ''"
            ><template v-for="(c, j) in b.type === 'code' ? [{ code: false, text: s.text }] : codeSpans(s.text)" :key="j"
              ><code v-if="c.code" class="font-mono text-[0.85em]">{{ c.text }}</code
              ><template v-else>{{ c.text }}</template></template
            ></component
          ></template
        >
      </p>
    </template>
  </div>
</template>
