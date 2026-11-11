<script setup lang="ts">
import { computed } from 'vue'
import { inlinePieces, parseWordDiff, proseBlocks } from '../../lib/wordDiff'

/**
 * A section's change read as the doc reads (a proposed change, a change applied in the app): the body font, the
 * source's wrapped lines joined into paragraphs, headings and list items as the page shows them, `code` in code
 * style without backticks; removed words struck in red, added ones in green, a changed paragraph marked in
 * Claude's green.
 */
const props = defineProps<{ diff: string }>()
const blocks = computed(() =>
  proseBlocks(parseWordDiff(props.diff, 2).flatMap(f => f.rows)).map(b =>
    b.type === 'para' ? { ...b, pieces: inlinePieces(b.segments) } : b.type === 'code' ? { ...b, pieces: b.segments.map(s => [{ text: s.text, code: false, bold: false }]) } : b,
  ),
)
const tag = (kind: string) => (kind === 'del' ? 'del' : kind === 'add' ? 'ins' : 'span')
const tone = (kind: string) => (kind === 'del' ? 'bg-red-100 text-red-800 decoration-red-400' : kind === 'add' ? 'bg-emerald-100 text-emerald-900 no-underline' : '')
</script>

<template>
  <div class="space-y-2 rounded-md border border-stone-200 bg-surface px-3 py-2 text-sm leading-6 text-stone-800">
    <template v-for="(b, i) in blocks" :key="i">
      <p v-if="b.type === 'skip'" class="text-xs text-stone-400 italic">⋯ {{ $tn(b.count, '{n} línea sin cambios', '{n} líneas sin cambios') }}</p>
      <p v-else-if="b.type === 'note'" class="text-xs text-stone-500">{{ b.text }}</p>
      <div
        v-else
        class="doc-prose-block -ml-3 flex gap-1.5 border-l-4 pl-2 break-words"
        :class="[b.changed ? 'is-changed' : 'border-transparent', b.type === 'code' ? 'font-mono text-xs whitespace-pre-wrap' : '', b.type === 'para' && b.marker === 'heading' ? 'font-semibold text-stone-900' : '']"
      >
        <span v-if="b.type === 'para' && b.marker && b.marker !== 'heading'" class="shrink-0 text-stone-500" aria-hidden="true">{{ b.marker === 'bullet' ? '•' : b.marker }}</span>
        <p class="min-w-0">
          <template v-for="(s, k) in b.segments" :key="k"
            ><template v-if="s.kind === 'add' && b.segments[k - 1]?.kind === 'del'">{{ ' ' }}</template
            ><component :is="tag(s.kind)" :class="tone(s.kind)"
              ><template v-for="(c, j) in b.pieces[k]" :key="j"
                ><code v-if="c.code" class="doc-code">{{ c.text }}</code
                ><strong v-else-if="c.bold">{{ c.text }}</strong
                ><template v-else>{{ c.text }}</template></template
              ></component
            ></template
          >
        </p>
      </div>
    </template>
  </div>
</template>
