<script setup lang="ts">
import { computed } from 'vue'
import { parseWordDiff } from '../../lib/wordDiff'

/** One commit's change to an instructions file: removed words struck in red, added ones in green. */
const props = defineProps<{ diff: string; showPaths?: boolean }>()
const files = computed(() => parseWordDiff(props.diff))
</script>

<template>
  <div class="overflow-x-auto rounded-md border border-stone-200 bg-white font-mono text-xs leading-5">
    <template v-for="(file, f) in files" :key="f">
      <div
        v-if="showPaths && file.path"
        class="sticky left-0 border-b border-stone-200 bg-stone-100 px-2 py-1 font-sans font-medium text-stone-700"
      >
        {{ file.path }}
      </div>
      <template v-for="(row, r) in file.rows" :key="`${f}-${r}`">
        <div v-if="row.type === 'hunk'" class="border-y border-stone-100 bg-stone-50 px-2 font-sans text-stone-500">
          {{ $t('Línea {n}', { n: row.line }) }}<template v-if="row.section"> · {{ row.section }}</template>
        </div>
        <div v-else-if="row.type === 'skip'" class="px-2 font-sans text-stone-400 italic">
          ⋯ {{ $tn(row.count, '{n} línea sin cambios', '{n} líneas sin cambios') }}
        </div>
        <div v-else-if="row.type === 'note'" class="px-2 py-0.5 font-sans text-stone-600">
          {{
            row.text === 'new'
              ? $t('Archivo nuevo')
              : row.text === 'deleted'
                ? $t('Archivo borrado')
                : row.text.startsWith('renamed:')
                  ? $t('Renombrado desde {path}', { path: row.text.slice(8) })
                  : row.text
          }}
        </div>
        <div
          v-else
          class="border-l-4 px-2 break-words whitespace-pre-wrap"
          :class="{
            'border-transparent text-stone-700': row.change === 'ctx',
            'border-emerald-500 bg-emerald-50': row.change === 'add',
            'border-red-400 bg-red-50': row.change === 'del',
            'border-amber-400': row.change === 'mod',
          }"
        >
          <template v-for="(s, k) in row.segments" :key="k"
            ><span v-if="s.kind === 'ctx'">{{ s.text }}</span
            ><del v-else-if="s.kind === 'del'" class="bg-red-100 text-red-800 decoration-red-400">{{ s.text }}</del
            ><ins v-else class="bg-emerald-100 text-emerald-900 no-underline">{{ s.text }}</ins></template
          ><template v-if="!row.segments.length">&nbsp;</template>
        </div>
      </template>
    </template>
  </div>
</template>
