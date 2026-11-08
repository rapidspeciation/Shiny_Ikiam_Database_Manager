<script setup lang="ts">
/** The identifier's first species for a point's photo, with bars; the note's and the row's species marked. */
/** `p` null: a name the identifier does not list (below 1 %, or `past` its first five). */
defineProps<{ lines: { name: string; p: number | null; note: boolean; row: boolean; past?: boolean }[] }>()
const pct = (p: number) => (p >= 0.995 ? '> 99 %' : `${Math.round(p * 100)} %`)
</script>

<template>
  <section class="w-full rounded-md border border-stone-200 px-2 py-1.5 text-xs sm:w-56" :aria-label="$t('Identificador')">
    <h3 class="mb-1 font-medium text-stone-600">{{ $t('Identificador') }}</h3>
    <ul class="space-y-1">
      <li v-for="l in lines" :key="l.name">
        <div class="flex items-baseline gap-1">
          <span class="min-w-0 truncate italic" :title="l.name">{{ l.name }}</span>
          <span v-if="l.note" class="tag">{{ $t('nota') }}</span>
          <span v-if="l.row" class="tag">{{ $t('fila') }}</span>
          <span v-if="l.p !== null" class="ml-auto tabular-nums">{{ pct(l.p) }}</span>
          <span v-else class="ml-auto whitespace-nowrap text-amber-800">{{ l.past ? $t('no entre las 5') : '< 1 %' }}</span>
        </div>
        <div v-if="l.p !== null" class="h-1.5 overflow-hidden rounded-sm bg-stone-100" aria-hidden="true">
          <div class="h-full bg-brand-600" :style="{ width: `${Math.round(l.p * 100)}%` }" />
        </div>
      </li>
    </ul>
  </section>
</template>

<style scoped>
.tag {
  flex: none;
  border-radius: 0.25rem;
  border: 1px solid currentColor;
  padding: 0 0.25rem;
  font-size: 0.7rem;
  line-height: 1.1rem;
  font-style: normal;
}
</style>
