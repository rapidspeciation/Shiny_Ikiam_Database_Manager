<script setup lang="ts">
import { ArrowDown, ArrowUp } from 'lucide-vue-next'
import type { RecordedOrder, RecordedSort } from '../lib/deathsCart'

/** How a list for the paper notebook is sorted: one of `options`, ↑/↓ (kept by the parent). */
defineProps<{ options: { by: RecordedSort; label: string }[] }>()
const order = defineModel<RecordedOrder>({ required: true })
</script>

<template>
  <div
    class="inline-flex overflow-hidden rounded-lg border border-stone-300 text-sm"
    role="group"
    :aria-label="$t('Ordenar por')"
  >
    <button
      v-for="(s, i) in options"
      :key="s.by"
      type="button"
      class="h-10 px-2.5"
      :class="[order.by === s.by ? 'bg-brand-700 text-white' : 'bg-white', i ? 'border-l border-stone-300' : '']"
      :aria-pressed="order.by === s.by"
      @click="order = { ...order, by: s.by }"
    >
      {{ s.label }}
    </button>
    <button
      type="button"
      class="grid h-10 w-10 place-items-center border-l border-stone-300 bg-white"
      :aria-label="order.desc ? $t('Descendente: cambiar a ascendente') : $t('Ascendente: cambiar a descendente')"
      :title="order.desc ? $t('Descendente') : $t('Ascendente')"
      @click="order = { ...order, desc: !order.desc }"
    >
      <ArrowDown v-if="order.desc" :size="16" /><ArrowUp v-else :size="16" />
    </button>
  </div>
</template>
