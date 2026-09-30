<script setup lang="ts">
import { onBeforeUnmount, onMounted, ref } from 'vue'

/**
 * Builds its content (a proposal's table) only when it comes near the visible
 * part of the list that scrolls around it (marked data-lazy-root), and keeps
 * it afterwards: a panel with many proposals, or a hidden one, does not lay
 * out tables nobody sees. Until then a box of about the same height keeps the
 * scrollbar right.
 */
defineProps<{ height: number }>()
const box = ref<HTMLElement>()
const seen = ref(typeof IntersectionObserver === 'undefined')
let watcher: IntersectionObserver | null = null
onMounted(() => {
  if (seen.value || !box.value) return
  watcher = new IntersectionObserver(
    entries => {
      if (!entries.some(e => e.isIntersecting)) return
      seen.value = true
      watcher?.disconnect()
    },
    { root: box.value.closest<HTMLElement>('[data-lazy-root]'), rootMargin: '400px 0px' },
  )
  watcher.observe(box.value)
})
onBeforeUnmount(() => watcher?.disconnect())
</script>

<template>
  <div ref="box">
    <slot v-if="seen" />
    <div
      v-else
      class="mt-2 rounded-md border border-stone-200 bg-white"
      :style="{ height: `min(${height}px, calc(100cqh - 1rem))` }"
    />
  </div>
</template>
