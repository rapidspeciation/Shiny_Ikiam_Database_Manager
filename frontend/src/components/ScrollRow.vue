<script setup lang="ts">
import { nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { moreSide, type MoreSide } from '../lib/scrollRow'

/**
 * A row of tabs or buttons that scrolls sideways when it does not fit the screen, never widening the page: the edge
 * with more to see fades out (style.css .scroll-row), and the item marked `data-active` is scrolled into sight when
 * the row opens and whenever `active` changes.
 */
const props = withDefaults(defineProps<{ as?: string; active?: unknown }>(), { as: 'div', active: undefined })
const row = ref<HTMLElement>()
const more = ref<MoreSide>('none')
const measure = () => {
  const el = row.value
  if (el) more.value = moreSide(el.scrollLeft, el.clientWidth, el.scrollWidth)
}
function reveal() {
  const el = row.value
  const item = el?.querySelector<HTMLElement>('[data-active]')
  if (!el || !item) return measure()
  // Only this row moves (scrollIntoView would also scroll the page).
  const left = item.getBoundingClientRect().left - el.getBoundingClientRect().left + el.scrollLeft
  if (left < el.scrollLeft) el.scrollLeft = left - 40
  else if (left + item.offsetWidth > el.scrollLeft + el.clientWidth) el.scrollLeft = left + item.offsetWidth - el.clientWidth + 40
  measure()
}
let observer: ResizeObserver | null = null
onMounted(() => {
  observer = new ResizeObserver(measure)
  if (row.value) {
    observer.observe(row.value)
    for (const child of Array.from(row.value.children)) observer.observe(child)
  }
  reveal()
})
onBeforeUnmount(() => observer?.disconnect())
watch(
  () => props.active,
  () => nextTick(reveal),
)
</script>

<template>
  <component :is="as" ref="row" class="scroll-row" :data-more="more === 'none' ? undefined : more" @scroll.passive="measure">
    <slot />
  </component>
</template>
