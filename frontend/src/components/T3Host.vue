<script setup lang="ts">
import { onBeforeUnmount, ref, watch } from 'vue'
import T3Frame from './T3Frame.vue'
import { t3Host } from '../lib/t3Host'

/**
 * T3's frame for the whole session (lib/t3Host.ts): fixed over the Asistente
 * tab's slot, following its size and place, and invisible (not unloaded) while
 * another tab is open, so coming back finds T3 as it was left.
 */
const frame = ref<InstanceType<typeof T3Frame>>()
const box = ref({ left: 0, top: 0, width: 0, height: 0 })
const shown = ref(false)
let observer: ResizeObserver | undefined

function measure() {
  const r = t3Host.slot?.getBoundingClientRect()
  shown.value = !!r && r.width > 0 && r.height > 0
  // Hidden, it keeps its last size: T3 does not lay itself out again for nothing.
  if (r && shown.value) box.value = { left: r.left, top: r.top, width: r.width, height: r.height }
}
watch(
  () => t3Host.slot,
  slot => {
    observer?.disconnect()
    if (slot) {
      observer = new ResizeObserver(measure)
      observer.observe(slot)
    }
    measure()
  },
  { immediate: true, flush: 'post' },
)
addEventListener('resize', measure)
onBeforeUnmount(() => {
  observer?.disconnect()
  removeEventListener('resize', measure)
  t3Host.seen = null
})
watch(
  () => t3Host.reconnects,
  () => frame.value?.connect(true),
)
</script>

<template>
  <div
    class="fixed flex flex-col"
    :class="{ invisible: !shown, 'pointer-events-none': !shown || t3Host.passThrough }"
    :style="{ left: `${box.left}px`, top: `${box.top}px`, width: `${box.width}px`, height: `${box.height}px` }"
    :aria-hidden="!shown"
  >
    <T3Frame
      ref="frame"
      :url="t3Host.url!"
      :environment-id="t3Host.environmentId"
      :open="t3Host.open"
      class="min-h-0 flex-1"
      @seen="value => (t3Host.seen = value)"
    />
  </div>
</template>
