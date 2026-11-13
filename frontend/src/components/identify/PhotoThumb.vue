<script setup lang="ts">
/**
 * One small photo that does something when tapped (Identificar's photo strip, the candidates' thumbnails, «Mis
 * identificaciones»): a square photo with a brand outline when it is the one shown, lifted on hover and pressed on
 * click, and an optional caption under it (the `caption` slot). With no photo, the same square on the sunken surface.
 */
import { ImageOff } from 'lucide-vue-next'
defineProps<{
  src?: string | null
  /** The button's name for screen readers (the photo is decorative). */
  label: string
  selected?: boolean
  /** The photo's edge in px (default 56). */
  size?: number
  /** A preserved specimen's photo is shown whole (contain), a live one fills the square. */
  contain?: boolean
  /** Inside a larger button (a list row): drawn the same, but not a button itself. */
  inert?: boolean
  /** The tile's width when its caption needs more room than the photo (px; default the photo's edge). */
  width?: number
}>()
defineEmits<{ select: [] }>()
</script>

<template>
  <component
    :is="inert ? 'span' : 'button'"
    :type="inert ? undefined : 'button'"
    class="thumb group flex shrink-0 flex-col items-center gap-1 rounded-md text-center"
    :style="{ width: `${width ?? size ?? 56}px` }"
    :aria-label="inert ? undefined : label"
    :aria-pressed="inert ? undefined : selected ? 'true' : 'false'"
    :aria-hidden="inert ? 'true' : undefined"
    @click="inert || $emit('select')"
  >
    <span
      class="thumb-photo relative block overflow-hidden rounded-md border-2 bg-photo-mat"
      :class="selected ? 'border-brand ring-2 ring-brand/35' : 'border-border group-hover:border-border-strong'"
      :style="{ width: `${size ?? 56}px`, height: `${size ?? 56}px` }"
    >
      <img v-if="src" :src="src" alt="" class="size-full" :class="contain ? 'object-contain' : 'object-cover'" draggable="false" />
      <!-- no photo of it: a quiet placeholder, not an empty dark tile -->
      <span v-else class="grid size-full place-items-center bg-surface-sunken text-fg-subtle" data-testid="thumb-none"><ImageOff :size="Math.round((size ?? 56) / 3)" /></span>
      <slot />
    </span>
    <slot name="caption" />
  </component>
</template>

<style scoped>
.thumb-photo {
  transition:
    transform 120ms ease,
    box-shadow 120ms ease,
    border-color 120ms ease;
}
.thumb:hover .thumb-photo {
  transform: translateY(-1px);
  box-shadow: 0 2px 6px rgb(0 0 0 / 0.18);
}
.thumb:active .thumb-photo {
  transform: translateY(0) scale(0.97);
  box-shadow: none;
}
@media (prefers-reduced-motion: reduce) {
  .thumb-photo,
  .thumb:hover .thumb-photo,
  .thumb:active .thumb-photo {
    transition: none;
    transform: none;
  }
}
</style>
