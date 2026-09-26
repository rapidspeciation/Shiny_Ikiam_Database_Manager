<script setup lang="ts">
import { code128Svg } from '../lib/barcode'

/** Printable tube labels. Only visible when printing (see style.css). */
defineProps<{ labels: { tube: string; id: string; cam: string; tissue: string }[] }>()

function barcode(text: string) {
  try {
    return code128Svg(text)
  } catch {
    return ''
  }
}
</script>

<template>
  <Teleport to="body">
    <div class="print-labels">
      <div v-for="label in labels" :key="label.tube" class="print-label">
        <div class="print-label-code" v-html="barcode(label.tube)" />
        <strong>{{ label.tube }}</strong>
        <span>{{ label.id }} · {{ label.cam }}</span>
        <span>{{ label.tissue }}</span>
      </div>
    </div>
  </Teleport>
</template>
