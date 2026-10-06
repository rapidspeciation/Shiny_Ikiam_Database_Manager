<script setup lang="ts">
import { computed } from 'vue'
import { estimatedSection, sectionDiffers } from '../../lib/transects'

/**
 * The transect section a Wikiloc point lies in by its GPS position (or "off the
 * trail" and how far), beside its row's Transect_section; amber when they differ.
 */
const props = defineProps<{
  lat: number | null | undefined
  lon: number | null | undefined
  /** The row's Transect_section; nothing shown for it when undefined. */
  row?: string | number | null
}>()
const estimate = computed(() =>
  typeof props.lat === 'number' && typeof props.lon === 'number' ? estimatedSection(props.lat, props.lon) : null,
)
const rowSection = computed(() => {
  const n = Number(String(props.row ?? '').trim())
  return n >= 1 && n <= 4 ? n : null
})
const differs = computed(() => !!estimate.value && sectionDiffers(estimate.value, props.row))
</script>

<template>
  <span
    v-if="estimate"
    class="inline-flex flex-wrap items-baseline gap-x-1 rounded px-1 text-xs"
    :class="differs ? 'bg-amber-100 font-medium text-amber-900' : 'text-stone-600'"
    :title="
      differs
        ? $t('La posición GPS del punto cae en otro transecto que el de su fila')
        : $t('Transecto según la posición GPS del punto (a {d} m del sendero)', { d: estimate.distance })
    "
  >
    <span>{{
      estimate.section !== null
        ? $t('GPS: T{n}', { n: estimate.section })
        : $t('GPS: fuera del sendero ({d} m)', { d: estimate.distance })
    }}</span>
    <span v-if="row !== undefined"
      >· {{ rowSection !== null ? $t('fila: T{n}', { n: rowSection }) : $t('fila: sin transecto') }}</span
    >
  </span>
</template>
