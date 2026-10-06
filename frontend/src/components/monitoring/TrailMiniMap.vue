<script setup lang="ts">
import { onBeforeUnmount, onMounted, ref, watch } from 'vue'
import L from 'leaflet'
import { satelliteLayer, transectsLayer } from '../../lib/trailMap'

/**
 * A small satellite map of one walk: the trail's sections T1–T4, the walk's
 * Wikiloc line and its points (amber while undecided, green when paired, grey
 * when never entered), the chosen one enlarged. Clicking a point chooses it.
 */
export interface MiniPoint {
  lat: number
  lon: number
  label: string
  state: 'open' | 'paired' | 'none'
}
const props = defineProps<{ points: MiniPoint[]; selected: number | null; track?: [number, number][] }>()
const emit = defineEmits<{ select: [index: number] }>()

const COLORS = { open: '#f59e0b', paired: '#16a34a', none: '#a8a29e' }
const host = ref<HTMLDivElement>()
let map: L.Map | null = null
let overlay: L.LayerGroup | null = null
let resize: ResizeObserver | null = null

function draw() {
  if (!map || !overlay) return
  overlay.clearLayers()
  if (props.track?.length) L.polyline(props.track, { color: '#ffffff', weight: 2, opacity: 0.8, dashArray: '4 4' }).addTo(overlay)
  props.points.forEach((p, i) => {
    const chosen = i === props.selected
    L.circleMarker([p.lat, p.lon], {
      radius: chosen ? 9 : 6,
      color: chosen ? '#1c1917' : '#ffffff',
      weight: chosen ? 3 : 1.5,
      fillColor: COLORS[p.state],
      fillOpacity: 0.95,
    })
      .bindTooltip(p.label, { direction: 'top' })
      .on('click', () => emit('select', i))
      .addTo(overlay!)
  })
  const at = props.selected !== null ? props.points[props.selected] : null
  if (at && !map.getBounds().pad(-0.1).contains([at.lat, at.lon])) map.panTo([at.lat, at.lon])
}

onMounted(() => {
  map = L.map(host.value!, { zoomControl: true, attributionControl: true })
  satelliteLayer().addTo(map)
  const trail = transectsLayer({ weight: 4 }).addTo(map)
  overlay = L.layerGroup().addTo(map)
  const bounds = trail.getBounds()
  for (const p of props.points) bounds.extend([p.lat, p.lon])
  map.fitBounds(bounds, { padding: [12, 12] })
  draw()
  resize = new ResizeObserver(() => map?.invalidateSize())
  resize.observe(host.value!)
})
onBeforeUnmount(() => {
  resize?.disconnect()
  map?.remove()
  map = null
})
watch(() => [props.points, props.selected, props.track], draw, { deep: true })
</script>

<template>
  <div ref="host" class="h-full w-full rounded border border-stone-200 bg-stone-100"></div>
</template>
