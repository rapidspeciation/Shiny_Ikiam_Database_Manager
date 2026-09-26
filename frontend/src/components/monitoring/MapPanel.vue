<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import L from 'leaflet'
import 'leaflet/dist/leaflet.css'
import { Trash2 } from 'lucide-vue-next'
import { useMonitoring, type StoredCapture, type StoredTrack } from '../../composables/useMonitoring'
import { api } from '../../lib/api'
import { formatSerial, isoToSerial } from '../../lib/dates'
import { formatMinutes } from '../../lib/monitoring'
import { errorText, notify } from '../../lib/notice'
import { persistentRef } from '../../lib/persist'
import { SECTIONS } from '../../lib/transects'
import { useSession } from '../../stores/session'

/**
 * "Mapa": the trail sections, the stored monitoring walks and the capture
 * points on a satellite map, like the QGIS maps of the monthly reports.
 */
const session = useSession()
const { rows, tracks, tracksLoaded, loadTracks } = useMonitoring()

// Colour-blind-safe palette (Okabe–Ito, then Paul Tol).
const PALETTE = [
  '#E69F00',
  '#56B4E9',
  '#009E73',
  '#F0E442',
  '#0072B2',
  '#D55E00',
  '#CC79A7',
  '#882255',
  '#44AA99',
  '#DDCC77',
  '#AA4499',
  '#117733',
  '#88CCEE',
  '#332288',
  '#999933',
]

const hidden = persistentRef<string[]>('monitoring:hidden', [])
const colorBy = persistentRef<'species' | 'track'>('monitoring:colorBy', 'species')
const speciesFilter = persistentRef('monitoring:species', '')
const showLines = persistentRef('monitoring:lines', true)
const showRecaptures = persistentRef('monitoring:recaptures', true)
const panel = ref(true)

const visible = computed(() => tracks.value.filter(t => !hidden.value.includes(t.id)))
const trackColor = computed(() => new Map(tracks.value.map((t, i) => [t.id, PALETTE[i % PALETTE.length]])))
const points = computed(() =>
  visible.value.flatMap(t =>
    t.captures.filter(c => !speciesFilter.value || c.species === speciesFilter.value).map(c => ({ track: t, capture: c })),
  ),
)
const speciesList = computed(() => {
  const counts = new Map<string, number>()
  for (const t of visible.value)
    for (const c of t.captures) if (c.species) counts.set(c.species, (counts.get(c.species) || 0) + 1)
  return [...counts].sort((a, b) => b[1] - a[1])
})
const speciesColor = computed(() => new Map(speciesList.value.map(([s], i) => [s, PALETTE[i % PALETTE.length]])))
const colorOf = (t: StoredTrack, c: StoredCapture) =>
  colorBy.value === 'track' ? trackColor.value.get(t.id)! : speciesColor.value.get(c.species || '') || '#78716c'

/** The sheet row a stored capture became (same day and mark, or same species and minute). */
function sheetRow(t: StoredTrack, c: StoredCapture) {
  const day = isoToSerial(t.date)
  return rows.value.find(r => {
    if (r.values.Collection_date !== day) return false
    if (c.markId && String(r.values.FieldMark_ID ?? '').toUpperCase() === c.markId) return true
    const m = typeof r.values.Collection_time === 'number' ? Math.round(r.values.Collection_time * 1440) : null
    return !!c.species && r.values.SPECIES === c.species && c.minutes !== null && m !== null && Math.abs(m - c.minutes) <= 2
  })
}

const host = ref<HTMLDivElement>()
let map: L.Map | null = null
let layer: L.LayerGroup | null = null
let resize: ResizeObserver | null = null

const escape = (s: string) => s.replace(/[&<>"']/g, ch => `&#${ch.charCodeAt(0)};`)

function popup(t: StoredTrack, c: StoredCapture) {
  const row = sheetRow(t, c)
  const lines = [
    `<b><i>${escape(c.species || 'Sin especie')}</i> ${escape(c.subspecies || '')}</b>`,
    [
      c.sex === 'female' ? 'hembra' : c.sex === 'male' ? 'macho' : '',
      formatMinutes(c.minutes),
      c.height !== null ? `${c.height} m` : '',
    ]
      .filter(Boolean)
      .join(' · '),
    c.markId ? `Marca <b>${escape(c.markId)}</b>${c.recapture ? ' (recaptura)' : ''}` : 'Preservado',
    `${formatSerial(isoToSerial(t.date))} · ${escape((t.collector || '').split(' - ')[0])}${c.section ? ` · T${c.section}` : ''}`,
    `<span style="color:#78716c">${escape(c.text)}</span>`,
    row ? `Collection_data fila ${row.row}` : '<span style="color:#b45309">Aún no está en la hoja</span>',
  ]
  return lines.filter(Boolean).join('<br>')
}

function draw() {
  if (!map || !layer) return
  layer.clearLayers()
  if (showLines.value)
    for (const t of visible.value)
      if (t.track.length > 1)
        L.polyline(
          t.track.map(p => [p[0], p[1]] as [number, number]),
          // White unless colouring by walk, so the GPS line does not look like a transect section.
          {
            color: colorBy.value === 'track' ? trackColor.value.get(t.id) : '#ffffff',
            weight: 1.5,
            opacity: 0.85,
            dashArray: '3 4',
          },
        )
          .bindTooltip(`${formatSerial(isoToSerial(t.date))} · ${(t.collector || '').split(' - ')[0]}`, { sticky: true })
          .addTo(layer)
  if (showRecaptures.value) {
    const byMark = new Map<string, [number, number][]>()
    for (const { capture } of points.value)
      if (capture.markId) {
        const list = byMark.get(capture.markId) || []
        list.push([capture.lat, capture.lon])
        byMark.set(capture.markId, list)
      }
    for (const [id, list] of byMark)
      if (list.length > 1)
        L.polyline(list, { color: '#ef4444', weight: 2.5, dashArray: '2 5' })
          .bindTooltip(`Recaptura ${id}`, { sticky: true })
          .addTo(layer)
  }
  for (const { track, capture } of points.value)
    L.circleMarker([capture.lat, capture.lon], {
      radius: 7,
      color: capture.recapture ? '#ffffff' : '#1c1917',
      weight: capture.recapture ? 3 : 1,
      fillColor: colorOf(track, capture),
      fillOpacity: 0.95,
    })
      .bindPopup(popup(track, capture))
      .bindTooltip(`${capture.species || '?'}${capture.markId ? ` · ${capture.markId}` : ''}`, { direction: 'top' })
      .addTo(layer)
}

onMounted(() => {
  map = L.map(host.value!, { zoomControl: true, attributionControl: true })
  const satellite = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
    maxZoom: 20,
    maxNativeZoom: 19,
    attribution: 'Imágenes © Esri, Maxar, Earthstar Geographics',
  }).addTo(map)
  const streets = L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
    maxZoom: 20,
    maxNativeZoom: 19,
    attribution: '© OpenStreetMap',
  })
  L.control.layers({ Satélite: satellite, Calles: streets }, undefined, { position: 'topright' }).addTo(map)
  L.control.scale({ imperial: false }).addTo(map)
  const trail = L.featureGroup()
  for (const s of SECTIONS) {
    L.polyline(s.path, { color: s.color, weight: 5, opacity: 0.95 })
      .bindTooltip(`Transecto ${s.section}`, { sticky: true })
      .addTo(trail)
    L.marker(s.path[Math.floor(s.path.length / 2)], {
      icon: L.divIcon({ className: 'transect-label', html: `T${s.section}`, iconSize: [26, 16] }),
      interactive: false,
    }).addTo(trail)
  }
  trail.addTo(map)
  map.fitBounds(trail.getBounds(), { padding: [30, 30] })
  layer = L.layerGroup().addTo(map)
  draw()
  resize = new ResizeObserver(() => map?.invalidateSize())
  resize.observe(host.value!)
})
onBeforeUnmount(() => {
  resize?.disconnect()
  map?.remove()
  map = null
})
watch([visible, points, colorBy, showLines, showRecaptures, rows], draw)

function toggle(id: string) {
  hidden.value = hidden.value.includes(id) ? hidden.value.filter(h => h !== id) : [...hidden.value, id]
}
const canDelete = (t: StoredTrack) =>
  session.user?.username === t.createdBy || ['reviewer', 'admin'].includes(session.user?.role || '')
async function remove(t: StoredTrack) {
  if (!confirm(`¿Quitar el recorrido "${t.name}" del mapa? Las filas de la hoja no cambian.`)) return
  try {
    await api(`monitoring/tracks/${encodeURIComponent(t.id)}`, { method: 'DELETE', body: {} })
    await loadTracks()
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
const withoutGps = computed(() => {
  const days = new Set(tracks.value.map(t => isoToSerial(t.date)))
  return rows.value.filter(r => typeof r.values.Collection_date === 'number' && !days.has(r.values.Collection_date)).length
})
</script>

<template>
  <div class="relative flex h-full min-h-0">
    <aside
      v-if="panel"
      class="flex w-72 shrink-0 flex-col overflow-y-auto border-r border-stone-200 bg-white text-sm max-md:absolute max-md:inset-y-0 max-md:left-0 max-md:z-[1000] max-md:shadow-lg"
    >
      <div class="space-y-3 border-b border-stone-200 p-3">
        <label class="block">
          <span class="field-label">Colorear puntos por</span>
          <select v-model="colorBy" class="field-input">
            <option value="species">Especie</option>
            <option value="track">Recorrido</option>
          </select>
        </label>
        <label class="block">
          <span class="field-label">Especie</span>
          <select v-model="speciesFilter" class="field-input">
            <option value="">Todas</option>
            <option v-for="[s, n] in speciesList" :key="s" :value="s">{{ s }} ({{ n }})</option>
          </select>
        </label>
        <label class="flex items-center gap-2"><input v-model="showLines" type="checkbox" /> Mostrar trazados GPS</label>
        <label class="flex items-center gap-2"><input v-model="showRecaptures" type="checkbox" /> Unir recapturas</label>
      </div>
      <div class="p-3">
        <p class="field-label">Recorridos ({{ tracks.length }})</p>
        <p v-if="tracksLoaded && !tracks.length" class="hint">Aún no hay recorridos. Súbelos en “Importar”.</p>
        <ul class="space-y-1">
          <li v-for="t in tracks" :key="t.id" class="group flex items-center gap-2">
            <input :id="`t-${t.id}`" type="checkbox" :checked="!hidden.includes(t.id)" @change="toggle(t.id)" />
            <span class="h-3 w-3 shrink-0 rounded-full" :style="{ background: trackColor.get(t.id) }" />
            <label :for="`t-${t.id}`" class="min-w-0 flex-1 truncate" :title="t.name">
              {{ formatSerial(isoToSerial(t.date)) }} · {{ (t.collector || '').split(' - ')[0] }}
              <span class="text-stone-500">({{ t.captures.length }})</span>
            </label>
            <button
              v-if="canDelete(t)"
              class="btn-ghost invisible group-hover:visible"
              title="Quitar recorrido"
              @click="remove(t)"
            >
              <Trash2 :size="13" />
            </button>
          </li>
        </ul>
      </div>
      <div v-if="colorBy === 'species' && speciesList.length" class="border-t border-stone-200 p-3">
        <p class="field-label">Especies</p>
        <ul class="space-y-0.5 text-xs">
          <li v-for="[s] in speciesList" :key="s" class="flex items-center gap-2">
            <span
              class="h-2.5 w-2.5 shrink-0 rounded-full border border-stone-700"
              :style="{ background: speciesColor.get(s) }"
            />
            <i>{{ s }}</i>
          </li>
        </ul>
        <p class="hint mt-2">Borde blanco: recaptura.</p>
      </div>
      <p class="hint border-t border-stone-200 p-3">
        Transectos T1–T4 reconstruidos del mapa QGIS de los reportes sobre el GPS del 26-Sep-26. {{ withoutGps }} registros de
        monitoreo son de días sin recorrido subido; sube sus GPX con “Solo guardar el recorrido” para verlos.
      </p>
    </aside>
    <div class="relative min-w-0 flex-1">
      <div ref="host" class="absolute inset-0" />
      <button class="btn absolute bottom-6 left-3 z-[1000] shadow" @click="panel = !panel">
        {{ panel ? 'Ocultar panel' : 'Capas y filtros' }}
      </button>
    </div>
  </div>
</template>

<style>
.leaflet-container {
  font-family: inherit;
}
.transect-label {
  background: rgba(28, 25, 23, 0.75);
  color: white;
  font:
    600 11px 'Fira Sans',
    sans-serif;
  border-radius: 3px;
  text-align: center;
  line-height: 16px;
}
</style>
