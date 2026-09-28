<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import L from 'leaflet'
import 'leaflet/dist/leaflet.css'
import { ExternalLink, Link2, RotateCcw, Trash2, X } from 'lucide-vue-next'
import FilterSelect, { type FilterOption } from './FilterSelect.vue'
import { useMonitoring, type StoredCapture, type StoredTrack } from '../../composables/useMonitoring'
import { api } from '../../lib/api'
import { formatSerial, isoToSerial } from '../../lib/dates'
import { formatMinutes, markHistories } from '../../lib/monitoring'
import {
  collectorOf,
  facetCounts,
  individualKey,
  kindOfCapture,
  passes,
  rankColors,
  sectionOf,
  sexOf,
  type FacetKey,
  type MapFilters,
  type MapPoint,
} from '../../lib/monitoringMap'
import { errorText, notify } from '../../lib/notice'
import { SECTIONS, distance } from '../../lib/transects'
import { useSession } from '../../stores/session'

/**
 * "Mapa": the capture points of the monitoring walks on a satellite map, with
 * filters like the atlas (ithomiini_maps): nothing chosen shows everything.
 * The filters and layers live in the page link, so a view can be shared.
 */
const session = useSession()
const { rows, tracks, tracksLoaded, loadTracks } = useMonitoring()
const route = useRoute()
const router = useRouter()

// Okabe–Ito first, as in the atlas; species beyond the top 10 are grey.
const PALETTE = ['#0072B2', '#E69F00', '#009E73', '#CC79A7', '#56B4E9', '#D55E00', '#F0E442', '#332288', '#882255', '#44AA99']
const OTHER = '#a8a29e'
const SEX_COLORS: Record<string, string> = { female: '#CC79A7', male: '#0072B2', unknown: OTHER }
const KIND_COLORS: Record<string, string> = { preserved: '#E69F00', marked: '#009E73', recapture: '#56B4E9' }
const SEX_LABELS: Record<string, string> = { female: 'Hembra', male: 'Macho', unknown: 'Sin sexo' }
const KIND_LABELS: Record<string, string> = { preserved: 'Preservado', marked: 'Marcado', recapture: 'Recaptura' }

// ------------------------------------------------------------ link state
function listQuery(key: string) {
  return computed<string[]>({
    get: () =>
      String(route.query[key] ?? '')
        .split(',')
        .filter(Boolean),
    set: value => router.replace({ query: { ...route.query, [key]: value.length ? value.join(',') : undefined } }),
  })
}
function textQuery<T extends string>(key: string, initial: T) {
  return computed<T>({
    get: () => (String(route.query[key] ?? '') || initial) as T,
    set: value => router.replace({ query: { ...route.query, [key]: value === initial ? undefined : value } }),
  })
}
function flagQuery(key: string, initial: boolean) {
  return computed<boolean>({
    get: () => (route.query[key] === undefined ? initial : route.query[key] === '1'),
    set: value => router.replace({ query: { ...route.query, [key]: value === initial ? undefined : value ? '1' : '0' } }),
  })
}
const QUERY: Record<FacetKey, string> = {
  years: 'anios',
  dates: 'fechas',
  collectors: 'recolectores',
  species: 'especies',
  sections: 'transectos',
  sexes: 'sexos',
  kinds: 'tipos',
}
const lists = Object.fromEntries(Object.entries(QUERY).map(([k, q]) => [k, listQuery(q)])) as Record<
  FacetKey,
  ReturnType<typeof listQuery>
>
const individual = textQuery<string>('individuo', '')
const mode = textQuery<'puntos' | 'grupos' | 'calor'>('capa', 'puntos')
const colorBy = textQuery<'especie' | 'recorrido' | 'sexo' | 'tipo'>('color', 'especie')
const showTransects = flagQuery('trazado', true)
const showGps = flagQuery('gps', false)
const showRecaptures = flagQuery('unir', false)
const heatRadius = ref(22)
const panel = ref(true)

const filters = computed<MapFilters>(() => ({
  years: lists.years.value,
  dates: lists.dates.value,
  collectors: lists.collectors.value,
  species: lists.species.value,
  sections: lists.sections.value,
  sexes: lists.sexes.value,
  kinds: lists.kinds.value,
  individual: individual.value,
}))
const filterCount = computed(
  () => Object.values(lists).reduce((n, l) => n + (l.value.length ? 1 : 0), 0) + (individual.value ? 1 : 0),
)
function clearFilters() {
  const query = { ...route.query }
  for (const key of [...Object.values(QUERY), 'individuo']) delete query[key]
  router.replace({ query })
}
async function copyLink() {
  try {
    await navigator.clipboard.writeText(location.href)
    notify('Enlace copiado: abre el mapa con estos filtros', 'success')
  } catch {
    notify('No se pudo copiar el enlace', 'error')
  }
}

// ------------------------------------------------------------ data
type Point = MapPoint<StoredTrack>
const allPoints = computed<Point[]>(() => tracks.value.flatMap(walk => walk.captures.map(capture => ({ walk, capture }))))
const points = computed(() => allPoints.value.filter(p => passes(p, filters.value)))
/** Walks with a shown point, plus every walk of a chosen date (some have only a trail). */
const shownWalks = computed(() => {
  const ids = new Set(points.value.map(p => p.walk.id))
  return tracks.value.filter(t => ids.has(t.id) || lists.dates.value.includes(t.date))
})
const counts = (key: FacetKey) => facetCounts(allPoints.value, filters.value, key)

const speciesTotals = computed(() => {
  const out = new Map<string, number>()
  for (const p of allPoints.value) if (p.capture.species) out.set(p.capture.species, (out.get(p.capture.species) || 0) + 1)
  return out
})
/** Ranked over all walks, so a species keeps its colour whatever the filters. */
const speciesColor = computed(() => rankColors(speciesTotals.value, PALETTE))
const walkColor = computed(
  () =>
    new Map(
      [...shownWalks.value].sort((a, b) => a.date.localeCompare(b.date)).map((t, i) => [t.id, PALETTE[i % PALETTE.length]]),
    ),
)
function colorOf(p: Point) {
  switch (colorBy.value) {
    case 'recorrido':
      return walkColor.value.get(p.walk.id) || OTHER
    case 'sexo':
      return SEX_COLORS[sexOf(p.capture)]
    case 'tipo':
      return KIND_COLORS[kindOfCapture(p.capture)]
    default:
      return speciesColor.value.get(p.capture.species || '') || OTHER
  }
}

// ------------------------------------------------------------ filter options
const dateLabel = (iso: string) => formatSerial(isoToSerial(iso))
const dateOptions = computed<FilterOption[]>(() => {
  const n = counts('dates')
  const byDate = new Map<string, Set<string>>()
  for (const t of tracks.value) {
    // Dates outside the chosen years or collectors are left out of the list.
    const inYears = !lists.years.value.length || lists.years.value.includes(t.date.slice(0, 4))
    const inCollectors = !lists.collectors.value.length || lists.collectors.value.includes(collectorOf(t))
    if (!(inYears && inCollectors) && !lists.dates.value.includes(t.date)) continue
    byDate.set(t.date, (byDate.get(t.date) || new Set()).add(collectorOf(t)))
  }
  return [...byDate]
    .sort((a, b) => b[0].localeCompare(a[0]))
    .map(([date, who]) => ({
      value: date,
      label: `${dateLabel(date)} · ${[...who].filter(Boolean).join(', ')}`,
      count: n.get(date) || 0,
      group: date.slice(0, 4),
    }))
})
const speciesOptions = computed<FilterOption[]>(() => {
  const n = counts('species')
  return [...speciesTotals.value.keys()]
    .map(s => ({ value: s, label: s, count: n.get(s) || 0, color: speciesColor.value.get(s) || OTHER, italic: true }))
    .sort((a, b) => b.count - a.count || a.label.localeCompare(b.label))
})
interface Chip {
  value: string
  label: string
  count: number
}
function chips(key: FacetKey, values: string[], label: (v: string) => string = v => v): Chip[] {
  const n = counts(key)
  return values.map(value => ({ value, label: label(value), count: n.get(value) || 0 }))
}
const chipGroups = computed(() => [
  {
    key: 'years' as const,
    title: 'Año',
    chips: chips('years', [...new Set(tracks.value.map(t => t.date.slice(0, 4)))].sort().reverse()),
  },
  {
    key: 'collectors' as const,
    title: 'Recolector',
    chips: chips('collectors', [...new Set(tracks.value.map(collectorOf).filter(Boolean))].sort()),
  },
  {
    key: 'sections' as const,
    title: 'Transecto',
    chips: chips('sections', ['1', '2', '3', '4', 'none'], v => (v === 'none' ? 'Sin' : `T${v}`)),
  },
  { key: 'sexes' as const, title: 'Sexo', chips: chips('sexes', ['female', 'male', 'unknown'], v => SEX_LABELS[v]) },
  { key: 'kinds' as const, title: 'Tipo', chips: chips('kinds', ['preserved', 'marked', 'recapture'], v => KIND_LABELS[v]) },
])
function toggleChip(key: FacetKey, value: string) {
  const list = lists[key]
  list.value = list.value.includes(value) ? list.value.filter(v => v !== value) : [...list.value, value]
}

// ------------------------------------------------------------ legend
// Folded on phones, where it would cover half the map.
const legendOpen = ref(window.innerWidth >= 768)
const legend = computed(() => {
  const tally = new Map<string, number>()
  const keyOf = (p: Point) =>
    colorBy.value === 'sexo'
      ? sexOf(p.capture)
      : colorBy.value === 'tipo'
        ? kindOfCapture(p.capture)
        : colorBy.value === 'recorrido'
          ? p.walk.id
          : p.capture.species || ''
  for (const p of points.value) tally.set(keyOf(p), (tally.get(keyOf(p)) || 0) + 1)
  if (colorBy.value === 'sexo' || colorBy.value === 'tipo') {
    const labels = colorBy.value === 'sexo' ? SEX_LABELS : KIND_LABELS
    const colors = colorBy.value === 'sexo' ? SEX_COLORS : KIND_COLORS
    const key: FacetKey = colorBy.value === 'sexo' ? 'sexes' : 'kinds'
    return {
      items: Object.keys(labels).map(v => ({
        value: v,
        label: labels[v],
        color: colors[v],
        count: tally.get(v) || 0,
        key,
        italic: false,
      })),
      other: null,
    }
  }
  if (colorBy.value === 'recorrido') {
    const walks = shownWalks.value.filter(t => tally.has(t.id)).sort((a, b) => b.date.localeCompare(a.date))
    return {
      items: walks.slice(0, 12).map(t => ({
        value: t.date,
        label: `${dateLabel(t.date)} · ${collectorOf(t)}`,
        color: walkColor.value.get(t.id)!,
        count: tally.get(t.id) || 0,
        key: 'dates' as FacetKey,
        italic: false,
      })),
      other:
        walks.length > 12
          ? { groups: walks.length - 12, count: walks.slice(12).reduce((n, t) => n + tally.get(t.id)!, 0) }
          : null,
    }
  }
  const ranked = [...tally].filter(([s]) => s).sort((a, b) => b[1] - a[1])
  const colored = ranked.filter(([s]) => speciesColor.value.has(s))
  const rest = ranked.filter(([s]) => !speciesColor.value.has(s))
  return {
    items: colored.map(([s, n]) => ({
      value: s,
      label: s,
      color: speciesColor.value.get(s)!,
      count: n,
      key: 'species' as FacetKey,
      italic: true,
    })),
    other: rest.length ? { groups: rest.length, count: rest.reduce((n, [, c]) => n + c, 0) } : null,
  }
})

// ------------------------------------------------------------ individuals
const histories = computed(
  () => new Map(markHistories(rows.value).map(h => [individualKey(h.id, String(h.events[0].row.values.SPECIES ?? '')), h])),
)
const chosenIndividual = computed(() => (individual.value ? histories.value.get(individual.value) : undefined))

// ------------------------------------------------------------ map
function sheetRow(t: StoredTrack, c: StoredCapture) {
  // The row the capture was matched to when it was stored.
  if (c.row) return rows.value.find(r => r.row === c.row)
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
let transects: L.FeatureGroup | null = null
let overlay: L.LayerGroup | null = null
let data: L.Layer | null = null
let resize: ResizeObserver | null = null
let plugins: Promise<void> | null = null

/** leaflet.heat and leaflet.markercluster extend the global L; load them only when first needed. */
function loadPlugins() {
  ;(window as unknown as { L: typeof L }).L = L
  plugins ||= Promise.all([
    import('leaflet.heat'),
    import('leaflet.markercluster'),
    import('leaflet.markercluster/dist/MarkerCluster.css'),
  ]).then(() => {})
  return plugins
}

const escape = (s: string) => s.replace(/[&<>"']/g, ch => `&#${ch.charCodeAt(0)};`)

function popup(t: StoredTrack, c: StoredCapture) {
  const row = sheetRow(t, c)
  const key = c.markId && c.species ? individualKey(c.markId, c.species) : ''
  const history = key ? histories.value.get(key) : undefined
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
    `${dateLabel(t.date)} · ${escape(collectorOf(t))}${c.section ? ` · T${c.section}` : ''}`,
    `<span style="color:#78716c">${escape(c.text)}</span>`,
    row ? `Collection_data fila ${row.row}` : '<span style="color:#b45309">Aún no está en la hoja</span>',
    history
      ? `<a href="#/monitoreo?vista=recapturas&individuo=${encodeURIComponent(key)}">Ver sus ${history.events.length} capturas con fotos →</a>`
      : '',
  ]
  const photos = (c.photos || [])
    .map(
      id =>
        `<a href="api/monitoring/photos/${id}" target="_blank" rel="noopener"><img src="api/monitoring/photos/${id}" alt="" width="200" height="150" style="width:200px;height:150px;object-fit:cover;border-radius:4px;margin-top:4px;background:#f5f5f4"></a>`,
    )
    .join('')
  return lines.filter(Boolean).join('<br>') + photos
}

/** A walk's GPS line, broken where the signal jumped (no straight lines across the forest). */
function gpsPieces(t: StoredTrack) {
  const pieces: [number, number][][] = [[]]
  let last: [number, number] | null = null
  for (const p of t.track) {
    const here: [number, number] = [p[0], p[1]]
    if (last && distance(last, here) > 60) pieces.push([])
    pieces.at(-1)!.push(here)
    last = here
  }
  return pieces.filter(p => p.length > 1)
}

/** Cluster icon: the count on a ring coloured by what the cluster holds (as in the atlas). */
function clusterIcon(cluster: L.MarkerCluster) {
  const colors = new Map<string, number>()
  const children = cluster.getAllChildMarkers() as unknown as L.CircleMarker[]
  for (const m of children) {
    const color = m.options.fillColor || OTHER
    colors.set(color, (colors.get(color) || 0) + 1)
  }
  let at = 0
  const stops = [...colors]
    .sort((a, b) => b[1] - a[1])
    .map(([color, n]) => {
      const from = at
      at += (n / children.length) * 360
      return `${color} ${from}deg ${at}deg`
    })
  const size = children.length < 10 ? 30 : children.length < 50 ? 36 : children.length < 150 ? 42 : 50
  return L.divIcon({
    className: '',
    iconSize: [size, size],
    html: `<div class="map-cluster" style="width:${size}px;height:${size}px;background:conic-gradient(${stops.join(',')})"><span>${children.length}</span></div>`,
  })
}

function marker(p: Point) {
  const c = p.capture
  return L.circleMarker([c.lat, c.lon], {
    radius: 7,
    color: c.recapture ? '#ffffff' : '#1c1917',
    weight: c.recapture ? 3 : 1,
    fillColor: colorOf(p),
    fillOpacity: 0.95,
  })
    .bindPopup(() => popup(p.walk, c), { maxWidth: 240 })
    .bindTooltip(`${c.species || '?'}${c.markId ? ` · ${c.markId}` : ''}`, { direction: 'top' })
}

let heat: L.HeatLayer | null = null
const HEAT_MAX_ZOOM = 18
const heatBlur = () => Math.round(heatRadius.value * 0.8)
/**
 * The count that shows as the top colour: the busiest spot at this zoom, the
 * way leaflet.heat adds up points (otherwise the whole trail saturates).
 */
function heatMax() {
  if (!map) return 1
  const cell = (heatRadius.value + heatBlur()) / 2
  const weight = 1 / 2 ** Math.max(0, Math.min(HEAT_MAX_ZOOM - map.getZoom(), 12))
  const grid = new Map<string, number>()
  let top = 0
  for (const { capture } of points.value) {
    const at = map.latLngToContainerPoint([capture.lat, capture.lon])
    const key = `${Math.floor(at.x / cell)}|${Math.floor(at.y / cell)}`
    const n = (grid.get(key) || 0) + weight
    grid.set(key, n)
    top = Math.max(top, n)
  }
  return Math.max(top, weight)
}

let drawing = 0
async function draw() {
  if (!map || !overlay) return
  const run = ++drawing
  if (mode.value !== 'puntos') await loadPlugins()
  if (run !== drawing || !map || !overlay) return
  overlay.clearLayers()
  if (data) map.removeLayer(data)
  data = null
  heat = null

  if (showTransects.value) transects?.addTo(map)
  else transects?.remove()

  if (showGps.value)
    for (const t of shownWalks.value)
      for (const piece of gpsPieces(t))
        L.polyline(piece, {
          color: colorBy.value === 'recorrido' ? walkColor.value.get(t.id) : '#ffffff',
          weight: 2,
          opacity: 0.9,
          dashArray: '4 4',
        })
          .bindTooltip(`${dateLabel(t.date)} · ${collectorOf(t)} (GPS de Wikiloc)`, { sticky: true })
          .addTo(overlay)

  if (showRecaptures.value || individual.value) {
    // Same mark and same species: a mark reused on another species is another butterfly.
    const byMark = new Map<string, [number, number][]>()
    for (const { capture } of [...points.value].sort((a, b) => a.walk.date.localeCompare(b.walk.date)))
      if (capture.markId && capture.species) {
        const key = individualKey(capture.markId, capture.species)
        byMark.set(key, [...(byMark.get(key) || []), [capture.lat, capture.lon]])
      }
    for (const [key, list] of byMark)
      if (list.length > 1)
        L.polyline(list, { color: '#ef4444', weight: 2.5, dashArray: '2 5' })
          .bindTooltip(`Recaptura ${key.replace('|', ' · ')}`, { sticky: true })
          .addTo(overlay)
  }

  if (mode.value === 'calor') {
    heat = L.heatLayer(
      points.value.map(p => [p.capture.lat, p.capture.lon, 1] as L.HeatLatLngTuple),
      {
        radius: heatRadius.value,
        blur: heatBlur(),
        maxZoom: HEAT_MAX_ZOOM,
        max: heatMax(),
        minOpacity: 0.2,
        gradient: { 0.1: 'rgba(49,113,161,0.35)', 0.3: '#428fac', 0.55: '#65b9ac', 0.8: '#b8dd9b', 1: '#f6e8a5' },
      },
    )
    data = heat
  } else if (mode.value === 'grupos') {
    const group = L.markerClusterGroup({
      maxClusterRadius: 45,
      spiderfyOnMaxZoom: true,
      showCoverageOnHover: true,
      polygonOptions: { color: '#4ade80', weight: 1, fillOpacity: 0.15 },
      iconCreateFunction: clusterIcon,
    })
    group.addLayers(points.value.map(marker))
    data = group
  } else {
    const group = L.layerGroup()
    for (const p of points.value) marker(p).addTo(group)
    data = group
  }
  data.addTo(map)
}

onMounted(() => {
  // Canvas draws hundreds of points much faster than one SVG element each.
  map = L.map(host.value!, { zoomControl: true, attributionControl: true, preferCanvas: true })
  const satellite = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
    maxZoom: 20,
    // Esri has no imagery of the trail at zoom 19 ("Map data not yet available"); zoom 18 is enlarged instead.
    maxNativeZoom: 18,
    attribution: 'Imágenes © Esri, Maxar, Earthstar Geographics',
  }).addTo(map)
  const streets = L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
    maxZoom: 20,
    maxNativeZoom: 19,
    attribution: '© OpenStreetMap',
  })
  L.control.layers({ Satélite: satellite, Calles: streets }, undefined, { position: 'topright' }).addTo(map)
  L.control.scale({ imperial: false }).addTo(map)
  transects = L.featureGroup()
  for (const s of SECTIONS) {
    L.polyline(s.path, { color: s.color, weight: 5, opacity: 0.95 })
      .bindTooltip(`Transecto ${s.section}`, { sticky: true })
      .addTo(transects)
    L.marker(s.path[Math.floor(s.path.length / 2)], {
      icon: L.divIcon({ className: 'transect-label', html: `T${s.section}`, iconSize: [26, 16] }),
      interactive: false,
    }).addTo(transects)
  }
  map.fitBounds(transects.getBounds(), { padding: [30, 30] })
  overlay = L.layerGroup().addTo(map)
  map.on('zoomend', () => heat?.setOptions({ max: heatMax() }))
  draw()
  fitIndividual()
  resize = new ResizeObserver(() => map?.invalidateSize())
  resize.observe(host.value!)
})
onBeforeUnmount(() => {
  resize?.disconnect()
  map?.remove()
  map = null
})
watch([points, shownWalks, colorBy, mode, showTransects, showGps, showRecaptures, heatRadius, rows], draw)
/** Choosing one butterfly zooms to its captures. */
function fitIndividual() {
  if (!map || !individual.value || !points.value.length) return
  map.fitBounds(L.latLngBounds(points.value.map(p => [p.capture.lat, p.capture.lon])), { maxZoom: 18, padding: [60, 60] })
}
watch(() => [individual.value, points.value.length], fitIndividual)

// ------------------------------------------------------------ walks of the chosen days
const chosenWalks = computed(() => (lists.dates.value.length ? tracks.value.filter(t => lists.dates.value.includes(t.date)) : []))
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
      class="flex w-76 shrink-0 flex-col overflow-y-auto border-r border-stone-200 bg-white text-sm max-md:absolute max-md:inset-y-0 max-md:left-0 max-md:z-[1200] max-md:pb-20 max-md:shadow-lg"
    >
      <div class="space-y-3 border-b border-stone-200 p-3">
        <div class="stat flex items-baseline gap-1.5 bg-stone-50">
          <span class="text-2xl font-semibold text-brand-700 tabular-nums">{{ points.length }}</span>
          <span class="text-stone-600">de {{ allPoints.length }} capturas</span>
          <span class="ml-auto text-xs text-stone-500">{{ shownWalks.length }} recorridos</span>
        </div>

        <div v-if="individual" class="flex items-start gap-2 rounded-md border border-red-200 bg-red-50 px-2.5 py-2 text-red-950">
          <div class="min-w-0 flex-1">
            <p class="font-medium">
              Individuo {{ individual.split('|')[0] }} · <i>{{ individual.split('|')[1] }}</i>
            </p>
            <p v-if="chosenIndividual" class="text-xs">
              {{ chosenIndividual.events.length }} capturas ·
              <RouterLink :to="{ query: { vista: 'recapturas', individuo: individual } }" class="underline">ver fotos</RouterLink>
            </p>
          </div>
          <button class="btn-ghost" title="Quitar" @click="individual = ''"><X :size="14" /></button>
        </div>

        <FilterSelect
          v-model="lists.dates.value"
          label="Fechas de monitoreo"
          :options="dateOptions"
          all-label="Todas las fechas"
          placeholder="Buscar fecha (p. ej. sep-26)"
        />
        <FilterSelect
          v-model="lists.species.value"
          label="Especies"
          :options="speciesOptions"
          all-label="Todas las especies"
          placeholder="Buscar especie"
        />

        <div v-for="g in chipGroups" :key="g.key">
          <span class="field-label">{{ g.title }}</span>
          <div class="flex flex-wrap gap-1">
            <button
              v-for="c in g.chips"
              :key="c.value"
              type="button"
              class="rounded-full border px-2 py-0.5 text-xs"
              :class="
                lists[g.key].value.includes(c.value)
                  ? 'border-brand-600 bg-brand-50 font-medium text-brand-900'
                  : c.count
                    ? 'border-stone-300 text-stone-700 hover:bg-stone-100'
                    : 'border-stone-200 text-stone-400'
              "
              :aria-pressed="lists[g.key].value.includes(c.value)"
              @click="toggleChip(g.key, c.value)"
            >
              {{ c.label }} <span class="tabular-nums opacity-70">{{ c.count }}</span>
            </button>
          </div>
        </div>

        <div class="flex gap-2">
          <button class="btn flex-1" :disabled="!filterCount" @click="clearFilters">
            <RotateCcw :size="14" /> Quitar filtros<template v-if="filterCount"> ({{ filterCount }})</template>
          </button>
          <button class="btn" title="Copiar el enlace a esta vista" @click="copyLink"><Link2 :size="14" /> Compartir</button>
        </div>
      </div>

      <div class="space-y-3 border-b border-stone-200 p-3">
        <div>
          <span class="field-label">Mostrar como</span>
          <div class="grid grid-cols-3 overflow-hidden rounded-md border border-stone-300 text-center text-sm">
            <button
              v-for="m in [
                { id: 'puntos', label: 'Puntos' },
                { id: 'grupos', label: 'Grupos' },
                { id: 'calor', label: 'Calor' },
              ] as const"
              :key="m.id"
              type="button"
              class="py-1.5 not-first:border-l not-first:border-stone-300"
              :class="mode === m.id ? 'bg-brand-700 font-medium text-white' : 'bg-white text-stone-700 hover:bg-stone-100'"
              @click="mode = m.id"
            >
              {{ m.label }}
            </button>
          </div>
        </div>
        <label v-if="mode === 'calor'" class="block">
          <span class="field-label">Radio del calor: {{ heatRadius }} px</span>
          <input v-model.number="heatRadius" type="range" min="8" max="45" class="w-full" />
        </label>
        <label v-else class="block">
          <span class="field-label">Colorear por</span>
          <select v-model="colorBy" class="field-input">
            <option value="especie">Especie</option>
            <option value="sexo">Sexo</option>
            <option value="tipo">Tipo (preservado, marcado, recaptura)</option>
            <option value="recorrido">Recorrido</option>
          </select>
        </label>
        <label class="flex items-center gap-2"><input v-model="showTransects" type="checkbox" /> Transectos T1–T4</label>
        <label class="flex items-center gap-2"><input v-model="showGps" type="checkbox" /> Trazados GPS de Wikiloc</label>
        <label class="flex items-center gap-2"><input v-model="showRecaptures" type="checkbox" /> Unir recapturas</label>
      </div>

      <div v-if="chosenWalks.length" class="border-b border-stone-200 p-3">
        <p class="field-label">Recorridos de {{ lists.dates.value.length === 1 ? 'ese día' : 'esos días' }}</p>
        <ul class="space-y-1">
          <li v-for="t in chosenWalks" :key="t.id" class="group flex items-center gap-1.5">
            <span class="min-w-0 flex-1 truncate" :title="t.name">
              {{ dateLabel(t.date) }} · {{ collectorOf(t) }} <span class="text-stone-500">({{ t.captures.length }})</span>
            </span>
            <a v-if="t.wikiloc" :href="t.wikiloc.url" target="_blank" rel="noopener" class="btn-ghost" title="Abrir en Wikiloc">
              <ExternalLink :size="13" />
            </a>
            <button v-if="canDelete(t)" class="btn-ghost" title="Quitar recorrido del mapa" @click="remove(t)">
              <Trash2 :size="13" />
            </button>
          </li>
        </ul>
      </div>

      <p v-if="tracksLoaded && !tracks.length" class="hint p-3">Aún no hay recorridos. Súbelos en “Importar”.</p>
      <p class="hint p-3">
        Transectos T1–T4 reconstruidos del mapa QGIS de los reportes sobre el GPS del 26-Sep-26. {{ withoutGps }} registros de
        monitoreo son de días sin recorrido subido; sube sus GPX con “Solo guardar el recorrido” para verlos.
      </p>
    </aside>

    <div class="relative min-w-0 flex-1">
      <div ref="host" class="absolute inset-0" />
      <div class="absolute bottom-6 left-3 z-[1300] flex max-w-[calc(100%-1.5rem)] flex-col items-start gap-2">
        <div
          v-if="mode !== 'calor' && legend.items.length"
          class="max-h-[45vh] w-64 overflow-y-auto rounded-md border border-stone-200 bg-white/95 text-xs shadow"
          :class="panel ? 'max-md:hidden' : ''"
        >
          <button
            type="button"
            class="flex w-full items-center justify-between px-2.5 py-1.5 font-semibold tracking-wide text-stone-600 uppercase"
            @click="legendOpen = !legendOpen"
          >
            {{ { especie: 'Especies', sexo: 'Sexo', tipo: 'Tipo', recorrido: 'Recorridos' }[colorBy] }}
            <span class="font-normal normal-case">{{ legendOpen ? 'ocultar' : 'ver' }}</span>
          </button>
          <ul v-if="legendOpen" class="border-t border-stone-100 py-1">
            <li v-for="item in legend.items" :key="item.value">
              <button
                type="button"
                class="flex w-full items-center gap-2 px-2.5 py-0.5 text-left hover:bg-stone-100"
                :class="lists[item.key].value.includes(item.value) ? 'bg-brand-50' : ''"
                title="Filtrar por esto"
                @click="toggleChip(item.key, item.value)"
              >
                <span class="h-2.5 w-2.5 shrink-0 rounded-full border border-stone-700" :style="{ background: item.color }" />
                <span class="min-w-0 flex-1 truncate" :class="item.italic ? 'italic' : ''">{{ item.label }}</span>
                <span class="text-stone-500 tabular-nums">{{ item.count }}</span>
              </button>
            </li>
            <li v-if="legend.other" class="flex items-center gap-2 px-2.5 py-0.5 text-stone-500">
              <span class="h-2.5 w-2.5 shrink-0 rounded-full border border-stone-700" :style="{ background: OTHER }" />
              <span class="flex-1"
                >Otras · {{ legend.other.groups }} {{ colorBy === 'recorrido' ? 'recorridos' : 'especies' }}</span
              >
              <span class="tabular-nums">{{ legend.other.count }}</span>
            </li>
            <li v-if="colorBy !== 'tipo'" class="px-2.5 pt-1 text-stone-500">Borde blanco: recaptura</li>
          </ul>
        </div>
        <div
          v-else-if="mode === 'calor'"
          class="rounded-md border border-stone-200 bg-white/95 px-2.5 py-1.5 text-xs shadow"
          :class="panel ? 'max-md:hidden' : ''"
        >
          <p class="mb-1 text-stone-600">Capturas por zona</p>
          <div class="heat-scale h-2 w-40 rounded" />
          <div class="flex justify-between text-stone-500"><span>pocas</span><span>muchas</span></div>
        </div>
        <button class="btn shadow" @click="panel = !panel">{{ panel ? 'Ocultar panel' : 'Capas y filtros' }}</button>
      </div>
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
.map-cluster {
  display: grid;
  place-items: center;
  border-radius: 9999px;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.5);
}
.map-cluster span {
  display: grid;
  place-items: center;
  width: calc(100% - 10px);
  height: calc(100% - 10px);
  border-radius: 9999px;
  background: #34404b;
  color: white;
  font:
    600 12px 'Fira Sans',
    sans-serif;
}
.heat-scale {
  background: linear-gradient(90deg, rgba(49, 113, 161, 0.35), #428fac, #65b9ac, #b8dd9b, #f6e8a5);
}
</style>
