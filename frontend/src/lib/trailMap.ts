import L from 'leaflet'
import 'leaflet/dist/leaflet.css'
import './trailMap.css'
import { t } from './i18n'
import { SECTIONS } from './transects'

/**
 * The Leaflet pieces the monitoring maps share (Mapa, and the small map of a
 * walk's pairing board): the satellite and street layers and the trail drawn
 * as its four coloured sections T1–T4.
 */
export function satelliteLayer() {
  return L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
    maxZoom: 20,
    // Esri has no imagery of the trail at zoom 19 ("Map data not yet available"); zoom 18 is enlarged instead.
    maxNativeZoom: 18,
    attribution: t('Imágenes © Esri, Maxar, Earthstar Geographics'),
  })
}

export function streetsLayer() {
  return L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
    maxZoom: 20,
    maxNativeZoom: 19,
    attribution: '© OpenStreetMap',
  })
}

/** The four sections of the trail, coloured and labelled T1–T4. */
export function transectsLayer({ weight = 5 }: { weight?: number } = {}) {
  const group = L.featureGroup()
  for (const s of SECTIONS) {
    L.polyline(s.path, { color: s.color, weight, opacity: 0.95 })
      .bindTooltip(() => t('Transecto {n}', { n: s.section }), { sticky: true })
      .addTo(group)
    L.marker(s.path[Math.floor(s.path.length / 2)], {
      icon: L.divIcon({ className: 'transect-label', html: `T${s.section}`, iconSize: [26, 16] }),
      interactive: false,
    }).addTo(group)
  }
  return group
}
