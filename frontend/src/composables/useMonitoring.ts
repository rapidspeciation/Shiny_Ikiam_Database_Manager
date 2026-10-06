import { computed, ref } from 'vue'
import { api } from '../lib/api'
import {
  isMonitoringRow,
  locateCapture,
  matchWalk,
  monitoringDays,
  noPurpose,
  taxaFrom,
  tribesFrom,
  type ImportedCapture,
  type TrackPoint,
} from '../lib/monitoring'
import { recapturesOutsideSheet, type LoosePoint } from '../lib/monitoringMap'
import { errorText, notify } from '../lib/notice'
import { useTables } from '../stores/tables'
import { useSheet } from './useSheet'

/** A monitoring walk kept in the app: its GPS track and the captures marked on it. */
export interface StoredTrack {
  id: string
  date: string
  collector: string | null
  name: string
  createdBy: string
  createdAt: string
  track: TrackPoint[]
  captures: StoredCapture[]
  wikiloc?: { id: string; url: string } | null
  /** Rows of the walk's day a person said have no point (the pairing board). */
  rowsWithoutPoint?: string[]
  /** Whether the person signed in may remove it (server/monitoring.mjs canDeleteTrack). */
  canDelete?: boolean
}

export type StoredCapture = Pick<
  ImportedCapture,
  | 'lat'
  | 'lon'
  | 'ele'
  | 'text'
  | 'seq'
  | 'species'
  | 'subspecies'
  | 'sex'
  | 'minutes'
  | 'height'
  | 'cloud'
  | 'markId'
  | 'section'
  | 'photos'
> & {
  recapture: boolean
  row?: number | null
  recordId?: string | null
  /** Paired by a person: with a row ('manual') or with none ('none'). */
  link?: 'manual' | 'none' | null
  /** Stored without a row because its pairing was doubtful: left off the map until paired in Dudas. */
  doubt?: boolean
  /** On the map only: a recapture that is not a row of the sheet (its individual's key). */
  outside?: string
}

/** A walk read from a public Wikiloc page by tools/wikiloc, waiting for review. */
export interface WikilocWalk {
  id: string
  wikilocId: string
  url: string
  name: string
  date: string | null
  status: 'waiting' | 'imported'
  trackId: string | null
  createdBy: string
  /** Set when the walk comes from a followed profile (e.g. "AA - Alex Arias"). */
  collector?: string | null
  /** The Wikiloc profile number of the trail's author. */
  author?: string | null
  /** Wikiloc's "Fecha de realización", e.g. "abril 2025". */
  recorded?: string | null
  track: TrackPoint[]
  waypoints: { lat: number; lon: number; ele: number | null; text: string; photos: string[] }[]
}

// Shared by the three monitoring panels, so switching between them does not reload.
const tracks = ref<StoredTrack[]>([])
const tracksLoaded = ref(false)
const walks = ref<WikilocWalk[]>([])

async function loadWalks() {
  try {
    walks.value = (await api<{ walks: WikilocWalk[] }>('monitoring/wikiloc')).walks
  } catch (e) {
    notify(errorText(e), 'error')
  }
}

/** An imported walk back to "por revisar" (server/monitoring.mjs reopenWalk). */
async function reopenWalk(id: string) {
  await api(`monitoring/wikiloc/${encodeURIComponent(id)}/reopen`, { method: 'POST', body: {} })
  await loadWalks()
  return walks.value.find(w => w.id === id) || null
}

async function loadTracks() {
  try {
    tracks.value = (await api<{ tracks: StoredTrack[] }>('monitoring/tracks')).tracks
    tracksLoaded.value = true
  } catch (e) {
    notify(errorText(e), 'error')
  }
}

/** Collection_data restricted to the Ikiam monitoring, plus the stored GPS tracks. */
export function useMonitoring() {
  const module = ref('Collection_data')
  const sheet = useSheet(module)
  const tables = useTables()
  tables.load('SamplingDay_data').catch(() => {})
  /** Collector-days recorded as monitoring in SamplingDay_data. */
  const days = computed(() => (void tables.version, monitoringDays(tables.tables.SamplingDay_data?.rows || [])))
  const rows = computed(() => sheet.table.value?.rows.filter(r => isMonitoringRow(r, days.value)) || [])
  /** Monitoring rows whose Purpose was left empty or "NA" (to fix in the sheet). */
  const withoutPurpose = computed(() => rows.value.filter(noPurpose))
  const taxa = computed(() => taxaFrom(sheet.table.value?.rows || []))
  /** Names seen at Ikiam: abbreviations and a missing subspecies in the notes are read against them first. */
  const localTaxa = computed(() =>
    taxaFrom((sheet.table.value?.rows || []).filter(r => /^ikiam$/i.test(String(r.values.Collection_location ?? '').trim()))),
  )
  /** The 30-preserved rule applies to Ithomiini only (e.g. Heliconius numata is preserved on purpose). */
  const tribes = computed(() => tribesFrom(sheet.table.value?.rows || []))
  const isIthomiini = (species: string | null | undefined) => !!species && tribes.value.get(species) === 'Ithomiini'
  /**
   * Recaptures that are not rows of the sheet: written only in notes, or only as
   * Wikiloc points (on the map, or in walks waiting for review for more than two
   * weeks: a recent walk may just not be entered yet).
   */
  const outsideRecaptures = computed(() => {
    if (!sheet.table.value) return []
    const loose: LoosePoint[] = []
    for (const t of tracks.value)
      t.captures.forEach((c, i) => {
        // A doubtful point's mark may be misread: it is no recapture until paired.
        if (!c.row && c.markId && !c.doubt)
          loose.push({ ...c, date: t.date, collector: t.collector, photos: c.photos || [], ref: `${t.id}|${i}` })
      })
    const recent = new Date(Date.now() - 14 * 864e5).toISOString().slice(0, 10)
    for (const w of walks.value) {
      if (w.status !== 'waiting' || !w.date || !w.collector || w.date > recent) continue
      const points = w.waypoints.map(p => locateCapture({ ...p, time: null }, taxa.value, localTaxa.value))
      const match = matchWalk(rows.value, w.date, w.collector, points)
      points.forEach((c, i) => {
        if (!match.matches[i].rows.length && c.markId)
          loose.push({ ...c, date: match.date, collector: w.collector!, ref: `${w.id}|${i}` })
      })
    }
    return recapturesOutsideSheet(rows.value, loose)
  })
  if (!tracksLoaded.value) {
    loadTracks()
    loadWalks()
  }
  return {
    ...sheet,
    rows,
    days,
    withoutPurpose,
    taxa,
    localTaxa,
    tribes,
    isIthomiini,
    outsideRecaptures,
    tracks,
    tracksLoaded,
    loadTracks,
    walks,
    loadWalks,
    reopenWalk,
  }
}
