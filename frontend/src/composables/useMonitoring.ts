import { computed, ref } from 'vue'
import { api } from '../lib/api'
import {
  isMonitoringRow,
  monitoringDays,
  noPurpose,
  taxaFrom,
  tribesFrom,
  type ImportedCapture,
  type TrackPoint,
} from '../lib/monitoring'
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
> & { recapture: boolean }

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
  /** The 30-preserved rule applies to Ithomiini only (e.g. Heliconius numata is preserved on purpose). */
  const tribes = computed(() => tribesFrom(sheet.table.value?.rows || []))
  const isIthomiini = (species: string | null | undefined) => !!species && tribes.value.get(species) === 'Ithomiini'
  if (!tracksLoaded.value) {
    loadTracks()
    loadWalks()
  }
  return { ...sheet, rows, days, withoutPurpose, taxa, tribes, isIthomiini, tracks, tracksLoaded, loadTracks, walks, loadWalks }
}
