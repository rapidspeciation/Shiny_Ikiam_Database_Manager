import { computed, ref } from 'vue'
import { api } from '../lib/api'
import { isMonitoringRow, taxaFrom, tribesFrom, type ImportedCapture, type TrackPoint } from '../lib/monitoring'
import { errorText, notify } from '../lib/notice'
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
> & { recapture: boolean }

// Shared by the three monitoring panels, so switching between them does not reload.
const tracks = ref<StoredTrack[]>([])
const tracksLoaded = ref(false)

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
  const rows = computed(() => sheet.table.value?.rows.filter(isMonitoringRow) || [])
  const taxa = computed(() => taxaFrom(sheet.table.value?.rows || []))
  /** The 30-preserved rule applies to Ithomiini only (e.g. Heliconius numata is preserved on purpose). */
  const tribes = computed(() => tribesFrom(sheet.table.value?.rows || []))
  const isIthomiini = (species: string | null | undefined) => !!species && tribes.value.get(species) === 'Ithomiini'
  if (!tracksLoaded.value) loadTracks()
  return { ...sheet, rows, taxa, tribes, isIthomiini, tracks, tracksLoaded, loadTracks }
}
