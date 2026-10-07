import { ref, watch, type Ref } from 'vue'
import type { ClutchDay, ClutchLog, DayNote } from './useClutchDay'
import type { GroupRow } from '../lib/clutchGroups'
import { usePhotoUploads } from './usePhotoUploads'
import { api } from '../lib/api'
import type { ClutchPhoto } from '../lib/clutchPhotos'
import type { ClutchEvent, ClutchTallies, YoungRow } from '../lib/clutches'

export interface ClutchRecord {
  events: ClutchEvent[]
  young: YoungRow[]
  photos?: ClutchPhoto[]
  /** Its groups by count (open ones in the order of their parentheses, then those ended). */
  groups?: GroupRow[]
  /** Regroupings and formulas edited by hand. */
  log?: ClutchLog[]
  /** Its notes of each day. */
  notes?: DayNote[]
  tally: ClutchTallies
}

/**
 * What the app keeps of one clutch besides the sheet (GET clutches/events): its
 * events day by day, the eggs and larvae registered one by one in Emergidos,
 * its photos and what the events add up to. Asked again when an event is
 * recorded or taken back, when the day's list changes and when a photo arrives;
 * the editor's chips (their photos) and its history both read it.
 */
export function useClutchRecord(recordId: Ref<string>, day: ClutchDay) {
  const uploads = usePhotoUploads()
  const data = ref<ClutchRecord | null>(null)
  async function load() {
    const id = recordId.value
    if (!id) return
    try {
      const out = await api<ClutchRecord>(`clutches/events?recordId=${encodeURIComponent(id)}`)
      if (id === recordId.value) data.value = out
    } catch {
      /* offline: keep what was shown */
    }
  }
  watch(recordId, () => (data.value = null))
  watch(
    () => [recordId.value, day.eventsVersion.value, day.day.value.events?.length, day.day.value.photos?.length, day.day.value.notes?.length, uploads.stored.value],
    load,
    { immediate: true },
  )
  /** A photo taken away (its viewer): gone at once. */
  function photoRemoved(id: string) {
    if (data.value) data.value = { ...data.value, photos: (data.value.photos ?? []).filter(p => p.id !== id) }
  }
  return { data, load, photoRemoved }
}
export type ClutchRecordState = ReturnType<typeof useClutchRecord>
