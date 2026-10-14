import { reactive } from 'vue'
import { tn, t } from './i18n'

/**
 * The «Renombrar fotos» tab's frame: the Rename Photos app (github.com/Fr4nzz/rename_photos_AI,
 * built for this site and served from rename-app/), which reads the IDs on the envelopes of a
 * folder of wing photos on the person's computer and renames the files after them.
 * Like T3's (lib/t3Host.ts), the frame lives for the whole session once the tab was opened
 * (RenameHost, in App.vue): taken out of the page it would load again and lose the folder and its
 * readings. The tab only marks where it goes (`slot`).
 * RenameHost also links the frame to the app's server, so the assistant can check the batch
 * (server/rename-photos.mjs): `state` is what the frame last said of it.
 */
export interface RenameRow {
  file: string
  id: string
  skip?: boolean
  /** Why the reader left the photo for a person (none: sure, or confirmed). */
  review: string[]
  /** What the assistant changed in it. */
  byHost?: string[]
  status?: string
}
export interface RenameState {
  folder: string | null
  canWrite: boolean
  files: number
  reading: { on: boolean; percent: number; message: string }
  rows: RenameRow[]
}
export const renameHost = reactive({
  /** The tab was opened this session: the frame is loaded and kept. */
  started: false,
  /** The place the frame covers (null: the tab is not on screen). */
  slot: null as HTMLElement | null,
  /** The pointer goes through the frame (the divider beside it is being dragged). */
  passThrough: false,
  /** The frame's app answered. */
  ready: false,
  /** The app's build is not on this server. */
  missing: false,
  state: null as RenameState | null,
})

/** A batch in numbers. */
export function batchCounts(state: RenameState | null) {
  const rows = state?.rows ?? []
  return {
    photos: rows.length,
    waiting: rows.filter(r => !r.skip && r.review.length > 0).length,
    withoutId: rows.filter(r => !r.skip && !r.id).length,
    byAssistant: rows.filter(r => r.byHost?.length).length,
    renamed: rows.filter(r => r.status === 'Renamed').length,
  }
}

/** "PERU batch · 312 fotos · 14 por revisar": the batch in a line (empty before a folder is read). */
export function batchLine(state: RenameState | null): string {
  if (!state) return ''
  const c = batchCounts(state)
  const parts: string[] = []
  if (state.folder) parts.push(state.folder)
  if (state.reading.on) parts.push(t('leyendo {percent}%', { percent: state.reading.percent }))
  else if (c.photos) {
    parts.push(tn(c.photos, '{n} foto', '{n} fotos'))
    parts.push(c.waiting ? tn(c.waiting, '{n} por revisar', '{n} por revisar') : t('ninguna por revisar'))
    if (c.byAssistant) parts.push(tn(c.byAssistant, '{n} cambiada por el asistente', '{n} cambiadas por el asistente'))
  } else if (state.files) parts.push(tn(state.files, '{n} foto sin leer', '{n} fotos sin leer'))
  return parts.join(' · ')
}

/** A picture as text, for the JSON the server takes. */
export async function blobBase64(blob: Blob): Promise<string> {
  const bytes = new Uint8Array(await blob.arrayBuffer())
  let binary = ''
  for (let i = 0; i < bytes.length; i += 0x8000) binary += String.fromCharCode(...bytes.subarray(i, i + 0x8000))
  return btoa(binary)
}
