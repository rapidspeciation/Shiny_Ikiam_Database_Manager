import { reactive } from 'vue'
import { tn, t } from './i18n'

/**
 * The «Fotos» tab's frame: the Rename Photos app (github.com/Fr4nzz/rename_photos_AI,
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
  /** The size of everything in the folder, RAW files too (null until counted). */
  folderBytes?: number | null
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
  /** This page's name for its batch on the server (RenameHost). */
  tab: '',
})

/** The way from the camera's files to the sheets, a step at a time. */
export const RENAME_STEPS = ['open', 'read', 'review', 'rename', 'upload', 'update'] as const
export type RenameStep = (typeof RENAME_STEPS)[number]
export const stepLabel = (step: RenameStep): string =>
  ({
    open: t('Abrir carpeta'),
    read: t('Leer los IDs'),
    review: t('Revisar los IDs'),
    rename: t('Renombrar'),
    upload: t('Subir a Drive'),
    update: t('Actualizar hojas y sitios'),
  })[step]

/**
 * The step a batch is at: its folder to open, its photos to read, photos waiting for a person,
 * files to rename, then to upload; `uploaded`: the person said they are on Drive.
 */
export function stepOf(state: RenameState | null, uploaded = false): RenameStep {
  if (!state || (!state.files && !state.rows.length)) return 'open'
  if (!state.rows.length || state.reading.on) return 'read'
  const named = state.rows.filter(r => !r.skip && r.id)
  if (state.rows.some(r => !r.skip && (r.review.length > 0 || !r.id))) return 'review'
  if (named.some(r => r.status !== 'Renamed')) return 'rename'
  return uploaded ? 'update' : 'upload'
}

/** Where a batch's files go on Drive (GET rename/upload; server/rename-photos.mjs uploadPlace). */
export interface UploadFolder {
  folder: string
  link?: string
  inDriveForDesktop: string
  alsoNear?: string
}
export interface UploadPlace {
  photos?: UploadFolder
  rawFiles?: UploadFolder
  noFolderYet?: string
}
/** A folder's own name, the last of its path. */
export const folderName = (path: string) => path.split('/').filter(Boolean).at(-1) ?? path

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

/** The project account's space on Drive (GET chat/drive-space; server/photo-links-tool.mjs space). */
export interface DriveSpace {
  account?: string
  usedGb: number
  limitGb?: number
  freeGb?: number
  inBinGb?: number
  percent?: number
  nearlyFull?: boolean
}
const oneDecimal = (n: number) => (Math.round(n * 10) / 10).toLocaleString(undefined, { maximumFractionDigits: 1 })
/**
 * The space as the tab's bar says it, and how urgent it is: 'full' when the open folder is larger
 * than what is left, 'low' when little is left (the server's threshold), else 'ok'.
 */
export function spaceLine(space: DriveSpace | null, folderBytes: number | null | undefined): { text: string; level: 'ok' | 'low' | 'full'; folderGb: number | null } | null {
  if (!space || space.limitGb === undefined || space.freeGb === undefined) return null
  const folderGb = folderBytes ? Math.round((folderBytes / 2 ** 30) * 10) / 10 : null
  const level = folderGb !== null && folderGb > space.freeGb ? 'full' : space.nearlyFull ? 'low' : 'ok'
  return {
    text: t('Drive del proyecto: {free} GB libres de {limit}', { free: oneDecimal(space.freeGb), limit: oneDecimal(space.limitGb) }),
    level,
    folderGb,
  }
}

/** A picture as text, for the JSON the server takes. */
export async function blobBase64(blob: Blob): Promise<string> {
  const bytes = new Uint8Array(await blob.arrayBuffer())
  let binary = ''
  for (let i = 0; i < bytes.length; i += 0x8000) binary += String.fromCharCode(...bytes.subarray(i, i + 0x8000))
  return btoa(binary)
}

/** A run of a public site's workflow on GitHub, as the server tells it (server/site-updates.mjs). */
export interface SiteRun {
  started?: string
  ended?: string
  /** 'running', or how it ended: 'success', 'failure', 'cancelled'… */
  state: string
  link?: string
  note?: string
}
export interface SiteStatus {
  site: string
  url: string
  lastUpdate?: SiteRun | 'never run'
  update?: SiteRun
  publishing?: SiteRun
  error?: string
}
export interface SitesAnswer {
  sites: SiteStatus[]
  ready: boolean
}
/** How a run went, in a word. */
export function siteRunLabel(state: string): string {
  if (state === 'running') return t('en curso')
  if (state === 'success') return t('bien')
  if (state === 'failure') return t('falló')
  if (state === 'cancelled') return t('cancelada')
  return state
}
