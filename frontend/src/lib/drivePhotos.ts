import { t } from './i18n'

/**
 * A batch of photos uploaded to Drive without being renamed, read on the server (server/drive-photos.mjs):
 * its card's summary (in the proposal's `sync`) and its rows (GET api/drive-photos/<batch>).
 */
export interface DrivePhotosSync {
  type: 'photos'
  batch: string
  folder: string
  /** reading → ready → applied (or failed, discarded) */
  state: 'reading' | 'ready' | 'applied' | 'failed' | 'discarded'
  error?: string | null
  plan?: string | null
  photos?: number
  read?: number
  ids?: number
  marked?: number
  toRemove?: number
  turned?: number
  order?: string
}

export interface DrivePhotoRow {
  file: string
  n: number
  name: string
  url?: string | null
  folder?: string
  taken?: string | null
  raw?: string
  rawName?: string
  read: boolean
  camid?: string | null
  /** What the reader read, when another ID was set after. */
  readAs?: string | null
  conf?: number
  view?: string
  rotation?: number
  remove?: boolean
  marked?: boolean
  review?: string[]
  setBy?: 'reader' | 'assistant' | 'person'
  note?: string
  error?: string
}

/** The name a row's photo takes once applied, or '' without an ID. */
export function newName(row: DrivePhotoRow): string {
  if (row.remove) return ''
  return row.camid && row.view ? `${row.camid}${row.view}.JPG` : ''
}

/** Why a row is shown to look at, in a few words. */
export function reasonText(reason: string): string {
  return (
    {
      'no ID read': t('sin ID leído'),
      'unsure reading': t('lectura dudosa'),
      'rotation-outlier': t('girada distinto que su sesión'),
      checks: t('su ID no suma dos fotos, o está fuera de orden'),
    } as Record<string, string>
  )[reason] ?? reason
}

/** The rows to show: those to look at (and those someone changed), or all. */
export function shownRows(rows: DrivePhotoRow[], which: 'look' | 'all'): DrivePhotoRow[] {
  return which === 'all' ? rows : rows.filter(r => r.marked || r.error || r.remove || (r.setBy && r.setBy !== 'reader'))
}
