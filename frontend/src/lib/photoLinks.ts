// A reading of the photo folders on Drive for the Photo_links sheet, as the assistant's
// sync_photo_links leaves it in Cambios propuestos (server/photo-links-tool.mjs cardOf): what
// the sheet lacks, what looks wrong in the new photos, and how far its writing is.
import { t, tn } from './i18n'

/** What a new photo is flagged for (server/photo-links.mjs checkPhotos). */
export const CHECK_KINDS = ['rename', 'unknownId', 'unnamed', 'outOfOrder', 'otherFolder', 'unpaired', 'beforeCollection', 'otherCountry'] as const
export type CheckKind = (typeof CHECK_KINDS)[number]

export interface PhotoFlag {
  kind: CheckKind
  /** rename: the name the sheets look up. */
  to?: string
  /** unknownId: workbook IDs it may stand for. */
  maybe?: string[]
  /** outOfOrder: the IDs of the photos taken just before and after. */
  around?: string[]
  /** otherFolder: the other folder with photos of the same ID; `copy`: the same photo there. */
  other?: string
  copy?: boolean
  /** unpaired: the view no listed folder has. */
  missing?: 'dorsal' | 'ventral'
  /** beforeCollection: days before the butterfly's date. */
  days?: number
  date?: string
  /** otherCountry: the butterfly's country, and its folder's. */
  country?: string
  folder?: string
}
export interface PhotoExample extends Omit<PhotoFlag, 'kind'> {
  name: string
  in?: string
  /** The photo on Drive. */
  url?: string
}
export interface PhotoCheck {
  count: number
  examples: PhotoExample[]
}
export type PhotoChecks = { photos: number; linked: number; butterflies: number; otherViews: number } & Record<CheckKind, PhotoCheck>

export type PhotoChange = 'new' | 'folder' | 'rename' | 'relink' | 'remove' | 'move' | 'details'
/** A row the reading would write (GET chat/proposals/<id>/photo-links/rows). */
export interface PhotoRow {
  change: PhotoChange
  folder: string
  name: string
  /** When the photo was taken, by the camera's clock. */
  taken?: string
  /** The file on Drive. */
  url?: string
  /** An existing row of the sheet. */
  row?: number
  /** rename: the name the sheet has. */
  from?: string
  checks?: PhotoFlag[]
}

export interface PhotoProgress {
  state: 'writing' | 'done' | 'queued' | 'unconfirmed' | 'failed'
  saved?: number
  saves?: number
  written?: number
  total?: number
  /** Rows left out because they changed in the sheet meanwhile, and why. */
  skipped?: number
  why?: string
  error?: string
  leftFolders?: string[]
}

export interface PhotoSync {
  add: string[]
  only: string[]
  onlyFolders: string[] | null
  read: { at: string; folders: number; files: number; seconds: number; rows: number }
  /** Rows to write: new ones and changed ones. */
  total: number
  newFiles: number
  newFolders: string[]
  byFolder: { folder: string; new: number; files: number }[]
  changes: { renamed: number; relinked: number; removed: number; moved: number; details: number; statusRows: number }
  examples: {
    renamed: { row: number; from: string; to: string }[]
    relinked: { row: number; name: string }[]
    removed: { row: number; path: string }[]
  }
  checks: PhotoChecks | null
  sameName: { copies: number; different: number; examples: { name: string; in: string; listed: string | null }[] }
  notListed: { folder: string; files: number; photos: number; namedAsSpecimens: number }[]
  leftOut: { toDelete: number; temp: { folder: string; files: number }[] }
  untouched: { outside: number; unchecked: number; repeated: number; unread: number }
  notFound: string[]
  progress?: PhotoProgress
  /** Its rows are no longer kept (part written, or read long ago): read again before applying. */
  stale?: boolean
}

export const checkLabel = (kind: CheckKind): string =>
  ({
    rename: t('Nombre que las hojas no encuentran'),
    unknownId: t('ID que no está en la base'),
    unnamed: t('Sin renombrar (nombre de la cámara)'),
    outOfOrder: t('ID lejos de los de las fotos tomadas antes y después'),
    otherFolder: t('El mismo ID tiene fotos en otra carpeta'),
    unpaired: t('Falta la otra vista'),
    beforeCollection: t('Tomada antes de la fecha de la mariposa'),
    otherCountry: t('Mariposa de otro país que su carpeta'),
  })[kind]

/** What a flag says about its photo, in a few words. */
export function flagText(flag: PhotoFlag | (PhotoExample & { kind: CheckKind })): string {
  switch (flag.kind) {
    case 'rename':
      return t('las hojas buscan {to}', { to: flag.to ?? '' })
    case 'unknownId':
      return flag.maybe?.length ? t('ID que no está en la base (¿{ids}?)', { ids: flag.maybe.join(', ') }) : t('ID que no está en la base')
    case 'unnamed':
      return t('nombre de la cámara')
    case 'outOfOrder':
      return flag.around?.length ? t('tomada entre {ids}', { ids: flag.around.join(' · ') }) : t('fuera de orden')
    case 'otherFolder':
      return flag.copy
        ? t('la misma foto está en {folder}', { folder: flag.other ?? '' })
        : t('otra foto del mismo ID en {folder}', { folder: flag.other ?? '' })
    case 'unpaired':
      return flag.missing === 'dorsal' ? t('falta la dorsal') : t('falta la ventral')
    case 'beforeCollection':
      return tn(flag.days ?? 0, 'tomada {n} día antes de su fecha ({date})', 'tomada {n} días antes de su fecha ({date})', { date: flag.date ?? '' })
    case 'otherCountry':
      return t('es de {country}; su carpeta, de {folder}', { country: flag.country ?? '', folder: flag.folder ?? '' })
  }
}

export const changeLabel = (change: PhotoChange): string =>
  ({
    new: t('nueva'),
    folder: t('carpeta nueva'),
    rename: t('renombrada'),
    relink: t('enlazada de nuevo'),
    remove: t('fila vaciada'),
    move: t('movida'),
    details: t('dueño o tamaño'),
  })[change]

/** The checks that found something, the most frequent first. */
export function foundChecks(checks: PhotoChecks | null): { kind: CheckKind; count: number; examples: PhotoExample[] }[] {
  if (!checks) return []
  return CHECK_KINDS.map(kind => ({ kind, count: checks[kind]?.count ?? 0, examples: checks[kind]?.examples ?? [] }))
    .filter(c => c.count > 0)
    .sort((a, b) => b.count - a.count)
}

/** Rows written with the folders in `skip` left for later (their new files and their own row). */
export function rowsToWrite(sync: PhotoSync, skip: ReadonlySet<string>): number {
  let left = sync.total
  for (const f of sync.byFolder) if (skip.has(f.folder)) left -= f.new
  for (const f of sync.newFolders) if (skip.has(f)) left -= 1
  return Math.max(left, 0)
}

/** How far the writing is, 0–100. */
export const progressPercent = (p: PhotoProgress | undefined): number =>
  !p?.total ? 0 : Math.min(100, Math.round(((p.written ?? 0) / p.total) * 100))

/** The rows shown: all, those to look at, or those flagged for one thing. */
export function filterRows(rows: PhotoRow[], show: 'all' | 'flagged' | CheckKind): PhotoRow[] {
  if (show === 'all') return rows
  if (show === 'flagged') return rows.filter(r => r.checks?.length)
  return rows.filter(r => r.checks?.some(c => c.kind === show))
}

// ------------------------------------------------------------ the files put in order (organize_photos)
/** What is done to a file: binned (the account's own), or renamed …_todelete for its owner to delete (`mark`). */
export type FileChange = 'rename' | 'move' | 'bin' | 'mark' | 'folder' | 'shortcut'
/** A change of the plan (GET chat/proposals/<id>/photo-links/rows). */
export interface FileRow {
  change: FileChange
  folder: string
  name: string
  /** The new name, or the folder it goes into. */
  to?: string
  url?: string
  /** A RAW file following its photo. */
  raw?: boolean
  /** Removed because the same file is elsewhere. */
  copy?: boolean
}
export interface FilesProgress {
  state: 'writing' | 'done' | 'failed'
  written?: number
  total?: number
  failed?: number
  errors?: string[]
  /** 'sheet': the files are done, Photo_links is following. */
  phase?: 'sheet'
  sheetRows?: number
  sheetNote?: string
  error?: string
}
/** A plan of changes to the photo files on Drive, as its card keeps it (server/photo-links-tool.mjs filesCard). */
export interface PhotoFiles {
  type: 'files'
  total: number
  counts: Partial<Record<FileChange, number>>
  raw: number
  problems: { count: number; examples: string[] }
  progress?: FilesProgress
  stale?: boolean
}
export const FILE_CHANGES: FileChange[] = ['rename', 'move', 'bin', 'mark', 'folder', 'shortcut']
export const fileChangeLabel = (change: FileChange): string =>
  ({
    rename: t('renombrar'),
    move: t('mover'),
    bin: t('a la papelera'),
    mark: t('marcar …_todelete'),
    folder: t('carpeta nueva'),
    shortcut: t('acceso directo'),
  })[change]
/** "3 renombrar · 2 a la papelera": what a plan does, in a line. */
export const fileCounts = (counts: PhotoFiles['counts']): string =>
  FILE_CHANGES.filter(c => counts[c])
    .map(c => `${counts[c]} ${fileChangeLabel(c)}`)
    .join(' · ')
/** What a row shows after its arrow: the new name, the folder, or nothing. */
export const fileTarget = (row: FileRow): string => (row.change === 'bin' || row.change === 'folder' ? '' : (row.to ?? ''))
