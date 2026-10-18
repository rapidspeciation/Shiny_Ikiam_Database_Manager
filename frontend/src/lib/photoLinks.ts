// A reading of the photo folders on Drive for the Photo_links sheet, as the assistant's
// sync_photo_links leaves it in Cambios propuestos (server/photo-links-tool.mjs cardOf): what
// the sheet lacks, what looks wrong in the new photos, and how far its writing is.
import { t, tn } from './i18n'

/** What a new photo is flagged for (server/photo-links.mjs checkPhotos). */
export const CHECK_KINDS = ['rename', 'unknownId', 'unnamed', 'outOfOrder', 'otherFolder', 'unpaired', 'beforeCollection', 'otherCountry'] as const
/** What the wing models read on a photo of the folders read, listed before or new (server/photo-links.mjs). */
export const WING_KINDS = ['wingSide', 'wingTurned'] as const
export type CheckKind = (typeof CHECK_KINDS)[number] | (typeof WING_KINDS)[number]

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
  /** wingSide: the side its wings show; `pair`, the verdict on the CAM's two photos ("suffix swapped"…). */
  side?: 'dorsal' | 'ventral'
  pair?: string
  /** wingTurned: how it is turned, and the clockwise turn that puts it upright. */
  turn?: string
  degrees?: number
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
export type PhotoChecks = { photos: number; linked: number; butterflies: number; otherViews: number } & Record<(typeof CHECK_KINDS)[number], PhotoCheck>
export type WingChecks = Record<(typeof WING_KINDS)[number], PhotoCheck>

export type PhotoChange = 'new' | 'folder' | 'rename' | 'relink' | 'remove' | 'move' | 'details' | 'duplicate'
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
  /** details: what changes (owner, size), before → after */
  detail?: string
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
  /** each folder's new rows and its rows that change (by kind: rename, move, remove, relink, details…) */
  byFolder: { folder: string; new: number; files: number | null; changed?: number; kinds?: Record<string, number> }[]
  changes: { renamed: number; relinked: number; removed: number; moved: number; details: number; statusRows: number }
  examples: {
    renamed: { row: number; from: string; to: string }[]
    relinked: { row: number; name: string }[]
    removed: { row: number; path: string }[]
  }
  checks: PhotoChecks | null
  /** The wing models' findings on the photos of the folders read. */
  wings?: WingChecks | null
  sameName: { copies: number; different: number; examples: { name: string; in: string; listed: string | null }[] }
  /** Photos the sheet listed twice: rows emptied (the same file, a copy), and names of two different photos. */
  listedTwice?: {
    sameFile: number
    copies: number
    copyExamples: { name: string; kept: string; emptied: string[] }[]
    different: number
    differentExamples: { name: string; rows: { row: number; path: string; url: string; kb: number }[] }[]
  } | null
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
    wingSide: t('Las alas muestran el otro lado que la d o la v del nombre'),
    wingTurned: t('Las alas dicen que la foto está girada'),
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
    case 'wingSide': {
      const side = flag.side === 'dorsal' ? t('muestra la dorsal') : t('muestra la ventral')
      return flag.pair === 'suffix swapped' ? `${side} (${t('d y v al revés')})` : flag.pair === 'both photos same side' ? `${side} (${t('las dos fotos del mismo lado')})` : side
    }
    case 'wingTurned':
      return t('girarla {degrees}° a la derecha', { degrees: flag.degrees ?? '' })
  }
}

export const changeLabel = (change: PhotoChange): string =>
  ({
    new: t('nueva'),
    folder: t('carpeta nueva'),
    rename: t('renombrada'),
    relink: t('enlazada de nuevo'),
    remove: t('fila vaciada'),
    duplicate: t('repetida: vaciada'),
    move: t('movida'),
    details: t('dueño o tamaño'),
  })[change]

/** The checks that found something, the most frequent first. */
export function foundChecks(checks: PhotoChecks | WingChecks | null | undefined): { kind: CheckKind; count: number; examples: PhotoExample[] }[] {
  if (!checks) return []
  const c = checks as Partial<Record<CheckKind, PhotoCheck>>
  return [...CHECK_KINDS, ...WING_KINDS].map(kind => ({ kind, count: c[kind]?.count ?? 0, examples: c[kind]?.examples ?? [] }))
    .filter(found => found.count > 0)
    .sort((a, b) => b.count - a.count)
}

/** Rows written with some folders' new rows (`skipNew`; a folder not listed yet counts its own row among them) and changed rows (`skipChanges`) left for later. */
export function rowsToWrite(sync: PhotoSync, skipNew: ReadonlySet<string>, skipChanges: ReadonlySet<string> = skipNew): number {
  let left = sync.total
  for (const f of sync.byFolder) {
    if (skipNew.has(f.folder)) left -= f.new
    if (skipChanges.has(f.folder)) left -= f.changed ?? 0
  }
  return Math.max(left, 0)
}

/** The rows that would be written, as the ticks are: a new row (or a new folder's own) by `skipNew`, a changed one by `skipChanges`. */
export function rowsWanted(rows: PhotoRow[], skipNew: ReadonlySet<string>, skipChanges: ReadonlySet<string>): PhotoRow[] {
  if (!skipNew.size && !skipChanges.size) return rows
  return rows.filter(r => (r.change === 'new' || r.change === 'folder' ? !skipNew.has(r.folder) : !skipChanges.has(r.folder)))
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
/**
 * What is done to a file: binned (the account's own), or renamed …_todelete for its owner to delete (`mark`).
 * `sheet`: the sheet «Transfer Ownership & Free Up Space» left ready for another account's step.
 */
export type FileChange = 'rename' | 'move' | 'bin' | 'delete' | 'mark' | 'folder' | 'shortcut' | 'sheet'
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
  /** Removed because the same file is elsewhere: in the folder `keptIn`, named `keptAs` when another name. */
  copy?: boolean
  keptIn?: string
  keptAs?: string
  /** The sheets find this file, not the one kept. */
  lookedUp?: boolean
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
  /** The sheet is ready for another account's step. */
  handover?: Handover
  error?: string
}
/**
 * What another account does once the plan is applied, in the sheet «Transfer Ownership & Free Up
 * Space» (server/photo-files.mjs transferOf): the app writes the folder in it, and the person
 * signed in to that account presses `button`.
 */
export interface Handover {
  step: 'copy' | 'delete'
  button: string
  folder: string
  folderUrl?: string
  sheet: string
  sheetUrl?: string
  subfolders: boolean
  /** delete: for good, not to the bin. */
  permanent?: boolean
  /** delete: the accounts with …_todelete files there; copy: the accounts whose files are there now. */
  by?: { account: string; files: number }[]
  /** The sheet's last run, as it logged it. */
  lastRun?: string
  /** Added by the app for the files the plan leaves to other accounts. */
  auto?: boolean
}
/** A plan of changes to the photo files on Drive, as its card keeps it (server/photo-links-tool.mjs filesCard). */
export interface PhotoFiles {
  type: 'files'
  total: number
  counts: Partial<Record<FileChange, number>>
  raw: number
  problems: { count: number; examples: string[] }
  handover?: Handover
  progress?: FilesProgress
  stale?: boolean
}
export const FILE_CHANGES: FileChange[] = ['rename', 'move', 'bin', 'delete', 'mark', 'folder', 'shortcut', 'sheet']
export const fileChangeLabel = (change: FileChange): string =>
  ({
    rename: t('renombrar'),
    move: t('mover'),
    bin: t('a la papelera'),
    delete: t('borrar definitivamente'),
    mark: t('marcar …_todelete'),
    folder: t('carpeta nueva'),
    shortcut: t('acceso directo'),
    sheet: t('hoja de traspaso'),
  })[change]
/** "x@… (214) · y@… (3)": the accounts of a handover, with their files. */
export const handoverAccounts = (handover: Handover | undefined): string =>
  (handover?.by ?? []).map(b => `${b.account || t('cuenta desconocida')} (${b.files})`).join(' · ')
/** "3 renombrar · 2 a la papelera": what a plan does, in a line. */
export const fileCounts = (counts: PhotoFiles['counts']): string =>
  FILE_CHANGES.filter(c => counts[c])
    .map(c => `${counts[c]} ${fileChangeLabel(c)}`)
    .join(' · ')
/** What a row shows after its arrow: the new name, the folder, or nothing. */
export const fileTarget = (row: FileRow): string => (row.change === 'bin' || row.change === 'delete' || row.change === 'folder' ? '' : (row.to ?? ''))

/**
 * The rows whose photos show beside a card's table after a click on `row` (`order`: the rows as the
 * table lists them now): a click, that row alone (again: none); Ctrl/⌘, that row added or taken out;
 * Shift, the rows from the last one clicked (`anchor`) to it added.
 */
export function pickRows<T>(picked: readonly T[], row: T, how: { add?: boolean; range?: boolean }, order: readonly T[], anchor: T | null): T[] {
  if (how.range && anchor !== null) {
    const [a, b] = [order.indexOf(anchor), order.indexOf(row)]
    if (a >= 0 && b >= 0) {
      const span = order.slice(Math.min(a, b), Math.max(a, b) + 1)
      return [...picked, ...span.filter(r => !picked.includes(r))]
    }
  }
  if (how.add || how.range) return picked.includes(row) ? picked.filter(r => r !== row) : [...picked, row]
  return picked.length === 1 && picked[0] === row ? [] : [row]
}

/** A Drive file's ID in its link (…/d/<id>/view). */
export const driveFileOf = (url: string | undefined) => /\/d\/([\w-]{20,})/.exec(url ?? '')?.[1] ?? ''
