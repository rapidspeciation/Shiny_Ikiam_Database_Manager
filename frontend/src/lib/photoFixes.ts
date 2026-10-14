import type { CheckKind, PhotoRow } from './photoLinks'
import { t, tn } from './i18n'

/**
 * The fix a check of a Photo_links reading offers, worked out from the reading's rows (their checks and
 * their files' links): changes to the files for a plan of organize_photos (POST api/photo-links/plan,
 * made only with its card's «Aplicar»), or folders whose photos are read on the server (drive_photos).
 * Null when the check has no fix that is safe without a look: those go to Claude.
 */
export interface PhotoFix {
  label: string
  title: string
  count: number
  operations?: { action: 'rename' | 'remove'; file: string; name?: string }[]
  folders?: string[]
}

/** «CAM07214_D 2.jpg» with another ID: the name the sheets look up, <ID><view><extra>.JPG. */
export function renamedTo(name: string, id: string): string | null {
  const m = /^(.*?\d)[\s_-]*([dv])[\s_-]*(\d*)(\.[^.]+)$/i.exec(name)
  if (!m) return null
  const ext = /^\.jpe?g$/i.test(m[4]) ? '.JPG' : m[4]
  return `${id}${m[2].toLowerCase()}${m[3]}${ext}`
}

export function fixFor(kind: CheckKind, rows: PhotoRow[]): PhotoFix | null {
  const flagged = rows.filter(r => r.url && r.checks?.some(c => c.kind === kind))
  const flag = (r: PhotoRow) => r.checks!.find(c => c.kind === kind)!
  if (kind === 'rename') {
    const operations = flagged.flatMap(r => (flag(r).to ? [{ action: 'rename' as const, file: r.url!, name: flag(r).to! }] : []))
    return operations.length
      ? { label: tn(operations.length, 'Corregir {n} nombre', 'Corregir los {n} nombres'), title: t('Renombra cada foto al nombre que las hojas buscan (con su RAW): un plan que aplicas en su tarjeta'), count: operations.length, operations }
      : null
  }
  if (kind === 'unknownId') {
    const operations = flagged.flatMap(r => {
      const maybe = flag(r).maybe ?? []
      const to = maybe.length === 1 ? renamedTo(r.name, maybe[0]) : null
      return to ? [{ action: 'rename' as const, file: r.url!, name: to }] : []
    })
    return operations.length
      ? { label: tn(operations.length, 'Renombrar {n} al ID probable', 'Renombrar {n} al ID probable'), title: t('Solo las que tienen un único ID probable de la base (un dígito distinto): un plan que revisas y aplicas en su tarjeta'), count: operations.length, operations }
      : null
  }
  if (kind === 'otherFolder') {
    const operations = flagged.filter(r => flag(r).copy).map(r => ({ action: 'remove' as const, file: r.url! }))
    return operations.length
      ? { label: tn(operations.length, 'Quitar {n} copia', 'Quitar {n} copias'), title: t('Quita de esta carpeta las fotos que son la misma foto que ya está en otra (la otra se queda): un plan que aplicas en su tarjeta'), count: operations.length, operations }
      : null
  }
  if (kind === 'unnamed') {
    const folders = [...new Set(flagged.map(r => r.folder).filter(Boolean))]
    return folders.length
      ? { label: tn(folders.length, 'Leer sus IDs en el servidor', 'Leer sus IDs en el servidor ({n} carpetas)'), title: t('El servidor lee el ID del sobre de cada foto sin renombrar y te muestra un lote para renombrarlas'), count: flagged.length, folders }
      : null
  }
  return null
}
