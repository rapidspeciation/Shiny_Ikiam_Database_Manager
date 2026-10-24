import type { FileRow } from './photoLinks'

/** Select a change and every change it requires. */
export function includeFileChanges(rows: readonly FileRow[], selected: ReadonlySet<number>, indexes: readonly number[]): Set<number> {
  const next = new Set(selected)
  const add = (index: number) => {
    if (index < 0 || index >= rows.length || next.has(index)) return
    next.add(index)
    for (const need of rows[index].needs ?? []) add(need.on)
    const any = rows[index].needsAny ?? []
    if (any.length && !any.some(i => next.has(i))) add(any[0])
  }
  for (const index of indexes) add(index)
  return next
}

/** Remove a change and every selected change that can no longer run. */
export function excludeFileChanges(rows: readonly FileRow[], selected: ReadonlySet<number>, indexes: readonly number[]): Set<number> {
  const next = new Set(selected)
  for (const index of indexes) next.delete(index)
  let changed = true
  while (changed) {
    changed = false
    for (const index of next) {
      const row = rows[index]
      if (row.needs?.some(need => !next.has(need.on)) || (row.needsAny?.length && !row.needsAny.some(i => next.has(i)))) {
        next.delete(index)
        changed = true
      }
    }
  }
  return next
}

export function leftOutFileChanges(rows: readonly FileRow[], selected: ReadonlySet<number>): number[] {
  return rows.flatMap((_, index) => selected.has(index) ? [] : [index])
}
