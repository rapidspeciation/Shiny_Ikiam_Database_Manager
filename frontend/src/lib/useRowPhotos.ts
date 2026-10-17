import { computed, ref, type Ref } from 'vue'
import type { RowComponent, TabulatorFull } from 'tabulator-tables'
import { driveFileOf, pickRows } from './photoLinks'

/**
 * The rows of a photo card's table (Tabulator) whose photos show beside it (CardPhotos): a click on a
 * row shows its photo, Ctrl/⌘ + click adds or takes one out, Shift + click adds the rows between.
 * A file's name in the table is a link to Drive: a click picks it too (the middle button opens Drive).
 */
export function useRowPhotos<T extends { name: string; url?: string; file?: string }>(note: (row: T) => string = () => '') {
  const picked = ref([]) as Ref<T[]>
  let anchor: T | null = null
  let grid: TabulatorFull | null = null
  const fileOf = (row: T) => row.file || driveFileOf(row.url)
  const photos = computed(() =>
    picked.value.flatMap(r => {
      const file = fileOf(r)
      return file ? [{ file, name: r.name, note: note(r), ...(r.url ? { link: r.url } : {}) }] : []
    }),
  )
  const isPicked = (row: RowComponent) => picked.value.includes(row.getData() as T)
  function mark() {
    for (const row of grid?.getRows() ?? []) row.getElement()?.classList.toggle('is-picked', isPicked(row))
  }
  /** Tabulator's rowFormatter: a row drawn again keeps its mark. */
  const rowFormatter = (row: RowComponent) => row.getElement().classList.toggle('is-picked', isPicked(row))
  function attach(table: TabulatorFull) {
    grid = table
    table.on('rowClick', (e: UIEvent, row: RowComponent) => {
      const event = e as MouseEvent
      const data = row.getData() as T
      if ((event.target as HTMLElement | null)?.closest('a')) event.preventDefault()
      if (!fileOf(data)) return
      // Shift + click selects the page's text otherwise.
      if (event.shiftKey) getSelection()?.removeAllRanges()
      const order = table.getRows('active').map(r => r.getData() as T)
      picked.value = pickRows(picked.value, data, { add: event.ctrlKey || event.metaKey, range: event.shiftKey }, order, anchor)
      anchor = data
      mark()
    })
  }
  function clear() {
    picked.value = []
    anchor = null
    grid = null
  }
  return { picked, photos, attach, rowFormatter, clear }
}
