<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { TabulatorFull as Tabulator } from 'tabulator-tables'
import type { CellComponent, ColumnDefinition, RowComponent } from 'tabulator-tables'
import 'tabulator-tables/dist/css/tabulator_simple.min.css'
import { Eye, Table2, X } from 'lucide-vue-next'
import { displayValue } from '../../lib/cells'
import { copyText } from '../../lib/clipboard'
import {
  attachColumnFit,
  attachCopyMarker,
  plainCopy,
  backToGrid,
  followSelection,
  selectedCell,
  watchSize,
  type CellBarInfo,
  type CellBarNote,
} from '../../lib/gridKit'
import type { Proposal } from '../../lib/proposals'
import { compareCells, gridRows, isMarked, rowPlace, severalPhotos, tableCounts, type GridRow, type TableRow } from '../../lib/rowsTable'
import type { CellValue, Field } from '../../lib/types'
import { locale, t } from '../../lib/i18n'
import { notify } from '../../lib/notice'
import CellBar from '../CellBar.vue'

/**
 * A table of sheet rows the assistant shows beside the chat (show_rows), in
 * the Cambios propuestos panel: only to read, with the sheet's values as they
 * are now (the panel gets them again whenever one of its rows changes). The
 * columns are the ones the assistant chose; each can be sorted (a click on its
 * name; empty cells last) and fitted (double click on its border). The
 * assistant's note on a row is the "Nota IA" column beside the ID; a note on a
 * cell gives it a corner mark (its tooltip and the bar above the table say
 * it); rows or cells it marks are yellow. A row no longer in the sheet stays,
 * grey. «Cerrar» takes it out of the panel (its link still opens it).
 * A table read from notebook photos says each row's line ("Línea") and, in
 * «Revisar con la foto», the row selected brings its photo and line (`row`).
 */
const props = defineProps<{ table: Proposal }>()
const emit = defineEmits<{ close: []; row: [photo: number | null, line: number | null] }>()

const host = ref<HTMLDivElement>()
let grid: Tabulator | null = null
let built = false
const touch = typeof window !== 'undefined' && window.matchMedia('(pointer: coarse)').matches
const wide = typeof window !== 'undefined' && window.matchMedia('(min-width: 768px)').matches

const rows = computed<TableRow[]>(() => props.table.rows ?? [])
const fields = computed(() => props.table.fields)
const sheet = computed(() => props.table.sheets?.[0] ?? '')
const counts = computed(() => tableCounts(rows.value))
const noted = computed(() => rows.value.some(r => r.note))
/** The rows say where they are on the photos: a "Línea" column (photo · line when they are on several). */
const several = computed(() => severalPhotos(rows.value))
const paged = computed(() => rows.value.some(r => r.page?.line) || several.value)
const open = computed(() => props.table.status === 'shown')
let byKey = new Map<string, TableRow>()

const typeOf = (field: string) => (props.table.types[field] ?? 'text') as Field['type']
const show = (field: string, value: CellValue | undefined) => displayValue(value, { key: field, type: typeOf(field) })
const sorter = ((a: CellValue, b: CellValue, _ra: unknown, _rb: unknown, _col: unknown, dir: 'asc' | 'desc') =>
  compareCells(a, b, dir)) as never

/** A cell: its value, its look (marked, a row gone from the sheet) and the assistant's note on it (corner mark, tooltip). */
function formatter(field: string) {
  return (cell: CellComponent) => {
    const data = cell.getData() as GridRow
    const row = byKey.get(data.__key)
    const el = cell.getElement()
    const note = row?.cells?.[field] ?? ''
    el.classList.toggle('is-marked', isMarked(row, field))
    el.classList.toggle('has-comment', !!note)
    el.title = [note ? t('IA: {text}', { text: note }) : '', row?.missing ? t('Esta fila ya no está en la hoja') : '']
      .filter(Boolean)
      .join('\n')
    const text = show(field, cell.getValue())
    if (!note) return text
    const box = document.createElement('span')
    const mark = document.createElement('span')
    mark.className = 'comment-mark'
    mark.setAttribute('aria-hidden', 'true')
    box.append(text, mark)
    return box
  }
}

function columns(): ColumnDefinition[] {
  const cols: ColumnDefinition[] = []
  const id = {
    title: 'ID',
    field: '__label',
    frozen: true,
    cssClass: 'rows-label',
    sorter,
  } as ColumnDefinition
  if (!wide) cols.push(id)
  cols.push({
    title: t('Fila'),
    field: '__row',
    width: 62,
    frozen: wide,
    hozAlign: 'right',
    cssClass: 'row-number',
    sorter,
    formatter: (cell: CellComponent) => {
      const value = cell.getValue() as number | null
      cell.getElement().title = value === null ? t('Esta fila ya no está en la hoja') : ''
      return value === null ? '—' : String(value)
    },
  } as ColumnDefinition)
  if (paged.value)
    cols.push({
      title: t('Línea'),
      field: '__line',
      width: several.value ? 58 : 50,
      frozen: wide,
      hozAlign: 'right',
      cssClass: 'row-number',
      headerTooltip: several.value ? t('Foto · línea del cuaderno') : t('Línea del cuaderno'),
      sorter,
    } as ColumnDefinition)
  if (wide) cols.push(id)
  if (noted.value)
    cols.push({
      title: t('Nota IA'),
      field: '__note',
      width: 180,
      minWidth: 70,
      cssClass: 'rows-note',
      headerTooltip: t('Nota de la IA sobre la fila'),
      sorter,
      formatter: (cell: CellComponent) => {
        const note = String(cell.getValue() ?? '')
        cell.getElement().title = note
        return note
      },
    } as ColumnDefinition)
  for (const field of fields.value)
    cols.push({
      title: field,
      field,
      minWidth: 70,
      maxInitialWidth: 260,
      sorter,
      formatter: formatter(field) as never,
    } as ColumnDefinition)
  return cols
}
/** A cell as copied: dates as 2026-10-04, the rest as shown (lib/clipboard). */
function copyCell(cell: CellComponent) {
  const field = cell.getField()
  return field.startsWith('__') ? String(cell.getValue() ?? '') : copyText(cell.getValue(), { key: field, type: typeOf(field) })
}
function rowLook(row: RowComponent) {
  const r = byKey.get((row.getData() as GridRow).__key)
  const el = row.getElement()
  el.classList.toggle('is-missing-row', !!r?.missing)
  el.classList.toggle('is-marked-row', !!r?.highlight)
}

// ------------------------------------------------------------ the cell bar (read-only)
const bar = ref<CellBarInfo | null>(null)
function describe(cell: CellComponent | null): CellBarInfo | null {
  if (!cell) return null
  const data = cell.getData() as GridRow
  const field = cell.getField()
  const row = byKey.get(data.__key)
  const base = { index: data.__key, field, row: data.__label, editable: false, multiline: false }
  const column = { __label: 'ID', __row: t('Fila'), __note: t('Nota IA'), __line: t('Línea') }[field] ?? field
  const value = data[field] as CellValue
  const text =
    field === '__row' ? (value === null ? '—' : String(value)) : field.startsWith('__') ? String(value ?? '') : show(field, value)
  const notes: CellBarNote[] = []
  if (row?.cells?.[field]) notes.push({ label: t('IA'), text: row.cells[field], kind: 'hint' })
  if (row?.missing) notes.push({ text: t('Esta fila ya no está en la hoja'), kind: 'edited' })
  return { ...base, column, text, notes, readonly: t('Tabla del asistente: solo para leer') }
}
/** The row selected last: a new one says its photo and line (for the photo beside the table). */
let selectedKey: string | null = null
function showBar() {
  const cell = grid ? selectedCell(grid) : null
  bar.value = describe(cell)
  const key = cell ? (cell.getData() as GridRow).__key : null
  if (key === selectedKey) return
  selectedKey = key
  if (key) emit('row', ...rowPlace(byKey.get(key)))
}

// ------------------------------------------------------------ drawing
let shownColumns = ''
let shownData = ''
const layoutKey = () => [fields.value.join('|'), noted.value, paged.value, several.value, locale.value].join('\n')
function sync() {
  if (!grid || !built) return
  byKey = new Map(rows.value.map(r => [r.key, r]))
  const layout = layoutKey()
  if (layout !== shownColumns) {
    shownColumns = layout
    grid.setColumns(columns())
    shownData = ''
  }
  const data = gridRows(rows.value, fields.value)
  const signature = JSON.stringify([data, rows.value.map(r => [r.cells, r.marked, r.highlight, r.missing])])
  if (signature === shownData) return
  shownData = signature
  // The person's sort is kept when the values change.
  void grid.replaceData(data)
}

let fit: { destroy: () => void } | null = null
let copied: ReturnType<typeof attachCopyMarker> | null = null
let sizeWatch: { disconnect: () => void } | null = null
let shownWatch: ResizeObserver | null = null
onMounted(() => {
  if (!host.value) return
  byKey = new Map(rows.value.map(r => [r.key, r]))
  shownColumns = layoutKey()
  grid = new Tabulator(host.value, {
    data: [],
    index: '__key',
    columns: columns(),
    layout: 'fitData',
    // At most the panel's height (less its title and bar), so the column names stay in sight (see ProposalSheet).
    maxHeight: 'max(10rem, calc(100cqh - 9rem))',
    autoResize: false,
    placeholder: t('Sin filas'),
    selectableRange: touch ? false : 1,
    selectableRangeColumns: true,
    selectableRangeRows: true,
    selectableRangeClearCells: false,
    clipboard: 'copy',
    // Copied as plain text: dates as 2026-10-04, the rest as shown (lib/clipboard).
    ...plainCopy(() => grid, copyCell),
    // A click on a column's name sorts it (again: the other way, then as the assistant gave it).
    columnDefaults: { headerSort: true, headerSortTristate: true, resizable: 'header' },
    rowFormatter: rowLook,
  } as unknown as ConstructorParameters<typeof Tabulator>[1])
  grid.on('tableBuilt', () => {
    built = true
    sync()
  })
  const container = host.value.parentElement!
  if (!touch) copied = attachCopyMarker(grid, container, message => notify(message))
  fit = attachColumnFit(grid, host.value, {
    text: (data, field) => (field.startsWith('__') ? String(data[field] ?? '') : show(field, data[field] as CellValue)),
  })
  const follow = followSelection(grid, showBar)
  for (const event of ['scrollVertical', 'scrollHorizontal'] as const) grid.on(event as 'renderComplete', follow)
  sizeWatch = watchSize(() => grid, host.value)
  // Built while the panel was hidden: drawn when it shows.
  let hidden = !host.value.offsetWidth
  shownWatch = new ResizeObserver(([entry]) => {
    const zero = !entry.contentRect.width
    if (hidden && !zero && grid && built) grid.redraw(true)
    hidden = zero
  })
  shownWatch.observe(host.value)
})
onBeforeUnmount(() => {
  fit?.destroy()
  copied?.destroy()
  sizeWatch?.disconnect()
  shownWatch?.disconnect()
  grid?.destroy()
  grid = null
})
watch(() => [props.table, locale.value], sync)
</script>

<template>
  <div class="mt-2 rounded-md border border-sky-300 bg-white text-stone-800">
    <p class="flex flex-wrap items-center gap-x-2 border-b border-stone-200 px-2 py-1.5 text-xs font-medium">
      <Table2 :size="13" class="shrink-0 text-sky-700" />
      <span>
        {{ $t('Tabla') }} · {{ sheet }} · {{ $tn(counts.rows, '{n} fila', '{n} filas') }}
        <span class="font-normal text-stone-500">— {{ table.reason }}</span>
      </span>
      <span
        class="flex items-center gap-1 rounded bg-sky-50 px-1.5 py-0.5 font-normal text-sky-900"
        :title="$t('El asistente muestra estas filas con los valores actuales de la hoja: no cambia nada')"
      >
        <Eye :size="12" /> {{ $t('solo lectura') }}
      </span>
      <span v-if="counts.missing" class="font-normal text-stone-500">
        {{ $tn(counts.missing, '{n} ya no está en la hoja', '{n} ya no están en la hoja') }}
      </span>
      <button
        v-if="open"
        type="button"
        class="ml-auto flex items-center gap-0.5 rounded px-1 py-0.5 font-normal text-stone-500 hover:bg-stone-100 hover:text-stone-800"
        :title="$t('Quitar esta tabla del panel (su enlace la sigue abriendo)')"
        @click="emit('close')"
      >
        <X :size="13" /> {{ $t('Cerrar') }}
      </button>
      <span v-else class="ml-auto font-normal text-stone-500">{{ $t('Cerrada') }}</span>
    </p>
    <div class="sheet-grid rows-table">
      <CellBar :info="bar" notes-line @back="move => grid && backToGrid(grid, move)" />
      <!-- The grid's own box: the copied cells' border is placed in it, below the bar. -->
      <div class="relative">
        <div ref="host" tabindex="-1" />
      </div>
    </div>
  </div>
</template>

<style>
.rows-table .tabulator {
  font-size: 12px;
}
.rows-table .tabulator-cell.rows-label {
  font-weight: 600;
}
.rows-table .tabulator-cell.rows-note {
  color: #57534e;
}
/* Marked by the assistant: the row, or a cell. */
.rows-table .tabulator-row.is-marked-row .tabulator-cell,
.rows-table .tabulator-cell.is-marked {
  background: #fef9c3;
}
/* No longer in the sheet: grey, in italics. */
.rows-table .tabulator-row.is-missing-row .tabulator-cell {
  background: #fafaf9;
  color: #a8a29e;
  font-style: italic;
}
/* The assistant says something about the cell: a corner, as a comment in Google Sheets. */
.rows-table .tabulator-cell.has-comment {
  position: relative;
}
.rows-table .tabulator-cell .comment-mark {
  position: absolute;
  top: 0;
  right: 0;
  border-style: solid;
  border-width: 0 7px 7px 0;
  border-color: transparent #ea8600 transparent transparent;
  pointer-events: none;
}
</style>
