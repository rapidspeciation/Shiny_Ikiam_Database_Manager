<script setup lang="ts">
import { computed, ref } from 'vue'
import { ChevronRight, CircleHelp, Columns3, SquarePen, Table2 } from 'lucide-vue-next'
import { readingValue } from '../../lib/cells'
import { tx } from '../../lib/i18n'
import { sharedFold, sharedNote, stackRows, type StackField, type StackRow } from '../../lib/proposalStack'
import type { ProposalChange } from '../../lib/proposals'
import type { CellValue } from '../../lib/types'

/**
 * A proposal's table on a phone (lib/proposalStack.ts): one box per row, its changed cells one under the other,
 * the sheet's value over the new one, in the table's colours (the assistant's green, the person's blue, a doubtful
 * reading amber, an unreadable cell hatched red, a cell edited in the sheet violet). Nothing scrolls sideways, so
 * every change is seen before «Aplicar». A cell that waits for a decision opens in the table with a tap. Columns
 * that only carry the template's NA or NOT_COLLECTED are counted on each row and opened on request, as the table
 * folds them. Dates read in the interface language (the table shows them as the sheet does).
 */
const props = defineProps<{
  rows: ProposalChange[]
  /** Every column of the table, in its order (the folded ones too). */
  fields: string[]
  types: Record<string, string>
  newRowFormulas?: string[]
  /** The template's columns (only NA or NOT_COLLECTED), folded until asked for. */
  template?: string[]
}>()
const emit = defineEmits<{ cell: [key: string, field: string] }>()

const items = computed(() => stackRows(props.rows, props.fields, props.newRowFormulas ?? [], props.template ?? []))
/** A note every row shares is said once, above them. */
const common = computed(() => sharedNote(items.value))
/** The template's cells every row folds alike: counted once, above the rows (else each row counts its own). */
const commonFold = computed(() => sharedFold(items.value))
const templatesOpen = ref(false)
const shownFields = (r: StackRow) => (templatesOpen.value ? r.fields : r.fields.filter(f => !f.template))
const folded = (r: StackRow) => r.fields.filter(f => f.template).length
const show = (field: string, value: CellValue | undefined) => readingValue(value, { key: field, type: (props.types[field] ?? 'text') as 'text' })
const whyOf = (f: StackField) => (f.why ? tx(f.why, f.whyMsg) : '')
/**
 * The few words of a cell that is not simply written, as the table's legend has them (Spanish keys). The
 * assistant's and the person's values say whose they are beside the value.
 */
const TAG: Record<StackField['state'], string> = {
  ai: '',
  person: '',
  doubtful: 'dudosa',
  unreadable: 'ilegible',
  kept: 'editada en la hoja',
  reverted: 'IA sin aplicar',
  warned: 'falta',
}
const tagOf = (f: StackField) => TAG[f.state] || (f.inferred ? 'deducida' : '')
</script>

<template>
  <ul class="proposal-stack" data-proposal-stack>
    <li v-if="common" class="px-1 text-xs leading-snug text-fg-muted" data-shared-note>{{ common }}</li>
    <li v-if="commonFold" class="px-1">
      <button type="button" class="stack-template" :aria-expanded="templatesOpen" data-template @click="templatesOpen = !templatesOpen">
        <Columns3 :size="14" aria-hidden="true" />
        {{
          templatesOpen
            ? $t('Plegar la plantilla')
            : $tn(commonFold, 'Cada fila: +{n} columna de plantilla', 'Cada fila: +{n} columnas de plantilla')
        }}
        <span v-if="!templatesOpen" class="font-normal text-fg-subtle">· NA / NOT_COLLECTED</span>
      </button>
    </li>
    <li
      v-for="r in items"
      :key="r.key"
      class="stack-row"
      :class="{ 'is-written': r.written, 'is-refused': r.refused, 'is-look': r.look, 'is-marked': r.marked }"
      :data-row="r.key"
    >
      <div class="flex flex-wrap items-baseline gap-x-2 gap-y-0.5">
        <strong class="text-base leading-tight" :class="{ 'stack-dead': r.dead }">{{ r.label || '—' }}</strong>
        <span v-if="r.refused" class="stack-chip is-refused">{{ $t('no se escribe') }}</span>
        <span v-else-if="r.create" class="stack-chip is-new">{{ $t('fila nueva') }}</span>
        <span v-if="r.dead" class="stack-chip is-dead">{{ $t('muerta en la hoja') }}</span>
        <span v-if="r.marked" class="stack-chip is-marked">{{ $t('marcada') }}</span>
        <span v-if="r.written" class="stack-chip">{{ $t('ya escrita') }}</span>
        <span v-if="r.row" class="ml-auto text-xs text-fg-subtle tabular-nums">{{ $t('fila {row}', { row: r.row }) }}</span>
      </div>
      <p
        v-if="r.note && r.note !== common"
        class="mt-0.5 text-xs leading-snug"
        :class="r.refused ? 'text-error-text' : r.look ? 'text-pending-text' : 'text-fg-muted'"
        data-note
      >
        {{ r.note }}
      </p>
      <dl v-if="r.fields.length" class="stack-fields">
        <component
          :is="f.needsTable ? 'button' : 'div'"
          v-for="f in shownFields(r)"
          :key="f.field"
          class="stack-field"
          :class="`is-${f.state}`"
          :type="f.needsTable ? 'button' : undefined"
          :data-field="f.field"
          :data-state="f.state"
          @click="f.needsTable && emit('cell', r.key, f.field)"
        >
          <dt class="flex items-baseline gap-2">
            <span class="min-w-0 flex-1 truncate">{{ f.field }}</span>
            <span v-if="tagOf(f)" class="stack-tag" :class="{ 'is-inferred': f.inferred }">{{ $t(tagOf(f)) }}</span>
          </dt>
          <dd>
            <!-- The sheet's value now: over the new one, struck when it is replaced. -->
            <span v-if="!r.create && f.state !== 'kept' && f.state !== 'reverted'" class="stack-old" data-old>
              <span class="stack-side">{{ $t('hoja') }}</span>
              <s v-if="show(f.field, f.was)">{{ show(f.field, f.was) }}</s>
              <i v-else>{{ $t('vacío') }}</i>
            </span>
            <span class="stack-new" data-new>
              <span class="stack-side">{{ f.state === 'kept' || f.state === 'reverted' ? $t('hoja') : f.state === 'person' ? $t('tú') : $t('IA') }}</span>
              <b v-if="show(f.field, f.value)">{{ show(f.field, f.value) }}</b>
              <i v-else>{{ f.state === 'unreadable' ? $t('sin leer') : $t('vacío') }}</i>
            </span>
            <!-- Set aside, not written: the proposal's value (the sheet's edit stays), or the assistant's (set back). -->
            <span v-if="f.state === 'kept' || f.state === 'reverted'" class="stack-old" data-aside>
              <span class="stack-side">{{ $t('IA') }}</span>
              <s v-if="show(f.field, f.aside)">{{ show(f.field, f.aside) }}</s>
              <i v-else>{{ $t('vacío') }}</i>
            </span>
            <span v-if="whyOf(f)" class="stack-why">{{ whyOf(f) }}</span>
            <span v-if="f.needsTable" class="stack-go">
              <CircleHelp v-if="f.state === 'doubtful'" :size="14" aria-hidden="true" />
              <SquarePen v-else-if="f.state === 'unreadable'" :size="14" aria-hidden="true" />
              <Table2 v-else :size="14" aria-hidden="true" />
              {{ f.state === 'unreadable' ? $t('Escribirla en la tabla') : $t('Revisarla en la tabla') }}
              <ChevronRight :size="14" class="ml-auto" aria-hidden="true" />
            </span>
          </dd>
        </component>
        <!-- The template's columns: written all the same, counted here and opened on request. -->
        <button
          v-if="folded(r) && !commonFold"
          type="button"
          class="stack-template"
          :aria-expanded="templatesOpen"
          data-template
          @click="templatesOpen = !templatesOpen"
        >
          <Columns3 :size="14" aria-hidden="true" />
          {{ templatesOpen ? $t('Plegar la plantilla') : $tn(folded(r), '+{n} columna de plantilla', '+{n} columnas de plantilla') }}
          <span v-if="!templatesOpen" class="font-normal text-fg-subtle">· NA / NOT_COLLECTED</span>
        </button>
      </dl>
    </li>
    <li v-if="!items.length" class="px-1 py-3 text-sm text-fg-muted">{{ $t('Esta tabla no cambia ninguna celda.') }}</li>
  </ul>
</template>

<style scoped>
.proposal-stack {
  display: grid;
  gap: 8px;
  padding: 8px;
}
/* One row of the sheet: a box with the brand's edge on its left, as the table marks the row it writes. */
.stack-row {
  border: 1px solid var(--border-strong);
  border-left: 3px solid var(--cell-ai-edge);
  border-radius: 8px;
  background: var(--surface);
  padding: 10px 10px 10px 11px;
}
.stack-row.is-written {
  border-left-color: var(--border-strong);
  opacity: 0.8;
}
.stack-row.is-refused {
  border-left-color: var(--error-edge);
}
.stack-row.is-look {
  border-left-color: var(--pending-edge);
}
/* Marked by the assistant for a look: the table's yellow row. */
.stack-row.is-marked {
  background: var(--color-yellow-100);
}
.stack-chip.is-refused {
  background: var(--cell-error);
  color: var(--error-text);
}
.stack-chip.is-marked {
  background: var(--color-yellow-300);
  color: var(--fg);
}
/* Not written on the notebook line: italic, as the table writes it. */
.stack-tag.is-inferred {
  font-style: italic;
  font-weight: 400;
}
/* The template's columns, folded: a line a finger can hit. */
.stack-template {
  display: flex;
  width: 100%;
  min-height: 44px;
  align-items: center;
  gap: 6px;
  text-align: left;
  font-size: 13px;
  font-weight: 500;
  color: var(--fg-muted);
}
.stack-dead {
  border-radius: 3px;
  background: var(--color-yellow-300);
  padding: 0 4px;
  color: var(--fg);
}
.stack-chip {
  border-radius: 3px;
  background: var(--surface-sunken);
  padding: 0 5px;
  font-size: 12px;
  white-space: nowrap;
  color: var(--fg-muted);
}
.stack-chip.is-new {
  background: var(--cell-ai);
  box-shadow: inset 0 0 0 1px var(--cell-ai-edge);
  color: var(--cell-ai-text);
}
.stack-chip.is-dead {
  background: var(--color-yellow-300);
  color: var(--fg);
}
.stack-fields {
  display: grid;
  margin-top: 8px;
  border-top: 1px solid var(--border);
}
/* One cell: its column's name, then the sheet's value over the new one. */
.stack-field {
  display: block;
  width: 100%;
  border-bottom: 1px solid var(--border);
  padding: 7px 0;
  text-align: left;
}
.stack-field:last-child {
  border-bottom: 0;
  padding-bottom: 0;
}
.stack-field dt {
  font-size: 12px;
  font-weight: 600;
  letter-spacing: 0.01em;
  color: var(--fg-muted);
}
.stack-tag {
  flex: none;
  border-radius: 3px;
  padding: 0 5px;
  font-size: 11px;
  font-weight: 500;
  white-space: nowrap;
}
.stack-field dd {
  display: grid;
  gap: 3px;
  margin-top: 3px;
  font-size: 14px;
  line-height: 1.35;
}
.stack-old,
.stack-new {
  display: grid;
  grid-template-columns: 2.4rem minmax(0, 1fr);
  align-items: baseline;
  column-gap: 6px;
  overflow-wrap: anywhere;
}
.stack-side {
  font-size: 11px;
  color: var(--fg-subtle);
}
.stack-old {
  color: var(--fg-muted);
}
/* Replaced: struck with a thin line, so it still reads. */
.stack-old s {
  text-decoration-thickness: 1px;
  text-decoration-color: var(--fg-subtle);
}
.stack-new {
  border-radius: 4px;
  padding: 3px 6px;
  margin-inline: -6px;
}
.stack-new b {
  font-weight: 600;
}
/* Whose value it is, in the value's own ink (it sits on the value's fill). */
.stack-new .stack-side {
  color: inherit;
  opacity: 0.85;
}
.stack-new i,
.stack-old i {
  color: var(--fg-subtle);
}
/* The table's colours, kind by kind. */
.is-ai .stack-new,
.is-ai .stack-tag {
  background: var(--cell-ai);
  box-shadow: inset 0 0 0 1px var(--cell-ai-edge);
  color: var(--cell-ai-text);
}
.is-person .stack-new,
.is-person .stack-tag {
  background: var(--cell-person);
  box-shadow: inset 0 0 0 1px var(--cell-person-edge);
  color: var(--cell-person-text);
}
.is-doubtful .stack-new,
.is-doubtful .stack-tag,
.is-warned .stack-new,
.is-warned .stack-tag {
  background: var(--color-amber-50);
  color: var(--color-amber-900);
  outline: 2px dashed var(--color-amber-500);
  outline-offset: -2px;
}
.is-warned .stack-new,
.is-warned .stack-tag {
  outline-style: solid;
}
.is-unreadable .stack-new,
.is-unreadable .stack-tag {
  background: repeating-linear-gradient(135deg, var(--color-red-50) 0 5px, color-mix(in srgb, var(--color-red-100) 60%, var(--surface)) 5px 10px);
  color: var(--color-red-900);
  outline: 2px dotted var(--color-red-600);
  outline-offset: -2px;
}
.is-kept .stack-new,
.is-kept .stack-tag {
  background: var(--cell-sheet-edit);
  color: var(--cell-sheet-edit-text);
  outline: 2px solid var(--cell-sheet-edit-edge);
  outline-offset: -2px;
}
.is-reverted .stack-tag {
  outline: 1px dashed var(--color-emerald-400);
  color: var(--fg-muted);
}
.stack-why {
  font-size: 12px;
  color: var(--fg-muted);
}
/* What a tap does, on a row of its own a finger can hit (44 px with the cell above it). */
.stack-go {
  display: flex;
  align-items: center;
  gap: 6px;
  min-height: 28px;
  font-size: 13px;
  font-weight: 500;
  color: var(--brand-text);
}
button.stack-field {
  min-height: 44px;
  cursor: pointer;
}
button.stack-field:active {
  background: var(--surface-sunken);
}
</style>
