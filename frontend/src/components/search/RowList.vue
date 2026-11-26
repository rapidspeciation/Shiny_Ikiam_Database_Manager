<script setup lang="ts">
import { ChevronRight } from 'lucide-vue-next'
import { markParts, type RowListItem } from '../../lib/rowList'

/**
 * A sheet's rows on a phone (lib/rowList.ts): one line per row, as tall as a finger needs, with its ID, species and
 * a few facts; a tap opens the row. In the Buscador's results the searched text is marked, with the cell where it is.
 * The one who shows the list gives it the lines to draw: a long sheet a part at a time, the rest on request (`left`,
 * `more`), so a sheet of thousands of rows builds only the lines on screen.
 */
withDefaults(
  defineProps<{
    items: RowListItem[]
    /** The text searched, marked where a line shows it. */
    mark?: string
    /** The row open now (its line stays marked). */
    current?: string | null
    /** Rows not drawn yet: a button after the lines asks for more. */
    left?: number
    /** That button's words. */
    moreLabel?: string
    /** More are being read. */
    busy?: boolean
  }>(),
  { mark: '', current: null, left: 0, moreLabel: '', busy: false },
)
const emit = defineEmits<{ open: [id: string]; more: [] }>()
/** The searched text is in the species: its whole name shows (never cut where the match is). */
const inSpecies = (item: RowListItem, mark: string) => !!mark.trim() && item.species.toLowerCase().includes(mark.trim().toLowerCase())
</script>

<template>
  <ul class="row-list" data-row-list>
    <li v-for="item in items" :key="item.id">
      <button
        type="button"
        class="row-item"
        :class="{ 'is-current': item.id === current, 'is-new': item.isNew, 'is-dirty': item.dirty }"
        :data-row="item.row || undefined"
        @click="emit('open', item.id)"
      >
        <span class="min-w-0 flex-1">
          <span class="flex items-baseline gap-x-2" :class="{ 'flex-wrap': inSpecies(item, mark) }">
            <span class="row-title">
              <template v-if="item.title"
                ><template v-for="(p, i) in markParts(item.title, mark)" :key="i"
                  ><mark v-if="p.hit">{{ p.text }}</mark
                  ><template v-else>{{ p.text }}</template></template
                ></template
              >
              <template v-else>{{ $t('Sin ID') }}</template>
            </span>
            <span v-if="item.isNew" class="row-new">{{ $t('nueva') }}</span>
            <span v-if="item.dirty" class="row-new">{{ $t('sin guardar') }}</span>
            <span v-if="item.species" class="row-species" :class="{ 'is-whole': inSpecies(item, mark) }"
              ><template v-for="(p, i) in markParts(item.species, mark)" :key="i"
                ><mark v-if="p.hit">{{ p.text }}</mark
                ><template v-else>{{ p.text }}</template></template
              ></span
            >
          </span>
          <span v-if="item.facts.length" class="row-facts">{{ item.facts.join(' · ') }}</span>
          <span v-if="item.match" class="row-match" data-match>
            <span class="row-match-field">{{ item.match.field }}</span>
            <template v-for="(p, i) in markParts(item.match.text, mark)" :key="i"
              ><mark v-if="p.hit">{{ p.text }}</mark
              ><template v-else>{{ p.text }}</template></template
            >
          </span>
        </span>
        <span v-if="item.row" class="row-number">{{ $t('fila {row}', { row: item.row }) }}</span>
        <ChevronRight :size="18" class="flex-none text-fg-subtle" aria-hidden="true" />
      </button>
    </li>
    <li v-if="left > 0">
      <button type="button" class="btn min-h-11 w-full" :disabled="busy" data-more @click="emit('more')">{{ moreLabel }}</button>
    </li>
  </ul>
</template>

<style scoped>
.row-list {
  display: grid;
  gap: 6px;
  padding: 8px;
}
/* One row of the sheet: a box a finger can hit (56 px at least), the brand's edge once it is the one open. */
.row-item {
  display: flex;
  width: 100%;
  min-height: 56px;
  align-items: center;
  gap: 8px;
  border: 1px solid var(--border-strong);
  border-radius: 8px;
  background: var(--surface);
  padding: 8px 6px 8px 12px;
  text-align: left;
  color: var(--fg);
  transition:
    border-color 120ms ease,
    transform 120ms ease;
}
.row-item:active {
  transform: scale(0.99);
  border-color: var(--brand);
  background: var(--select);
}
@media (hover: hover) {
  .row-item:hover {
    border-color: var(--brand);
  }
}
.row-item.is-current {
  border-color: var(--brand);
  box-shadow: inset 3px 0 0 var(--brand);
}
.row-item.is-new {
  border-color: var(--pending-edge);
  background: var(--cell-pending);
}
.row-title {
  flex: none;
  font-size: 16px;
  font-weight: 600;
  line-height: 1.25;
}
.row-species {
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  font-size: 14px;
  font-style: italic;
  color: var(--fg);
}
/* With the searched text in it: the whole name, on as many lines as it takes. */
.row-species.is-whole {
  overflow: visible;
  white-space: normal;
  overflow-wrap: anywhere;
}
.row-item.is-dirty {
  border-color: var(--pending-edge);
  box-shadow: inset 3px 0 0 var(--pending-edge);
}
.row-new {
  flex: none;
  border-radius: 3px;
  padding: 0 5px;
  font-size: 12px;
  font-weight: 500;
  color: var(--pending-text);
  box-shadow: inset 0 0 0 1px var(--pending-edge);
}
.row-facts,
.row-match {
  display: block;
  margin-top: 1px;
  font-size: 13px;
  line-height: 1.35;
  color: var(--fg-muted);
  overflow-wrap: anywhere;
}
.row-match-field {
  margin-right: 5px;
  font-size: 12px;
  font-weight: 600;
}
.row-number {
  flex: none;
  font-size: 12px;
  font-variant-numeric: tabular-nums;
  color: var(--fg-subtle);
}
mark {
  border-radius: 2px;
  background: var(--cell-match-strong);
  padding: 0 1px;
  color: inherit;
}
@media (prefers-reduced-motion: reduce) {
  .row-item {
    transition: none;
  }
  .row-item:active {
    transform: none;
  }
}
</style>
