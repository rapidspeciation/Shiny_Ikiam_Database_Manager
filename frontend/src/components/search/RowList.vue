<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { ChevronRight } from 'lucide-vue-next'
import { markParts, type RowListItem } from '../../lib/rowList'

/**
 * A sheet's rows on a phone (lib/rowList.ts): one line per row, as tall as a finger needs, with its ID, species and
 * a few facts; a tap opens the row. In the Buscador's results the searched text is marked, with the cell where it is.
 * Long lists are drawn a part at a time (`step` rows, more on request).
 */
const props = withDefaults(
  defineProps<{
    items: RowListItem[]
    /** The text searched, marked where a line shows it. */
    mark?: string
    /** Rows drawn at a time. */
    step?: number
    /** The row open now (its line stays marked). */
    current?: string | null
  }>(),
  { mark: '', step: 60, current: null },
)
const emit = defineEmits<{ open: [id: string] }>()

const shown = ref(props.step)
// Another list (a sheet, a search, a filter): from its top again.
watch(
  () => [props.items.length, props.items[0]?.id],
  () => (shown.value = props.step),
)
const visible = computed(() => props.items.slice(0, shown.value))
const left = computed(() => Math.max(0, props.items.length - shown.value))
</script>

<template>
  <ul class="row-list" data-row-list>
    <li v-for="item in visible" :key="item.id">
      <button
        type="button"
        class="row-item"
        :class="{ 'is-current': item.id === current, 'is-new': item.isNew }"
        :data-row="item.row || undefined"
        @click="emit('open', item.id)"
      >
        <span class="min-w-0 flex-1">
          <span class="flex items-baseline gap-2">
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
            <span v-if="item.species" class="row-species"
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
    <li v-if="left">
      <button type="button" class="btn min-h-11 w-full" data-more @click="shown += step">
        {{ $tn(Math.min(step, left), 'Mostrar {n} fila más', 'Mostrar {n} filas más') }}
        <span class="font-normal text-fg-subtle">· {{ $tn(left, 'queda {n}', 'quedan {n}') }}</span>
      </button>
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
