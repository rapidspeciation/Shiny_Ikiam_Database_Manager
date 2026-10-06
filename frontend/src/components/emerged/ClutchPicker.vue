<script setup lang="ts">
import { computed, ref } from 'vue'
import { Search, X } from 'lucide-vue-next'
import SexBadge from '../SexBadge.vue'
import { isBlank } from '../../lib/cells'
import { clutchState, countCell, hasClutch, parentsOf, readCount, stageDurations, totalOf, undatedTail, type ClutchState, type CountField } from '../../lib/clutches'
import { formatSerial } from '../../lib/dates'
import type { CellValue, TableRow } from '../../lib/types'
import { usePending } from '../../stores/pending'
import { t } from '../../lib/i18n'

/**
 * Which clutch the butterflies emerged from: the clutches emerging and those
 * with pupae first (the oldest pupae first: they emerge about 8 days after
 * pupating, the species' own days as Clutches gives them), then the other clutches still going; any clutch by number,
 * species or parent. Each shows its pupae and adults (the sums of Clutches),
 * the butterflies already in Insectary_data and the cards not saved yet.
 */
const props = defineProps<{
  rows: TableRow[]
  /** The sheet's sum formulas by row (useClutchDay), to read the counts as Clutches shows them. */
  sums: Record<string, Record<string, string>>
  /** Butterflies already in Insectary_data by clutch, with the last emergence day. */
  registered: Map<string, { n: number; last: number | null }>
  /** Unsaved cards by clutch. */
  drafts: Map<string, number>
  today: number
  current: string
}>()
const emit = defineEmits<{ pick: [clutch: string]; close: [] }>()
const pending = usePending()
const query = ref('')

/** Each species' days per stage, from every clutch in the sheet (pupa to adult: 8 for most, 10 for Melinaea). */
const durations = computed(() =>
  stageDurations(
    props.rows.map(r => ({
      species: r.values.SPECIES ?? null,
      laid: r.values['DATE LAID'] ?? null,
      hatch: r.values['HATCHING DATE'] ?? null,
      pupa: r.values['PUPA DATE'] ?? null,
      emerge: r.values['EMERGENCE DATE'] ?? null,
    })),
  ),
)

interface Item {
  row: TableRow
  number: string
  species: string
  state: ClutchState
  pupae: number | null
  adults: number | null
  pupaDate: number | null
  generation: string
  parents: { female: string; male: string } | null
  search: string
}
const value = (row: TableRow, field: string) => pending.value(row, field)
const total = (row: TableRow, field: CountField) => {
  const c = readCount(countCell(row, field, pending.value, pending.isDirty(row.id, field), props.sums[row.id]))
  return c.terms.length ? totalOf(c.terms) : null
}
const items = computed<Item[]>(() => {
  void pending.edited
  const all = props.rows.filter(r => r.observed)
  const out: Item[] = []
  all.forEach((row, i) => {
    const get = (f: string) => value(row, f)
    if (!hasClutch(get)) return
    const counts = (f: CountField) => readCount(countCell(row, f, pending.value, pending.isDirty(row.id, f), props.sums[row.id]))
    const state = clutchState(get, counts, props.today, { undated: undatedTail(i, all.length) })
    const number = String(row.values['CLUTCH NUMBER'] ?? '').trim()
    if (!number) return
    const species = isBlank(get('SPECIES')) ? '' : String(get('SPECIES'))
    const parents = parentsOf(get('NOTES'))
    const pupa = get('PUPA DATE')
    const generation = isBlank(get('Generation')) || get('Generation') === 'NA' ? '' : String(get('Generation'))
    out.push({
      row,
      number,
      species,
      state,
      pupae: total(row, 'NUMBER OF PUPA'),
      adults: total(row, 'NUMBER OF ADULTS'),
      pupaDate: typeof pupa === 'number' ? pupa : null,
      generation,
      parents: parents ? { female: parents.female, male: parents.male } : null,
      search: `${number} ${species} ${parents ? `${parents.female} ${parents.male}` : ''}`.toLowerCase(),
    })
  })
  return out
})
const sortKey = (v: CellValue | number | null) => (typeof v === 'number' ? v : Infinity)
/** Emerging, then with pupae, then the rest still going; a clutch with cards not saved always shows. */
const groups = computed(() => {
  const going = items.value.filter(i => !i.state.ended || props.drafts.has(i.number) || i.number === props.current)
  const emerging = going.filter(i => i.state.stage === 'adult').sort((a, b) => sortKey(value(a.row, 'EMERGENCE DATE')) - sortKey(value(b.row, 'EMERGENCE DATE')))
  const pupae = going.filter(i => i.state.stage === 'pupa').sort((a, b) => sortKey(a.pupaDate) - sortKey(b.pupaDate))
  const rest = going
    .filter(i => i.state.stage !== 'adult' && i.state.stage !== 'pupa')
    .sort((a, b) => sortKey(a.state.start) - sortKey(b.state.start))
  return [
    { key: 'emerging', title: t('Emergiendo'), items: emerging },
    { key: 'pupae', title: t('Con pupas'), items: pupae },
    { key: 'rest', title: t('Huevos y larvas en curso'), items: rest },
  ].filter(g => g.items.length)
})
const found = computed(() => {
  const q = query.value.trim().toLowerCase()
  if (!q) return []
  const starts = items.value.filter(i => i.number.toLowerCase().startsWith(q))
  const contains = items.value.filter(i => !i.number.toLowerCase().startsWith(q) && i.search.includes(q))
  return [...starts.reverse(), ...contains.reverse()].slice(0, 40)
})
const due = (i: Item) => (i.pupaDate !== null && i.state.stage === 'pupa' ? i.pupaDate + durations.value.of(i.species).pupa : null)
const dueText = (i: Item) => {
  const d = due(i)
  return d === null ? '' : formatSerial(d)
}
</script>

<template>
  <section class="px-3 pt-3 pb-4">
    <div class="flex items-center gap-2">
      <div class="relative min-w-0 flex-1">
        <Search :size="18" class="pointer-events-none absolute top-1/2 left-3 -translate-y-1/2 text-stone-400" />
        <input
          v-model="query"
          class="h-12 w-full rounded-xl border border-stone-300 bg-white pr-11 pl-9 text-base placeholder:text-stone-400 [&::-webkit-search-cancel-button]:appearance-none focus:border-brand-600 focus:ring-2 focus:ring-brand-100 focus:outline-none"
          type="search"
          inputmode="search"
          autocomplete="off"
          enterkeyhint="search"
          :placeholder="$t('Buscar clutch: número, especie o padre')"
          :aria-label="$t('Buscar clutch: número, especie o padre')"
        />
        <button v-if="query" class="absolute top-1/2 right-0.5 grid h-11 w-11 -translate-y-1/2 place-items-center text-stone-500" :aria-label="$t('Borrar búsqueda')" @click="query = ''">
          <X :size="18" />
        </button>
      </div>
      <button v-if="current" class="btn h-12 shrink-0 px-3" @click="emit('close')">{{ $t('Cancelar') }}</button>
    </div>

    <template v-for="group in query ? [{ key: 'found', title: '', items: found }] : groups" :key="group.key">
      <h2 v-if="group.title" class="mt-4 mb-1.5 text-xs font-semibold tracking-wide text-stone-500 uppercase">{{ group.title }}</h2>
      <p v-if="query && !group.items.length" class="p-6 text-center text-sm text-stone-500">{{ $t('Ningún clutch con «{q}»', { q: query }) }}</p>
      <ul class="mt-2 grid grid-cols-[repeat(auto-fill,minmax(17rem,1fr))] gap-2">
        <li v-for="item in group.items" :key="item.row.id">
          <button
            class="block w-full rounded-xl border bg-white px-3 py-2 text-left shadow-sm active:bg-stone-50"
            :class="item.number === current ? 'border-brand-600 ring-2 ring-brand-100' : 'border-stone-200'"
            @click="emit('pick', item.number)"
          >
            <span class="flex flex-wrap items-center gap-1.5">
              <span class="text-xl font-semibold tabular-nums">{{ item.number }}</span>
              <span v-if="item.generation" class="rounded bg-violet-100 px-1.5 text-xs font-medium text-violet-800">{{ item.generation }}</span>
              <span v-if="drafts.get(item.number)" class="ml-auto rounded-full bg-amber-50 px-2 py-0.5 text-xs font-medium text-amber-900 ring-1 ring-amber-300">
                {{ $tn(drafts.get(item.number)!, '{n} sin guardar', '{n} sin guardar') }}
              </span>
            </span>
            <span class="mt-0.5 flex min-w-0 items-center gap-1.5 text-sm">
              <span class="min-w-0 truncate">{{ item.species || $t('sin especie') }}</span>
              <span v-if="item.parents" class="ml-auto flex shrink-0 items-center gap-1 text-xs font-medium tabular-nums text-stone-700">
                <SexBadge sex="female" />{{ item.parents.female }} <SexBadge sex="male" />{{ item.parents.male }}
              </span>
            </span>
            <span class="mt-1.5 grid grid-cols-3 gap-1">
              <span class="rounded-md bg-stone-50 px-1.5 py-1">
                <span class="block text-[11px] leading-tight text-stone-500">{{ $t('Pupas') }}</span>
                <span class="block text-lg leading-tight font-semibold tabular-nums">{{ item.pupae ?? '—' }}</span>
              </span>
              <span class="rounded-md bg-stone-50 px-1.5 py-1">
                <span class="block text-[11px] leading-tight text-stone-500">{{ $t('Adultos') }}</span>
                <span class="block text-lg leading-tight font-semibold tabular-nums">{{ item.adults ?? '—' }}</span>
              </span>
              <span class="rounded-md bg-stone-50 px-1.5 py-1" :title="$t('Mariposas de este clutch en Insectary_data')">
                <span class="block text-[11px] leading-tight text-stone-500">{{ $t('Registrados') }}</span>
                <span class="block text-lg leading-tight font-semibold tabular-nums">{{ registered.get(item.number)?.n ?? 0 }}</span>
              </span>
            </span>
            <span class="mt-1 block truncate text-xs text-stone-500">
              <template v-if="due(item) !== null">
                {{ $t('Pupas desde {date}', { date: formatSerial(item.pupaDate!) }) }} ·
                <span :class="due(item)! <= today ? 'font-medium text-brand-800' : ''">{{ $t('emergen hacia {date}', { date: dueText(item) }) }}</span>
              </template>
              <template v-else-if="registered.get(item.number)?.last">
                {{ $t('Último emergido {date}', { date: formatSerial(registered.get(item.number)!.last!) }) }}
              </template>
            </span>
          </button>
        </li>
      </ul>
    </template>
    <p v-if="!query && !groups.length" class="p-6 text-center text-sm text-stone-500">{{ $t('Ningún clutch en curso: busca el clutch por su número.') }}</p>
  </section>
</template>
