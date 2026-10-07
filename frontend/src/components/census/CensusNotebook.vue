<script setup lang="ts">
import { computed, onBeforeUnmount, ref, watch } from 'vue'
import { ArrowLeft, Copy, Loader2 } from 'lucide-vue-next'
import DateField from '../DateField.vue'
import PaperOrder from '../PaperOrder.vue'
import SexBadge from '../SexBadge.vue'
import { useCensus } from '../../composables/useCensus'
import { api } from '../../lib/api'
import { noteDay } from '../../lib/clutches'
import { dayLabel, formatSerial, isoToSerial, serialFromIso, serialToIso, todayIso } from '../../lib/dates'
import { readOrder, type RecordedOrder } from '../../lib/deathsCart'
import { errorText, notify } from '../../lib/notice'
import { filterPaper, lineText, notebookCopy, sortPaper, type NotebookLine, type NotebookUpdate } from '../../lib/paperNotebook'
import { persistentRef } from '../../lib/persist'
import { useLive } from '../../stores/live'
import { t } from '../../lib/i18n'

/**
 * «Actualizar el cuaderno»: every butterfly of the species censused on a day
 * (several at once: a day of salapia, lysimnia and polymnia) in the insectary
 * that day, in the paper notebook's order (its rows) or by emergence, each
 * with what to do on paper: ☺ seen, ✗ disappeared (to write), ▬ already dead
 * in the database (to highlight). After a census (its day and the species
 * censused that day), or on demand (`free`: any day and species).
 */
const props = defineProps<{ day: string; species?: string[]; free?: boolean }>()
const emit = defineEmits<{ leave: [] }>()
const census = useCensus()
const live = useLive()

const day = ref(props.day)
/** The species asked for (binomials); empty: those censused that day. */
const chosen = ref<string[]>(props.species ?? [])
const data = ref<NotebookUpdate | null>(null)
const loading = ref(false)
const failed = ref('')
const storedOrder = persistentRef<RecordedOrder>('census:notebook-order', { by: 'row', desc: false }, { lasting: true })
const order = computed<RecordedOrder>({
  get: () => {
    const o = readOrder(storedOrder.value)
    return o.by === 'emergence' ? o : { by: 'row' as const, desc: o.desc }
  },
  set: v => (storedOrder.value = v),
})
const only = persistentRef('census:notebook-only', true, { lasting: true })
/** How far back the dead before the day are listed (by emergence): the notebook's pages in use. */
const span = persistentRef<'1' | '3' | '6' | 'all'>('census:notebook-span', '3', { lasting: true })
const serial = computed(() => isoToSerial(day.value))
const since = computed(() => {
  if (span.value === 'all') return ''
  const d = new Date(`${day.value}T00:00:00Z`)
  d.setUTCMonth(d.getUTCMonth() - Number(span.value))
  return d.toISOString().slice(0, 10)
})
const dayError = computed(() =>
  serialFromIso(day.value) === null ? t('Fecha no válida: el año debe estar entre 1990 y 2099') : '',
)

let asked = 0
async function load() {
  if (dayError.value) return
  const ask = ++asked
  loading.value = true
  try {
    const q = new URLSearchParams({ day: day.value, species: chosen.value.join(','), since: since.value })
    const out = await api<NotebookUpdate>(`census/notebook?${q}`)
    if (ask !== asked) return
    data.value = out
    failed.value = ''
  } catch (e) {
    if (ask === asked) failed.value = errorText(e)
  } finally {
    if (ask === asked) loading.value = false
  }
}
watch([day, chosen, since], () => void load(), { immediate: true })
// A census finished or reopened, a save, an entry kept or saved: the list follows a moment later.
let timer: ReturnType<typeof setTimeout> | undefined
watch([() => live.census, () => live.revision, () => live.counts.staged], () => {
  clearTimeout(timer)
  timer = setTimeout(load, 800)
})
onBeforeUnmount(() => clearTimeout(timer))

/** The species to choose from: those of the list, those censused that day, those alive now. */
const speciesOptions = computed(() => {
  const all = [
    ...(data.value?.species ?? []),
    ...(data.value?.censused ?? []),
    ...(props.free ? (census.overview.value?.species ?? []).map(s => s.species) : []),
  ]
  return [...new Set(all)]
})
const shownSpecies = computed(() => data.value?.species ?? chosen.value)
function toggle(species: string) {
  const now = shownSpecies.value
  // At least one: with none, the list would be those censused that day again.
  if (now.length === 1 && now[0] === species) return
  chosen.value = now.includes(species) ? now.filter(s => s !== species) : [...now, species]
}

const lines = computed(() => data.value?.lines ?? [])
const shown = computed(() => sortPaper(filterPaper(lines.value, only.value), order.value))
const counts = computed(() => ({
  write: lines.value.filter(l => l.todo === 'write').length,
  highlight: lines.value.filter(l => l.todo === 'highlight').length,
  seen: lines.value.filter(l => l.census?.status === 'seen').length,
  all: lines.value.length,
}))
const several = computed(() => shownSpecies.value.length > 1)
const waiting = (l: NotebookLine) =>
  l.todo === 'write' ? !!l.census?.waiting || !!l.death?.staged : l.todo === 'highlight' ? !!l.death?.staged : !!l.staged
const look = (l: NotebookLine) =>
  l.todo === 'write'
    ? 'bg-red-50 text-red-900'
    : l.todo === 'highlight'
      ? 'bg-yellow-200 text-stone-900'
      : l.census?.status === 'seen'
        ? 'bg-white text-emerald-900'
        : 'bg-white text-stone-500'
function details(l: NotebookLine) {
  return [
    several.value ? l.species.split(' ').slice(0, 2).join(' ') : '',
    l.entered === null
      ? ''
      : l.wild
        ? t('Capturada {date}', { date: formatSerial(l.entered) })
        : t('Emergió {date}', { date: formatSerial(l.entered) }),
  ]
    .filter(Boolean)
    .join(' · ')
}
async function copy() {
  const title = `${shownSpecies.value.join(', ')} · ${noteDay(serial.value)}`
  try {
    await navigator.clipboard.writeText(notebookCopy(shown.value, serial.value, title))
    notify(t('Copiado'), 'success')
  } catch {
    notify(t('No se pudo copiar'), 'error')
  }
}
const quickDays = computed(() => [
  { iso: todayIso(), name: t('Hoy') },
  { iso: serialToIso(isoToSerial(todayIso()) - 1), name: t('Ayer') },
])
const chip = (on: boolean) =>
  on ? 'border-stone-800 bg-stone-800 text-white' : 'border-stone-300 bg-white text-stone-800 active:bg-stone-100'
</script>

<template>
  <div :class="free ? 'h-full overflow-y-auto' : ''" data-census-notebook>
    <header v-if="free" class="sticky top-0 z-10 flex items-center gap-2 border-b border-stone-200 bg-white px-3 py-2 sm:px-4">
      <button
        class="grid h-11 w-11 shrink-0 place-items-center rounded-lg text-stone-600 active:bg-stone-100"
        :aria-label="$t('Volver a los censos')"
        @click="emit('leave')"
      >
        <ArrowLeft :size="22" />
      </button>
      <p class="min-w-0 flex-1 truncate text-base font-semibold">{{ $t('Actualizar el cuaderno') }}</p>
    </header>

    <div :class="free ? 'mx-auto max-w-3xl space-y-3 px-3 pt-3 pb-10 sm:px-4' : 'space-y-3'">
      <p class="text-sm text-stone-600">
        {{
          $t(
            'Cada mariposa de estas especies en el insectario ese día, en el orden del cuaderno: ☺ vista en el censo; ✗ desaparecida (escribirlo); ▬ ya muerta en la base de datos (resaltarla).',
          )
        }}
      </p>

      <template v-if="free">
        <span class="field-label">{{ $t('Día del censo') }}</span>
        <div class="grid max-w-md grid-cols-[1fr_1fr_minmax(9rem,1.4fr)] gap-2">
          <button
            v-for="d in quickDays"
            :key="d.iso"
            type="button"
            class="h-11 rounded-lg border text-base font-medium"
            :class="chip(day === d.iso)"
            :aria-pressed="day === d.iso"
            @click="day = d.iso"
          >
            {{ d.name }}
          </button>
          <DateField v-model="day" class="field-input h-11 text-base" />
        </div>
        <p v-if="dayError" class="text-sm text-red-700">{{ dayError }}</p>
        <p v-else class="text-sm text-stone-600">{{ dayLabel(day) }}</p>
      </template>

      <!-- The species: those censused that day first; several at once. -->
      <div>
        <span class="field-label">{{ $t('Especies') }}</span>
        <p v-if="data && !speciesOptions.length" class="text-sm text-stone-500">
          {{ $t('Ningún censo terminado ese día: elige especies.') }}
        </p>
        <div class="flex flex-wrap gap-1.5">
          <button
            v-for="s in speciesOptions"
            :key="s"
            type="button"
            class="min-h-10 rounded-full border px-3 text-sm italic"
            :class="chip(shownSpecies.includes(s))"
            :aria-pressed="shownSpecies.includes(s)"
            @click="toggle(s)"
          >
            {{ s }}
            <span v-if="data?.censused.includes(s)" class="not-italic opacity-70"> · {{ $t('censada') }}</span>
          </button>
        </div>
      </div>

      <div class="flex flex-wrap items-center gap-2">
        <div
          class="inline-flex overflow-hidden rounded-lg border border-stone-300 text-sm"
          role="group"
          :aria-label="$t('Mostrar')"
        >
          <button
            type="button"
            class="h-10 px-2.5"
            :class="only ? 'bg-brand-700 text-white' : 'bg-white'"
            :aria-pressed="only"
            @click="only = true"
          >
            {{ $t('Solo lo que hay que marcar ({n})', { n: counts.write + counts.highlight }) }}
          </button>
          <button
            type="button"
            class="h-10 border-l border-stone-300 px-2.5"
            :class="!only ? 'bg-brand-700 text-white' : 'bg-white'"
            :aria-pressed="!only"
            @click="only = false"
          >
            {{ $t('Todas ({n})', { n: counts.all }) }}
          </button>
        </div>
        <PaperOrder
          v-model="order"
          :options="[
            { by: 'row', label: 'Insectary ID' },
            { by: 'emergence', label: $t('Emergencia') },
          ]"
        />
        <button type="button" class="btn h-10" :disabled="!shown.length" @click="copy">
          <Copy :size="15" /> {{ $t('Copiar como texto') }}
        </button>
      </div>

      <div
        class="flex flex-wrap items-center gap-1.5 text-sm text-stone-600"
        role="group"
        :aria-label="$t('Muertas antes del día')"
      >
        <span>{{ $t('Muertas antes, emergidas en:') }}</span>
        <button
          v-for="s in [
            { key: '1', label: $t('1 mes') },
            { key: '3', label: $t('3 meses') },
            { key: '6', label: $t('6 meses') },
            { key: 'all', label: $t('todo') },
          ] as const"
          :key="s.key"
          type="button"
          class="min-h-9 rounded-full border px-2.5"
          :class="chip(span === s.key)"
          :aria-pressed="span === s.key"
          @click="span = s.key"
        >
          {{ s.label }}
        </button>
      </div>

      <p v-if="data" class="text-sm font-medium text-stone-700">
        {{
          $t('✗ {write} por escribir · ▬ {highlight} por resaltar · ☺ {seen} vistas', {
            write: counts.write,
            highlight: counts.highlight,
            seen: counts.seen,
          })
        }}
        <Loader2 v-if="loading" :size="14" class="ml-1 inline animate-spin text-stone-400" />
      </p>
      <p v-if="failed" class="text-sm text-red-700">{{ failed }}</p>
      <p v-else-if="!data" class="text-sm text-stone-500">
        <Loader2 :size="14" class="inline animate-spin" /> {{ $t('Cargando…') }}
      </p>
      <p v-else-if="!shown.length" class="text-sm text-stone-500">
        {{ only && lines.length ? $t('Nada que marcar en el cuaderno.') : $t('Ninguna mariposa de estas especies ese día.') }}
      </p>

      <ol v-if="shown.length" class="divide-y divide-stone-200 overflow-hidden rounded-xl border border-stone-200">
        <li
          v-for="l in shown"
          :key="l.recordId"
          class="flex min-h-12 items-center gap-3 px-3 py-1.5"
          :class="[look(l), waiting(l) ? 'border-l-4 border-l-amber-500' : '']"
          :data-line="l.id"
        >
          <span class="w-14 shrink-0 font-semibold text-stone-900 tabular-nums">{{ l.id }}</span>
          <span class="w-6 shrink-0 text-center text-lg leading-none" aria-hidden="true">{{ lineText(l, serial).mark }}</span>
          <span class="min-w-0 flex-1">
            <span class="block text-sm">
              {{ lineText(l, serial).text }}
              <span v-if="waiting(l)" class="text-xs font-medium text-amber-800"> · {{ $t('aún no en Google Sheets') }}</span>
            </span>
            <span class="flex flex-wrap items-center gap-x-1 text-xs text-stone-500">
              <SexBadge :sex="l.sex" />
              <span>{{ details(l) }}</span>
            </span>
          </span>
        </li>
      </ol>
    </div>
  </div>
</template>
