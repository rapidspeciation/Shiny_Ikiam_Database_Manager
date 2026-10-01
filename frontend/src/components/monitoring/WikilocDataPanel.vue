<script setup lang="ts">
import ChoiceField from '../ChoiceField.vue'
import { computed, onMounted, ref } from 'vue'
import { Download, ExternalLink, MapPinned, RefreshCw } from 'lucide-vue-next'
import { api } from '../../lib/api'
import { formatSerial, isoToSerial } from '../../lib/dates'
import { t, tm, type Msg } from '../../lib/i18n'
import { formatMinutes } from '../../lib/monitoring'
import { errorText, notify } from '../../lib/notice'

/**
 * Monitoreo → Wikiloc: what the app holds from Wikiloc (walks, points,
 * photos, per collector, and the monitoring days without a walk), the
 * downloads for the team (monitoring rows, points and GPS lines), and the
 * corrections the points suggest for the sheet (server/suggestions/
 * wikiloc-transects.mjs): the transect section from each point's place, the
 * time of its note, and the rows and points that have no partner. Read-only:
 * the sheet is corrected in Tablas.
 */
type Certainty = 'certain' | 'likely' | 'check'
interface WalkRef {
  id: string
  date: string
  collector: string | null
  name: string
  url: string | null
  stored: boolean
}
interface Evidence {
  walk: WalkRef
  point?: number
  text?: string
  photos?: string[]
  pairing?: string
  distance?: number
  margin?: number
  section?: number
}
interface Suggestion {
  sheet: string
  row: number
  recordId: string
  field: string
  current: string | number | null
  suggested: string | number | null
  certainty: Certainty
  reason: string
  reasonMsg?: Msg
  evidence: Evidence
}
interface Discrepancy {
  kind: 'far' | 'date' | 'point-without-row' | 'row-without-point'
  row: number | null
  date: number | null
  collector: string | null
  species: string | null
  sex: string | null
  mark: string | null
  minutes: number | null
  section: string | null
  reason: string
  reasonMsg?: Msg
  evidence: Evidence
}
interface CollectorCount {
  collector: string | null
  stored: number
  waiting: number
  first: string
  last: string
  points: number
  linked: number
  daysWithoutWalk: number
}
interface WalkInfo extends WalkRef {
  points: number
  linked: number
  timed: boolean
}
interface Data {
  counts: {
    points: number
    linked: number
    agree: number
    filled: number
    differ: number
    far: number
    monitoringRows: number
    blankSections: number
    byCertainty: Record<Certainty, number>
  }
  suggestions: Suggestion[]
  discrepancies: Discrepancy[]
  daysWithoutWalk: { date: number; collector: string | null; rows: number; blankSections: number }[]
  inventory: {
    stored: number
    waiting: number
    first: string | null
    last: string | null
    points: number
    linked: number
    photos: number
    photoBytes: number
    timed: number
    collectors: CollectorCount[]
    walks: WalkInfo[]
  }
}

const data = ref<Data | null>(null)
const loading = ref(false)
async function load() {
  loading.value = true
  try {
    data.value = await api<Data>('monitoring/wikiloc-data')
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    loading.value = false
  }
}
onMounted(load)

// ------------------------------------------------------------ labels
const day = (iso: string | null) => (iso ? formatSerial(isoToSerial(iso)) : '—')
const initials = (c: string | null) => (c || '').split(' - ')[0].trim()
// Spanish; t() where shown.
const CERTAINTY: Record<Certainty, string> = { certain: 'Segura', likely: 'Probable', check: 'Verificar' }
const CERTAINTY_HINT: Record<Certainty, string> = {
  certain: 'El punto está bien dentro de una sección y emparejado con su fila por marca, por una persona o por hora, especie y sexo',
  likely: 'El punto está cerca de un límite o lejos del sendero, o su emparejamiento no es seguro',
  check: 'Mirar la foto y la nota antes de cambiar la hoja',
}
const CERTAINTY_CLASS: Record<Certainty, string> = {
  certain: 'bg-brand-50 text-brand-800',
  likely: 'bg-sky-50 text-sky-800',
  check: 'bg-amber-100 text-amber-900',
}
const KIND: Record<Discrepancy['kind'], string> = {
  far: 'Puntos lejos del sendero',
  date: 'Marca en una fila de otro día',
  'point-without-row': 'Puntos de Wikiloc sin fila en la hoja',
  'row-without-point': 'Filas sin punto en el recorrido de ese día',
}
/** A cell as the sheet shows it: times as h:mm, blank as —. */
function cell(field: string, value: string | number | null) {
  if (value === null || value === '') return '—'
  if (field === 'Collection_time' && typeof value === 'number') return formatMinutes(Math.round((value % 1) * 1440))
  return String(value)
}
const reasonOf = (x: { reason: string; reasonMsg?: Msg }) => (x.reasonMsg ? tm(x.reasonMsg) : x.reason)
const mapLink = (iso: string) => `#/monitoreo?vista=mapa&fechas=${iso}`

// ------------------------------------------------------------ filters
const certainty = ref<Certainty | ''>('')
const field = ref('')
const who = ref('')
const fields = computed(() => [...new Set((data.value?.suggestions || []).map(s => s.field))])
const collectors = computed(() =>
  [...new Set((data.value?.suggestions || []).map(s => initials(s.evidence.walk.collector)))].filter(Boolean).sort(),
)
const shown = computed(() =>
  (data.value?.suggestions || []).filter(
    s =>
      (!certainty.value || s.certainty === certainty.value) &&
      (!field.value || s.field === field.value) &&
      (!who.value || initials(s.evidence.walk.collector) === who.value),
  ),
)
const LIMIT = 200
const showAll = ref(false)
const visible = computed(() => (showAll.value ? shown.value : shown.value.slice(0, LIMIT)))
const groups = computed(() => {
  const out = new Map<Discrepancy['kind'], Discrepancy[]>()
  for (const d of data.value?.discrepancies || []) out.set(d.kind, [...(out.get(d.kind) || []), d])
  return [...out]
})

// ------------------------------------------------------------ downloads
const walk = ref('')
const walkOptions = computed(() =>
  (data.value?.inventory.walks || []).map(w => ({
    value: w.id,
    label: `${day(w.date)} · ${initials(w.collector)} · ${w.name}${w.stored ? '' : ` (${t('por revisar')})`}`,
  })),
)
const exportUrl = (file: string, walkId?: string) =>
  `api/monitoring/export/${file}${walkId ? `?walk=${encodeURIComponent(walkId)}` : ''}`

/** The corrections shown, as a CSV in the interface language. */
function downloadCorrections() {
  const q = (v: unknown) => {
    const s = String(v ?? '')
    const safe = /^[=+\-@\t]/.test(s) ? `'${s}` : s
    return /[",\r\n]/.test(safe) ? `"${safe.replaceAll('"', '""')}"` : safe
  }
  const header = ['Sheet', 'Sheet_row', 'Field', 'Current', 'Suggested', 'Certainty', 'Reason', 'Walk_date', 'Collector', 'Wikiloc_note', 'Walk_link']
  const lines = shown.value.map(s =>
    [
      s.sheet,
      s.row,
      s.field,
      cell(s.field, s.current),
      cell(s.field, s.suggested),
      s.certainty,
      reasonOf(s),
      s.evidence.walk.date,
      s.evidence.walk.collector,
      s.evidence.text,
      s.evidence.walk.url,
    ]
      .map(q)
      .join(','),
  )
  const a = document.createElement('a')
  a.href = URL.createObjectURL(new Blob(['\uFEFF' + [header.join(','), ...lines].join('\r\n')], { type: 'text/csv;charset=utf-8' }))
  a.download = t('correcciones_wikiloc.csv')
  a.click()
  URL.revokeObjectURL(a.href)
}
const megabytes = (bytes: number) => Math.round(bytes / 1e6)
</script>

<template>
  <div class="h-full overflow-y-auto">
    <div class="toolbar">
      <p class="hint max-w-3xl pb-1">
        {{
          $t(
            'Lo que la app guarda de Wikiloc, las descargas para el equipo y las correcciones que sugieren los puntos para la hoja. Solo lectura: la hoja se corrige en Tablas.',
          )
        }}
      </p>
      <button class="btn ml-auto" :disabled="loading" :title="$t('Calcular de nuevo')" @click="load">
        <RefreshCw :size="14" :class="{ 'animate-spin': loading }" /> {{ $t('Actualizar') }}
      </button>
    </div>

    <div class="space-y-6 p-3 sm:p-4">
      <p v-if="loading && !data" class="hint">{{ $t('Comparando los puntos de Wikiloc con la hoja…') }}</p>

      <template v-if="data">
        <!-- What the app holds -->
        <section class="space-y-2">
          <h2 class="text-base font-semibold">{{ $t('Datos de Wikiloc en la app') }}</h2>
          <p class="text-sm">
            {{
              $t(
                '{stored} recorridos en el mapa y {waiting} por revisar, del {first} al {last}: {points} puntos ({linked} con su fila de la hoja) y {photos} fotos ({mb} MB).',
                {
                  stored: data.inventory.stored,
                  waiting: data.inventory.waiting,
                  first: day(data.inventory.first),
                  last: day(data.inventory.last),
                  points: data.inventory.points,
                  linked: data.inventory.linked,
                  photos: data.inventory.photos,
                  mb: megabytes(data.inventory.photoBytes),
                },
              )
            }}
          </p>
          <p v-if="data.inventory.timed < data.inventory.stored + data.inventory.waiting" class="hint">
            {{
              $t(
                'Las líneas que vienen de la página pública de Wikiloc no tienen horas GPS; solo las de un GPX subido a la app ({n}) las tienen.',
                { n: data.inventory.timed },
              )
            }}
          </p>
          <div class="overflow-x-auto">
            <table class="text-sm">
              <thead class="text-left text-xs text-stone-500">
                <tr>
                  <th class="py-1 pr-4 font-normal">{{ $t('Colector') }}</th>
                  <th class="py-1 pr-4 text-right font-normal">{{ $t('En el mapa') }}</th>
                  <th class="py-1 pr-4 text-right font-normal">{{ $t('Por revisar') }}</th>
                  <th class="py-1 pr-4 font-normal">{{ $t('Desde') }}</th>
                  <th class="py-1 pr-4 font-normal">{{ $t('Hasta') }}</th>
                  <th class="py-1 pr-4 text-right font-normal">{{ $t('Puntos') }}</th>
                  <th class="py-1 text-right font-normal" :title="$t('Días con filas de monitoreo de esa persona pero sin recorrido en la app')">
                    {{ $t('Días sin recorrido') }}
                  </th>
                </tr>
              </thead>
              <tbody class="tabular-nums">
                <tr v-for="c in data.inventory.collectors" :key="c.collector || ''" class="border-t border-stone-100">
                  <td class="py-1 pr-4">{{ c.collector || $t('sin recolector') }}</td>
                  <td class="py-1 pr-4 text-right">{{ c.stored }}</td>
                  <td class="py-1 pr-4 text-right">{{ c.waiting }}</td>
                  <td class="py-1 pr-4">{{ day(c.first) }}</td>
                  <td class="py-1 pr-4">{{ day(c.last) }}</td>
                  <td class="py-1 pr-4 text-right">{{ c.points }}</td>
                  <td class="py-1 text-right">{{ c.daysWithoutWalk }}</td>
                </tr>
              </tbody>
            </table>
          </div>
        </section>

        <!-- Downloads -->
        <section class="space-y-2">
          <h2 class="text-base font-semibold">{{ $t('Descargas') }}</h2>
          <div class="flex flex-wrap gap-2">
            <a
              class="btn"
              :href="exportUrl('rows.csv')"
              download
              :title="$t('Cada fila de monitoreo de Collection_data con todas sus columnas y, al final, la posición de su punto de Wikiloc')"
              ><Download :size="15" /> {{ $t('Filas de monitoreo (CSV)') }}</a
            >
            <a class="btn" :href="exportUrl('points.csv')" download :title="$t('Cada punto de Wikiloc con su fila de la hoja')"
              ><Download :size="15" /> {{ $t('Puntos de todos los recorridos (CSV)') }}</a
            >
            <a class="btn" :href="exportUrl('walks.gpx')" download :title="$t('Las líneas GPS y los puntos de todos los recorridos')"
              ><Download :size="15" /> {{ $t('Todos los recorridos (GPX)') }}</a
            >
          </div>
          <div class="flex flex-wrap items-end gap-2">
            <label class="block w-full max-w-md">
              <span class="field-label">{{ $t('Un recorrido') }}</span>
              <ChoiceField v-model="walk" class="field-input" :freetext="false" :options="walkOptions" :placeholder="$t('Elige un recorrido')" />
            </label>
            <a class="btn" :class="{ 'pointer-events-none opacity-50': !walk }" :href="walk ? exportUrl('points.csv', walk) : undefined" download
              ><Download :size="15" /> {{ $t('Puntos (CSV)') }}</a
            >
            <a class="btn" :class="{ 'pointer-events-none opacity-50': !walk }" :href="walk ? exportUrl('walks.gpx', walk) : undefined" download
              ><Download :size="15" /> {{ $t('Recorrido (GPX)') }}</a
            >
          </div>
          <p class="hint">
            {{
              $t(
                'Fechas como AAAA-MM-DD y horas como h:mm, para que las hojas de cálculo las lean bien. Las filas de monitoreo llevan al final Wikiloc_latitude, Wikiloc_longitude, Wikiloc_transect_section y la nota del punto.',
              )
            }}
          </p>
        </section>

        <!-- Corrections -->
        <section class="space-y-2">
          <h2 class="text-base font-semibold">{{ $t('Correcciones sugeridas') }}</h2>
          <p class="text-sm">
            {{
              $t(
                'El transecto se calcula con la posición de cada punto sobre las secciones T1–T4 del sendero. De los {checked} puntos cuya fila tiene transecto, {agree} coinciden; {filled} filas sin transecto se pueden llenar y {differ} filas dicen otra sección.',
                {
                  checked: data.counts.agree + data.counts.differ,
                  agree: data.counts.agree,
                  filled: data.counts.filled,
                  differ: data.counts.differ,
                },
              )
            }}
          </p>
          <div class="flex flex-wrap items-end gap-2">
            <div class="flex flex-wrap gap-1" role="group" :aria-label="$t('Certeza')">
              <button
                class="rounded-full border px-2.5 py-1 text-sm"
                :class="!certainty ? 'border-brand-600 bg-brand-50 text-brand-800' : 'border-stone-300 bg-white'"
                @click="certainty = ''"
              >
                {{ $t('Todas') }} <span class="tabular-nums text-stone-500">{{ data.suggestions.length }}</span>
              </button>
              <button
                v-for="c in ['certain', 'likely', 'check'] as Certainty[]"
                :key="c"
                class="rounded-full border px-2.5 py-1 text-sm"
                :class="certainty === c ? 'border-brand-600 bg-brand-50 text-brand-800' : 'border-stone-300 bg-white'"
                :title="$t(CERTAINTY_HINT[c])"
                @click="certainty = certainty === c ? '' : c"
              >
                {{ $t(CERTAINTY[c]) }} <span class="tabular-nums text-stone-500">{{ data.counts.byCertainty[c] }}</span>
              </button>
            </div>
            <label class="block w-44">
              <span class="field-label">{{ $t('Columna') }}</span>
              <ChoiceField
                v-model="field"
                class="field-input"
                :freetext="false"
                :options="[{ value: '', label: $t('Todas') }, ...fields.map(f => ({ value: f, label: f }))]"
              />
            </label>
            <label class="block w-32">
              <span class="field-label">{{ $t('Colector') }}</span>
              <ChoiceField
                v-model="who"
                class="field-input"
                :freetext="false"
                :options="[{ value: '', label: $t('Todos') }, ...collectors.map(c => ({ value: c, label: c }))]"
              />
            </label>
            <button class="btn ml-auto" :disabled="!shown.length" :title="$t('Las correcciones de la lista, como CSV')" @click="downloadCorrections">
              <Download :size="15" /> CSV
            </button>
          </div>
          <p v-if="!shown.length" class="hint">{{ $t('No hay correcciones con esos filtros.') }}</p>
          <div v-else class="overflow-x-auto rounded-md border border-stone-200 bg-white">
            <table class="w-full text-sm">
              <thead class="sticky top-0 bg-stone-50 text-left text-xs text-stone-500">
                <tr>
                  <th class="px-2 py-1.5 font-normal">{{ $t('Fila') }}</th>
                  <th class="px-2 py-1.5 font-normal">{{ $t('Recorrido') }}</th>
                  <th class="px-2 py-1.5 font-normal">{{ $t('Columna') }}</th>
                  <th class="px-2 py-1.5 font-normal">{{ $t('Ahora') }}</th>
                  <th class="px-2 py-1.5 font-normal">{{ $t('Sugerido') }}</th>
                  <th class="px-2 py-1.5 font-normal">{{ $t('Certeza') }}</th>
                  <th class="px-2 py-1.5 font-normal">{{ $t('Por qué') }}</th>
                  <th class="px-2 py-1.5 font-normal">{{ $t('Nota de Wikiloc') }}</th>
                  <th class="px-2 py-1.5"></th>
                </tr>
              </thead>
              <tbody class="divide-y divide-stone-100">
                <tr v-for="s in visible" :key="`${s.recordId}|${s.field}`" class="align-top">
                  <td class="px-2 py-1.5 tabular-nums">{{ s.row }}</td>
                  <td class="px-2 py-1.5 whitespace-nowrap">{{ day(s.evidence.walk.date) }} · {{ initials(s.evidence.walk.collector) }}</td>
                  <td class="px-2 py-1.5 font-mono text-xs">{{ s.field }}</td>
                  <td class="px-2 py-1.5 text-stone-500">{{ cell(s.field, s.current) }}</td>
                  <td class="px-2 py-1.5 font-medium">{{ cell(s.field, s.suggested) }}</td>
                  <td class="px-2 py-1.5">
                    <span class="rounded px-1.5 py-0.5 text-xs font-medium whitespace-nowrap" :class="CERTAINTY_CLASS[s.certainty]">{{
                      $t(CERTAINTY[s.certainty])
                    }}</span>
                  </td>
                  <td class="min-w-64 px-2 py-1.5 text-xs text-stone-700">{{ reasonOf(s) }}</td>
                  <td class="min-w-48 px-2 py-1.5 text-xs">«{{ s.evidence.text }}»</td>
                  <td class="px-2 py-1.5 whitespace-nowrap">
                    <a class="btn-ghost" :href="mapLink(s.evidence.walk.date)" :title="$t('Ver el día en el mapa')"><MapPinned :size="14" /></a>
                    <a
                      v-if="s.evidence.walk.url"
                      class="btn-ghost"
                      :href="s.evidence.walk.url"
                      target="_blank"
                      rel="noopener"
                      :title="$t('Abrir en Wikiloc')"
                      ><ExternalLink :size="14"
                    /></a>
                  </td>
                </tr>
              </tbody>
            </table>
            <button v-if="shown.length > LIMIT && !showAll" class="btn m-2" @click="showAll = true">
              {{ $t('Ver las {n}', { n: shown.length }) }}
            </button>
          </div>
        </section>

        <!-- Other discrepancies -->
        <section class="space-y-2">
          <h2 class="text-base font-semibold">{{ $t('Otras diferencias') }}</h2>
          <p v-if="!groups.length" class="hint">{{ $t('Ninguna.') }}</p>
          <details v-for="[kind, list] in groups" :key="kind" class="rounded-md border border-stone-200 bg-white">
            <summary class="cursor-pointer px-3 py-2 text-sm font-medium">
              {{ $t(KIND[kind]) }} <span class="tabular-nums text-stone-500">{{ list.length }}</span>
            </summary>
            <ul class="divide-y divide-stone-100 text-sm">
              <li v-for="(d, i) in list" :key="i" class="flex flex-wrap gap-x-3 px-3 py-1.5">
                <span class="whitespace-nowrap">{{ d.date !== null ? formatSerial(d.date) : '—' }} · {{ initials(d.collector) }}</span>
                <span v-if="d.row" class="tabular-nums">{{ $t('Fila {n}', { n: d.row }) }}</span>
                <span v-if="d.species" class="italic">{{ d.species }}</span>
                <span v-if="d.mark">{{ d.mark }}</span>
                <span v-if="d.minutes !== null">{{ formatMinutes(d.minutes) }}</span>
                <span v-if="d.evidence.text" class="text-stone-600">«{{ d.evidence.text }}»</span>
                <span class="text-xs text-stone-500">{{ reasonOf(d) }}</span>
                <a
                  v-if="d.evidence.walk.url"
                  class="btn-ghost ml-auto"
                  :href="d.evidence.walk.url"
                  target="_blank"
                  rel="noopener"
                  :title="$t('Abrir en Wikiloc')"
                  ><ExternalLink :size="14"
                /></a>
              </li>
            </ul>
          </details>
          <details v-if="data.daysWithoutWalk.length" class="rounded-md border border-stone-200 bg-white">
            <summary class="cursor-pointer px-3 py-2 text-sm font-medium">
              {{ $t('Días de monitoreo sin recorrido en la app') }}
              <span class="tabular-nums text-stone-500">{{ data.daysWithoutWalk.length }}</span>
            </summary>
            <p class="hint px-3">
              {{
                $t(
                  'Días con filas de monitoreo pero sin recorrido de Wikiloc en la app (no se buscó o su título no dice «monitoreo»): sus transectos no se pueden calcular.',
                )
              }}
            </p>
            <ul class="divide-y divide-stone-100 text-sm">
              <li v-for="d in data.daysWithoutWalk" :key="`${d.date}|${d.collector}`" class="flex gap-3 px-3 py-1.5">
                <span class="whitespace-nowrap">{{ formatSerial(d.date) }} · {{ initials(d.collector) }}</span>
                <span>{{ $tn(d.rows, '{n} fila', '{n} filas') }}</span>
                <span v-if="d.blankSections" class="text-amber-800">{{ $t('{n} sin transecto', { n: d.blankSections }) }}</span>
              </li>
            </ul>
          </details>
        </section>
      </template>
    </div>
  </div>
</template>
