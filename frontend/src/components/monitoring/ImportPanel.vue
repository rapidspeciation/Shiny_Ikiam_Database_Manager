<script setup lang="ts">
import { computed, onMounted, ref, shallowRef, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { ExternalLink, FileUp, ImagePlus, ListPlus, MapPin, Trash2 } from 'lucide-vue-next'
import SheetGrid from '../SheetGrid.vue'
import WikilocBar from './WikilocBar.vue'
import { useMonitoring, type WikilocWalk } from '../../composables/useMonitoring'
import { api, requestId } from '../../lib/api'
import { isBlank } from '../../lib/cells'
import { formatSerial, isoToSerial } from '../../lib/dates'
import {
  MARK_THRESHOLD,
  captureValues,
  existingRow,
  formatMinutes,
  hasMark,
  locateCapture,
  parseGpx,
  preservedForRule,
  trackLength,
  trackSpan,
  type Gpx,
  type ImportedCapture,
} from '../../lib/monitoring'
import { errorText, notify } from '../../lib/notice'
import { persistentRef } from '../../lib/persist'
import { fillIfBlank, orderColumns } from '../../lib/rows'
import type { CellValue, TableRow } from '../../lib/types'
import { usePending } from '../../stores/pending'
import { useSession } from '../../stores/session'
import { useTables } from '../../stores/tables'

/**
 * "Importar recorrido": a Wikiloc GPX of one monitoring walk becomes new
 * Collection_data rows (one per waypoint), reviewed in the grid before saving.
 * The track and the capture points are kept in the app for the map.
 */
const MODULE = 'Collection_data'
const DAY_SHEET = 'SamplingDay_data'
const pending = usePending()
const tables = useTables()
const session = useSession()
const { table, rows, taxa, isIthomiini, options, creates, createFormulas, tracks, loadTracks, walks, loadWalks } = useMonitoring()
tables.load(DAY_SHEET).catch(() => {})

/** The walk being reviewed: a GPX file, or a walk read from a Wikiloc page (with photos). */
const file = shallowRef<{ name: string; gpx: Gpx; walk?: WikilocWalk } | null>(null)
const date = ref('')
const collector = persistentRef('monitoring:collector', '')
const skip = ref<Set<number>>(new Set())
const busy = ref(false)
const recentCount = ref(15)

const collectors = computed(() => (options.value.Collector || []).filter(c => / - /.test(c)))
const initials = computed(() => collector.value.split(' - ')[0].trim())

/** Loads a GPX (chosen with the button or shared from the phone) for review. */
function loadGpx(name: string, text: string) {
  try {
    const gpx = parseGpx(text)
    if (!gpx.waypoints.length && !gpx.track.length) return notify('El archivo no tiene puntos ni recorrido', 'error')
    file.value = { name, gpx }
    date.value = trackSpan(gpx.track)?.date || /(\d{4}-\d{2}-\d{2})/.exec(name)?.[1] || ''
    skip.value = new Set()
    detectCollector()
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
async function choose(event: Event) {
  const input = event.target as HTMLInputElement
  const picked = input.files?.[0]
  input.value = ''
  if (picked) loadGpx(picked.name, await picked.text())
}

/**
 * Something shared to the installed app from Android's share menu (see
 * public/sw.js): a GPX file is opened for review, a Wikiloc link is queued.
 */
const route = useRoute()
const router = useRouter()
const bar = ref<InstanceType<typeof WikilocBar>>()
async function receiveShared() {
  const flag = route.query.compartido
  if (!flag) return
  router.replace({ query: { vista: 'importar' } })
  if (flag === '0') return notify('La app aún no estaba lista; vuelve a compartir desde Wikiloc.', 'error')
  const inbox = await caches.open('ithomiini-share')
  const response = await inbox.match('./shared')
  await inbox.delete('./shared')
  if (!response) return
  const shared = (await response.json()) as { file: { name: string; text: string } | null; text: string; url: string }
  if (shared.file) loadGpx(shared.file.name || 'compartido.gpx', shared.file.text)
  else if (/wikiloc\.com/.test(`${shared.url} ${shared.text}`)) await bar.value?.queue(`${shared.url} ${shared.text}`)
  else notify('Lo compartido no es un GPX ni un enlace de Wikiloc', 'error')
}
const onShared = () => receiveShared().catch(e => notify(errorText(e), 'error'))
onMounted(onShared)
// Also when the app was already open on this screen.
watch(() => route.query.compartido, flag => flag && onShared())

const waiting = computed(() => walks.value.filter(w => w.status === 'waiting'))
function openWalk(w: WikilocWalk) {
  file.value = {
    name: w.name,
    gpx: { name: w.name, track: w.track, waypoints: w.waypoints.map(p => ({ ...p, time: null })) },
    walk: w,
  }
  date.value = w.date || ''
  skip.value = new Set()
  detectCollector()
}
async function removeWalk(w: WikilocWalk) {
  if (!confirm(`¿Quitar "${w.name}" de la lista? Se puede volver a traer desde Wikiloc.`)) return
  try {
    await api(`monitoring/wikiloc/${encodeURIComponent(w.id)}`, { method: 'DELETE', body: {} })
    if (file.value?.walk?.id === w.id) file.value = null
    await loadWalks()
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
/** A GPX of the same walk already stored (it has the GPS times): the Wikiloc photos can join it. */
const gpxTrack = computed(() =>
  file.value?.walk
    ? tracks.value.find(t => t.date === date.value && t.collector === collector.value && !t.wikiloc) || null
    : null,
)
async function addPhotosToTrack() {
  if (!file.value?.walk || !gpxTrack.value) return
  busy.value = true
  try {
    const result = await api<{ matched: number }>(`monitoring/tracks/${encodeURIComponent(gpxTrack.value.id)}/photos`, {
      method: 'POST',
      body: { walkId: file.value.walk.id },
    })
    await Promise.all([loadTracks(), loadWalks()])
    notify(`Fotos añadidas a ${result.matched} puntos del recorrido del ${formatSerial(isoToSerial(date.value))}`, 'success')
    file.value = null
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}
const photoUrl = (id: string) => `api/monitoring/photos/${id}`

/** Wikiloc names like "Monitoreo ithomidos FCH 26 SEP 2026" carry the collector's initials. */
function detectCollector() {
  if (!file.value) return
  const name = `${file.value.gpx.name} ${file.value.name}`
  const found = collectors.value.find(c => new RegExp(`\\b${c.split(' - ')[0].trim()}\\b`).test(name))
  if (found) collector.value = found
}
// The list of collectors arrives with Collection_data, possibly after the file was chosen.
watch(collectors, detectCollector)

const span = computed(() => (file.value ? trackSpan(file.value.gpx.track) : null))
const captures = computed<ImportedCapture[]>(() =>
  (file.value?.gpx.waypoints || [])
    .map(w => locateCapture(w, taxa.value))
    .sort((a, b) => (a.seq ?? 1e9) - (b.seq ?? 1e9) || (a.minutes ?? 0) - (b.minutes ?? 0)),
)

/** Earlier rows of each field mark, to tell a new mark from a recapture. */
const marks = computed(() => {
  const out = new Map<string, TableRow[]>()
  for (const row of rows.value.filter(hasMark)) {
    const id = String(row.values.FieldMark_ID).trim().toUpperCase()
    out.set(id, [...(out.get(id) || []), row])
  }
  return out
})
/** Rows with the same field mark from before this walk: if any, the capture is a recapture. */
function earlier(c: ImportedCapture) {
  if (!c.markId || !date.value) return []
  const day = isoToSerial(date.value)
  return (marks.value.get(c.markId) || []).filter(
    r => typeof r.values.Collection_date === 'number' && r.values.Collection_date < day,
  )
}
/** Earlier rows of the same mark on the same species: the capture is a recapture. */
const sameIndividual = (c: ImportedCapture) =>
  earlier(c).filter(r => !!c.species && String(r.values.SPECIES ?? '').toLowerCase() === c.species.toLowerCase())
// The 30-preserved rule counts every preserved butterfly from Ikiam and Casa de Lin.
// Counted up to the day of the walk, so an old walk is judged by the count it had then.
const preservedBySpecies = computed(() =>
  preservedForRule(table.value?.rows || [], /^\d{4}-\d{2}-\d{2}$/.test(date.value) ? isoToSerial(date.value) : undefined),
)

interface Check {
  text: string
  kind: 'ok' | 'info' | 'warn'
}
const checks = computed(() =>
  captures.value.map((c, i) => {
    // Until the sheet is loaded, marks and names cannot be checked yet.
    if (!table.value) return { existing: null, list: [{ kind: 'info', text: 'Cargando la hoja…' } as Check] }
    const out: Check[] = []
    const existing = date.value ? existingRow(rows.value, date.value, c) : null
    // Already in the sheet: nothing will be written, so no further checks.
    if (existing) return { existing, list: [{ kind: 'info', text: `Ya está en la hoja (fila ${existing.row})` } as Check] }
    if (c.markId) {
      const first = sameIndividual(c)[0]
      const others = earlier(c).filter(r => !sameIndividual(c).includes(r))
      if (first)
        out.push({
          kind: 'ok',
          text: `Recaptura de ${c.markId} (marcada ${formatSerial(first.values.Collection_date as number)})`,
        })
      else out.push({ kind: 'ok', text: `Nueva marca ${c.markId}` })
      for (const r of others)
        out.push({ kind: 'warn', text: `${c.markId} ya se usó para ${r.values.SPECIES} (fila ${r.row}): ¿ID repetida?` })
      if (!first && c.recaptureNote && !others.length)
        out.push({ kind: 'warn', text: `Dice recaptura, pero ${c.markId} no está en la hoja` })
      if (captures.value.some((o, j) => j !== i && o.markId === c.markId))
        out.push({ kind: 'warn', text: `${c.markId} aparece dos veces en este recorrido` })
    } else {
      out.push({ kind: 'info', text: 'Preservado (sin marca)' })
      const preserved = c.species ? preservedBySpecies.value.get(c.species) || 0 : 0
      if (preserved >= MARK_THRESHOLD && isIthomiini(c.species))
        out.push({ kind: 'warn', text: `${c.species} ya tiene ${preserved} preservados: ¿no debía marcarse?` })
    }
    if (!c.species) out.push({ kind: 'warn', text: 'Sin especie' })
    else if (!c.known) out.push({ kind: 'warn', text: 'Nombre no encontrado en la hoja: revisar' })
    if (!c.sex) out.push({ kind: 'warn', text: 'Sin sexo' })
    if (c.minutes === null) out.push({ kind: 'warn', text: 'Sin hora' })
    if (c.height === null) out.push({ kind: 'warn', text: 'Sin altura' })
    if (!c.cloud) out.push({ kind: 'warn', text: 'Sin clima' })
    if (c.section === null) out.push({ kind: 'warn', text: `Lejos del sendero (${c.sectionDistance} m)` })
    if (c.rest) out.push({ kind: 'info', text: `A notas: “${c.rest}”` })
    return { existing, list: out }
  }),
)
const included = computed(() => captures.value.filter((_, i) => !skip.value.has(i) && !checks.value[i].existing))
function toggle(i: number) {
  const next = new Set(skip.value)
  if (next.has(i)) next.delete(i)
  else next.add(i)
  skip.value = next
}

const ready = computed(() => !!table.value && !!file.value && /^\d{4}-\d{2}-\d{2}$/.test(date.value) && !!collector.value)

/** Keeps the track and its points in the app (same file twice is stored once). */
async function storeTrack() {
  if (!file.value) return
  await api('monitoring/tracks', {
    method: 'POST',
    body: {
      requestId: requestId(),
      date: date.value,
      collector: collector.value,
      name: file.value.gpx.name || file.value.name,
      track: file.value.gpx.track,
      wikilocWalkId: file.value.walk?.id,
      captures: captures.value.map(c => ({
        lat: c.lat,
        lon: c.lon,
        ele: c.ele,
        text: c.text,
        seq: c.seq,
        species: c.species,
        subspecies: c.subspecies,
        sex: c.sex,
        minutes: c.minutes,
        height: c.height,
        cloud: c.cloud,
        markId: c.markId,
        recapture: sameIndividual(c).length > 0,
        section: c.section,
        photos: c.photos,
      })),
    },
  })
  await Promise.all([loadTracks(), loadWalks()])
}

async function onlyTrack() {
  busy.value = true
  try {
    await storeTrack()
    notify('Recorrido guardado en la app; ya aparece en el mapa', 'success')
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}

/** The day's SamplingDay_data row: fills start and end if empty, or adds the row. */
function samplingDay() {
  if (!span.value) return 'sin hora de inicio/fin en el GPX'
  const serial = isoToSerial(date.value)
  const start = span.value.start / 1440
  const end = span.value.end / 1440
  const existing = tables.tables[DAY_SHEET]?.rows.find(
    r => r.observed && r.values.Date === serial && String(r.values.Collectors_initials ?? '').trim() === initials.value,
  )
  if (existing) {
    const label = `${initials.value} ${formatSerial(serial)}`
    const filled = [
      fillIfBlank(DAY_SHEET, existing, label, 'Start_time', start),
      fillIfBlank(DAY_SHEET, existing, label, 'End_time', end),
    ].filter(Boolean).length
    return filled
      ? `hora de inicio/fin completada en SamplingDay_data (fila ${existing.row})`
      : 'SamplingDay_data ya tenía este día'
  }
  if (
    pending.creates.some(
      c => c.module === DAY_SHEET && c.values.Date === serial && c.values.Collectors_initials === initials.value,
    )
  )
    return 'SamplingDay_data ya pendiente'
  pending.addCreate(DAY_SHEET, `${initials.value} ${formatSerial(serial)}`, {
    Date: serial,
    Location: 'Ikiam',
    Purpose: 'Monitoring',
    Start_time: start,
    End_time: end,
    Collectors_initials: initials.value,
  })
  return 'fila nueva en SamplingDay_data'
}

async function addRows() {
  if (!ready.value) return
  busy.value = true
  try {
    try {
      await storeTrack()
    } catch (e) {
      // The rows matter most; the track can be uploaded again later.
      notify(`El recorrido no se guardó en la app (${errorText(e)}); las filas sí se añaden.`, 'error')
    }
    for (const c of included.value) {
      const values = captureValues(c, { date: date.value, collector: collector.value, section: c.section })
      for (const field of createFormulas.value) delete values[field]
      const label = [c.markId || c.species, formatMinutes(c.minutes)].filter(Boolean).join(' ')
      pending.addCreate(MODULE, label || 'monitoreo', values)
    }
    const day = samplingDay()
    pending.touch()
    notify(`${included.value.length} filas nuevas en Collection_data y ${day}. Revisa y pulsa Guardar.`, 'success')
    file.value = null
  } finally {
    busy.value = false
  }
}

const columns = computed(() =>
  table.value
    ? orderColumns(table.value.columns, [
        'Release_Collect',
        'FieldMark_ID',
        'SPECIES',
        'Subspecies_Form',
        'Sex',
        'Collection_date',
        'Collection_time',
        'Transect_section',
        'Flight_height',
        'Cloud_cover',
        'Rainfall',
        'Collector',
        'Identifier',
        'ID_status',
        'CAM_ID',
        'Tube_1_id',
        'Tube_1_tissue',
        'Butterfly_weight',
        'Preservation_medium',
        'Notes_Collection_data',
      ])
    : [],
)
const rowOptions = {
  Subspecies_Form: (row: Record<string, CellValue>) =>
    taxa.value.get(String(row.SPECIES ?? '')) || options.value.Subspecies_Form || [],
}
const recent = computed(() => rows.value.slice(-recentCount.value))
const monitoringCreates = computed(() => creates.value.filter(c => /^monitoring/i.test(String(c.values.Purpose ?? ''))))
const dayCreates = computed(() => pending.creates.filter(c => c.module === DAY_SHEET))

const chip = { ok: 'bg-brand-50 text-brand-700', info: 'bg-stone-100 text-stone-700', warn: 'bg-amber-100 text-amber-900' }
const minutes = (m: number | null) => formatMinutes(m) || '—'
const clean = (v: unknown) => (isBlank(v as CellValue) ? '—' : String(v))
</script>

<template>
  <div class="flex h-full flex-col">
    <div class="toolbar">
      <label class="btn-primary cursor-pointer" :class="{ 'pointer-events-none opacity-50': !session.canEdit }">
        <FileUp :size="15" /> Elegir GPX de Wikiloc
        <input type="file" accept=".gpx,application/gpx+xml,application/xml,text/xml" class="sr-only" @change="choose" />
      </label>
      <label>
        <span class="field-label">Fecha</span>
        <input v-model="date" type="date" class="field-input" />
      </label>
      <label class="min-w-52">
        <span class="field-label">Recolector</span>
        <select v-model="collector" class="field-input">
          <option value="" disabled>Elegir…</option>
          <option v-for="c in collectors" :key="c" :value="c">{{ c }}</option>
        </select>
      </label>
      <p v-if="file" class="hint pb-1.5">
        {{ file.gpx.name || file.name }} · {{ file.gpx.waypoints.length }} puntos
        <template v-if="span"> · {{ minutes(span.start) }}–{{ minutes(span.end) }}</template>
        · {{ (trackLength(file.gpx.track) / 1000).toFixed(2) }} km
      </p>
    </div>

    <WikilocBar ref="bar" />

    <div
      v-if="waiting.length"
      class="flex flex-wrap items-center gap-2 border-b border-stone-200 bg-brand-50 px-3 py-2 text-sm sm:px-4"
    >
      <span class="font-medium text-brand-700">Desde Wikiloc, por revisar:</span>
      <span
        v-for="w in waiting"
        :key="w.id"
        class="inline-flex items-center gap-1 rounded-md border bg-white py-0.5 pr-0.5 pl-2"
        :class="file?.walk?.id === w.id ? 'border-brand-600' : 'border-stone-300'"
      >
        <button class="hover:underline" @click="openWalk(w)">
          {{ w.date ? formatSerial(isoToSerial(w.date)) : w.name }} · {{ w.waypoints.length }} puntos
        </button>
        <a :href="w.url" target="_blank" rel="noopener" class="btn-ghost" title="Abrir en Wikiloc"><ExternalLink :size="13" /></a>
        <button class="btn-ghost" title="Quitar de la lista" @click="removeWalk(w)"><Trash2 :size="13" /></button>
      </span>
    </div>

    <section v-if="file" class="max-h-[60%] shrink-0 overflow-auto border-b border-stone-200 bg-white">
      <p v-if="file.walk" class="bg-stone-50 px-3 py-1.5 text-xs text-stone-600">
        Leído de la página pública de Wikiloc: sin horas GPS del recorrido (SamplingDay_data no se completa).
        <template v-if="!date"> Escribe la fecha arriba.</template>
      </p>
      <!-- Phones: one card per waypoint. -->
      <ul class="divide-y divide-stone-100 text-sm sm:hidden">
        <li
          v-for="(c, i) in captures"
          :key="i"
          class="flex gap-3 px-3 py-2"
          :class="{ 'opacity-50': skip.has(i) || checks[i].existing }"
        >
          <input
            type="checkbox"
            class="mt-1"
            :checked="!skip.has(i) && !checks[i].existing"
            :disabled="!!checks[i].existing"
            :aria-label="`Incluir ${c.text}`"
            @change="toggle(i)"
          />
          <a v-for="id in c.photos.slice(0, 1)" :key="id" :href="photoUrl(id)" target="_blank" rel="noopener" class="shrink-0">
            <img :src="photoUrl(id)" alt="" loading="lazy" class="h-16 w-16 rounded object-cover" />
          </a>
          <div class="min-w-0">
            <p>
              <b>{{ c.seq !== null ? `M${c.seq}` : '' }}</b> <i>{{ clean(c.species) }}</i> {{ c.subspecies || '' }}
            </p>
            <p class="text-xs text-stone-600">
              {{ clean(c.sex) }} · {{ minutes(c.minutes) }} · {{ c.height === null ? '—' : `${c.height} m` }} ·
              {{ clean(c.cloud) }} · <b>{{ clean(c.markId) }}</b> · {{ c.section ? `T${c.section}` : '—' }}
            </p>
            <p class="mt-1">
              <span
                v-for="(k, j) in checks[i].list"
                :key="j"
                class="mr-1 mb-0.5 inline-block rounded px-1.5 py-0.5 text-xs"
                :class="chip[k.kind]"
              >
                {{ k.text }}
              </span>
            </p>
          </div>
        </li>
      </ul>
      <table class="hidden w-full text-left text-xs sm:table">
        <thead class="sticky top-0 bg-stone-100 text-stone-600">
          <tr>
            <th class="px-2 py-1.5"></th>
            <th v-if="file.walk" class="px-2 py-1.5">Foto</th>
            <th class="px-2 py-1.5">Punto</th>
            <th class="px-2 py-1.5">Especie</th>
            <th class="px-2 py-1.5">Sexo</th>
            <th class="px-2 py-1.5">Hora</th>
            <th class="px-2 py-1.5">Altura</th>
            <th class="px-2 py-1.5">Clima</th>
            <th class="px-2 py-1.5">Marca</th>
            <th class="px-2 py-1.5">T</th>
            <th class="px-2 py-1.5">Revisión</th>
          </tr>
        </thead>
        <tbody>
          <tr
            v-for="(c, i) in captures"
            :key="i"
            class="border-t border-stone-100 align-top"
            :class="{ 'opacity-50': skip.has(i) || checks[i].existing }"
          >
            <td class="px-2 py-1.5">
              <input
                type="checkbox"
                :checked="!skip.has(i) && !checks[i].existing"
                :disabled="!!checks[i].existing"
                :aria-label="`Incluir ${c.text}`"
                @change="toggle(i)"
              />
            </td>
            <td v-if="file.walk" class="px-2 py-1.5">
              <a v-for="id in c.photos" :key="id" :href="photoUrl(id)" target="_blank" rel="noopener" class="mr-1 inline-block">
                <img
                  :src="photoUrl(id)"
                  alt=""
                  loading="lazy"
                  class="h-14 w-14 rounded object-cover hover:ring-2 hover:ring-brand-600"
                />
              </a>
            </td>
            <td class="max-w-64 px-2 py-1.5 text-stone-500" :title="c.text">{{ c.text }}</td>
            <td class="px-2 py-1.5 whitespace-nowrap">
              <i>{{ clean(c.species) }}</i> {{ c.subspecies || '' }}
            </td>
            <td class="px-2 py-1.5">{{ clean(c.sex) }}</td>
            <td class="px-2 py-1.5">{{ minutes(c.minutes) }}</td>
            <td class="px-2 py-1.5">{{ c.height === null ? '—' : `${c.height} m` }}</td>
            <td class="px-2 py-1.5 whitespace-nowrap">{{ clean(c.cloud) }}</td>
            <td class="px-2 py-1.5 font-medium">{{ clean(c.markId) }}</td>
            <td class="px-2 py-1.5">{{ c.section ? `T${c.section}` : '—' }}</td>
            <td class="px-2 py-1.5">
              <span
                v-for="(k, j) in checks[i].list"
                :key="j"
                class="mr-1 mb-0.5 inline-block rounded px-1.5 py-0.5"
                :class="chip[k.kind]"
              >
                {{ k.text }}
              </span>
            </td>
          </tr>
        </tbody>
      </table>
      <div class="flex flex-wrap items-center gap-2 px-3 py-2">
        <button class="btn-primary" :disabled="!ready || busy || !included.length" @click="addRows">
          <ListPlus :size="15" /> Añadir {{ included.length }} filas a Collection_data
        </button>
        <button v-if="gpxTrack" class="btn" :disabled="busy" @click="addPhotosToTrack">
          <ImagePlus :size="15" /> Solo añadir las fotos al GPX ya subido de este día
        </button>
        <button class="btn" :disabled="!ready || busy" title="Para recorridos cuyas filas ya están en la hoja" @click="onlyTrack">
          <MapPin :size="15" /> Solo guardar el recorrido (mapa)
        </button>
        <span class="hint">
          El transecto sale de la posición GPS; la marca repetida cuenta como recaptura. Nada se escribe en la hoja hasta pulsar
          Guardar.
        </span>
      </div>
    </section>

    <p class="hint px-4 py-1">
      <template v-if="monitoringCreates.length">{{ monitoringCreates.length }} filas nuevas de monitoreo (verde)</template>
      <template v-else> Elige o comparte el GPX de Wikiloc, o pega el enlace de la ruta arriba. </template>
      <template v-if="dayCreates.length"> · {{ dayCreates.length }} fila nueva en SamplingDay_data</template>
      · últimos {{ recentCount }} registros de monitoreo.
      <button class="underline" @click="recentCount += 15">Cargar más</button>
    </p>
    <div class="min-h-0 flex-1">
      <p v-if="!table" class="p-6 text-stone-500">Cargando Collection_data…</p>
      <SheetGrid
        v-else
        :module="MODULE"
        :rows="recent"
        :creates="monitoringCreates"
        :columns="columns"
        :options="options"
        :row-options="rowOptions"
        :create-formulas="createFormulas"
        :frozen="['Release_Collect']"
        label-field="FieldMark_ID"
        @notice="notify"
        @remove-create="
          id => {
            pending.removeCreate(id)
            pending.touch()
          }
        "
      />
    </div>
  </div>
</template>
