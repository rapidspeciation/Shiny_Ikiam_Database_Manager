<script setup lang="ts">
import DateField from '../DateField.vue'
import { computed, onBeforeUnmount, onMounted, ref, shallowRef, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import {
  ChevronDown,
  ChevronLeft,
  ChevronRight,
  Copy,
  ExternalLink,
  FileUp,
  ImagePlus,
  ListPlus,
  MapPin,
  RotateCcw,
  Trash2,
  X,
} from 'lucide-vue-next'
import SheetGrid from '../SheetGrid.vue'
import WikilocBar from './WikilocBar.vue'
import { useMonitoring, type StoredTrack, type WikilocWalk } from '../../composables/useMonitoring'
import { useFieldSampleIds } from '../../composables/useFieldSampleIds'
import { api, requestId } from '../../lib/api'
import { isBlank } from '../../lib/cells'
import { formatSerial, isoToSerial } from '../../lib/dates'
import { t } from '../../lib/i18n'
import {
  captureValues,
  capturesToStore,
  collectorFromName,
  collectorLabel,
  doubtfulMatch,
  existingRow,
  formatMinutes,
  hasMark,
  identificationFollows,
  locateCapture,
  phraseText,
  matchWalk,
  monitoringCollectors,
  parseGpx,
  preservedForRule,
  reviewCapture,
  samplingDayRow,
  timesFromTrack,
  trackLength,
  trackSpan,
  walkMarkRoles,
  withSheetValues,
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
const {
  table,
  rows,
  taxa,
  localTaxa,
  lists,
  isIthomiini,
  options,
  creates,
  createFormulas,
  tracks,
  loadTracks,
  walks,
  loadWalks,
  reopenWalk,
} = useMonitoring()
tables.load(DAY_SHEET).catch(() => {})
const sampleIds = useFieldSampleIds({ table: () => table.value, lists: () => lists.value })

/** The walk being reviewed: a GPX file, or a walk read from a Wikiloc page (with photos). */
const file = shallowRef<{ name: string; gpx: Gpx; walk?: WikilocWalk } | null>(null)
const date = ref('')
/**
 * Whose walk it is. Taken from the followed profile, the GPX author or the
 * title; never silently from last time: the last one used is only offered.
 */
const collector = ref('')
const collectorFrom = ref('')
const lastCollector = persistentRef('monitoring:collector', '')
const skip = ref<Set<number>>(new Set())
const busy = ref(false)
const recentCount = ref(15)

// The monitoring team first (AA, FCH, MJS today), then everyone else; no "NA - Missing data".
const collectorLists = computed(() =>
  monitoringCollectors(options.value.Collector || [], rows.value, tables.tables[DAY_SHEET]?.rows || []),
)
const people = computed(() => [...collectorLists.value.usual, ...collectorLists.value.others])
const collectorChoices = computed(() =>
  (['usual', 'others'] as const).flatMap(kind =>
    collectorLists.value[kind].map(c => ({
      value: c,
      label: c,
      group: kind === 'usual' ? t('Monitoreo') : t('Otros'),
      hint: c === lastCollector.value ? t('el último usado') : undefined,
    })),
  ),
)
const isReviewer = computed(() => ['reviewer', 'admin'].includes(session.user?.role || ''))
const initials = computed(() => collector.value.split(' - ')[0].trim())
const shortName = (c: string) => collectorLabel(c, people.value)

/** Followed Wikiloc profiles: a GPX whose author is one of them belongs to that profile's collector. */
const profiles = ref<{ wikilocUser: string; collector: string | null }[]>([])
async function loadProfiles() {
  try {
    profiles.value = (await api<{ profiles: typeof profiles.value }>('monitoring/wikiloc/profiles')).profiles
  } catch {
    /* only a hint for the collector */
  }
}

/** Loads a GPX (chosen with the button or shared from the phone) for review. */
function loadGpx(name: string, text: string) {
  try {
    const gpx = parseGpx(text)
    if (!gpx.waypoints.length && !gpx.track.length) return notify(t('El archivo no tiene puntos ni recorrido'), 'error')
    file.value = { name, gpx }
    date.value = trackSpan(gpx.track)?.date || /(\d{4}-\d{2}-\d{2})/.exec(name)?.[1] || ''
    skip.value = new Set()
    detectCollector()
  } catch (e) {
    // "El archivo no es un GPX" comes from lib/monitoring.ts, which has no interface language.
    notify(t(errorText(e)), 'error')
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
  if (flag === '0') return notify(t('La app aún no estaba lista; vuelve a compartir desde Wikiloc.'), 'error')
  const inbox = await caches.open('ithomiini-share')
  const response = await inbox.match('./shared')
  await inbox.delete('./shared')
  if (!response) return
  const shared = (await response.json()) as { file: { name: string; text: string } | null; text: string; url: string }
  if (shared.file) loadGpx(shared.file.name || 'compartido.gpx', shared.file.text)
  else if (/wikiloc\.com/.test(`${shared.url} ${shared.text}`)) await bar.value?.queue(`${shared.url} ${shared.text}`)
  else notify(t('Lo compartido no es un GPX ni un enlace de Wikiloc'), 'error')
}
const onShared = () => receiveShared().catch(e => notify(errorText(e), 'error'))
onMounted(() => {
  onShared()
  loadProfiles()
})
// Also when the app was already open on this screen.
watch(
  () => route.query.compartido,
  flag => flag && onShared(),
)

// ------------------------------------------------------------ walk lists

const waiting = computed(() => walks.value.filter(w => w.status === 'waiting'))
/** Phones: the lists fold away so the grid keeps its rows. */
const showWaiting = ref(false)
const showStored = ref(false)
const storedCount = ref(20)
const walkOf = (t: StoredTrack) => walks.value.find(w => w.trackId === t.id) || null

function openWalk(w: WikilocWalk) {
  file.value = {
    name: w.name,
    gpx: { name: w.name, track: w.track, waypoints: w.waypoints.map(p => ({ ...p, time: null })) },
    walk: w,
  }
  date.value = w.date || ''
  skip.value = new Set()
  showWaiting.value = false
  detectCollector()
}
async function removeWalk(w: WikilocWalk) {
  if (!confirm(t('¿Quitar "{name}" de la lista? Se puede volver a traer desde Wikiloc.', { name: w.name }))) return
  try {
    await api(`monitoring/wikiloc/${encodeURIComponent(w.id)}`, { method: 'DELETE', body: {} })
    if (file.value?.walk?.id === w.id) file.value = null
    await loadWalks()
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
/** An imported walk opened again for review (its rows may have been removed, or its notes corrected). */
async function reviewAgain(w: WikilocWalk) {
  try {
    const again = await reopenWalk(w.id)
    if (again) openWalk(again)
    showStored.value = false
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
/** Off the map; a Wikiloc walk goes back to "por revisar" (server/monitoring.mjs deleteTrack). */
async function removeTrack(walk: StoredTrack) {
  if (
    !confirm(
      t('¿Quitar del mapa el recorrido "{name}" ({date})? Las filas de la hoja no cambian.', {
        name: walk.name,
        date: formatSerial(isoToSerial(walk.date)),
      }),
    )
  )
    return
  try {
    await api(`monitoring/tracks/${encodeURIComponent(walk.id)}`, { method: 'DELETE', body: {} })
    await Promise.all([loadTracks(), loadWalks()])
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
    notify(
      t('Fotos añadidas a {n} puntos del recorrido del {date}', {
        n: result.matched,
        date: formatSerial(isoToSerial(date.value)),
      }),
      'success',
    )
    file.value = null
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}
const photoUrl = (id: string) => `api/monitoring/photos/${id}`

/** The collector of the walk: followed profile, GPX author, then initials or name in the title. */
function detectCollector() {
  if (!file.value) return
  const { walk, gpx, name } = file.value
  const profile = (id?: string | null) => (id && profiles.value.find(p => p.wikilocUser === id)?.collector) || null
  const found = (
    [
      // Spanish; t() where shown.
      [walk?.collector, 'del perfil de Wikiloc'],
      [profile(walk?.author ?? gpx.author?.id), 'del perfil de Wikiloc'],
      [gpx.author?.name ? collectorFromName(gpx.author.name, people.value) : null, 'del autor del GPX'],
      [collectorFromName(`${gpx.name} ${name}`, people.value), 'del título'],
    ] as [string | null | undefined, string][]
  ).find(([c]) => !!c)
  collector.value = found?.[0] || ''
  collectorFrom.value = found?.[1] || ''
}
// The collectors and profiles arrive with the sheet, possibly after the file was chosen.
watch([people, profiles], () => {
  if (file.value && !collector.value) detectCollector()
})
function pickCollector(value: string) {
  collector.value = value
  collectorFrom.value = ''
}

// ------------------------------------------------------------ review

const span = computed(() => (file.value ? trackSpan(file.value.gpx.track) : null))
const captures = computed<ImportedCapture[]>(() => {
  if (!file.value) return []
  const list = file.value.gpx.waypoints
    .map(w => locateCapture(w, taxa.value, localTaxa.value))
    .sort((a, b) => (a.seq ?? 1e9) - (b.seq ?? 1e9) || (a.minutes ?? 0) - (b.minutes ?? 0))
  // A point noted without time takes it from the GPS track (GPX files only).
  return timesFromTrack(list, file.value.gpx.track)
})

/** New mark, recapture (same mark, species and sex) or reused ID, from the marks before the walk. */
const roles = computed(() => (table.value ? walkMarkRoles(rows.value, date.value, captures.value) : []))
// The 30-preserved rule counts every preserved butterfly from Ikiam, Casa de Lin and Mariposario Ikiam.
// Counted up to the day of the walk, so an old walk is judged by the count it had then.
const preservedBySpecies = computed(() =>
  preservedForRule(table.value?.rows || [], /^\d{4}-\d{2}-\d{2}$/.test(date.value) ? isoToSerial(date.value) : undefined),
)

/** The walk's points paired with the collector's rows of that day (the same matcher as the map and the assistant). */
const pairing = computed(() =>
  table.value && /^\d{4}-\d{2}-\d{2}$/.test(date.value) && collector.value
    ? matchWalk(rows.value, date.value, collector.value, captures.value, { shift: false })
    : null,
)
// Spanish; t() where shown.
const HOW: Record<string, string> = { tie: 'empate con otra fila', order: 'orden del recorrido', sure: 'hora', mark: 'marca' }
const checks = computed(() =>
  captures.value.map((c, i) => {
    // Until the sheet is loaded, marks and names cannot be checked yet.
    if (!table.value) return { existing: null, recapture: null, list: [{ kind: 'info' as const, text: t('Cargando la hoja…') }] }
    const m = pairing.value?.matches[i]
    const review = reviewCapture(c, i, {
      rows: rows.value,
      date: date.value,
      captures: captures.value,
      roles: roles.value,
      preserved: preservedBySpecies.value,
      isIthomiini,
      ...(m ? { existing: m.rows[0] ?? null } : {}),
    })
    // A doubtful pairing is said here; on the map the point waits without a row in Dudas de emparejamiento.
    // The checks come in Spanish with their phrase (lib/monitoring.ts runs on the server too): shown in the interface language.
    review.list = review.list.map(k => (k.phrase ? { ...k, text: phraseText(k.phrase, t) } : k))
    if (m && m.rows.length && doubtfulMatch(m)) {
      const conflicts = m.conflicts.filter(k => k !== 'hora')
      review.list.push({
        kind: 'warn',
        text: conflicts.length
          ? t('Fila {n} por {how} (no coincide: {conflicts}): se guarda sin fila, para emparejar en Dudas', {
              n: m.rows[0].row,
              how: t(HOW[m.confidence]),
              conflicts: conflicts.map(k => t(k)).join(', '),
            })
          : t('Fila {n} por {how}: se guarda sin fila, para emparejar en Dudas', { n: m.rows[0].row, how: t(HOW[m.confidence]) }),
      })
    }
    return review
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

/** Keeps a walk's track and points in the app (the same walk twice is stored once). */
async function sendTrack(t: {
  name: string
  track: Gpx['track']
  walkId?: string
  date: string
  collector: string
  captures: (ImportedCapture & { doubt?: boolean })[]
  /** Captures already paired with their sheet rows (bulk import). */
  paired?: boolean
}) {
  // Paired as a whole walk (each row once, the closest); without a collector, each point on its own.
  const match = t.paired ? null : t.collector ? matchWalk(rows.value, t.date, t.collector, t.captures, { shift: false }) : null
  // Doubtful points go without a row, for Dudas; points without one get their new row when it is saved.
  const withRows: (ImportedCapture & { doubt?: boolean })[] = t.paired
    ? t.captures
    : match
      ? capturesToStore(t.captures, match.matches, { unpaired: false })
      : t.captures.map(raw => withSheetValues(raw, existingRow(rows.value, t.date, raw)))
  const recaptures = walkMarkRoles(rows.value, t.date, withRows)
  await api('monitoring/tracks', {
    method: 'POST',
    body: {
      requestId: requestId(),
      date: t.date,
      collector: t.collector,
      name: t.name,
      track: t.track,
      wikilocWalkId: t.walkId,
      captures: withRows.map((c, i) => ({
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
        recapture: recaptures[i]?.role === 'recapture',
        section: c.section,
        photos: c.photos,
        row: (c as { row?: number }).row ?? null,
        recordId: (c as { recordId?: string }).recordId ?? null,
        doubt: c.doubt || undefined,
      })),
    },
  })
}
async function storeTrack() {
  if (!file.value) return
  await sendTrack({
    name: file.value.gpx.name || file.value.name,
    track: file.value.gpx.track,
    walkId: file.value.walk?.id,
    date: date.value,
    collector: collector.value,
    captures: captures.value,
  })
  await Promise.all([loadTracks(), loadWalks()])
}

/**
 * Waiting Wikiloc walks that are already (partly) in the sheet go on the map:
 * points paired by mark or surely with their rows; doubtful points (a tie,
 * placed only by order, a note that disagrees, no row) without a row, flagged
 * for Dudas de emparejamiento and Revisión de datos, so one unclear point does
 * not hold back the walk. Walks without any point go on the map as a trail;
 * walks none of whose points are in the sheet stay for review (their rows are
 * added in the import).
 */
const registered = computed(() => {
  if (!table.value) return []
  return waiting.value
    .filter(w => w.date && w.collector)
    .map(w => {
      const all = w.waypoints.map(p => locateCapture({ ...p, time: null }, taxa.value, localTaxa.value))
      const match = matchWalk(rows.value, w.date!, w.collector!, all)
      return {
        w,
        date: match.date,
        paired: match.pairs.length,
        captures: capturesToStore(all, match.matches),
        doubts: match.matches.filter(m => !m.rows.length || doubtfulMatch(m)).length,
      }
    })
    .filter(x => x.paired || !x.w.waypoints.length)
})
const doubtCount = computed(() => registered.value.reduce((n, x) => n + x.doubts, 0))
async function registerAll() {
  const extra = doubtCount.value
    ? ` ${t('{n} puntos dudosos o sin fila se guardan sin emparejar y quedan en Dudas y en Revisión de datos.', { n: doubtCount.value })}`
    : ''
  if (
    !confirm(
      t('¿Pasar al mapa {n} recorridos ya registrados en la hoja? No se añaden filas.', { n: registered.value.length }) + extra,
    )
  )
    return
  busy.value = true
  let done = 0
  try {
    for (const { w, date: day, captures } of registered.value) {
      await sendTrack({ name: w.name, track: w.track, walkId: w.id, date: day, collector: w.collector!, captures, paired: true })
      done++
    }
    notify(t('{n} recorridos pasados al mapa', { n: done }), 'success')
  } catch (e) {
    notify(t('{n} pasados; luego falló: {error}', { n: done, error: errorText(e) }), 'error')
  } finally {
    await Promise.all([loadTracks(), loadWalks()])
    busy.value = false
  }
}

async function onlyTrack() {
  busy.value = true
  try {
    await storeTrack()
    notify(t('Recorrido guardado en la app; ya aparece en el mapa'), 'success')
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}

// ------------------------------------------------------------ SamplingDay_data

/** The collector's SamplingDay_data row of the walk's day, or one whose Date was typed wrong. */
const dayRow = computed(() =>
  file.value && /^\d{4}-\d{2}-\d{2}$/.test(date.value) && initials.value
    ? samplingDayRow(tables.tables[DAY_SHEET]?.rows || [], date.value, initials.value, span.value)
    : null,
)
function fixDayDate() {
  const found = dayRow.value
  if (!found?.broken) return
  fillIfBlank(
    DAY_SHEET,
    found.row,
    `${initials.value} ${formatSerial(isoToSerial(date.value))}`,
    'Date',
    isoToSerial(date.value),
    true,
  )
  pending.touch()
  notify(t('Date de SamplingDay_data fila {n} corregida (sin guardar)', { n: found.row.row }), 'success', 2500)
}
/**
 * What happened to SamplingDay_data with the last import; stays under the list
 * until the next one. Kept as the Spanish key and its values, shown with t().
 */
const dayNote = ref<{ key: string; vars?: Record<string, string | number>; warn?: boolean } | null>(null)

/** The day's SamplingDay_data row: fills start and end if empty, or adds the row. */
function samplingDay(): NonNullable<typeof dayNote.value> {
  if (!span.value) return { key: 'SamplingDay_data: sin hora de inicio/fin (no es un GPX)' }
  const serial = isoToSerial(date.value)
  const start = span.value.start / 1440
  const end = span.value.end / 1440
  const found = dayRow.value
  const label = `${initials.value} ${formatSerial(serial)}`
  if (found?.broken)
    return {
      key: 'SamplingDay_data fila {n} es este día pero su Date ({date}) no es una fecha: corrígela; no se añadió otra',
      vars: { n: found.row.row, date: String(found.row.values.Date) },
      warn: true,
    }
  if (found) {
    const filled = [
      fillIfBlank(DAY_SHEET, found.row, label, 'Start_time', start),
      fillIfBlank(DAY_SHEET, found.row, label, 'End_time', end),
    ].filter(Boolean).length
    return filled
      ? { key: 'SamplingDay_data: hora de inicio/fin completada (fila {n})', vars: { n: found.row.row } }
      : { key: 'SamplingDay_data ya tenía este día' }
  }
  if (
    pending.creates.some(
      c => c.module === DAY_SHEET && c.values.Date === serial && c.values.Collectors_initials === initials.value,
    )
  )
    return { key: 'SamplingDay_data ya pendiente' }
  pending.addCreate(DAY_SHEET, label, {
    Date: serial,
    Location: 'Ikiam',
    Purpose: 'Monitoring',
    Start_time: start,
    End_time: end,
    Collectors_initials: initials.value,
  })
  return { key: 'SamplingDay_data: fila nueva' }
}

// ------------------------------------------------------------ adding the rows

/** Photos and note of each new row, by its pending id (kept with the unsaved rows). */
const photoIndex = persistentRef<Record<string, { photos: string[]; text: string; label: string }>>(
  'monitoring:photos',
  {},
  { lasting: true },
)

async function addRows() {
  if (!ready.value) return
  busy.value = true
  try {
    try {
      await storeTrack()
    } catch (e) {
      // The rows matter most; the track can be uploaded again later.
      notify(t('El recorrido no se guardó en la app ({error}); las filas sí se añaden.', { error: errorText(e) }), 'error')
    }
    const list = included.value
    // Preserved butterflies get the next CAM and tube, as in Colecta.
    const ids = await sampleIds.next(list.filter(c => !c.markId).length)
    let k = 0
    for (const c of list) {
      const id = c.markId ? null : ids[k++]
      const values = captureValues(c, {
        date: date.value,
        collector: collector.value,
        section: c.section,
        cam: id?.cam,
        tube: id?.tube,
      })
      for (const field of createFormulas.value) delete values[field]
      const label = [c.markId || c.species, formatMinutes(c.minutes)].filter(Boolean).join(' ')
      // The walk waits for Guardar: automatic saving would write rows whose species are still to identify.
      const item = pending.addCreate(MODULE, label || 'monitoreo', values, { manual: true })
      const shownLabel = [c.markId, c.species || t('sin especie'), formatMinutes(c.minutes)].filter(Boolean).join(' ')
      photoIndex.value[item.clientId] = { photos: c.photos, text: c.text, label: shownLabel }
    }
    lastCollector.value = collector.value
    dayNote.value = samplingDay()
    pending.touch()
    // Short: on phones the message sits over the grid the person is about to edit.
    notify(t('{n} filas nuevas: revisa y pulsa Guardar', { n: list.length }), 'success', 2500)
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
const blankSpecies = (v: Record<string, CellValue>) => isBlank(v.SPECIES) || v.SPECIES === 'NA'
/** New rows, those still to identify first (their photos are beside the grid), then by time. */
const monitoringCreates = computed(() =>
  creates.value
    .filter(c => /^monitoring/i.test(String(c.values.Purpose ?? '')))
    .map((c, i) => ({ c, i }))
    .sort(
      (a, b) =>
        Number(blankSpecies(b.c.values)) - Number(blankSpecies(a.c.values)) ||
        Number(a.c.values.Collection_time ?? 9) - Number(b.c.values.Collection_time ?? 9) ||
        a.i - b.i,
    )
    .map(x => x.c),
)
const dayCreates = computed(() => pending.creates.filter(c => c.module === DAY_SHEET))

// ID_status follows the species typed in a new row (COMPLETE, or To_identify when emptied).
const speciesSeen = new Map<string, string>()
watch(
  () => monitoringCreates.value.map(c => [c.clientId, String(c.values.SPECIES ?? '')] as const),
  list => {
    let changed = false
    for (const [id, species] of list) {
      const before = speciesSeen.get(id)
      speciesSeen.set(id, species)
      // Only when the species itself changed: an ID_status set by hand stays.
      if (before === undefined || before === species) continue
      const create = pending.creates.find(c => c.clientId === id)
      if (!create) continue
      for (const [field, value] of Object.entries(identificationFollows(create.values))) {
        pending.updateCreate(id, field, value)
        changed = true
      }
    }
    if (changed) pending.touch()
  },
  { immediate: true },
)
// Photos of rows no longer pending are forgotten; saved rows find theirs in the stored track.
watch(
  () => pending.creates.length,
  (now, before) => {
    const ids = new Set(pending.creates.map(c => c.clientId))
    for (const id of Object.keys(photoIndex.value)) if (!ids.has(id)) delete photoIndex.value[id]
    // Saved rows: the stored captures are linked to them on the server's next read.
    if (before !== undefined && now < before) setTimeout(loadTracks, 1500)
  },
  { immediate: true },
)

// ------------------------------------------------------------ the selected row: photos, duplicate

const selectedId = ref<string | null>(null)
/** Stored captures by the sheet row they belong to (saved rows show the photos of their Wikiloc point). */
const savedCaptures = computed(() => {
  const out = new Map<string, { photos: string[]; text: string; label: string }>()
  for (const walk of tracks.value)
    for (const c of walk.captures)
      if (c.recordId)
        out.set(c.recordId, {
          photos: c.photos || [],
          text: c.text,
          label: [c.markId, c.species || t('sin especie'), formatMinutes(c.minutes)].filter(Boolean).join(' '),
        })
  return out
})
const captureOf = (id: string | null) => (id ? photoIndex.value[id] || savedCaptures.value.get(id) || null : null)
/** The capture shown beside the grid; ←/→ move through the new rows in grid order. */
const viewing = ref<string | null>(null)
watch(selectedId, id => {
  if (captureOf(id)) viewing.value = id
})
const navIds = computed(() => monitoringCreates.value.map(c => c.clientId).filter(id => photoIndex.value[id]))
const shown = computed(() => captureOf(viewing.value))
const shownIndex = computed(() => (viewing.value ? navIds.value.indexOf(viewing.value) : -1))
function stepCapture(by: number) {
  const n = navIds.value.length
  if (!n) return
  const at = shownIndex.value < 0 ? 0 : (shownIndex.value + by + n) % n
  viewing.value = navIds.value[at]
}

/** Enlarged photo; ←/→ go through every photo of the new rows, Esc closes. */
const viewer = ref<{ photos: { id: string; label: string; owner: string }[]; index: number } | null>(null)
function enlarge(id: string) {
  const ids = navIds.value.includes(viewing.value || '') ? navIds.value : viewing.value ? [viewing.value] : []
  const photos = ids.flatMap(owner =>
    (captureOf(owner)?.photos || []).map(p => ({ id: p, label: captureOf(owner)!.label, owner })),
  )
  viewer.value = {
    photos,
    index: Math.max(
      0,
      photos.findIndex(p => p.id === id),
    ),
  }
}
function stepPhoto(by: number) {
  if (!viewer.value) return
  const n = viewer.value.photos.length
  viewer.value.index = (viewer.value.index + by + n) % n
  viewing.value = viewer.value.photos[viewer.value.index].owner
}
function onKey(event: KeyboardEvent) {
  if (!viewer.value) return
  if (event.key === 'Escape') viewer.value = null
  else if (event.key === 'ArrowRight') stepPhoto(1)
  else if (event.key === 'ArrowLeft') stepPhoto(-1)
  else return
  event.preventDefault()
}
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))

/**
 * "Duplicar fila", for a capture typed by hand: a new row copying the selected
 * one, without what belongs to one butterfly only (time, mark, CAM, tube, weight).
 */
function duplicateRow() {
  const id = selectedId.value
  if (!id || !table.value) return
  const create = pending.creates.find(c => c.clientId === id)
  const row = create ? null : table.value.rows.find(r => r.id === id)
  if (!create && !row) return
  const values: Record<string, CellValue> = create
    ? { ...create.values }
    : Object.fromEntries(
        table.value.columns.filter(f => !row!.formulas.includes(f.key)).map(f => [f.key, pending.value(row!, f.key)]),
      )
  for (const field of createFormulas.value) delete values[field]
  delete values.Collection_time
  if (hasMark({ values } as TableRow)) delete values.FieldMark_ID
  for (const field of ['CAM_ID', 'Tube_1_id']) if (String(values[field] ?? '').trim() !== 'NA') delete values[field]
  if (typeof values.Butterfly_weight === 'number') delete values.Butterfly_weight
  for (const [k, v] of Object.entries(values)) if (v === null || v === '' || v === undefined) delete values[k]
  pending.addCreate(MODULE, 'copia', values)
  pending.touch()
  notify(t('Fila copiada (arriba): escribe la hora y la marca, o el CAM y el tubo'), 'success', 3000)
}

const chip = { ok: 'bg-brand-50 text-brand-700', info: 'bg-stone-100 text-stone-700', warn: 'bg-amber-100 text-amber-900' }
const minutes = (m: number | null) => formatMinutes(m) || '—'
const clean = (v: unknown) => (isBlank(v as CellValue) ? '—' : String(v))
const dayLabel = (iso: string) => formatSerial(isoToSerial(iso))
</script>

<template>
  <div class="flex h-full flex-col">
    <div class="toolbar">
      <label class="btn-primary cursor-pointer" :class="{ 'pointer-events-none opacity-50': !session.canEdit }">
        <FileUp :size="15" /> {{ $t('Elegir GPX de Wikiloc') }}
        <input type="file" accept=".gpx,application/gpx+xml,application/xml,text/xml" class="sr-only" @change="choose" />
      </label>
      <!-- Date and collector matter only while a walk is being reviewed. -->
      <template v-if="file">
        <label>
          <span class="field-label">{{ $t('Fecha') }}</span>
          <DateField v-model="date" class="field-input" />
        </label>
        <label class="min-w-52">
          <span class="field-label">
            {{ $t('Recolector') }}
            <span v-if="collectorFrom" class="font-normal text-stone-500">({{ $t(collectorFrom) }})</span>
          </span>
          <ChoiceField
            :model-value="collector"
            class="field-input"
            :class="{ 'border-amber-500': !collector }"
            :placeholder="$t('Elegir…')"
            :freetext="false"
            :options="collectorChoices"
            @update:model-value="pickCollector"
          />
        </label>
        <button
          v-if="!collector && lastCollector && people.includes(lastCollector)"
          class="btn mb-0.5"
          :title="$t('No se sabe quién hizo el recorrido: confirma')"
          @click="pickCollector(lastCollector)"
        >
          {{ $t('¿{name}? (el último usado)', { name: lastCollector }) }}
        </button>
        <p class="hint pb-1.5">
          {{ file.gpx.name || file.name }} · {{ $t('{n} puntos', { n: file.gpx.waypoints.length }) }}
          <template v-if="span"> · {{ minutes(span.start) }}–{{ minutes(span.end) }}</template>
          · {{ (trackLength(file.gpx.track) / 1000).toFixed(2) }} km
          <button class="ml-1 underline" @click="file = null">{{ $t('cerrar') }}</button>
        </p>
      </template>
    </div>

    <WikilocBar ref="bar" @reopened="showWaiting = true" />

    <div v-if="waiting.length || tracks.length" class="border-b border-stone-200 bg-brand-50 px-3 py-1.5 text-sm sm:px-4">
      <div class="flex flex-wrap items-center gap-2">
        <button
          v-if="waiting.length"
          class="inline-flex items-center gap-1 font-medium text-brand-700"
          :aria-expanded="showWaiting"
          @click="showWaiting = !showWaiting"
        >
          {{ $t('Desde Wikiloc, por revisar ({n})', { n: waiting.length }) }}
          <ChevronDown :size="14" class="sm:hidden" :class="{ 'rotate-180': showWaiting }" />
        </button>
        <button v-if="registered.length" class="btn-primary py-0.5" :disabled="busy" @click="registerAll">
          <MapPin :size="14" /> {{ $t('Pasar al mapa {n}', { n: registered.length })
          }}<span class="hidden sm:inline">&nbsp;{{ $t('ya registrados en la hoja') }}</span>
        </button>
        <button class="ml-auto text-xs text-stone-700 underline" :aria-expanded="showStored" @click="showStored = !showStored">
          {{ $t('Ya en el mapa ({n})', { n: tracks.length }) }}
        </button>
        <!-- Reviewers match every stored walk again there, and anyone of the team pairs the doubtful points. -->
        <button
          v-if="session.canEdit && tracks.length"
          class="text-xs text-stone-700 underline"
          :title="$t('Puntos cuyo emparejamiento con la hoja es dudoso, y lo que cambiaría al emparejar de nuevo')"
          @click="router.replace({ query: { vista: 'dudas' } })"
        >
          {{ isReviewer ? $t('Emparejar de nuevo y dudas') : $t('Dudas de emparejamiento') }}
        </button>
      </div>
      <div
        v-if="waiting.length"
        class="mt-1 max-h-32 flex-wrap gap-2 overflow-y-auto"
        :class="showWaiting ? 'flex' : 'hidden sm:flex'"
      >
        <span
          v-for="w in waiting"
          :key="w.id"
          class="inline-flex items-center gap-1 rounded-md border bg-white py-0.5 pr-0.5 pl-2"
          :class="file?.walk?.id === w.id ? 'border-brand-600' : 'border-stone-300'"
        >
          <button class="hover:underline" @click="openWalk(w)">
            {{ w.date ? dayLabel(w.date) : $t('{name} (sin día)', { name: w.recorded || w.name }) }}
            <template v-if="w.collector"> · {{ shortName(w.collector) }}</template> ·
            {{ $t('{n} puntos', { n: w.waypoints.length }) }}
            <template v-if="w.trackId"> · {{ $t('ya importado') }}</template>
          </button>
          <a :href="w.url" target="_blank" rel="noopener" class="btn-ghost" :title="$t('Abrir en Wikiloc')"
            ><ExternalLink :size="13"
          /></a>
          <button class="btn-ghost" :title="$t('Quitar de la lista')" @click="removeWalk(w)"><Trash2 :size="13" /></button>
        </span>
      </div>
      <!-- Walks on the map: review one again, or remove it (uploader, whoever brought it in, reviewers). -->
      <ul
        v-if="showStored"
        class="mt-1 max-h-48 divide-y divide-stone-200 overflow-y-auto rounded-md border border-stone-200 bg-white text-xs"
      >
        <li v-for="t in tracks.slice(0, storedCount)" :key="t.id" class="flex flex-wrap items-center gap-2 px-2 py-1">
          <span class="font-medium">{{ dayLabel(t.date) }}</span>
          <span>{{ t.collector ? shortName(t.collector) : $t('sin recolector') }}</span>
          <span class="text-stone-500">
            {{ $t('{n} puntos', { n: t.captures.length }) }} ·
            {{ $t('{n} con fila', { n: t.captures.filter(c => c.row).length }) }}
            <template v-if="t.captures.some(c => c.doubt)">
              ·
              <span class="text-amber-800">{{
                $t('{n} en Dudas', { n: t.captures.filter(c => c.doubt).length })
              }}</span></template
            >
            <template v-if="t.wikiloc"> · Wikiloc</template><template v-else> · GPX</template>
          </span>
          <span class="ml-auto flex gap-1">
            <button
              v-if="walkOf(t)"
              class="btn-ghost py-0.5"
              :title="$t('Volver a revisar este recorrido')"
              @click="reviewAgain(walkOf(t)!)"
            >
              <RotateCcw :size="13" /> {{ $t('Revisar de nuevo') }}
            </button>
            <a
              v-if="t.wikiloc"
              :href="t.wikiloc.url"
              target="_blank"
              rel="noopener"
              class="btn-ghost py-0.5"
              :title="$t('Abrir en Wikiloc')"
            >
              <ExternalLink :size="13" />
            </a>
            <button v-if="t.canDelete" class="btn-ghost py-0.5" :title="$t('Quitar del mapa')" @click="removeTrack(t)">
              <Trash2 :size="13" />
            </button>
          </span>
        </li>
        <li v-if="tracks.length > storedCount" class="px-2 py-1">
          <button class="underline" @click="storedCount += 40">{{ $t('Ver más') }}</button>
        </li>
      </ul>
    </div>

    <section v-if="file" class="max-h-[60%] shrink-0 overflow-auto border-b border-stone-200 bg-white">
      <p v-if="file.walk" class="bg-stone-50 px-3 py-1.5 text-xs text-stone-600">
        {{ $t('Leído de la página pública de Wikiloc: sin horas GPS del recorrido (SamplingDay_data no se completa).') }}
        <template v-if="!date">
          {{ $t('Escribe la fecha arriba')
          }}<template v-if="file.walk.recorded"> (Wikiloc: {{ file.walk.recorded }})</template>.</template
        >
      </p>
      <p v-if="dayRow?.broken" class="flex flex-wrap items-center gap-2 bg-amber-50 px-3 py-1.5 text-xs text-amber-900">
        {{
          $t('SamplingDay_data fila {n} parece este día ({initials}), pero su Date es {date}: no se añade otra fila.', {
            n: dayRow.row.row,
            initials,
            date: String(dayRow.row.values.Date),
          })
        }}
        <button class="btn py-0.5" @click="fixDayDate">{{ $t('Corregir su Date a {date}', { date: dayLabel(date) }) }}</button>
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
            :aria-label="$t('Incluir {name}', { name: c.text })"
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
            <th v-if="file.walk" class="px-2 py-1.5">{{ $t('Foto') }}</th>
            <th class="px-2 py-1.5">{{ $t('Punto') }}</th>
            <th class="px-2 py-1.5">{{ $t('Especie') }}</th>
            <th class="px-2 py-1.5">{{ $t('Sexo') }}</th>
            <th class="px-2 py-1.5">{{ $t('Hora') }}</th>
            <th class="px-2 py-1.5">{{ $t('Altura') }}</th>
            <th class="px-2 py-1.5">{{ $t('Clima') }}</th>
            <th class="px-2 py-1.5">{{ $t('Marca') }}</th>
            <th class="px-2 py-1.5">T</th>
            <th class="px-2 py-1.5">{{ $t('Revisión') }}</th>
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
                :aria-label="$t('Incluir {name}', { name: c.text })"
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
          <ListPlus :size="15" /> {{ $t('Añadir {n} filas a Collection_data', { n: included.length }) }}
        </button>
        <span v-if="file && !collector" class="text-xs text-amber-800">{{ $t('Elige el recolector arriba.') }}</span>
        <button v-if="gpxTrack" class="btn" :disabled="busy" @click="addPhotosToTrack">
          <ImagePlus :size="15" /> {{ $t('Solo añadir las fotos al GPX ya subido de este día') }}
        </button>
        <button
          class="btn"
          :disabled="!ready || busy"
          :title="$t('Para recorridos cuyas filas ya están en la hoja')"
          @click="onlyTrack"
        >
          <MapPin :size="15" /> {{ $t('Solo guardar el recorrido (mapa)') }}
        </button>
        <span class="hint">
          {{
            $t(
              'El transecto sale de la posición GPS; la marca con la misma especie y sexo cuenta como recaptura. Los preservados reciben el siguiente CAM y tubo. Nada se escribe en la hoja hasta pulsar Guardar.',
            )
          }}
        </span>
      </div>
    </section>

    <div class="flex flex-wrap items-center gap-x-2 gap-y-1 px-3 py-1 text-xs text-stone-600 sm:px-4">
      <template v-if="monitoringCreates.length">
        <span>{{ $t('{n} filas nuevas (verde), las sin especie primero', { n: monitoringCreates.length }) }}</span>
      </template>
      <span v-else class="hidden sm:inline">{{
        $t('Elige o comparte el GPX de Wikiloc, o pega el enlace de la ruta arriba.')
      }}</span>
      <span v-if="dayCreates.length">· {{ $t('{n} fila nueva en SamplingDay_data', { n: dayCreates.length }) }}</span>
      <span v-if="dayNote" :class="{ 'text-amber-800': dayNote.warn }">· {{ $t(dayNote.key, dayNote.vars) }}</span>
      <span class="hidden sm:inline">· {{ $t('últimos {n} registros', { n: recentCount }) }}</span>
      <button class="underline" @click="recentCount += 15">{{ $t('Cargar más') }}</button>
      <button
        class="btn ml-auto py-0.5"
        :disabled="!selectedId || !session.canEdit"
        :title="$t('Copia la fila elegida en una nueva, sin hora, marca, CAM ni tubo')"
        @click="duplicateRow"
      >
        <Copy :size="14" /> {{ $t('Duplicar fila') }}
      </button>
    </div>
    <div class="flex min-h-0 flex-1 flex-col sm:flex-row">
      <!-- The chosen row's Wikiloc photos and note: beside the grid on computers, a strip above it on phones. -->
      <aside
        v-if="shown"
        class="order-first flex shrink-0 gap-2 border-b border-stone-200 bg-stone-50 p-2 text-xs sm:order-last sm:w-64 sm:flex-col sm:overflow-y-auto sm:border-b-0 sm:border-l"
      >
        <div class="flex items-center gap-1 sm:w-full">
          <button class="btn-ghost p-1" :title="$t('Captura anterior')" :disabled="navIds.length < 2" @click="stepCapture(-1)">
            <ChevronLeft :size="16" />
          </button>
          <span class="min-w-0 flex-1 truncate text-center font-medium">{{ shown.label }}</span>
          <button class="btn-ghost p-1" :title="$t('Captura siguiente')" :disabled="navIds.length < 2" @click="stepCapture(1)">
            <ChevronRight :size="16" />
          </button>
          <button class="btn-ghost p-1" :title="$t('Cerrar')" @click="viewing = null"><X :size="14" /></button>
        </div>
        <div class="flex gap-1 overflow-x-auto sm:flex-wrap">
          <button v-for="id in shown.photos" :key="id" class="shrink-0" :title="$t('Ver grande')" @click="enlarge(id)">
            <img
              :src="photoUrl(id)"
              alt=""
              loading="lazy"
              class="h-20 w-20 rounded object-cover hover:ring-2 hover:ring-brand-600 sm:h-28 sm:w-28"
            />
          </button>
          <span v-if="!shown.photos.length" class="text-stone-500">{{ $t('Sin fotos en Wikiloc') }}</span>
        </div>
        <p class="line-clamp-3 min-w-0 text-stone-600 sm:line-clamp-none" :title="shown.text">
          {{ $t('Nota: {note}', { note: shown.text }) }}
        </p>
      </aside>
      <div class="min-h-0 min-w-0 flex-1">
        <p v-if="!table" class="p-6 text-stone-500">{{ $t('Cargando Collection_data…') }}</p>
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
          @select="id => (selectedId = id)"
          @remove-create="
            id => {
              pending.removeCreate(id)
              pending.touch()
            }
          "
        />
      </div>
    </div>

    <div
      v-if="viewer && viewer.photos.length"
      class="fixed inset-0 z-[2000] flex flex-col bg-black/90 text-white"
      role="dialog"
      aria-modal="true"
      @click.self="viewer = null"
    >
      <div class="flex items-center gap-3 px-4 py-2 text-sm">
        <span class="font-medium">{{ viewer.photos[viewer.index].label }}</span>
        <span class="text-stone-300">{{ $t('foto {n} de {total}', { n: viewer.index + 1, total: viewer.photos.length }) }}</span>
        <button class="ml-auto rounded p-1 hover:bg-white/10" :title="$t('Cerrar (Esc)')" @click="viewer = null">
          <X :size="20" />
        </button>
      </div>
      <div class="relative flex min-h-0 flex-1 items-center justify-center" @click.self="viewer = null">
        <button
          v-if="viewer.photos.length > 1"
          class="absolute left-2 rounded-full bg-white/10 p-2 hover:bg-white/20"
          :title="$t('Anterior (←)')"
          @click="stepPhoto(-1)"
        >
          <ChevronLeft :size="24" />
        </button>
        <img :src="photoUrl(viewer.photos[viewer.index].id)" alt="" class="max-h-full max-w-full object-contain" />
        <button
          v-if="viewer.photos.length > 1"
          class="absolute right-2 rounded-full bg-white/10 p-2 hover:bg-white/20"
          :title="$t('Siguiente (→)')"
          @click="stepPhoto(1)"
        >
          <ChevronRight :size="24" />
        </button>
      </div>
    </div>
  </div>
</template>
