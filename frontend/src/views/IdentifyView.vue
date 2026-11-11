<script setup lang="ts">
/**
 * Identificar: a butterfly from a photo, on this phone, also without signal (DESIGN in
 * .work/offline-id-tab-20261103/DESIGN.md). The models run in the page (lib/offlineEngine.ts); the wording of a
 * result is lib/identifyResult.ts; reference photos lib/identifyRefs.ts; the person's choices stay on the phone
 * (lib/identifyLog.ts). The team sees the flash-frozen gap of the candidate being compared (StockPanel).
 * Without an account: the models from Hugging Face, the static field photos, no request to the team's API (no
 * inventory, no species-photos), and the inventory a team session left on the phone is removed.
 * The team also sees an experimental sex estimate for in-hand photos (SexEstimate, lib/sexModel.ts); its model is
 * the team's (signed in only) and is removed from the phone when signed out.
 * Compare and the search show the photos of the person's country first (lib/photoCountry.ts): the photo's GPS
 * position when it has one, else the side of the Andes (Ecuador) or Descargas' country; then the neighbours, then the
 * rest; a subspecies the model names puts its range first.
 * Behind VITE_CONTRIB=1, the team can also send the photo with their labels to train the AI (ContributeCard,
 * lib/contribOutbox.ts: kept on the phone until there is signal; «Mis envíos» lists and deletes them).
 */
import { computed, onBeforeUnmount, onMounted, ref, shallowRef, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { Camera, Check, ChevronDown, Download, ImageUp, RotateCcw, ScanSearch, Search, TriangleAlert, WifiOff } from 'lucide-vue-next'
import { effectiveMode, identifyOnServer, jpegBase64, onlineError, photoCanvas, serverRanking, squareCrop, type Mode, type ServerAnswer } from '../lib/identifyOnline'
import RefCompare from '../components/identify/RefCompare.vue'
import StockPanel from '../components/identify/StockPanel.vue'
import IdentifyAbout from '../components/identify/IdentifyAbout.vue'
import DownloadsPanel, { type ModelRow, type SexRow } from '../components/identify/DownloadsPanel.vue'
import TaxonSearch from '../components/identify/TaxonSearch.vue'
import { usePacks } from '../lib/usePacks'
import { mbText, numText, packHint, scopeSize, toDownload, type PackScope } from '../lib/photoPacks'
import { regionLabel, taxaLabel } from '../lib/usePacks'
import SexEstimate, { type SexState } from '../components/identify/SexEstimate.vue'
import FieldSexEstimate from '../components/identify/FieldSexEstimate.vue'
import { fieldSexState as fieldSexOf, type FieldSexState } from '../lib/fieldSex'
import { SEX_FILE, sexApplies } from '../lib/sexModel'
import { PUBLIC_FILES } from '../lib/publicModels'
import { ITHOMIINI_GENERA } from '../lib/ithomiiniGenera'
import { MODEL_ROLES, sameFile, storedName } from '../lib/offlineId'
import { appRoot } from '../lib/appRoot'
import { t } from '../lib/i18n'
import { chooseModel, createEngine, KINDS, type Listing, type ModelFile } from '../lib/offlineEngine'
import { leafScores, speciesSides, subspeciesBySpecies, sumSpecies, type Head, type ModelKind, type Side } from '../lib/offlineId'
import { takePhoto, type Photo } from '../lib/offlinePhoto'
import { epithet, groupByGenus, headline, higherRanks, isIthomiini, percent, rank, shortName, SSP_MIN, SSP_SHARE, subspeciesHint, TRIBE_RUNG, type Candidate, type Higher, type StockDetail, type Taxonomy } from '../lib/identifyResult'
import { loadTaxonomy } from '../lib/taxonomy'
import { orderRefs, referencePhotos, savedPhotos, taxonRange, type Refs } from '../lib/identifyRefs'
import { photoCountry, warmShapes, type Place as PhotoPlace } from '../lib/photoCountry'
import { readLog, writeLog, clearLog, type LogEntry } from '../lib/identifyLog'
import type { FrozenStock } from '../lib/frozenStock'
import { useSession } from '../stores/session'
declare const __BUILD_ID__: string
import ContributeCard, { type Labels, type Outcome } from '../components/identify/ContributeCard.vue'
import MyContributions from '../components/identify/MyContributions.vue'
import { CONSENT_VERSION, contribFor, GPS_DECIMALS } from '../lib/contribConfig'
import { AlreadyListed, createOutbox, httpSend, OutboxFull, type Item, type Outbox } from '../lib/contribOutbox'
import { askPersistence, browserLock, idbStore } from '../lib/contribIdb'
import { photoMeta, roundGps } from '../lib/exifMeta'
import { headSource, listSource, type TaxonSource } from '../lib/taxonPick'

const session = useSession()

// ---------------------------------------------------------------- the three places: identify, search, downloads
const route = useRoute()
const router = useRouter()
type Place = 'identify' | 'search' | 'downloads'
const place = computed<Place>(() => (route.query.vista === 'descargas' ? 'downloads' : route.query.vista === 'buscar' ? 'search' : 'identify'))
function go(p: Place) {
  if (p === place.value) return
  const { vista: _v, taxon: _t, foto: _f, ...rest } = route.query
  router.push({ query: { ...rest, ...(p === 'identify' ? {} : { vista: p === 'downloads' ? 'descargas' : 'buscar' }) } })
  window.scrollTo({ top: 0 })
}
const PLACES: { id: Place; label: string; icon: typeof Camera }[] = [
  { id: 'identify', label: 'Identificar', icon: ScanSearch },
  { id: 'search', label: 'Buscar', icon: Search },
  { id: 'downloads', label: 'Descargas', icon: Download },
]
const pageFiles = () => [
  location.href.split('#')[0],
  ...[...document.querySelectorAll<HTMLScriptElement | HTMLLinkElement>('script[src], link[rel=stylesheet], link[rel=modulepreload], link[rel=manifest]')].map(
    e => ('src' in e && e.src) || (e as HTMLLinkElement).href,
  ),
].filter(Boolean)
const signedIn = () => !!session.user
const engine = createEngine(new URL('prueba-offline/', appRoot()).href, pageFiles, { signedIn })
/** The session known (the header asks for it; offline the service worker answers, or it gives up). */
const sessionKnown = () =>
  session.ready
    ? Promise.resolve()
    : new Promise<void>(done => {
        const stop = watch(
          () => session.ready,
          v => v && (stop(), done()),
        )
        setTimeout(() => (stop(), done()), 8000)
      })

const CHOSEN = 'prueba-offline:model' // shared with the test page
const SIDE = 'identify:side'
const MODE = 'identify:mode'
const LAST = 'identify:last'
const store = {
  get: (k: string) => {
    try {
      return localStorage.getItem(k)
    } catch {
      return null
    }
  },
  set: (k: string, v: string) => {
    try {
      localStorage.setItem(k, v)
    } catch {
      /* private mode */
    }
  },
}
const mb = (b: number) => Math.max(1, Math.round(b / 1048576))
const roughMb = (b: number) => (b > 200 * 1048576 ? Math.round(b / 10485760) * 10 : Math.max(1, Math.round(b / 5242880) * 5))

// ---------------------------------------------------------------- models: list, readiness, download

const online = ref(navigator.onLine)
const listing = ref<Listing | null>(null)
const files = computed<ModelFile[] | null>(() => (listing.value && 'files' in listing.value ? listing.value.files : null))
const ready = ref<Record<ModelKind, boolean>>({ small: false, full: false })
const checked = ref(false)
const wanted = ref<ModelKind>((store.get(CHOSEN) as ModelKind | null) === 'full' ? 'full' : 'small')
const active = computed(() => chooseModel(wanted.value, ready.value))
/** Each model's files are on the phone (perhaps an older version than the server lists). */
const stored = ref<Record<ModelKind, boolean>>({ small: false, full: false })
const download = ref<{ kind: ModelKind; done: number; total: number; retrying?: string } | null>(null)
const dlError = ref<string | null>(null)
const space = ref<{ free: number } | null>(null)
const sizes = ref<Record<ModelKind, { plain: number; wire: number; total: number } | null>>({ small: null, full: null })
const isIos = /iPad|iPhone|iPod/.test(navigator.userAgent) || (navigator.platform === 'MacIntel' && navigator.maxTouchPoints > 1)
const installed = matchMedia('(display-mode: standalone)').matches || (navigator as Navigator & { standalone?: boolean }).standalone === true

async function refresh() {
  online.value = navigator.onLine
  listing.value = await engine.list()
  ready.value = await engine.ready(files.value)
  let s = await engine.stored()
  // Only the head's JSON is new (schema 2, the subspecies' names, 3 Nov 2026): taken by itself (under 1 MB), so a
  // phone that has the model does not ask for a download. Once per page; on failure the button stays.
  if (files.value && !headTried) {
    const list = files.value
    for (const k of KINDS) {
      const need = list.filter(f => MODEL_ROLES[k].includes(f.role) && !sameFile(s?.files[f.name], f))
      if (ready.value[k] || !need.length || need.some(f => f.role !== 'head-meta') || sexDownload.value) continue
      headTried = true
      head.value = null
      await engine.download(k, list, () => {}).catch(() => null)
      ready.value = await engine.ready(list)
      s = await engine.stored()
    }
    // A result on screen is ranked again with the new JSON.
    if (headTried && result.value) head.value = await engine.loadHead().catch(() => null)
  }
  for (const k of KINDS) sizes.value[k] = files.value ? engine.toDownload(k, files.value, s) : null
  for (const k of KINDS) stored.value[k] = !!s && MODEL_ROLES[k].every(r => storedName(s!.files, r) != null)
  sexStored.value = !!s && storedName(s.files, 'sex') != null
  sexReady.value = !!session.user && (await engine.sexReady(files.value))
  sexSize.value = session.user && files.value ? engine.sexToDownload(files.value, s).plain : null
  const est = await navigator.storage?.estimate?.().catch(() => null)
  space.value = est?.quota ? { free: Math.max(0, (est.quota ?? 0) - (est.usage ?? 0)) } : null
  checked.value = true
  if (ready.value.small || ready.value.full) {
    // Models from the test page: this page and its runtime are kept now (online) before it says it works offline.
    pageOffline.value = await engine.pageOffline()
  }
}
let headTried = false
/** Whether this page opens without signal: null while it is being checked (and kept, online). */
const pageOffline = ref<boolean | null>(null)

const label = (k: ModelKind) => (k === 'small' ? t('Rápido y liviano') : t('Más preciso'))
/** The public copies' sizes: what the line says before the list is known (offline). */
const publicSize = (k: ModelKind) => PUBLIC_FILES.filter(f => MODEL_ROLES[k].includes(f.role)).reduce((a, f) => a + f.bytes, 0)
/** Minutes on a 10 Mbit/s connection (a fair home or campus wifi), at least 1. */
const minutes = (bytes: number) => Math.max(1, Math.round((bytes * 8) / 10e6 / 60))
function sizeLine(k: ModelKind) {
  const s = sizes.value[k] ?? { plain: 1, wire: publicSize(k), total: publicSize(k) }
  if (!s.plain) return t('Ya está en este teléfono.')
  return k === 'small'
    ? t('Descarga unos {n} MB y ocupa unos {m} MB. Un poco menos preciso.', { n: roughMb(s.wire), m: roughMb(s.total) })
    : t('Descarga unos {n} MB (unos {min} minutos con un wifi de 10 Mbit/s; con datos móviles, mucho más). Ocupa unos {m} MB en el teléfono.', {
        n: roughMb(s.wire),
        min: minutes(s.wire),
        m: roughMb(s.total),
      })
}
const fromHf = computed(() => !!listing.value && 'source' in listing.value && listing.value.source === 'public')
/** The first screen's one-tap download (the fast model and the photos announced with it): free space it needs. */
const firstLacksSpace = computed(() => {
  const s = sizes.value.small
  const need = (s?.plain ?? 0) + (firstPack.value?.bytes ?? 0)
  return s && space.value && need > space.value.free ? { need: roughMb(need), free: mb(space.value.free) } : null
})
const lacksSpace = computed(() => {
  const s = sizes.value[wanted.value]
  return s && space.value && s.plain > space.value.free ? { need: roughMb(s.plain), free: mb(space.value.free) } : null
})

async function startDownload(kind: ModelKind = wanted.value) {
  if (sexDownload.value || download.value) return
  wanted.value = kind
  dlError.value = null
  // Asked inside the tap (Chrome weighs the gesture): the phone then does not clear the models when short of space.
  navigator.storage?.persist?.().catch(() => null)
  if (!files.value) await refresh()
  if (!files.value) {
    dlError.value =
      listing.value && 'error' in listing.value && listing.value.error === 'server'
        ? t('El servidor no respondió. Pruebe de nuevo en un momento.')
        : t('Hace falta conexión a internet para descargar.')
    return
  }
  download.value = { kind, done: 0, total: 1 }
  try {
    await engine.download(kind, files.value, (done, total, retrying) => (download.value = { kind, done, total, retrying }))
    store.set(CHOSEN, kind)
  } catch (err) {
    const m = (err as Error).message
    dlError.value =
      m === 'changed-on-server'
        ? t('El modelo cambió en el servidor: recargue la página y descargue de nuevo.')
        : m === 'missing-on-server'
          ? t('Este modelo no está disponible por ahora.')
          : /quota/i.test(m)
            ? t('No hay espacio suficiente en el teléfono.')
            : t('La descarga se cortó. Toque Descargar para seguir donde quedó.')
  } finally {
    download.value = null
    await refresh()
  }
}

async function removeModel(kind: ModelKind) {
  if (!confirm(t('¿Borrar este modelo del teléfono? Para volver a usarlo hay que descargarlo otra vez.'))) return
  await engine.removeModel(kind)
  // A result read by this model stays on screen until the next photo; a restored one needs a model.
  if (!KINDS.some(k => k !== kind && ready.value[k])) {
    result.value = null
    head.value = null
    try {
      sessionStorage.removeItem(LAST)
    } catch {
      /* nothing kept */
    }
  }
  await refresh()
}
function useModel(kind: ModelKind) {
  wanted.value = kind
  store.set(CHOSEN, kind)
}
/** The Descargas place's rows (DownloadsPanel). */
const modelRows = computed<ModelRow[]>(() =>
  KINDS.map(k => {
    const s = sizes.value[k] ?? { plain: publicSize(k), wire: publicSize(k), total: publicSize(k) }
    return {
      kind: k,
      label: label(k),
      ready: ready.value[k],
      outdated: !ready.value[k] && stored.value[k] && !!files.value,
      downloadMb: roughMb(s.wire || s.plain || publicSize(k)),
      takesMb: roughMb(s.total),
      recommended: k === 'small',
    }
  }),
)

// ---------------------------------------------------------------- side of the Andes

type SideChoice = 'none' | 'East' | 'West'
const side = ref<SideChoice>(((s: string | null) => (s === 'East' || s === 'West' ? s : 'none'))(store.get(SIDE)))
watch(side, v => store.set(SIDE, v))
const sideValue = computed<Side>(() => (side.value === 'none' ? null : side.value))
// ---------------------------------------------------------------- where the photo is read: the server or this phone

/** The person's choice on this device (null: none yet; online then when it can be). */
const modeChoice = ref<Mode | null>(((m: string | null) => (m === 'online' || m === 'phone' ? m : null))(store.get(MODE)))
const mode = computed<Mode>(() => effectiveMode(modeChoice.value, !!session.user, online.value))
function chooseMode(m: Mode) {
  if (m === 'online' && (!session.user || !online.value)) return
  modeChoice.value = m
  store.set(MODE, m)
}
/** What can read a photo now: the server, or a model on the phone. */
const canIdentify = computed(() => mode.value === 'online' || anyReady.value)

const SIDE_OPTIONS: { value: SideChoice; label: string }[] = [
  { value: 'none', label: 'No sé' },
  { value: 'East', label: 'Este (Amazonía)' },
  { value: 'West', label: 'Oeste (Costa)' },
]

// ---------------------------------------------------------------- a photo

interface Result {
  at: string
  /** Who read it: this phone's model (`feature`, `kind`) or the server (`server`, `photo`, `side`). */
  source: Mode
  feature?: Float32Array
  crop: string
  status: string
  kind?: ModelKind
  ms: number
  server?: ServerAnswer
  /** The smaller photo sent (base64 JPEG): sent again when the side changes. */
  photo?: string
  side?: Side
  /** The sex estimate (team only): the model's logit, or why it does not apply. */
  sex?: { z: number } | { na: 'nowings' }
  /** The country of the photo's GPS position (ISO2), when it has one in the Neotropics. */
  country?: string | null
}
const result = shallowRef<Result | null>(null)
const head = shallowRef<Head | null>(null)
const busy = ref<'loading' | 'sending' | 'analysing' | null>(null)
const runError = ref<string | null>(null)
const firstRun = ref(true)
const camera = ref<HTMLInputElement | null>(null)
const picker = ref<HTMLInputElement | null>(null)
const sideControl = ref<HTMLElement | null>(null)
function focusSide() {
  sideControl.value?.scrollIntoView({ behavior: 'smooth', block: 'center' })
  sideControl.value?.querySelector<HTMLElement>('[aria-checked=true]')?.focus({ preventScroll: true })
}

function cropUrl(rgb: Uint8Array) {
  const c = document.createElement('canvas')
  c.width = c.height = 224
  const ctx = c.getContext('2d')!
  const im = ctx.createImageData(224, 224)
  for (let i = 0; i < 224 * 224; i++) {
    im.data[i * 4] = rgb[i * 3]
    im.data[i * 4 + 1] = rgb[i * 3 + 1]
    im.data[i * 4 + 2] = rgb[i * 3 + 2]
    im.data[i * 4 + 3] = 255
  }
  ctx.putImageData(im, 0, 0)
  return c.toDataURL('image/jpeg', 0.85)
}

async function identify(photo: Photo) {
  if (mode.value === 'online') return identifyOnline(photo)
  const kind = active.value
  runError.value = null
  if (!kind) return
  try {
    const country = await photoCountry(photo.bytes).catch(() => null)
    if (!engine.loaded(kind)) {
      busy.value = 'loading'
      await engine.load(kind)
    }
    busy.value = 'analysing'
    const out = await engine.run(photo, kind)
    head.value = engine.head()
    firstRun.value = false
    const at = new Date().toISOString()
    lastPhoto.value = { at, photo, cut: { square: out.square, status: out.status } }
    result.value = { at, source: 'phone', feature: out.feature, crop: cropUrl(out.square), status: out.status, kind, ms: out.ms, country }
    compared.value = null
    pick.value = null
    keepLast()
    window.scrollTo({ top: 0, behavior: 'smooth' })
    estimateSex()
  } catch (err) {
    const m = String((err as Error)?.message ?? err)
    runError.value = /memory|allocation|out of/i.test(m)
      ? t('El teléfono se quedó sin memoria. Cierre otras apps y pruebe de nuevo, o use el modelo Rápido y liviano.')
      : m === 'not-downloaded'
        ? t('Faltan partes del modelo en el teléfono: descárguelo de nuevo.')
        : t('No se pudo analizar la foto. Pruebe con otra.')
  } finally {
    busy.value = null
  }
}
async function identifyOnline(photo: Photo) {
  runError.value = null
  busy.value = 'sending'
  try {
    const canvas = await photoCanvas(photo.blob).catch(() => null)
    if (!canvas) {
      runError.value = t('No se pudo leer la foto. Pruebe con otra.')
      return
    }
    const sent = jpegBase64(canvas)
    const country = await photoCountry(photo.bytes).catch(() => null)
    const side = sideValue.value
    const started = performance.now()
    const out = await identifyOnServer(sent, side)
    firstRun.value = false
    const at = new Date().toISOString()
    lastPhoto.value = { at, photo }
    result.value = {
      at,
      source: 'online',
      crop: squareCrop(canvas, out.wingBox),
      status: out.crop ?? 'success',
      ms: Math.round(performance.now() - started),
      server: out,
      photo: sent,
      side,
      country,
    }
    compared.value = null
    pick.value = null
    keepLast()
    window.scrollTo({ top: 0, behavior: 'smooth' })
    estimateSex()
  } catch (err) {
    runError.value = onlineError(err)
  } finally {
    busy.value = null
    sideChangedMeanwhile()
  }
}
/** A server result read again with the side now chosen (the server applies the side); on failure the side goes back. */
async function rereadOnline() {
  const r = result.value
  // Only while online is in use: otherwise the side is kept for the next photo and the old result stays as it is.
  if (!r || r.source !== 'online' || !r.photo || r.side === sideValue.value || busy.value || mode.value !== 'online') return
  const side = sideValue.value
  runError.value = null
  busy.value = 'analysing'
  try {
    const out = await identifyOnServer(r.photo, side)
    if (result.value !== r) return
    result.value = { ...r, server: out, status: out.crop ?? r.status, side }
    keepLast()
  } catch (err) {
    runError.value = onlineError(err)
    sideFromResult(r)
  } finally {
    busy.value = null
    sideChangedMeanwhile()
  }
}
/** The side changed while the server was reading: read again with the side now chosen. */
function sideChangedMeanwhile() {
  const r = result.value
  if (r?.source === 'online' && r.side !== sideValue.value) rereadOnline()
}
function sideFromResult(r: Result) {
  const back: SideChoice = r.side === 'East' || r.side === 'West' ? r.side : 'none'
  if (side.value !== back) side.value = back
}
function clearResult() {
  result.value = null
  lastPhoto.value = null
  sexError.value = null
  compared.value = null
  pick.value = null
  try {
    sessionStorage.removeItem(LAST)
  } catch {
    /* nothing kept */
  }
}
// A new photo replaces the last result at once: a failed one must not leave the previous butterfly on screen.
const onPhoto = (e: Event) => {
  clearResult()
  runError.value = null
  return takePhoto(e.target as HTMLInputElement, identify, () => (runError.value = t('No se pudo leer la foto. Si está en la nube (Google Fotos), descárguela primero o use Tomar foto.')))
}

// A photo pasted (Ctrl+V / ⌘V) or dropped on the page: the same path as «Elegir foto».
const isMac = /Mac|iPhone|iPad/.test(navigator.platform || navigator.userAgent)
const finePointer = matchMedia('(pointer: fine)').matches
const dragging = ref(false)
function imageFile(list: DataTransfer | null): File | null {
  if (!list) return null
  for (const item of Array.from(list.items ?? [])) if (item.kind === 'file' && item.type.startsWith('image/')) return item.getAsFile()
  return Array.from(list.files ?? []).find(f => f.type.startsWith('image/')) ?? null
}
function photoFile(file: File) {
  if (!canIdentify.value || busy.value || place.value !== 'identify') return
  onPhoto({ target: { files: [file], value: '' } } as unknown as Event)
}
function onPaste(e: ClipboardEvent) {
  const file = imageFile(e.clipboardData)
  if (!file) return // text pasted into a field stays text
  e.preventDefault()
  photoFile(file)
}
const hasFiles = (e: DragEvent) => Array.from(e.dataTransfer?.types ?? []).includes('Files')
function onDragOver(e: DragEvent) {
  if (!hasFiles(e)) return
  e.preventDefault() // else the browser opens the image instead of the page
  if (canIdentify.value && place.value === 'identify') dragging.value = true
}
function onDrop(e: DragEvent) {
  if (!hasFiles(e)) return
  e.preventDefault()
  dragging.value = false
  const file = imageFile(e.dataTransfer)
  if (file) photoFile(file)
}
const onDragLeave = (e: DragEvent) => {
  if (!e.relatedTarget) dragging.value = false
}

// ---------------------------------------------------------------- the result

// On the phone: every leaf's score (after the side's prior): the species sum them, the subspecies are the subspecies
// leaves. From the server: its lists (it applied the side).
const fromServer = computed(() => (result.value?.source === 'online' && result.value.server ? serverRanking(result.value.server) : null))
const leafP = computed(() =>
  result.value?.source === 'phone' && result.value.feature && head.value ? leafScores(head.value, result.value.feature, sideValue.value) : null,
)
const scores = computed(() => (leafP.value && head.value ? sumSpecies(head.value, leafP.value) : null))
const ranked = computed(() =>
  fromServer.value ? { species: fromServer.value.species, genus: fromServer.value.genus } : scores.value && head.value ? rank(scores.value, head.value.meta.species, 5) : null,
)
/** The candidates' subspecies (most probable first), by species name; empty with an older head (no names). */
const subspecies = computed(() => {
  if (fromServer.value) return fromServer.value.ssp
  const out = new Map<string, Candidate[]>()
  const h = head.value
  if (!h || !leafP.value || !ranked.value) return out
  const idx = ranked.value.species.map(c => h.meta.species.indexOf(c.name))
  for (const [i, list] of subspeciesBySpecies(h, leafP.value, idx)) out.set(h.meta.species[i], list)
  return out
})
/** The subspecies shown in a candidate's list: those of at least 1 %, the rest counted. */
const sspShown = (name: string) => (subspecies.value.get(name) ?? []).filter(c => c.p >= SSP_MIN)
const sspRest = (name: string) => (subspecies.value.get(name)?.length ?? 0) - sspShown(name).length
const openSsp = ref<string | null>(null)
const grouped = computed(() => (ranked.value ? groupByGenus(ranked.value.species, ranked.value.genus) : null))
const top = computed(() => ranked.value?.species[0])
/** With the flag (TRIBE_RUNG): the head's tribe and subfamily per species, for the phone's totals. */
const taxonomy = shallowRef<Taxonomy | null>(null)
watch(
  () => head.value?.meta.head_sha256,
  sha => {
    if (TRIBE_RUNG && sha && !taxonomy.value) loadTaxonomy(sha).then(t => (taxonomy.value = t))
  },
  { immediate: true },
)
const higher = computed<Higher | null>(() => {
  if (!TRIBE_RUNG) return null
  if (fromServer.value) return fromServer.value.higher
  return scores.value && head.value && taxonomy.value ? higherRanks(scores.value, head.value.meta.species, taxonomy.value) : null
})
const said = computed(() => (ranked.value ? headline(top.value, ranked.value.genus, higher.value) : null))
/**
 * The Ithomiini badge under the headline: from the top species, except under a tribe answer (the tribe says it) or a
 * subfamily answer (Danainae holds Ithomiini and others: neither the badge nor «No parece una Ithomiini.»).
 */
const ithomiiniBadge = computed<boolean | null>(() => {
  if (said.value?.kind === 'tribe') return said.value.name === 'Ithomiini'
  if (said.value?.kind === 'subfamily') return null
  return isIthomiini(top.value?.name ?? '', ITHOMIINI_GENERA)
})
const sspHint = computed(() => {
  const hint = top.value ? subspeciesHint(said.value, top.value, subspecies.value.get(top.value.name)) : null
  // The server lists only its 5 most probable subspecies: one listed does not mean the model knows only one.
  return hint && fromServer.value ? { ...hint, only: false } : hint
})
/** The line's score: of the whole photo when the species is clear; «if it is this species», its share of the species. */
const sspPercent = (p: number) =>
  sspHint.value?.kind === 'if' && top.value?.p ? t('{p} de esa especie', { p: percent(p / top.value.p) }) : percent(p)
const compared = ref<string | null>(null)
const pick = ref<string | null>(null)
const showing = computed(() => compared.value ?? top.value?.name ?? '')
const openGenus = ref<string | null>(null)
watch(grouped, g => (openGenus.value = g?.groups[0]?.genus ?? null))
watch(top, c => (openSsp.value = c?.name ?? null))
/** The frozen panel's subspecies: the closest one of the species compared, when it carries enough of its score. */
const stockFocus = computed(() => {
  const c = ranked.value?.species.find(x => x.name === showing.value)
  const s = c && subspecies.value.get(c.name)?.[0]
  return s && s.p >= SSP_MIN && s.p >= SSP_SHARE * c.p ? { epithet: epithet(s.name), p: s.p } : null
})
const recordedSides = computed<Map<string, { East: boolean; West: boolean }>>(() =>
  result.value?.source === 'online'
    ? new Map(Object.entries(result.value.server?.recorded ?? {}))
    : head.value
      ? speciesSides(head.value)
      : new Map(),
)
const noWings = computed(() => !!result.value && result.value.status !== 'success')

function nextCandidate() {
  const list = ranked.value?.species ?? []
  const i = list.findIndex(c => c.name === showing.value)
  compared.value = list[(i + 1) % list.length]?.name ?? null
}
function choose(name: string) {
  pick.value = pick.value === name ? null : name
  keepLast()
  if (result.value && top.value)
    log.value = writeLog({ at: result.value.at, thumb: smallThumb.value, ai: top.value.name, p: top.value.p, pick: pick.value, side: sideValue.value }, log.value)
}

// The result in the log once ranked (a smaller copy of the square).
const smallThumb = ref('')
watch(result, r => {
  if (!r) return
  const img = new Image()
  img.onload = () => {
    const c = document.createElement('canvas')
    c.width = c.height = 72
    c.getContext('2d')!.drawImage(img, 0, 0, 72, 72)
    smallThumb.value = c.toDataURL('image/jpeg', 0.7)
    if (top.value) log.value = writeLog({ at: r.at, thumb: smallThumb.value, ai: top.value.name, p: top.value.p, pick: pick.value, side: sideValue.value }, log.value)
  }
  img.src = r.crop
})

// Kept for this browser session: going to another tab and back shows the result again.
function keepLast() {
  const r = result.value
  if (!r) return
  try {
    let feature: string | undefined
    if (r.feature) {
      const f = new Uint8Array(r.feature.buffer.slice(0))
      let bin = ''
      for (let i = 0; i < f.length; i++) bin += String.fromCharCode(f[i])
      feature = btoa(bin)
    }
    sessionStorage.setItem(LAST, JSON.stringify({ ...r, feature, compared: compared.value, pick: pick.value }))
  } catch {
    /* no session storage: not restored */
  }
}
async function restoreLast() {
  try {
    const raw = sessionStorage.getItem(LAST)
    if (!raw) return
    const v = JSON.parse(raw)
    if (v.source === 'online') {
      // A server result: shown as it was, its side with it (a session that ended leaves nothing of the team's).
      if (!session.user) return sessionStorage.removeItem(LAST)
      result.value = v
      sideFromResult(v)
    } else {
      if (!anyReady.value) return
      const bin = atob(v.feature)
      const bytes = new Uint8Array(bin.length)
      for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i)
      head.value = await engine.loadHead()
      result.value = { ...v, source: 'phone', feature: new Float32Array(bytes.buffer) }
    }
    compared.value = v.compared ?? null
    pick.value = v.pick ?? null
  } catch {
    /* gone with the models: nothing to show */
  }
}
watch(compared, keepLast)
// A server result is read again with the new side (the phone's is ranked again by itself).
watch(sideValue, () => rereadOnline())
// Back to online with a side changed meanwhile: the server's result read again with it.
watch(mode, m => m === 'online' && sideChangedMeanwhile())
// The log names what the screen shows first: a change of side rewrites the entry (the same time).
watch(sideValue, () => {
  if (result.value && top.value && smallThumb.value)
    log.value = writeLog({ at: result.value.at, thumb: smallThumb.value, ai: top.value.name, p: top.value.p, pick: pick.value, side: sideValue.value }, log.value)
})

// ---------------------------------------------------------------- the sex estimate (team only, experimental)

const sexReady = ref(false)
/** Bytes still to download for it (the detector may be on the phone already); null: not known. */
const sexSize = ref<number | null>(null)
const sexDownload = ref<{ done: number; total: number } | null>(null)
const sexError = ref<string | null>(null)
let sexBusy = false
/** The last photo, and its square when this phone read it: in memory only, for the estimate. */
const lastPhoto = shallowRef<{ at: string; photo: Photo; cut?: { square: Uint8Array; status: string } } | null>(null)
const sexListed = computed(() => !!files.value?.some(f => f.role === 'sex' && f.name === SEX_FILE.name))
const sexStored = ref(false)
/** Descargas' sex row: the team's only, when the server offers it or the phone has it. */
const sexRow = computed<SexRow | null>(() => {
  if (!session.user || !(sexListed.value || sexStored.value)) return null
  return {
    ready: sexReady.value,
    outdated: !sexReady.value && sexStored.value && !!files.value,
    downloadMb: mb(sexSize.value || SEX_FILE.bytes),
    takesMb: mb(SEX_FILE.bytes),
    progress: sexDownload.value,
    error: sexError.value,
  }
})
async function removeSex() {
  if (!confirm(t('¿Borrar este modelo del teléfono? Para volver a usarlo hay que descargarlo otra vez.'))) return
  await engine.forgetSex()
  await refresh()
}

/** The estimate of the result on screen, once (the server's result too: the box and the square come from this phone). */
async function estimateSex() {
  const r = result.value, p = lastPhoto.value
  if (sexBusy || !r || r.sex || !session.user || !sexReady.value || !p || p.at !== r.at) return
  sexBusy = true
  sexError.value = null
  try {
    const cut = p.cut ?? (await engine.square(p.photo))
    const sex = cut.status !== 'success' ? { na: 'nowings' as const } : { z: await engine.sexLogit(cut.square) }
    if (result.value?.at !== r.at) return
    result.value = { ...result.value, sex }
    keepLast()
  } catch (err) {
    if (result.value?.at === r.at)
      sexError.value = /memory|allocation|out of/i.test(String((err as Error)?.message ?? err))
        ? t('El teléfono se quedó sin memoria para el sexo estimado.')
        : t('No se pudo estimar el sexo en esta foto.')
  } finally {
    sexBusy = false
    // Another photo came meanwhile: its turn now.
    if (result.value && result.value.at !== r.at) estimateSex()
  }
}

async function downloadSexModel() {
  if (!files.value || sexDownload.value || download.value) return
  sexError.value = null
  sexDownload.value = { done: 0, total: 1 }
  try {
    await engine.downloadSex(files.value, (done, total) => (sexDownload.value = { done, total }))
    sexReady.value = await engine.sexReady(files.value)
    ready.value = await engine.ready(files.value)
    sexSize.value = engine.sexToDownload(files.value, await engine.stored()).plain
    sexStored.value = true
  } catch {
    sexError.value = t('No se pudo descargar el modelo. Pruebe de nuevo.')
  } finally {
    sexDownload.value = null
  }
  estimateSex()
}

const sexState = computed<SexState | null>(() => {
  const r = result.value
  if (!session.user || !r || !ranked.value) return null
  const name = said.value?.kind === 'genus' ? (said.value as { genus: string }).genus : top.value?.name
  const applies = sexApplies(name, ithomiiniGenera)
  if (applies === 'no') return null
  if (applies === 'genus') return { kind: 'genus', genus: name!.split(' ')[0] }
  if (r.sex) return 'na' in r.sex ? { kind: 'nowings' } : { kind: 'ready', z: r.sex.z }
  if (sexDownload.value) return { kind: 'downloading', ...sexDownload.value }
  if (sexError.value) return { kind: 'error', message: sexError.value }
  // A result restored after another tab, without its photo: nothing to estimate.
  if (sexReady.value) return lastPhoto.value?.at === r.at ? { kind: 'running' } : null
  if (!sexListed.value) return null // not on this server, or no list (offline)
  return { kind: 'download', mb: mb(sexSize.value ?? SEX_FILE.bytes) }
})

// ---------------------------------------------------------------- the general sex line (team only, online, experimental)

/** lib/fieldSex.ts: null unless the page names one of the six species and the server's block is for it. */
const fieldSexState = computed<FieldSexState | null>(() => {
  const r = result.value
  if (!session.user || !r || !ranked.value) return null
  // Hidden when the person picked «En este teléfono»; a signal drop (effective mode phone, choice still online)
  // leaves a shown answer in place.
  return fieldSexOf({ mode: modeChoice.value === 'phone' ? 'phone' : 'online', source: r.source, topName: top.value?.name, answer: r.server?.fieldSex, ithomiiniGenera })
})

// ---------------------------------------------------------------- photos sent to train the AI (team only, VITE_CONTRIB)

const contribOn = computed(() => contribFor(session.user))
let outbox: Outbox | null = null
const myItems = ref<Item[]>([])
const pendingCount = ref(0)
const outboxPaused = ref(false)
let outboxTimer: ReturnType<typeof setTimeout> | null = null
let lastContrib: string | null = null
/** The photo the card would send: the server's copy (online), or this phone's photo (in memory, not after a restore). */
const contribPhoto = computed(() => {
  const r = result.value
  if (!r) return false
  return r.source === 'online' ? !!r.photo : lastPhoto.value?.at === r.at
})
/** The location and day in the photo's EXIF (read before the canvas drops them). */
const photoInfo = computed(() => (lastPhoto.value && lastPhoto.value.at === result.value?.at ? photoMeta(lastPhoto.value.photo.bytes) : { gps: null, takenAt: null }))
const contribShown = computed(() => {
  if (!ranked.value || !top.value) return null
  if (said.value?.kind === 'genus') return { name: (said.value as { genus: string }).genus, rank: 'genus' as const }
  return { name: top.value.name, rank: 'species' as const }
})

let outboxStarting: Promise<void> | null = null
/**
 * The outbox once the session is known (also when it came after the page's wait for it). It resolves once the storage
 * is open; the first send runs on by itself, so Enviar never waits for it.
 */
function startOutbox(): Promise<void> {
  if (!contribFor(session.user)) return Promise.resolve()
  outboxStarting ??= (async () => {
    outbox = createOutbox({ store: await idbStore(), send: httpSend, lock: browserLock })
    syncOutbox({ force: true, status: true }).catch(() => null)
  })().catch(() => {
    outboxStarting = null
  })
  return outboxStarting
}
/**
 * Sends what waits (now: `force`), then the list and the next timed try. Paused for the sign-in (401/403): no timer;
 * the signal coming back, the page shown again or Enviar try again.
 */
async function syncOutbox({ force = false, status = false } = {}) {
  if (!outbox || !session.user) return null
  const uid = session.user.id
  const r = await outbox.flush(uid, { force })
  outboxPaused.value = r.stopped === 'auth'
  if ((status || r.sent) && !r.stopped) await outbox.refresh(uid).catch(() => null)
  myItems.value = await outbox.list(uid)
  pendingCount.value = await outbox.pending(uid)
  if (outboxTimer) clearTimeout(outboxTimer)
  const at = r.stopped === 'auth' ? null : await outbox.nextAt(uid)
  if (at !== null) outboxTimer = setTimeout(() => syncOutbox(), Math.max(1000, at - Date.now()))
  return r
}
/** The card waits at most this long for the send (weak signal): the item is safe on the phone and goes on by itself. */
const CARD_WAIT_MS = 8000
const within = <T,>(p: Promise<T>, ms: number) => Promise.race([p, new Promise<null>(ok => setTimeout(() => ok(null), ms))])
const onVisible = () => document.visibilityState === 'visible' && syncOutbox({ force: true })

const hex = (b: ArrayBuffer) => [...new Uint8Array(b)].map(x => x.toString(16).padStart(2, '0')).join('')
/** What the AI said, kept with the item (DESIGN §4 «Metadata kept»); the person's labels are apart. */
function prediction() {
  const r = result.value!
  const genus = [...(ranked.value?.genus ?? new Map()).entries()].sort((a, b) => b[1] - a[1]).slice(0, 3)
  return {
    source: r.source,
    model: r.kind ?? null,
    // The head that read it: the phone's (its sha256), or the file the server lists (online).
    head: r.source === 'phone' ? (head.value?.meta.head_sha256 ?? null) : null,
    headFile: files.value?.find(f => f.role === 'head-meta')?.name ?? null,
    status: r.status,
    wingBox: r.server?.wingBox ?? null,
    prior: r.server?.prior ?? null,
    build: __BUILD_ID__,
    side: r.source === 'online' ? (r.side ?? null) : sideValue.value,
    species: (ranked.value?.species ?? []).slice(0, 5).map(c => ({ name: c.name, p: c.p })),
    genus: genus.map(([name, p]) => ({ name, p })),
    subspecies: top.value ? (subspecies.value.get(top.value.name) ?? []).slice(0, 3).map(c => ({ name: c.name, p: c.p })) : [],
    sex: r.sex && 'z' in r.sex ? { z: r.sex.z, model: SEX_FILE.name, sha256: SEX_FILE.sha256 } : null,
    pick: pick.value,
    at: r.at,
  }
}
async function contribute(labels: Labels): Promise<Outcome> {
  const r = result.value
  const uid = session.user?.id
  await startOutbox()
  if (!r || !uid || !outbox) return 'error'
  let photo: string
  try {
    photo = r.source === 'online' && r.photo ? r.photo : jpegBase64(await photoCanvas(lastPhoto.value!.photo.blob))
  } catch {
    return 'error'
  }
  const bytes = Uint8Array.from(atob(photo), c => c.charCodeAt(0))
  const meta = photoInfo.value
  try {
    const item = await outbox.add(uid, {
      photo,
      sha256: hex(await crypto.subtle.digest('SHA-256', bytes)),
      thumb: r.crop.replace(/^data:image\/jpeg;base64,/, ''),
      mini: smallThumb.value,
      payload: {
        createdAt: new Date().toISOString(),
        takenAt: meta.takenAt,
        labels: { species: labels.species, sex: labels.sex, photoKind: labels.photoKind },
        prediction: prediction(),
        gps: labels.includeGps && meta.gps ? roundGps(meta.gps, GPS_DECIMALS) : null,
        consent: { version: CONSENT_VERSION },
      },
    })
    lastContrib = item.id
  } catch (e) {
    return e instanceof OutboxFull ? 'full' : e instanceof AlreadyListed ? 'already' : 'error'
  }
  askPersistence()
  const sync = await within(syncOutbox({ force: true }), CARD_WAIT_MS)
  if (sync?.already.includes(lastContrib!)) return 'already'
  const item = myItems.value.find(i => i.id === lastContrib)
  if (item?.state === 'failed') {
    refusal.value = item.lastError
    return 'refused'
  }
  return item?.state === 'sent' ? 'sent' : 'queued'
}
/** Why the server refused the last item (shown on the card). */
const refusal = ref<string | null>(null)
async function undoContribution() {
  if (!outbox || !lastContrib) return
  await outbox.remove(lastContrib)
  lastContrib = null
  await within(syncOutbox({ force: true }), CARD_WAIT_MS)
}
async function removeContribution(id: string) {
  if (!outbox) return
  await outbox.remove(id)
  await syncOutbox({ force: true })
}

/** «Es otra especie»: the model's names (the head's list), else the result's candidates. */
const headNames = shallowRef<{ species: string[]; leaves?: string[]; is_ssp?: string } | null>(null)
let headNamesTried = false
async function loadHeadNames() {
  if (headNames.value || headNamesTried) return
  headNamesTried = true
  const f = files.value?.find(x => x.role === 'head-meta')
  try {
    if (f) {
      const r = await fetch(f.url ?? new URL(`prueba-offline/models/${f.name}`, appRoot()).href, { credentials: f.url ? 'omit' : 'same-origin' })
      if (r.ok) return void (headNames.value = await r.json())
    }
  } catch {
    /* no signal: the phone's copy */
  }
  if (anyReady.value) headNames.value = (await engine.loadHead().catch(() => null))?.meta ?? null
}
watch(contribShown, v => v && contribOn.value && loadHeadNames())
const taxa = computed<TaxonSource>(() => {
  const meta = head.value?.meta ?? headNames.value
  return meta ? headSource(meta) : listSource(ranked.value?.species.map(c => c.name) ?? [])
})

// ---------------------------------------------------------------- reference photos

const refs = ref<Record<string, Refs>>({})
const refsLoading = ref(false)
let refsTimer: ReturnType<typeof setTimeout> | null = null
async function loadRefs(names: string[], tries = 0) {
  if (!names.length) return
  refsLoading.value = true
  // What the phone has first (no network), then the server's and the rest merged in.
  if (!tries) {
    const saved = await savedPhotos(names)
    refs.value = { ...refs.value, ...Object.fromEntries(Object.entries(saved).filter(([n]) => !refs.value[n]?.photos.length)) }
  }
  const { refs: got, pending } = await referencePhotos(names, { signedIn: signedIn() })
  refs.value = { ...refs.value, ...got }
  refsLoading.value = pending.length > 0 && tries < 8
  if (refsLoading.value) refsTimer = setTimeout(() => loadRefs(pending, tries + 1), 4000)
}
const shownNames = () => ranked.value?.species.map(c => c.name).join('|')
watch(
  // Again when the signal comes or goes (and after a save, saveField): offline only the saved subspecies photos.
  () => [shownNames(), online.value] as const,
  ([names]) => {
    if (refsTimer) clearTimeout(refsTimer)
    if (names) loadRefs(names.split('|'))
  },
)
/**
 * The photos of the species compared in the order of their countries (photoPlace); in the same rank its own photos
 * first, then its subspecies' in the model's order (the closest first).
 */
const refsShown = computed<Refs | undefined>(() => {
  const r = refs.value[showing.value]
  if (!r) return r
  const order = (subspecies.value.get(showing.value) ?? []).map(c => c.name)
  const at = (taxon: string) => {
    const i = order.indexOf(taxon)
    return i < 0 ? order.length : i
  }
  return { ...r, photos: orderRefs(r.photos, photoPlace.value, at) }
})
const thumbOf = (name: string) => refs.value[name]?.photos.find(p => p.level !== 'genus')?.src

// The photo packs (lib/usePacks.ts, chosen in Descargas). Only the first screen's button, which says so, saves the
// chosen pack after the model (without photos a species never looked up online has none in the field: 3 Nov 2026,
// Mechanitis in airplane mode; 11 Nov 2026). A model downloaded in Descargas never starts a pack by itself.
const packs = usePacks()
packs.loadInstalled()
/** The person's country from the settings: the side of the Andes means Ecuador, else Descargas' countries. */
const settingHome = computed<string[]>(() => {
  if (side.value !== 'none') return ['EC']
  const r = packs.pack.value?.installed.scope.region ?? packs.scope.value.region
  return r === 'neotropics' ? [] : r
})
/** The subspecies the model names for the species compared (as the frozen panel takes it), and its range. */
const sspNamed = computed(() => {
  const c = ranked.value?.species.find(x => x.name === showing.value)
  const s = c && subspecies.value.get(c.name)?.[0]
  return s && s.p >= SSP_MIN && s.p >= SSP_SHARE * c.p ? s.name : null
})
const sspRange = ref<string[] | null>(null)
watch(
  [sspNamed, () => packs.pack.value],
  async ([name]) => {
    sspRange.value = null
    const range = name ? await taxonRange(name) : null
    if (sspNamed.value === name) sspRange.value = range
  },
  { immediate: true },
)
const photoPlace = computed<PhotoPlace>(() => ({ home: result.value?.country ? [result.value.country] : settingHome.value, range: sspRange.value }))
/** The line under Compare's photo saying whose photos come first (a subspecies' range, then the person's country). */
const firstLine = computed(() => {
  const p = photoPlace.value
  const gps = !!result.value?.country
  const country = p.home.length ? regionLabel(p.home) : null
  if (p.range?.length && sspNamed.value) {
    const ssp = sspNamed.value.split(' ').slice(2).join(' ')
    if (!country) return t('Primero fotos del área de la subespecie {ssp}', { ssp })
    return gps
      ? t('Primero fotos del área de la subespecie {ssp}, luego de {country} (GPS de su foto)', { ssp, country })
      : t('Primero fotos del área de la subespecie {ssp}, luego de {country}', { ssp, country })
  }
  if (!country) return null
  return gps ? t('Primero fotos de {country} (GPS de su foto)', { country }) : t('Primero fotos de {country}', { country })
})
/** What the first download brings with it (the chosen pack, saved after the model). */
const firstPack = computed(() => {
  if (!packs.index.value || packs.pack.value || packs.autoDone()) return null
  const z = scopeSize(packs.index.value, packs.scope.value)
  return z.photos - z.missing ? { n: numText(z.photos - z.missing), mb: mbText(z.bytes - z.missingBytes), bytes: z.bytes - z.missingBytes } : null
})
/** The first screen's button: the model and the photos that follow it, one total. */
const firstMb = computed(() => {
  const s = sizes.value.small
  return roughMb((s ? s.wire || s.plain : 0) || publicSize('small')) + (firstPack.value ? Math.round(firstPack.value.bytes / 1048576) : 0)
})
// The first screen needs the list for that line (and its size); elsewhere it is read where it is used.
watch(
  () => checked.value && !(ready.value.small || ready.value.full) && online.value,
  need => need && packs.loadIndex(),
  { immediate: true },
)
async function firstDownload() {
  packs.askAuto()
  await startDownload('small')
  autoPack()
}
/** The announced save: after the first screen's download, or again on a later open when it was cut off. */
function autoPack() {
  if (packs.autoPending() && (ready.value.small || ready.value.full) && online.value && !packs.pack.value && !packs.progress.value) packs.install()
}
/**
 * Compare without a saved photo, offline (lib/photoPacks.ts packHint): the choice in Descargas that has its photo and
 * what it downloads (beside the pack on the phone, as Descargas' button says); that no photo of it can be downloaded; or, with no list on the phone, the way to Descargas.
 */
const gapHint = computed<{ text: string; go: boolean; scope: PackScope | null } | null>(() => {
  const name = showing.value
  if (!name || online.value || refsLoading.value || refs.value[name]?.photos.length) return null
  const idx = packs.index.value
  if (!idx && !packs.indexFailed.value) return null // the list is still being read
  const h = packHint(idx, name, packs.pack.value?.installed.scope ?? packs.scope.value)
  if (!h) return null
  if (h.kind === 'none') return { text: t('No hay foto de referencia para descargar.'), go: false, scope: null }
  if (h.kind === 'downloads') return { text: t('En Descargas: fotos para comparar'), go: true, scope: null }
  return { text: t('En Descargas: {taxa} · {region}, {mb} MB', { taxa: taxaLabel(h.scope.taxa), region: regionLabel(h.scope.region), mb: mbText(packs.pack.value ? toDownload(idx!, h.scope, packs.pack.value.index) : h.bytes) }), go: true, scope: h.scope }
})
watch(
  () => !!result.value && !online.value && !refsLoading.value && !refs.value[showing.value]?.photos.length,
  need => need && packs.loadIndex(),
)
function fixGap() {
  if (!gapHint.value?.go) return
  if (gapHint.value.scope) packs.scope.value = gapHint.value.scope
  go('downloads')
}
// A pack saved or removed: the photos on screen are asked again.
watch(packs.changed, () => {
  const names = shownNames()
  if (refsTimer) clearTimeout(refsTimer)
  if (names) loadRefs(names.split('|'))
})

// ---------------------------------------------------------------- the team's frozen stock

const stock = ref<{ stock: FrozenStock; asOf: string } | null>(null)
// One list signed in or not (lib/ithomiiniGenera.ts): the workbook's Tribe column misses genera with no row.
const ithomiiniGenera = ITHOMIINI_GENERA
const detailOf = (name: string) => stock.value?.stock.detail?.[name] as StockDetail | undefined

// ---------------------------------------------------------------- log, start

const log = ref<LogEntry[]>(readLog())
function forget() {
  if (!confirm(t('¿Borrar la lista de identificaciones de este teléfono?'))) return
  clearLog()
  log.value = []
}
const when = (iso: string) => new Date(iso).toLocaleString(undefined, { day: 'numeric', month: 'short', hour: '2-digit', minute: '2-digit' })

const goOnline = () => {
  online.value = true
  syncOutbox({ force: true })
  refresh().then(autoPack) // keeps the page too when it was opened without signal; a cut-off first save goes on
  if (session.user) engine.refreshStock().then(async () => (stock.value = await engine.stock()))
}
const goOffline = () => (online.value = false)
onMounted(async () => {
  // The countries' outlines, for a photo's GPS position later without signal (the service worker keeps them).
  if (navigator.onLine) warmShapes()
  addEventListener('online', goOnline)
  addEventListener('offline', goOffline)
  addEventListener('paste', onPaste)
  addEventListener('dragover', onDragOver)
  addEventListener('dragleave', onDragLeave)
  addEventListener('drop', onDrop)
  document.addEventListener('visibilitychange', onVisible)
  await sessionKnown()
  if (unmounted) return // left during the wait
  const known = session.ready
  let signedInAs = session.user?.id ?? null
  stopSessionWatch = watch(
    () => session.user?.id ?? null,
    async id => {
      if (id === signedInAs) return
      // Signed in or out on this page: the page again, so nothing of the other state stays on screen (the team's
      // reference photos, its inventory) and no answer of the old one lands after.
      if (known || signedInAs) return location.reload()
      // The session only came after the wait (slow signal): taken as it is, without a reload (no loop).
      signedInAs = id
      startOutbox()
      await refresh()
      stock.value = await engine.stock()
      engine.refreshStock().then(async () => (stock.value = await engine.stock()))
    },
  )
  // Signed out (the server said so): the team's inventory a session left on this phone goes.
  if (known && !session.user && navigator.onLine) await Promise.all([engine.forgetStock(), engine.forgetSex()])
  if (session.user) stock.value = await engine.stock()
  await refresh()
  await restoreLast()
  autoPack()
  startOutbox()
  if (session.user) engine.refreshStock().then(async () => (stock.value = await engine.stock()))
})
let stopSessionWatch: (() => void) | null = null
let unmounted = false
onBeforeUnmount(() => {
  unmounted = true
  stopSessionWatch?.()
  removeEventListener('online', goOnline)
  removeEventListener('offline', goOffline)
  removeEventListener('paste', onPaste)
  removeEventListener('dragover', onDragOver)
  removeEventListener('dragleave', onDragLeave)
  removeEventListener('drop', onDrop)
  document.removeEventListener('visibilitychange', onVisible)
  if (refsTimer) clearTimeout(refsTimer)
  if (outboxTimer) clearTimeout(outboxTimer)
})
const anyReady = computed(() => ready.value.small || ready.value.full)
</script>

<template>
  <div class="mx-auto w-full max-w-5xl px-4 pt-3 pb-16 sm:px-6">
    <p v-if="TRIBE_RUNG" class="mb-2 text-xs text-fg-subtle" data-testid="tribe-rung-on">{{ $t('Respuesta de tribu y subfamilia activada para pruebas') }}</p>
    <!-- The three places: identify a photo, look a taxon up, what the phone keeps -->
    <nav class="mb-4 grid grid-cols-3 gap-1 rounded-xl border border-border-strong bg-surface-sunken p-1" :aria-label="$t('Identificar')" data-testid="places">
      <button
        v-for="p in PLACES"
        :key="p.id"
        class="place flex min-h-12 items-center justify-center gap-1.5 rounded-lg px-2 text-[0.95rem] font-medium"
        :class="place === p.id ? 'bg-brand text-on-brand shadow-sm' : 'text-fg hover:bg-surface'"
        :aria-current="place === p.id ? 'page' : undefined"
        :data-testid="`place-${p.id}`"
        @click="go(p.id)"
      >
        <component :is="p.icon" :size="18" class="shrink-0" />{{ $t(p.label) }}
        <span v-if="p.id === 'downloads' && packs.progress.value" class="size-2 rounded-full bg-amber-400" :aria-label="$t('Descargando')" />
      </button>
    </nav>

    <TaxonSearch v-if="place === 'search'" :signed-in="!!session.user" :online="online" :home="settingHome" @downloads="go('downloads')" />

    <DownloadsPanel
      v-else-if="place === 'downloads'"
      :models="modelRows"
      :active="active"
      :download="download"
      :error="dlError"
      :lacks-space="lacksSpace"
      :ios-note="isIos && !installed && !anyReady"
      :from-hf="fromHf"
      :sex="sexRow"
      :online="online"
      @download="startDownload"
      @use="useModel"
      @remove="removeModel"
      @sex-download="downloadSexModel"
      @sex-remove="removeSex"
    />

    <template v-else>
    <!-- Where the photo is read: the server (signed in, with signal) or this phone. Signed out: this phone only. -->
    <p v-if="contribOn && pendingCount" class="mb-3 inline-flex items-center gap-1.5 rounded-full bg-cell-pending px-2.5 py-1 text-xs font-medium text-pending-text" role="status" data-testid="contrib-pending">
      <WifiOff v-if="!outboxPaused" :size="14" />{{ $tn(pendingCount, '1 envío pendiente', '{n} envíos pendientes') }}<template v-if="outboxPaused"> · {{ $t('Inicie sesión para enviar.') }}</template>
    </p>
    <div v-if="checked && session.user" class="mb-3" data-testid="mode">
      <div class="inline-flex flex-wrap rounded-md border border-control" role="radiogroup" :aria-label="$t('Dónde se analiza la foto')">
        <button
          role="radio"
          :aria-checked="mode === 'online'"
          :disabled="!online"
          class="rounded-l-md px-3 py-2 text-sm disabled:cursor-not-allowed disabled:text-fg-subtle"
          :class="mode === 'online' ? 'bg-brand text-on-brand' : 'bg-surface text-fg hover:bg-stone-100'"
          @click="chooseMode('online')"
        >
          {{ $t('En línea (servidor)') }}<span v-if="!online" class="text-xs"> · {{ $t('sin señal') }}</span>
        </button>
        <button
          role="radio"
          :aria-checked="mode === 'phone'"
          class="rounded-r-md px-3 py-2 text-sm"
          :class="mode === 'phone' ? 'bg-brand text-on-brand' : 'bg-surface text-fg hover:bg-stone-100'"
          @click="chooseMode('phone')"
        >
          {{ $t('En este teléfono') }}
        </button>
      </div>
      <p class="hint mt-1">
        {{
          mode === 'online'
            ? $t('La foto se envía al servidor del proyecto, que la analiza y no la guarda. No hace falta descargar nada.')
            : !online
              ? anyReady
                ? $t('Sin señal: se usa el modelo de este teléfono.')
                : $t('Sin señal y sin modelo en este teléfono: conéctese para usar el servidor o descargar el modelo.')
              : anyReady
                ? $t('Con el modelo descargado; funciona también sin señal.')
                : $t('Primero hay que descargar el modelo a este teléfono.')
        }}
      </p>
    </div>
    <p v-else-if="checked" class="mb-3 text-xs text-fg-subtle" data-testid="mode">
      {{ $t('La foto se analiza en este teléfono. La opción en línea (servidor) es solo para el equipo, con su cuenta.') }}
    </p>

    <!-- On the phone: whether it works here without signal, and with which model. -->
    <div v-if="mode === 'phone' && anyReady" class="mb-4 flex flex-wrap items-center gap-x-3 gap-y-1 text-sm">
      <span v-if="pageOffline" class="inline-flex items-center gap-1.5 font-medium text-success-text"><Check :size="16" />{{ $t('Listo para usar sin internet') }}</span>
      <span v-else-if="pageOffline === null" class="text-fg-muted">{{ $t('Preparando para usar sin internet…') }}</span>
      <span v-else class="font-medium text-pending-text">{{ $t('Abra esta página una vez con internet para usarla sin señal.') }}</span>
      <span class="whitespace-nowrap">
        <span class="text-fg-muted">{{ label(active!) }}</span>
        <button class="ml-1 min-h-11 px-1 text-brand-text underline-offset-2 hover:underline" @click="go('downloads')">{{ $t('Cambiar') }}</button>
      </span>
      <span v-if="!online" class="inline-flex items-center gap-1 text-fg-muted"><WifiOff :size="14" />{{ $t('Ahora sin señal') }}</span>
      <!-- The photos announced with the first download, saving (long enough to walk out of signal meanwhile). -->
      <span v-if="packs.progress.value" class="basis-full font-medium text-pending-text tabular-nums" role="status" data-testid="pack-saving">
        {{ $t('Guardando fotos para comparar: {done} de {total}', { done: numText(packs.progress.value.done), total: numText(packs.progress.value.total) }) }}
      </span>
    </div>

    <!-- A. No model on the phone yet: the fast one in one tap, the rest in Descargas -->
    <section v-if="checked && mode === 'phone' && !anyReady" class="max-w-xl" data-testid="first-download">
      <h1 class="text-xl font-semibold">{{ $t('Identificar una mariposa') }}</h1>
      <p class="mt-1 text-fg-muted">
        {{ $t('Tome una foto y la app sugiere qué especie es. Funciona en este teléfono, también sin internet, después de una descarga.') }}
      </p>
      <div v-if="download" class="mt-4" role="status">
        <progress class="h-2 w-full accent-[var(--brand)]" :value="download.done" :max="download.total" />
        <p class="mt-1 text-sm text-fg-muted">
          {{ download.retrying ? $t('Se cortó la conexión; reintentando…') : $t('{done} de {total} MB', { done: mb(download.done), total: mb(download.total) }) }}
        </p>
      </div>
      <div v-else class="mt-4 flex flex-wrap items-center gap-x-4 gap-y-2">
        <button class="btn-primary min-h-12 px-5 text-base" :disabled="!online || !!sexDownload" data-testid="first-get" @click="firstDownload">
          <Download :size="20" />{{ $t('Descargar el identificador') }} · {{ firstMb }} MB
        </button>
        <button class="min-h-11 font-medium text-brand-text underline-offset-2 hover:underline" @click="go('downloads')">{{ $t('Más opciones en Descargas') }}</button>
      </div>
      <p v-if="firstPack" class="mt-2 text-sm text-fg-muted">{{ $t('Incluye {n} fotos para comparar ({mb} MB).', firstPack) }}</p>
      <p v-if="firstLacksSpace" class="mt-3 rounded-md bg-amber-50 p-3 text-sm text-amber-950">{{ $t('Hacen falta unos {need} MB libres; este teléfono tiene {free} MB.', firstLacksSpace) }}</p>
      <p v-if="!online" class="mt-2 text-sm text-fg-muted">{{ $t('Hace falta conexión a internet para descargar.') }}</p>
      <p v-if="isIos && !installed" class="mt-3 rounded-md bg-amber-50 p-3 text-sm text-amber-950">
        {{ $t('En iPhone, primero agregue la app a la pantalla de inicio (Compartir → Agregar a inicio) y descargue desde el ícono: lo descargado en Safari no se ve desde el ícono.') }}
      </p>
      <p v-if="dlError" class="mt-3 rounded-md bg-cell-error p-3 text-sm text-error-text" role="alert">{{ dlError }}</p>
      <IdentifyAbout class="mt-4" :team="!!session.user" />
    </section>

    <!-- B and C: taking a photo, the result -->
    <template v-else-if="checked && canIdentify">
      <div class="grid gap-6 lg:grid-cols-[minmax(0,1fr)_minmax(0,1fr)]">
        <div class="min-w-0">
          <template v-if="result && ranked">
            <p
              v-if="noWings"
              class="mb-3 flex gap-2 rounded-md border border-pending-edge bg-cell-pending p-3 text-sm text-pending-text"
              role="alert"
            >
              <TriangleAlert :size="18" class="mt-0.5 shrink-0" />
              <span>{{ $t('No encontramos las alas con claridad: el resultado puede estar mal. Pruebe otra foto, más cerca y con las alas abiertas.') }}</span>
            </p>
            <p class="text-sm text-fg-muted">
              {{
                said?.kind === 'genus'
                  ? $t('Posible género')
                  : said?.kind === 'tribe'
                    ? $t('Posible tribu')
                    : said?.kind === 'subfamily'
                      ? $t('Posible subfamilia')
                      : said?.kind === 'unsure'
                        ? $t('No estamos seguros. Lo más parecido:')
                        : $t('Posible identificación')
              }}
            </p>
            <h1 class="text-2xl leading-tight font-semibold" data-testid="headline">
              <template v-if="said?.kind === 'genus'">
                <i>{{ said.genus }}</i>
                <span class="block text-base font-normal text-fg-muted">{{ $t('la especie no es segura') }}</span>
              </template>
              <template v-else-if="said?.kind === 'tribe' || said?.kind === 'subfamily'">
                {{ said.kind === 'tribe' ? $t('Tribu {name}', { name: said.name }) : $t('Subfamilia {name}', { name: said.name }) }}
                <span class="block text-base font-normal text-fg-muted">{{ $t('el género no es seguro') }}</span>
              </template>
              <i v-else>{{ top?.name }}</i>
            </h1>
            <p class="mt-1 text-sm text-fg-muted">
              <span v-if="ithomiiniBadge === true" class="mr-2 rounded bg-brand-soft px-1.5 py-0.5 text-xs font-medium text-brand-text">Ithomiini</span>
              <span v-else-if="ithomiiniBadge === false" class="mr-2">{{ $t('No parece una Ithomiini.') }}</span>
              {{
                said?.kind === 'genus'
                  ? $t('{genus}: {p} en total', { genus: (said as { genus: string }).genus, p: percent(ranked.genus.get((said as { genus: string }).genus) ?? 0) })
                  : said?.kind === 'tribe' || said?.kind === 'subfamily'
                    ? $t('{genus}: {p} en total', { genus: said.name, p: percent(said.p) })
                    : percent(top?.p ?? 0)
              }}
              · {{ $t('puntuación de la IA, no una garantía') }}
            </p>
            <p v-if="sspHint" class="mt-1 text-sm" data-testid="ssp-hint">
              <span class="text-fg-muted">{{ sspHint.kind === 'species' ? $t('Subespecie más probable:') : $t('Si fuera esta especie, la subespecie más parecida:') }}</span>
              {{ ' ' }}<i class="font-medium">{{ shortName(sspHint.name) }}</i>&nbsp;<span class="whitespace-nowrap tabular-nums">({{ sspPercent(sspHint.p) }})</span>
              <template v-if="sspHint.alt">
                {{ ' ' }}{{ $t('o') }} <i>{{ shortName(sspHint.alt.name) }}</i>&nbsp;<span class="whitespace-nowrap tabular-nums">({{ sspPercent(sspHint.alt.p) }})</span>
              </template>
              <span v-if="sspHint.only" class="text-fg-muted"> · {{ $t('la única subespecie que conoce el modelo') }}</span>
            </p>
            <p class="mt-1 text-sm text-fg-muted">
              {{ $t('Lado de los Andes') }}: {{ $t(SIDE_OPTIONS.find(o => o.value === side)!.label) }}
              <button class="ml-1 text-brand-text hover:underline" @click="focusSide">{{ $t('cambiar') }}</button>
            </p>
            <p class="mt-1 text-xs text-fg-subtle" data-testid="result-source">
              {{ result.source === 'online' ? $t('Analizada en línea, en el servidor del proyecto.') : $t('Analizada en este teléfono ({model}).', { model: label(result.kind ?? 'small') }) }}
            </p>
            <SexEstimate v-if="sexState" :state="sexState" @download="downloadSexModel" />
            <FieldSexEstimate v-else-if="fieldSexState" :state="fieldSexState" />
            <ContributeCard
              v-if="contribOn && !noWings && contribPhoto && contribShown"
              :key="result.at"
              :shown="contribShown"
              :pick="pick"
              :source="taxa"
              :has-gps="!!photoInfo.gps"
              :send="contribute"
              :undo="undoContribution"
              :refusal="refusal"
            />

            <section class="mt-4">
              <div class="mb-2 flex items-baseline justify-between gap-2">
                <h2 class="font-semibold">{{ $t('Compare') }}: <i class="font-normal">{{ showing }}</i></h2>
                <span v-if="pick === showing" class="text-sm text-success-text">{{ $t('Su elección') }} ✓</span>
              </div>
              <RefCompare :mine="result.crop" :name="showing" :refs="refsShown" :loading="refsLoading && !refs[showing]" :online="online" :hint="gapHint" :first="firstLine" @hint="fixGap" />
              <div class="mt-3 flex flex-wrap gap-2">
                <button class="btn-primary px-4 py-2" :aria-pressed="pick === showing" @click="choose(showing)">
                  {{ pick === showing ? $t('Quitar mi elección') : $t('Se parece a esta') }}
                </button>
                <button class="btn px-4 py-2" @click="nextCandidate">{{ $t('No se parece: ver la siguiente') }}</button>
              </div>
              <p class="mt-1 text-xs text-fg-subtle">{{ $t('Su elección se guarda solo en este teléfono.') }}</p>
            </section>
          </template>

          <!-- Taking a photo (first thing when there is no result) -->
          <section :class="result ? 'mt-6 border-t border-border pt-4' : ''">
            <h1 v-if="!result" class="mb-3 text-xl font-semibold">{{ $t('Identificar una mariposa') }}</h1>
            <div class="flex flex-wrap gap-2">
              <button class="btn-primary px-5 py-3 text-base" :disabled="!!busy" @click="camera?.click()">
                <Camera :size="20" />{{ result ? $t('Otra foto') : $t('Tomar foto') }}
              </button>
              <button class="btn px-5 py-3 text-base" :disabled="!!busy" @click="picker?.click()"><ImageUp :size="20" />{{ $t('Elegir foto') }}</button>
              <input ref="camera" type="file" accept="image/*" capture="environment" class="hidden" @change="onPhoto" />
              <input ref="picker" type="file" accept="image/*" class="hidden" @change="onPhoto" />
            </div>
            <p v-if="!result" class="mt-2 text-sm text-fg-muted">{{ $t('Alas abiertas, de cerca, que la mariposa llene la foto.') }}</p>
            <p v-if="finePointer" class="mt-1 text-sm text-fg-subtle">
              {{ $t('También puede pegar una imagen ({keys}) o arrastrarla a la página.', { keys: isMac ? '⌘V' : 'Ctrl+V' }) }}
            </p>
            <p v-if="busy" class="mt-3 text-sm" role="status">
              {{
                busy === 'loading'
                  ? $t('Preparando el identificador… la primera foto tarda más.')
                  : busy === 'sending'
                    ? $t('Enviando la foto al servidor y analizándola…')
                    : $t('Analizando la foto…')
              }}
            </p>
            <p v-if="runError" class="mt-3 rounded-md bg-cell-error p-3 text-sm text-error-text" role="alert">{{ runError }}</p>
            <div ref="sideControl" class="mt-4">
              <span class="field-label">{{ $t('Lado de los Andes (opcional)') }}</span>
              <div class="inline-flex flex-wrap rounded-md border border-control" role="radiogroup" :aria-label="$t('Lado de los Andes (opcional)')">
                <button
                  v-for="o in SIDE_OPTIONS"
                  :key="o.value"
                  role="radio"
                  :aria-checked="side === o.value"
                  class="px-3 py-2 text-sm first:rounded-l-md last:rounded-r-md"
                  :class="side === o.value ? 'bg-brand text-on-brand' : 'bg-surface text-fg hover:bg-stone-100'"
                  @click="side = o.value"
                >
                  {{ $t(o.label) }}
                </button>
              </div>
              <p class="hint mt-1">{{ result ? $t('Al cambiarlo, la lista se reordena.') : $t('Ayuda a ordenar los resultados.') }}</p>
            </div>
          </section>
        </div>

        <div class="min-w-0 space-y-4">
          <!-- The candidates, one list grouped by genus -->
          <section v-if="grouped">
            <h2 class="mb-2 font-semibold">{{ $t('Las más parecidas') }}</h2>
            <ul class="divide-y divide-border rounded-lg border border-border bg-surface">
              <li v-for="g in grouped.groups" :key="g.genus">
                <button
                  class="flex w-full items-center justify-between gap-2 px-3 py-2 text-left"
                  :aria-expanded="openGenus === g.genus"
                  @click="openGenus = openGenus === g.genus ? null : g.genus"
                >
                  <span><i class="font-medium">{{ g.genus }}</i></span>
                  <span class="flex items-center gap-2 text-sm tabular-nums text-fg-muted">
                    {{ percent(g.p) }}
                    <ChevronDown :size="16" class="transition-transform" :class="openGenus === g.genus ? 'rotate-180' : ''" />
                  </span>
                </button>
                <ul v-if="openGenus === g.genus" class="pb-1">
                  <li v-for="c in g.species" :key="c.name">
                    <div class="flex items-center" :class="showing === c.name ? 'bg-brand-soft' : ''">
                    <button
                      class="flex min-w-0 flex-1 items-center gap-3 py-1.5 pl-3 text-left hover:bg-surface-sunken"
                      :class="sspShown(c.name).length ? 'pr-1' : 'pr-3'"
                      @click="compared = c.name"
                    >
                      <img v-if="thumbOf(c.name)" :src="thumbOf(c.name)" alt="" class="size-11 shrink-0 rounded bg-photo-mat object-cover" />
                      <span v-else class="size-11 shrink-0 rounded bg-surface-sunken" />
                      <span class="min-w-0 flex-1">
                        <i>{{ c.name.split(' ').slice(1).join(' ') }}</i>
                        <span v-if="showing === c.name" class="ml-1 text-xs text-brand-text">{{ $t('comparando') }}</span>
                        <span v-if="pick === c.name" class="ml-1 text-xs text-success-text">{{ $t('su elección') }} ✓</span>
                      </span>
                      <span class="text-sm tabular-nums text-fg-muted">{{ percent(c.p) }}</span>
                    </button>
                    <button
                      v-if="sspShown(c.name).length"
                      class="flex shrink-0 items-center gap-0.5 self-stretch px-2 text-xs text-brand-text hover:bg-surface-sunken"
                      :aria-expanded="openSsp === c.name"
                      :aria-label="$t('Subespecies de {name}', { name: c.name })"
                      @click="openSsp = openSsp === c.name ? null : c.name"
                    >
                      {{ $t('subesp.') }}
                      <ChevronDown :size="14" class="transition-transform" :class="openSsp === c.name ? 'rotate-180' : ''" />
                    </button>
                    </div>
                    <ul v-if="openSsp === c.name && sspShown(c.name).length" class="mb-1 ml-[4.25rem] mr-3 border-l border-border pl-2 text-sm" :aria-label="$t('Subespecies de {name}', { name: c.name })">
                      <li v-for="s in sspShown(c.name)" :key="s.name" class="flex justify-between gap-2 py-0.5">
                        <i>{{ s.name.split(' ').slice(2).join(' ') }}</i><span class="tabular-nums text-fg-muted">{{ percent(s.p) }}</span>
                      </li>
                      <li v-if="sspRest(c.name)" class="py-0.5 text-xs text-fg-subtle">{{ $tn(sspRest(c.name), 'otra con menos de 1 %', 'otras {n} con menos de 1 %') }}</li>
                    </ul>
                  </li>
                </ul>
              </li>
              <li v-if="grouped.rest >= 0.005" class="flex justify-between px-3 py-2 text-sm text-fg-muted">
                <span>{{ $t('Otros géneros') }}</span><span class="tabular-nums">{{ percent(grouped.rest) }}</span>
              </li>
            </ul>
          </section>

          <!-- The team: flash-frozen specimens of the species being compared -->
          <StockPanel
            v-if="result && stock && session.user && showing"
            :name="showing"
            :detail="detailOf(showing)"
            :recorded="recordedSides.get(showing)"
            :as-of="stock.asOf"
            :focus="stockFocus"
            :names-subspecies="result.source === 'online' || !!head?.ssp"
          />

          <MyContributions v-if="contribOn && (myItems.length || outboxPaused)" :items="myItems" :paused="outboxPaused" @remove="removeContribution" />

          <details v-if="log.length" class="rounded-lg border border-border bg-surface p-3 text-sm">
            <summary class="cursor-pointer font-medium">{{ $t('Mis identificaciones') }} <span class="font-normal text-fg-muted">({{ $t('en este teléfono') }})</span></summary>
            <ul class="mt-2 space-y-2">
              <li v-for="e in log" :key="e.at" class="flex items-center gap-3">
                <img v-if="e.thumb" :src="e.thumb" alt="" class="size-10 rounded object-cover" />
                <span class="min-w-0 flex-1">
                  <i>{{ e.pick ?? e.ai }}</i>
                  <span class="block text-xs text-fg-muted">
                    {{ when(e.at) }} · {{ e.pick ? (e.pick === e.ai ? $t('coincide con la IA') : $t('la IA sugería {ai}', { ai: e.ai })) : $t('sugerencia de la IA') }}
                  </span>
                </span>
              </li>
            </ul>
            <button class="btn mt-3" @click="forget"><RotateCcw :size="14" />{{ $t('Borrar la lista') }}</button>
          </details>

          <button class="flex min-h-12 w-full items-center gap-2 rounded-lg border border-border bg-surface px-3 text-left text-sm font-medium hover:bg-surface-sunken" data-testid="to-downloads" @click="go('downloads')">
            <Download :size="18" class="shrink-0 text-brand-text" />
            <span class="flex-1">{{ $t('Descargas') }}</span>
            <span v-if="!packs.progress.value" class="font-normal text-fg-muted">{{ packs.pack.value ? $t('{n} fotos', { n: numText(packs.pack.value.installed.photos) }) : $t('sin fotos para comparar') }}</span>
          </button>
          <IdentifyAbout :team="!!session.user" />
        </div>
      </div>
    </template>
    <p v-else-if="!checked" class="text-fg-muted">{{ $t('Cargando…') }}</p>
    </template>
    <div
      v-if="dragging"
      class="pointer-events-none fixed inset-0 z-50 grid place-items-center bg-brand/15 p-6"
      aria-hidden="true"
    >
      <p class="rounded-lg border-2 border-dashed border-brand bg-surface px-6 py-4 text-lg font-medium">{{ $t('Suelte la foto para identificarla') }}</p>
    </div>
  </div>
</template>
