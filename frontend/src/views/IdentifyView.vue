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
 * Behind VITE_CONTRIB=1, the team can also send the photo with the real sex (and the species they chose) to train the
 * AI (SexPanel, lib/contribOutbox.ts: kept on the phone until there is signal; «Mis envíos» lists and deletes them). No
 * location is sent: the photo's EXIF is taken out before it leaves the phone; only the day it was taken goes.
 * Layout (13 Nov 2026, ui-loop): the photos of this visit at the top (PhotoBar: add, camera, clear); the species and
 * its % with the person's photo beside the reference photo (lib/bestPhotos.ts), both open full screen with zoom
 * (PhotoViewer), and the other likely species as thumbnails that switch the comparison in place (CandidateStrip);
 * then the sex (SexPanel); then the full list, the side of the Andes and the rest. «Mis identificaciones» keeps each
 * photo and result (lib/identifyHistory.ts), so an item loads again, with the models that read it.
 */
import { computed, onBeforeUnmount, onMounted, ref, shallowRef, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { Check, ChevronDown, Download, ScanSearch, Search, TriangleAlert, WifiOff } from 'lucide-vue-next'
import { effectiveMode, identifyOnServer, jpegBase64, onlineError, photoCanvas, serverRanking, squareCrop, type Mode, type ServerAnswer } from '../lib/identifyOnline'
import RefCompare from '../components/identify/RefCompare.vue'
import StockPanel from '../components/identify/StockPanel.vue'
import IdentifyAbout from '../components/identify/IdentifyAbout.vue'
import DownloadsPanel, { type ModelRow, type SexRow } from '../components/identify/DownloadsPanel.vue'
import TaxonSearch from '../components/identify/TaxonSearch.vue'
import { usePacks } from '../lib/usePacks'
import { mbText, numText, packHint, scopeSize, toDownload, type PackScope } from '../lib/photoPacks'
import { regionLabel, taxaLabel } from '../lib/usePacks'
import type { SexState } from '../components/identify/SexEstimate.vue'
import SexPanel, { type Labels, type Outcome } from '../components/identify/SexPanel.vue'
import PhotoBar from '../components/identify/PhotoBar.vue'
import CandidateStrip from '../components/identify/CandidateStrip.vue'
import PhotoViewer, { type ViewerPhoto } from '../components/identify/PhotoViewer.vue'
import MyIdentifications from '../components/identify/MyIdentifications.vue'
import TaxonPick from '../components/identify/TaxonPick.vue'
import { bestPhotos, bestThumb, loadCurated, viewerSrc } from '../lib/bestPhotos'
import { curatedPhotos } from '../lib/curatedPhotos'
import type { RefPhoto } from '../lib/identifyRefs'
import { base64Jpeg, canvasJpeg, copyPhoto, downloadPhoto, historyStore, outdated, prune, type HistoryRecord, type Versions } from '../lib/identifyHistory'
import { fieldSexState as fieldSexOf, type FieldSexState } from '../lib/fieldSex'
import { phoneTiming, timed, timingLine, type PhoneTiming } from '../lib/identifyTiming'
import { parseCalibration, SEX_DEFAULT, SEX_FILE, sexApplies, sexShown, type SexCalibration, type SexModelInfo } from '../lib/sexModel'
import { PUBLIC_FILES } from '../lib/publicModels'
import { ITHOMIINI_GENERA } from '../lib/ithomiiniGenera'
import { isCurrent, MODEL_ROLES, storedName } from '../lib/offlineId'
import { appRoot } from '../lib/appRoot'
import { locale, t } from '../lib/i18n'
import { chooseModel, createEngine, KINDS, type Listing, type ModelFile, type Offer } from '../lib/offlineEngine'
import { leafScores, speciesSides, subspeciesBySpecies, sumSpecies, type Head, type ModelKind, type Side } from '../lib/offlineId'
import { readPhoto, takePhoto, type Photo } from '../lib/offlinePhoto'
import { epithet, groupByGenus, headline, higherRanks, isIthomiini, percent, rank, shortName, SSP_MIN, SSP_SHARE, subspeciesHint, TRIBE_RUNG, type Candidate, type Higher, type StockDetail, type Taxonomy } from '../lib/identifyResult'
import { loadTaxonomy } from '../lib/taxonomy'
import { referencePhotos, savedPhotos, taxonRange, type Refs } from '../lib/identifyRefs'
import { comparePlace, photoCountry, warmShapes, type Place as PhotoPlace } from '../lib/photoCountry'
import { readLog, writeLog, writeLogAll, clearLog, type LogEntry } from '../lib/identifyLog'
import type { FrozenStock } from '../lib/frozenStock'
import { useSession } from '../stores/session'
declare const __BUILD_ID__: string
import MyContributions from '../components/identify/MyContributions.vue'
import { CONSENT_VERSION, contribFor } from '../lib/contribConfig'
import { AlreadyListed, createOutbox, httpSend, OutboxFull, type Item, type Outbox } from '../lib/contribOutbox'
import { askPersistence, browserLock, idbStore } from '../lib/contribIdb'
import { photoMeta, stripJpegMeta } from '../lib/exifMeta'
import { headSource, listSource, type TaxonHit, type TaxonSource } from '../lib/taxonPick'

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
const PLACES: { id: Place; label: string; icon: typeof ScanSearch }[] = [
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
// MB as phones and the models' reports count them (10^6 bytes): model C is «172 MB», model B «43 MB».
const mb = (b: number) => Math.max(1, Math.round(b / 1e6))
const roughMb = (b: number) => (b > 200e6 ? Math.round(b / 10e6) * 10 : Math.max(1, Math.round(b / 5e6) * 5))

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
/** A model downloading: its progress, and `waiting` while the connection is down (it goes on by itself). */
const download = ref<{ kind: ModelKind; done: number; total: number; waiting?: boolean } | null>(null)
/** What a download cut off before (a reload, the browser closed) left on the phone: «Continuar» (engine.saved). */
const partials = ref<Record<ModelKind | 'sex', { done: number; total: number } | null>>({ small: null, full: null, sex: null })
const dlError = ref<string | null>(null)
const space = ref<{ free: number } | null>(null)
const sizes = ref<Record<ModelKind, { plain: number; wire: number; total: number } | null>>({ small: null, full: null })
/** What the list offers against the phone (a first download, a newer version, nothing new); null: no list known. */
const offers = ref<Record<ModelKind | 'sex', Offer | null>>({ small: null, full: null, sex: null })
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
      const need = list.filter(f => MODEL_ROLES[k].includes(f.role) && !isCurrent(s?.files[f.name], f))
      if (engine.offer(k, list, s).state !== 'update' || need.some(f => f.role !== 'head-meta') || sexDownload.value || download.value || engine.busy(k)) continue
      headTried = true
      head.value = null
      await engine.download(k, list, () => {}).catch(() => null)
      ready.value = await engine.ready(list)
      s = await engine.stored()
    }
    // A result on screen is ranked again with the new JSON.
    if (headTried && result.value) head.value = await engine.loadHead().catch(() => null)
  }
  storedHead.value = s ? (storedName(s.files, 'head-meta') ?? null) : null
  for (const k of KINDS) sizes.value[k] = files.value ? engine.toDownload(k, files.value, s) : null
  for (const k of KINDS) stored.value[k] = !!s && MODEL_ROLES[k].every(r => storedName(s!.files, r) != null)
  for (const k of KINDS) offers.value[k] = files.value ? engine.offer(k, files.value, s) : null
  offers.value.sex = session.user && files.value ? engine.offer('sex', files.value, s) : null
  sexStored.value = !!s && storedName(s.files, 'sex') != null
  sexReady.value = !!session.user && (await engine.sexReady())
  sexInstalled.value = session.user ? await engine.sexInfo() : null
  sexSize.value = session.user && files.value ? engine.sexToDownload(files.value, s).plain : null
  // A download cut off before: what it kept (none while one runs on this page: its own bar shows).
  for (const k of [...KINDS, 'sex'] as const) {
    const cut = files.value && (k !== 'sex' || session.user) && !engine.busy(k) ? await engine.saved(k, files.value).catch(() => null) : null
    // All bytes in but not checked yet (the page closed during «Comprobando…») counts too: «Continuar» checks and swaps.
    partials.value[k] = cut && cut.done > 0 ? cut : null
  }
  const est = await navigator.storage?.estimate?.().catch(() => null)
  space.value = est?.quota ? { free: Math.max(0, (est.quota ?? 0) - (est.usage ?? 0)) } : null
  checked.value = true
  if (ready.value.small || ready.value.full) {
    // Models from the test page: this page and its runtime are kept now (online) before it says it works offline.
    pageOffline.value = await engine.pageOffline()
  }
}
let headTried = false
/** The identifier's head on this phone and the one the server lists (its file name): the version of a result. */
const storedHead = ref<string | null>(null)
const listedHead = computed(() => files.value?.find(f => f.role === 'head-meta')?.name ?? null)
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
  partials.value[kind] = null
  const updating = offers.value[kind]?.state === 'update'
  try {
    await engine.download(kind, files.value, (done, total, state) => (download.value = { kind, done, total, waiting: state === 'waiting' }))
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
            : updating
              ? t('No se pudo actualizar. Sigue la versión instalada; pruebe de nuevo.')
              : t('La descarga se cortó. Toque Continuar para seguir donde quedó.')
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
/** A model's note (its manifest's) in the page's language, else the other one. */
const noteOf = (n: { es?: string; en?: string } | null | undefined) => (locale.value === 'en' ? (n?.en ?? n?.es) : (n?.es ?? n?.en)) ?? null
/** The Descargas place's rows (DownloadsPanel). */
const modelRows = computed<ModelRow[]>(() =>
  KINDS.map(k => {
    const s = sizes.value[k] ?? { plain: publicSize(k), wire: publicSize(k), total: publicSize(k) }
    const o = offers.value[k]
    return {
      kind: k,
      label: label(k),
      ready: ready.value[k],
      // On the phone and the list offers a newer version (a phone with no stored version counts as older).
      outdated: o?.state === 'update',
      // An update says its exact size (one file of about 23 MB, not «25 MB»); a first download its rounded size.
      downloadMb: o?.state === 'update' ? mb(s.wire || s.plain) : roughMb(s.wire || s.plain || publicSize(k)),
      takesMb: roughMb(s.total),
      recommended: k === 'small',
      versions: o ? { from: o.from, to: o.to, fromMb: o.fromBytes ? mb(o.fromBytes) : null, toMb: o.toBytes ? mb(o.toBytes) : null } : undefined,
      note: noteOf(o?.notes),
      partial: partials.value[k],
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
  /** The sex estimate (team only): the model's logit and the model that gave it (its calibration reads it), or why it does not apply. */
  sex?: { z: number; m?: SexModelInfo } | { na: 'nowings' }
  /** The country of the photo's GPS position (ISO2), when it has one in the Neotropics. */
  country?: string | null
  /** This phone's times for the photo (lib/identifyTiming.ts): loads it needed, the sex model's run. */
  timing?: PhoneTiming
  /** The identifier that read it (its head's file: the server's listed one, or this phone's). */
  idVersion?: string | null
}
const result = shallowRef<Result | null>(null)
const head = shallowRef<Head | null>(null)
const busy = ref<'loading' | 'sending' | 'analysing' | null>(null)
const runError = ref<string | null>(null)
const firstRun = ref(true)
const camera = ref<HTMLInputElement | null>(null)
const picker = ref<HTMLInputElement | null>(null)
/** What a photo loaded from «Mis identificaciones» brings that its kept JPEG no longer has (the canvas dropped the EXIF). */
interface Known {
  takenAt: string | null
  country: string | null
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

/**
 * The photos' run: «Borrar fotos» (and «×» on the photo shown) bumps it, so a read still under way when the person
 * cleared writes nothing when it comes back (no result, no thumbnail, no error), and the rest of a several-photo
 * queue stops.
 */
let run = 0
async function identify(photo: Photo, known?: Known) {
  if (mode.value === 'online') return identifyOnline(photo, known)
  const kind = active.value
  runError.value = null
  if (!kind) return
  const mine = run
  try {
    const country = known ? known.country : await photoCountry(photo.bytes).catch(() => null)
    // the load (when needed) and the run, measured once: the runtime and the head count too
    const phase = await timed(engine.loadGeneration, async () => {
      if (!engine.loaded(kind)) {
        busy.value = 'loading'
        await engine.load(kind)
      }
      busy.value = 'analysing'
      return engine.run(photo, kind)
    })
    if (mine !== run) return
    const out = phase.value
    const timing = phoneTiming(phase, out.ms)
    head.value = engine.head()
    firstRun.value = false
    const at = new Date().toISOString()
    lastPhoto.value = { at, photo, cut: { square: out.square, status: out.status }, takenAt: known?.takenAt }
    result.value = { at, source: 'phone', feature: out.feature, crop: cropUrl(out.square), status: out.status, kind, ms: out.ms, country, timing, idVersion: storedHead.value }
    compared.value = null
    pick.value = null
    keepLast()
    window.scrollTo({ top: 0, behavior: 'smooth' })
    estimateSex()
  } catch (err) {
    if (mine !== run) return
    const m = String((err as Error)?.message ?? err)
    runError.value = /memory|allocation|out of/i.test(m)
      ? t('El teléfono se quedó sin memoria. Cierre otras apps y pruebe de nuevo, o use el modelo Rápido y liviano.')
      : m === 'not-downloaded'
        ? t('Faltan partes del modelo en el teléfono: descárguelo de nuevo.')
        : t('No se pudo analizar la foto. Pruebe con otra.')
  } finally {
    if (mine === run) busy.value = null
  }
}
async function identifyOnline(photo: Photo, known?: Known) {
  runError.value = null
  busy.value = 'sending'
  const mine = run
  try {
    const canvas = await photoCanvas(photo.blob).catch(() => null)
    if (mine !== run) return
    if (!canvas) {
      runError.value = t('No se pudo leer la foto. Pruebe con otra.')
      return
    }
    const sent = jpegBase64(canvas)
    const country = known ? known.country : await photoCountry(photo.bytes).catch(() => null)
    const side = sideValue.value
    const started = performance.now()
    const out = await identifyOnServer(sent, side)
    if (mine !== run) return
    firstRun.value = false
    const at = new Date().toISOString()
    lastPhoto.value = { at, photo, takenAt: known?.takenAt }
    result.value = {
      at,
      idVersion: listedHead.value,
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
    if (mine === run) runError.value = onlineError(err)
  } finally {
    if (mine === run) {
      busy.value = null
      sideChangedMeanwhile()
    }
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
  const mine = run
  try {
    const started = performance.now()
    const out = await identifyOnServer(r.photo, side)
    if (result.value !== r || mine !== run) return
    // the timing line shows this read's time, not the first one's
    result.value = { ...r, server: out, status: out.crop ?? r.status, side, ms: Math.round(performance.now() - started) }
    keepLast()
  } catch (err) {
    if (mine !== run) return
    runError.value = onlineError(err)
    sideFromResult(r)
  } finally {
    if (mine === run) {
      busy.value = null
      sideChangedMeanwhile()
    }
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
  pickHit.value = null
  otherOpen.value = false
  viewer.value = null
  try {
    sessionStorage.removeItem(LAST)
  } catch {
    /* nothing kept */
  }
}
// A new photo replaces the last result at once: a failed one must not leave the previous butterfly on screen.
// Several chosen at once: all read first (Android's grant on a picked file does not last), then identified in turn;
// each goes to the strip at the top, the last one stays on screen.
const unreadable = () => (runError.value = t('No se pudo leer la foto. Si está en la nube (Google Fotos), descárguela primero o use Tomar foto.'))
const onPhoto = async (e: Event) => {
  const input = e.target as HTMLInputElement
  const many = Array.from(input.files ?? [])
  runError.value = null
  if (many.length > 1) {
    const photos: Photo[] = []
    for (const f of many) await readPhoto(f).then(p => photos.push(p), unreadable)
    input.value = ''
    const mine = run
    for (const p of photos) {
      if (mine !== run) return
      clearResult()
      await identify(p)
    }
    return
  }
  clearResult()
  return takePhoto(input, identify, unreadable)
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

/** «Es esta»: the species compared is the person's answer (a tap again takes it back); kept with the item. */
function choose(name: string) {
  pick.value = pick.value === name ? null : name
  pickHit.value = null
  keepLast()
}
/** «Otra especie»: a name the list above does not show (the model's names, or free text). */
const otherOpen = ref(false)
const otherName = ref('')
const pickHit = ref<TaxonHit | null>(null)
function chooseOther(hit: TaxonHit | null) {
  const name = (hit?.name ?? otherName.value).trim().replace(/\s+/g, ' ')
  if (name.length < 3) return
  pick.value = name
  pickHit.value = hit
  otherOpen.value = false
  otherName.value = ''
  if (ranked.value?.species.some(c => c.name === name)) compared.value = name
  keepLast()
}
/** The species label sent with the sex: the person's choice, confirmed when it is the name the result shows. */
const speciesLabel = computed<Labels['species']>(() => {
  const name = pick.value
  if (!name) return null
  const shown = contribShown.value
  const hit = pickHit.value?.name === name ? pickHit.value : taxa.value.search(name, 1).find(h => h.name.toLowerCase() === name.toLowerCase())
  const words = name.split(' ').length
  return {
    name: hit?.name ?? name,
    rank: hit?.rank ?? (words >= 3 ? 'subspecies' : words === 2 ? 'species' : 'genus'),
    confirmed: !!shown && shown.name === name,
    freeText: !hit,
  }
})

// The result in the log once ranked (a smaller copy of the square), and its photo kept to load it again.
// Each result's 72 px square by its time (the strip and the list show it); its crop, to draw it again after a re-read.
const thumbs = ref<Record<string, { src: string; crop: string }>>({})
const smallThumb = computed(() => (result.value ? (thumbs.value[result.value.at]?.src ?? '') : ''))
watch(result, r => {
  if (!r) return
  // What the screen says of it now (another photo may replace it before its thumbnail is drawn).
  const snap = snapshot(r)
  if (thumbs.value[r.at]?.crop === r.crop) return void remember(snap)
  const mine = run
  const img = new Image()
  img.onload = () => {
    if (mine !== run) return
    const c = document.createElement('canvas')
    c.width = c.height = 72
    c.getContext('2d')!.drawImage(img, 0, 0, 72, 72)
    thumbs.value = { ...thumbs.value, [r.at]: { src: c.toDataURL('image/jpeg', 0.7), crop: r.crop } }
    addShot(r.at)
    remember(snap)
  }
  img.src = r.crop
})

/** The result as kept (sessionStorage for a tab switch, the history for «Mis identificaciones»): its feature as base64. */
function serialResult(r: Result) {
  let feature: string | undefined
  if (r.feature) {
    const f = new Uint8Array(r.feature.buffer.slice(0))
    let bin = ''
    for (let i = 0; i < f.length; i++) bin += String.fromCharCode(f[i])
    feature = btoa(bin)
  }
  return { ...r, feature }
}
/** A kept result back; a phone result needs the phone's head to be ranked (null when no model is on the phone). */
async function revive(v: Result & { feature?: string | Float32Array }): Promise<Result | null> {
  if (v.source === 'online') return session.user ? { ...v, feature: undefined } : null
  if (!anyReady.value || typeof v.feature !== 'string') return null
  const bin = atob(v.feature)
  const bytes = new Uint8Array(bin.length)
  for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i)
  head.value = await engine.loadHead()
  return { ...v, source: 'phone', feature: new Float32Array(bytes.buffer) }
}

// Kept for this browser session: going to another tab and back shows the result again.
function keepLast() {
  const r = result.value
  if (!r) return
  try {
    sessionStorage.setItem(LAST, JSON.stringify({ ...serialResult(r), compared: compared.value, pick: pick.value }))
  } catch {
    /* no session storage: not restored */
  }
}
async function restoreLast() {
  try {
    const raw = sessionStorage.getItem(LAST)
    if (!raw) return
    const v = JSON.parse(raw)
    const r = await revive(v)
    if (!r) {
      // A server result of a session that ended leaves nothing of the team's.
      if (v.source === 'online') sessionStorage.removeItem(LAST)
      return
    }
    result.value = r
    if (r.source === 'online') sideFromResult(r)
    compared.value = v.compared ?? null
    pick.value = v.pick ?? null
    // Its photo, when it was kept: the sex can be estimated and sent again after a tab switch.
    const rec = await (await historyStore()).get(r.at).catch(() => null)
    if (rec && result.value?.at === r.at) {
      lastPhoto.value = { at: r.at, photo: await photoOf(rec), takenAt: rec.takenAt }
      estimateSex()
    }
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
watch(sideValue, () => result.value && remember(snapshot(result.value)))

// ---------------------------------------------------------------- the sex estimate (team only, experimental)

const sexReady = ref(false)
/** Bytes still to download for it (the detector may be on the phone already); null: not known. */
const sexSize = ref<number | null>(null)
const sexDownload = ref<{ done: number; total: number; waiting?: boolean } | null>(null)
const sexError = ref<string | null>(null)
let sexBusy = false
/** The last photo, and its square when this phone read it: in memory only, for the estimate. */
const lastPhoto = shallowRef<{ at: string; photo: Photo; cut?: { square: Uint8Array; status: string }; takenAt?: string | null } | null>(null)
/** The server offers a sex model this phone can take (lib/offlineEngine.ts sexFiles: its SHA-256 and calibration). */
const sexListed = computed(() => !!offers.value.sex && offers.value.sex.state !== 'none')
const sexStored = ref(false)
/** The sex model on the phone (its version and calibration), or null. */
const sexInstalled = ref<SexModelInfo | null>(null)
/** The calibration the estimate applies with: the installed model's, else the offered one's, else model B's. */
const sexCalibration = computed<SexCalibration>(
  () => sexInstalled.value?.calibration ?? parseCalibration(files.value?.find(f => f.role === 'sex')?.calibration) ?? SEX_DEFAULT,
)
/** Descargas' sex row: the team's only, when the server offers it or the phone has it. */
const sexRow = computed<SexRow | null>(() => {
  if (!session.user || !(sexListed.value || sexStored.value)) return null
  const o = offers.value.sex
  const listed = files.value?.find(f => f.role === 'sex')
  return {
    genera: sexCalibration.value.genera.length,
    ready: sexReady.value,
    // On the phone and the list offers a newer version (a phone with no stored version counts as older).
    outdated: o?.state === 'update',
    downloadMb: mb(sexSize.value || listed?.bytes || SEX_FILE.bytes),
    takesMb: mb(sexInstalled.value?.bytes ?? listed?.bytes ?? SEX_FILE.bytes),
    progress: sexDownload.value,
    error: sexError.value,
    versions: {
      from: sexInstalled.value?.version ?? null,
      to: o?.to ?? null,
      fromMb: sexInstalled.value ? mb(sexInstalled.value.bytes) : null,
      toMb: listed ? mb(listed.bytes) : null,
    },
    note: noteOf(listed?.notes),
    partial: partials.value.sex,
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
    const ran = cut.status !== 'success' ? null : await timed(engine.loadGeneration, () => engine.sexLogit(cut.square))
    const sex = ran ? { z: ran.value, m: engine.sexLoaded() ?? undefined } : { na: 'nowings' as const }
    if (result.value?.at !== r.at) return
    // the sex model's own time, on this phone's line (it ran only when the wings were found), its load included once
    const sexMs = engine.lastMs.sex ?? ran?.ms ?? null
    const timing = ran && result.value.timing ? { ...result.value.timing, sexMs, sexLoadMs: ran.fresh ? Math.max(0, ran.ms - (sexMs ?? 0)) : 0, sexFresh: ran.fresh } : result.value.timing
    result.value = { ...result.value, sex, timing }
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
  const updating = offers.value.sex?.state === 'update'
  try {
    await engine.downloadSex(files.value, (done, total, state) => (sexDownload.value = { done, total, waiting: state === 'waiting' }))
  } catch (err) {
    // An update that failed (a cut, a file that is not the listed one): the installed version stays and keeps working.
    // A full phone: trying again cannot help (an update needs the new model beside the installed one).
    sexError.value = /quota/i.test(String((err as Error)?.message ?? err))
      ? t('No hay espacio suficiente en el teléfono.')
      : updating
        ? t('No se pudo actualizar. Sigue la versión instalada; pruebe de nuevo.')
        : t('No se pudo descargar el modelo. Pruebe de nuevo.')
  } finally {
    sexDownload.value = null
  }
  await refresh()
  estimateSex()
}

const sexState = computed<SexState | null>(() => {
  const r = result.value
  if (!session.user || !r || !ranked.value) return null
  const name = said.value?.kind === 'genus' ? (said.value as { genus: string }).genus : top.value?.name
  const applies = sexApplies(name, ithomiiniGenera, sexCalibration.value.genera)
  if (applies === 'no') return null
  if (applies === 'genus') return { kind: 'genus', genus: name!.split(' ')[0] }
  if (r.sex) return 'na' in r.sex ? { kind: 'nowings' } : { kind: 'ready', z: r.sex.z, calibration: r.sex.m?.calibration ?? SEX_DEFAULT }
  // An update downloading: the installed version keeps estimating meanwhile.
  if (sexDownload.value && !sexReady.value) return { kind: 'downloading', ...sexDownload.value }
  if (sexError.value) return { kind: 'error', message: sexError.value }
  // A result restored after another tab, without its photo: nothing to estimate.
  if (sexReady.value) return lastPhoto.value?.at === r.at ? { kind: 'running' } : null
  if (!sexListed.value) return null // not on this server, or no list (offline)
  return { kind: 'download', mb: mb(sexSize.value ?? SEX_FILE.bytes) }
})

/** The timing line under the result (lib/identifyTiming.ts). */
const timeText = computed(() => (result.value ? timingLine(result.value, t, locale.value) : null))

// ---------------------------------------------------------------- the general sex line (team only, online, experimental)

/** lib/fieldSex.ts: null unless the server's block is for the species the page names (the six verified, or an estimated
 * species of the model); never for the 13 Ithomiini genera of the in-hand line. For another Ithomiini it replaces the
 * in-hand line's «No aplica» note. */
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
/** The day in the photo's EXIF (read before the canvas drops it); the EXIF's location is never sent. */
const photoTaken = computed(() => {
  const p = lastPhoto.value
  if (!p || p.at !== result.value?.at) return null
  return p.takenAt !== undefined ? p.takenAt : photoMeta(p.photo.bytes).takenAt
})
/**
 * The consent's contact address, from the server's config (GET …/consent); the last one known when there is no
 * signal; null (the sheet says how to ask the team) when never known.
 */
const CONTACT_KEY = 'identify:contrib-contact'
const contribContact = ref<string | null>(null)
try {
  contribContact.value = localStorage.getItem(CONTACT_KEY)
} catch {
  /* private mode */
}
let contactAsked = false
async function loadContact() {
  if (contactAsked || !contribFor(session.user)) return
  contactAsked = true
  const a = await httpSend('GET', 'identify/contributions/consent')
  if (a.status !== 200) return void (contactAsked = false)
  const c = typeof a.body?.contact === 'string' ? a.body.contact : null
  contribContact.value = c
  try {
    if (c) localStorage.setItem(CONTACT_KEY, c)
    else localStorage.removeItem(CONTACT_KEY)
  } catch {
    /* private mode */
  }
}
/** The person accepted the current consent: what this phone queued under an older one goes now. */
async function reconsent() {
  const uid = session.user?.id
  if (!outbox || !uid) return
  if (await outbox.reconsent(uid)) await syncOutbox({ force: true })
}
/** Base64 of a JPEG without its metadata (EXIF, GPS); null when it is not a JPEG that can be checked. */
function cleanBase64(b64: string): string | null {
  const bytes = stripJpegMeta(Uint8Array.from(atob(b64), c => c.charCodeAt(0)))
  if (!bytes) return null
  let text = ''
  for (let i = 0; i < bytes.length; i += 0x8000) text += String.fromCharCode(...bytes.subarray(i, i + 0x8000))
  return btoa(text)
}
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
    loadContact().catch(() => (contactAsked = false))
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
    sex: r.sex && 'z' in r.sex ? { z: r.sex.z, model: r.sex.m?.name ?? SEX_FILE.name, sha256: r.sex.m?.sha256 ?? SEX_FILE.sha256, version: r.sex.m?.version ?? null } : null,
    pick: pick.value,
    at: r.at,
  }
}
async function contribute(labels: Labels): Promise<Outcome> {
  const r = result.value
  const uid = session.user?.id
  await startOutbox()
  if (!r || !uid || !outbox) return 'error'
  // Both are canvas JPEGs (no EXIF); their metadata segments are taken out all the same before they leave the phone.
  let photo: string | null
  let thumb: string | null
  try {
    photo = cleanBase64(r.source === 'online' && r.photo ? r.photo : jpegBase64(await photoCanvas(lastPhoto.value!.photo.blob)))
    thumb = cleanBase64(r.crop.replace(/^data:image\/jpeg;base64,/, ''))
  } catch {
    return 'error'
  }
  if (!photo) return 'error'
  const bytes = Uint8Array.from(atob(photo), c => c.charCodeAt(0))
  try {
    const item = await outbox.add(uid, {
      photo,
      sha256: hex(await crypto.subtle.digest('SHA-256', bytes)),
      thumb,
      mini: smallThumb.value,
      payload: {
        createdAt: new Date().toISOString(),
        takenAt: photoTaken.value,
        labels: { species: labels.species, sex: labels.sex, photoKind: labels.photoKind ?? null },
        prediction: prediction(),
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
  // The curated order (lib/curatedPhotos.ts), its best field photo first, once it is in for these photos.
  const c = curatedShown.value
  if (c && c.name === showing.value && c.of === r.photos && c.place === photoPlace.value) {
    return { ...r, photos: c.hero ? [c.hero, ...c.photos.filter(p => p !== c.hero)] : c.photos }
  }
  const order = (subspecies.value.get(showing.value) ?? []).map(c => c.name)
  const at = (taxon: string) => {
    const i = order.indexOf(taxon)
    return i < 0 ? order.length : i
  }
  return { ...r, photos: bestPhotos(showing.value, r.photos, photoPlace.value, at) }
})
const thumbOf = (name: string) => bestThumb(name, refs.value[name]?.photos, photoPlace.value)
/** The candidates under the comparison: the AI's most likely species (up to 5), each with its best photo. */
const candidates = computed(() => (ranked.value?.species ?? []).slice(0, 5).map(c => ({ ...c, thumb: thumbOf(c.name) })))

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
/**
 * Whose photos first. A subspecies the model merely names puts its range first only when the person's country is in
 * it (Fable r4-F4: a named Brazilian subspecies must not put Brazil before the photo's own Ecuador); without a
 * country, its range. The search, where the person picks the subspecies, always takes the range.
 */
const photoPlace = computed<PhotoPlace>(() => comparePlace(result.value?.country ? [result.value.country] : settingHome.value, sspRange.value))
/**
 * The comparison's reference photos from the curation (lane L2, lib/curatedPhotos.ts curatedPhotos: the best field
 * photo as the hero, the person's country first only among photos of similar quality, the species' Sanger wing crop).
 * Kept with the photos and place it was made for: until then refsShown keeps the country order.
 */
const curatedShown = shallowRef<{ name: string; of: RefPhoto[]; place: PhotoPlace; photos: RefPhoto[]; hero: RefPhoto | null; wing: RefPhoto | null } | null>(null)
loadCurated()
watch(
  () => [showing.value, refs.value[showing.value]?.photos, photoPlace.value] as const,
  async ([name, photos, place]) => {
    if (!name || !photos) return void (curatedShown.value = null)
    const got = await curatedPhotos(name, photos, place)
    if (showing.value === name && refs.value[name]?.photos === photos && photoPlace.value === place) curatedShown.value = { name, of: photos, place, ...got }
  },
  { immediate: true },
)
/** The species' Sanger wing crop beside the reference photo (none for a genus-level photo set). */
const wingShown = computed(() => (curatedShown.value?.name === showing.value ? curatedShown.value.wing : null))
/** What the first download brings with it (the chosen pack, saved after the model). */
const firstPack = computed(() => {
  if (!packs.index.value || packs.pack.value || packs.autoDone()) return null
  const z = scopeSize(packs.index.value, packs.scope.value)
  return z.photos - z.missing ? { n: numText(z.photos - z.missing), mb: mbText(z.bytes - z.missingBytes), bytes: z.bytes - z.missingBytes } : null
})
/** The first screen's button: the model and the photos that follow it, one total. */
const firstMb = computed(() => {
  const s = sizes.value.small
  return roughMb((s ? s.wire || s.plain : 0) || publicSize('small')) + (firstPack.value ? Math.round(firstPack.value.bytes / 1e6) : 0)
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
  historyStore().then(h => h.clear()).catch(() => null)
}

// ---------------------------------------------------------------- this visit's photos (the strip at the top)

const SHOTS = 'identify:shots'
function readShots(): string[] {
  try {
    const v = JSON.parse(sessionStorage.getItem(SHOTS) || '[]')
    return Array.isArray(v) ? v.filter(x => typeof x === 'string') : []
  } catch {
    return []
  }
}
const shots = ref<string[]>(readShots())
watch(shots, v => {
  try {
    sessionStorage.setItem(SHOTS, JSON.stringify(v))
  } catch {
    /* not kept for a tab switch */
  }
})
const addShot = (at: string) => !shots.value.includes(at) && (shots.value = [...shots.value, at])
const shotList = computed(() =>
  shots.value
    .map(at => ({ at, thumb: thumbs.value[at]?.src || log.value.find(e => e.at === at)?.thumb || '' }))
    .filter(x => x.thumb),
)
/** «Borrar fotos»: the strip and the result on screen; «Mis identificaciones» keeps them. */
function clearPhotos() {
  run++
  busy.value = null
  shots.value = []
  clearResult()
  runError.value = null
}
const busyText = computed(() =>
  busy.value === 'loading'
    ? t('Preparando el identificador… la primera foto tarda más.')
    : busy.value === 'sending'
      ? t('Analizando en el servidor…')
      : busy.value === 'analysing'
        ? t('Analizando la foto…')
        : null,
)

// ---------------------------------------------------------------- «Mis identificaciones»: kept, loaded again

/** The models a result was read with, and those that read photos now (Reprocesar when they differ). */
const versionsOf = (r: Result): Versions => ({
  id: r.idVersion ?? null,
  sex: r.sex && 'z' in r.sex ? (r.sex.m?.version ?? r.sex.m?.sha256 ?? null) : null,
})
const currentVersions = computed<Versions>(() => ({
  id: mode.value === 'online' ? listedHead.value : storedHead.value,
  sex: sexInstalled.value?.version ?? sexInstalled.value?.sha256 ?? null,
}))
const entries = computed(() => log.value.map(e => ({ ...e, outdated: !!e.saved && outdated(e.v, currentVersions.value) })))
/** The sex line's call for the list (♀ 72 %). */
const sexSummary = computed<LogEntry['sex']>(() => {
  const r = result.value
  if (fieldSexState.value && (!sexState.value || sexState.value.kind === 'genus')) {
    const f = fieldSexState.value
    const named = f.status !== 'cell_under_90' && f.sure
    return { call: named ? f.sex : 'unsure', pct: named && f.pct != null ? Math.round(f.pct) : null }
  }
  if (!r?.sex || !('z' in r.sex)) return null
  const shown = sexShown(r.sex.z, r.sex.m?.calibration ?? SEX_DEFAULT)
  return { call: shown.call === 'hembra' ? 'female' : shown.call === 'macho' ? 'male' : 'unsure', pct: shown.call === 'no seguro' ? null : Math.round(Math.min(99, shown.p * 100)) }
})

/** The photo kept with a result: the server's copy, or this phone's photo as a JPEG of at most 1600 px. */
const keptPhotos = new Map<string, Blob>()
async function keptPhoto(r: Result, original: Blob | null): Promise<Blob | null> {
  const had = keptPhotos.get(r.at)
  if (had) return had
  let blob: Blob | null = null
  if (r.photo) blob = base64Jpeg(r.photo)
  else if (original) blob = await photoCanvas(original).then(c => canvasJpeg(c), () => null)
  if (blob) keptPhotos.set(r.at, blob)
  return blob
}
const photoOf = async (rec: HistoryRecord): Promise<Photo> => {
  const bytes = await rec.photo.arrayBuffer()
  return { bytes, blob: rec.photo, name: `${rec.at}.jpg`, type: 'image/jpeg', size: bytes.byteLength }
}
/** A result and what the screen says of it now: its ranking's first name, the person's choices, the sex, its photo. */
interface Snapshot {
  r: Result
  top: Candidate | undefined
  compared: string | null
  pick: string | null
  side: Side
  sex: LogEntry['sex']
  photo: Blob | null
  takenAt: string | null
}
function snapshot(r: Result): Snapshot {
  const here = lastPhoto.value?.at === r.at
  return { r, top: top.value, compared: compared.value, pick: pick.value, side: sideValue.value, sex: sexSummary.value, photo: here ? lastPhoto.value!.photo.blob : null, takenAt: here ? photoTaken.value : null }
}
/** The result in the list and in the history (its photo, choices, sex and models), once it has a thumbnail. In turn. */
let remembering: Promise<void> = Promise.resolve()
function remember(snap: Snapshot) {
  remembering = remembering.then(() => rememberNow(snap)).catch(() => undefined)
  return remembering
}
async function rememberNow({ r, top: top1, compared: cmp, pick: pk, side: sd, sex, photo: original, takenAt }: Snapshot) {
  const thumb = thumbs.value[r.at]?.src
  if (!top1 || !thumb) return
  const v = versionsOf(r)
  const before = log.value.find(e => e.at === r.at)
  let saved = before?.saved ?? false
  const photo = await keptPhoto(r, original)
  if (photo) {
    try {
      const store = await historyStore()
      const old = before ? await store.get(r.at) : null
      await store.put({
        at: r.at,
        photo,
        result: JSON.stringify(serialResult(r)),
        compared: cmp,
        pick: pk,
        takenAt: takenAt ?? old?.takenAt ?? null,
        country: r.country ?? null,
        versions: v,
      })
      saved = true
    } catch {
      /* full or private: the line only */
    }
  }
  log.value = writeLog({ at: r.at, thumb, ai: top1.name, p: top1.p, pick: pk, side: sd, sex: sex ?? before?.sex ?? null, v, saved }, log.value)
  historyStore()
    .then(h => prune(h, log.value.map(e => e.at)))
    .catch(() => null)
}
watch([compared, pick, sexSummary], () => result.value && remember(snapshot(result.value)))

const notice = ref<string | null>(null)
let noticeTimer: ReturnType<typeof setTimeout> | null = null
function say(text: string) {
  notice.value = text
  if (noticeTimer) clearTimeout(noticeTimer)
  noticeTimer = setTimeout(() => (notice.value = null), 4000)
}
/** An item back in the panel as if its photo had just been added: its result, choices and sex; Reprocesar reads it again. */
async function loadEntry(at: string, { rerun = false } = {}) {
  if (busy.value) return
  const rec = await (await historyStore()).get(at).catch(() => null)
  if (!rec) return say(t('Esta foto ya no está en el teléfono.'))
  const photo = await photoOf(rec)
  const known: Known = { takenAt: rec.takenAt, country: rec.country }
  const kept = rerun ? null : await revive(JSON.parse(rec.result)).catch(() => null)
  if (!kept) {
    // Read again (Reprocesar, or a phone result with no model on the phone now): a new item replaces the old one.
    clearResult()
    await identify(photo, known)
    const now = result.value
    if (!now || now.at === at) return
    pick.value = rec.pick
    shots.value = shots.value.filter(x => x !== at)
    log.value = log.value.filter(e => e.at !== at)
    writeLogAll(log.value)
    ;(await historyStore()).delete(at).catch(() => null)
    return
  }
  clearResult()
  keptPhotos.set(at, rec.photo)
  lastPhoto.value = { at, photo, takenAt: rec.takenAt }
  result.value = kept
  if (kept.source === 'online') sideFromResult(kept)
  compared.value = rec.compared
  pick.value = rec.pick
  addShot(at)
  keepLast()
  window.scrollTo({ top: 0, behavior: 'smooth' })
  estimateSex()
}
/** The strip's thumbnails: the one on screen stays; another loads from the history. */
function selectShot(at: string) {
  if (result.value?.at !== at) loadEntry(at)
}
async function photoAction(at: string, what: 'copy' | 'download') {
  const rec = await (await historyStore()).get(at).catch(() => null)
  if (!rec) return say(t('Esta foto ya no está en el teléfono.'))
  const e = log.value.find(x => x.at === at)
  if (what === 'download') return downloadPhoto(rec.photo, e?.pick ?? e?.ai ?? 'foto', at)
  say((await copyPhoto(rec.photo)) ? t('Foto copiada.') : t('No se pudo copiar la foto en este navegador; use Descargar.'))
}

// ---------------------------------------------------------------- full screen with zoom

const viewer = shallowRef<{ photos: ViewerPhoto[]; start: number } | null>(null)
/** The person's photo, whole, while it is on screen (an object URL of the photo; the square when it is not here). */
const mineUrl = shallowRef<string | null>(null)
watch(
  () => (lastPhoto.value && lastPhoto.value.at === result.value?.at ? lastPhoto.value.photo.blob : null),
  (blob, _, onCleanup) => {
    mineUrl.value = blob ? URL.createObjectURL(blob) : null
    const url = mineUrl.value
    onCleanup(() => url && URL.revokeObjectURL(url))
  },
)
function openViewer(which: 'mine' | 'ref' | 'wing', src?: string) {
  const r = result.value
  if (!r) return
  const genus = showing.value.split(' ')[0]
  const mine: ViewerPhoto = { src: mineUrl.value ?? r.crop, name: '', caption: t('Su foto'), credit: '', license: '', source: null }
  const refPhotos: ViewerPhoto[] = (refsShown.value?.photos ?? []).map(p => ({
    src: viewerSrc(p.src),
    name: p.level === 'subspecies' && p.taxon ? p.taxon : showing.value,
    kicker: p.level === 'genus' ? t('Foto de otra especie del género {genus}', { genus }) : p.level === 'subspecies' ? t('Subespecie') : p.level === 'specimen' ? t('Ejemplar del proyecto') : undefined,
    credit: p.credit,
    license: p.license,
    source: p.source ?? null,
    country: p.country ?? null,
    preserved: p.kind === 'preserved' ? (p.specimen ?? '') : null,
  }))
  // The wing crop second, after the hero (as Buscar's viewer has it).
  const w = wingShown.value
  const wing: ViewerPhoto[] = w ? [{ src: w.src, name: showing.value, kicker: t('Alas de un ejemplar preservado'), credit: w.credit, license: '', source: null, preserved: w.specimen ?? '' }] : []
  const list = [mine, ...refPhotos.slice(0, 1), ...wing, ...refPhotos.slice(1)]
  const at = which === 'ref' ? (refsShown.value?.photos ?? []).findIndex(p => p.src === src) : -1
  viewer.value = { photos: list, start: which === 'mine' ? 0 : which === 'wing' ? 2 : at <= 0 ? 1 : at + 1 + wing.length }
}

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
    <p v-if="contribOn && pendingCount" class="mb-3 inline-flex items-center gap-1.5 rounded-full bg-cell-pending px-2.5 py-1 text-xs font-medium text-pending-text" role="status" data-testid="contrib-pending">
      <WifiOff v-if="!outboxPaused" :size="14" />{{ $tn(pendingCount, '1 envío pendiente', '{n} envíos pendientes') }}<template v-if="outboxPaused"> · {{ $t('Inicie sesión para enviar.') }}</template>
    </p>

    <!-- The photos of this visit: add, camera, clear (the Wings Gallery's strip) -->
    <PhotoBar
      v-if="checked && canIdentify"
      class="mb-4"
      :shots="shotList"
      :current="result?.at ?? null"
      :busy="busyText"
      :disabled="!!busy"
      :paste-keys="finePointer ? (isMac ? '⌘V' : 'Ctrl+V') : null"
      @select="selectShot"
      @files="picker?.click()"
      @camera="camera?.click()"
      @clear="clearPhotos"
    />
    <input ref="camera" type="file" accept="image/*" capture="environment" class="hidden" @change="onPhoto" />
    <input ref="picker" type="file" accept="image/*" multiple class="hidden" @change="onPhoto" />
    <p v-if="runError" class="mb-4 rounded-md bg-cell-error p-3 text-sm text-error-text" role="alert">{{ runError }}</p>

    <div v-if="checked" class="grid gap-6" :class="result && ranked ? 'lg:grid-cols-[minmax(0,1fr)_minmax(0,1fr)]' : 'max-w-xl'">
      <div v-if="(mode === 'phone' && !anyReady) || (result && ranked)" class="min-w-0">
        <!-- A. No model on the phone yet: the fast one in one tap, the rest in Descargas -->
        <section v-if="mode === 'phone' && !anyReady" data-testid="first-download">
          <h1 class="text-xl font-semibold">{{ $t('Identificar una mariposa') }}</h1>
          <p class="mt-1 text-fg-muted">
            {{ $t('Tome una foto y la app sugiere qué especie es. Funciona en este teléfono, también sin internet, después de una descarga.') }}
          </p>
          <div v-if="download" class="mt-4" role="status">
            <progress class="h-2 w-full accent-[var(--brand)]" :value="download.done" :max="download.total" />
            <p class="mt-1 text-sm text-fg-muted">
              <span v-if="download.waiting" class="font-medium text-pending-text" data-testid="first-waiting">{{ $t('Esperando conexión…') }}</span>
              <template v-if="download.total > 1">{{ download.waiting ? ' · ' : '' }}{{ $t('{done} de {total} MB', { done: mb(download.done), total: mb(download.total) }) }}</template>
            </p>
          </div>
          <div v-else class="mt-4 flex flex-wrap items-center gap-x-4 gap-y-2">
            <button class="btn-primary min-h-12 px-5 text-base" :disabled="!online || !!sexDownload" data-testid="first-get" @click="firstDownload">
              <!-- A first download cut off by a reload: it goes on from what it kept. -->
              <Download :size="20" />{{ partials.small ? $t('Continuar la descarga') : $t('Descargar el identificador') }} · {{ partials.small ? mb(Math.max(0, partials.small.total - partials.small.done)) : firstMb }} MB
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
        </section>

        <!-- B. The result: the species, the person's photo beside its reference photo, the other candidates, then the sex -->
        <template v-else-if="result && ranked">
          <p v-if="noWings" class="mb-3 flex gap-2 rounded-md border border-pending-edge bg-cell-pending p-3 text-sm text-pending-text" role="alert">
            <TriangleAlert :size="18" class="mt-0.5 shrink-0" />
            <span>{{ $t('No encontramos las alas con claridad: el resultado puede estar mal. Pruebe otra foto, más cerca y con las alas abiertas.') }}</span>
          </p>
          <div class="flex items-start justify-between gap-3">
            <div class="min-w-0">
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
                <span v-if="ithomiiniBadge === true" class="ml-1 rounded bg-brand-soft px-1.5 py-0.5 text-xs font-medium text-brand-text">Ithomiini</span>
                <span v-else-if="ithomiiniBadge === false" class="ml-1">· {{ $t('No parece una Ithomiini.') }}</span>
              </p>
              <!-- data-top: the top species whatever the headline says (browser checks: tools, lab notes). -->
              <h1 class="text-2xl leading-tight font-semibold" data-testid="headline" :data-top="top?.name" :data-top-p="top?.p">
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
            </div>
            <p class="shrink-0 pt-4 text-2xl leading-tight font-semibold text-brand-text tabular-nums" data-testid="headline-p" :title="$t('Puntuación de la IA')">
              {{
                said?.kind === 'genus'
                  ? percent(ranked.genus.get((said as { genus: string }).genus) ?? 0)
                  : said?.kind === 'tribe' || said?.kind === 'subfamily'
                    ? percent(said.p)
                    : percent(top?.p ?? 0)
              }}
            </p>
          </div>
          <p v-if="sspHint" class="mt-0.5 text-sm" data-testid="ssp-hint">
            <span class="text-fg-muted">{{ sspHint.kind === 'species' ? $t('Subespecie más probable:') : $t('Si fuera esta especie, la subespecie más parecida:') }}</span>
            {{ ' ' }}<i class="font-medium">{{ shortName(sspHint.name) }}</i>&nbsp;<span class="whitespace-nowrap tabular-nums">({{ sspPercent(sspHint.p) }})</span>
            <template v-if="sspHint.alt">
              {{ ' ' }}{{ $t('o') }} <i>{{ shortName(sspHint.alt.name) }}</i>&nbsp;<span class="whitespace-nowrap tabular-nums">({{ sspPercent(sspHint.alt.p) }})</span>
            </template>
            <span v-if="sspHint.only" class="text-fg-muted"> · {{ $t('la única subespecie que conoce el modelo') }}</span>
          </p>

          <section class="mt-3" data-testid="compare" :aria-label="$t('Comparar')">
            <RefCompare :mine="result.crop" :name="showing" :refs="refsShown" :loading="refsLoading && !refs[showing]" :online="online" :hint="gapHint" :wing="wingShown" @hint="fixGap" @zoom="openViewer" />
            <p v-if="showing !== top?.name" class="mt-1 text-sm">{{ $t('Comparando con') }} <i class="font-medium">{{ showing }}</i></p>
            <CandidateStrip class="mt-2" :candidates="candidates" :showing="showing" :pick="pick" @compare="n => (compared = n)" />
            <div class="mt-2 flex flex-wrap items-center gap-2">
              <button
                class="min-h-11 rounded-md border px-4 text-sm font-medium"
                :class="pick === showing ? 'border-brand bg-brand-soft text-brand-text' : 'border-brand bg-brand text-on-brand hover:bg-brand-hover'"
                :aria-pressed="pick === showing"
                data-testid="pick-this"
                @click="choose(showing)"
              >
                <span class="inline-flex items-center gap-1.5"><Check :size="16" />{{ pick === showing ? $t('Su elección') : $t('Es esta') }}</span>
              </button>
              <button class="btn min-h-11 px-4" :aria-expanded="otherOpen" data-testid="pick-other" @click="otherOpen = !otherOpen">{{ $t('Otra especie…') }}</button>
              <span v-if="pick && pick !== showing && !candidates.some(c => c.name === pick)" class="text-sm text-success-text" data-testid="pick-named">{{ $t('Su elección') }}: <i>{{ pick }}</i> ✓</span>
            </div>
            <div v-if="otherOpen" class="mt-2 flex gap-2">
              <TaxonPick v-model="otherName" :source="taxa" class="min-w-0 flex-1" @pick="chooseOther" />
              <button class="btn min-h-11 px-4" :disabled="otherName.trim().length < 3" @click="chooseOther(null)">{{ $t('Usar') }}</button>
            </div>
          </section>

          <SexPanel
            :key="result.at"
            class="mt-4"
            :state="sexState"
            :field="fieldSexState"
            :can-send="contribOn && !noWings && contribPhoto && !!contribShown"
            :species="speciesLabel"
            :user-id="session.user?.id ?? null"
            :contact="contribContact"
            :send="contribute"
            :undo="undoContribution"
            :refusal="refusal"
            @download="downloadSexModel"
            @consented="reconsent"
          />
        </template>
      </div>

      <div class="min-w-0 space-y-4">
        <!-- The candidates, one list grouped by genus, with where the photo was taken -->
        <section v-if="grouped" data-testid="closest">
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

        <!-- Where it was photographed (it orders the list) and where the photo is read -->
        <section v-if="canIdentify" class="rounded-lg border border-border bg-surface p-3" data-testid="settings">
          <span class="field-label">{{ $t('Lado de los Andes (opcional)') }}</span>
          <div class="inline-flex flex-wrap rounded-md border border-control" role="radiogroup" :aria-label="$t('Lado de los Andes (opcional)')">
            <button
              v-for="o in SIDE_OPTIONS"
              :key="o.value"
              role="radio"
              :aria-checked="side === o.value"
              class="min-h-11 px-3 text-sm first:rounded-l-md last:rounded-r-md"
              :class="side === o.value ? 'bg-brand text-on-brand' : 'bg-surface text-fg hover:bg-surface-sunken'"
              @click="side = o.value"
            >
              {{ $t(o.label) }}
            </button>
          </div>
          <p class="hint mt-1">{{ result ? $t('Al cambiarlo, la lista se reordena.') : $t('Ayuda a ordenar los resultados.') }}</p>
        </section>
        <section v-if="session.user" class="rounded-lg border border-border bg-surface p-3" data-testid="mode">
          <span class="field-label">{{ $t('Dónde se analiza la foto') }}</span>
          <div class="inline-flex flex-wrap rounded-md border border-control" role="radiogroup" :aria-label="$t('Dónde se analiza la foto')">
            <button
              role="radio"
              :aria-checked="mode === 'online'"
              :disabled="!online"
              class="min-h-11 rounded-l-md px-3 text-sm disabled:cursor-not-allowed disabled:text-fg-subtle"
              :class="mode === 'online' ? 'bg-brand text-on-brand' : 'bg-surface text-fg hover:bg-surface-sunken'"
              @click="chooseMode('online')"
            >
              {{ $t('En línea (servidor)') }}<span v-if="!online" class="text-xs"> · {{ $t('sin señal') }}</span>
            </button>
            <button
              role="radio"
              :aria-checked="mode === 'phone'"
              class="min-h-11 rounded-r-md px-3 text-sm"
              :class="mode === 'phone' ? 'bg-brand text-on-brand' : 'bg-surface text-fg hover:bg-surface-sunken'"
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
          <!-- On the phone: whether it works here without signal, and with which model. -->
          <div v-if="mode === 'phone' && anyReady" class="mt-2 flex flex-wrap items-center gap-x-3 gap-y-1 text-sm">
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
        </section>
        <p v-else class="text-xs text-fg-subtle" data-testid="mode">
          {{ $t('La foto se analiza en este teléfono. La opción en línea (servidor) es solo para el equipo, con su cuenta.') }}
        </p>

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

        <MyIdentifications
          v-if="entries.length"
          :entries="entries"
          :current="result?.at ?? null"
          :busy="!!busy"
          @load="loadEntry"
          @reprocess="at => loadEntry(at, { rerun: true })"
          @copy="at => photoAction(at, 'copy')"
          @download="at => photoAction(at, 'download')"
          @forget="forget"
        />
        <p v-if="notice" class="text-sm text-fg-muted" role="status" data-testid="notice">{{ notice }}</p>
        <MyContributions v-if="contribOn && (myItems.length || outboxPaused)" :items="myItems" :paused="outboxPaused" :contact="contribContact" :user-id="session.user!.id" @remove="removeContribution" @reconsent="reconsent" />

        <button class="flex min-h-12 w-full items-center gap-2 rounded-lg border border-border bg-surface px-3 text-left text-sm font-medium hover:bg-surface-sunken" data-testid="to-downloads" @click="go('downloads')">
          <Download :size="18" class="shrink-0 text-brand-text" />
          <span class="flex-1">{{ $t('Descargas') }}</span>
          <span v-if="!packs.progress.value" class="font-normal text-fg-muted">{{ packs.pack.value ? $t('{n} fotos', { n: numText(packs.pack.value.installed.photos) }) : $t('sin fotos para comparar') }}</span>
        </button>
        <IdentifyAbout :team="!!session.user" />
        <p v-if="result" class="text-xs text-fg-subtle" data-testid="result-source">
          {{ result.source === 'online' ? $t('Analizada en línea, en el servidor del proyecto.') : $t('Analizada en este teléfono ({model}).', { model: label(result.kind ?? 'small') }) }}
          <span v-if="timeText" class="tabular-nums" data-testid="result-time">{{ timeText }}</span>
        </p>
      </div>
    </div>
    <p v-else class="text-fg-muted">{{ $t('Cargando…') }}</p>
    <PhotoViewer v-if="viewer" :photos="viewer.photos" :start="viewer.start" back @close="viewer = null" />
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
