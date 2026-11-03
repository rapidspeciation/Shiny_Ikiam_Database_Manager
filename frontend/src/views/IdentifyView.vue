<script setup lang="ts">
/**
 * Identificar: a butterfly from a photo, on this phone, also without signal (DESIGN in
 * .work/offline-id-tab-20261103/DESIGN.md). The models run in the page (lib/offlineEngine.ts); the wording of a
 * result is lib/identifyResult.ts; reference photos lib/identifyRefs.ts; the person's choices stay on the phone
 * (lib/identifyLog.ts). The team sees the flash-frozen gap of the candidate being compared (StockPanel).
 * Without an account: the models from Hugging Face, the static field photos, no request to the team's API (no
 * inventory, no species-photos), and the inventory a team session left on the phone is removed.
 */
import { computed, onBeforeUnmount, onMounted, ref, shallowRef, watch } from 'vue'
import { Camera, Check, ChevronDown, ImageUp, RotateCcw, TriangleAlert, WifiOff } from 'lucide-vue-next'
import RefCompare from '../components/identify/RefCompare.vue'
import StockPanel from '../components/identify/StockPanel.vue'
import IdentifyAbout from '../components/identify/IdentifyAbout.vue'
import { ITHOMIINI_GENERA, PUBLIC_FILES } from '../lib/publicModels'
import { MODEL_ROLES } from '../lib/offlineId'
import { appRoot } from '../lib/appRoot'
import { t } from '../lib/i18n'
import { chooseModel, createEngine, KINDS, type Listing, type ModelFile } from '../lib/offlineEngine'
import { speciesScores, speciesSides, type Head, type ModelKind, type Side } from '../lib/offlineId'
import { takePhoto, type Photo } from '../lib/offlinePhoto'
import { groupByGenus, headline, isIthomiini, percent, rank, type StockDetail } from '../lib/identifyResult'
import { fieldSet, referencePhotos, saveFieldPhotos, type FieldSet, type Refs } from '../lib/identifyRefs'
import { readLog, writeLog, clearLog, type LogEntry } from '../lib/identifyLog'
import type { FrozenStock } from '../lib/frozenStock'
import { useSession } from '../stores/session'

const session = useSession()
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
const choosing = ref(false)
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
  const s = await engine.stored()
  for (const k of KINDS) sizes.value[k] = files.value ? engine.toDownload(k, files.value, s) : null
  const est = await navigator.storage?.estimate?.().catch(() => null)
  space.value = est?.quota ? { free: Math.max(0, (est.quota ?? 0) - (est.usage ?? 0)) } : null
  checked.value = true
  if (ready.value.small || ready.value.full) {
    choosing.value = false
    // Models from the test page: this page and its runtime are kept now (online) before it says it works offline.
    pageOffline.value = await engine.pageOffline()
  }
}
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
const lacksSpace = computed(() => {
  const s = sizes.value[wanted.value]
  return s && space.value && s.plain > space.value.free ? { need: roughMb(s.plain), free: mb(space.value.free) } : null
})

async function startDownload() {
  const kind = wanted.value
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

async function removeModels() {
  if (!confirm(t('¿Borrar los modelos de este teléfono? Para volver a usarlos hay que descargarlos otra vez.'))) return
  await engine.remove()
  result.value = null
  head.value = null
  sessionStorage.removeItem(LAST)
  await refresh()
}

// ---------------------------------------------------------------- side of the Andes

type SideChoice = 'none' | 'East' | 'West'
const side = ref<SideChoice>(((s: string | null) => (s === 'East' || s === 'West' ? s : 'none'))(store.get(SIDE)))
watch(side, v => store.set(SIDE, v))
const sideValue = computed<Side>(() => (side.value === 'none' ? null : side.value))
const SIDE_OPTIONS: { value: SideChoice; label: string }[] = [
  { value: 'none', label: 'No sé' },
  { value: 'East', label: 'Este (Amazonía)' },
  { value: 'West', label: 'Oeste (Costa)' },
]

// ---------------------------------------------------------------- a photo

interface Result {
  at: string
  feature: Float32Array
  crop: string
  status: string
  kind: ModelKind
  ms: number
}
const result = shallowRef<Result | null>(null)
const head = shallowRef<Head | null>(null)
const busy = ref<'loading' | 'analysing' | null>(null)
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
  const kind = active.value
  runError.value = null
  if (!kind) return
  try {
    if (!engine.loaded(kind)) {
      busy.value = 'loading'
      await engine.load(kind)
    }
    busy.value = 'analysing'
    const out = await engine.run(photo, kind)
    head.value = engine.head()
    firstRun.value = false
    result.value = { at: new Date().toISOString(), feature: out.feature, crop: cropUrl(out.square), status: out.status, kind, ms: out.ms }
    compared.value = null
    pick.value = null
    keepLast()
    window.scrollTo({ top: 0, behavior: 'smooth' })
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
function clearResult() {
  result.value = null
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
  if (!anyReady.value || busy.value || choosing.value) return
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
  if (anyReady.value && !choosing.value) dragging.value = true
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

const scores = computed(() => (result.value && head.value ? speciesScores(head.value, result.value.feature, sideValue.value) : null))
const ranked = computed(() => (scores.value && head.value ? rank(scores.value, head.value.meta.species, 5) : null))
const grouped = computed(() => (ranked.value ? groupByGenus(ranked.value.species, ranked.value.genus) : null))
const top = computed(() => ranked.value?.species[0])
const said = computed(() => (ranked.value ? headline(top.value, ranked.value.genus) : null))
const compared = ref<string | null>(null)
const pick = ref<string | null>(null)
const showing = computed(() => compared.value ?? top.value?.name ?? '')
const openGenus = ref<string | null>(null)
watch(grouped, g => (openGenus.value = g?.groups[0]?.genus ?? null))
const recordedSides = computed(() => (head.value ? speciesSides(head.value) : new Map()))
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
    const f = new Uint8Array(r.feature.buffer.slice(0))
    let bin = ''
    for (let i = 0; i < f.length; i++) bin += String.fromCharCode(f[i])
    sessionStorage.setItem(LAST, JSON.stringify({ ...r, feature: btoa(bin), compared: compared.value, pick: pick.value }))
  } catch {
    /* no session storage: not restored */
  }
}
async function restoreLast() {
  try {
    const raw = sessionStorage.getItem(LAST)
    if (!raw) return
    const v = JSON.parse(raw)
    const bin = atob(v.feature)
    const bytes = new Uint8Array(bin.length)
    for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i)
    head.value = await engine.loadHead()
    result.value = { ...v, feature: new Float32Array(bytes.buffer) }
    compared.value = v.compared ?? null
    pick.value = v.pick ?? null
  } catch {
    /* gone with the models: nothing to show */
  }
}
watch(compared, keepLast)
// The log names what the screen shows first: a change of side rewrites the entry (the same time).
watch(sideValue, () => {
  if (result.value && top.value && smallThumb.value)
    log.value = writeLog({ at: result.value.at, thumb: smallThumb.value, ai: top.value.name, p: top.value.p, pick: pick.value, side: sideValue.value }, log.value)
})

// ---------------------------------------------------------------- reference photos

const refs = ref<Record<string, Refs>>({})
const refsLoading = ref(false)
let refsTimer: ReturnType<typeof setTimeout> | null = null
async function loadRefs(names: string[], tries = 0) {
  if (!names.length) return
  refsLoading.value = true
  const { refs: got, pending } = await referencePhotos(names, { signedIn: signedIn() })
  refs.value = { ...refs.value, ...got }
  refsLoading.value = pending.length > 0 && tries < 8
  if (refsLoading.value) refsTimer = setTimeout(() => loadRefs(pending, tries + 1), 4000)
}
watch(
  () => ranked.value?.species.map(c => c.name).join('|'),
  names => {
    if (refsTimer) clearTimeout(refsTimer)
    if (names) loadRefs(names.split('|'))
  },
)
const thumbOf = (name: string) => refs.value[name]?.photos.find(p => p.level !== 'genus')?.src

const field = ref<FieldSet | null>(fieldSet())
const fieldProgress = ref<{ done: number; total: number } | null>(null)
const fieldNote = ref<string | null>(null)
async function saveField() {
  fieldNote.value = null
  fieldProgress.value = { done: 0, total: 1 }
  try {
    const out = await saveFieldPhotos((done, total) => (fieldProgress.value = { done, total }))
    field.value = out
    fieldNote.value = out.failed
      ? t('{n} fotos no se guardaron: toque Actualizar fotos para completarlas.', { n: out.failed })
      : out.pending
        ? t('El servidor todavía reúne fotos de {n} especies: vuelva a tocar Guardar más tarde para completarlas.', { n: out.pending })
        : null
  } catch {
    fieldNote.value = t('Hace falta conexión a internet.')
  } finally {
    fieldProgress.value = null
  }
}

// ---------------------------------------------------------------- the team's frozen stock

const stock = ref<{ stock: FrozenStock; asOf: string } | null>(null)
// The team's list from the workbook when signed in; else the static one shipped with the models.
const ithomiiniGenera = computed(() => (session.user && stock.value?.stock.ithomiiniGenera) || ITHOMIINI_GENERA)
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
  refresh() // keeps the page too when it was opened without signal
  if (session.user) engine.refreshStock().then(async () => (stock.value = await engine.stock()))
}
const goOffline = () => (online.value = false)
onMounted(async () => {
  addEventListener('online', goOnline)
  addEventListener('offline', goOffline)
  addEventListener('paste', onPaste)
  addEventListener('dragover', onDragOver)
  addEventListener('dragleave', onDragLeave)
  addEventListener('drop', onDrop)
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
      await refresh()
      stock.value = await engine.stock()
      engine.refreshStock().then(async () => (stock.value = await engine.stock()))
    },
  )
  // Signed out (the server said so): the team's inventory a session left on this phone goes.
  if (known && !session.user && navigator.onLine) await engine.forgetStock()
  if (session.user) stock.value = await engine.stock()
  await refresh()
  if (ready.value.small || ready.value.full) await restoreLast()
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
  if (refsTimer) clearTimeout(refsTimer)
})
const anyReady = computed(() => ready.value.small || ready.value.full)
</script>

<template>
  <div class="mx-auto w-full max-w-5xl px-4 pt-4 pb-16 sm:px-6">
    <!-- The one status line: whether it works here without signal, and with which model. -->
    <div v-if="anyReady && !choosing" class="mb-4 flex flex-wrap items-center gap-x-3 gap-y-1 text-sm">
      <span v-if="pageOffline" class="inline-flex items-center gap-1.5 font-medium text-success-text"><Check :size="16" />{{ $t('Listo para usar sin internet') }}</span>
      <span v-else-if="pageOffline === null" class="text-fg-muted">{{ $t('Preparando para usar sin internet…') }}</span>
      <span v-else class="font-medium text-pending-text">{{ $t('Abra esta página una vez con internet para usarla sin señal.') }}</span>
      <span class="whitespace-nowrap">
        <span class="text-fg-muted">{{ label(active!) }}</span>
        <button class="ml-2 text-brand-text underline-offset-2 hover:underline" @click="choosing = true">{{ $t('Cambiar') }}</button>
      </span>
      <span v-if="!online" class="inline-flex items-center gap-1 text-fg-muted"><WifiOff :size="14" />{{ $t('Ahora sin señal') }}</span>
    </div>

    <!-- A. Choosing and downloading a model -->
    <section v-if="checked && (!anyReady || choosing)" class="max-w-xl">
      <h1 class="text-xl font-semibold">{{ $t('Identificar una mariposa') }}</h1>
      <p class="mt-1 text-fg-muted">
        {{ $t('Tome una foto y la app sugiere qué especie es. Funciona en este teléfono, también sin internet, después de una descarga.') }}
      </p>
      <fieldset class="mt-4 space-y-2">
        <legend class="sr-only">{{ $t('Modelo') }}</legend>
        <label
          v-for="k in KINDS"
          :key="k"
          class="flex cursor-pointer gap-3 rounded-lg border p-3"
          :class="wanted === k ? 'border-brand bg-brand-soft' : 'border-border bg-surface'"
        >
          <input v-model="wanted" type="radio" name="model" :value="k" class="mt-1 size-4 accent-[var(--focus)]" />
          <span>
            <span class="block font-medium"
              >{{ label(k) }}<span v-if="k === 'small' && !ready[k]" class="ml-2 rounded bg-brand-soft px-1.5 py-0.5 text-xs font-medium text-brand-text">{{ $t('recomendado') }}</span
              ><span v-if="ready[k]" class="ml-2 text-sm font-normal text-success-text">{{ $t('en el teléfono') }} ✓</span></span
            >
            <span class="block text-sm text-fg-muted">{{ sizeLine(k) }}</span>
          </span>
        </label>
      </fieldset>
      <p v-if="isIos && !installed && !ready[wanted]" class="mt-3 rounded-md bg-amber-50 p-3 text-sm text-amber-950">
        {{ $t('En iPhone, primero agregue la app a la pantalla de inicio (Compartir → Agregar a inicio) y descargue desde el ícono: lo descargado en Safari no se ve desde el ícono.') }}
      </p>
      <p v-if="lacksSpace" class="mt-3 rounded-md bg-amber-50 p-3 text-sm text-amber-950">
        {{ $t('Hacen falta unos {need} MB libres; este teléfono tiene {free} MB.', lacksSpace) }}
      </p>
      <div class="mt-4 flex flex-wrap items-center gap-3">
        <button v-if="!ready[wanted]" class="btn-primary px-5 py-2.5 text-base" :disabled="!!download" @click="startDownload">{{ $t('Descargar') }}</button>
        <button v-else class="btn-primary px-5 py-2.5 text-base" @click="(store.set(CHOSEN, wanted), (choosing = false))">{{ $t('Usar este') }}</button>
        <button v-if="choosing" class="btn" @click="choosing = false">{{ $t('Volver') }}</button>
      </div>
      <div v-if="download" class="mt-4" role="status">
        <progress class="h-2 w-full accent-[var(--brand)]" :value="download.done" :max="download.total" />
        <p class="mt-1 text-sm text-fg-muted">
          {{ download.retrying ? $t('Se cortó la conexión; reintentando…') : $t('{done} de {total} MB', { done: mb(download.done), total: mb(download.total) }) }}
        </p>
      </div>
      <p v-if="dlError" class="mt-3 rounded-md bg-cell-error p-3 text-sm text-error-text" role="alert">{{ dlError }}</p>
      <p v-if="fromHf" class="mt-4 text-xs text-fg-subtle">{{ $t('Gratis y sin cuenta: los modelos se descargan de Hugging Face y quedan en este teléfono.') }}</p>
      <IdentifyAbout class="mt-4" />
    </section>

    <!-- B and C: taking a photo, the result -->
    <template v-else-if="anyReady">
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
              {{ said?.kind === 'genus' ? $t('Posible género') : said?.kind === 'unsure' ? $t('No estamos seguros. Lo más parecido:') : $t('Posible identificación') }}
            </p>
            <h1 class="text-2xl leading-tight font-semibold">
              <template v-if="said?.kind === 'genus'">
                <i>{{ said.genus }}</i>
                <span class="block text-base font-normal text-fg-muted">{{ $t('la especie no es segura') }}</span>
              </template>
              <i v-else>{{ top?.name }}</i>
            </h1>
            <p class="mt-1 text-sm text-fg-muted">
              <span v-if="isIthomiini(top?.name ?? '', ithomiiniGenera) === true" class="mr-2 rounded bg-brand-soft px-1.5 py-0.5 text-xs font-medium text-brand-text">Ithomiini</span>
              <span v-else-if="isIthomiini(top?.name ?? '', ithomiiniGenera) === false" class="mr-2">{{ $t('No parece una Ithomiini.') }}</span>
              {{ said?.kind === 'genus' ? $t('{genus}: {p} en total', { genus: (said as { genus: string }).genus, p: percent(ranked.genus.get((said as { genus: string }).genus) ?? 0) }) : percent(top?.p ?? 0) }}
              · {{ $t('puntuación de la IA, no una garantía') }}
            </p>
            <p class="mt-1 text-sm text-fg-muted">
              {{ $t('Lado de los Andes') }}: {{ $t(SIDE_OPTIONS.find(o => o.value === side)!.label) }}
              <button class="ml-1 text-brand-text hover:underline" @click="focusSide">{{ $t('cambiar') }}</button>
            </p>

            <section class="mt-4">
              <div class="mb-2 flex items-baseline justify-between gap-2">
                <h2 class="font-semibold">{{ $t('Compare') }}: <i class="font-normal">{{ showing }}</i></h2>
                <span v-if="pick === showing" class="text-sm text-success-text">{{ $t('Su elección') }} ✓</span>
              </div>
              <RefCompare :mine="result.crop" :name="showing" :refs="refs[showing]" :loading="refsLoading && !refs[showing]" :online="online" />
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
              {{ busy === 'loading' ? $t('Preparando el identificador… la primera foto tarda más.') : $t('Analizando la foto…') }}
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
                    <button
                      class="flex w-full items-center gap-3 px-3 py-1.5 text-left hover:bg-surface-sunken"
                      :class="showing === c.name ? 'bg-brand-soft' : ''"
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
          />

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

          <details class="rounded-lg border border-border bg-surface p-3 text-sm">
            <summary class="cursor-pointer font-medium">{{ $t('Modelo y almacenamiento') }}</summary>
            <dl class="mt-2 grid grid-cols-[auto_1fr] gap-x-3 gap-y-1">
              <dt class="text-fg-muted">{{ $t('En uso') }}</dt>
              <dd>{{ label(active!) }} <button class="ml-1 text-brand-text hover:underline" @click="choosing = true">{{ $t('Cambiar') }}</button></dd>
              <template v-if="engine.lastMs.photo">
                <dt class="text-fg-muted">{{ $t('Última foto') }}</dt>
                <dd>{{ $t('{s} s', { s: (engine.lastMs.photo / 1000).toFixed(1) }) }}</dd>
              </template>
            </dl>
            <h3 class="mt-3 font-medium">{{ $t('Fotos para el campo') }}</h3>
            <p class="text-fg-muted">{{ $t('Una foto de cada especie de Ithomiini, para comparar sin internet.') }}</p>
            <p v-if="field" class="mt-1">{{ $t('{n} fotos guardadas ({mb} MB), {date}', { n: field.count, mb: (field.bytes / 1048576).toFixed(1), date: when(field.at) }) }}</p>
            <p v-if="fieldProgress" class="mt-1" role="status">{{ $t('{done} de {total} fotos', fieldProgress) }}</p>
            <p v-if="fieldNote" class="mt-1 text-fg-muted">{{ fieldNote }}</p>
            <button class="btn mt-2" :disabled="!!fieldProgress || !online" @click="saveField">
              {{ field ? $t('Actualizar fotos') : $t('Guardar fotos') }}
            </button>
            <h3 class="mt-3 font-medium">{{ $t('Instalar en el teléfono') }}</h3>
            <p class="text-fg-muted">
              {{ $t('Android: menú ⋮ → «Instalar app». iPhone: Compartir → «Agregar a inicio», y abra la app desde el ícono.') }}
            </p>
            <button class="btn-danger mt-3" @click="removeModels">{{ $t('Borrar los modelos de este teléfono') }}</button>
          </details>
          <IdentifyAbout />
        </div>
      </div>
    </template>
    <p v-else-if="!checked" class="text-fg-muted">{{ $t('Cargando…') }}</p>
    <div
      v-if="dragging"
      class="pointer-events-none fixed inset-0 z-50 grid place-items-center bg-brand/15 p-6"
      aria-hidden="true"
    >
      <p class="rounded-lg border-2 border-dashed border-brand bg-surface px-6 py-4 text-lg font-medium">{{ $t('Suelte la foto para identificarla') }}</p>
    </div>
  </div>
</template>
