<script setup lang="ts">
import { computed, defineAsyncComponent, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { Check, ChevronDown, ExternalLink, FolderSearch, Globe, History, Loader2, RefreshCw, ScanSearch, ScanText } from 'lucide-vue-next'
import { api } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import { intlLocale, t } from '../lib/i18n'
import { siteRunLabel, type ReadingsStatus, type SiteStatus, type SitesAnswer } from '../lib/renameHost'
import { KINDS, kindLabel, type CurationSummary } from '../lib/photoCuration'

const ProposalsLive = defineAsyncComponent(() => import('./assistant/ProposalsLive.vue'))

/**
 * The «Fotos» tab's Photo_links and public sites, without the assistant: what the sheet's menu
 * «📷 Photo Database Tools» does. «Leer Drive» reads all the listed folders, or the ones chosen
 * (or a new one by its Drive link), and compares them with Photo_links: the result is the same card
 * the assistant's sync_photo_links makes, with its «Aplicar» (new photos, rows that follow a renamed,
 * replaced or deleted file, as when photos were copied to another account). Below, the Wings Gallery
 * and the maps: when each took the workbook's data last, and a button that starts its update.
 */
const props = defineProps<{
  /** Folders to read at once (the ones «Ya las subí» names), with the time asked, to ask again. */
  preset?: { folders: string[]; at: number } | null
  /** A proposal whose link was clicked in the chat beside (the time asked, to ask again): its card shown here. */
  linked?: { id: string; at: number } | null
}>()

interface Folder {
  path: string
  link: string
  files: number
}
const folders = ref<Folder[]>([])
const which = ref<'all' | 'chosen'>('all')
const chosen = ref(new Set<string>())
const filter = ref('')
const added = ref('')
const reading = ref(false)
const CARD_KEY = 'photo-links:card'
const card = ref(localStorage.getItem(CARD_KEY) ?? '')
const upToDate = ref('')
const result = ref<HTMLElement>()
/** The card was written in this visit: the gallery's update comes next. */
const written = ref(false)
// ------------------------------------------------------------ the photo archive review
const archive = ref<CurationSummary | null>(null)
const archiveError = ref('')
const archiveBusy = ref(false)
/** The section picked: a kind of problem (none: all), a folder and a year; the counts follow the folder and year. */
const archiveKind = ref('')
const archiveFolder = ref('')
const archiveYear = ref('')
/** The folders and years of the whole archive, for the lists (the counts of a narrowed summary would hide the others). */
const archiveFolders = ref<{ name: string; n: number }[]>([])
const archiveYears = ref<{ name: string | number; n: number }[]>([])
const kindRows = computed(() => KINDS.filter(k => archive.value?.kinds[k]?.n).map(k => ({ kind: k, ...archive.value!.kinds[k] })))
const archiveCount = computed(() => (archiveKind.value ? (archive.value?.kinds[archiveKind.value]?.n ?? 0) : (archive.value?.total ?? 0)))
async function loadArchive() {
  archiveError.value = ''
  try {
    const query = new URLSearchParams(Object.entries({ folder: archiveFolder.value, year: archiveYear.value }).filter(([, v]) => v))
    const out = await api<CurationSummary>(`photo-curation/summary${query.size ? `?${query}` : ''}`)
    archive.value = out
    if (!query.size) {
      archiveFolders.value = out.folders.filter(f => !/^\d+ more$/.test(f.name) && f.name !== 'not in Photo_links')
      archiveYears.value = out.years.filter(y => y.name !== 'unknown')
    }
  } catch (e) {
    archiveError.value = errorText(e)
  }
}
watch([archiveFolder, archiveYear], () => void loadArchive())
async function reviewArchive() {
  archiveBusy.value = true
  archiveError.value = ''
  try {
    const out = await api<{ proposalId: string }>('photo-curation/show', {
      method: 'POST',
      body: { kind: archiveKind.value, folder: archiveFolder.value, year: archiveYear.value, limit: 40 },
    })
    if (out.proposalId) showCard(out.proposalId, 'curation')
  } catch (e) {
    archiveError.value = errorText(e)
  } finally {
    archiveBusy.value = false
  }
}

// ------------------------------------------------------------ the card on show, and the ones before it
/** One card at a time in the workspace; the ones it replaced, to go back to (the newest first). */
const RECENT_KEY = 'photo-links:recent'
type Recent = { id: string; kind: 'reading' | 'curation' | 'photos'; at: string }
const recent = ref<Recent[]>(JSON.parse(localStorage.getItem(RECENT_KEY) ?? '[]'))
const recentOpen = ref(false)
function showCard(id: string, kind: Recent['kind']) {
  const before = card.value
  if (before && before !== id) {
    const was = recent.value.find(r => r.id === before)
    recent.value = [{ id: before, kind: was?.kind ?? cardKind.value, at: new Date().toISOString() }, ...recent.value.filter(r => r.id !== before && r.id !== id)].slice(0, 8)
    localStorage.setItem(RECENT_KEY, JSON.stringify(recent.value))
  }
  cardKind.value = kind
  card.value = id
  localStorage.setItem(CARD_KEY, id)
  upToDate.value = ''
  recentOpen.value = false
}
const cardKind = ref<Recent['kind']>((localStorage.getItem(`${CARD_KEY}:kind`) as Recent['kind']) || 'reading')
watch(cardKind, k => localStorage.setItem(`${CARD_KEY}:kind`, k))
const recentLabel = (r: Recent) =>
  `${r.kind === 'curation' ? t('Revisión del archivo') : r.kind === 'photos' ? t('Fotos sin renombrar') : 'Photo_links'} · ${new Date(r.at).toLocaleString(intlLocale(), { day: '2-digit', month: '2-digit', hour: '2-digit', minute: '2-digit' })}`
/** A card kept from an earlier visit that is no longer waiting (applied, discarded) is not shown again. */
async function dropUsedCard() {
  if (!card.value) return
  try {
    const { proposals } = await api<{ proposals: { id: string; status: string }[] }>(`chat/proposals?${new URLSearchParams({ only: card.value, all: '1' })}`)
    const status = proposals.find(p => p.id === card.value)?.status
    if (!status || !['pending', 'partial', 'applying', 'queued'].includes(status)) showCard('', cardKind.value)
  } catch {
    // Kept as it was.
  }
}
/** The sidebar's other tools, folded or open as the person left them. */
const FOLDS_KEY = 'photo-links:folds'
const folds = ref<Record<string, boolean>>({ read: true, unrenamed: false, sites: false, ...JSON.parse(localStorage.getItem(FOLDS_KEY) ?? '{}') })
function fold(key: string, event: Event) {
  folds.value = { ...folds.value, [key]: (event.target as HTMLDetailsElement).open }
  localStorage.setItem(FOLDS_KEY, JSON.stringify(folds.value))
}
const onCurationOpen = (event: Event) => showCard((event as CustomEvent<string>).detail, 'curation')
let wasOpen = false
/** The card's count of what waits: from one to none, it was applied or discarded (asked which). */
async function onCount(n: number) {
  if (n > 0) {
    wasOpen = true
    return
  }
  if (!wasOpen || !card.value) return
  wasOpen = false
  try {
    const { proposals } = await api<{ proposals: { id: string; status: string }[] }>(
      `chat/proposals?${new URLSearchParams({ only: card.value, all: '1' })}`,
    )
    if (proposals.find(p => p.id === card.value)?.status === 'applied') written.value = true
  } catch {
    // Without it, no nudge.
  }
}

/** The folders listed: the top ones, or every one whose path has the text typed. */
const shown = computed(() => {
  const q = filter.value.trim().toLowerCase()
  const list = q ? folders.value.filter(f => f.path.toLowerCase().includes(q)) : folders.value.filter(f => !f.path.includes('/'))
  return list.slice(0, 200)
})
function toggle(path: string) {
  const next = new Set(chosen.value)
  if (!next.delete(path)) next.add(path)
  chosen.value = next
  which.value = 'chosen'
}

async function loadFolders() {
  try {
    folders.value = (await api<{ folders: Folder[] }>('photo-links/folders')).folders
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
async function read() {
  const only = which.value === 'chosen' ? [...chosen.value] : []
  const add = added.value.trim() ? [added.value.trim()] : []
  if (which.value === 'chosen' && !only.length && !add.length) return notify(t('Elige al menos una carpeta'), 'error')
  reading.value = true
  upToDate.value = ''
  try {
    const out = await api<{ proposalId: string | null; upToDate: boolean; notFound: string[] }>('photo-links/read', {
      method: 'POST',
      body: { only, add },
    })
    if (out.notFound.length) notify(t('No se encontró en Drive: {folders}', { folders: out.notFound.join(', ') }), 'error')
    if (out.proposalId) showCard(out.proposalId, 'reading')
    else {
      upToDate.value = new Date().toLocaleTimeString(intlLocale(), { hour: '2-digit', minute: '2-digit' })
      card.value = ''
      localStorage.removeItem(CARD_KEY)
    }
    if (add.length) added.value = ''
    // On a phone the result is under the folders and the sites: brought into sight.
    await nextTick()
    if (!matchMedia('(min-width: 768px)').matches) result.value?.scrollIntoView({ behavior: 'smooth', block: 'start' })
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    reading.value = false
  }
}
watch(card, () => (written.value = false))
const emit = defineEmits<{ used: [] }>()
watch(
  () => props.preset?.at,
  () => {
    if (!props.preset?.folders.length) return
    chosen.value = new Set(props.preset.folders)
    which.value = 'chosen'
    filter.value = ''
    emit('used')
    void read()
  },
  { immediate: true },
)

// ------------------------------------------------------------ photos uploaded without being renamed
/** A folder of camera-named photos on Drive (its link, or its path in Photo_links), read on the server. */
const uploaded = ref('')
const readingIds = ref(false)
async function readIds() {
  if (!uploaded.value.trim()) return notify(t('Pega el enlace de la carpeta, o su ruta en Photo_links'), 'error')
  readingIds.value = true
  try {
    const out = await api<{ batch: string; photos: number }>('drive-photos/start', { method: 'POST', body: { folder: uploaded.value.trim() } })
    showCard(out.batch, 'photos')
    uploaded.value = ''
    notify(t('Leyendo {n} fotos en el servidor (un segundo cada una)', { n: out.photos }))
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    readingIds.value = false
  }
}

// ------------------------------------------------------------ the public sites
const sites = ref<SiteStatus[]>([])
const sitesReady = ref(true)
const starting = ref<'' | 'gallery' | 'maps'>('')
const gbif = ref(false)
let timer: ReturnType<typeof setTimeout> | null = null
const running = (s: SiteStatus) =>
  [s.update?.state, s.lastUpdate && typeof s.lastUpdate === 'object' ? s.lastUpdate.state : null, s.publishing?.state].includes('running')
/** The photo readings (species, sex, wings) of the photos Photo_links lists, read on the server after each sync. */
const readings = ref<ReadingsStatus | null>(null)
async function loadSites(fresh = false) {
  if (timer) clearTimeout(timer)
  try {
    const [out, read] = await Promise.all([api<SitesAnswer>(`sites${fresh ? '?fresh=1' : ''}`), api<ReadingsStatus>('readings').catch(() => null)])
    sites.value = out.sites
    sitesReady.value = out.ready
    readings.value = read
  } catch {
    // The section shows what it had.
  }
  // While a run goes on, asked again now and then (GitHub answers 60 times an hour to the server).
  if (sites.value.some(running) || readings.value?.state === 'reading') timer = setTimeout(() => void loadSites(true), 45_000)
}
async function update(key: 'gallery' | 'maps') {
  starting.value = key
  try {
    const out = await api<SitesAnswer>('sites/update', { method: 'POST', body: { sites: [key], gbif: key === 'maps' && gbif.value } })
    const site = out.sites[0]
    if (key === 'gallery' && !site?.error) written.value = false
    if (site?.error) notify(site.error, 'error')
    else if (site?.update?.note) notify(site.update.note)
    else notify(t('Actualización iniciada: el sitio muestra los datos nuevos en unos minutos'))
    await loadSites(true)
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    starting.value = ''
  }
}
const KEYS = ['gallery', 'maps'] as const
const when = (iso?: string) =>
  iso ? new Date(iso).toLocaleString(intlLocale(), { day: '2-digit', month: '2-digit', year: 'numeric', hour: '2-digit', minute: '2-digit' }) : ''

/** Cards made from the reading's (a fix's plan, a batch read on the server): shown below it, the latest first. */
const extra = ref<string[]>([])
function cardMade(event: Event) {
  const id = (event as CustomEvent<string>).detail
  if (id && id !== card.value) extra.value = [id, ...extra.value.filter(x => x !== id)].slice(0, 4)
}
watch(
  () => props.linked,
  link => {
    if (!link) return
    if (link.id !== card.value) extra.value = [link.id, ...extra.value.filter(x => x !== link.id)].slice(0, 4)
    void nextTick(() => document.querySelector(`[data-photo-links-panel] [data-proposal="${link.id}"]`)?.scrollIntoView({ block: 'nearest', behavior: 'smooth' }))
  },
  { immediate: true },
)
onMounted(() => {
  void loadFolders()
  void loadSites()
  void loadArchive()
  void dropUsedCard()
  window.addEventListener('photos:card', cardMade)
  window.addEventListener('curation:open', onCurationOpen)
})
onBeforeUnmount(() => {
  if (timer) clearTimeout(timer)
  window.removeEventListener('photos:card', cardMade)
  window.removeEventListener('curation:open', onCurationOpen)
})
</script>

<template>
  <div class="flex min-h-0 flex-1 flex-col overflow-y-auto md:flex-row md:overflow-hidden" data-photo-links-panel>
    <aside class="flex shrink-0 flex-col gap-4 border-b border-stone-200 bg-white p-3 text-sm md:w-80 md:overflow-y-auto md:border-r md:border-b-0">
      <!-- The photo archive against the database: a section to review in the workspace. -->
      <section class="space-y-2" data-archive-summary>
        <h2 class="flex items-center gap-1.5 font-medium text-stone-800"><ScanSearch :size="15" class="text-emerald-700" /> {{ $t('Revisión del archivo de fotos') }}</h2>
        <p v-if="archive" class="text-xs text-stone-600">
          {{ $tn(archive.total, '{n} caso', '{n} casos') }} ·
          <span class="text-emerald-800">{{ $t('{n} seguros', { n: archive.tiers.clear ?? 0 }) }}</span> ·
          <span class="text-sky-800">{{ $t('{n} a pensar', { n: archive.tiers.reason ?? 0 }) }}</span> ·
          <span class="text-amber-800">{{ $t('{n} mirar', { n: archive.tiers.look ?? 0 }) }}</span>
        </p>
        <p v-if="archive?.sandboxDrive" class="rounded border border-amber-300 bg-amber-50 px-2 py-1 text-xs text-amber-950" data-lab-drive>{{ $t('Lab: los cambios de archivos son simulados. Los cambios en la base se guardan en la copia de prueba.') }}</p>
        <div v-if="archive" class="overflow-hidden rounded border border-stone-200 text-xs" role="listbox" :aria-label="$t('Tipo de problema')" data-kinds>
          <p class="flex items-center gap-2 bg-stone-50 px-2 py-0.5 text-[10px] text-stone-500" aria-hidden="true">
            <span class="flex-1" /><span class="w-7 text-right">{{ $t('seguro') }}</span><span class="w-7 text-right">{{ $t('pensar') }}</span><span class="w-7 text-right">{{ $t('mirar') }}</span>
          </p>
          <button
            v-for="k in [{ kind: '', n: archive.total, clear: archive.tiers.clear ?? 0, reason: archive.tiers.reason ?? 0, look: archive.tiers.look ?? 0 }, ...kindRows]"
            :key="k.kind"
            type="button"
            role="option"
            :aria-selected="archiveKind === k.kind"
            class="flex w-full items-center gap-2 border-t border-stone-100 px-2 py-1 text-left first:border-t-0"
            :class="archiveKind === k.kind ? 'bg-emerald-50 font-medium text-stone-900' : 'text-stone-700 hover:bg-stone-50'"
            :data-kind="k.kind || 'all'"
            @click="archiveKind = k.kind"
          >
            <span class="min-w-0 flex-1 truncate">{{ k.kind ? kindLabel(k.kind) : $t('Todos los problemas') }}</span>
            <span class="w-7 text-right text-emerald-800 tabular-nums" :title="$t('Seguros')">{{ k.clear }}</span>
            <span class="w-7 text-right text-sky-800 tabular-nums" :title="$t('A pensar')">{{ k.reason }}</span>
            <span class="w-7 text-right text-amber-800 tabular-nums" :title="$t('Mirar la foto')">{{ k.look }}</span>
          </button>
        </div>
        <div v-if="archive" class="grid grid-cols-2 gap-1.5">
          <select v-model="archiveFolder" class="min-w-0 rounded border border-stone-300 bg-white px-1.5 py-1 text-xs" :aria-label="$t('Carpeta')">
            <option value="">{{ $t('Todas las carpetas') }}</option>
            <option v-for="f in archiveFolders" :key="f.name" :value="f.name">{{ f.name.split('/').slice(-2).join('/') }} · {{ f.n }}</option>
          </select>
          <select v-model="archiveYear" class="min-w-0 rounded border border-stone-300 bg-white px-1.5 py-1 text-xs" :aria-label="$t('Año')">
            <option value="">{{ $t('Todos los años') }}</option>
            <option v-for="y in archiveYears" :key="y.name" :value="String(y.name)">{{ y.name }} · {{ y.n }}</option>
          </select>
        </div>
        <button v-if="archive" type="button" class="btn-primary w-full justify-center bg-emerald-700 hover:bg-emerald-800" :disabled="archiveBusy || !archiveCount" data-review-archive @click="reviewArchive">
          <Loader2 v-if="archiveBusy" :size="15" class="animate-spin" />
          {{ $tn(archiveCount, 'Revisar {n} caso', 'Revisar {n} casos') }}
        </button>
        <p v-if="archiveError" role="alert" class="text-xs text-red-800">{{ archiveError }} <button type="button" class="underline" @click="loadArchive">{{ $t('Reintentar') }}</button></p>
      </section>
      <!-- Photo_links: which folders to read. -->
      <details class="fold border-t border-stone-200 pt-3" :open="folds.read" data-read @toggle="fold('read', $event)">
        <summary class="flex cursor-pointer items-center gap-1.5 font-medium text-stone-800"><ChevronDown :size="14" class="chev text-stone-400" /><FolderSearch :size="15" class="text-emerald-700" /> Photo_links · {{ $t('Leer Drive') }}</summary>
        <div class="mt-2 space-y-2">
        <p class="text-xs text-stone-600">
          {{ $t('Lee las carpetas de fotos en Drive y las compara con la hoja: fotos nuevas, archivos renombrados, borrados o copiados a otra cuenta.') }}
        </p>
        <label class="flex items-center gap-2 text-xs">
          <input v-model="which" type="radio" value="all" class="text-emerald-700" />
          {{ $tn(folders.filter(f => !f.path.includes('/')).length, 'La carpeta listada', 'Las {n} carpetas listadas') }}
        </label>
        <label class="flex items-center gap-2 text-xs">
          <input v-model="which" type="radio" value="chosen" class="text-emerald-700" />
          {{ chosen.size ? $tn(chosen.size, 'Solo la elegida', 'Solo las {n} elegidas') : $t('Solo las que elija (más rápido)') }}
        </label>
        <div v-if="which === 'chosen'" class="rounded border border-stone-200">
          <input
            v-model="filter"
            type="search"
            class="w-full border-0 border-b border-stone-200 px-2 py-1 text-xs"
            :placeholder="$t('Buscar una carpeta (también las de dentro)')"
          />
          <ul class="max-h-56 overflow-y-auto py-0.5 text-xs" data-folder-list>
            <li v-for="f in shown" :key="f.path">
              <label class="flex items-center gap-2 px-2 py-0.5 hover:bg-stone-50">
                <input type="checkbox" class="rounded border-stone-300 text-emerald-700" :checked="chosen.has(f.path)" @change="toggle(f.path)" />
                <span class="min-w-0 flex-1 break-all">{{ f.path }}</span>
                <span class="text-stone-400 tabular-nums">{{ f.files }}</span>
              </label>
            </li>
            <li v-if="!shown.length" class="px-2 py-1 text-stone-500">{{ $t('Ninguna carpeta listada con ese nombre') }}</li>
          </ul>
        </div>
        <label class="block text-xs text-stone-600">
          {{ $t('Una carpeta nueva para listar (enlace de Drive):') }}
          <input v-model="added" type="text" class="mt-0.5 w-full rounded border border-stone-300 px-2 py-1 text-xs" placeholder="https://drive.google.com/drive/folders/…" />
        </label>
        <button class="btn-primary w-full justify-center bg-emerald-700 hover:bg-emerald-800" :disabled="reading" data-read-drive @click="read">
          <Loader2 v-if="reading" :size="15" class="animate-spin" /><FolderSearch v-else :size="15" />
          {{ reading ? $t('Leyendo Drive (hasta medio minuto)…') : $t('Leer Drive') }}
        </button>
        </div>
      </details>

      <!-- Photos uploaded without being renamed: their IDs read on the server. -->
      <details class="fold border-t border-stone-200 pt-3" :open="folds.unrenamed" data-unrenamed @toggle="fold('unrenamed', $event)">
        <summary class="flex cursor-pointer items-center gap-1.5 font-medium text-stone-800"><ChevronDown :size="14" class="chev text-stone-400" /><ScanText :size="15" class="text-emerald-700" /> {{ $t('Fotos subidas sin renombrar') }}</summary>
        <div class="mt-2 space-y-2">
        <p class="text-xs text-stone-600">
          {{ $t('El servidor lee el ID del sobre de cada foto (IMG_…), las ordena y te muestra solo las dudosas; se renombran al aplicar.') }}
        </p>
        <input
          v-model="uploaded"
          type="text"
          class="w-full rounded border border-stone-300 px-2 py-1 text-xs"
          :placeholder="$t('Enlace de la carpeta en Drive, o su ruta')"
          @keydown.enter="readIds"
        />
         <button class="btn w-full justify-center py-1" :disabled="readingIds" data-read-ids @click="readIds">
          <Loader2 v-if="readingIds" :size="14" class="animate-spin" /><ScanText v-else :size="14" /> {{ $t('Leer los IDs en el servidor') }}
        </button>
        </div>
      </details>

      <!-- The public sites. -->
      <details class="fold border-t border-stone-200 pt-3" :open="folds.sites || written" data-sites @toggle="fold('sites', $event)">
        <summary class="flex cursor-pointer items-center gap-1.5 font-medium text-stone-800">
          <ChevronDown :size="14" class="chev text-stone-400" /><Globe :size="15" class="text-emerald-700" /> {{ $t('Sitios públicos') }}
          <button class="btn-ghost ml-auto p-1" :title="$t('Ver de nuevo cómo van')" @click.prevent="loadSites(true)"><RefreshCw :size="13" /></button>
        </summary>
        <div class="mt-2 space-y-2">
        <p class="text-xs text-stone-600">{{ $t('Toman los datos del libro cuando se actualizan; después de escribir Photo_links, actualiza la Wings Gallery.') }}</p>
        <p v-if="written" class="rounded bg-emerald-50 px-2 py-1 text-xs text-emerald-900" data-next>
          {{ $t('Photo_links está escrito: actualiza la Wings Gallery para que muestre las fotos nuevas.') }}
        </p>
        <p v-if="!sitesReady" class="rounded bg-amber-50 px-2 py-1 text-xs text-amber-900">{{ $t('Este servidor no tiene la contraseña de las actualizaciones: solo se ve cómo van.') }}</p>
        <div v-for="(s, i) in sites" :key="s.site" class="rounded border border-stone-200 p-2 text-xs" :data-site="KEYS[i]">
          <p class="flex items-center gap-1.5">
            <a class="font-medium text-emerald-800 hover:underline" :href="s.url" target="_blank" rel="noopener">{{ s.site }} <ExternalLink :size="11" class="inline" /></a>
          </p>
          <p class="mt-0.5 text-stone-600">
            <template v-if="typeof s.lastUpdate === 'object'">
              {{ $t('Datos:') }} <a :href="s.lastUpdate.link" target="_blank" rel="noopener" class="hover:underline">{{ siteRunLabel(s.lastUpdate.state) }} · {{ when(s.lastUpdate.started) }}</a>
            </template>

            <template v-else>{{ $t('Nunca actualizado') }}</template>
          </p>
          <p v-if="s.publishing" class="text-stone-600">
            {{ $t('Publicación:') }}
            <a v-if="s.publishing.link" :href="s.publishing.link" target="_blank" rel="noopener" class="hover:underline">{{ siteRunLabel(s.publishing.state) }} · {{ when(s.publishing.started) }}</a>
            <span v-else>{{ siteRunLabel(s.publishing.state) }}</span>
          </p>
          <p v-if="KEYS[i] === 'gallery' && readings && readings.state !== 'off'" class="text-stone-600" data-readings>
            {{ $t('Lecturas de las fotos (especie, sexo, alas):') }}
            <template v-if="readings.batch">{{ $t('leyendo, {done} de {n} fotos', { done: readings.batch.done, n: readings.batch.photos }) }}</template>
            <template v-else-if="readings.last">{{ $t('{n} fotos leídas · {at}', { n: readings.last.readings, at: when(readings.last.endedAt) }) }}</template>
            <template v-else>{{ $t('al día') }}</template>
            <template v-if="readings.galleryUpdate?.due"> · {{ $t('el sitio se actualiza solo a las {at}', { at: when(readings.galleryUpdate.due) }) }}</template>
          </p>
          <label v-if="KEYS[i] === 'maps'" class="mt-1 flex items-center gap-1.5 text-stone-600">
            <input v-model="gbif" type="checkbox" class="rounded border-stone-300 text-emerald-700" />
            {{ $t('También GBIF e iNaturalist (un cuarto de hora más)') }}
          </label>
          <button
            class="mt-1.5 w-full justify-center py-1"
            :class="written && KEYS[i] === 'gallery' ? 'btn-primary bg-emerald-700 hover:bg-emerald-800' : 'btn'"
            :disabled="!sitesReady || !!starting || running(s)"
            :data-update="KEYS[i]"
            @click="update(KEYS[i])"
          >
            <Loader2 v-if="starting === KEYS[i] || running(s)" :size="13" class="animate-spin" /><Check v-else :size="13" />
            {{ running(s) ? $t('Actualizando…') : $t('Actualizar el sitio') }}
          </button>
        </div>
        </div>
      </details>
    </aside>

    <!-- The reading: the card of Cambios propuestos, applied here. -->
    <div ref="result" class="flex min-h-[75vh] min-w-0 flex-1 flex-col bg-stone-50 md:min-h-0">
      <p v-if="upToDate" class="m-3 flex items-center gap-1.5 rounded border border-emerald-200 bg-emerald-50 px-3 py-2 text-sm text-emerald-900" data-up-to-date>
        <Check :size="15" /> {{ $t('Photo_links ya está al día con esas carpetas (leídas a las {at}).', { at: upToDate }) }}
      </p>
      <!-- The cards this one replaced, to go back to. -->
      <div v-if="recent.length" class="relative flex justify-end px-3 pt-2 text-xs" data-recent>
        <button type="button" class="btn-ghost flex items-center gap-1 px-1.5 py-0.5 text-stone-600" :aria-expanded="recentOpen" @click="recentOpen = !recentOpen">
          <History :size="13" /> {{ $t('Tarjetas recientes') }} <ChevronDown :size="13" />
        </button>
        <ul v-if="recentOpen" class="absolute top-full right-3 z-30 mt-1 w-64 overflow-hidden rounded border border-stone-200 bg-white shadow-lg">
          <li v-for="r in recent" :key="r.id">
            <button type="button" class="w-full px-3 py-1.5 text-left hover:bg-stone-50" @click="showCard(r.id, r.kind)">{{ recentLabel(r) }}</button>
          </li>
        </ul>
      </div>
      <div v-if="extra.length" class="max-h-[55%] shrink-0 overflow-y-auto border-b-2 border-stone-300" data-extra-cards>
        <ProposalsLive v-for="id in extra" :key="id" layout="page" :only="id" class="h-[24rem] border-b border-stone-200" />
      </div>
      <ProposalsLive v-if="card" :key="card" layout="page" :only="card" class="min-h-0 flex-1" @count="onCount" />
      <p v-else-if="!upToDate" class="m-auto max-w-sm p-6 text-center text-sm text-stone-500">
        {{ $t('Elige una sección del archivo para revisarla, o pulsa «Leer Drive»: aquí verás lo que cambiaría antes de aplicarlo.') }}
      </p>
    </div>
  </div>
</template>
<style scoped>
.fold > summary {
  list-style: none;
}
.fold > summary::-webkit-details-marker {
  display: none;
}
.fold:not([open]) .chev {
  transform: rotate(-90deg);
}
</style>
