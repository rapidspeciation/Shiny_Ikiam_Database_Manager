<script setup lang="ts">
import { computed, defineAsyncComponent, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { Check, ExternalLink, FolderSearch, Globe, Loader2, RefreshCw } from 'lucide-vue-next'
import { api } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import { intlLocale, t } from '../lib/i18n'
import { siteRunLabel, type SiteStatus, type SitesAnswer } from '../lib/renameHost'

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
    if (out.proposalId) {
      card.value = out.proposalId
      localStorage.setItem(CARD_KEY, out.proposalId)
    } else {
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
watch(
  () => props.preset?.at,
  () => {
    if (!props.preset?.folders.length) return
    chosen.value = new Set(props.preset.folders)
    which.value = 'chosen'
    filter.value = ''
    void read()
  },
)

// ------------------------------------------------------------ the public sites
const sites = ref<SiteStatus[]>([])
const sitesReady = ref(true)
const starting = ref<'' | 'gallery' | 'maps'>('')
const gbif = ref(false)
let timer: ReturnType<typeof setTimeout> | null = null
const running = (s: SiteStatus) =>
  [s.update?.state, s.lastUpdate && typeof s.lastUpdate === 'object' ? s.lastUpdate.state : null, s.publishing?.state].includes('running')
async function loadSites(fresh = false) {
  if (timer) clearTimeout(timer)
  try {
    const out = await api<SitesAnswer>(`sites${fresh ? '?fresh=1' : ''}`)
    sites.value = out.sites
    sitesReady.value = out.ready
  } catch {
    // The section shows what it had.
  }
  // While a run goes on, asked again now and then (GitHub answers 60 times an hour to the server).
  if (sites.value.some(running)) timer = setTimeout(() => void loadSites(true), 45_000)
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

onMounted(() => {
  void loadFolders()
  void loadSites()
})
onBeforeUnmount(() => timer && clearTimeout(timer))
</script>

<template>
  <div class="flex min-h-0 flex-1 flex-col overflow-y-auto md:flex-row md:overflow-hidden" data-photo-links-panel>
    <aside class="flex shrink-0 flex-col gap-4 border-b border-stone-200 bg-white p-3 text-sm md:w-80 md:overflow-y-auto md:border-r md:border-b-0">
      <!-- Photo_links: which folders to read. -->
      <section class="space-y-2" data-read>
        <h2 class="font-medium text-stone-800">Photo_links</h2>
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
      </section>

      <!-- The public sites. -->
      <section class="space-y-2 border-t border-stone-200 pt-3" data-sites>
        <h2 class="flex items-center gap-1.5 font-medium text-stone-800">
          <Globe :size="15" class="text-emerald-700" /> {{ $t('Sitios públicos') }}
          <button class="btn-ghost ml-auto p-1" :title="$t('Ver de nuevo cómo van')" @click="loadSites(true)"><RefreshCw :size="13" /></button>
        </h2>
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
      </section>
    </aside>

    <!-- The reading: the card of Cambios propuestos, applied here. -->
    <div ref="result" class="flex min-h-[75vh] min-w-0 flex-1 flex-col bg-stone-50 md:min-h-0">
      <p v-if="upToDate" class="m-3 flex items-center gap-1.5 rounded border border-emerald-200 bg-emerald-50 px-3 py-2 text-sm text-emerald-900" data-up-to-date>
        <Check :size="15" /> {{ $t('Photo_links ya está al día con esas carpetas (leídas a las {at}).', { at: upToDate }) }}
      </p>
      <ProposalsLive v-if="card" :key="card" layout="page" :only="card" class="min-h-0 flex-1" @count="onCount" />
      <p v-else-if="!upToDate" class="m-auto max-w-sm p-6 text-center text-sm text-stone-500">
        {{ $t('Elige las carpetas y pulsa «Leer Drive»: aquí verás lo que cambiaría en Photo_links antes de aplicarlo.') }}
      </p>
    </div>
  </div>
</template>
