<script setup lang="ts">
// The taxon search of Identificar: one box for a genus, a species or a subspecies (lib/taxonSearch.ts). A genus shows
// its species with photos; a species its photo, more photos of it and its subspecies' photos; a subspecies its photo
// and the rest of its species (taxonView). What the phone has comes first (the installed pack's names and photos, no
// network), then with signal the server's whole list and, signed in, the team's photo search are merged in. A photo
// opens full screen with the name large (PhotoViewer). Photos from the person's country come first (`home`: the side of
// the Andes or Descargas' country, lib/photoCountry.ts), then its neighbours', then the rest; a subspecies chosen puts
// the photos from its range first. Each photo names its country with its credit.
// The address holds the state: the taxon (?taxon=, a taxon chosen from another is a new history entry, so Back returns
// to the genus) and the full-screen photo (?foto=, so the phone's Back closes it).
import { computed, nextTick, onMounted, ref, shallowRef, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { Search, WifiOff, X } from 'lucide-vue-next'
import { t } from '../../lib/i18n'
import { installedPack, numText, onlineIndex } from '../../lib/photoPacks'
import { orderRefs, referencePhotos, savedPhotos, type Refs } from '../../lib/identifyRefs'
import { countryName } from '../../lib/usePacks'
import type { Place } from '../../lib/photoCountry'
import { catalog, search, taxonView, type Card, type Catalog, type Hit } from '../../lib/taxonSearch'
import PhotoViewer, { type ViewerPhoto } from './PhotoViewer.vue'

const props = withDefaults(defineProps<{ signedIn: boolean; online: boolean; home?: string[] }>(), { home: () => [] })
const emit = defineEmits<{ downloads: [] }>()
const route = useRoute()
const router = useRouter()

const cat = shallowRef<Catalog | null>(null)
const packCount = ref(0)
const loaded = ref(false)
/** The installed pack's names at once; the server's list merged in after, with signal. */
async function loadCatalog() {
  const pack = await installedPack()
  packCount.value = pack?.installed.photos ?? 0
  cat.value = catalog(pack?.index.taxa ?? [])
  loaded.value = true
  fromAddress()
  if (!props.online) return
  const online = await onlineIndex().catch(() => null)
  if (online) {
    cat.value = catalog(pack?.index.taxa ?? [], online.taxa)
    fromAddress()
  }
}

const SUGGESTIONS = 5
const query = ref('')
const open = ref(false)
const active = ref(0)
const hits = computed<Hit[]>(() => (cat.value ? search(cat.value, query.value, SUGGESTIONS) : []))
watch(hits, () => (active.value = 0))
const chosen = ref<Hit | null>(null)
const input = ref<HTMLInputElement | null>(null)

const RANK = { genus: 'Género', species: 'Especie', subspecies: 'Subespecie' } as const
const rankWord = (r: Hit['rank']) => t(RANK[r])
const familyOf = (h: Hit) => (h.group === 'ithomiini' ? 'Ithomiini' : h.family)

const addressTaxon = () => (typeof route.query.taxon === 'string' ? route.query.taxon : null)
/** The taxon in the address shown (after Back, a reload, a shared link); none: the empty box. */
function fromAddress() {
  const name = addressTaxon()
  if ((chosen.value?.name ?? null) === name || !cat.value) return
  const hit = name ? search(cat.value, name, 1).find(h => h.name === name) : null
  if (name && !hit) return // not in the lists (yet): the server's list may bring it
  chosen.value = hit ?? null
  query.value = hit?.name ?? ''
}
watch(() => route.query.taxon, fromAddress)

function pick(h: Hit | undefined) {
  if (!h) return
  open.value = false
  input.value?.blur()
  if (h.name === chosen.value?.name) return
  const { taxon: _t, foto: _f, ...rest } = route.query
  // From another taxon shown (tapped or typed): a new entry, so Back returns to it; the first one replaces the empty box.
  const to = { query: { ...rest, taxon: h.name } }
  if (addressTaxon()) router.push(to)
  else router.replace(to)
  chosen.value = h
  query.value = h.name
}
function onKey(e: KeyboardEvent) {
  if (e.key === 'ArrowDown') {
    e.preventDefault()
    open.value = true
    active.value = Math.min(hits.value.length - 1, active.value + 1)
  } else if (e.key === 'ArrowUp') {
    e.preventDefault()
    active.value = Math.max(0, active.value - 1)
  } else if (e.key === 'Enter') {
    e.preventDefault()
    pick(hits.value[active.value])
  } else if (e.key === 'Escape') open.value = false
}
function clear() {
  query.value = ''
  chosen.value = null
  const { taxon: _t, foto: _f, ...rest } = route.query
  router.replace({ query: rest })
  nextTick(() => input.value?.focus())
}
function onInput() {
  open.value = true
  if (chosen.value && query.value !== chosen.value.name) chosen.value = null
}

// ---------------------------------------------------------------- the chosen taxon's photos

const names = computed(() =>
  !chosen.value ? [] : chosen.value.rank === 'genus' ? (cat.value?.speciesOf.get(chosen.value.name) ?? []) : [chosen.value.name.split(' ').slice(0, 2).join(' ')],
)
const refs = ref<Record<string, Refs>>({})
const loading = ref(false)
let asked = 0
/** The server is asked this many names at a time (a big genus is several calls, each answered as it comes). */
const BATCH = 20
async function loadPhotos() {
  const list = names.value
  const ticket = ++asked
  refs.value = {}
  if (!list.length) return
  loading.value = true
  try {
    // The phone's photos at once.
    const saved = await savedPhotos(list)
    if (ticket !== asked) return
    refs.value = saved
    if (!props.online) return
    for (let i = 0; i < list.length; i += BATCH) {
      let part = list.slice(i, i + BATCH)
      for (let round = 0; round < 3 && part.length; round++) {
        if (round) await new Promise(r => setTimeout(r, 4000))
        // The server's whole list beside the pack for every name: a subspecies outside the pack shows too.
        const got = await referencePhotos(part, { signedIn: props.signedIn, everySource: true })
        if (ticket !== asked) return
        // A name the phone had photos of keeps them first; the server's add to them.
        const merged = { ...refs.value }
        for (const [n, r] of Object.entries(got.refs)) if (r.photos.length || !merged[n]) merged[n] = r
        refs.value = merged
        part = got.pending
      }
    }
  } finally {
    if (ticket === asked) loading.value = false
  }
}
watch([names, () => props.online], loadPhotos)

const broken = ref(new Set<string>())
const fail = (src: string) => (broken.value = new Set([...broken.value, src]))
/** The person's countries, and a subspecies' range when one is chosen. */
const place = computed<Place>(() => ({
  home: props.home,
  range: chosen.value?.rank === 'subspecies' ? (cat.value?.taxa.get(chosen.value.name)?.countries ?? null) : null,
}))
const shownRefs = computed(() =>
  Object.fromEntries(Object.entries(refs.value).map(([n, r]) => [n, { photos: orderRefs(r.photos.filter(p => !broken.value.has(p.src)), place.value) }])),
)
/** «© Ana · CC-BY · Ecuador»: the credit line of a photo (a preserved specimen named first, with its ID). */
const creditLine = (p: Card['photo'], license = true) =>
  [p.kind === 'preserved' ? [t('Ejemplar preservado'), p.specimen].filter(Boolean).join(' ') : null, p.credit, license ? p.license : null, p.country ? countryName(p.country) : null]
    .filter(Boolean)
    .join(' · ')
const view = computed(() => (chosen.value && cat.value ? taxonView(cat.value, chosen.value, shownRefs.value) : null))
const lead = computed(() => view.value?.lead ?? null)
const sections = computed(() => view.value?.sections ?? [])
const cards = computed<Card[]>(() => [...(lead.value ? [lead.value] : []), ...sections.value.flatMap(s => s.cards)])
const noPhoto = computed(() => (loading.value ? [] : (view.value?.noPhoto ?? [])))
const sectionTitle = (id: 'species' | 'more' | 'ssp', n: number) =>
  id === 'species' ? t('Especies con foto ({n})', { n }) : id === 'more' ? t('Más fotos de la especie') : chosen.value?.rank === 'subspecies' ? t('Otras subespecies') : t('Subespecies')
const kickerText = (c: Card) =>
  c.kicker === 'specimen' ? [t('Ejemplar del proyecto'), c.photo.credit, c.photo.sex].filter(Boolean).join(' · ') : c.kicker === 'subspecies' ? t('Subespecie') : c.kicker === 'species' ? t('Especie') : ''

// ---------------------------------------------------------------- full screen (?foto=)

const viewerStart = ref<number | null>(null)
const viewerOpen = computed(() => route.query.foto != null && viewerStart.value != null)
const viewerPhotos = computed<ViewerPhoto[]>(() =>
  cards.value.map(c => ({
    src: c.photo.src,
    name: c.name,
    kicker: c.kicker === 'specimen' || c.kicker === 'subspecies' ? kickerText(c) : undefined,
    credit: c.photo.credit,
    license: c.photo.license,
    source: c.photo.source ?? null,
    country: c.photo.country ?? null,
    preserved: c.photo.kind === 'preserved' ? (c.photo.specimen ?? '') : null,
  })),
)
function openCard(c: Card) {
  viewerStart.value = cards.value.indexOf(c)
  router.push({ query: { ...route.query, foto: '1' } })
}
/** Closed with the ×, Esc or the phone's Back: the entry it opened goes. */
function closeViewer() {
  if (route.query.foto != null) router.back()
}
// Opened from an address with ?foto= (a reload): nothing to step through, the entry is dropped.
watch(
  () => route.query.foto,
  f => {
    if (f == null) viewerStart.value = null
    else if (viewerStart.value == null) {
      const { foto: _f, ...rest } = route.query
      router.replace({ query: rest })
    }
  },
  { immediate: true },
)

const goTo = (name: string) => cat.value && pick(search(cat.value, name, 1)[0])

onMounted(loadCatalog)
watch(
  () => props.online,
  () => loadCatalog(),
)
</script>

<template>
  <section class="max-w-3xl" data-testid="taxon-search">
    <div class="relative">
      <label for="taxon-q" class="sr-only">{{ $t('Buscar un taxón') }}</label>
      <Search :size="20" class="pointer-events-none absolute top-1/2 left-3 -translate-y-1/2 text-fg-muted" />
      <input
        id="taxon-q"
        ref="input"
        v-model="query"
        type="text"
        inputmode="search"
        enterkeyhint="search"
        role="combobox"
        autocomplete="off"
        autocapitalize="off"
        spellcheck="false"
        :aria-expanded="open && hits.length > 0 && !chosen"
        aria-controls="taxon-hits"
        :aria-activedescendant="open && hits.length ? `taxon-hit-${active}` : undefined"
        :placeholder="$t('Género, especie o subespecie')"
        class="block h-12 w-full rounded-lg border-2 border-control bg-surface pr-12 pl-10 text-base text-fg placeholder:text-fg-subtle focus:border-focus focus:outline-none"
        @input="onInput"
        @focus="open = true"
        @keydown="onKey"
      />
      <button v-if="query" class="absolute top-1/2 right-1 grid size-11 -translate-y-1/2 place-items-center rounded-md text-fg-muted hover:bg-surface-sunken" :aria-label="$t('Borrar')" @click="clear">
        <X :size="20" />
      </button>
      <!-- A row is chosen by a tap (click), never by the touch that starts a scroll; mousedown kept from taking the focus. -->
      <ul
        v-if="open && hits.length && !chosen"
        id="taxon-hits"
        role="listbox"
        class="absolute inset-x-0 top-full z-30 mt-1 overflow-hidden rounded-lg border border-border-strong bg-surface shadow-lg"
      >
        <li
          v-for="(h, i) in hits"
          :id="`taxon-hit-${i}`"
          :key="h.name"
          role="option"
          :aria-selected="i === active"
          class="flex min-h-12 cursor-pointer items-center justify-between gap-3 border-b border-border px-3 py-2 last:border-b-0"
          :class="i === active ? 'bg-brand-soft' : 'hover:bg-surface-sunken'"
          @mousedown.prevent
          @click="pick(h)"
        >
          <i class="min-w-0 truncate text-base">{{ h.name }}</i>
          <span class="shrink-0 text-xs text-fg-muted">{{ rankWord(h.rank) }}<template v-if="familyOf(h)"> · {{ familyOf(h) }}</template></span>
        </li>
      </ul>
    </div>

    <p v-if="!online" class="mt-2 flex items-center gap-1.5 text-sm text-fg-muted">
      <WifiOff :size="16" class="shrink-0" />
      <template v-if="packCount">{{ $t('Sin señal: busca en las {n} fotos guardadas.', { n: numText(packCount) }) }}</template>
      <template v-else-if="loaded">
        {{ $t('Sin señal y sin fotos guardadas.') }}
        <button class="min-h-11 font-medium text-brand-text underline underline-offset-2" @click="emit('downloads')">{{ $t('Descargas') }}</button>
      </template>
    </p>
    <p v-if="query.trim().length >= 2 && !hits.length && loaded && !chosen" class="mt-2 text-sm text-fg-muted" data-testid="no-match">
      {{ online ? $t('Ningún taxón con ese nombre.') : $t('Ningún taxón con ese nombre en las fotos guardadas.') }}
    </p>

    <!-- The chosen taxon -->
    <template v-if="chosen">
      <header class="mt-4 flex flex-wrap items-baseline gap-x-2">
        <h2 class="text-2xl leading-tight font-semibold"><i>{{ chosen.name }}</i></h2>
        <span class="text-sm text-fg-muted">{{ rankWord(chosen.rank) }}<template v-if="familyOf(chosen)"> · {{ familyOf(chosen) }}</template></span>
      </header>
      <p v-if="loading && !cards.length" class="mt-3 text-fg-muted" role="status">{{ $t('Buscando fotos…') }}</p>
      <p v-else-if="!loading && !cards.length" class="mt-3 rounded-lg border border-border bg-surface p-3 text-fg-muted">
        {{ online ? $t('No encontramos fotos de este taxón.') : $t('No hay fotos guardadas de este taxón en el teléfono.') }}
      </p>

      <!-- A species or subspecies: its photo large -->
      <button v-if="lead" class="photo-card mt-3 block w-full overflow-hidden rounded-xl border-2 border-brand bg-surface text-left lg:max-w-xl" data-testid="lead-photo" @click="openCard(lead)">
        <img :src="lead.photo.src" alt="" class="aspect-[4/3] w-full bg-photo-mat object-cover" @error="fail(lead.photo.src)" />
        <span class="block px-3 py-2">
          <span v-if="lead.name !== chosen.name || lead.kicker === 'specimen'" class="block text-xs font-medium text-fg-muted uppercase">{{ kickerText(lead) }}</span>
          <i class="block text-lg font-semibold">{{ lead.name }}</i>
          <span class="block truncate text-xs text-fg-muted" data-testid="lead-credit">{{ creditLine(lead.photo) }}</span>
        </span>
      </button>

      <template v-for="s in sections" :key="s.id">
        <h3 class="mt-5 mb-2 font-semibold">{{ sectionTitle(s.id, s.cards.length) }}</h3>
        <ul class="grid grid-cols-2 gap-3 sm:grid-cols-3" :data-testid="`photo-grid-${s.id}`" data-grid="photos">
          <li v-for="c in s.cards" :key="c.key">
            <button class="photo-card block w-full overflow-hidden rounded-lg border border-border-strong bg-surface text-left" @click="openCard(c)">
              <img :src="c.photo.src" alt="" loading="lazy" class="aspect-square w-full bg-photo-mat object-cover" @error="fail(c.photo.src)" />
              <span class="block px-2 py-1.5">
                <i v-if="c.label" class="block text-[0.95rem] leading-snug font-medium break-words">{{ c.label }}</i>
                <span v-if="c.viaSsp" class="block text-xs text-fg-muted">{{ $t('Foto de la subespecie') }} <i>{{ c.viaSsp }}</i></span>
                <span v-if="!c.label && c.kicker === 'specimen'" class="block text-xs text-fg-muted">{{ kickerText(c) }}</span>
                <span v-else-if="!c.label" class="block truncate text-xs text-fg-muted">{{ creditLine(c.photo, false) }}</span>
                <span v-else-if="c.photo.country || c.photo.kind === 'preserved'" class="block truncate text-xs text-fg-muted">{{
                  [c.photo.kind === 'preserved' ? $t('Ejemplar preservado') : null, c.photo.country ? countryName(c.photo.country) : null].filter(Boolean).join(' · ')
                }}</span>
              </span>
            </button>
          </li>
        </ul>
      </template>

      <div v-if="noPhoto.length" class="mt-4 rounded-lg border border-border bg-surface p-3 text-sm">
        <p class="font-medium text-fg-muted">{{ $t('Sin foto aquí') }}</p>
        <ul class="mt-1 flex flex-wrap gap-x-1 gap-y-0">
          <li v-for="n in noPhoto" :key="n">
            <button class="min-h-11 rounded px-1.5 text-left text-brand-text hover:bg-surface-sunken" @click="goTo(n)">
              <i>{{ chosen.rank === 'genus' ? n.split(' ').slice(1).join(' ') : n.split(' ').slice(2).join(' ') }}</i>
            </button>
          </li>
        </ul>
      </div>
      <p v-if="chosen.rank === 'subspecies'" class="mt-3">
        <button class="min-h-11 text-brand-text underline-offset-2 hover:underline" @click="goTo(chosen.name.split(' ').slice(0, 2).join(' '))">
          {{ $t('Ver la especie') }} <i>{{ chosen.name.split(' ').slice(0, 2).join(' ') }}</i>
        </button>
      </p>
      <p v-if="chosen.rank !== 'genus'" class="mt-1">
        <button class="min-h-11 text-brand-text underline-offset-2 hover:underline" @click="goTo(chosen.name.split(' ')[0])">{{ $t('Ver el género') }} <i>{{ chosen.name.split(' ')[0] }}</i></button>
      </p>
    </template>
    <p v-else-if="!query" class="mt-3 text-sm text-fg-muted">{{ $t('Escriba un nombre para ver sus fotos. Toque una foto para verla en grande.') }}</p>

    <PhotoViewer v-if="viewerOpen" :photos="viewerPhotos" :start="viewerStart!" @close="closeViewer" />
  </section>
</template>

<style scoped>
.photo-card {
  transition:
    transform 0.15s ease,
    box-shadow 0.15s ease;
}
.photo-card img {
  transition: transform 0.25s ease;
}
@media (hover: hover) {
  .photo-card:hover {
    transform: translateY(-2px);
    box-shadow: 0 6px 18px rgb(0 0 0 / 0.14);
  }
  .photo-card:hover img {
    transform: scale(1.03);
  }
}
.photo-card:active {
  transform: scale(0.98);
}
</style>
