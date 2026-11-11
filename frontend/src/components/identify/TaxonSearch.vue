<script setup lang="ts">
// The taxon search of Identificar: one box for a genus, a species or a subspecies (lib/taxonSearch.ts). A genus shows
// its species with photos; a species its photo and its subspecies' photos; a subspecies its photo and its siblings'.
// Photos: the installed pack without signal, and with signal also the server's whole list and, signed in, the team's
// photo search (lib/identifyRefs.ts referencePhotos). A photo opens full screen with the name large (PhotoViewer).
import { computed, nextTick, onMounted, ref, shallowRef, watch } from 'vue'
import { Search, WifiOff, X } from 'lucide-vue-next'
import { t } from '../../lib/i18n'
import { installedPack, onlineIndex } from '../../lib/photoPacks'
import { referencePhotos, type RefPhoto, type Refs } from '../../lib/identifyRefs'
import { catalog, search, type Catalog, type Hit } from '../../lib/taxonSearch'
import PhotoViewer, { type ViewerPhoto } from './PhotoViewer.vue'

const props = defineProps<{ signedIn: boolean; online: boolean; initial?: string | null }>()
const emit = defineEmits<{ downloads: []; chosen: [name: string | null] }>()

const cat = shallowRef<Catalog | null>(null)
const packCount = ref(0)
const loaded = ref(false)
async function loadCatalog() {
  const pack = await installedPack()
  const online = props.online ? await onlineIndex().catch(() => null) : null
  packCount.value = pack?.installed.photos ?? 0
  cat.value = catalog(pack?.index.taxa ?? [], online?.taxa ?? [])
  loaded.value = true
}

const query = ref('')
const open = ref(false)
const active = ref(0)
const hits = computed<Hit[]>(() => (cat.value ? search(cat.value, query.value, 8) : []))
watch(hits, () => (active.value = 0))
const chosen = ref<Hit | null>(null)
const input = ref<HTMLInputElement | null>(null)

const RANK = { genus: 'Género', species: 'Especie', subspecies: 'Subespecie' } as const
const rankWord = (r: Hit['rank']) => t(RANK[r])
const familyOf = (h: Hit) => (h.group === 'ithomiini' ? 'Ithomiini' : h.family)

function pick(h: Hit | undefined) {
  if (!h) return
  chosen.value = h
  query.value = h.name
  open.value = false
  emit('chosen', h.name)
  input.value?.blur()
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
  emit('chosen', null)
  nextTick(() => input.value?.focus())
}
function onInput() {
  open.value = true
  if (chosen.value && query.value !== chosen.value.name) chosen.value = null
}

// ---------------------------------------------------------------- what the chosen taxon shows

const speciesOfGenus = computed(() => (chosen.value?.rank === 'genus' ? (cat.value?.speciesOf.get(chosen.value.name) ?? []) : []))
const speciesName = computed(() => (!chosen.value || chosen.value.rank === 'genus' ? null : chosen.value.name.split(' ').slice(0, 2).join(' ')))
const refs = ref<Record<string, Refs>>({})
const loading = ref(false)
let asked = 0
async function loadPhotos() {
  const names = chosen.value?.rank === 'genus' ? speciesOfGenus.value : speciesName.value ? [speciesName.value] : []
  if (!names.length) return
  const ticket = ++asked
  loading.value = true
  try {
    let got = await referencePhotos(names, { signedIn: props.signedIn })
    if (ticket !== asked) return
    refs.value = got.refs
    // The server still looking some up: asked once more.
    if (got.pending.length) {
      await new Promise(r => setTimeout(r, 4000))
      if (ticket !== asked) return
      got = await referencePhotos(got.pending, { signedIn: props.signedIn })
      if (ticket === asked) refs.value = { ...refs.value, ...got.refs }
    }
  } finally {
    if (ticket === asked) loading.value = false
  }
}
watch([chosen, () => props.online], () => {
  refs.value = {}
  loadPhotos()
})

const broken = ref(new Set<string>())
const fail = (src: string) => (broken.value = new Set([...broken.value, src]))
const usable = (p: RefPhoto) => !broken.value.has(p.src) && p.level !== 'genus'
/** A species' own photos (not its subspecies'). */
const ownPhotos = (sp: string) => (refs.value[sp]?.photos ?? []).filter(p => usable(p) && p.level !== 'subspecies')
const sspPhotos = (sp: string) => (refs.value[sp]?.photos ?? []).filter(p => usable(p) && p.level === 'subspecies')

interface Card {
  key: string
  name: string
  kicker?: string
  photo: RefPhoto
}
const kickerOf = (p: RefPhoto) => (p.level === 'specimen' ? t('Ejemplar del proyecto') : p.level === 'subspecies' ? t('Subespecie') : undefined)
/** The cards shown, in order; the viewer steps through the same list. */
const cards = computed<Card[]>(() => {
  const h = chosen.value
  if (!h) return []
  if (h.rank === 'genus')
    return speciesOfGenus.value.flatMap(sp => {
      const p = ownPhotos(sp)[0]
      return p ? [{ key: sp, name: sp, photo: p }] : []
    })
  const sp = speciesName.value!
  const own = ownPhotos(sp).map((p, i) => ({ key: `${sp}#${i}`, name: sp, kicker: kickerOf(p), photo: p }))
  const ssp = sspPhotos(sp).map(p => ({ key: p.taxon ?? p.src, name: p.taxon ?? sp, kicker: t('Subespecie'), photo: p }))
  if (h.rank === 'subspecies') {
    const mine = ssp.filter(c => c.name === h.name)
    return [...mine, ...own, ...ssp.filter(c => c.name !== h.name)]
  }
  return [...own, ...ssp]
})
/** The first card of a species or subspecies is the big one; the rest go in the grid. */
const lead = computed(() => (chosen.value && chosen.value.rank !== 'genus' ? (cards.value[0] ?? null) : null))
const leadIsTaxon = computed(() => !!lead.value && lead.value.name === chosen.value?.name)
const grid = computed(() => (lead.value ? cards.value.slice(1) : cards.value))
/** Names the lists know but with no photo here. */
const noPhoto = computed(() => {
  const h = chosen.value
  if (!h || loading.value) return []
  if (h.rank === 'genus') return speciesOfGenus.value.filter(sp => !ownPhotos(sp).length)
  const sp = speciesName.value!
  const withPhoto = new Set(sspPhotos(sp).map(p => p.taxon))
  return (cat.value?.sspOf.get(sp) ?? []).filter(n => !withPhoto.has(n))
})
const gridTitle = computed(() => {
  const h = chosen.value
  if (!h || !grid.value.length) return null
  if (h.rank === 'genus') return t('Especies con foto ({n})', { n: grid.value.length })
  return h.rank === 'species' ? t('Subespecies') : t('Más fotos de la especie')
})

const viewer = ref<number | null>(null)
const viewerPhotos = computed<ViewerPhoto[]>(() =>
  cards.value.map(c => ({ src: c.photo.src, name: c.name, kicker: c.kicker, credit: c.photo.credit, license: c.photo.license, source: c.photo.source ?? null })),
)
const openCard = (c: Card) => (viewer.value = cards.value.indexOf(c))
const goTo = (name: string) => cat.value && pick(search(cat.value, name, 1)[0])

const epithetOf = (name: string, h: Hit | null) => (h?.rank === 'genus' ? name.split(' ').slice(1).join(' ') : name)

onMounted(async () => {
  await loadCatalog()
  if (props.initial && cat.value) {
    const hit = search(cat.value, props.initial, 1)[0]
    if (hit) pick(hit)
  }
})
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
        type="search"
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
          @pointerdown.prevent="pick(h)"
        >
          <i class="min-w-0 truncate text-base">{{ h.name }}</i>
          <span class="shrink-0 text-xs text-fg-muted">{{ rankWord(h.rank) }}<template v-if="familyOf(h)"> · {{ familyOf(h) }}</template></span>
        </li>
      </ul>
    </div>

    <p v-if="!online" class="mt-2 flex items-center gap-1.5 text-sm text-fg-muted">
      <WifiOff :size="16" class="shrink-0" />
      <template v-if="packCount">{{ $t('Sin señal: busca en las {n} fotos guardadas.', { n: packCount }) }}</template>
      <template v-else-if="loaded">
        {{ $t('Sin señal y sin fotos guardadas.') }}
        <button class="font-medium text-brand-text underline underline-offset-2" @click="emit('downloads')">{{ $t('Descargas') }}</button>
      </template>
    </p>
    <p v-else-if="query.trim().length >= 2 && !hits.length && loaded && !chosen" class="mt-2 text-sm text-fg-muted">{{ $t('Ningún taxón con ese nombre.') }}</p>

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
      <button v-if="lead" class="photo-card mt-3 block w-full overflow-hidden lg:max-w-xl rounded-xl border-2 border-brand bg-surface text-left" data-testid="lead-photo" @click="openCard(lead)">
        <img :src="lead.photo.src" alt="" class="aspect-[4/3] w-full bg-photo-mat object-cover" @error="fail(lead.photo.src)" />
        <span class="block px-3 py-2">
          <span v-if="!leadIsTaxon" class="block text-xs font-medium text-fg-muted uppercase">{{ lead.kicker ?? $t('Especie') }}</span>
          <i class="block text-lg font-semibold">{{ lead.name }}</i>
          <span class="block truncate text-xs text-fg-muted">{{ lead.photo.credit }}<template v-if="lead.photo.license"> · {{ lead.photo.license }}</template></span>
        </span>
      </button>

      <h3 v-if="gridTitle" class="mt-5 mb-2 font-semibold">{{ gridTitle }}</h3>
      <ul v-if="grid.length" class="grid grid-cols-2 gap-3 sm:grid-cols-3" data-testid="photo-grid">
        <li v-for="c in grid" :key="c.key">
          <button class="photo-card block w-full overflow-hidden rounded-lg border border-border-strong bg-surface text-left" @click="openCard(c)">
            <img :src="c.photo.src" alt="" loading="lazy" class="aspect-square w-full bg-photo-mat object-cover" @error="fail(c.photo.src)" />
            <span class="block px-2 py-1.5">
              <i class="block text-[0.95rem] leading-snug font-medium break-words">{{ epithetOf(c.name, chosen) }}</i>
              <span v-if="c.kicker && chosen.rank !== 'species'" class="block text-xs text-fg-muted">{{ c.kicker }}</span>
            </span>
          </button>
        </li>
      </ul>

      <div v-if="noPhoto.length" class="mt-4 rounded-lg border border-border bg-surface p-3 text-sm">
        <p class="font-medium text-fg-muted">{{ $t('Sin foto aquí') }}</p>
        <ul class="mt-1 flex flex-wrap gap-x-1 gap-y-0">
          <li v-for="n in noPhoto" :key="n">
            <button class="min-h-11 rounded px-1.5 text-left text-brand-text hover:bg-surface-sunken" @click="goTo(n)"><i>{{ epithetOf(n, chosen) }}</i></button>
          </li>
        </ul>
      </div>
      <p v-if="speciesName && chosen.rank === 'subspecies'" class="mt-3">
        <button class="min-h-11 text-brand-text underline-offset-2 hover:underline" @click="goTo(speciesName)">{{ $t('Ver la especie') }} <i>{{ speciesName }}</i></button>
      </p>
      <p v-if="chosen.rank !== 'genus'" class="mt-1">
        <button class="min-h-11 text-brand-text underline-offset-2 hover:underline" @click="goTo(chosen.name.split(' ')[0])">{{ $t('Ver el género') }} <i>{{ chosen.name.split(' ')[0] }}</i></button>
      </p>
    </template>
    <p v-else-if="!query" class="mt-3 text-sm text-fg-muted">{{ $t('Escriba un nombre para ver sus fotos. Toque una foto para verla en grande.') }}</p>

    <PhotoViewer v-if="viewer != null" :photos="viewerPhotos" :start="viewer" @close="viewer = null" />
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
