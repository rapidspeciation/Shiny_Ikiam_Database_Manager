<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { ChevronLeft, ChevronRight, MapPinned, X } from 'lucide-vue-next'
import FilterSelect, { type FilterOption } from './FilterSelect.vue'
import { useMonitoring } from '../../composables/useMonitoring'
import { formatSerial } from '../../lib/dates'
import { formatMinutes, markHistories } from '../../lib/monitoring'
import { individuals, type Individual } from '../../lib/monitoringMap'

/**
 * "Recapturas": every marked butterfly caught more than once, with the photos
 * of its first capture and of each recapture side by side, so the mark and the
 * species can be checked by eye.
 */
const { rows, tracks } = useMonitoring()
const route = useRoute()
const router = useRouter()

const all = computed(() => individuals(markHistories(rows.value), tracks.value))

const chosen = computed({
  get: () => String(route.query.individuo ?? ''),
  set: v => router.replace({ query: { ...route.query, individuo: v || undefined } }),
})
const search = ref('')
const species = ref<string[]>([])
const onlyPhotos = ref(false)
const sort = ref<'recent' | 'captures' | 'span'>('recent')

const speciesOptions = computed<FilterOption[]>(() => {
  const n = new Map<string, number>()
  for (const i of all.value) {
    const s = String(i.events[0].row.values.SPECIES ?? '')
    n.set(s, (n.get(s) || 0) + 1)
  }
  return [...n]
    .sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]))
    .map(([s, count]) => ({ value: s, label: s, count, italic: true }))
})
const shown = computed(() => {
  if (chosen.value) return all.value.filter(i => i.key === chosen.value)
  const q = search.value.trim().toLowerCase()
  const list = all.value.filter(
    i =>
      (!q || `${i.id} ${i.species}`.toLowerCase().includes(q)) &&
      (!species.value.length || species.value.includes(String(i.events[0].row.values.SPECIES ?? ''))) &&
      (!onlyPhotos.value || i.photos > 0),
  )
  const last = (i: Individual) => i.events.at(-1)?.date ?? 0
  return [...list].sort(
    sort.value === 'captures'
      ? (a, b) => b.events.length - a.events.length || last(b) - last(a)
      : sort.value === 'span'
        ? (a, b) => (b.span ?? 0) - (a.span ?? 0)
        : (a, b) => last(b) - last(a),
  )
})
const withPhotos = computed(() => all.value.filter(i => i.photos > 0).length)

const date = (serial: number | null) => (serial === null ? '—' : formatSerial(serial))
const sexLabel = (s: string) => (/^f/i.test(s) ? 'hembra' : /^m/i.test(s) ? 'macho' : '')
const photoUrl = (id: string) => `api/monitoring/photos/${id}`
function onMap(i: Individual) {
  router.replace({ query: { vista: 'mapa', individuo: i.key, unir: '1' } })
}

// ------------------------------------------------------------ enlarged photo
const viewer = ref<{ individual: Individual; index: number } | null>(null)
const viewerPhotos = computed(() =>
  viewer.value ? viewer.value.individual.events.flatMap((e, n) => e.photos.map(id => ({ id, event: e, capture: n + 1 }))) : [],
)
const current = computed(() => (viewer.value ? viewerPhotos.value[viewer.value.index] : null))
function open(i: Individual, id: string) {
  const index = i.events.flatMap(e => e.photos).indexOf(id)
  viewer.value = { individual: i, index: Math.max(0, index) }
}
function step(by: number) {
  if (!viewer.value) return
  const n = viewerPhotos.value.length
  viewer.value.index = (viewer.value.index + by + n) % n
}
function onKey(event: KeyboardEvent) {
  if (!viewer.value) return
  if (event.key === 'Escape') viewer.value = null
  else if (event.key === 'ArrowRight') step(1)
  else if (event.key === 'ArrowLeft') step(-1)
}
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <div class="h-full overflow-y-auto">
    <div class="toolbar">
      <template v-if="chosen">
        <button class="btn" @click="chosen = ''"><ChevronLeft :size="14" /> Todos los individuos</button>
      </template>
      <template v-else>
        <label class="block w-44">
          <span class="field-label">Marca o especie</span>
          <input v-model="search" class="field-input" placeholder="p. ej. B39" />
        </label>
        <div class="w-64">
          <FilterSelect v-model="species" label="Especies" :options="speciesOptions" all-label="Todas las especies" />
        </div>
        <label class="block">
          <span class="field-label">Ordenar</span>
          <select v-model="sort" class="field-input">
            <option value="recent">Última captura más reciente</option>
            <option value="captures">Más capturas</option>
            <option value="span">Más tiempo entre la primera y la última</option>
          </select>
        </label>
        <label class="flex items-center gap-2 pb-2 text-sm">
          <input v-model="onlyPhotos" type="checkbox" /> Solo con fotos ({{ withPhotos }})
        </label>
      </template>
      <p class="hint ml-auto pb-2">
        {{ shown.length }} de {{ all.length }} individuos recapturados (misma marca y misma especie)
      </p>
    </div>

    <div class="space-y-3 p-3 sm:p-4">
      <p v-if="!all.length" class="hint">Aún no hay recapturas en Collection_data.</p>
      <section v-for="i in shown" :key="i.key" class="rounded-md border border-stone-200 bg-white">
        <header class="flex flex-wrap items-baseline gap-x-3 gap-y-1 border-b border-stone-100 px-3 py-2">
          <h2 class="text-base font-semibold">{{ i.id }}</h2>
          <span
            ><i>{{ i.species }}</i> <span class="text-stone-500">{{ sexLabel(i.sex) }}</span></span
          >
          <span class="text-sm text-stone-500">
            {{ i.events.length }} capturas<template v-if="i.span !== null"> en {{ i.span }} días</template>
          </span>
          <button class="btn-ghost ml-auto gap-1 text-sm" title="Ver sus capturas en el mapa" @click="onMap(i)">
            <MapPinned :size="15" /> En el mapa
          </button>
        </header>
        <ol class="flex gap-3 overflow-x-auto p-3">
          <li v-for="(e, n) in i.events" :key="e.row.id" class="flex shrink-0 items-start gap-3">
            <div
              v-if="n"
              class="flex h-36 w-16 flex-col items-center justify-center text-center text-xs text-stone-500"
              :title="e.metres !== null ? 'Distancia entre los puntos GPS de las dos capturas' : ''"
            >
              <ChevronRight :size="18" />
              <span v-if="e.days !== null">+{{ e.days }} d</span>
              <span v-if="e.metres !== null">{{ e.metres }} m</span>
            </div>
            <figure class="w-48">
              <div class="flex gap-1">
                <button
                  v-for="id in e.photos.slice(0, 2)"
                  :key="id"
                  type="button"
                  class="block flex-1 overflow-hidden rounded bg-stone-100"
                  @click="open(i, id)"
                >
                  <img :src="photoUrl(id)" alt="" loading="lazy" class="h-36 w-full object-cover" />
                </button>
                <div
                  v-if="!e.photos.length"
                  class="flex h-36 w-full items-center justify-center rounded border border-dashed border-stone-300 text-xs text-stone-400"
                >
                  Sin foto
                </div>
              </div>
              <figcaption class="mt-1 text-xs leading-snug">
                <span class="font-medium">{{ n ? `Recaptura ${n}` : 'Marcado' }}</span> · {{ date(e.date) }}<br />
                <span class="text-stone-500">
                  {{ e.collector }}<template v-if="e.section"> · T{{ e.section }}</template
                  ><template v-if="e.minutes !== null"> · {{ formatMinutes(e.minutes) }}</template> · fila {{ e.row.row }}
                  <template v-if="e.photos.length > 2"> · +{{ e.photos.length - 2 }} fotos</template>
                </span>
              </figcaption>
            </figure>
          </li>
        </ol>
      </section>
    </div>

    <div
      v-if="viewer && current"
      class="fixed inset-0 z-[2000] flex flex-col bg-black/90 text-white"
      role="dialog"
      aria-modal="true"
      @click.self="viewer = null"
    >
      <div class="flex items-center gap-3 px-4 py-2 text-sm">
        <span class="font-medium">{{ viewer.individual.id }}</span>
        <i>{{ viewer.individual.species }}</i>
        <span class="text-stone-300">
          {{ current.capture === 1 ? 'Marcado' : `Recaptura ${current.capture - 1}` }} · {{ date(current.event.date) }} ·
          {{ current.event.collector }} · foto {{ viewer.index + 1 }} de {{ viewerPhotos.length }}
        </span>
        <button class="ml-auto rounded p-1 hover:bg-white/10" title="Cerrar (Esc)" @click="viewer = null">
          <X :size="20" />
        </button>
      </div>
      <div class="relative flex min-h-0 flex-1 items-center justify-center" @click.self="viewer = null">
        <button
          v-if="viewerPhotos.length > 1"
          class="absolute left-2 rounded-full bg-white/10 p-2 hover:bg-white/20"
          @click="step(-1)"
        >
          <ChevronLeft :size="24" />
        </button>
        <img :src="photoUrl(current.id)" alt="" class="max-h-full max-w-full object-contain" />
        <button
          v-if="viewerPhotos.length > 1"
          class="absolute right-2 rounded-full bg-white/10 p-2 hover:bg-white/20"
          @click="step(1)"
        >
          <ChevronRight :size="24" />
        </button>
      </div>
    </div>
  </div>
</template>
