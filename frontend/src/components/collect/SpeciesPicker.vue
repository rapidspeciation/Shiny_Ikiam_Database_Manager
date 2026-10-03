<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref } from 'vue'
import { Search, X } from 'lucide-vue-next'
import { useKeyboard } from '../../composables/usePhone'
import { searchSpecies, type SpeciesEntry } from '../../lib/collect'

/**
 * Choosing a butterfly's species, with a thumb: a search box over the whole
 * screen (a phone) or a box in the middle (a computer); nothing typed shows
 * the species of this list and the latest collected first. Each word typed
 * begins a word of the name, so the notebook's shorthand finds it ("pol
 * eury", "deceptus", "mech mess"). A form is chosen with its species.
 */
const props = defineProps<{
  entries: SpeciesEntry[]
  /** The species and form chosen now (marked in the list). */
  current?: string
  /** The sheet's species list has arrived: a name outside it can be offered as typed. */
  known: boolean
}>()
const emit = defineEmits<{ pick: [entry: SpeciesEntry]; close: [] }>()

const keyboard = useKeyboard()
const query = ref('')
const input = ref<HTMLInputElement>()
const results = computed(() => searchSpecies(props.entries, query.value, 60))
/** Two words typed and nothing found: the name as typed (the sheet's list marks it if it is not there). */
const typedName = computed(() => {
  const q = query.value.trim().replace(/\s+/g, ' ')
  if (q.split(' ').length < 2 || results.value.length) return null
  const [genus, ...rest] = q.split(' ')
  return `${genus[0].toUpperCase()}${genus.slice(1).toLowerCase()} ${rest.join(' ').toLowerCase()}`
})
function pick(entry: SpeciesEntry) {
  emit('pick', entry)
}
function enter() {
  if (results.value[0]) pick(results.value[0])
  else if (typedName.value) pick({ species: typedName.value, form: '', label: typedName.value })
}
const onKey = (e: KeyboardEvent) => e.key === 'Escape' && emit('close')
onMounted(() => {
  window.addEventListener('keydown', onKey)
  nextTick(() => input.value?.focus())
})
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
/** The part of the screen above the keyboard (the list ends there). */
const style = computed(() =>
  keyboard.open.value ? { top: `${keyboard.visibleTop.value}px`, height: `${keyboard.visibleBottom.value - keyboard.visibleTop.value}px` } : undefined,
)
</script>

<template>
  <div class="fixed inset-0 z-40 flex justify-center bg-black/40 sm:items-start sm:pt-[8vh]" :style="style" @click.self="emit('close')">
    <section
      class="flex h-full w-full flex-col bg-white sm:h-auto sm:max-h-[80vh] sm:max-w-lg sm:rounded-xl sm:shadow-xl"
      role="dialog"
      :aria-label="$t('Elegir especie')"
    >
      <div class="flex items-center gap-2 border-b border-stone-200 p-2">
        <div class="relative min-w-0 flex-1">
          <Search :size="20" class="pointer-events-none absolute top-1/2 left-3 -translate-y-1/2 text-stone-400" />
          <input
            ref="input"
            v-model="query"
            class="h-12 w-full rounded-xl border border-stone-300 bg-white pr-3 pl-10 text-lg focus:border-brand-600 focus:ring-2 focus:ring-brand-100 focus:outline-none"
            :placeholder="$t('Especie o forma (p. ej. mech mess, deceptus)')"
            :aria-label="$t('Buscar especie')"
            type="search"
            autocomplete="off"
            autocorrect="off"
            autocapitalize="off"
            spellcheck="false"
            enterkeyhint="done"
            @keydown.enter.prevent="enter"
          />
        </div>
        <button class="grid h-12 w-12 shrink-0 place-items-center rounded-lg text-stone-600 active:bg-stone-100" :aria-label="$t('Cerrar')" @click="emit('close')">
          <X :size="22" />
        </button>
      </div>
      <p v-if="!query" class="px-3 pt-2 text-xs text-stone-500">{{ $t('Las de esta lista y las últimas colectadas primero') }}</p>
      <ul class="min-h-0 flex-1 divide-y divide-stone-100 overflow-y-auto" role="listbox">
        <li v-for="e in results" :key="e.label">
          <button
            class="flex min-h-13 w-full items-center gap-2 px-3 py-2 text-left active:bg-brand-50"
            :class="{ 'bg-brand-50': e.label === current }"
            role="option"
            :aria-selected="e.label === current"
            @click="pick(e)"
          >
            <span class="min-w-0 flex-1">
              <span class="block text-base italic" :class="e.form ? 'text-stone-500' : 'font-medium text-stone-900'">{{ e.species }}</span>
              <span v-if="e.form" class="block text-base font-semibold text-stone-900">{{ e.form }}</span>
            </span>
          </button>
        </li>
        <li v-if="typedName">
          <button class="flex min-h-13 w-full items-center px-3 py-2 text-left active:bg-brand-50" @click="pick({ species: typedName, form: '', label: typedName })">
            <span>
              <span class="block text-base italic">{{ typedName }}</span>
              <span class="block text-xs text-amber-800">{{
                known ? $t('No está en la lista de especies de la hoja: revísalo') : $t('Usar como está escrito')
              }}</span>
            </span>
          </button>
        </li>
        <li v-if="!results.length && !typedName" class="px-3 py-6 text-sm text-stone-500">{{ $t('Ninguna especie empieza así') }}</li>
      </ul>
    </section>
  </div>
</template>
