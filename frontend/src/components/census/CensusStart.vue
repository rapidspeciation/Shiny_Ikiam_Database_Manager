<script setup lang="ts">
import { computed, ref } from 'vue'
import { BookCheck, BookOpenCheck, ChevronRight, Loader2, Users } from 'lucide-vue-next'
import DateField from '../DateField.vue'
import CensusCounts from './CensusCounts.vue'
import { useCensus } from '../../composables/useCensus'
import { dayLabel, isoToSerial, serialFromIso, serialToIso, todayIso } from '../../lib/dates'
import { errorText, notify } from '../../lib/notice'
import { useSession } from '../../stores/session'
import { t } from '../../lib/i18n'

/**
 * The census's start: the censuses going on now (to join from another phone),
 * a new one (the day, today by default, then one tap on the species: its
 * butterflies alive are counted beside it), the past ones, and «Actualizar el
 * cuaderno» for any day and species (what to mark on paper).
 */
const emit = defineEmits<{ notebook: [] }>()
const census = useCensus()
const session = useSession()
const day = ref(todayIso())
const busy = ref('')
const showAll = ref(false)
const today = computed(() => isoToSerial(todayIso()))
const quickDays = computed(() => [
  { iso: todayIso(), name: t('Hoy') },
  { iso: serialToIso(today.value - 1), name: t('Ayer') },
])
const dayError = computed(() =>
  serialFromIso(day.value) === null ? t('Fecha no válida: el año debe estar entre 1990 y 2099') : '',
)
const species = computed(() => census.overview.value?.species ?? [])
const shownSpecies = computed(() => (showAll.value ? species.value : species.value.slice(0, 8)))

async function start(name: string) {
  if (dayError.value || busy.value) return
  busy.value = name
  try {
    const out = await census.start(name, day.value)
    if (out.joined) notify(t('Te uniste al censo de {who}', { who: out.census.createdByName }), 'success')
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = ''
  }
}
const short = (s: string) => s.split(' ').slice(1).join(' ') || s
const genus = (s: string) => s.split(' ')[0]
const dayOf = (iso: string) => dayLabel(iso).split(' · ')[0]
const choice = (on: boolean) =>
  on ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800 active:bg-stone-100'
</script>

<template>
  <div class="mx-auto h-full max-w-5xl overflow-y-auto px-3 pt-3 pb-10 sm:px-4">
    <p class="mb-3 text-sm text-stone-600">
      {{
        $t(
          'Suelta las mariposas de una especie una a una y marca cada ID que veas: las que no aparezcan se registran como desaparecidas (Disappearance) en la fecha del censo.',
        )
      }}
    </p>

    <!-- Going on now: anyone can join from their phone. -->
    <section v-if="census.overview.value?.open.length" class="mb-5">
      <h2 class="mb-1.5 text-sm font-semibold text-stone-700">{{ $t('En curso') }}</h2>
      <ul class="grid gap-2 sm:grid-cols-2">
        <li v-for="c in census.overview.value.open" :key="c.id">
          <button
            class="flex w-full items-center gap-3 rounded-xl border-2 border-brand-600 bg-white px-3 py-3 text-left shadow-sm active:bg-brand-50"
            @click="census.open(c.id)"
          >
            <span class="min-w-0 flex-1">
              <span class="block truncate text-base font-semibold">{{ c.species }}</span>
              <span class="block text-sm text-stone-600">
                {{ dayOf(c.day) }} · {{ $t('empezó {who}', { who: c.createdByName }) }}
              </span>
              <span class="mt-1 block text-sm font-medium text-brand-800">
                {{ $t('{seen} de {total} vistas', { seen: c.counts.seen, total: c.counts.roster }) }}
                <template v-if="c.people.length"> · <Users :size="13" class="inline" /> {{ c.people.join(', ') }}</template>
              </span>
            </span>
            <span class="shrink-0 rounded-lg bg-brand-700 px-3 py-2 text-sm font-semibold text-white">{{ $t('Unirme') }}</span>
          </button>
        </li>
      </ul>
    </section>

    <!-- A new census: the day, then one tap on the species. -->
    <section v-if="session.canEdit" class="mb-6 rounded-xl border border-stone-200 bg-white p-3 shadow-sm">
      <h2 class="mb-2 text-base font-semibold text-stone-800">{{ $t('Nuevo censo') }}</h2>
      <span class="field-label">{{ $t('Fecha del censo') }}</span>
      <div class="grid max-w-md grid-cols-[1fr_1fr_minmax(9rem,1.4fr)] gap-2">
        <button
          v-for="d in quickDays"
          :key="d.iso"
          class="h-12 rounded-lg border text-base font-medium"
          :class="choice(day === d.iso)"
          :aria-pressed="day === d.iso"
          @click="day = d.iso"
        >
          {{ d.name }}
        </button>
        <DateField v-model="day" class="field-input h-12 text-base" />
      </div>
      <p v-if="dayError" class="mt-1 text-sm text-red-700">{{ dayError }}</p>
      <p v-else class="mt-1 text-sm text-stone-600">{{ dayLabel(day) }}</p>

      <span class="field-label mt-4">{{ $t('Especie (vivas en el insectario)') }}</span>
      <p v-if="!census.overview.value" class="text-sm text-stone-500">{{ $t('Cargando…') }}</p>
      <p v-else-if="!species.length" class="text-sm text-stone-500">{{ $t('No hay mariposas vivas en el insectario.') }}</p>
      <ul class="grid grid-cols-[repeat(auto-fill,minmax(14rem,1fr))] gap-2">
        <li v-for="s in shownSpecies" :key="s.species">
          <button
            class="flex min-h-14 w-full items-center gap-2 rounded-lg border border-stone-300 bg-white px-3 py-2 text-left active:bg-brand-50 disabled:opacity-60"
            :disabled="!!busy || !!dayError"
            @click="start(s.species)"
          >
            <span class="min-w-0 flex-1">
              <span class="block text-xs text-stone-500 italic">{{ genus(s.species) }}</span>
              <span class="block text-base leading-tight font-medium italic">{{ short(s.species) }}</span>
              <span v-if="(s.subspecies?.length ?? 0) > 1" class="block text-xs leading-snug text-stone-500">
                <template v-for="(sub, i) in s.subspecies" :key="sub.species"
                  >{{ i ? ' · ' : '' }}<i>{{ sub.species.split(' ').slice(2).join(' ') || '—' }}</i> {{ sub.alive }}</template
                >
              </span>
            </span>
            <Loader2 v-if="busy === s.species" :size="18" class="shrink-0 animate-spin text-brand-700" />
            <span v-else class="shrink-0 rounded-full bg-brand-50 px-2 py-0.5 text-sm font-semibold text-brand-800">{{
              s.alive
            }}</span>
          </button>
        </li>
      </ul>
      <button v-if="species.length > 8 && !showAll" class="mt-2 h-11 text-sm text-stone-600 underline" @click="showAll = true">
        {{ $t('Ver las {n} especies', { n: species.length }) }}
      </button>
    </section>

    <!-- Past censuses: tap one to see it, with its lines for the notebook. -->
    <section>
      <div class="mb-1.5 flex flex-wrap items-center gap-2">
        <h2 class="flex-1 text-sm font-semibold text-stone-700">{{ $t('Censos anteriores') }}</h2>
        <button type="button" class="btn h-11" @click="emit('notebook')">
          <BookOpenCheck :size="16" /> {{ $t('Actualizar el cuaderno') }}
        </button>
      </div>
      <p v-if="census.overview.value && !census.overview.value.history.length" class="text-sm text-stone-500">
        {{ $t('Aún no hay censos.') }}
      </p>
      <ul class="divide-y divide-stone-100 overflow-hidden rounded-xl border border-stone-200 bg-white">
        <li v-for="c in census.overview.value?.history ?? []" :key="c.id">
          <button class="flex w-full items-center gap-3 px-3 py-2.5 text-left active:bg-stone-50" @click="census.open(c.id)">
            <span class="w-24 shrink-0 text-sm font-medium text-stone-700">{{ dayOf(c.day) }}</span>
            <span class="min-w-0 flex-1">
              <span class="block truncate text-sm font-medium italic">{{ c.species }}</span>
              <CensusCounts :census="c" class="text-xs" />
              <span class="block truncate text-xs text-stone-500">{{
                [c.createdByName, ...c.people].filter((p, i, all) => all.indexOf(p) === i).join(', ')
              }}</span>
            </span>
            <BookCheck v-if="c.notebookAt" :size="18" class="shrink-0 text-brand-700" :aria-label="$t('Pasado al cuaderno')" />
            <ChevronRight :size="18" class="shrink-0 text-stone-400" />
          </button>
        </li>
      </ul>
    </section>
  </div>
</template>
