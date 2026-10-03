<script setup lang="ts">
import { computed, nextTick, ref } from 'vue'
import ChoiceField from '../ChoiceField.vue'
import DateField from '../DateField.vue'
import type { CollectState } from '../../composables/useCollect'
import { personCode, weatherLabel } from '../../lib/collect'
import { dayLabel, isoToSerial, serialFromIso, serialToIso, todayIso } from '../../lib/dates'
import { t } from '../../lib/i18n'

/**
 * What the whole outing shares, chosen once: the day, the place (the latest
 * places first), who went out (each butterfly's Collector is one of them),
 * who identified them, and the weather. The butterflies follow it, unless one
 * was given its own value in its card.
 */
const props = defineProps<{ state: CollectState }>()
const emit = defineEmits<{ done: [] }>()
const { header, recentPlaces, places, recentPeople, people, rainfalls, clouds, setDay, setTeam } = props.state

const quickDates = computed(() => [
  { iso: todayIso(), name: t('Hoy') },
  { iso: serialToIso(isoToSerial(todayIso()) - 1), name: t('Ayer') },
])
const dateError = computed(() =>
  header.value.date && serialFromIso(header.value.date) === null ? t('Fecha no válida: el año debe estar entre 1990 y 2099') : '',
)
/** The chips: the latest places (the one chosen always among them). */
const placeChips = computed(() => {
  const top = recentPlaces.value.slice(0, 6)
  const now = header.value.location
  return now && !top.includes(now) ? [now, ...top.slice(0, 5)] : top
})
/** People of the latest collections, as chips (those chosen always among them). */
const personChips = computed(() => {
  const chosen = [...header.value.team, header.value.identifier].filter(Boolean)
  // One chip per person: the list can hold a name twice, once with a trailing space.
  const seen = new Set<string>()
  const out = [...chosen, ...recentPeople.value].filter(p => !seen.has(p.trim()) && !!seen.add(p.trim()))
  return out.slice(0, Math.max(8, chosen.length))
})
function toggleTeam(person: string) {
  const team = header.value.team
  setTeam(team.includes(person) ? team.filter(p => p !== person) : [...team, person])
}
/** A chip's name: the person's code, with the first name when two chips share it. */
function chipLabel(person: string) {
  const code = personCode(person)
  const twin = personChips.value.some(p => p !== person && personCode(p) === code)
  return twin ? `${code} · ${(person.split(' - ')[1] ?? '').trim().split(/\s+/)[0]}` : code
}
/** Someone not among the chips, from the whole list: added to the team, and the box empties again. */
const another = ref('')
function addTeam(person: string) {
  another.value = person
  if (person && !header.value.team.includes(person)) setTeam([...header.value.team, person])
  nextTick(() => (another.value = ''))
}
const rainChips = computed(() => rainfalls.value.filter(v => !/^NA$/i.test(v)))
const cloudChips = computed(() => clouds.value)
const chip = (on: boolean) =>
  on ? 'border-brand-700 bg-brand-700 text-white' : 'border-stone-300 bg-white text-stone-800 active:bg-stone-100'
</script>

<template>
  <section class="space-y-4 border-b border-stone-200 bg-white px-3 py-3" :aria-label="$t('El día de colecta')">
    <div>
      <h2 class="mb-1.5 text-sm font-semibold text-stone-700">Collection_date</h2>
      <div class="grid grid-cols-[1fr_1fr_minmax(9rem,1.4fr)] gap-2 sm:max-w-md">
        <button
          v-for="d in quickDates"
          :key="d.iso"
          class="h-12 rounded-lg border text-base font-medium"
          :class="chip(header.date === d.iso)"
          :aria-pressed="header.date === d.iso"
          @click="header.date = d.iso"
        >
          {{ d.name }}
        </button>
        <DateField v-model="header.date" class="field-input h-12 text-base" :aria-label="'Collection_date'" />
      </div>
      <p v-if="dateError" class="mt-1 text-sm text-red-700">{{ dateError }}</p>
      <p v-else-if="header.date" class="mt-1 text-sm text-stone-600 first-letter:uppercase">{{ dayLabel(header.date) }}</p>
      <p v-else class="mt-1 text-sm text-amber-800">{{ $t('Elige la fecha') }}</p>
    </div>

    <div>
      <h2 class="mb-1.5 text-sm font-semibold text-stone-700">{{ $t('Lugar') }} <span class="font-normal text-stone-500">Collection_location</span></h2>
      <div class="flex flex-wrap gap-2">
        <button
          v-for="p in placeChips"
          :key="p"
          class="min-h-11 rounded-lg border px-3 py-1.5 text-left text-sm font-medium"
          :class="chip(header.location === p)"
          :aria-pressed="header.location === p"
          @click="setDay('location', p)"
        >
          {{ p }}
        </button>
      </div>
      <ChoiceField
        :model-value="header.location"
        class="field-input mt-2 h-11 text-base sm:max-w-md"
        :options="places"
        :placeholder="$t('Otro lugar (Location_data)…')"
        :aria-label="$t('Otro lugar')"
        @update:model-value="setDay('location', $event)"
      />
    </div>

    <div>
      <h2 class="mb-1.5 text-sm font-semibold text-stone-700">{{ $t('Quiénes colectaron') }} <span class="font-normal text-stone-500">Collector</span></h2>
      <div class="flex flex-wrap gap-2">
        <button
          v-for="p in personChips"
          :key="p"
          class="min-h-11 min-w-14 rounded-lg border px-3 text-base font-semibold"
          :class="chip(header.team.includes(p))"
          :aria-pressed="header.team.includes(p)"
          :title="p"
          @click="toggleTeam(p)"
        >
          {{ chipLabel(p) }}
        </button>
      </div>
      <ChoiceField
        :model-value="another"
        class="field-input mt-2 h-11 text-base sm:max-w-md"
        :options="people"
        :freetext="false"
        :placeholder="$t('Otra persona…')"
        :aria-label="$t('Añadir otra persona')"
        @update:model-value="addTeam($event)"
      />
      <p class="mt-1 text-xs text-stone-500">
        {{ header.team.length > 1 ? $t('Cada tarjeta elige quién la colectó.') : $t('Marca a todos los que salieron: cada tarjeta elige quién la colectó.') }}
      </p>
    </div>

    <div>
      <h2 class="mb-1.5 text-sm font-semibold text-stone-700">{{ $t('Quién identificó') }} <span class="font-normal text-stone-500">Identifier</span></h2>
      <div class="flex flex-wrap gap-2">
        <button
          v-for="p in personChips"
          :key="p"
          class="min-h-11 min-w-14 rounded-lg border px-3 text-base font-semibold"
          :class="chip(header.identifier === p)"
          :aria-pressed="header.identifier === p"
          :title="p"
          @click="setDay('identifier', p)"
        >
          {{ chipLabel(p) }}
        </button>
      </div>
      <ChoiceField
        :model-value="header.identifier"
        class="field-input mt-2 h-11 text-base sm:max-w-md"
        :options="people"
        :freetext="false"
        :placeholder="$t('Otra persona…')"
        :aria-label="'Identifier'"
        @update:model-value="setDay('identifier', $event)"
      />
    </div>

    <div class="grid gap-4 sm:grid-cols-2">
      <div>
        <h2 class="mb-1.5 text-sm font-semibold text-stone-700">{{ $t('Lluvia') }} <span class="font-normal text-stone-500">Rainfall</span></h2>
        <div class="flex flex-wrap gap-2">
          <button
            v-for="r in rainChips"
            :key="r"
            class="min-h-11 rounded-lg border px-3 py-1 text-left text-sm"
            :class="chip(header.rainfall === r)"
            :aria-pressed="header.rainfall === r"
            :title="r"
            @click="setDay('rainfall', r)"
          >
            <span class="font-semibold">{{ weatherLabel(r).code }}</span>
            <span v-if="weatherLabel(r).words" class="ml-1 opacity-80">{{ weatherLabel(r).words }}</span>
          </button>
        </div>
      </div>
      <div>
        <h2 class="mb-1.5 text-sm font-semibold text-stone-700">{{ $t('Nubes') }} <span class="font-normal text-stone-500">Cloud_cover</span></h2>
        <div class="flex flex-wrap gap-2">
          <button
            v-for="c in cloudChips"
            :key="c"
            class="min-h-11 rounded-lg border px-3 py-1 text-left text-sm"
            :class="chip(header.cloud === c)"
            :aria-pressed="header.cloud === c"
            :title="c"
            @click="setDay('cloud', header.cloud === c ? '' : c)"
          >
            <span class="font-semibold">{{ weatherLabel(c).code }}</span>
            <span v-if="weatherLabel(c).words" class="ml-1 opacity-80">{{ weatherLabel(c).words }}</span>
          </button>
        </div>
        <p class="mt-1 text-xs text-stone-500">{{ $t('Si cambia durante el día, cámbiala en la tarjeta (Más).') }}</p>
      </div>
    </div>

    <button class="btn-primary h-12 w-full text-base sm:w-auto sm:px-8" @click="emit('done')">{{ $t('Listo') }}</button>
  </section>
</template>
