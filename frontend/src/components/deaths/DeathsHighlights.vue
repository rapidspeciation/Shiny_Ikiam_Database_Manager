<script setup lang="ts">
import { computed, onBeforeUnmount, ref, watch } from 'vue'
import { ChevronDown, ChevronRight, Copy, Highlighter, Loader2 } from 'lucide-vue-next'
import DateField from '../DateField.vue'
import PaperOrder from '../PaperOrder.vue'
import SexBadge from '../SexBadge.vue'
import { api } from '../../lib/api'
import { formatSerial, serialFromIso, todayIso } from '../../lib/dates'
import { DEFAULT_ORDER, readOrder, type RecordedOrder } from '../../lib/deathsCart'
import { errorText, notify } from '../../lib/notice'
import {
  highlightsCopy,
  highlightsSince,
  momentLabel,
  shortDay,
  sortPaper,
  type EnteredDeaths,
  type EnteredDeath,
  type SinceChoice,
} from '../../lib/paperNotebook'
import { persistentRef } from '../../lib/persist'
import { useLive } from '../../stores/live'
import { t } from '../../lib/i18n'

/**
 * «Filas para resaltar»: every butterfly whose death was entered in the
 * database since a moment (by default the last time this person opened the
 * list; or the last 7 days, or a day chosen), in the paper notebook's order,
 * to highlight its row on paper. When a death was entered comes from the
 * history (server/paper-notebook.mjs), not from its Death_date; those kept in
 * the app are listed too, in amber.
 */
const live = useLive()
const open = ref(false)
const data = ref<EnteredDeaths | null>(null)
const loading = ref(false)
const failed = ref('')
/** When this person had last opened the list before now (the server keeps it per person). */
const lastOpened = ref<string | null>(null)
const choice = ref<SinceChoice>({ kind: 'last' })
const day = ref('')
const storedOrder = persistentRef<RecordedOrder>('deaths:highlights-order', DEFAULT_ORDER, { lasting: true })
const order = computed<RecordedOrder>({ get: () => readOrder(storedOrder.value), set: v => (storedOrder.value = v) })
const since = computed(() => highlightsSince(choice.value, lastOpened.value, todayIso()))

let asked = 0
async function load() {
  const ask = ++asked
  loading.value = true
  try {
    const out = await api<EnteredDeaths>(`deaths/highlights?${new URLSearchParams({ since: since.value })}`)
    if (ask !== asked) return
    data.value = out
    failed.value = ''
  } catch (e) {
    if (ask === asked) failed.value = errorText(e)
  } finally {
    if (ask === asked) loading.value = false
  }
}
async function toggle() {
  open.value = !open.value
  if (!open.value) return
  // Opened now: the list starts where it was opened last, and the next time starts here.
  try {
    lastOpened.value = (await api<{ previous: string | null }>('deaths/highlights/opened', { method: 'POST', body: {} })).previous
  } catch {
    /* The last 7 days, then. */
  }
  choice.value = { kind: 'last' }
  void load()
}
watch(since, () => {
  if (open.value) void load()
})
function pickDay(iso: string) {
  day.value = iso
  if (serialFromIso(iso) !== null) choice.value = { kind: 'day', day: iso }
}
// A save or an entry kept in the app: the list follows a moment later.
let timer: ReturnType<typeof setTimeout> | undefined
watch([() => live.revision, () => live.counts.staged], () => {
  if (!open.value) return
  clearTimeout(timer)
  timer = setTimeout(load, 800)
})
onBeforeUnmount(() => clearTimeout(timer))

const items = computed(() => sortPaper(data.value?.items ?? [], order.value))
const beforeHistory = computed(() => !!data.value?.historyStart && data.value.since < data.value.historyStart)
function death(i: EnteredDeath) {
  const date = typeof i.death.date === 'number' ? shortDay(i.death.date) : (i.death.date ?? '')
  return [date, i.death.cause].filter(Boolean).join(', ')
}
function details(i: EnteredDeath) {
  return [
    i.species,
    i.entered === null ? '' : t('Emergió {date}', { date: formatSerial(i.entered) }),
    t('anotada {when}', { when: momentLabel(i.enteredAt) }),
    i.by.length ? t('por {names}', { names: i.by.join(', ') }) : '',
    i.source === 'sheets' ? t('en Google Sheets') : '',
  ]
    .filter(Boolean)
    .join(' · ')
}
async function copy() {
  try {
    await navigator.clipboard.writeText(
      highlightsCopy(items.value, t('Filas para resaltar desde {when}', { when: momentLabel(since.value) })),
    )
    notify(t('Copiado'), 'success')
  } catch {
    notify(t('No se pudo copiar'), 'error')
  }
}
const chip = (on: boolean) =>
  on ? 'border-stone-800 bg-stone-800 text-white' : 'border-stone-300 bg-white text-stone-800 active:bg-stone-100'
</script>

<template>
  <section class="px-3 pt-4 pb-6" data-highlights>
    <button type="button" class="flex min-h-11 w-full items-center gap-2 text-left" :aria-expanded="open" @click="toggle">
      <Highlighter :size="17" class="shrink-0 text-yellow-600" />
      <span class="flex-1 text-sm font-semibold text-stone-700">
        {{ $t('Filas para resaltar') }}
        <span class="font-normal text-stone-500">{{ $t('(muertes anotadas en la base, para resaltarlas en el cuaderno)') }}</span>
      </span>
      <ChevronDown v-if="open" :size="18" class="shrink-0 text-stone-500" /><ChevronRight
        v-else
        :size="18"
        class="shrink-0 text-stone-500"
      />
    </button>

    <div v-if="open" class="mt-2 space-y-2">
      <div class="flex flex-wrap items-center gap-1.5 text-sm" role="group" :aria-label="$t('Anotadas desde')">
        <span class="text-stone-600">{{ $t('Anotadas desde:') }}</span>
        <button
          type="button"
          class="min-h-10 rounded-full border px-3"
          :class="chip(choice.kind === 'last')"
          :aria-pressed="choice.kind === 'last'"
          @click="choice = { kind: 'last' }"
        >
          {{
            lastOpened
              ? $t('la última vez que la abrí ({when})', { when: momentLabel(lastOpened) })
              : $t('la última vez que la abrí')
          }}
        </button>
        <button
          type="button"
          class="min-h-10 rounded-full border px-3"
          :class="chip(choice.kind === 'week')"
          :aria-pressed="choice.kind === 'week'"
          @click="choice = { kind: 'week' }"
        >
          {{ $t('7 días') }}
        </button>
        <DateField
          :model-value="choice.kind === 'day' ? choice.day : day"
          class="field-input h-10 w-40"
          :aria-label="$t('Desde el día')"
          @update:model-value="pickDay"
        />
      </div>
      <p class="text-xs text-stone-500">
        {{
          $t('Desde {when}. Cuándo se anotó cada muerte sale del Historial (no de su Death_date).', { when: momentLabel(since) })
        }}
        <template v-if="choice.kind === 'day'"> {{ $t('El día empieza a las 00:00 de Ecuador.') }}</template>
      </p>
      <p v-if="beforeHistory" class="rounded-lg border border-amber-300 bg-amber-50 px-3 py-2 text-sm text-amber-950">
        {{
          $t('El historial empieza el {date}: las muertes anotadas antes no salen aquí.', {
            date: momentLabel(data!.historyStart!),
          })
        }}
      </p>

      <div class="flex flex-wrap items-center gap-2">
        <PaperOrder
          v-model="order"
          :options="[
            { by: 'row', label: $t('Fila') },
            { by: 'id', label: 'Insectary ID' },
            { by: 'emergence', label: $t('Emergencia') },
          ]"
        />
        <button type="button" class="btn h-10" :disabled="!items.length" @click="copy">
          <Copy :size="15" /> {{ $t('Copiar como texto') }}
        </button>
        <Loader2 v-if="loading" :size="15" class="animate-spin text-stone-400" />
      </div>

      <p v-if="failed" class="text-sm text-red-700">{{ failed }}</p>
      <p v-else-if="!data" class="text-sm text-stone-500">
        <Loader2 :size="14" class="inline animate-spin" /> {{ $t('Cargando…') }}
      </p>
      <p v-else-if="!items.length" class="text-sm text-stone-500">{{ $t('Ninguna muerte anotada desde entonces.') }}</p>
      <ol v-else class="divide-y divide-stone-200 overflow-hidden rounded-xl border border-stone-200">
        <li
          v-for="i in items"
          :key="i.recordId"
          class="flex min-h-12 items-center gap-3 bg-yellow-100 px-3 py-1.5"
          :class="i.death.staged ? 'border-l-4 border-l-amber-500' : ''"
          :data-highlight="i.id"
        >
          <span class="w-14 shrink-0 font-semibold tabular-nums">{{ i.id }}</span>
          <span class="w-6 shrink-0 text-center text-lg leading-none" aria-hidden="true">▬</span>
          <span class="min-w-0 flex-1">
            <span class="block text-sm">
              {{ death(i) }}
              <span v-if="i.death.staged" class="text-xs font-medium text-amber-800"> · {{ $t('aún no en Google Sheets') }}</span>
            </span>
            <span class="flex flex-wrap items-center gap-x-1 text-xs text-stone-500">
              <SexBadge :sex="i.sex" />
              <span>{{ details(i) }}</span>
              <span>· {{ $t('fila {row}', { row: i.row ?? '—' }) }}</span>
            </span>
          </span>
        </li>
      </ol>
      <p v-if="data" class="text-xs text-stone-500">
        {{ $tn(items.length, '{n} fila para resaltar', '{n} filas para resaltar') }}
      </p>
    </div>
  </section>
</template>
