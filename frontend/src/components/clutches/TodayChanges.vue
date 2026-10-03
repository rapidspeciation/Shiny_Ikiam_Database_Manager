<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { Check, ChevronLeft, ChevronRight, Copy, Info, RefreshCw, Undo2 } from 'lucide-vue-next'
import DateField from '../DateField.vue'
import UndoDialog from '../history/UndoDialog.vue'
import type { ClutchDay, Day, ServerDayChange } from '../../composables/useClutchDay'
import type { ClutchEvent } from '../../lib/clutches'
import { eventLine } from './eventWords'
import { useUndo } from '../../composables/useUndo'
import { api } from '../../lib/api'
import { changeText, dayText, MODULE, noteDay } from '../../lib/clutches'
import { dayLabel, formatSerial, isoToSerial, serialToIso, todayIso } from '../../lib/dates'
import { errorText, notify } from '../../lib/notice'
import { persistentRef } from '../../lib/persist'
import { usePending } from '../../stores/pending'
import { useSession } from '../../stores/session'
import { t } from '../../lib/i18n'

/**
 * The Clutches tab's history: a day's changes to clutches (today by default),
 * clutch by clutch and field by field (before → after), mine or everyone's.
 * It is what goes into the paper notebook at the end of the round (by hand or
 * copied as text), and where an accident is undone: a clutch's changes of the
 * day, or one field's, after the Historial's preview and confirmation (a cell
 * changed again since is not touched). The changes come from the history (the
 * app, the assistant and Google Sheets), so a change made in the table counts
 * too; "checked, no change" is listed apart. Checks are only in the app.
 */
const props = defineProps<{ day: ClutchDay; initials: string }>()
const emit = defineEmits<{ open: [recordId: string] }>()
const session = useSession()
const pending = usePending()
const onlyMine = persistentRef('clutches:today-mine', true)
const me = computed(() => session.user?.id ?? '')

// --- Which day: today (followed live by useClutchDay) or another one, asked for here
const chosen = ref(todayIso())
const other = ref<Day | null>(null)
const isToday = computed(() => chosen.value === todayIso())
const data = computed<Day>(() => (isToday.value ? props.day.day.value : (other.value ?? { day: chosen.value, checks: [], changes: [] })))
async function loadOther() {
  if (isToday.value) return (other.value = null)
  try {
    other.value = await api<Day>(`clutches/day?day=${encodeURIComponent(chosen.value)}`)
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
watch(chosen, value => {
  if (!value) chosen.value = todayIso()
  else void loadOther()
})
const step = (n: number) => (chosen.value = serialToIso(isoToSerial(chosen.value) + n))

/** The changes behind a line that can be undone (in «Míos», only mine). */
const partsOf = (changes: ServerDayChange[]) =>
  changes.flatMap(c => (c.parts ?? []).filter(p => !onlyMine.value || p.actor === me.value).map(p => p.changeId))

const groups = computed(() => {
  const out = new Map<
    string,
    { recordId: string; clutch: string; species: string; isNew: boolean; who: string[]; at: string; changes: ServerDayChange[]; events: ClutchEvent[] }
  >()
  for (const c of data.value.changes) {
    if (onlyMine.value && !c.actorIds.includes(me.value)) continue
    let g = out.get(c.recordId)
    if (!g) out.set(c.recordId, (g = { recordId: c.recordId, clutch: c.clutch, species: c.species, isNew: c.isNew, who: [], at: c.at, changes: [], events: [] }))
    g.changes.push(c)
    if (c.at > g.at) g.at = c.at
    for (const w of c.actors) if (!g.who.includes(w)) g.who.push(w)
  }
  // The day's app-only events (died, disappeared, preserved…), with the clutch's changes.
  for (const e of data.value.events ?? []) {
    if (onlyMine.value && e.actor !== me.value) continue
    let g = out.get(e.recordId)
    if (!g) out.set(e.recordId, (g = { recordId: e.recordId, clutch: e.clutch ?? '', species: '', isNew: false, who: [], at: e.createdAt, changes: [], events: [] }))
    g.events.push(e)
    if (e.createdAt > g.at) g.at = e.createdAt
    const who = e.name || e.username || ''
    if (who && !g.who.includes(who)) g.who.push(who)
  }
  // In the notebook's order: by clutch number.
  return [...out.values()].sort((a, b) => a.clutch.localeCompare(b.clutch, 'en', { numeric: true }))
})
/** Checked with nothing changed (by this person, or by anyone). */
const unchanged = computed(() => {
  const changed = new Set(data.value.changes.map(c => c.recordId))
  const out = new Map<string, { clutch: string; who: string[] }>()
  for (const c of data.value.checks) {
    if (c.fields.length || changed.has(c.recordId) || (onlyMine.value && c.actor !== me.value)) continue
    const g = out.get(c.recordId) ?? { clutch: c.clutch ?? '', who: [] }
    const who = c.name || c.username || ''
    if (who && !g.who.includes(who)) g.who.push(who)
    out.set(c.recordId, g)
  }
  return [...out.values()].sort((a, b) => a.clutch.localeCompare(b.clutch, 'en', { numeric: true }))
})
const unsaved = computed(() => Object.values(pending.edits).filter(e => e.module === MODULE).length + pending.creates.filter(c => c.module === MODULE).length)
const dayName = computed(() => (data.value.day ? noteDay(isoToSerial(data.value.day)) : ''))
const time = (iso: string) => new Date(iso).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })

const text = computed(() => {
  const title = onlyMine.value ? `Clutches ${dayName.value} ${props.initials}` : `Clutches ${dayName.value}`
  const out = dayText(
    title,
    groups.value.map(g => ({
      clutch: g.isNew ? `${g.clutch} (${t('nuevo')})` : g.clutch,
      species: g.species,
      changes: g.changes,
      events: g.events.map(eventLine),
    })),
    formatSerial,
    t('quitado'),
  )
  return unchanged.value.length ? `${out}\n\n${t('Revisados sin cambios')}: ${unchanged.value.map(u => u.clutch).join(', ')}` : out
})
const copied = ref(false)
/** Without the clipboard (an old browser, no secure page) the text is shown to select and copy by hand. */
const showText = ref(false)
async function copy() {
  if (!navigator.clipboard) {
    showText.value = true
    return
  }
  try {
    await navigator.clipboard.writeText(text.value)
    copied.value = true
    setTimeout(() => (copied.value = false), 2500)
    notify(t('Copiado: pégalo donde quieras'), 'success')
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
const refreshing = ref(false)
async function refresh() {
  refreshing.value = true
  await (isToday.value ? props.day.loadDay() : loadOther())
  refreshing.value = false
}

// --- Undo: a clutch's changes of the day, or one field's (preview, then confirm)
const { undoing, reason, busy, review, cancel, confirm } = useUndo(async () => {
  await Promise.all([props.day.loadDay(), props.day.loadState(), loadOther()])
})
function undoClutch(g: (typeof groups.value)[number]) {
  review({ changeIds: partsOf(g.changes) }, t('Deshacer los cambios de {day} del clutch {clutch}', { day: dayName.value, clutch: g.clutch }))
}
function undoField(g: (typeof groups.value)[number], c: ServerDayChange) {
  review({ changeIds: partsOf([c]) }, t('Deshacer {field} del clutch {clutch}', { field: c.field, clutch: g.clutch }))
}
</script>

<template>
  <div class="px-3 pt-3 pb-8">
    <div class="flex flex-wrap items-center gap-2">
      <div class="inline-flex overflow-hidden rounded-lg border border-stone-300 text-sm" role="group">
        <button class="h-11 px-3" :class="onlyMine ? 'bg-brand-700 text-white' : 'bg-white'" :aria-pressed="onlyMine" @click="onlyMine = true">
          {{ $t('Míos') }}
        </button>
        <button class="h-11 border-l border-stone-300 px-3" :class="!onlyMine ? 'bg-brand-700 text-white' : 'bg-white'" :aria-pressed="!onlyMine" @click="onlyMine = false">
          {{ $t('De todos') }}
        </button>
      </div>
      <button class="btn h-11 px-3" :aria-label="$t('Actualizar')" :disabled="refreshing" @click="refresh"><RefreshCw :size="16" :class="{ 'animate-spin': refreshing }" /></button>
      <button class="btn-primary ml-auto h-11 px-4" :disabled="!groups.length && !unchanged.length" @click="copy">
        <Check v-if="copied" :size="16" /><Copy v-else :size="16" /> {{ $t('Copiar como texto') }}
      </button>
    </div>
    <div class="mt-2 flex items-center gap-1">
      <button class="btn h-11 w-11 shrink-0 justify-center px-0" :aria-label="$t('Día anterior')" @click="step(-1)"><ChevronLeft :size="18" /></button>
      <DateField v-model="chosen" class="field-input h-11 min-w-0 text-base" />
      <button class="btn h-11 w-11 shrink-0 justify-center px-0" :aria-label="$t('Día siguiente')" :disabled="isToday" @click="step(1)">
        <ChevronRight :size="18" />
      </button>
      <button v-if="!isToday" class="btn h-11 shrink-0 px-3" @click="chosen = todayIso()">{{ $t('Hoy') }}</button>
    </div>
    <p class="mt-2 text-xs text-stone-600">
      {{ dayLabel(chosen) }} ·
      {{ $t('Cambios del día para copiar al cuaderno: antes → después. Deshacer pide confirmación.') }}
    </p>
    <p class="mt-1 flex items-start gap-1 text-xs text-stone-500">
      <Info :size="14" class="mt-px shrink-0" />
      <span>{{ $t('Las marcas «Revisado» son solo de la app: no se escriben en Google Sheets, todos las ven y se borran al día siguiente.') }}</span>
    </p>
    <textarea
      v-if="showText"
      class="field-input mt-2 min-h-40 font-mono text-sm"
      readonly
      :value="text"
      :aria-label="$t('Copiar como texto')"
      @focus="($event.target as HTMLTextAreaElement).select()"
    />
    <p v-if="unsaved && isToday" class="mt-1 text-xs text-amber-900">
      {{ $tn(unsaved, '{n} clutch con cambios sin guardar todavía.', '{n} clutches con cambios sin guardar todavía.') }}
    </p>
    <p v-if="!groups.length && !unchanged.length" class="py-6 text-center text-sm text-stone-500">
      {{ isToday ? $t('Ningún cambio hoy todavía.') : $t('Ningún cambio ese día.') }}
    </p>
    <ul class="mt-2 space-y-2">
      <li v-for="g in groups" :key="g.recordId" class="rounded-xl border border-stone-200 bg-white shadow-sm" :data-clutch="g.clutch">
        <div class="flex items-center gap-1 pr-1">
          <button class="flex min-h-12 min-w-0 flex-1 items-baseline gap-2 px-3 py-2 text-left" :aria-label="$t('Abrir el clutch {clutch}', { clutch: g.clutch })" @click="emit('open', g.recordId)">
            <span class="text-lg font-semibold">{{ g.clutch }}</span>
            <span v-if="g.isNew" class="rounded bg-brand-100 px-1.5 text-xs font-medium text-brand-800">{{ $t('nuevo') }}</span>
            <span class="min-w-0 flex-1 truncate text-sm text-stone-600">{{ g.species }}</span>
            <span class="shrink-0 text-xs text-stone-500">{{ time(g.at) }}</span>
          </button>
          <button
            v-if="session.canEdit && partsOf(g.changes).length"
            class="btn h-11 shrink-0 px-3 text-sm"
            :title="$t('Deshacer los cambios de {day} del clutch {clutch}', { day: dayName, clutch: g.clutch })"
            @click="undoClutch(g)"
          >
            <Undo2 :size="16" /> {{ $t('Deshacer') }}
          </button>
        </div>
        <ul class="divide-y divide-stone-100 border-t border-stone-100">
          <li v-for="c in g.changes" :key="c.field" class="flex items-center gap-1 py-1 pr-1 pl-3 leading-snug">
            <span class="min-w-0 flex-1">
              <span class="block text-[11px] font-medium tracking-wide text-stone-500">{{ c.field }}</span>
              <span class="block text-[15px] break-words tabular-nums">{{ changeText(c, formatSerial, $t('quitado')) }}</span>
            </span>
            <button
              v-if="session.canEdit && partsOf([c]).length && g.changes.length > 1"
              class="grid h-11 w-11 shrink-0 place-items-center rounded-md text-stone-600 active:bg-stone-100"
              :aria-label="$t('Deshacer {field} del clutch {clutch}', { field: c.field, clutch: g.clutch })"
              :title="$t('Deshacer {field} del clutch {clutch}', { field: c.field, clutch: g.clutch })"
              @click="undoField(g, c)"
            >
              <Undo2 :size="16" />
            </button>
          </li>
          <li v-for="e in g.events" :key="e.id" class="bg-sky-50/60 py-1.5 pr-1 pl-3 text-sm leading-snug">
            <span class="block text-[11px] font-medium tracking-wide text-stone-500">{{ $t('Evento (solo en la app)') }}</span>
            <span class="block break-words tabular-nums">{{ eventLine(e) }}</span>
          </li>
        </ul>
        <p v-if="!onlyMine" class="px-3 pb-2 text-xs text-stone-500">{{ g.who.join(', ') }}</p>
      </li>
    </ul>
    <section v-if="unchanged.length" class="mt-4">
      <h3 class="text-xs font-semibold tracking-wide text-stone-500 uppercase">{{ $t('Revisados sin cambios') }}</h3>
      <p class="mt-1 flex flex-wrap gap-1.5">
        <span v-for="u in unchanged" :key="u.clutch" class="rounded-full bg-stone-100 px-2.5 py-1 text-sm">
          {{ u.clutch }}<span v-if="!onlyMine" class="text-xs text-stone-500"> · {{ u.who.join(', ') }}</span>
        </span>
      </p>
    </section>
    <UndoDialog v-if="undoing" v-model:reason="reason" :review="undoing" :busy="busy" @cancel="cancel" @confirm="confirm" />
  </div>
</template>
