<script setup lang="ts">
import { computed, ref } from 'vue'
import { Check, Copy, RefreshCw } from 'lucide-vue-next'
import type { ClutchDay } from '../../composables/useClutchDay'
import { changeText, dayText, MODULE, noteDay } from '../../lib/clutches'
import { formatSerial, isoToSerial } from '../../lib/dates'
import { errorText, notify } from '../../lib/notice'
import { persistentRef } from '../../lib/persist'
import { usePending } from '../../stores/pending'
import { useSession } from '../../stores/session'
import { t } from '../../lib/i18n'

/**
 * Today's changes to clutches, field by field (before → after), compact enough
 * to copy into the paper notebook at the end of the round, by hand or as text.
 * The changes come from the history (the app, the assistant and Google Sheets),
 * so a change made in the table counts too; "checked, no change" is listed apart.
 */
const props = defineProps<{ day: ClutchDay; initials: string }>()
const emit = defineEmits<{ open: [recordId: string] }>()
const session = useSession()
const pending = usePending()
const onlyMine = persistentRef('clutches:today-mine', true)
const me = computed(() => session.user?.id ?? '')

const groups = computed(() => {
  const out = new Map<string, { recordId: string; clutch: string; species: string; isNew: boolean; who: string[]; at: string; changes: typeof props.day.day.value.changes }>()
  for (const c of props.day.day.value.changes) {
    if (onlyMine.value && !c.actorIds.includes(me.value)) continue
    let g = out.get(c.recordId)
    if (!g) out.set(c.recordId, (g = { recordId: c.recordId, clutch: c.clutch, species: c.species, isNew: c.isNew, who: [], at: c.at, changes: [] }))
    g.changes.push(c)
    if (c.at > g.at) g.at = c.at
    for (const w of c.actors) if (!g.who.includes(w)) g.who.push(w)
  }
  // In the notebook's order: by clutch number.
  return [...out.values()].sort((a, b) => a.clutch.localeCompare(b.clutch, 'en', { numeric: true }))
})
/** Checked with nothing changed (by this person, or by anyone). */
const unchanged = computed(() => {
  const changed = new Set(props.day.day.value.changes.map(c => c.recordId))
  const out = new Map<string, { clutch: string; who: string[] }>()
  for (const c of props.day.day.value.checks) {
    if (c.fields.length || changed.has(c.recordId) || (onlyMine.value && c.actor !== me.value)) continue
    const g = out.get(c.recordId) ?? { clutch: c.clutch ?? '', who: [] }
    const who = c.name || c.username || ''
    if (who && !g.who.includes(who)) g.who.push(who)
    out.set(c.recordId, g)
  }
  return [...out.values()].sort((a, b) => a.clutch.localeCompare(b.clutch, 'en', { numeric: true }))
})
const unsaved = computed(() => Object.values(pending.edits).filter(e => e.module === MODULE).length + pending.creates.filter(c => c.module === MODULE).length)
const dayName = computed(() => (props.day.day.value.day ? noteDay(isoToSerial(props.day.day.value.day)) : ''))
const time = (iso: string) => new Date(iso).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })

const text = computed(() => {
  const title = onlyMine.value ? `Clutches ${dayName.value} ${props.initials}` : `Clutches ${dayName.value}`
  const out = dayText(
    title,
    groups.value.map(g => ({ clutch: g.isNew ? `${g.clutch} (${t('nuevo')})` : g.clutch, species: g.species, changes: g.changes })),
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
  await props.day.loadDay()
  refreshing.value = false
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
    <p class="mt-2 text-xs text-stone-600">
      {{ $t('Cambios de hoy ({day}) para copiar al cuaderno: antes → después.', { day: dayName }) }}
    </p>
    <textarea
      v-if="showText"
      class="field-input mt-2 min-h-40 font-mono text-sm"
      readonly
      :value="text"
      :aria-label="$t('Copiar como texto')"
      @focus="($event.target as HTMLTextAreaElement).select()"
    />
    <p v-if="unsaved" class="mt-1 text-xs text-amber-900">
      {{ $tn(unsaved, '{n} clutch con cambios sin guardar todavía.', '{n} clutches con cambios sin guardar todavía.') }}
    </p>
    <p v-if="!groups.length && !unchanged.length" class="py-6 text-center text-sm text-stone-500">{{ $t('Ningún cambio hoy todavía.') }}</p>
    <ul class="mt-2 space-y-2">
      <li v-for="g in groups" :key="g.recordId" class="rounded-xl border border-stone-200 bg-white shadow-sm">
        <button class="block w-full px-3 py-2 text-left" @click="emit('open', g.recordId)">
          <span class="flex items-baseline gap-2">
            <span class="text-lg font-semibold">{{ g.clutch }}</span>
            <span v-if="g.isNew" class="rounded bg-brand-100 px-1.5 text-xs font-medium text-brand-800">{{ $t('nuevo') }}</span>
            <span class="min-w-0 flex-1 truncate text-sm text-stone-600">{{ g.species }}</span>
            <span class="shrink-0 text-xs text-stone-500">{{ time(g.at) }}</span>
          </span>
          <span class="mt-1 block space-y-0.5">
            <span v-for="c in g.changes" :key="c.field" class="block py-0.5 leading-snug">
              <span class="block text-[11px] font-medium tracking-wide text-stone-500">{{ c.field }}</span>
              <span class="block text-[15px] break-words tabular-nums">{{ changeText(c, formatSerial, $t('quitado')) }}</span>
            </span>
          </span>
          <span v-if="!onlyMine" class="mt-1 block text-xs text-stone-500">{{ g.who.join(', ') }}</span>
        </button>
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
  </div>
</template>
