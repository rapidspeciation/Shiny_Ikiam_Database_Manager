<script setup lang="ts">
import { computed, ref } from 'vue'
import { Camera, Info, Loader2, RotateCcw, Trash2, X } from 'lucide-vue-next'
import ClutchPhotoAdd from './ClutchPhotoAdd.vue'
import ClutchPhotoViewer from './ClutchPhotoViewer.vue'
import type { ClutchDay } from '../../composables/useClutchDay'
import type { ClutchRecordState } from '../../composables/useClutchRecord'
import { usePhotoUploads } from '../../composables/usePhotoUploads'
import { photoUrl, type ClutchPhoto } from '../../lib/clutchPhotos'
import type { ClutchEvent, Expected, Stage, StageTally, YoungRow } from '../../lib/clutches'
import { dayLabel, formatSerial, isoToSerial, todayIso } from '../../lib/dates'
import { errorText, notify } from '../../lib/notice'
import { useSession } from '../../stores/session'
import { eventLine, stageWord } from './eventWords'

/**
 * What the paper cannot hold, for one clutch (only in the app): its events day
 * by day (hatched, died, disappeared, preserved: who and when), the eggs and
 * larvae of it registered one by one in Emergidos, its photos (each with the
 * event it shows, or the day's; a tap opens them to zoom), the photos still
 * being sent, and the numbers the team counts with, each explained.
 */
const props = defineProps<{
  recordId: string
  clutch: string
  day: ClutchDay
  /** The sheet's totals as the person sees them (pending edits included), by stage. */
  totals: Partial<Record<Stage, number | null>>
  /** What should be in the cage today (lib/clutches outlook). */
  expected: Expected
  canEdit: boolean
  /** Photos can be added (a clutch already in the sheet). */
  canPhoto: boolean
  initials: (name: string) => string
  /** The clutch's events, Emergidos' rows and photos (useClutchRecord, loaded by the editor). */
  record: ClutchRecordState
}>()
const session = useSession()
const uploads = usePhotoUploads()
const data = computed(() => props.record.data.value)

const subtract = computed(() => props.day.settings.subtractPreserved)
const tally = (stage: Stage): StageTally => data.value?.tally[stage] ?? { gained: 0, died: 0, disappeared: 0, preserved: 0 }
const known = (stage: Stage) => Object.values(tally(stage)).some(n => n > 0)
/** The stages with something recorded: their numbers, explained. */
const numbers = computed(() =>
  (['larva', 'pupa'] as Stage[])
    .filter(known)
    .map(stage => ({ stage, tally: tally(stage), sheet: props.totals[stage] ?? 0, cage: stage === 'larva' ? props.expected.larvae : props.expected.pupae })),
)
const photos = computed(() => data.value?.photos ?? [])
/** Days newest first (today always, for its photos); Emergidos' rows whose ID an event already names are left out. */
const days = computed(() => {
  const named = new Set((data.value?.events ?? []).flatMap(e => e.ids))
  const out = new Map<string, { events: ClutchEvent[]; young: YoungRow[]; photos: ClutchPhoto[] }>()
  const at = (day: string) => out.get(day) ?? (out.set(day, { events: [], young: [], photos: [] }), out.get(day)!)
  for (const e of data.value?.events ?? []) at(e.day).events.push(e)
  for (const y of data.value?.young ?? []) if (!named.has(y.id)) at(y.day ?? '').young.push(y)
  const events = new Set((data.value?.events ?? []).map(e => e.id))
  // Photos of an event go with it; the rest are the day's.
  for (const p of photos.value) if (!p.eventId || !events.has(p.eventId)) at(p.day).photos.push(p)
  return [...out.entries()].sort((a, b) => b[0].localeCompare(a[0]))
})
const ofEvent = (e: ClutchEvent) => photos.value.filter(p => p.eventId === e.id)
const sending = computed(() => uploads.uploads.filter(u => u.recordId === props.recordId))
const time = (iso: string) => new Date(iso).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })
const mine = (e: ClutchEvent) => e.actor === session.user?.id || ['reviewer', 'admin'].includes(session.user?.role ?? '')
const removing = ref<string | null>(null)
async function remove(e: ClutchEvent) {
  removing.value = e.id
  try {
    await props.day.removeEvent(e.id)
  } catch (err) {
    notify(errorText(err), 'error')
  } finally {
    removing.value = null
  }
}
const explain = ref(false)

// --- Photos: added for a day (or one of its events), opened to zoom
const adding = ref<{ day: string; eventId: string | null } | null>(null)
const eventsOf = (day: string) => (data.value?.events ?? []).filter(e => e.day === day)
const viewing = ref<number | null>(null)
const open = (p: ClutchPhoto) => (viewing.value = photos.value.findIndex(x => x.id === p.id))
const removed = (id: string) => props.record.photoRemoved(id)
const progress = (u: (typeof sending.value)[number]) => (u.total ? Math.round((u.sent / u.total) * 100) : 0)
</script>

<template>
  <section class="border-b border-stone-100 py-3" :aria-label="$t('Eventos y fotos (solo en la app)')">
    <div class="flex items-center gap-2">
      <h3 class="field-label mb-0 flex-1">{{ $t('Eventos y fotos (solo en la app)') }}</h3>
      <button type="button" class="flex h-9 items-center gap-1 text-xs text-stone-600 underline" :aria-expanded="explain" @click="explain = !explain">
        <Info :size="14" /> {{ $t('¿Qué significa cada número?') }}
      </button>
    </div>
    <div v-if="explain" class="mt-1 space-y-1 rounded-md bg-stone-100 px-3 py-2 text-xs text-stone-700">
      <p>{{ $t('En la jaula: las que deberían estar hoy para contarlas (las del número menos las que ya pasaron a pupa y las preservadas).') }}</p>
      <p>
        {{
          subtract
            ? $t('NUMBER OF LARVAE en la hoja: el equipo resta las preservadas, como las que murieron o desaparecieron.')
            : $t('NUMBER OF LARVAE en la hoja: las larvas usadas. Las preservadas no se restan; solo las que murieron o desaparecieron (20 larvas, 10 preservadas, 5 pupas, 5 murieron → 15).')
        }}
      </p>
      <p>{{ $t('Los eventos y las fotos se guardan solo en la app (no en Google Sheets), con quién y cuándo; cada evento escribe también su nota en NOTES. Las larvas preservadas en Emergidos cuentan también.') }}</p>
    </div>
    <ul v-if="numbers.length" class="mt-2 space-y-2">
      <li v-for="s in numbers" :key="s.stage" class="rounded-lg border border-stone-200 bg-white px-3 py-2">
        <p class="text-xs font-semibold tracking-wide text-stone-500 uppercase">{{ stageWord(s.stage) }}</p>
        <div class="mt-1 grid grid-cols-3 gap-2 text-center">
          <div>
            <p class="text-xl font-semibold tabular-nums">{{ s.cage ?? '—' }}</p>
            <p class="text-[11px] leading-tight text-stone-600">{{ $t('en la jaula (para contar hoy)') }}</p>
          </div>
          <div>
            <p class="text-xl font-semibold tabular-nums">{{ s.tally.preserved }}</p>
            <p class="text-[11px] leading-tight text-stone-600">{{ $t('preservadas') }}</p>
          </div>
          <div>
            <p class="text-xl font-semibold tabular-nums">{{ s.sheet }}</p>
            <p class="text-[11px] leading-tight text-stone-600">{{ $t('en la hoja') }}</p>
          </div>
        </div>
        <p class="mt-1 text-xs text-stone-600 tabular-nums">
          {{
            [
              s.tally.gained ? `+${s.tally.gained} ${s.stage === 'larva' ? $t('eclosionaron') : $t('pupas nuevas')}` : '',
              s.tally.died ? `−${s.tally.died} ${$t('murieron')}` : '',
              s.tally.disappeared ? `−${s.tally.disappeared} ${$t('desaparecieron')}` : '',
              s.tally.preserved ? `${s.tally.preserved} ${$t('se preservaron')}` : '',
            ]
              .filter(Boolean)
              .join(' · ')
          }}
          <span class="text-stone-500">· {{ $t('registrado en la app') }}</span>
        </p>
      </li>
    </ul>

    <!-- Photos of today (the camera, the gallery), and those still on their way. -->
    <div v-if="canEdit && canPhoto" class="mt-2 flex items-center gap-2">
      <button type="button" class="btn h-11 px-3" @click="adding = { day: todayIso(), eventId: null }">
        <Camera :size="18" /> {{ $t('Foto de hoy') }}
      </button>
      <span class="text-xs text-stone-500">{{ $t('Del clutch, o de un evento (+5 eclosionaron, −1 desapareció)') }}</span>
    </div>
    <ul v-if="sending.length" class="mt-2 space-y-1.5" :aria-label="$t('Fotos enviándose')">
      <li v-for="u in sending" :key="u.id" class="flex items-center gap-2 rounded-md border border-stone-200 bg-white p-1.5 text-sm">
        <img v-if="u.preview" :src="u.preview" alt="" class="h-12 w-12 shrink-0 rounded object-cover" />
        <span v-else class="grid h-12 w-12 shrink-0 place-items-center rounded bg-stone-100"><Loader2 :size="18" class="animate-spin text-stone-500" /></span>
        <span class="min-w-0 flex-1">
          <span class="block text-xs" :class="u.status === 'failed' ? 'text-red-800' : 'text-stone-700'">
            <template v-if="u.status === 'preparing'">{{ $t('Achicando la foto…') }}</template>
            <template v-else-if="u.status === 'sending'">{{ $t('Enviando… {p} %', { p: progress(u) }) }}</template>
            <template v-else-if="u.status === 'waiting'">{{ u.attempts ? $t('Sin conexión: se reintenta sola ({n})', { n: u.attempts }) : $t('En cola') }}</template>
            <template v-else-if="u.status === 'done'">{{ $t('Foto guardada') }}</template>
            <template v-else>{{ u.error || $t('No se pudo enviar') }}</template>
          </span>
          <span class="mt-1 block h-1.5 overflow-hidden rounded bg-stone-100">
            <span class="block h-full bg-brand-600 transition-[width]" :style="{ width: `${u.status === 'done' ? 100 : progress(u)}%` }" />
          </span>
        </span>
        <button v-if="u.status === 'failed'" type="button" class="btn h-11 shrink-0 px-2" @click="uploads.retry(u.id)"><RotateCcw :size="16" /> {{ $t('Reintentar') }}</button>
        <button
          v-if="u.status !== 'done' && u.status !== 'sending'"
          type="button"
          class="grid h-11 w-11 shrink-0 place-items-center text-stone-500"
          :aria-label="$t('No enviar esta foto')"
          @click="uploads.discard(u.id)"
        >
          <X :size="16" />
        </button>
      </li>
    </ul>

    <p v-if="!days.length" class="mt-1 text-sm text-stone-500">
      {{ canEdit ? $t('Nada todavía. Al restar con −N la app pregunta si murieron, desaparecieron o se preservaron.') : $t('Nada todavía.') }}
    </p>
    <ol v-else class="mt-2 space-y-2">
      <li v-for="[d, g] in days" :key="d">
        <div class="flex items-center gap-2">
          <p class="flex-1 text-xs font-medium text-stone-500">{{ d ? dayLabel(d) : $t('sin fecha') }}</p>
          <button
            v-if="canEdit && canPhoto && d"
            type="button"
            class="flex h-9 items-center gap-1 px-1 text-xs text-stone-600"
            :aria-label="$t('Foto de este día')"
            @click="adding = { day: d, eventId: null }"
          >
            <Camera :size="15" />
          </button>
        </div>
        <ul class="mt-0.5 divide-y divide-stone-100 rounded-md border border-stone-200 bg-white">
          <li v-for="e in g.events" :key="e.id" class="py-1 pr-1 pl-3 text-sm">
            <div class="flex items-center gap-2">
              <span class="min-w-0 flex-1 break-words tabular-nums">{{ eventLine(e) }}</span>
              <span class="shrink-0 text-xs text-stone-500">{{ initials(e.name || e.username || '') }} {{ time(e.createdAt) }}</span>
              <button
                v-if="canEdit && canPhoto"
                type="button"
                class="grid h-11 w-11 shrink-0 place-items-center rounded-md text-stone-500 active:bg-stone-100"
                :aria-label="$t('Foto de este evento')"
                :title="$t('Foto de este evento')"
                @click="adding = { day: e.day, eventId: e.id }"
              >
                <Camera :size="16" />
              </button>
              <button
                v-if="canEdit && mine(e)"
                type="button"
                class="grid h-11 w-11 shrink-0 place-items-center rounded-md text-stone-500 active:bg-stone-100"
                :aria-label="$t('Borrar este evento')"
                :title="$t('Borrar este evento')"
                :disabled="removing === e.id"
                @click="remove(e)"
              >
                <Trash2 :size="16" />
              </button>
            </div>
            <div v-if="ofEvent(e).length" class="flex gap-1.5 overflow-x-auto pb-1">
              <button v-for="p in ofEvent(e)" :key="p.id" type="button" class="shrink-0" :aria-label="p.note || $t('Foto de este evento')" @click="open(p)">
                <img :src="photoUrl(p.id, 'thumb')" :alt="p.note || ''" loading="lazy" class="h-16 w-16 rounded object-cover" />
              </button>
            </div>
          </li>
          <li v-for="y in g.young" :key="y.id" class="flex items-center gap-2 px-3 py-1.5 text-sm">
            <span class="min-w-0 flex-1">
              {{ stageWord(y.stage) }}: {{ y.kind === 'preserved' ? $t('se preservó') : $t('se encontró muerta') }}
              <span class="font-medium">{{ y.id }}</span> <span class="text-xs text-stone-500">{{ y.lifestage }}</span>
            </span>
            <span class="shrink-0 text-xs text-stone-500">{{ $t('Emergidos') }}{{ y.day ? ` · ${formatSerial(isoToSerial(y.day))}` : '' }}</span>
          </li>
          <li v-if="g.photos.length" class="flex gap-1.5 overflow-x-auto px-3 py-1.5">
            <button v-for="p in g.photos" :key="p.id" type="button" class="shrink-0 text-left" :aria-label="p.note || $t('Foto del día')" @click="open(p)">
              <img :src="photoUrl(p.id, 'thumb')" :alt="p.note || ''" loading="lazy" class="h-16 w-16 rounded object-cover" />
              <span v-if="p.note" class="block w-16 truncate text-[11px] text-stone-600">{{ p.note }}</span>
            </button>
          </li>
        </ul>
      </li>
    </ol>
    <ClutchPhotoAdd
      v-if="adding"
      :record-id="recordId"
      :clutch="clutch"
      :day="adding.day"
      :events="eventsOf(adding.day)"
      :event-id="adding.eventId"
      @close="adding = null"
    />
    <ClutchPhotoViewer
      v-if="viewing !== null && viewing >= 0 && photos.length"
      v-model="viewing"
      :photos="photos"
      :events="data?.events ?? []"
      :initials="initials"
      @removed="removed"
      @close="viewing = null"
    />
  </section>
</template>
