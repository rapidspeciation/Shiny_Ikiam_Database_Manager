<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref } from 'vue'
import { Camera, Check, Loader2, Minus, Plus, Trash2, X } from 'lucide-vue-next'
import DateField from '../DateField.vue'
import type { ClutchEvent, Loss, Stage } from '../../lib/clutches'
import { photoUrl, type ClutchPhoto } from '../../lib/clutchPhotos'
import { dayLabel, isoToSerial, serialToIso, todayIso } from '../../lib/dates'
import { t } from '../../lib/i18n'
import { kindWord } from './eventWords'

/**
 * A term of a count, opened from its chip (any, old or new): mostly to give it
 * photos (several: added, seen, taken away in the viewer) and a note, to say
 * its day, and to take a loss from it («− De este número»: 1 died of the +27,
 * the cause first, then how many). A term written before the app has no event
 * yet: it gets one the first time it is given a photo, a note or a loss (its
 * day: the stage's first date when it is the first +, else not known), without
 * changing the formula. Taking it out of the sum is there too, small, at the
 * bottom, with a confirm.
 */
const props = defineProps<{
  field: string
  /** The stage of the count (none for the dissections: no events). */
  stage: Stage | null
  term: number
  event: ClutchEvent | null
  /** The day a term written before the app would get (ISO), or null: not known. */
  adoptDay?: string | null
  /** The group's name, when the count has groups. */
  group: string | null
  photos: ClutchPhoto[]
  /** The losses taken from this term (their events). */
  losses: ClutchEvent[]
  canEdit: boolean
  /** The person may change or delete it (theirs, a term of the sheet, or a reviewer's or admin's). */
  mine: boolean
  canPhoto: boolean
  /** Something is being recorded (the term getting its event, a loss). */
  busy?: boolean
  subtractPreserved: boolean
  /** The line a loss would add to NOTES, '' when NOTES cannot be written. */
  noteFor?: (e: { kind: Loss; count: number }) => string
  initials: (name: string) => string
}>()
const emit = defineEmits<{
  close: []
  save: [patch: { day?: string; dayKnown?: boolean; kind?: string; note?: string | null }]
  remove: []
  addPhoto: []
  viewPhoto: [index: number]
  loss: [loss: { kind: Loss; count: number }]
}>()

const e = computed(() => props.event)
const initialDay = () => e.value?.day ?? props.adoptDay ?? todayIso()
const initialKnown = () => (e.value ? e.value.dayKnown !== false : !!props.adoptDay)
const day = ref(initialDay())
const dayKnown = ref(initialKnown())
const kind = ref(e.value?.kind ?? '')
const note = ref(e.value?.note ?? '')
const changed = computed(
  () =>
    day.value !== initialDay() ||
    dayKnown.value !== initialKnown() ||
    (!!e.value && kind.value !== e.value.kind) ||
    (note.value.trim() || null) !== (e.value?.note ?? null),
)
const sign = computed(() => (props.term < 0 ? `−${-props.term}` : `+${props.term}`))
const label = computed(() => (e.value && !(e.value.adopted && props.term < 0) ? `${sign.value} ${kindWord(e.value.kind)}` : sign.value))
const causes = computed(() => (e.value && (e.value.kind === 'died' || e.value.kind === 'disappeared') ? (['died', 'disappeared'] as const) : null))
const gain = computed(() => props.term > 0 && (!e.value || ['laid', 'hatched', 'pupated', 'emerged'].includes(e.value.kind)))
/** Events can be kept for this count (a stage's, not the dissections). */
const withEvents = computed(() => !!props.stage)
const editable = computed(() => props.canEdit && withEvents.value && props.mine && e.value?.kind !== 'transfer')
function save() {
  const out: { day?: string; dayKnown?: boolean; kind?: string; note?: string | null } = {}
  if (day.value !== initialDay() || dayKnown.value !== initialKnown()) {
    out.dayKnown = dayKnown.value
    if (dayKnown.value) out.day = day.value
  }
  if (e.value && kind.value !== e.value.kind) out.kind = kind.value
  if ((note.value.trim() || null) !== (e.value?.note ?? null)) out.note = note.value.trim() || null
  emit('save', out)
}
const yesterday = () => serialToIso(isoToSerial(todayIso()) - 1)
const pickDay = (iso: string) => {
  if (iso && iso <= todayIso() && iso >= '2020-01-01') {
    day.value = iso
    dayKnown.value = true
  }
}

// «− De este número»: the cause first, then how many (not more than the term has left)
const taken = computed(() => props.losses.reduce((n, l) => n + l.count, 0))
const left = computed(() => Math.max(0, props.term - taken.value))
const losing = ref(false)
const lossKind = ref<Loss | null>(null)
const lossText = ref('1')
const lossBox = ref<HTMLInputElement>()
const lossN = computed(() => (/^\d{1,4}$/.test(lossText.value.trim()) ? Number(lossText.value.trim()) : null))
const lossWord = (k: Loss) => (k === 'died' ? t('Murieron') : k === 'disappeared' ? t('Desaparecieron') : t('Se preservaron'))
async function pick(k: Loss) {
  lossKind.value = k
  await nextTick()
  lossBox.value?.select()
}
const stepLoss = (by: number) => (lossText.value = String(Math.max(1, Math.min(left.value || 1, (lossN.value ?? 0) + by))))
const lossError = computed(() => (lossN.value !== null && lossN.value > left.value ? t('Este número solo tiene {n}', { n: left.value }) : ''))
const lossNote = computed(() => (lossKind.value && lossN.value ? (props.noteFor?.({ kind: lossKind.value, count: lossN.value }) ?? '') : ''))
function confirmLoss() {
  if (!lossKind.value || !lossN.value || lossError.value) return
  emit('loss', { kind: lossKind.value, count: lossN.value })
}

const confirming = ref(false)
const onKey = (ev: KeyboardEvent) => {
  if (ev.key !== 'Escape') return
  if (losing.value) losing.value = false
  else if (confirming.value) confirming.value = false
  else emit('close')
}
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <section class="mt-2 rounded-xl border-2 border-brand-600 bg-white p-3 shadow-sm" role="region" :aria-label="label">
      <header class="flex items-start gap-2">
        <div class="min-w-0 flex-1">
          <h3 class="text-lg font-semibold tabular-nums" :class="term < 0 ? 'text-red-800' : ''">
            {{ label }}<span v-if="losses.length" class="ml-1 text-base font-normal text-red-800">(−{{ taken }})</span>
          </h3>
          <p class="text-xs text-stone-500">{{ field }}<template v-if="group"> · {{ $t('grupo {name}', { name: group }) }}</template></p>
          <p v-if="e && !e.adopted" class="text-xs text-stone-500">{{ $t('Registrado por {who} · {when}', { who: initials(e.name || e.username || ''), when: new Date(e.createdAt).toLocaleString() }) }}</p>
          <p v-else-if="!e && withEvents" class="text-xs text-stone-500">{{ $t('Escrito en la hoja o en el cuaderno') }}</p>
        </div>
        <Loader2 v-if="busy" :size="18" class="mt-2 animate-spin text-stone-500" />
        <button type="button" class="grid h-10 w-10 shrink-0 place-items-center rounded-md text-stone-500 active:bg-stone-100" :aria-label="$t('Cerrar')" :title="$t('Cerrar (Esc)')" @click="emit('close')"><X :size="20" /></button>
      </header>

      <template v-if="withEvents">
        <!-- Photos first: what a number is opened for, most of the time. -->
        <section class="mt-3">
          <div class="flex items-center gap-2">
            <span class="field-label mb-0 flex-1">{{ $t('Fotos') }}</span>
            <button v-if="canEdit && canPhoto" type="button" class="btn-primary h-11 px-3" :disabled="busy" @click="emit('addPhoto')">
              <Camera :size="18" /> {{ $t('Añadir foto') }}
            </button>
          </div>
          <div v-if="photos.length" class="mt-1.5 flex gap-1.5 overflow-x-auto pb-1">
            <button v-for="(p, i) in photos" :key="p.id" type="button" class="shrink-0" :aria-label="p.note || $t('Foto {n}', { n: i + 1 })" @click="emit('viewPhoto', i)">
              <img :src="photoUrl(p.id, 'thumb')" :alt="p.note || ''" loading="lazy" class="h-20 w-20 rounded object-cover" />
            </button>
          </div>
          <p v-else class="text-sm text-stone-500">{{ $t('Sin fotos (opcional).') }}</p>
        </section>

        <label class="mt-3 block">
          <span class="field-label">{{ $t('Nota (opcional, en inglés)') }}</span>
          <input v-model="note" class="field-input h-11 text-base" maxlength="200" autocomplete="off" :disabled="!editable" enterkeyhint="done" @keydown.enter.prevent="changed && save()" />
        </label>

        <section v-if="e?.kind !== 'transfer'" class="mt-3">
          <span class="field-label">{{ gain && stage === 'larva' ? $t('Día de la eclosión') : $t('Día') }}</span>
          <div v-if="editable" class="flex flex-wrap gap-2">
            <DateField :model-value="dayKnown ? day : ''" class="field-input h-11 w-40 text-base" @update:model-value="pickDay" />
            <button type="button" class="btn h-11 px-3" @click="pickDay(todayIso())">{{ $t('Hoy') }}</button>
            <button type="button" class="btn h-11 px-3" @click="pickDay(yesterday())">{{ $t('Ayer') }}</button>
            <button v-if="gain" type="button" class="btn h-11 px-3" :class="{ 'border-brand-700 bg-brand-50': !dayKnown }" @click="dayKnown = false">
              {{ $t('Desconocida') }}
            </button>
          </div>
          <p class="mt-1 text-sm">{{ dayKnown ? dayLabel(day) : $t('NA: no se sabe') }}</p>
        </section>
        <section v-if="causes && editable" class="mt-3">
          <span class="field-label">{{ $t('¿Qué pasó?') }}</span>
          <div class="flex gap-2">
            <button
              v-for="c in causes"
              :key="c"
              type="button"
              class="btn h-11 flex-1"
              :class="kind === c ? 'border-red-400 bg-red-50 font-semibold text-red-800' : ''"
              :aria-pressed="kind === c"
              @click="kind = c"
            >
              {{ kindWord(c) }}
            </button>
          </div>
        </section>
        <p v-if="e?.ids.length" class="mt-2 text-sm">{{ e.ids.join(', ') }}</p>

        <!-- Losses taken from this number, and one more. -->
        <section v-if="gain && stage !== 'adult'" class="mt-4 border-t border-stone-200 pt-3">
          <ul v-if="losses.length" class="mb-2 space-y-0.5 text-sm text-red-800 tabular-nums">
            <li v-for="l in losses" :key="l.id">−{{ l.count }} {{ kindWord(l.kind) }} · {{ dayLabel(l.day) }}</li>
          </ul>
          <button v-if="canEdit && !losing" type="button" class="btn h-12 w-full border-red-300 text-base text-red-800" :disabled="busy || !left" @click="(losing = true), (lossKind = null), (lossText = '1')">
            <Minus :size="18" /> {{ $t('De este número') }} <span class="text-xs font-normal text-stone-500">({{ $t('quedan {n}', { n: left }) }})</span>
          </button>
          <div v-if="losing" class="rounded-lg border border-red-200 bg-red-50/40 p-2.5" role="group" :aria-label="$t('De este número')">
            <template v-if="!lossKind">
              <p class="text-sm font-medium">{{ $t('¿Qué pasó?') }} <span class="text-xs font-normal text-stone-500">{{ $t('primero la causa, luego cuántas') }}</span></p>
              <div class="mt-1.5 grid grid-cols-3 gap-1.5">
                <button v-for="k in (['died', 'disappeared', 'preserved'] as const)" :key="k" type="button" class="btn h-12 justify-center px-1" :class="k === 'preserved' ? 'border-sky-500 text-sky-900' : 'border-red-300 text-red-800'" @click="pick(k)">
                  {{ lossWord(k) }}
                </button>
              </div>
              <button type="button" class="mt-1.5 h-10 w-full text-sm text-stone-600" @click="losing = false">{{ $t('Cancelar') }}</button>
            </template>
            <template v-else>
              <div class="flex flex-wrap items-center gap-2">
                <button type="button" class="flex h-10 items-center rounded-full border border-red-300 bg-white px-3 text-sm font-semibold text-red-800" @click="lossKind = null">{{ lossWord(lossKind) }}</button>
                <span class="text-sm text-stone-700">{{ $t('¿Cuántas?') }}</span>
                <span class="ml-auto flex items-stretch overflow-hidden rounded-lg border border-stone-300 bg-white">
                  <button type="button" class="grid h-11 w-11 place-items-center active:bg-stone-100" :aria-label="$t('Una menos')" @click="stepLoss(-1)"><Minus :size="18" /></button>
                  <input
                    ref="lossBox"
                    v-model="lossText"
                    class="h-11 w-14 border-x border-stone-300 text-center text-xl font-semibold tabular-nums outline-none"
                    type="text"
                    inputmode="numeric"
                    maxlength="4"
                    autocomplete="off"
                    enterkeyhint="done"
                    :aria-label="$t('Cuántas')"
                    @keydown.enter.prevent="confirmLoss"
                  />
                  <button type="button" class="grid h-11 w-11 place-items-center active:bg-stone-100" :aria-label="$t('Una más')" @click="stepLoss(1)"><Plus :size="18" /></button>
                </span>
              </div>
              <p v-if="lossKind === 'preserved'" class="mt-1 text-xs text-stone-600">
                {{ subtractPreserved ? $t('Se restan de {field}, como dice el ajuste del equipo.', { field }) : $t('Se quedan en {field}, como dice el ajuste del equipo.', { field }) }}
              </p>
              <p v-if="lossNote" class="mt-1.5 text-xs break-words text-stone-700">
                {{ $t('Se añade a NOTES:') }} <span class="rounded bg-amber-50 px-1 text-stone-900">{{ lossNote }}</span>
              </p>
              <p v-if="lossError" class="mt-1 text-sm text-red-700">{{ lossError }}</p>
              <div class="mt-2 flex gap-2">
                <button type="button" class="btn h-11 flex-1" @click="losing = false">{{ $t('Cancelar') }}</button>
                <button type="button" class="btn-primary h-11 flex-[2]" :disabled="busy || !lossN || !!lossError" @click="confirmLoss">
                  <Check :size="18" /> {{ $t('Restar {n}', { n: lossN ?? '' }) }}
                </button>
              </div>
            </template>
          </div>
        </section>
      </template>

      <p v-if="e?.kind === 'transfer'" class="mt-2 text-xs text-stone-600">{{ $t('Un traspaso entre grupos se cambia con «Reagrupar» (o «Deshacer el último paso»).') }}</p>
      <p v-if="canEdit && e && !mine" class="mt-2 text-xs text-stone-500">{{ t('Solo quien lo registró (o un revisor) puede cambiarlo.') }}</p>

      <div class="mt-3 flex gap-2">
        <button type="button" class="btn h-11 flex-1" @click="emit('close')">{{ $t('Cancelar') }}</button>
        <button type="button" class="btn-primary h-11 flex-[2]" :disabled="busy" @click="editable && changed ? save() : emit('close')"><Check :size="18" /> {{ $t('Listo') }}</button>
      </div>

      <!-- Taking it out of the sum: secondary, at the bottom, with a confirm. -->
      <div v-if="canEdit && (!e || (mine && e.kind !== 'transfer'))" class="mt-4 border-t border-stone-100 pt-2">
        <div v-if="confirming" class="rounded-lg border border-red-300 bg-red-50 p-2.5" role="alertdialog">
          <p class="text-sm font-medium text-red-900">{{ $t('¿Quitar {term} de la suma de {field}?', { term: sign, field }) }}</p>
          <p v-if="e" class="text-xs text-red-900">{{ $t('Se borra el evento con su término; sus fotos quedan como fotos del día.') }}</p>
          <div class="mt-2 flex gap-2">
            <button type="button" class="btn h-11 flex-1" @click="confirming = false">{{ $t('Cancelar') }}</button>
            <button type="button" class="h-11 flex-1 rounded-lg bg-red-700 px-3 font-semibold text-white" @click="emit('remove')">
              <Trash2 :size="16" class="-mt-0.5 inline" /> {{ $t('Quitar de la suma') }}
            </button>
          </div>
        </div>
        <button v-else type="button" class="flex h-9 items-center gap-1 text-xs text-stone-500 underline" @click="confirming = true">
          <Trash2 :size="13" /> {{ $t('Quitar {term} de la suma…', { term: sign }) }}
        </button>
      </div>
  </section>
</template>
