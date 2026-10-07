<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref } from 'vue'
import { Camera, Check, Trash2, X } from 'lucide-vue-next'
import DateField from '../DateField.vue'
import type { ClutchEvent } from '../../lib/clutches'
import { photoUrl, type ClutchPhoto } from '../../lib/clutchPhotos'
import { dayLabel, isoToSerial, serialToIso, todayIso } from '../../lib/dates'
import { t } from '../../lib/i18n'
import { kindWord } from './eventWords'

/**
 * A term of a count, opened from its chip: its event (what happened, the day,
 * or that it is not known, the group, a note, its photos, who recorded it),
 * to correct what is said of it, add photos, or delete it with its term. A
 * term written before the app (in the sheet or the notebook) has no event:
 * it can only be taken out of the sum.
 */
const props = defineProps<{
  field: string
  term: number
  event: ClutchEvent | null
  /** The group's name, when the count has groups. */
  group: string | null
  photos: ClutchPhoto[]
  canEdit: boolean
  /** The person may change or delete it (theirs, or a reviewer's or admin's). */
  mine: boolean
  canPhoto: boolean
  initials: (name: string) => string
}>()
const emit = defineEmits<{
  close: []
  save: [patch: { day?: string; dayKnown?: boolean; kind?: string; note?: string | null }]
  remove: []
  addPhoto: []
  viewPhoto: [index: number]
}>()

const e = computed(() => props.event)
const day = ref(e.value?.day ?? todayIso())
const dayKnown = ref(e.value?.dayKnown !== false)
const kind = ref(e.value?.kind ?? '')
const note = ref(e.value?.note ?? '')
const changed = computed(
  () => !!e.value && (day.value !== e.value.day || dayKnown.value !== (e.value.dayKnown !== false) || kind.value !== e.value.kind || (note.value.trim() || null) !== (e.value.note ?? null)),
)
const causes = computed(() => (e.value && (e.value.kind === 'died' || e.value.kind === 'disappeared') ? (['died', 'disappeared'] as const) : null))
const hatched = computed(() => e.value?.kind === 'hatched')
const label = computed(() => {
  const sign = props.term < 0 ? `−${-props.term}` : `+${props.term}`
  return e.value ? `${sign} ${kindWord(e.value.kind)}` : sign
})
const confirming = ref(false)
function save() {
  const out: { day?: string; dayKnown?: boolean; kind?: string; note?: string | null } = {}
  if (!e.value) return
  if (day.value !== e.value.day) out.day = day.value
  if (dayKnown.value !== (e.value.dayKnown !== false)) out.dayKnown = dayKnown.value
  if (kind.value !== e.value.kind) out.kind = kind.value
  if ((note.value.trim() || null) !== (e.value.note ?? null)) out.note = note.value.trim() || null
  emit('save', out)
}
const yesterday = () => serialToIso(isoToSerial(todayIso()) - 1)
const pickDay = (iso: string) => {
  if (iso && iso <= todayIso()) {
    day.value = iso
    dayKnown.value = true
  }
}
const onKey = (ev: KeyboardEvent) => ev.key === 'Escape' && emit('close')
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <div class="fixed inset-0 z-50 flex items-end justify-center bg-black/30 sm:items-center" @click.self="emit('close')">
    <section
      class="max-h-full w-full overflow-y-auto rounded-t-2xl bg-white p-4 pb-[calc(1rem+env(safe-area-inset-bottom))] shadow-xl sm:max-w-lg sm:rounded-2xl"
      role="dialog"
      :aria-label="label"
    >
      <header class="flex items-start gap-2">
        <div class="min-w-0 flex-1">
          <h2 class="text-xl font-semibold tabular-nums" :class="term < 0 ? 'text-red-800' : ''">{{ label }}</h2>
          <p class="text-xs text-stone-500">{{ field }}<template v-if="group"> · {{ $t('grupo {name}', { name: group }) }}</template></p>
        </div>
        <button class="btn-ghost h-11 w-11 justify-center" :aria-label="$t('Cerrar')" @click="emit('close')"><X :size="22" /></button>
      </header>

      <template v-if="e">
        <p class="mt-1 text-xs text-stone-500">{{ $t('Registrado por {who} · {when}', { who: initials(e.name || e.username || ''), when: new Date(e.createdAt).toLocaleString() }) }}</p>
        <section class="mt-3">
          <span class="field-label">{{ hatched ? $t('Día de la eclosión') : $t('Día') }}</span>
          <template v-if="canEdit && mine && e.kind !== 'transfer'">
            <div class="flex flex-wrap gap-2">
              <DateField :model-value="dayKnown ? day : ''" class="field-input h-11 w-40 text-base" @update:model-value="pickDay" />
              <button type="button" class="btn h-11 px-3" @click="pickDay(todayIso())">{{ $t('Hoy') }}</button>
              <button type="button" class="btn h-11 px-3" @click="pickDay(yesterday())">{{ $t('Ayer') }}</button>
              <button v-if="hatched" type="button" class="btn h-11 px-3" :class="{ 'border-brand-700 bg-brand-50': !dayKnown }" @click="dayKnown = false">
                {{ $t('Desconocida (ya grandes)') }}
              </button>
            </div>
          </template>
          <p class="mt-1 text-sm">{{ dayKnown ? dayLabel(day) : $t('NA: no se sabe (ya grandes)') }}</p>
        </section>
        <section v-if="causes && canEdit && mine" class="mt-3">
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
        <p v-if="e.ids.length" class="mt-2 text-sm">{{ e.ids.join(', ') }}</p>
        <label class="mt-3 block">
          <span class="field-label">{{ $t('Nota (opcional, en inglés)') }}</span>
          <input v-model="note" class="field-input h-11 text-base" maxlength="200" autocomplete="off" :disabled="!canEdit || !mine" enterkeyhint="done" />
        </label>
        <section class="mt-3">
          <div class="flex items-center gap-2">
            <span class="field-label mb-0 flex-1">{{ $t('Fotos') }}</span>
            <button v-if="canEdit && canPhoto" type="button" class="btn h-10 px-3" @click="emit('addPhoto')"><Camera :size="16" /> {{ $t('Añadir foto') }}</button>
          </div>
          <div v-if="photos.length" class="mt-1 flex gap-1.5 overflow-x-auto pb-1">
            <button v-for="(p, i) in photos" :key="p.id" type="button" class="shrink-0" :aria-label="p.note || $t('Foto {n}', { n: i + 1 })" @click="emit('viewPhoto', i)">
              <img :src="photoUrl(p.id, 'thumb')" :alt="p.note || ''" loading="lazy" class="h-16 w-16 rounded object-cover" />
            </button>
          </div>
          <p v-else class="text-sm text-stone-500">{{ $t('Sin fotos (opcional).') }}</p>
        </section>
      </template>
      <p v-else class="mt-2 text-sm text-stone-700">{{ $t('Escrito en la hoja o en el cuaderno, sin evento en la app: sin fecha ni fotos.') }}</p>

      <div v-if="confirming" class="mt-4 rounded-lg border border-red-300 bg-red-50 p-2.5" role="alertdialog">
        <p class="text-sm font-medium text-red-900">{{ $t('¿Quitar {term} de la suma de {field}?', { term: term < 0 ? `−${-term}` : `+${term}`, field }) }}</p>
        <p v-if="e" class="text-xs text-red-900">{{ $t('Se borra el evento con su término; sus fotos quedan como fotos del día.') }}</p>
        <div class="mt-2 flex gap-2">
          <button type="button" class="btn h-11 flex-1" @click="confirming = false">{{ $t('Cancelar') }}</button>
          <button type="button" class="h-11 flex-1 rounded-lg bg-red-700 px-3 font-semibold text-white" @click="emit('remove')"><Trash2 :size="16" class="-mt-0.5 inline" /> {{ $t('Quitar de la suma') }}</button>
        </div>
      </div>
      <div v-else-if="canEdit" class="mt-4 flex gap-2">
        <button v-if="!e || (mine && e.kind !== 'transfer')" type="button" class="btn h-12 flex-1 border-red-300 text-red-800" @click="confirming = true"><Trash2 :size="16" /> {{ e ? $t('Borrar este evento') : $t('Quitar de la suma') }}</button>
        <button v-if="e && mine" type="button" class="btn-primary h-12 flex-[2]" :disabled="!changed" @click="save"><Check :size="18" /> {{ $t('Guardar') }}</button>
      </div>
      <p v-if="e?.kind === 'transfer'" class="mt-2 text-xs text-stone-600">{{ $t('Un traspaso entre grupos se cambia con «Reagrupar» (o «Deshacer el último paso»).') }}</p>
      <p v-if="canEdit && e && !mine" class="mt-2 text-xs text-stone-500">{{ t('Solo quien lo registró (o un revisor) puede cambiarlo.') }}</p>
    </section>
  </div>
</template>
