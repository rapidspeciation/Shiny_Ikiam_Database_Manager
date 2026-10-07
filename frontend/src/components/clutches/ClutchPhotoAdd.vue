<script setup lang="ts">
import { onBeforeUnmount, onMounted, ref } from 'vue'
import { Camera, Images, X } from 'lucide-vue-next'
import { usePhotoUploads } from '../../composables/usePhotoUploads'
import { PHOTO_CAPTIONS } from '../../lib/clutchPhotos'
import type { ClutchEvent } from '../../lib/clutches'
import { dayLabel } from '../../lib/dates'
import { eventLine } from './eventWords'

/**
 * Photos for a clutch's day: what they show (one of that day's events, "+5
 * eclosionaron", "−1 desaparecieron", or just the day) and a short caption are
 * chosen first, then the camera or the gallery (several at once); choosing them
 * starts sending at once (usePhotoUploads) and closes this sheet.
 */
const props = defineProps<{
  recordId: string
  clutch: string
  /** The day the photos are of (ISO). */
  day: string
  /** That day's events of the clutch, to link a photo to. */
  events: ClutchEvent[]
  /** The event the camera button was next to. */
  eventId?: string | null
  /** The clutch's groups (box A's larvae…), to link a photo to one. */
  groups?: { id: string; name: string }[]
  /** The group the camera button was for. */
  groupId?: string | null
}>()
const emit = defineEmits<{ close: [] }>()
const uploads = usePhotoUploads()
const link = ref<string | null>(props.eventId ?? null)
const group = ref<string | null>(props.eventId ? null : (props.groupId ?? null))
const pickEvent = (id: string | null) => ((link.value = id), (group.value = null))
const pickGroup = (id: string) => ((group.value = id), (link.value = null))
const note = ref('')
const camera = ref<HTMLInputElement>()
const gallery = ref<HTMLInputElement>()
function chosen(e: Event) {
  const input = e.target as HTMLInputElement
  const files = [...(input.files ?? [])].filter(f => f.type.startsWith('image/') || !f.type)
  input.value = ''
  if (!files.length) return
  void uploads.add(files, { recordId: props.recordId, clutch: props.clutch, day: props.day, eventId: link.value, groupId: group.value, note: note.value.trim() || null })
  emit('close')
}
const onKey = (e: KeyboardEvent) => e.key === 'Escape' && emit('close')
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <div class="fixed inset-0 z-50 flex items-end justify-center bg-black/30 sm:items-center" @click.self="emit('close')">
    <section class="max-h-full w-full overflow-y-auto rounded-t-2xl bg-white p-4 pb-[calc(1rem+env(safe-area-inset-bottom))] shadow-xl sm:max-w-lg sm:rounded-2xl" role="dialog" :aria-label="$t('Fotos del clutch {clutch}', { clutch })">
      <header class="flex items-center gap-2">
        <Camera :size="20" class="text-stone-600" />
        <h2 class="min-w-0 flex-1 text-lg font-semibold">{{ $t('Fotos del clutch {clutch}', { clutch }) }}</h2>
        <button class="btn-ghost h-11 w-11 justify-center" :aria-label="$t('Cerrar')" @click="emit('close')"><X :size="22" /></button>
      </header>
      <p class="text-sm text-stone-600">{{ dayLabel(day) }} · {{ $t('solo en la app (no van a Google Sheets)') }}</p>

      <h3 class="mt-3 field-label">{{ $t('¿Qué muestran?') }}</h3>
      <div class="flex flex-wrap gap-1.5" role="radiogroup">
        <button
          type="button"
          role="radio"
          class="min-h-10 rounded-full border px-3 text-sm"
          :class="link === null && group === null ? 'border-brand-700 bg-brand-50 font-medium text-brand-800' : 'border-stone-300 bg-white text-stone-700'"
          :aria-checked="link === null && group === null"
          @click="pickEvent(null)"
        >
          {{ $t('El clutch ese día') }}
        </button>
        <button
          v-for="e in events"
          :key="e.id"
          type="button"
          role="radio"
          class="min-h-10 rounded-full border px-3 text-sm tabular-nums"
          :class="link === e.id ? 'border-brand-700 bg-brand-50 font-medium text-brand-800' : 'border-stone-300 bg-white text-stone-700'"
          :aria-checked="link === e.id"
          @click="pickEvent(e.id)"
        >
          {{ eventLine(e) }}
        </button>
        <button
          v-for="g in groups ?? []"
          :key="g.id"
          type="button"
          role="radio"
          class="min-h-10 rounded-full border px-3 text-sm"
          :class="group === g.id ? 'border-brand-700 bg-brand-50 font-medium text-brand-800' : 'border-stone-300 bg-white text-stone-700'"
          :aria-checked="group === g.id"
          @click="pickGroup(g.id)"
        >
          {{ g.name }}
        </button>
      </div>

      <label class="mt-3 block">
        <span class="field-label">{{ $t('Leyenda (opcional, en inglés)') }}</span>
        <div class="mb-1.5 flex flex-wrap gap-1.5">
          <button v-for="c in PHOTO_CAPTIONS" :key="c" type="button" class="min-h-9 rounded-full border border-stone-300 bg-white px-3 text-sm active:bg-stone-100" @click="note = c">
            {{ c }}
          </button>
        </div>
        <input v-model="note" class="field-input h-11 text-base" maxlength="200" autocomplete="off" enterkeyhint="done" />
      </label>

      <div class="mt-4 grid grid-cols-2 gap-2">
        <button type="button" class="btn-primary h-14 flex-col justify-center text-base leading-tight" @click="camera?.click()">
          <Camera :size="22" /> {{ $t('Cámara') }}
        </button>
        <button type="button" class="btn h-14 flex-col justify-center text-base leading-tight" @click="gallery?.click()">
          <Images :size="22" /> {{ $t('Galería') }}
        </button>
      </div>
      <p class="mt-2 text-xs text-stone-500">{{ $t('Se achican en el teléfono (2560 px) antes de enviarse; sin la ubicación GPS.') }}</p>
      <input ref="camera" type="file" accept="image/*" capture="environment" class="hidden" @change="chosen" />
      <input ref="gallery" type="file" accept="image/*" multiple class="hidden" @change="chosen" />
    </section>
  </div>
</template>
