<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref } from 'vue'
import { Camera, Trash2, X } from 'lucide-vue-next'
import PhotoViewer from '../assistant/PhotoViewer.vue'
import { api } from '../../lib/api'
import { linkedEvent, photoUrl, sizeText, type ClutchPhoto } from '../../lib/clutchPhotos'
import type { ClutchEvent } from '../../lib/clutches'
import { dayLabel } from '../../lib/dates'
import { errorText, notify } from '../../lib/notice'
import { useSession } from '../../stores/session'
import { t } from '../../lib/i18n'
import { eventLine } from './eventWords'

/**
 * A clutch's photos full screen, to zoom and pan (the proposals' PhotoViewer,
 * @panzoom/panzoom): the others as thumbnails above, and under the photo its
 * day, who took it, the event it shows ("Larvas: −1 desaparecieron") and its
 * caption. Its author (or a reviewer) can take it away.
 */
const props = defineProps<{
  photos: ClutchPhoto[]
  events: ClutchEvent[]
  initials: (name: string) => string
  /** One more photo can be taken from here (the photos of one chip's event). */
  canAdd?: boolean
}>()
const index = defineModel<number>({ required: true })
const emit = defineEmits<{ close: []; removed: [id: string]; add: [] }>()
const session = useSession()
const photo = computed(() => props.photos[Math.min(index.value, props.photos.length - 1)])
const url = (n: number, size: 'thumb' | 'view') => photoUrl(props.photos[n]?.id ?? '', size === 'thumb' ? 'thumb' : 'full')
const alt = (n: number) => t('Foto {n} del clutch', { n: n + 1 })
const event = computed(() => (photo.value ? linkedEvent(photo.value, props.events) : null))
const mine = computed(() => !!photo.value && (photo.value.actor === session.user?.id || ['reviewer', 'admin'].includes(session.user?.role ?? '')))
const removing = ref(false)
async function remove() {
  const p = photo.value
  if (!p || !confirm(t('¿Quitar esta foto? Se borra para todos.'))) return
  removing.value = true
  try {
    await api(`clutches/photos/${encodeURIComponent(p.id)}`, { method: 'DELETE', body: {} })
    emit('removed', p.id)
    if (props.photos.length <= 1) emit('close')
    else index.value = Math.max(0, index.value - 1)
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    removing.value = false
  }
}
const onKey = (e: KeyboardEvent) => {
  if (e.key === 'Escape') emit('close')
  if (e.key === 'ArrowRight' && index.value < props.photos.length - 1) index.value++
  if (e.key === 'ArrowLeft' && index.value > 0) index.value--
}
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <div v-if="photo" class="fixed inset-0 z-50 flex flex-col bg-stone-900" role="dialog" :aria-label="$t('Fotos del clutch')">
    <div class="flex items-center gap-2 bg-stone-900 px-2 py-1 text-stone-100">
      <p class="min-w-0 flex-1 truncate text-sm">
        {{ $t('Foto {n} de {total}', { n: index + 1, total: photos.length }) }} · {{ dayLabel(photo.day) }}
      </p>
      <button v-if="canAdd" class="flex h-11 items-center gap-1 rounded-md px-2 text-sm text-stone-100" @click="emit('add')">
        <Camera :size="20" /> {{ $t('Otra foto') }}
      </button>
      <button v-if="mine" class="grid h-11 w-11 place-items-center rounded-md text-stone-200" :aria-label="$t('Quitar esta foto')" :disabled="removing" @click="remove">
        <Trash2 :size="20" />
      </button>
      <button class="grid h-11 w-11 place-items-center rounded-md text-stone-100" :aria-label="$t('Cerrar')" @click="emit('close')"><X :size="24" /></button>
    </div>
    <div class="min-h-0 flex-1">
      <PhotoViewer :url="url" :count="photos.length" :photo="index" :alt="alt" @update:photo="index = $event" />
    </div>
    <div class="bg-stone-900 px-3 py-2 pb-[calc(0.5rem+env(safe-area-inset-bottom))] text-sm text-stone-100">
      <p v-if="event" class="font-medium">{{ eventLine(event) }}</p>
      <p v-else class="text-stone-300">{{ $t('Foto del día') }}</p>
      <p v-if="photo.note">{{ photo.note }}</p>
      <p class="text-xs text-stone-400">
        {{ initials(photo.name || photo.username || '') }} · {{ new Date(photo.createdAt).toLocaleString() }} · {{ photo.width }}×{{ photo.height }} · {{ sizeText(photo.bytes) }}
      </p>
    </div>
  </div>
</template>
