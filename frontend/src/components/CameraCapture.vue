<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { Camera, Check, Loader2, RotateCcw, SwitchCamera, X } from 'lucide-vue-next'
import { frameOf, useCamera } from '../composables/useCamera'
import { nextCamera, photoName } from '../lib/camera'
import { t } from '../lib/i18n'

/**
 * The computer's camera inside the app, for the «Cámara» buttons on a desktop
 * (lib/camera.ts decides): the live picture, «Tomar foto», then «Usar foto» or
 * «Repetir». With several cameras a button switches between them (the choice
 * is remembered). When the camera cannot open it says why and offers a file
 * instead. The camera turns off when this closes.
 */
defineProps<{ title?: string }>()
const emit = defineEmits<{ photo: [file: File]; file: []; close: [] }>()

const camera = useCamera()
const video = ref<HTMLVideoElement>()
const taken = ref<{ file: File; url: string } | null>(null)
const taking = ref(false)
const failedFrame = ref(false)
const takeButton = ref<HTMLButtonElement>()
const useButton = ref<HTMLButtonElement>()

watch([camera.stream, video], async ([s, v]) => {
  if (!v) return
  v.srcObject = s
  if (!s) return
  void v.play().catch(() => {})
  await nextTick()
  takeButton.value?.focus()
})

const problemText = computed(() => {
  if (failedFrame.value) return t('No se pudo tomar la foto: vuelve a intentar.')
  switch (camera.problem.value) {
    case 'denied':
      return t('Sin permiso para usar la cámara: permítelo en el navegador (el ícono junto a la dirección) y vuelve a intentar.')
    case 'none':
      return t('No se encontró ninguna cámara en este equipo.')
    case 'busy':
      return t('La cámara está ocupada por otro programa (una videollamada…): ciérralo y vuelve a intentar.')
    case 'insecure':
      return t('El navegador solo abre la cámara en una dirección segura (https://).')
    case 'other':
      return t('No se pudo abrir la cámara.')
    default:
      return ''
  }
})

async function take() {
  if (!video.value || taking.value) return
  taking.value = true
  failedFrame.value = false
  try {
    const file = await frameOf(video.value, photoName())
    taken.value = { file, url: URL.createObjectURL(file) }
    await nextTick()
    useButton.value?.focus()
  } catch {
    failedFrame.value = true
  } finally {
    taking.value = false
  }
}
function retake() {
  if (taken.value) URL.revokeObjectURL(taken.value.url)
  taken.value = null
}
function accept() {
  if (!taken.value) return
  const { file } = taken.value
  retake()
  camera.stop()
  emit('photo', file)
}
function close() {
  camera.stop()
  emit('close')
}
function chooseFile() {
  camera.stop()
  emit('file')
}
const switchCamera = () => void camera.use(nextCamera(camera.devices.value, camera.deviceId.value))
const retry = () => ((failedFrame.value = false), void camera.start())

/** Escape closes the camera, not the dialog under it. */
function onKey(e: KeyboardEvent) {
  if (e.key !== 'Escape') return
  e.stopImmediatePropagation()
  close()
}
onMounted(() => {
  window.addEventListener('keydown', onKey, { capture: true })
  void camera.start()
})
onBeforeUnmount(() => {
  window.removeEventListener('keydown', onKey, { capture: true })
  if (taken.value) URL.revokeObjectURL(taken.value.url)
})
</script>

<template>
  <div class="fixed inset-0 z-[60] flex items-center justify-center bg-black/60 p-2 sm:p-4" @click.self="close">
    <section class="flex max-h-full w-full max-w-3xl flex-col rounded-2xl bg-white p-3 shadow-xl sm:p-4" role="dialog" :aria-label="title || $t('Cámara')" data-camera-capture>
      <header class="flex items-center gap-2">
        <Camera :size="20" class="text-stone-600" />
        <h2 class="min-w-0 flex-1 truncate text-lg font-semibold">{{ title || $t('Cámara') }}</h2>
        <button
          v-if="camera.devices.value.length > 1 && !taken"
          type="button"
          class="btn-ghost h-11 gap-1.5 px-2 text-sm"
          :title="$t('Cambiar de cámara')"
          :disabled="camera.starting.value"
          @click="switchCamera"
        >
          <SwitchCamera :size="20" />
          <span class="hidden max-w-48 truncate sm:inline">{{ camera.devices.value.find(d => d.deviceId === camera.deviceId.value)?.label || $t('Cambiar de cámara') }}</span>
        </button>
        <button type="button" class="btn-ghost h-11 w-11 justify-center" :aria-label="$t('Cerrar')" @click="close"><X :size="22" /></button>
      </header>

      <div class="relative mt-2 flex min-h-56 flex-1 items-center justify-center overflow-hidden rounded-xl bg-stone-900">
        <video v-show="camera.stream.value && !taken" ref="video" class="max-h-[70vh] w-full object-contain" autoplay muted playsinline />
        <img v-if="taken" :src="taken.url" :alt="$t('Foto tomada')" class="max-h-[70vh] w-full object-contain" />
        <p v-if="camera.starting.value" class="absolute inset-0 flex items-center justify-center gap-2 text-sm text-white">
          <Loader2 :size="18" class="animate-spin" /> {{ $t('Abriendo la cámara…') }}
        </p>
        <div v-else-if="camera.problem.value && !camera.stream.value" class="p-4 text-center text-sm text-white">
          <p role="alert">{{ problemText }}</p>
          <div class="mt-3 flex flex-wrap justify-center gap-2">
            <button v-if="camera.problem.value !== 'insecure'" type="button" class="btn h-10" @click="retry">{{ $t('Reintentar') }}</button>
          </div>
        </div>
      </div>
      <p v-if="failedFrame" class="mt-2 text-sm text-red-700" role="alert">{{ problemText }}</p>

      <div class="mt-3 flex flex-wrap items-center gap-2">
        <button type="button" class="text-sm text-brand-700 underline underline-offset-2" @click="chooseFile">{{ $t('Elegir un archivo') }}</button>
        <span class="flex-1" />
        <template v-if="taken">
          <button type="button" class="btn h-12 px-4" @click="retake"><RotateCcw :size="18" /> {{ $t('Repetir') }}</button>
          <button ref="useButton" type="button" class="btn-primary h-12 px-4" @click="accept"><Check :size="18" /> {{ $t('Usar foto') }}</button>
        </template>
        <button v-else ref="takeButton" type="button" class="btn-primary h-12 px-5" :disabled="!camera.stream.value || taking" @click="take">
          <Camera :size="18" /> {{ $t('Tomar foto') }}
        </button>
      </div>
    </section>
  </div>
</template>
