<script setup lang="ts">
import { onBeforeUnmount, onMounted, ref } from 'vue'
import { Check, Loader2, X } from 'lucide-vue-next'
import { t } from '../../lib/i18n'

/**
 * Reads tube barcodes with the phone's camera (Chrome's BarcodeDetector: the
 * 2D code under a FluidX tube or the bars on its side), one after another:
 * each new code read is sent up (`read`) and the parent says where it went
 * (`status`, e.g. "FS90415474 → D2E"). A code stays read until another one is
 * in view, so holding a tube still does not repeat it.
 */
const props = defineProps<{ status: string; statusKind: 'ok' | 'error' | 'idle'; next: string }>()
const emit = defineEmits<{ read: [value: string]; close: [] }>()

const video = ref<HTMLVideoElement>()
const starting = ref(true)
const failure = ref('')
let stream: MediaStream | null = null
let timer: ReturnType<typeof setTimeout> | undefined
let last = ''
let lastAt = 0
let stopped = false

interface Detected {
  rawValue: string
}
interface Detector {
  detect(source: HTMLVideoElement): Promise<Detected[]>
}
type DetectorClass = { new (options: { formats: string[] }): Detector; getSupportedFormats(): Promise<string[]> }

onMounted(async () => {
  try {
    const Detector = (window as unknown as { BarcodeDetector: DetectorClass }).BarcodeDetector
    const supported = await Detector.getSupportedFormats()
    const formats = ['data_matrix', 'code_128', 'qr_code', 'code_39'].filter(f => supported.includes(f))
    const detector = new Detector({ formats: formats.length ? formats : supported })
    stream = await navigator.mediaDevices.getUserMedia({ video: { facingMode: 'environment' }, audio: false })
    if (stopped) return stop()
    video.value!.srcObject = stream
    await video.value!.play()
    starting.value = false
    const tick = async () => {
      if (stopped || !video.value) return
      try {
        const codes = await detector.detect(video.value)
        const value = codes.map(c => c.rawValue.trim()).find(Boolean)
        const now = Date.now()
        if (value && (value !== last || now - lastAt > 4000)) {
          if (value !== last) {
            navigator.vibrate?.(60)
            emit('read', value)
          }
          last = value
        }
        if (value) lastAt = now
      } catch {
        /* A frame that cannot be read: try the next one. */
      }
      timer = setTimeout(tick, 150)
    }
    tick()
  } catch (e) {
    starting.value = false
    failure.value =
      e instanceof DOMException && e.name === 'NotAllowedError'
        ? t('Sin permiso para usar la cámara: actívalo en el navegador, o escribe los tubos.')
        : t('No se pudo abrir la cámara: escribe los tubos o usa un lector.')
  }
})
function stop() {
  stopped = true
  clearTimeout(timer)
  stream?.getTracks().forEach(track => track.stop())
  stream = null
}
onBeforeUnmount(stop)
</script>

<template>
  <div class="fixed inset-0 z-50 flex flex-col bg-black text-white" role="dialog" :aria-label="$t('Escanear tubos')">
    <div class="flex items-center gap-2 px-3 pt-[calc(0.5rem+env(safe-area-inset-top))] pb-2">
      <p class="min-w-0 flex-1 text-base font-semibold">{{ $t('Escanear tubos') }}</p>
      <button class="grid h-12 w-12 place-items-center rounded-lg active:bg-white/10" :aria-label="$t('Cerrar')" @click="emit('close')">
        <X :size="24" />
      </button>
    </div>
    <div class="relative min-h-0 flex-1">
      <video ref="video" class="h-full w-full object-cover" playsinline muted />
      <!-- Where to hold the tube's code. -->
      <div class="pointer-events-none absolute inset-0 grid place-items-center">
        <div class="h-40 w-64 max-w-[80%] rounded-2xl border-4 border-white/80 shadow-[0_0_0_9999px_rgba(0,0,0,0.35)]" />
      </div>
      <p v-if="starting" class="absolute inset-x-0 top-1/2 flex items-center justify-center gap-2 text-sm">
        <Loader2 :size="18" class="animate-spin" /> {{ $t('Abriendo la cámara…') }}
      </p>
      <p v-if="failure" class="absolute inset-x-4 top-1/3 rounded-xl bg-white p-4 text-base text-red-800">{{ failure }}</p>
    </div>
    <div class="space-y-2 px-3 pt-3 pb-[calc(0.75rem+env(safe-area-inset-bottom))]">
      <p
        class="min-h-12 rounded-xl px-3 py-2.5 text-base font-semibold"
        :class="props.statusKind === 'ok' ? 'bg-brand-700' : props.statusKind === 'error' ? 'bg-red-700' : 'bg-white/10'"
        role="status"
      >
        {{ props.status || $t('Apunta al código del tubo (abajo o en el lado)') }}
      </p>
      <p v-if="props.next" class="text-sm text-white/80">{{ props.next }}</p>
      <button class="flex h-13 w-full items-center justify-center gap-2 rounded-xl bg-white text-base font-semibold text-stone-900" @click="emit('close')">
        <Check :size="20" /> {{ $t('Listo') }}
      </button>
    </div>
  </div>
</template>
