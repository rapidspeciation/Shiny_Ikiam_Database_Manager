<script setup lang="ts">
import { computed, onActivated, onBeforeUnmount, onDeactivated, ref, watch } from 'vue'
import { Check, CloudUpload, Copy, ExternalLink, PanelRightClose, PanelRightOpen, Sparkles } from 'lucide-vue-next'
import { api } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import { t } from '../lib/i18n'
import {
  RENAME_STEPS,
  batchCounts,
  batchLine,
  folderName,
  renameHost,
  spaceLine,
  stepLabel,
  stepOf,
  type DriveSpace,
  type UploadPlace,
} from '../lib/renameHost'
import { ensureT3, t3Host } from '../lib/t3Host'

defineOptions({ name: 'RenameView' })

/**
 * «Renombrar fotos»: the wing photos of a folder on this computer named after the ID on each
 * one's envelope, before they are uploaded to Drive. The work is done by the Rename Photos app
 * in the frame (RenameHost over `slot`): it reads the IDs here, in the browser, and renames the
 * files in place. «Revisar con Claude» opens a chat of T3 Code beside it that starts with the
 * request to double-check the batch: the assistant looks at the envelopes it asks for, corrects
 * the rows that are clear and leaves the rest marked (server/rename-photos.mjs). Renaming stays
 * this page's own button.
 * The bar above the frame shows the way from the camera's files to the sheets, a step at a time,
 * with what to do now; once the photos are ready to upload it says where they go on Drive (the
 * folders of the nearest IDs' photos), and «Ya las subí» starts the chat that lists them in
 * Photo_links and updates the public sites.
 */
const slot = ref<HTMLElement>()
const chatSlot = ref<HTMLElement>()
const split = ref<HTMLElement>()
const chat = ref(false)
/** The chat's share of the width, in percent. */
const share = ref(Number(localStorage.getItem('rename:chat-share')) || 38)
const dragging = ref(false)
const asking = ref(false)
/** The request to paste by hand, when the chat could not be started with it. */
const prompt = ref('')
const copied = ref(false)
let onScreen = false

const counts = computed(() => batchCounts(renameHost.state))
/** The project account's space on Drive, where the renamed photos are uploaded; asked when the tab comes on screen. */
const drive = ref<DriveSpace | null>(null)
const space = computed(() => spaceLine(drive.value, renameHost.state?.folderBytes))
let spaceAt = 0
async function askSpace() {
  if (Date.now() - spaceAt < 5 * 60_000) return
  spaceAt = Date.now()
  try {
    drive.value = (await api<{ space: DriveSpace | null }>('chat/drive-space')).space
  } catch {
    // The bar goes without it.
  }
}
const line = computed(() => batchLine(renameHost.state))
const canReview = computed(() => counts.value.photos > 0 && !renameHost.state?.reading.on)
/** The person said the renamed files are on Drive (for this folder). */
const uploaded = ref('')
const step = computed(() => stepOf(renameHost.state, !!renameHost.state?.folder && uploaded.value === renameHost.state.folder))
const stepAt = computed(() => RENAME_STEPS.indexOf(step.value))
const hint = computed(() => {
  switch (step.value) {
    case 'open':
      return t('Pulsa «Open folder» y elige la carpeta con las fotos de la cámara.')
    case 'read':
      return renameHost.state?.reading.on ? t('Leyendo los sobres en este ordenador…') : t('Pulsa «Run PaddleOCR»: lee el ID del sobre de cada foto, en este ordenador.')
    case 'review':
      return t('Corrige o confirma las fotos marcadas, o deja que Claude las revise.')
    case 'rename':
      return t('Todo confirmado: pulsa «Rename» abajo.')
    case 'upload':
      return t('Renombradas. Súbelas a Drive con Drive para escritorio:')
    default:
      return t('El asistente actualiza Photo_links y los sitios en el chat.')
  }
})
/** Where the files go on Drive, asked once the batch has nothing waiting. */
const upload = ref<UploadPlace | null>(null)
let uploadFor = ''
async function askUpload() {
  const key = `${renameHost.state?.folder}|${counts.value.photos}`
  if (!renameHost.tab || uploadFor === key) return
  uploadFor = key
  try {
    upload.value = (await api<{ upload: UploadPlace | null }>(`rename/upload?tab=${renameHost.tab}`)).upload
  } catch {
    uploadFor = ''
  }
}
watch(step, now => (now === 'rename' || now === 'upload') && void askUpload(), { immediate: true })

function place() {
  renameHost.slot = onScreen ? (slot.value ?? null) : null
  if (onScreen) t3Host.slot = chat.value ? (chatSlot.value ?? null) : null
}
onActivated(() => {
  onScreen = true
  renameHost.started = true
  place()
  void askSpace()
})
onDeactivated(() => {
  onScreen = false
  renameHost.slot = null
  t3Host.slot = null
})
onBeforeUnmount(() => {
  // Signed out: the next session starts without the frame until it opens this tab.
  Object.assign(renameHost, { started: false, slot: null, ready: false, state: null })
})
watch([slot, chatSlot, chat], place, { flush: 'post' })
watch(dragging, now => {
  renameHost.passThrough = now
  t3Host.passThrough = now
})

async function showChat() {
  try {
    await ensureT3()
  } catch (e) {
    notify(errorText(e), 'error')
  }
  chat.value = true
}
async function review(done = false) {
  asking.value = true
  prompt.value = ''
  try {
    await ensureT3()
    const out = await api<{ chat: string | null; prompt: string }>('rename/review', { method: 'POST', body: done ? { step: 'uploaded' } : {} })
    if (done) uploaded.value = renameHost.state?.folder ?? ''
    chat.value = true
    if (out.chat) t3Host.open = { thread: out.chat }
    else prompt.value = out.prompt
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    asking.value = false
  }
}
async function copyPrompt() {
  try {
    await navigator.clipboard.writeText(prompt.value)
    copied.value = true
    setTimeout(() => (copied.value = false), 2500)
  } catch {
    notify(t('No se pudo copiar: selecciona el texto y cópialo.'), 'error')
  }
}

function resize(event: PointerEvent) {
  const target = event.currentTarget as HTMLElement
  target.setPointerCapture(event.pointerId)
  dragging.value = true
  const move = (e: PointerEvent) => {
    const r = split.value?.getBoundingClientRect()
    if (r?.width) share.value = Math.min(70, Math.max(22, Math.round(((r.right - e.clientX) / r.width) * 100)))
  }
  const end = () => {
    dragging.value = false
    localStorage.setItem('rename:chat-share', String(share.value))
    target.removeEventListener('pointermove', move)
    target.removeEventListener('pointerup', end)
    target.removeEventListener('pointercancel', end)
  }
  target.addEventListener('pointermove', move)
  target.addEventListener('pointerup', end)
  target.addEventListener('pointercancel', end)
}
</script>

<template>
  <div class="flex h-full flex-col">
    <div class="border-b border-stone-200 bg-white text-sm">
      <div class="flex flex-wrap items-center gap-x-3 gap-y-1 px-3 pt-1.5">
        <!-- The way from the camera's files to the sheets: done, now, to come. -->
        <ol class="flex min-w-0 flex-wrap items-center gap-1" :aria-label="$t('Pasos')" data-steps>
          <li
            v-for="(name, i) in RENAME_STEPS"
            :key="name"
            class="flex items-center gap-1 rounded-full px-2 py-0.5 text-xs"
            :class="i === stepAt ? 'bg-emerald-700 font-medium text-white' : i < stepAt ? 'text-emerald-800' : 'text-stone-400'"
            :aria-current="i === stepAt ? 'step' : undefined"
          >
            <Check v-if="i < stepAt" :size="12" />
            <span v-else class="tabular-nums">{{ i + 1 }}</span>
            {{ stepLabel(name) }}
          </li>
        </ol>
        <span class="ml-auto flex items-center gap-2">
          <!-- Where the photos go next: the project's Drive, and whether this folder fits. -->
          <span
            v-if="space"
            class="rounded px-2 py-0.5 text-xs"
            :class="
              space.level === 'full' ? 'bg-red-100 text-red-900' : space.level === 'low' ? 'bg-amber-100 text-amber-950' : 'text-stone-500'
            "
            :title="
              space.level === 'ok'
                ? $t('Donde se suben las fotos renombradas')
                : $t('Queda poco espacio: el asistente puede preparar el traspaso de carpetas a la cuenta que guarda las fotos')
            "
            data-drive-space
          >
            {{ space.text }}
            <template v-if="space.level === 'full'"> · {{ $t('esta carpeta ocupa {gb} GB: no cabe', { gb: space.folderGb ?? 0 }) }}</template>
            <template v-else-if="space.folderGb"> · {{ $t('esta carpeta: {gb} GB', { gb: space.folderGb }) }}</template>
          </span>
          <button
            v-if="step === 'upload'"
            class="btn-primary bg-emerald-700 px-3 py-1 hover:bg-emerald-800"
            :disabled="asking"
            :title="$t('Abre un chat que lista las fotos nuevas en Photo_links y después actualiza la Wings Gallery y los mapas')"
            data-uploaded
            @click="review(true)"
          >
            <CloudUpload :size="15" /> {{ asking ? $t('Abriendo el chat…') : $t('Ya las subí: actualizar') }}
          </button>
          <button
            :class="step === 'review' ? 'btn-primary bg-emerald-700 px-3 py-1 hover:bg-emerald-800' : 'btn px-3 py-1'"
            :disabled="!canReview || asking"
            :title="
              canReview
                ? $t('El asistente mira los sobres de las fotos marcadas y de una muestra, corrige lo que esté claro y te deja señalado lo demás')
                : $t('Primero lee las fotos de una carpeta')
            "
            data-review
            @click="review()"
          >
            <Sparkles :size="15" /> {{ asking && step !== 'upload' ? $t('Abriendo el chat…') : $t('Revisar con Claude') }}
          </button>
          <button
            class="btn-ghost"
            :title="chat ? $t('Ocultar el chat') : $t('Mostrar el chat del asistente')"
            :aria-pressed="chat"
            data-chat-toggle
            @click="chat ? (chat = false) : showChat()"
          >
            <PanelRightClose v-if="chat" :size="16" /><PanelRightOpen v-else :size="16" />
          </button>
        </span>
      </div>
      <!-- What to do now, the batch in a line, and where its files go once they are ready. -->
      <p class="flex flex-wrap items-baseline gap-x-2 gap-y-0.5 px-3 pt-0.5 pb-1.5 text-xs text-stone-600" data-now>
        <span class="font-medium text-stone-800" data-hint>{{ hint }}</span>
        <template v-if="(step === 'upload' || step === 'rename') && upload">
          <template v-if="upload.photos">
            <span v-if="step === 'rename'">{{ $t('Después se suben a:') }}</span>
            <span data-upload>
              {{ $t('fotos') }} →
              <a v-if="upload.photos.link" class="text-emerald-800 underline" :href="upload.photos.link" target="_blank" rel="noopener" :title="upload.photos.inDriveForDesktop"
                >{{ folderName(upload.photos.folder) }} <ExternalLink :size="11" class="inline"
              /></a>
              <span v-else :title="upload.photos.inDriveForDesktop">{{ folderName(upload.photos.folder) }}</span>
              <span class="text-stone-400"> ({{ upload.photos.inDriveForDesktop }})</span>
            </span>
            <span v-if="upload.rawFiles">
              · RAW →
              <a v-if="upload.rawFiles.link" class="text-emerald-800 underline" :href="upload.rawFiles.link" target="_blank" rel="noopener" :title="upload.rawFiles.inDriveForDesktop"
                >{{ folderName(upload.rawFiles.folder) }} <ExternalLink :size="11" class="inline"
              /></a>
              <span v-else :title="upload.rawFiles.inDriveForDesktop">{{ folderName(upload.rawFiles.folder) }}</span>
            </span>
          </template>
          <span v-else-if="upload.noFolderYet" data-upload>{{ $t('Es una colecta nueva: pide al asistente la carpeta donde subirlas.') }}</span>
        </template>
        <span v-if="line" class="ml-auto min-w-0 truncate text-stone-500" data-batch>{{ line }}</span>
      </p>
    </div>
    <p v-if="renameHost.missing" class="p-4 text-sm text-stone-600" data-missing>
      {{ $t('La app para renombrar fotos no está instalada en este servidor.') }}
      <a class="text-emerald-800 underline" href="https://fr4nzz.github.io/rename_photos_AI/" target="_blank" rel="noopener">{{
        $t('Ábrela en su propia página')
      }}</a>
    </p>
    <div v-else ref="split" class="flex min-h-0 flex-1" :class="{ 'select-none': dragging }" :style="{ '--share': `${share}%` }">
      <!-- The Rename Photos app's place: its frame (RenameHost) sits over it. -->
      <div ref="slot" class="min-h-0 min-w-0 flex-1" />
      <template v-if="chat">
        <div
          role="separator"
          aria-orientation="vertical"
          :aria-label="$t('Tamaño del chat')"
          :title="$t('Arrastra para cambiar el tamaño')"
          class="w-1.5 shrink-0 cursor-col-resize touch-none bg-stone-200 hover:bg-emerald-400"
          :class="{ 'bg-emerald-500': dragging }"
          @pointerdown="resize"
        />
        <div class="flex min-h-0 shrink-0 grow-0 basis-(--share) flex-col border-l border-stone-300">
          <div v-if="prompt" class="space-y-1.5 border-b border-amber-200 bg-amber-50 px-3 py-2 text-xs text-amber-950" data-prompt>
            <p>{{ $t('No se pudo abrir el chat con el pedido. Pégalo en un chat nuevo:') }}</p>
            <p class="rounded border border-amber-200 bg-white px-2 py-1 select-all">{{ prompt }}</p>
            <button class="btn px-2 py-0.5" @click="copyPrompt">
              <Check v-if="copied" :size="13" /><Copy v-else :size="13" /> {{ copied ? $t('Copiado') : $t('Copiar') }}
            </button>
          </div>
          <!-- T3's place: its frame (T3Host) sits over it. -->
          <div v-if="t3Host.url" ref="chatSlot" class="min-h-0 flex-1" />
          <p v-else class="p-4 text-sm text-stone-600">{{ $t('T3 Code no está configurado en este servidor.') }}</p>
        </div>
      </template>
    </div>
  </div>
</template>
