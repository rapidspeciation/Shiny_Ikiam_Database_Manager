<script setup lang="ts">
import { computed, defineAsyncComponent, onActivated, onBeforeUnmount, onDeactivated, onMounted, onUnmounted, ref, watch } from 'vue'
import { Check, Cloud, CloudUpload, Copy, ExternalLink, MessageSquarePlus, PanelRightClose, PanelRightOpen, Sparkles } from 'lucide-vue-next'
const PhotoLinksPanel = defineAsyncComponent(() => import('../components/PhotoLinksPanel.vue'))
import { api } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import { intlLocale, t } from '../lib/i18n'
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
 * «Fotos», in two parts. «Renombrar»: the wing photos of a folder on this computer named after the
 * ID on each one's envelope, before they are uploaded to Drive. The work is done by the Rename Photos app
 * in the frame (RenameHost over `slot`): it reads the IDs here, in the browser, and renames the
 * files in place. «Revisar con Claude» opens a chat of T3 Code beside it that starts with the
 * request to double-check the batch: the assistant looks at the envelopes it asks for, corrects
 * the rows that are clear and leaves the rest marked (server/rename-photos.mjs). Renaming stays
 * this page's own button.
 * The bar above the frame shows the way from the camera's files to the sheets, a step at a time,
 * with what to do now; once the photos are ready to upload it says where they go on Drive (the
 * folders of the nearest IDs' photos), and «Ya las subí» reads those folders in the other part.
 * «Photo_links y sitios» (PhotoLinksPanel): Photo_links brought up to date with Drive and the public
 * sites updated, with the app's own buttons, as the sheet's menu did: no assistant needed.
 */
/** The part shown, kept for the next visit. */
const section = ref<'rename' | 'links'>(localStorage.getItem('fotos:section') === 'links' ? 'links' : 'rename')
watch(section, now => localStorage.setItem('fotos:section', now))
/** The folders «Ya las subí» has the Photo_links part read. */
const preset = ref<{ folders: string[]; at: number } | null>(null)
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
      return t('En «Photo_links y sitios»: aplica lo que se leyó de Drive y luego actualiza la Wings Gallery.')
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
  renameHost.slot = onScreen && section.value === 'rename' ? (slot.value ?? null) : null
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
watch([slot, chatSlot, chat, section], place, { flush: 'post' })
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
/** «Ya las subí»: the Photo_links part reads the folders the files went to. */
function uploadedNow() {
  uploaded.value = renameHost.state?.folder ?? ''
  const folders = [upload.value?.photos?.folder, upload.value?.rawFiles?.folder].filter((f): f is string => !!f)
  section.value = 'links'
  preset.value = folders.length ? { folders, at: Date.now() } : null
}
/** The chat that already reviewed the folder open (kept by the server for this folder): «Revisar» goes back to it. */
const earlier = ref<{ chat: string; at: string } | null>(null)
let earlierFor = ''
async function askEarlier() {
  const key = `${renameHost.state?.folder ?? ''}|${counts.value.photos}`
  if (key === earlierFor) return
  earlierFor = key
  if (!renameHost.state?.folder || !counts.value.photos) return void (earlier.value = null)
  try {
    const out = await api<{ chat: string | null; at: string | null }>('rename/review')
    earlier.value = out.chat ? { chat: out.chat, at: out.at ?? '' } : null
  } catch {
    earlierFor = ''
  }
}
watch(() => [renameHost.state?.folder, counts.value.photos], () => void askEarlier(), { immediate: true })
const earlierDate = computed(() => (earlier.value?.at ? new Date(earlier.value.at).toLocaleDateString(intlLocale(), { day: '2-digit', month: '2-digit', year: 'numeric' }) : ''))

async function review(fresh = false) {
  asking.value = true
  prompt.value = ''
  // The frame's app shows its review: the cards Claude's changes go to.
  renameHost.show = { view: 'review', at: Date.now() }
  try {
    await ensureT3()
    const out = await api<{ chat: string | null; prompt: string; resumed?: boolean }>('rename/review', { method: 'POST', body: fresh ? { fresh: true } : {} })
    if (out.chat) earlier.value = { chat: out.chat, at: out.resumed ? (earlier.value?.at ?? '') : new Date().toISOString() }
    chat.value = true
    if (out.chat) t3Host.open = { thread: out.chat, made: !out.resumed }
    else prompt.value = out.prompt
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    asking.value = false
  }
}
// A card asked Claude (askClaude): its chat shows beside, or its request to paste when T3 could not start it.
function claudeOpened(event: Event) {
  const out = (event as CustomEvent<{ chat: string | null; prompt: string }>).detail
  prompt.value = out?.chat ? '' : (out?.prompt ?? '')
  chat.value = true
}
onMounted(() => window.addEventListener('claude:open', claudeOpened))
onUnmounted(() => window.removeEventListener('claude:open', claudeOpened))
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
      <div class="flex flex-wrap items-center gap-x-3 gap-y-1 px-3 pt-1.5" :class="{ 'pb-1.5': section === 'links' }">
        <!-- The tab's two parts. -->
        <div class="flex shrink-0 rounded-md border border-stone-300 p-0.5 text-xs" role="tablist" data-sections>
          <button
            v-for="[key, label] in [['rename', $t('Renombrar')], ['links', $t('Photo_links y sitios')]] as const"
            :key="key"
            role="tab"
            class="rounded px-2 py-0.5"
            :class="section === key ? 'bg-emerald-700 font-medium text-white' : 'text-stone-600 hover:text-stone-900'"
            :aria-selected="section === key"
            :data-section="key"
            @click="section = key"
          >
            {{ label }}
          </button>
        </div>
        <!-- The way from the camera's files to the sheets: done, now, to come. -->
        <ol v-if="section === 'rename'" class="flex min-w-0 flex-wrap items-center gap-1" :aria-label="$t('Pasos')" data-steps>
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
          <!-- Where the photos go next: the project's Drive as Google shows it, and whether this folder fits. -->
          <div
            v-if="space"
            class="flex items-center gap-2 text-xs"
            :title="
              space.level === 'ok'
                ? $t('Donde se suben las fotos renombradas')
                : $t('Queda poco espacio: el asistente puede preparar el traspaso de carpetas a la cuenta que guarda las fotos')
            "
            data-drive-space
          >
            <Cloud :size="15" :class="space.level === 'full' ? 'text-red-600' : space.level === 'low' ? 'text-amber-600' : 'text-stone-500'" />
            <div class="w-36">
              <div class="h-1.5 overflow-hidden rounded-full bg-stone-200" role="progressbar" aria-valuemin="0" aria-valuemax="100" :aria-valuenow="space.percent">
                <div
                  class="h-full rounded-full"
                  :class="space.level === 'full' || space.percent >= 90 ? 'bg-red-500' : space.level === 'low' ? 'bg-amber-500' : 'bg-blue-600'"
                  :style="{ width: `${Math.min(100, space.percent)}%` }"
                />
              </div>
              <p class="mt-0.5 text-[11px] leading-tight text-stone-700">{{ space.text }}</p>
            </div>
            <span v-if="space.level === 'full'" class="rounded bg-red-100 px-1.5 py-0.5 text-red-900">{{ $t('esta carpeta ocupa {gb} GB: no cabe', { gb: space.folderGb ?? 0 }) }}</span>
            <span v-else-if="space.folderGb" class="text-stone-500">{{ $t('esta carpeta: {gb} GB', { gb: space.folderGb }) }}</span>
          </div>
          <button
            v-if="section === 'rename' && step === 'upload'"
            class="btn-primary bg-emerald-700 px-3 py-1 hover:bg-emerald-800"
            :title="$t('Lee en Drive las carpetas donde se subieron, para listarlas en Photo_links; después se actualizan los sitios')"
            data-uploaded
            @click="uploadedNow"
          >
            <CloudUpload :size="15" /> {{ $t('Ya las subí: actualizar') }}
          </button>
          <button
            v-if="section === 'rename'"
            :class="step === 'review' ? 'btn-primary bg-emerald-700 px-3 py-1 hover:bg-emerald-800' : 'btn px-3 py-1'"
            :disabled="!canReview || asking"
            :title="
              !canReview
                ? $t('Primero lee las fotos de una carpeta')
                : earlier
                  ? $t('Abre el chat que ya revisó esta carpeta (del {date})', { date: earlierDate })
                  : $t('El asistente mira los sobres de las fotos marcadas y de una muestra, corrige lo que esté claro y te deja señalado lo demás')
            "
            data-review
            @click="review()"
          >
            <Sparkles :size="15" /> {{ asking ? $t('Abriendo el chat…') : earlier ? $t('Continuar la revisión') : $t('Revisar con Claude') }}
          </button>
          <!-- A chat of its own already reviewed this folder: another, from the start. -->
          <button
            v-if="section === 'rename' && earlier"
            class="btn-ghost"
            :disabled="asking"
            :title="$t('Empezar un chat nuevo para revisar esta carpeta (el anterior, del {date}, queda en T3)', { date: earlierDate })"
            data-review-fresh
            @click="review(true)"
          >
            <MessageSquarePlus :size="16" />
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
      <p v-if="section === 'rename'" class="flex flex-wrap items-baseline gap-x-2 gap-y-0.5 px-3 pt-0.5 pb-1.5 text-xs text-stone-600" data-now>
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
    <p v-if="section === 'rename' && renameHost.missing" class="p-4 text-sm text-stone-600" data-missing>
      {{ $t('La app para renombrar fotos no está instalada en este servidor.') }}
      <a class="text-emerald-800 underline" href="https://fr4nzz.github.io/rename_photos_AI/" target="_blank" rel="noopener">{{
        $t('Ábrela en su propia página')
      }}</a>
    </p>
    <div v-else ref="split" class="flex min-h-0 flex-1" :class="{ 'select-none': dragging }" :style="{ '--share': `${share}%` }">
      <PhotoLinksPanel v-if="section === 'links'" :preset="preset" class="min-h-0 min-w-0 flex-1" @used="preset = null" />
      <!-- The Rename Photos app's place: its frame (RenameHost) sits over it. -->
      <div v-else ref="slot" class="min-h-0 min-w-0 flex-1" />
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
