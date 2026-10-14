<script setup lang="ts">
import { computed, onActivated, onBeforeUnmount, onDeactivated, ref, watch } from 'vue'
import { Check, Copy, PanelRightClose, PanelRightOpen, Sparkles } from 'lucide-vue-next'
import { api } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import { t } from '../lib/i18n'
import { batchCounts, batchLine, renameHost } from '../lib/renameHost'
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
const line = computed(() => batchLine(renameHost.state))
const canReview = computed(() => counts.value.photos > 0 && !renameHost.state?.reading.on)

function place() {
  renameHost.slot = onScreen ? (slot.value ?? null) : null
  if (onScreen) t3Host.slot = chat.value ? (chatSlot.value ?? null) : null
}
onActivated(() => {
  onScreen = true
  renameHost.started = true
  place()
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
async function review() {
  asking.value = true
  prompt.value = ''
  try {
    await ensureT3()
    const out = await api<{ chat: string | null; prompt: string }>('rename/review', { method: 'POST', body: {} })
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
    <div class="flex flex-wrap items-center gap-x-3 gap-y-1 border-b border-stone-200 bg-white px-3 py-1.5 text-sm">
      <span v-if="line" class="min-w-0 truncate text-stone-700" data-batch>{{ line }}</span>
      <span v-else class="text-stone-500">{{ $t('Abre la carpeta con las fotos y pulsa «Run PaddleOCR»: los IDs se leen en este ordenador.') }}</span>
      <span class="ml-auto flex items-center gap-2">
        <button
          class="btn-primary bg-emerald-700 px-3 py-1 hover:bg-emerald-800"
          :disabled="!canReview || asking"
          :title="
            canReview
              ? $t('El asistente mira los sobres de las fotos marcadas y de una muestra, corrige lo que esté claro y te deja señalado lo demás')
              : $t('Primero lee las fotos de una carpeta')
          "
          data-review
          @click="review"
        >
          <Sparkles :size="15" /> {{ asking ? $t('Abriendo el chat…') : $t('Revisar con Claude') }}
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
