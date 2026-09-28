<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, reactive, ref, watch } from 'vue'
import { Mic, MicOff, Minimize2, Phone, PhoneOff, Send } from 'lucide-vue-next'
import { api, requestId, type ApiError } from '../../lib/api'
import { errorText, notify } from '../../lib/notice'
import {
  startCall,
  type CallControls,
  type CallServer,
  type CallView,
  type ToolAnswer,
  type VoiceSession,
} from '../../lib/liveCall'
import { useTables } from '../../stores/tables'
import ProposalGrid, { type Proposal } from '../ProposalGrid.vue'

/**
 * "Llamada": talk with the assistant with busy hands. It looks rows up, drafts
 * the changes you dictate and shows them here as they come; they are written
 * only when you say "sí, guárdalo" or press ✓. Hiding the panel, the tab or the
 * screen keeps the call going; only Colgar ends it.
 */
const tables = useTables()
const view = reactive<CallView>({
  state: 'ended',
  detail: '',
  muted: false,
  level: 0,
  lines: [],
  proposals: [],
  threadId: null,
  title: '',
  startedAt: 0,
  error: '',
})
const open = ref(false)
const configured = ref<boolean | null>(null)
const typed = ref('')
const applying = ref<string | null>(null)
const scroller = ref<HTMLElement>()
const now = ref(Date.now())
let controls: CallControls | null = null
const active = computed(() => view.state !== 'ended')

const tick = setInterval(() => {
  if (active.value) now.value = Date.now()
}, 1000)
onMounted(async () => {
  try {
    configured.value = (await api<{ voice?: { configured: boolean } }>('ai/status')).voice?.configured ?? false
  } catch {
    configured.value = null
  }
})
// Leaving the Asistente tab keeps the call (the tab stays alive); closing the app ends it.
onBeforeUnmount(() => {
  clearInterval(tick)
  controls?.hangUp()
  window.removeEventListener('beforeunload', warn)
})
function warn(event: BeforeUnloadEvent) {
  if (active.value) event.preventDefault()
}
window.addEventListener('beforeunload', warn)

function voiceError(e: ApiError) {
  if (e.code === 'voice_unconfigured') return 'La llamada no está configurada en el servidor (falta la clave de Gemini).'
  if (e.code === 'voice_rate_limited') return 'Demasiadas reconexiones seguidas. Espera unos minutos.'
  if (e.status === 401) return 'La sesión expiró. Vuelve a iniciar sesión.'
  if (e.code === 'provider_error') return 'Gemini no aceptó la llamada; intenta de nuevo en un momento.'
  return errorText(e)
}
const server: CallServer = {
  async session(threadId) {
    try {
      return await api<VoiceSession>('ai/voice/session', { method: 'POST', body: { threadId } })
    } catch (e) {
      const err = e as ApiError
      throw Object.assign(new Error(voiceError(err)), { fatal: [401, 403, 404, 429, 501].includes(err.status) })
    }
  },
  tool: body => api<ToolAnswer>('ai/voice/tool', { method: 'POST', body }),
  transcript: body => api('ai/voice/transcript', { method: 'POST', body }),
  applied: () => void reloadTables(),
}

/** Runs inside the click: the browser only lets a gesture start audio. */
function call() {
  open.value = true
  if (active.value || configured.value === false) return
  view.lines.splice(0)
  Object.assign(view, { proposals: [], threadId: null, title: '' })
  now.value = Date.now()
  controls = startCall(view, server)
}
function hangUp() {
  controls?.hangUp()
  controls = null
}
function toggleMute() {
  controls?.setMuted(!view.muted)
}
function send() {
  const text = typed.value.trim()
  if (!text || !active.value) return
  controls?.sendText(text)
  typed.value = ''
}

async function reloadTables() {
  await Promise.all(Object.keys(tables.tables).map(sheet => tables.load(sheet, true)))
}
async function apply(proposal: Proposal, indexes: number[]) {
  applying.value = proposal.id
  try {
    const out = await api<{ status: Proposal['status']; applied: number[] }>(`chat/proposals/${proposal.id}/apply`, {
      method: 'POST',
      body: { requestId: requestId(), indexes },
    })
    proposal.status = out.status
    proposal.applied = out.applied
    await reloadTables()
    notify(`${out.applied.length} ${out.applied.length === 1 ? 'fila aplicada' : 'filas aplicadas'} en Google Sheets`, 'success')
  } catch (e) {
    proposal.status = 'needs_review'
    notify(errorText(e), 'error')
  } finally {
    applying.value = null
  }
}
async function discard(proposal: Proposal) {
  try {
    await api(`chat/proposals/${proposal.id}/discard`, { method: 'POST', body: {} })
    proposal.status = 'discarded'
  } catch (e) {
    notify(errorText(e), 'error')
  }
}

// The newest proposal on top, where the eye is while dictating.
const newestFirst = computed(() => [...view.proposals].reverse())
const waiting = computed(() => view.proposals.filter(p => p.status === 'pending').length)
const clock = computed(() => {
  const seconds = Math.max(0, Math.floor((now.value - view.startedAt) / 1000))
  return `${Math.floor(seconds / 60)}:${String(seconds % 60).padStart(2, '0')}`
})
const stateText = computed(
  () =>
    ({
      connecting: 'Conectando…',
      listening: view.muted ? 'Micrófono silenciado' : 'Te escucho',
      speaking: 'El asistente habla',
      working: 'Trabajando…',
      reconnecting: 'Reconectando…',
      ended: 'Sin llamada',
    })[view.state],
)
const stateColor = computed(() =>
  view.state === 'ended'
    ? 'bg-stone-200 text-stone-500'
    : view.state === 'reconnecting' || view.state === 'connecting'
      ? 'bg-amber-100 text-amber-800'
      : view.muted
        ? 'bg-stone-200 text-stone-600'
        : 'bg-emerald-100 text-emerald-800',
)

// Follow the conversation unless the person scrolled up to read.
watch(
  () => [view.lines.length, view.lines[view.lines.length - 1]?.text],
  () => {
    const box = scroller.value
    if (!box || box.scrollHeight - box.scrollTop - box.clientHeight > 80) return
    nextTick(() => box.scrollTo({ top: box.scrollHeight }))
  },
)
</script>

<template>
  <button
    v-if="!active"
    class="flex items-center gap-1 rounded px-2 py-0.5 text-stone-700 hover:bg-stone-200"
    title="Hablar con el asistente: dicta y mira los cambios en vivo"
    @click="call"
  >
    <Phone :size="14" /> Llamada
  </button>
  <button
    v-else
    class="flex items-center gap-1 rounded bg-emerald-600 px-2 py-0.5 text-white"
    title="Volver a la llamada"
    @click="open = true"
  >
    <Phone :size="14" class="animate-pulse" /> En llamada {{ clock }}
    <span v-if="waiting" class="rounded bg-white/25 px-1">{{ waiting }}</span>
  </button>

  <div v-if="open" class="fixed inset-0 z-40 flex justify-end bg-stone-900/20" @click.self="open = false">
    <aside class="flex h-full w-full max-w-5xl flex-col bg-stone-50 shadow-xl">
      <header class="flex items-center gap-2 border-b border-stone-200 bg-white px-3 py-2">
        <h2 class="text-sm font-medium">Llamada con el asistente</h2>
        <span class="hint truncate">{{ view.title }}</span>
        <button class="btn-ghost ml-auto" title="Ocultar (la llamada sigue)" @click="open = false">
          <Minimize2 :size="16" />
        </button>
      </header>

      <div class="flex items-center gap-3 border-b border-stone-200 bg-white px-3 py-3">
        <div class="grid size-12 shrink-0 place-items-center rounded-full" :class="stateColor">
          <MicOff v-if="view.muted" :size="22" />
          <Mic v-else :size="22" :style="{ transform: `scale(${1 + view.level * 0.5})` }" />
        </div>
        <div class="min-w-0 flex-1">
          <p class="font-medium">
            {{ stateText }} <span v-if="active" class="hint">· {{ clock }}</span>
          </p>
          <p class="hint truncate">
            {{
              view.detail ||
              (view.state === 'listening' || view.state === 'speaking'
                ? 'Habla con normalidad; puedes interrumpir al asistente.'
                : '')
            }}
          </p>
          <div class="mt-1 h-1.5 overflow-hidden rounded bg-stone-200" aria-hidden="true">
            <div class="h-full rounded bg-emerald-500" :style="{ width: `${Math.round(view.level * 100)}%` }" />
          </div>
        </div>
        <template v-if="active">
          <button
            class="btn size-12 shrink-0 rounded-full"
            :class="{ 'bg-amber-100': view.muted }"
            :title="view.muted ? 'Activar el micrófono' : 'Silenciar el micrófono'"
            :aria-pressed="view.muted"
            @click="toggleMute"
          >
            <MicOff v-if="view.muted" :size="20" /><Mic v-else :size="20" />
          </button>
          <button
            class="grid size-12 shrink-0 place-items-center rounded-full bg-red-600 text-white hover:bg-red-700"
            title="Colgar"
            @click="hangUp"
          >
            <PhoneOff :size="20" />
          </button>
        </template>
        <button v-else-if="configured !== false" class="btn-primary h-12 shrink-0" @click="call">
          <Phone :size="16" /> Llamar
        </button>
      </div>
      <p v-if="view.error" class="bg-red-50 px-3 py-2 text-sm text-red-800">{{ view.error }}</p>
      <p v-if="configured === false" class="bg-amber-50 px-3 py-2 text-sm text-amber-900">
        La llamada no está configurada en el servidor: falta la clave de Gemini (ITHOMIINI_GEMINI_API_KEY_FILE).
      </p>

      <div class="grid min-h-0 flex-1 grid-rows-2 md:grid-cols-2 md:grid-rows-1">
        <section class="min-h-0 overflow-y-auto border-b border-stone-200 px-3 pb-3 md:border-r md:border-b-0">
          <h3 class="sticky top-0 z-10 bg-stone-50 pt-2 pb-1 text-xs font-medium text-stone-600">
            Cambios propuestos en esta llamada<span v-if="waiting"> · {{ waiting }} por revisar</span>
          </h3>
          <p v-if="!view.proposals.length" class="py-3 text-sm text-stone-500">
            Dicta lo que hay que anotar, por ejemplo "la 5VB es hembra y murió ayer". Los cambios aparecen aquí; se guardan en la
            hoja cuando dices "sí, guárdalo" o tocas ✓.
          </p>
          <ProposalGrid
            v-for="p in newestFirst"
            :key="p.id"
            :proposal="p"
            :busy="applying === p.id"
            @apply="indexes => apply(p, indexes)"
            @discard="discard(p)"
          />
        </section>

        <section class="flex min-h-0 flex-col">
          <div ref="scroller" class="flex-1 space-y-2 overflow-y-auto px-3 py-3 text-sm" aria-live="polite">
            <p v-if="!view.lines.length" class="text-stone-500">
              {{ active ? 'Aquí aparece lo que dicen los dos.' : 'Pulsa Llamar y habla: el asistente te responde en voz alta.' }}
            </p>
            <div v-for="line in view.lines" :key="line.id" :class="line.role === 'user' ? 'flex justify-end' : ''">
              <p
                class="max-w-[85%] rounded-lg px-3 py-1.5 whitespace-pre-wrap"
                :class="[
                  line.role === 'user' ? 'bg-brand-700 text-white' : 'border border-stone-200 bg-white',
                  line.final ? '' : 'opacity-70',
                ]"
              >
                {{ line.text.trim() || '…' }}
              </p>
            </div>
          </div>
          <form class="flex gap-2 border-t border-stone-200 bg-white p-2" @submit.prevent="send">
            <input
              v-model="typed"
              class="field-input flex-1"
              placeholder="Escribir en lugar de hablar"
              :disabled="!active"
              aria-label="Escribir al asistente"
            />
            <button class="btn-primary" :disabled="!active || !typed.trim()" title="Enviar"><Send :size="15" /></button>
          </form>
        </section>
      </div>
    </aside>
  </div>
</template>
