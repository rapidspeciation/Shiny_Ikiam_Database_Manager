<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { RouterLink, useRoute } from 'vue-router'
import { Camera, ExternalLink, ImagePlus, ListChecks, Plus, RefreshCw, Send, Trash2, X } from 'lucide-vue-next'
import { api, requestId } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import { useTables } from '../stores/tables'
import ProposalGrid, { type Proposal } from '../components/ProposalGrid.vue'
import T3Frame from '../components/T3Frame.vue'
import ProposalsLive from '../components/assistant/ProposalsLive.vue'
import VoiceCall from '../components/assistant/VoiceCall.vue'
import { persistentRef } from '../lib/persist'

/**
 * Chat with the database assistant. It can read notebook photos; the rows it
 * wants to change appear as a table to confirm, or are applied when you say so.
 */
interface Source {
  id: string
  type: string
  sheet?: string
  row?: number
  label?: string
  title?: string
}
interface Result {
  title: string
  columns: { key: string; label: string }[] | string[]
  rows: Record<string, unknown>[] | unknown[][]
}
interface Attachment {
  id: string
  name: string
}
interface Message {
  id: string
  role: 'user' | 'assistant'
  content: string
  sources: Source[]
  results: Result[]
  proposals: Proposal[]
  attachments?: Attachment[]
}
interface Thread {
  id: string
  title: string
  updatedAt: string
}

const tables = useTables()
const route = useRoute()
const threads = ref<Thread[]>([])
const current = ref<string | null>(null)
const messages = ref<Message[]>([])
const draft = ref('')
const photos = ref<{ id: string; name: string; url: string }[]>([])
const uploading = ref(false)
const sending = ref(false)
const applying = ref<string | null>(null)
const status = ref<{ configured: boolean; provider: string; model: string | null } | null>(null)
const scroller = ref<HTMLElement>()
const t3Url = ref<string | null>(null)
const mode = persistentRef<'t3' | 'chat'>('assistant:mode', 't3')
// Proposals (from T3 Code or any conversation) beside T3, or under it on phones; see ProposalsLive.
const panel = persistentRef('assistant:proposals', window.matchMedia('(min-width: 768px)').matches)
const waiting = ref(0)
const fresh = ref(false)
function arrived() {
  // A new proposal shows the panel, so the person sees the edit as the assistant makes it.
  fresh.value = !panel.value
  panel.value = true
  setTimeout(() => (fresh.value = false), 4000)
}
const t3Frame = ref<InstanceType<typeof T3Frame>>()
const gallery = ref<HTMLInputElement>()
const camera = ref<HTMLInputElement>()
const started = ref(0)
const now = ref(Date.now())
const tick = setInterval(() => (now.value = Date.now()), 1000)
onBeforeUnmount(() => clearInterval(tick))

onMounted(async () => {
  try {
    t3Url.value = (await api<{ url: string | null }>('t3/status')).url
    status.value = await api('ai/status')
    threads.value = (await api<{ threads: Thread[] }>('chat/threads')).threads
    if (route.query.hilo) await openLinked()
    else if (threads.value[0]) await open(threads.value[0].id)
  } catch (e) {
    notify(errorText(e), 'error')
  }
})

// "Preguntar en el chat" from a digitized page opens that page's conversation (?hilo=…).
async function openLinked() {
  const id = String(route.query.hilo ?? '')
  if (!id) return
  mode.value = 'chat'
  if (!threads.value.some(t => t.id === id))
    threads.value = (await api<{ threads: Thread[] }>('chat/threads')).threads
  await open(id)
}
watch(() => route.query.hilo, id => id && void openLinked())

async function open(id: string) {
  current.value = id
  messages.value = (await api<{ messages: Message[] }>(`chat/threads/${id}`)).messages
  scrollDown()
}
async function newThread(title = 'Nueva conversación') {
  const { thread } = await api<{ thread: Thread }>('chat/threads', { method: 'POST', body: { title } })
  threads.value.unshift(thread)
  current.value = thread.id
  messages.value = []
  return thread.id
}
async function remove(id: string) {
  if (!confirm('¿Borrar esta conversación?')) return
  await api(`chat/threads/${id}`, { method: 'DELETE' })
  threads.value = threads.value.filter(t => t.id !== id)
  if (current.value === id) {
    current.value = null
    messages.value = []
  }
}

/** Photos are shrunk on the phone before upload: a notebook page reads well at 2000 px. */
async function shrink(file: File): Promise<string> {
  const bitmap = await createImageBitmap(file)
  const scale = Math.min(1, 2000 / Math.max(bitmap.width, bitmap.height))
  const canvas = document.createElement('canvas')
  canvas.width = Math.round(bitmap.width * scale)
  canvas.height = Math.round(bitmap.height * scale)
  canvas.getContext('2d')!.drawImage(bitmap, 0, 0, canvas.width, canvas.height)
  return canvas.toDataURL('image/jpeg', 0.85).split(',')[1]
}
async function addPhotos(event: Event) {
  const input = event.target as HTMLInputElement
  const files = [...(input.files ?? [])].slice(0, 6 - photos.value.length)
  input.value = ''
  uploading.value = true
  try {
    for (const file of files) {
      const { attachment } = await api<{ attachment: { id: string; name: string } }>('attachments', {
        method: 'POST',
        body: { name: file.name || 'foto.jpg', mimeType: 'image/jpeg', dataBase64: await shrink(file), requestId: requestId() },
      })
      photos.value.push({ id: attachment.id, name: attachment.name, url: URL.createObjectURL(file) })
    }
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    uploading.value = false
  }
}

async function send() {
  const text = draft.value.trim() || (photos.value.length ? 'Revisa esta foto del cuaderno y compárala con la hoja.' : '')
  if (!text || sending.value || uploading.value) return
  sending.value = true
  started.value = Date.now()
  const attached = photos.value.splice(0)
  try {
    const id = current.value || (await newThread(text.slice(0, 60)))
    messages.value.push({
      id: `local-${Date.now()}`,
      role: 'user',
      content: text,
      sources: [],
      results: [],
      proposals: [],
      attachments: attached.map(p => ({ id: p.id, name: p.name })),
    })
    draft.value = ''
    scrollDown()
    const reply = await api<{ message: Message; applied?: string[] }>(`chat/threads/${id}/messages`, {
      method: 'POST',
      body: { message: text, attachmentIds: attached.map(p => p.id) },
    })
    messages.value.push(reply.message)
    if (reply.applied?.length) await reloadTables()
    scrollDown()
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    sending.value = false
  }
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
function scrollDown() {
  nextTick(() => scroller.value?.scrollTo({ top: scroller.value.scrollHeight }))
}
const clean = (text: string) => text.replace(/\[[A-Za-z0-9_-]{8,120}\]/g, '').trim()
const title = computed(() => threads.value.find(t => t.id === current.value)?.title || 'Nueva conversación')
const elapsed = computed(() => Math.max(0, Math.round((now.value - started.value) / 1000)))

// Report tables arrive as {key,label} columns and row objects.
const columnsOf = (r: Result) => r.columns.map(c => (typeof c === 'string' ? { key: c, label: c } : c))
const cellOf = (row: Record<string, unknown> | unknown[], key: string, i: number) =>
  Array.isArray(row) ? row[i] : (row as Record<string, unknown>)[key]
</script>

<template>
  <div class="flex h-full flex-col">
    <!-- One slim bar, so T3 keeps nearly the whole screen. The call lives here once, whatever the mode. -->
    <div class="flex items-center gap-1 border-b border-stone-200 bg-stone-100 px-2 text-sm">
      <template v-if="t3Url">
        <button
          v-for="[key, label] in [
            ['t3', 'T3 Code'],
            ['chat', 'Chat simple'],
          ] as const"
          :key="key"
          class="px-3 py-1"
          :class="mode === key ? 'border-b-2 border-brand-700 font-medium text-stone-900' : 'text-stone-600 hover:text-stone-900'"
          @click="mode = key"
        >
          {{ label }}
        </button>
      </template>
      <VoiceCall />
      <template v-if="t3Url && mode === 't3'">
        <button
          class="ml-auto flex items-center gap-1 rounded px-2 py-0.5"
          :class="
            fresh
              ? 'animate-pulse bg-emerald-600 text-white'
              : waiting
                ? 'bg-emerald-100 text-emerald-900'
                : 'text-stone-500'
          "
          :title="panel ? 'Ocultar los cambios propuestos' : 'Mostrar los cambios propuestos'"
          @click="panel = !panel"
        >
          <ListChecks :size="14" /> Cambios propuestos ({{ waiting }})
        </button>
        <button class="btn-ghost" title="Volver a conectar T3" @click="t3Frame?.connect(true)"><RefreshCw :size="13" /></button>
        <a class="btn-ghost" :href="t3Url" target="_blank" rel="noopener" title="Abrir T3 en otra pestaña"
          ><ExternalLink :size="13"
        /></a>
      </template>
    </div>
    <div v-if="t3Url && mode === 't3'" class="flex min-h-0 flex-1 flex-col md:flex-row">
      <T3Frame ref="t3Frame" :url="t3Url" class="min-h-0 flex-1" />
      <!-- Proposed edits beside T3 (under it on phones), updated as the assistant drafts them. -->
      <ProposalsLive
        v-show="panel"
        class="max-h-[45%] border-t border-stone-300 md:max-h-none md:w-[42%] md:max-w-3xl md:border-t-0 md:border-l"
        @count="n => (waiting = n)"
        @fresh="arrived"
        @close="panel = false"
      />
    </div>
    <div v-else class="flex min-h-0 flex-1">
      <aside class="hidden w-60 shrink-0 flex-col border-r border-stone-200 bg-white md:flex">
        <button class="btn m-3" @click="newThread()"><Plus :size="15" /> Nueva conversación</button>
        <ul class="flex-1 overflow-y-auto text-sm">
          <li v-for="t in threads" :key="t.id" class="group flex items-center" :class="{ 'bg-brand-50': t.id === current }">
            <button class="min-w-0 flex-1 truncate px-3 py-2 text-left" @click="open(t.id)">{{ t.title }}</button>
            <button class="btn-ghost invisible mr-1 group-hover:visible" title="Borrar" @click="remove(t.id)">
              <Trash2 :size="14" />
            </button>
          </li>
        </ul>
      </aside>
      <section class="flex min-w-0 flex-1 flex-col">
        <header class="flex items-center gap-2 border-b border-stone-200 bg-white px-4 py-2">
          <h2 class="flex-1 truncate font-medium">{{ title }}</h2>
          <span v-if="status?.configured" class="hint">{{ status.provider }} · {{ status.model }}</span>
          <button class="btn md:hidden" @click="newThread()"><Plus :size="15" /></button>
        </header>
        <p v-if="status && !status.configured" class="bg-amber-50 px-4 py-2 text-sm text-amber-900">
          El asistente no tiene un proveedor de IA configurado en el servidor.
        </p>
        <div ref="scroller" class="flex-1 space-y-4 overflow-y-auto px-4 py-4">
          <p v-if="!messages.length" class="max-w-2xl text-sm text-stone-500">
            Pregunta por registros ("¿qué tubos FS se usaron la semana pasada?") o manda la foto de una página del cuaderno: el
            asistente la transcribe, la compara con la hoja y te muestra los cambios en una tabla para que los confirmes.
          </p>
          <div v-for="m in messages" :key="m.id" :class="m.role === 'user' ? 'flex justify-end' : ''">
            <div
              class="max-w-full rounded-lg px-3 py-2 text-sm whitespace-pre-wrap md:max-w-4xl"
              :class="m.role === 'user' ? 'bg-brand-700 text-white' : 'border border-stone-200 bg-white'"
            >
              <div v-if="m.attachments?.length" class="mb-1.5 flex flex-wrap gap-1.5">
                <a v-for="a in m.attachments" :key="a.id" :href="`api/attachments/${a.id}/content`" target="_blank">
                  <img :src="`api/attachments/${a.id}/content`" :alt="a.name" class="h-20 rounded border border-white/40" />
                </a>
              </div>
              {{ m.role === 'assistant' ? clean(m.content) : m.content }}
              <div v-if="m.sources.length" class="mt-2 flex flex-wrap gap-1">
                <template v-for="s in m.sources" :key="s.id">
                  <RouterLink
                    v-if="s.type === 'record' && s.sheet"
                    :to="{ path: '/tablas', query: { hoja: s.sheet, buscar: s.label } }"
                    class="rounded bg-stone-100 px-1.5 py-0.5 text-xs text-stone-700 hover:bg-brand-50"
                  >
                    {{ s.label }} · {{ s.sheet }} fila {{ s.row }}
                  </RouterLink>
                  <span v-else class="rounded bg-stone-100 px-1.5 py-0.5 text-xs text-stone-700">{{ s.title || s.label }}</span>
                </template>
              </div>
              <div v-for="(r, i) in m.results" :key="i" class="mt-2 overflow-x-auto whitespace-normal">
                <p class="text-xs font-medium">{{ r.title }}</p>
                <table class="mt-1 text-xs">
                  <tr>
                    <th v-for="c in columnsOf(r)" :key="c.key" class="border-b border-stone-200 px-2 py-1 text-left">
                      {{ c.label }}
                    </th>
                  </tr>
                  <tr v-for="(row, j) in r.rows.slice(0, 25)" :key="j">
                    <td v-for="(c, k) in columnsOf(r)" :key="c.key" class="px-2 py-0.5">{{ cellOf(row, c.key, k) }}</td>
                  </tr>
                </table>
              </div>
              <div class="whitespace-normal">
                <ProposalGrid
                  v-for="p in m.proposals"
                  :key="p.id"
                  :proposal="p"
                  :busy="applying === p.id"
                  @apply="indexes => apply(p, indexes)"
                  @discard="discard(p)"
                />
              </div>
            </div>
          </div>
          <p v-if="sending" class="text-sm text-stone-500">
            Pensando… {{ elapsed }} s
            <span v-if="elapsed > 15" class="hint">(leer una página y compararla con la hoja toma cerca de un minuto)</span>
          </p>
        </div>
        <form class="border-t border-stone-200 bg-white p-3" @submit.prevent="send">
          <div v-if="photos.length || uploading" class="mb-2 flex flex-wrap items-center gap-2">
            <div v-for="(p, i) in photos" :key="p.id" class="relative">
              <img :src="p.url" :alt="p.name" class="h-16 rounded border border-stone-300" />
              <button
                type="button"
                class="absolute -top-1.5 -right-1.5 rounded-full bg-stone-700 p-0.5 text-white"
                title="Quitar"
                @click="photos.splice(i, 1)"
              >
                <X :size="12" />
              </button>
            </div>
            <span v-if="uploading" class="hint">Subiendo foto…</span>
          </div>
          <div class="flex gap-2">
            <input ref="camera" type="file" accept="image/*" capture="environment" class="hidden" @change="addPhotos" />
            <input ref="gallery" type="file" accept="image/*" multiple class="hidden" @change="addPhotos" />
            <div class="flex flex-col gap-1 self-end">
              <button type="button" class="btn-ghost" title="Tomar foto" :disabled="photos.length >= 6" @click="camera?.click()">
                <Camera :size="17" />
              </button>
              <button
                type="button"
                class="btn-ghost"
                title="Elegir fotos"
                :disabled="photos.length >= 6"
                @click="gallery?.click()"
              >
                <ImagePlus :size="17" />
              </button>
            </div>
            <textarea
              v-model="draft"
              rows="2"
              class="field-input flex-1 resize-none"
              :placeholder="
                photos.length ? 'Opcional: qué revisar en la foto' : 'Escribe tu pregunta o manda una foto del cuaderno'
              "
              @keydown.enter.exact.prevent="send"
            />
            <button class="btn-primary self-end" :disabled="sending || uploading || (!draft.trim() && !photos.length)">
              <Send :size="15" />
            </button>
          </div>
        </form>
      </section>
    </div>
  </div>
</template>
