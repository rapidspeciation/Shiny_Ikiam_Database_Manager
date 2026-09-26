<script setup lang="ts">
import { computed, nextTick, onMounted, ref } from 'vue'
import { RouterLink } from 'vue-router'
import { Plus, Send, Trash2, Check } from 'lucide-vue-next'
import { api, requestId } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import { useTables } from '../stores/tables'

/** Chat with the database assistant. Answers cite rows; proposed edits need approval. */
interface Source {
  id: string
  type: string
  sheet?: string
  row?: number
  label?: string
  title?: string
}
interface Proposal {
  id: string
  reason: string
  status: string
  changes: { recordId: string; before: Record<string, unknown>; values: Record<string, unknown> }[]
}
interface Result {
  title: string
  columns: string[]
  rows: unknown[][]
}
interface Message {
  id: string
  role: 'user' | 'assistant'
  content: string
  sources: Source[]
  results: Result[]
  proposals: Proposal[]
}
interface Thread {
  id: string
  title: string
  updatedAt: string
}

const tables = useTables()
const threads = ref<Thread[]>([])
const current = ref<string | null>(null)
const messages = ref<Message[]>([])
const draft = ref('')
const sending = ref(false)
const status = ref<{ configured: boolean; model: string | null } | null>(null)
const scroller = ref<HTMLElement>()

onMounted(async () => {
  try {
    status.value = await api('ai/status')
    threads.value = (await api<{ threads: Thread[] }>('chat/threads')).threads
    if (threads.value[0]) await open(threads.value[0].id)
  } catch (e) {
    notify(errorText(e), 'error')
  }
})

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
async function send() {
  const text = draft.value.trim()
  if (!text || sending.value) return
  sending.value = true
  try {
    const id = current.value || (await newThread(text.slice(0, 60)))
    messages.value.push({ id: `local-${Date.now()}`, role: 'user', content: text, sources: [], results: [], proposals: [] })
    draft.value = ''
    scrollDown()
    const reply = await api<{ message: Message }>(`chat/threads/${id}/messages`, { method: 'POST', body: { message: text } })
    messages.value.push(reply.message)
    scrollDown()
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    sending.value = false
  }
}
async function apply(proposal: Proposal) {
  try {
    await api(`chat/proposals/${proposal.id}/apply`, { method: 'POST', body: { requestId: requestId() } })
    proposal.status = 'applied'
    await Promise.all(Object.keys(tables.tables).map(sheet => tables.load(sheet, true)))
    notify('Cambios aplicados en Google Sheets', 'success')
  } catch (e) {
    proposal.status = 'needs_review'
    notify(errorText(e), 'error')
  }
}
function scrollDown() {
  nextTick(() => scroller.value?.scrollTo({ top: scroller.value.scrollHeight }))
}
const clean = (text: string) => text.replace(/\[[A-Za-z0-9_-]{8,120}\]/g, '').trim()
const title = computed(() => threads.value.find(t => t.id === current.value)?.title || 'Nueva conversación')
</script>

<template>
  <div class="flex h-full">
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
        <button class="btn md:hidden" @click="newThread()"><Plus :size="15" /></button>
      </header>
      <p v-if="status && !status.configured" class="bg-amber-50 px-4 py-2 text-sm text-amber-900">
        El asistente no tiene un proveedor de IA configurado en el servidor.
      </p>
      <div ref="scroller" class="flex-1 space-y-4 overflow-y-auto px-4 py-4">
        <p v-if="!messages.length" class="text-sm text-stone-500">
          Pregunta por registros, por ejemplo: "¿Cuántas hembras emergieron del clutch 994(6)?" o "¿Qué tubos FS se usaron la
          semana pasada?". Las respuestas citan las filas de la hoja; los cambios propuestos necesitan tu aprobación.
        </p>
        <div v-for="m in messages" :key="m.id" :class="m.role === 'user' ? 'flex justify-end' : ''">
          <div
            class="max-w-3xl rounded-lg px-3 py-2 text-sm whitespace-pre-wrap"
            :class="m.role === 'user' ? 'bg-brand-700 text-white' : 'border border-stone-200 bg-white'"
          >
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
            <div v-for="(r, i) in m.results" :key="i" class="mt-2 overflow-x-auto">
              <p class="text-xs font-medium">{{ r.title }}</p>
              <table class="mt-1 text-xs">
                <tr>
                  <th v-for="c in r.columns" :key="c" class="border-b border-stone-200 px-2 py-1 text-left">{{ c }}</th>
                </tr>
                <tr v-for="(row, j) in r.rows.slice(0, 25)" :key="j">
                  <td v-for="(cell, k) in row" :key="k" class="px-2 py-0.5">{{ cell }}</td>
                </tr>
              </table>
            </div>
            <div v-for="p in m.proposals" :key="p.id" class="mt-2 rounded border border-amber-300 bg-amber-50 p-2 text-stone-800">
              <p class="text-xs font-medium">Cambio propuesto: {{ p.reason }}</p>
              <ul class="mt-1 text-xs">
                <li v-for="c in p.changes" :key="c.recordId">
                  <span v-for="(value, field) in c.values" :key="field" class="mr-2">
                    {{ field }}: <span class="line-through">{{ c.before[field] ?? 'vacío' }}</span> → <strong>{{ value }}</strong>
                  </span>
                </li>
              </ul>
              <button v-if="p.status === 'pending'" class="btn-primary mt-2" @click="apply(p)">
                <Check :size="14" /> Aplicar en la hoja
              </button>
              <p v-else class="mt-1 text-xs">{{ p.status === 'applied' ? 'Aplicado' : 'Necesita revisión' }}</p>
            </div>
          </div>
        </div>
        <p v-if="sending" class="text-sm text-stone-500">Pensando…</p>
      </div>
      <form class="flex gap-2 border-t border-stone-200 bg-white p-3" @submit.prevent="send">
        <textarea
          v-model="draft"
          rows="2"
          class="field-input flex-1 resize-none"
          placeholder="Escribe tu pregunta"
          @keydown.enter.exact.prevent="send"
        />
        <button class="btn-primary self-end" :disabled="sending || !draft.trim()"><Send :size="15" /></button>
      </form>
    </section>
  </div>
</template>
