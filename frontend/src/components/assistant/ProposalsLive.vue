<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { ListChecks, X } from 'lucide-vue-next'
import ProposalGrid, { type Proposal } from '../ProposalGrid.vue'
import { api, requestId } from '../../lib/api'
import { errorText, notify } from '../../lib/notice'
import { useTables } from '../../stores/tables'

/**
 * The assistant's proposed edits beside the T3 chat, updated while it works:
 * the request waits on the server (long polling) and returns as soon as a
 * proposal is added, applied or discarded, from T3 Code, the chat or
 * Revisión de datos. The same review grid as the chat: green cells, ticks,
 * Aplicar and Descartar.
 */
const emit = defineEmits<{ count: [n: number]; fresh: []; close: [] }>()
const tables = useTables()
const proposals = ref<Proposal[]>([])
const revision = ref('')
const connected = ref(false)
const applying = ref<string | null>(null)
/** Proposals that just arrived, outlined for a few seconds. */
const arrived = ref(new Set<string>())

const open = (p: Proposal) => p.status === 'pending' || p.status === 'applying'
const pending = computed(() => proposals.value.filter(open))
const reviewed = computed(() => proposals.value.filter(p => !open(p)).slice(0, 5))
watch(() => pending.value.length, n => emit('count', n), { immediate: true })

function receive(next: Proposal[], first: boolean) {
  const known = new Set(proposals.value.map(p => p.id))
  const fresh = first ? [] : next.filter(p => open(p) && !known.has(p.id)).map(p => p.id)
  proposals.value = next
  if (!fresh.length) return
  emit('fresh')
  arrived.value = new Set([...arrived.value, ...fresh])
  setTimeout(() => (arrived.value = new Set([...arrived.value].filter(id => !fresh.includes(id)))), 4000)
}

let stopped = false
const sleep = (ms: number) => new Promise(resolve => setTimeout(resolve, ms))
const visible = () =>
  new Promise<void>(resolve => {
    const done = () => {
      if (document.visibilityState !== 'visible') return
      document.removeEventListener('visibilitychange', done)
      resolve()
    }
    document.addEventListener('visibilitychange', done)
  })
/** Follows the list for as long as the tab is open; a hidden browser tab stops asking. */
async function follow() {
  let first = true
  while (!stopped) {
    if (document.visibilityState !== 'visible') await visible()
    try {
      const out = await api<{ revision: string; proposals: Proposal[] }>(
        `chat/proposals?all=1&wait=1&revision=${encodeURIComponent(revision.value)}`,
      )
      connected.value = true
      if (out.revision !== revision.value || first) receive(out.proposals, first)
      revision.value = out.revision
      first = false
    } catch {
      connected.value = false
      await sleep(5000)
    }
  }
}
onMounted(follow)
onBeforeUnmount(() => (stopped = true))

async function apply(proposal: Proposal, indexes: number[]) {
  applying.value = proposal.id
  try {
    const out = await api<{ status: Proposal['status']; applied: number[] }>(`chat/proposals/${proposal.id}/apply`, {
      method: 'POST',
      body: { requestId: requestId(), indexes },
    })
    proposal.status = out.status
    proposal.applied = out.applied
    await Promise.all(Object.keys(tables.tables).map(sheet => tables.load(sheet, true)))
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
const origin = (p: Proposal) =>
  [p.source, p.createdAt ? new Date(p.createdAt).toLocaleTimeString('es-EC', { hour: '2-digit', minute: '2-digit' }) : '']
    .filter(Boolean)
    .join(' · ')
</script>

<template>
  <aside class="flex min-h-0 flex-col bg-stone-50">
    <header class="flex items-center gap-2 border-b border-stone-200 bg-white px-3 py-1.5">
      <ListChecks :size="15" class="text-emerald-700" />
      <h2 class="text-sm font-medium">Cambios propuestos</h2>
      <span class="text-xs text-stone-500">{{ pending.length ? `${pending.length} por revisar` : 'nada por revisar' }}</span>
      <span
        class="ml-auto flex items-center gap-1 text-[11px]"
        :class="connected ? 'text-emerald-700' : 'text-stone-400'"
        :title="connected ? 'Se actualiza en cuanto el asistente propone algo' : 'Sin conexión; se vuelve a intentar'"
      >
        <span class="h-1.5 w-1.5 rounded-full" :class="connected ? 'bg-emerald-600' : 'bg-stone-400'" /> en vivo
      </span>
      <button class="btn-ghost" title="Ocultar los cambios propuestos" @click="emit('close')"><X :size="15" /></button>
    </header>
    <div class="min-h-0 flex-1 overflow-y-auto px-3 pb-3">
      <p v-if="!pending.length" class="py-4 text-sm text-stone-500">
        Cuando el asistente proponga cambios en la hoja aparecerán aquí al momento, con las celdas cambiadas en verde. Desmarca
        las filas que no quieras y pulsa Aplicar, o dile «sí, aplícalo» en el chat.
      </p>
      <div
        v-for="p in pending"
        :key="p.id"
        class="rounded-md transition-shadow duration-700"
        :class="{ 'ring-2 ring-emerald-400': arrived.has(p.id) }"
      >
        <p class="mt-2 px-1 text-[11px] text-stone-500">{{ origin(p) }}</p>
        <!-- A notebook page's proposal changes as the page is corrected: its rows are ticked again. -->
        <ProposalGrid
          :key="p.changes.length"
          :proposal="p"
          :busy="applying === p.id"
          @apply="indexes => apply(p, indexes)"
          @discard="discard(p)"
        />
      </div>
      <details v-if="reviewed.length" class="mt-3">
        <summary class="cursor-pointer text-xs text-stone-600">Revisados hace poco ({{ reviewed.length }})</summary>
        <div v-for="p in reviewed" :key="p.id">
          <p class="mt-2 px-1 text-[11px] text-stone-500">{{ origin(p) }}</p>
          <ProposalGrid :proposal="p" />
        </div>
      </details>
    </div>
  </aside>
</template>
