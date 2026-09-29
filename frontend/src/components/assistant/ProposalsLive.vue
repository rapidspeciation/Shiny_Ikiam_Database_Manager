<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { useRouter } from 'vue-router'
import { ExternalLink, ListChecks, Maximize2, Minimize2, PanelBottom, PanelRight, X } from 'lucide-vue-next'
import ProposalGrid, { type Proposal } from '../ProposalGrid.vue'
import { api, requestId } from '../../lib/api'
import { errorText, notify } from '../../lib/notice'
import { useTables } from '../../stores/tables'
import { intlLocale, tn } from '../../lib/i18n'

/**
 * The assistant's proposed edits beside the T3 chat (at the right or below
 * it), or full screen in another browser tab (#/propuestas), updated while it
 * works: the request waits on the server (long polling) and returns as soon as
 * a proposal is added, revised (by the assistant or by the person in the
 * table), applied or discarded, from T3 Code (e.g. a notebook photo matched
 * with match_notebook), the chat or Revisión de datos.
 */
const props = withDefaults(
  defineProps<{
    /** Where it sits beside T3; 'page' = its own browser tab. */
    layout?: 'right' | 'bottom' | 'page'
    /** Over T3 (the whole Asistente area). */
    full?: boolean
    /** Only this proposal (#/propuestas/<id>). */
    only?: string
  }>(),
  { layout: 'right', full: false, only: '' },
)
const emit = defineEmits<{ count: [n: number]; fresh: []; close: []; layout: [value: 'right' | 'bottom']; full: [] }>()
const tables = useTables()
const router = useRouter()
const proposals = ref<Proposal[]>([])
const revision = ref('')
const connected = ref(false)
const applying = ref<string | null>(null)
/** Proposals that just arrived, outlined for a few seconds. */
const arrived = ref(new Set<string>())
const showReviewed = ref(false)

const open = (p: Proposal) => p.status === 'pending' || p.status === 'applying'
const mine = computed(() => (props.only ? proposals.value.filter(p => p.id === props.only) : proposals.value))
const pending = computed(() => mine.value.filter(open))
const reviewed = computed(() => mine.value.filter(p => !open(p)).slice(0, 5))
watch(
  () => pending.value.length,
  n => emit('count', n),
  { immediate: true },
)
const pageLink = computed(() => router.resolve(props.only ? `/propuestas/${props.only}` : '/propuestas').href)

function receive(next: Proposal[], first: boolean) {
  // An unchanged proposal keeps its object, so its table is not redrawn.
  const before = new Map(proposals.value.map(p => [p.id, p]))
  const kept = next.map(p => {
    const old = before.get(p.id)
    return old && old.revision === p.revision && old.status === p.status && old.applied?.length === p.applied?.length ? old : p
  })
  const fresh = first ? [] : next.filter(p => open(p) && !before.has(p.id)).map(p => p.id)
  proposals.value = kept
  if (!fresh.length) return
  emit('fresh')
  arrived.value = new Set([...arrived.value, ...fresh])
  setTimeout(() => (arrived.value = new Set([...arrived.value].filter(id => !fresh.includes(id)))), 4000)
}
/** A proposal saved from the table (the person's edits) replaces the copy shown, before the next poll. */
function replace(next: Proposal) {
  const at = proposals.value.findIndex(p => p.id === next.id)
  if (at < 0) return
  const list = [...proposals.value]
  list[at] = { ...list[at], ...next }
  proposals.value = list
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

async function apply(proposal: Proposal, indexes: number[], at: number | undefined) {
  applying.value = proposal.id
  try {
    const out = await api<{ status: Proposal['status']; applied: number[] }>(`chat/proposals/${proposal.id}/apply`, {
      method: 'POST',
      body: { requestId: requestId(), indexes, revision: at },
    })
    proposal.status = out.status
    proposal.applied = out.applied
    await Promise.all(Object.keys(tables.tables).map(sheet => tables.load(sheet, true)))
    notify(tn(out.applied.length, '{n} fila aplicada en Google Sheets', '{n} filas aplicadas en Google Sheets'), 'success')
  } catch (e) {
    // Changed meanwhile by the assistant: still pending, to look at again.
    if ((e as { code?: string }).code !== 'proposal_changed') proposal.status = 'needs_review'
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
  [p.source, p.createdAt ? new Date(p.createdAt).toLocaleTimeString(intlLocale(), { hour: '2-digit', minute: '2-digit' }) : '']
    .filter(Boolean)
    .join(' · ')
</script>

<template>
  <aside class="flex min-h-0 flex-col bg-stone-50">
    <header class="flex items-center gap-1.5 border-b border-stone-200 bg-white px-3 py-1.5">
      <ListChecks :size="15" class="text-emerald-700" />
      <h2 class="text-sm font-medium">{{ $t('Cambios propuestos') }}</h2>
      <span class="text-xs text-stone-500">{{
        pending.length ? $t('{n} por revisar', { n: pending.length }) : $t('nada por revisar')
      }}</span>
      <span
        class="ml-auto flex items-center gap-1 text-[11px]"
        :class="connected ? 'text-emerald-700' : 'text-stone-400'"
        :title="
          connected ? $t('Se actualiza en cuanto el asistente propone o corrige algo') : $t('Sin conexión; se vuelve a intentar')
        "
      >
        <span class="h-1.5 w-1.5 rounded-full" :class="connected ? 'bg-emerald-600' : 'bg-stone-400'" /> {{ $t('en vivo') }}
      </span>
      <template v-if="layout !== 'page'">
        <span class="mx-1 hidden h-4 w-px bg-stone-200 md:block" />
        <button
          class="btn-ghost hidden md:inline-flex"
          :class="{ 'bg-stone-100 text-emerald-800': layout === 'right' && !full }"
          :title="$t('A la derecha del chat')"
          :aria-label="$t('Panel a la derecha')"
          @click="emit('layout', 'right')"
        >
          <PanelRight :size="15" />
        </button>
        <button
          class="btn-ghost hidden md:inline-flex"
          :class="{ 'bg-stone-100 text-emerald-800': layout === 'bottom' && !full }"
          :title="$t('Debajo del chat')"
          :aria-label="$t('Panel debajo')"
          @click="emit('layout', 'bottom')"
        >
          <PanelBottom :size="15" />
        </button>
        <button class="btn-ghost" :title="full ? $t('Volver a ver el chat') : $t('Pantalla completa')" @click="emit('full')">
          <Minimize2 v-if="full" :size="15" /><Maximize2 v-else :size="15" />
        </button>
        <a
          class="btn-ghost"
          :href="pageLink"
          target="_blank"
          rel="noopener"
          :title="$t('Abrir en otra pestaña (p. ej. en otra pantalla, con el chat en esta)')"
          :aria-label="$t('Abrir en otra pestaña')"
        >
          <ExternalLink :size="15" />
        </a>
        <button class="btn-ghost" :title="$t('Ocultar los cambios propuestos')" @click="emit('close')"><X :size="15" /></button>
      </template>
    </header>
    <div class="min-h-0 flex-1 overflow-y-auto px-3 pb-3">
      <p v-if="!pending.length" class="py-4 text-sm text-stone-500">
        <template v-if="only && !mine.length">{{ $t('Esta propuesta ya no está en la lista.') }}</template>
        <template v-else>
          {{
            $t(
              'Cuando el asistente proponga cambios en la hoja aparecerán aquí al momento, con las celdas cambiadas en verde. Puedes corregirlas en la tabla como en Colecta o pedírselo al asistente (la tabla cambia en vivo); luego pulsa Aplicar, o dile «sí, aplícalo» en el chat.',
            )
          }}
        </template>
      </p>
      <div
        v-for="p in pending"
        :key="p.id"
        class="rounded-md transition-shadow duration-700"
        :class="{ 'ring-2 ring-emerald-400': arrived.has(p.id) }"
      >
        <p class="mt-2 px-1 text-[11px] text-stone-500">{{ origin(p) }}</p>
        <ProposalGrid
          :proposal="p"
          :busy="applying === p.id"
          @apply="(indexes, at) => apply(p, indexes, at)"
          @discard="discard(p)"
          @replace="replace"
        />
      </div>
      <details v-if="reviewed.length" class="mt-3" @toggle="showReviewed = ($event.target as HTMLDetailsElement).open">
        <summary class="cursor-pointer text-xs text-stone-600">
          {{ $t('Revisados hace poco ({n})', { n: reviewed.length }) }}
        </summary>
        <!-- Their tables are only built when opened. -->
        <div v-for="p in showReviewed ? reviewed : []" :key="p.id">
          <p class="mt-2 px-1 text-[11px] text-stone-500">{{ origin(p) }}</p>
          <ProposalGrid :proposal="p" />
        </div>
      </details>
    </div>
  </aside>
</template>
