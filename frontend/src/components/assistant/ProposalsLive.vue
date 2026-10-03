<script setup lang="ts">
import {
  computed,
  defineAsyncComponent,
  h,
  nextTick,
  onActivated,
  onBeforeUnmount,
  onDeactivated,
  onMounted,
  ref,
  watch,
} from 'vue'
import { useRouter } from 'vue-router'
import { ExternalLink, Link, ListChecks, Maximize2, Minimize2, PanelBottom, PanelRight, X } from 'lucide-vue-next'
import type { Proposal } from '../../lib/proposals'
import WhenSeen from './WhenSeen.vue'
import { api, requestId } from '../../lib/api'
import { errorText, notify } from '../../lib/notice'
import {
  cardHeight,
  chatOptions,
  elsewhere,
  hasChats,
  keepChoice,
  listQuery,
  pagePath,
  proposalPage,
  type ChatChoice,
  type ChatEntry,
  type ChatScope,
} from '../../lib/proposalChats'
import { afterMove, seenChat, type T3Seen } from '../../lib/t3Bridge'
import { useTables } from '../../stores/tables'
import { intlLocale, t, tn } from '../../lib/i18n'

// The tables (Tabulator) load apart, so the Asistente tab and T3 start without waiting for them.
const ProposalGrid = defineAsyncComponent({
  loader: () => import('../ProposalGrid.vue'),
  loadingComponent: {
    render: () =>
      h(
        'p',
        { class: 'mt-2 rounded-md border border-stone-200 bg-white px-3 py-6 text-sm text-stone-500' },
        t('Cargando la tabla…'),
      ),
  },
  delay: 150,
})

/**
 * The assistant's proposed edits beside the T3 chat (at the right or below
 * it), or full screen in another browser tab (ProposalsView), updated while it
 * works: the request waits on the server (long polling) and returns as soon as
 * a proposal is added, revised (by the assistant or by the person in the
 * table), applied or discarded, from T3 Code (e.g. a notebook photo matched
 * with match_notebook), the chat or Revisión de datos.
 * It shows the proposals of the chat open in the T3 frame beside it (its
 * bridge says which, lib/t3Bridge; without it, or on its own browser tab, the
 * server guesses from T3, see server/t3chats.mjs), or of the chat picked in its
 * selector, or all; each table is only built when it comes into view (WhenSeen).
 */
const props = withDefaults(
  defineProps<{
    /** Where it sits beside T3; 'page' = its own browser tab. */
    layout?: 'right' | 'bottom' | 'page'
    /** Over T3 (the whole Asistente area). */
    full?: boolean
    /** Only this proposal (#/propuestas/<id>). */
    only?: string
    /** Fixed to this chat (#/propuestas?chat=…): it stays whatever chat T3 opens. */
    chat?: string
    /** What the T3 frame beside it shows (T3Frame). */
    t3?: T3Seen | null
    /** A proposal a link asks for (#/asistente?propuesta=…): its chat's list, scrolled to it. */
    focus?: { id: string } | null
  }>(),
  { layout: 'right', full: false, only: '', chat: '', t3: null, focus: null },
)
const emit = defineEmits<{
  count: [n: number]
  fresh: []
  close: []
  layout: [value: 'right' | 'bottom']
  full: []
  /** On its own page: the chat picked in the selector ('' = follow T3), for the page's address. */
  chat: [value: string]
}>()
const tables = useTables()
const router = useRouter()
const proposals = ref<Proposal[]>([])
const revision = ref('')
/** The chats and titles the list came with, as the server sums them up (it answers "unchanged" while they hold). */
const stamp = ref('')
const connected = ref(false)
const applying = ref<string | null>(null)
/** Proposals that just arrived, outlined for a few seconds. */
const arrived = ref(new Set<string>())
const showReviewed = ref(false)
/** The chat shown ('auto': the one open in T3), the one T3 shows (tracked), the chats with proposals. */
const chosen = ref<ChatChoice>(props.chat || 'auto')
const scope = ref<ChatScope | null>(null)
const tracked = ref<ChatScope | null>(null)
const chats = ref<ChatEntry[]>([])
/** The chat the T3 frame shows (a thread, 'draft' or 'none'); undefined: the server guesses. */
const seen = computed(() => seenChat(props.t3))
const options = computed(() => chatOptions(tracked.value, chats.value, props.chat ? scope.value : null))
const others = computed(() => elsewhere(scope.value, chats.value))
/** The time only, when the list is one chat's; with the chat's title when it mixes chats. */
const mixed = computed(() => !scope.value || scope.value.chat === 'all' || scope.value.chat === 'app')

const open = (p: Proposal) => p.status === 'pending' || p.status === 'applying'
const mine = computed(() => (props.only ? proposals.value.filter(p => p.id === props.only) : proposals.value))
const pending = computed(() => mine.value.filter(open))
const reviewed = computed(() => (props.only ? [] : mine.value.filter(p => !open(p)).slice(0, 5)))
/** The tables shown first: the pending ones; on a proposal's own page, that proposal whatever its state. */
const cards = computed(() => (props.only ? mine.value : pending.value))
watch(
  () => pending.value.length,
  n => emit('count', n),
  { immediate: true },
)
/** Another tab fixed to what this shows: its proposal, or the chat shown (it stays when T3 opens another chat). */
const pageLink = computed(() => router.resolve(pagePath(props.only, scope.value)).href)
const proposalLink = (id: string) => router.resolve(proposalPage(id)).href
async function copyLink(id: string) {
  const url = new URL(proposalLink(id), location.href).href
  try {
    await navigator.clipboard.writeText(url)
    notify(t('Enlace copiado'))
  } catch {
    notify(url)
  }
}

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
/** Aborts the waiting request when the person picks another chat, so the list changes at once. */
let asking: AbortController | null = null
// Another tab of the app on screen (this view kept alive): no asking meanwhile; back here, it asks
// at once with the revision it holds (answered at once if anything changed).
let active = true
let resume: (() => void) | null = null
onDeactivated(() => {
  active = false
  asking?.abort()
})
onActivated(() => {
  active = true
  resume?.()
  resume = null
})
function choose(value: string) {
  // Its own page keeps the chat in its address (fixed to it; 'auto' follows T3 again).
  if (props.layout === 'page') return emit('chat', value === 'auto' ? '' : value)
  chosen.value = value === tracked.value?.chat ? 'auto' : value
  asking?.abort()
}
watch(
  () => props.chat,
  chat => {
    chosen.value = chat || 'auto'
    asking?.abort()
  },
)
watch(
  () => props.only,
  () => asking?.abort(),
)
// Another chat opened in the frame: its list at once (a chat picked by hand gives way to it, a fixed one stays).
watch(seen, (now, before) => {
  if (!props.chat) chosen.value = afterMove(chosen.value, before, now)
  asking?.abort()
})
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
  let asked = ''
  while (!stopped) {
    if (!active) await new Promise<void>(go => (resume = go))
    if (document.visibilityState !== 'visible') await visible()
    const ask = (asking = new AbortController())
    const wanted = props.only || `${chosen.value} ${seen.value ?? ''}`
    try {
      const out = await api<
        | { revision: string; stamp: string; scope: ChatScope; follow: ChatScope; chats: ChatEntry[]; proposals: Proposal[] }
        | { unchanged: true; revision: string; stamp: string }
      >(
        listQuery({
          chosen: chosen.value,
          follow: tracked.value,
          seen: seen.value,
          only: props.only,
          revision: asked === wanted ? revision.value : '',
          stamp: stamp.value,
        }),
        { signal: ask.signal },
      )
      connected.value = true
      // Nothing new but this page's own edits (it has those): the list stays as it is.
      if ('unchanged' in out) {
        revision.value = out.revision
        continue
      }
      // Another chat's list: its proposals are not new arrivals.
      const moved = !scope.value || out.scope.chat !== scope.value.chat || asked !== wanted
      receive(out.proposals, first || moved)
      if (!props.chat) chosen.value = keepChoice(chosen.value, tracked.value, out.follow)
      scope.value = out.scope
      tracked.value = out.follow
      chats.value = out.chats
      revision.value = out.revision
      stamp.value = out.stamp ?? ''
      asked = wanted
      first = false
    } catch {
      // Picked another chat meanwhile: asked again at once.
      if (ask.signal.aborted) continue
      connected.value = false
      await sleep(5000)
    }
  }
}
onMounted(follow)
onBeforeUnmount(() => {
  stopped = true
  asking?.abort()
})

// ------------------------------------------------------------ a proposal a link asks for
/** The proposal to bring into view once it is in the list (its chat chosen if it is not). */
const focusing = ref<string | null>(null)
const listBox = ref<HTMLElement>()
let lookedUp = ''
async function bring() {
  const id = focusing.value
  if (!id) return
  const found = proposals.value.find(p => p.id === id)
  if (!found) {
    if (lookedUp === id || props.only || !connected.value) return
    lookedUp = id
    try {
      const out = await api<{ proposals: { id: string; chat?: string | null }[] }>(
        `chat/proposals?${new URLSearchParams({ only: id, all: '1' })}`,
      )
      const it = out.proposals.find(p => p.id === id)
      if (it && focusing.value === id) choose(it.chat || 'app')
    } catch {
      /* stays on the list shown */
    }
    return
  }
  focusing.value = null
  lookedUp = ''
  if (!open(found)) showReviewed.value = true
  arrived.value = new Set([...arrived.value, id])
  setTimeout(() => (arrived.value = new Set([...arrived.value].filter(other => other !== id))), 4000)
  await nextTick()
  listBox.value?.querySelector(`[data-proposal="${id}"]`)?.scrollIntoView({ block: 'start', behavior: 'smooth' })
}
watch(
  () => props.focus,
  value => {
    if (!value) return
    focusing.value = value.id
    lookedUp = ''
    void bring()
  },
  { immediate: true },
)
watch([proposals, connected], () => void bring())

async function apply(proposal: Proposal, indexes: number[], at: number | undefined, doubtful?: 'confirm' | 'skip') {
  applying.value = proposal.id
  try {
    const out = await api<{ status: Proposal['status']; applied: number[] }>(`chat/proposals/${proposal.id}/apply`, {
      method: 'POST',
      body: { requestId: requestId(), indexes, revision: at, ...(doubtful ? { doubtful } : {}) },
    })
    proposal.status = out.status
    proposal.applied = out.applied
    await Promise.all(Object.keys(tables.tables).map(sheet => tables.load(sheet, true)))
    notify(tn(out.applied.length, '{n} fila aplicada en Google Sheets', '{n} filas aplicadas en Google Sheets'), 'success')
  } catch (e) {
    // Changed meanwhile by the assistant, or doubtful cells to look at first: still pending.
    const code = (e as { code?: string }).code
    if (code === 'doubtful_unchecked') notify(t('Hay celdas dudosas sin revisar: revísalas o elige cómo aplicarlas'), 'error')
    else {
      if (code !== 'proposal_changed') proposal.status = 'needs_review'
      notify(errorText(e), 'error')
    }
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
  [mixed.value ? p.source : '', p.createdAt ? new Date(p.createdAt).toLocaleTimeString(intlLocale(), { hour: '2-digit', minute: '2-digit' }) : '']
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
          :title="$t('Abrir en otra pestaña (p. ej. en otra pantalla): estas propuestas, aunque aquí cambies de chat')"
          :aria-label="$t('Abrir en otra pestaña')"
        >
          <ExternalLink :size="15" />
        </a>
        <button class="btn-ghost" :title="$t('Ocultar los cambios propuestos')" @click="emit('close')"><X :size="15" /></button>
      </template>
    </header>
    <!-- Whose proposals: the chat open in T3 (followed), another chat with proposals, those outside T3, all. -->
    <div
      v-if="!only && (chat || hasChats(tracked, chats))"
      class="flex items-center gap-2 border-b border-stone-200 bg-white px-3 py-1 text-xs text-stone-600"
    >
      <select
        class="min-w-0 max-w-full truncate rounded border border-stone-200 bg-white py-0.5 pr-6 pl-1.5 text-xs text-stone-800"
        :value="chosen"
        :aria-label="$t('Propuestas de qué chat')"
        :title="
          tracked?.how === 'recent'
            ? $t('T3 no dice qué chat está abierto: se muestra el último con actividad. Elige otro aquí.')
            : $t('Las propuestas del chat abierto en T3; elige otro chat o todos aquí.')
        "
        @change="choose(($event.target as HTMLSelectElement).value)"
      >
        <option v-for="o in options" :key="o.value" :value="o.value">{{ o.label }}</option>
      </select>
      <button v-if="others" class="shrink-0 hover:text-emerald-800 hover:underline" @click="choose('all')">
        {{ $tn(others, '{n} propuesta más en otro chat', '{n} propuestas más en otros chats') }}
      </button>
    </div>
    <!-- A size container: each table is at most its height (ProposalSheet), so its column names stay in sight. -->
    <div ref="listBox" class="min-h-0 flex-1 overflow-y-auto px-3 pb-3 [container-type:size]" data-lazy-root>
      <p v-if="!cards.length" class="py-4 text-sm text-stone-500">
        <template v-if="only && !mine.length">{{ scope ? $t('No se encontró esta propuesta en tu cuenta.') : $t('Cargando…') }}</template>
        <template v-else>
          <strong v-if="scope?.chat === 'draft'" class="block font-medium text-stone-700">{{
            $t('Aún no hay propuestas en este chat.')
          }}</strong>
          <strong v-else-if="!mixed" class="block font-medium text-stone-700">{{ $t('Este chat no tiene cambios por revisar.') }}</strong>
          {{
            $t(
              'Cuando el asistente proponga cambios en la hoja aparecerán aquí al momento, con las celdas cambiadas en verde. Puedes corregirlas en la tabla como en Colecta o pedírselo al asistente (la tabla cambia en vivo); luego pulsa Aplicar, o dile «sí, aplícalo» en el chat.',
            )
          }}
        </template>
      </p>
      <div
        v-for="p in cards"
        :key="p.id"
        :data-proposal="p.id"
        class="rounded-md transition-shadow duration-700"
        :class="{ 'ring-2 ring-emerald-400': arrived.has(p.id) }"
      >
        <div class="mt-2 flex items-center gap-1 px-1 text-[11px] text-stone-500">
          <span class="min-w-0 truncate">{{ origin(p) }}</span>
          <a
            v-if="only !== p.id"
            class="ml-auto shrink-0 rounded p-1 hover:bg-stone-200 hover:text-stone-800"
            :href="proposalLink(p.id)"
            target="_blank"
            rel="noopener"
            :title="$t('Abrir esta propuesta sola en otra pestaña')"
            :aria-label="$t('Abrir esta propuesta sola en otra pestaña')"
          >
            <ExternalLink :size="12" />
          </a>
          <button
            class="shrink-0 rounded p-1 hover:bg-stone-200 hover:text-stone-800"
            :class="{ 'ml-auto': only === p.id }"
            :title="$t('Copiar el enlace de esta propuesta')"
            :aria-label="$t('Copiar el enlace de esta propuesta')"
            @click="copyLink(p.id)"
          >
            <Link :size="12" />
          </button>
        </div>
        <WhenSeen :height="cardHeight(p.changes.length)">
          <ProposalGrid
            :proposal="p"
            :busy="applying === p.id"
            @apply="(indexes, at, doubtful) => apply(p, indexes, at, doubtful)"
            @discard="discard(p)"
            @replace="replace"
          />
        </WhenSeen>
      </div>
      <details
        v-if="reviewed.length"
        class="mt-3"
        :open="showReviewed"
        @toggle="showReviewed = ($event.target as HTMLDetailsElement).open"
      >
        <summary class="cursor-pointer text-xs text-stone-600">
          {{ $t('Revisados hace poco ({n})', { n: reviewed.length }) }}
        </summary>
        <!-- Their tables are only built when opened. -->
        <div
          v-for="p in showReviewed ? reviewed : []"
          :key="p.id"
          :data-proposal="p.id"
          class="rounded-md transition-shadow duration-700"
          :class="{ 'ring-2 ring-emerald-400': arrived.has(p.id) }"
        >
          <p class="mt-2 px-1 text-[11px] text-stone-500">{{ origin(p) }}</p>
          <WhenSeen :height="cardHeight(p.changes.length)"><ProposalGrid :proposal="p" /></WhenSeen>
        </div>
      </details>
    </div>
  </aside>
</template>
