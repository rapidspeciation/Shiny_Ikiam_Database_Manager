<script setup lang="ts">
import { computed, nextTick, onMounted, onUnmounted, ref, watch } from 'vue'
import { RouterLink, useRoute, useRouter } from 'vue-router'
import { ArrowLeft, Bot, ChevronDown, ChevronRight, FileCode2, FileText, History, ListTree, MessageSquare, Wrench } from 'lucide-vue-next'
import { api } from '../lib/api'
import { errorText } from '../lib/notice'
import { renderMarkdown } from '../lib/markdown'
import { commitTime, entryHref, resolveLink, type Entry, type InstructionsPage } from '../lib/instructions'
import DiffView from '../components/instructions/DiffView.vue'
import ToolList from '../components/instructions/ToolList.vue'
import DocReview from '../components/instructions/DocReview.vue'
import { act, countFor, loadReview, openCount, review } from '../lib/docReview'
import { notify } from '../lib/notice'
import { t3Host } from '../lib/t3Host'
import { t, tn } from '../lib/i18n'

/**
 * What the AI assistant is told, for anyone on the team: the brief (AGENTS.md,
 * as each T3 workspace gets it, with a generic person), the skills with their reference files,
 * the subagents and the MCP tools, each with its change history. Opened from
 * the Asistente bar; #/instrucciones?archivo=<path> opens one file.
 */
const route = useRoute()
const router = useRouter()
const page = ref<InstructionsPage | null>(null)
const failed = ref('')
async function loadPage() {
  try {
    page.value = await api<InstructionsPage>('instructions')
  } catch (e) {
    failed.value = errorText(e)
  }
}
onMounted(loadPage)

// ------------------------------------------------------------------ comments and «Pedir a Claude»
// The comments are read again every 20 s, every 4 s while Claude works on them; the text when a change is applied.
let timer: ReturnType<typeof setTimeout> | undefined
async function poll() {
  await loadReview()
  timer = setTimeout(poll, review.state?.ask.state === 'running' ? 4000 : 20_000)
}
onMounted(poll)
onUnmounted(() => clearTimeout(timer))
watch(
  () => review.state?.changes.map(c => `${c.seriesId}:${c.status}`).join(),
  (now, was) => {
    if (was !== undefined && now !== was) void loadPage()
  },
)
const asking = ref(false)
const running = computed(() => review.state?.ask.state === 'running')
const openThreads = computed(() => openCount(review.state))
async function askClaude() {
  asking.value = true
  try {
    const out = await act<{ chat: string | null; prompt?: string | null }>('ask')
    if (!out.chat && out.prompt) {
      await navigator.clipboard?.writeText(out.prompt).catch(() => {})
      notify(t('T3 no está disponible: el pedido está copiado para pegarlo en un chat.'))
    }
    clearTimeout(timer)
    timer = setTimeout(poll, 3000)
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    asking.value = false
  }
}
function openChat(chat: string | null) {
  if (!chat) return
  t3Host.open = { thread: chat }
  void router.push('/asistente')
}
function asked() {
  clearTimeout(timer)
  timer = setTimeout(poll, 3000)
}


const entries = computed(() => page.value?.entries ?? [])
const ids = computed(() => new Set(entries.value.map(e => e.id)))
/** The file shown: the link's, else (on computers) the brief. On phones without one, the list. */
const chosenId = computed(() => (typeof route.query.archivo === 'string' ? route.query.archivo : null))
const entry = computed(() => entries.value.find(e => e.id === chosenId.value) ?? (chosenId.value ? null : (entries.value[0] ?? null)))
const open = (id: string) => router.replace({ query: { ...route.query, archivo: id, ver: undefined } })

/** Sidebar groups; a skill's reference files under its SKILL.md. */
const groups = computed(() => [
  { key: 'brief', label: t('Instrucciones generales'), icon: FileText, items: entries.value.filter(e => e.group === 'brief') },
  { key: 'skills', label: t('Habilidades (skills)'), icon: ListTree, items: entries.value.filter(e => e.group === 'skills') },
  { key: 'agents', label: t('Subagentes (solo Claude)'), icon: Bot, items: entries.value.filter(e => e.group === 'agents') },
  { key: 'tools', label: t('Herramientas'), icon: Wrench, items: entries.value.filter(e => e.group === 'tools') },
])
const isMain = (e: Entry) => e.group !== 'skills' || e.id.endsWith(`/${e.skill}/SKILL.md`)
const label = (e: Entry) => (e.kind === 'tools' ? tn(e.tools?.length ?? 0, '{n} herramienta (MCP)', '{n} herramientas (MCP)') : e.title)

/** What each kind of file is, in one line above it. */
const about = computed(() => {
  const e = entry.value
  if (!e) return ''
  if (e.id === 'assistant/AGENTS.md')
    return t(
      'El resumen que cada chat del asistente (Claude o Codex) lee al empezar. Cada espacio de T3 lo recibe con el nombre de su persona; aquí, con una persona genérica.',
    )
  if (e.group === 'skills')
    return isMain(e)
      ? t('Una habilidad: el asistente la carga cuando la tarea coincide con su descripción. Claude Code la lee de .claude/skills y Codex de .agents/skills.')
      : t('Un archivo de referencia de la habilidad {skill}: el asistente lo lee cuando la habilidad lo indica.', { skill: e.skill })
  if (e.group === 'agents')
    return t('Un subagente de Claude Code: otro modelo al que el asistente encarga una tarea. Codex no tiene subagentes: hace esa tarea él mismo.')
  return t('Las herramientas de la app (servidor MCP) que recibe cada chat, tal como las ve el modelo: nombre, descripción y parámetros.')
})

// ------------------------------------------------------------------ the text and its table of contents
const linkTo = (href: string) => {
  const id = entry.value && resolveLink(entry.value.id, href, ids.value)
  return id ? entryHref(id) : null
}
const rendered = computed(() =>
  entry.value?.kind === 'markdown' ? renderMarkdown(entry.value.content ?? '', { link: linkTo }) : null,
)
const toc = computed(() => {
  if (entry.value?.kind === 'tools') return (entry.value.tools ?? []).map(tool => ({ id: `tool-${tool.name}`, text: tool.name, level: 2 }))
  const headings = rendered.value?.headings.filter(h => h.level === 2 || h.level === 3) ?? []
  return headings.length >= 3 ? headings : []
})
const pane = ref<HTMLElement>()
function jump(id: string) {
  document.getElementById(id)?.scrollIntoView({ behavior: 'smooth', block: 'start' })
}

// ------------------------------------------------------------------ history
const tab = computed<'text' | 'history'>(() => (route.query.ver === 'cambios' ? 'history' : 'text'))
const setTab = (value: 'text' | 'history') => router.replace({ query: { ...route.query, ver: value === 'history' ? 'cambios' : undefined } })
const diffs = ref<Record<string, string | { error: string } | null>>({})
const opened = ref<Set<string>>(new Set())
async function toggle(commit: string) {
  const e = entry.value
  if (!e) return
  const key = `${e.id} ${commit}`
  const next = new Set(opened.value)
  if (next.has(key)) next.delete(key)
  else next.add(key)
  opened.value = next
  if (!next.has(key) || key in diffs.value) return
  diffs.value = { ...diffs.value, [key]: null }
  try {
    const out = await api<{ diff: string }>(`instructions/diff?id=${encodeURIComponent(e.id)}&commit=${encodeURIComponent(commit)}`)
    diffs.value = { ...diffs.value, [key]: out.diff }
  } catch (err) {
    diffs.value = { ...diffs.value, [key]: { error: errorText(err) } }
  }
}
watch(
  () => [entry.value?.id, tab.value],
  async () => {
    await nextTick()
    pane.value?.scrollTo({ top: 0 })
  },
)
</script>

<template>
  <div class="flex h-full flex-col">
    <div class="flex items-center gap-2 border-b border-stone-200 bg-stone-100 px-2 py-1 text-sm">
      <RouterLink to="/asistente" class="btn-ghost gap-1 px-2 py-0.5 text-stone-700">
        <ArrowLeft :size="14" /> {{ $t('Asistente') }}
      </RouterLink>
      <h1 class="font-semibold text-stone-800">{{ $t('Instrucciones de la IA') }}</h1>
      <span v-if="page && !page.historySource" class="ml-auto hidden text-xs text-stone-500 sm:inline">
        {{ $t('El historial de cambios no está disponible en esta instalación.') }}
      </span>
      <span v-if="review.state?.admin && running" class="ml-auto flex items-center gap-1 text-xs text-stone-600">
        {{ $t('Claude está revisando') }} ·
        <button type="button" class="font-medium text-brand-text underline" @click="openChat(review.state.ask.chat)">{{ $t('Abrir chat') }}</button>
      </span>
      <button
        v-else-if="review.state?.admin && openThreads"
        type="button"
        class="btn ml-auto gap-1 px-2 py-0.5 text-xs"
        :class="{ 'sm:ml-3': page && !page.historySource }"
        :disabled="asking"
        @click="askClaude"
      >
        {{ $t('Pedir a Claude · {n}', { n: openThreads }) }}
      </button>
    </div>
    <p v-if="failed" class="p-4 text-sm text-red-700">{{ failed }}</p>
    <p v-else-if="!page" class="p-4 text-sm text-stone-500">{{ $t('Cargando…') }}</p>
    <div v-else class="flex min-h-0 flex-1">
      <!-- Every file; on phones the list alone until one is chosen. -->
      <nav
        class="w-full shrink-0 overflow-y-auto border-r border-stone-200 bg-surface py-2 md:block md:w-72"
        :class="chosenId ? 'hidden' : 'block'"
        :aria-label="$t('Archivos de instrucciones')"
      >
        <section v-for="group in groups" :key="group.key" class="mb-3">
          <h2 class="flex items-center gap-1.5 px-3 py-1 text-xs font-semibold tracking-wide text-stone-500 uppercase">
            <component :is="group.icon" :size="13" /> {{ group.label }}
          </h2>
          <ul>
            <li v-for="item in group.items" :key="item.id">
              <button
                class="flex w-full items-baseline gap-2 py-1.5 pr-3 text-left text-sm hover:bg-stone-100 md:py-1"
                :class="[
                  isMain(item) ? 'pl-5' : 'pl-9 text-stone-600',
                  entry?.id === item.id ? 'bg-brand-50 font-medium text-brand-800' : '',
                ]"
                :aria-current="entry?.id === item.id ? 'page' : undefined"
                @click="open(item.id)"
              >
                <FileCode2 v-if="item.kind === 'code'" :size="12" class="shrink-0 self-center text-stone-400" />
                <span class="min-w-0 flex-1 truncate" :class="{ 'font-mono text-[13px]': item.kind === 'markdown' || item.kind === 'code' }">{{
                  label(item)
                }}</span>
                <span
                  v-if="countFor(review.state, item.id)"
                  class="flex shrink-0 items-center gap-0.5 self-center text-xs text-stone-500"
                  :title="$t('Comentarios abiertos')"
                >
                  <MessageSquare :size="11" /> {{ countFor(review.state, item.id) }}
                </span>
                <span v-if="item.history?.length" class="shrink-0 text-xs text-stone-400">{{ item.history.length }}</span>
              </button>
            </li>
          </ul>
        </section>
      </nav>

      <!-- The chosen file. -->
      <section ref="pane" class="min-w-0 flex-1 overflow-y-auto bg-stone-50" :class="chosenId ? 'block' : 'hidden md:block'">
        <p v-if="!entry" class="p-4 text-sm text-stone-600">{{ $t('No hay ningún archivo con ese nombre.') }}</p>
        <div v-else class="mx-auto flex max-w-[90rem] gap-8 px-4 py-4 sm:px-6">
          <article class="min-w-0 flex-1">
            <button class="btn-ghost mb-2 gap-1 px-2 text-sm md:hidden" @click="router.replace({ query: {} })">
              <ArrowLeft :size="14" /> {{ $t('Todos los archivos') }}
            </button>
            <header class="border-b border-stone-200 pb-3">
              <h1 class="text-xl font-semibold text-stone-900">{{ label(entry) }}</h1>
              <p v-if="entry.kind !== 'tools'" class="mt-0.5 font-mono text-xs text-stone-500">
                {{ entry.id }}
              </p>
              <p class="mt-2 text-sm text-stone-600">{{ about }}</p>
              <!-- A skill's or subagent's description: how the model decides to use it. -->
              <dl v-if="entry.meta" class="mt-2 space-y-1 rounded-md border border-stone-200 bg-surface px-3 py-2 text-sm">
                <div v-if="entry.meta.description">
                  <dt class="mr-1 inline font-medium text-stone-700">{{ $t('Cuándo se usa') }}:</dt>
                  <dd class="inline text-stone-700">{{ entry.meta.description }}</dd>
                </div>
                <div v-if="entry.meta.model || entry.meta.effort || entry.meta.tools" class="flex flex-wrap gap-x-4 text-xs text-stone-600">
                  <span v-if="entry.meta.model">{{ $t('Modelo') }}: <code>{{ entry.meta.model }}</code></span>
                  <span v-if="entry.meta.effort">{{ $t('Esfuerzo') }}: <code>{{ entry.meta.effort }}</code></span>
                  <span v-if="entry.meta.tools">{{ $t('Herramientas') }}: <code>{{ entry.meta.tools }}</code></span>
                </div>
              </dl>
              <div class="mt-3 flex flex-wrap items-center gap-2 text-sm">
                <div class="inline-flex rounded-md border border-stone-300 bg-surface p-0.5" role="tablist">
                  <button
                    role="tab"
                    class="rounded px-3 py-1"
                    :class="tab === 'text' ? 'bg-brand text-white' : 'text-stone-700 hover:bg-stone-100'"
                    :aria-selected="tab === 'text'"
                    @click="setTab('text')"
                  >
                    {{ $t('Texto') }}
                  </button>
                  <button
                    role="tab"
                    class="flex items-center gap-1 rounded px-3 py-1"
                    :class="tab === 'history' ? 'bg-brand text-white' : 'text-stone-700 hover:bg-stone-100'"
                    :aria-selected="tab === 'history'"
                    @click="setTab('history')"
                  >
                    <History :size="13" /> {{ $t('Cambios ({n})', { n: entry.history?.length ?? 0 }) }}
                  </button>
                </div>
                <span v-if="entry.lastChanged" class="text-xs text-stone-500">
                  {{ $t('Último cambio: {date}', { date: commitTime(entry.lastChanged) }) }}
                </span>
              </div>
            </header>

            <template v-if="tab === 'text'">
              <!-- On narrow screens the contents fold above the text. -->
              <details v-if="toc.length" class="mt-3 rounded-md border border-stone-200 bg-surface px-3 py-2 text-sm" :class="entry.kind === 'markdown' ? '2xl:hidden' : 'xl:hidden'">
                <summary class="cursor-pointer font-medium text-stone-700">{{ $t('En esta página') }}</summary>
                <ul class="mt-1 space-y-0.5">
                  <li v-for="h in toc" :key="h.id" :class="{ 'pl-3': h.level === 3 }">
                    <button class="text-left text-brand-700 hover:underline" @click="jump(h.id)">{{ h.text }}</button>
                  </li>
                </ul>
              </details>
              <ToolList v-if="entry.kind === 'tools'" :tools="entry.tools ?? []" class="mt-4" />
              <pre v-else-if="entry.kind === 'code'" class="md-code mt-4">{{ entry.content }}</pre>
              <DocReview v-else :key="entry.id" class="mt-4" :entry-id="entry.id" :html="rendered?.html ?? ''" @asked="asked" />
            </template>

            <div v-else class="mt-4">
              <p v-if="!entry.history" class="text-sm text-stone-600">
                {{ $t('El historial de cambios no está disponible en esta instalación.') }}
              </p>
              <p v-else-if="!entry.history.length" class="text-sm text-stone-600">{{ $t('Sin cambios registrados todavía.') }}</p>
              <ol v-else class="space-y-2">
                <li v-for="c in entry.history" :key="c.commit" class="rounded-md border border-stone-200 bg-surface">
                  <button class="flex w-full items-start gap-2 px-3 py-2 text-left text-sm hover:bg-stone-50" @click="toggle(c.commit)">
                    <component
                      :is="opened.has(`${entry.id} ${c.commit}`) ? ChevronDown : ChevronRight"
                      :size="15"
                      class="mt-0.5 shrink-0 text-stone-400"
                    />
                    <span class="min-w-0 flex-1">
                      <span class="block text-stone-900">{{ c.subject }}</span>
                      <span class="text-xs text-stone-500">
                        {{ commitTime(c.date) }} · {{ c.author }} · <code>{{ c.commit.slice(0, 7) }}</code>
                      </span>
                    </span>
                  </button>
                  <div v-if="opened.has(`${entry.id} ${c.commit}`)" class="border-t border-stone-100 p-2">
                    <p v-if="diffs[`${entry.id} ${c.commit}`] === null" class="hint">{{ $t('Cargando…') }}</p>
                    <p v-else-if="typeof diffs[`${entry.id} ${c.commit}`] === 'object'" class="text-sm text-red-700">
                      {{ (diffs[`${entry.id} ${c.commit}`] as { error: string }).error }}
                    </p>
                    <DiffView
                      v-else
                      :diff="diffs[`${entry.id} ${c.commit}`] as string"
                      :show-paths="entry.kind === 'tools'"
                    />
                  </div>
                </li>
              </ol>
            </div>
          </article>

          <!-- The contents beside the text on wide screens. -->
          <aside v-if="tab === 'text' && toc.length" class="sticky top-4 hidden w-56 shrink-0 self-start" :class="entry.kind === 'markdown' ? '2xl:block' : 'xl:block'">
            <h2 class="mb-1 text-xs font-semibold tracking-wide text-stone-500 uppercase">{{ $t('En esta página') }}</h2>
            <ul class="max-h-[80vh] space-y-0.5 overflow-y-auto border-l border-stone-200 text-sm">
              <li v-for="h in toc" :key="h.id">
                <button
                  class="block w-full truncate py-0.5 text-left text-stone-600 hover:text-brand-700"
                  :class="h.level === 3 ? 'pl-6' : 'pl-3'"
                  :title="h.text"
                  @click="jump(h.id)"
                >
                  {{ h.text }}
                </button>
              </li>
            </ul>
          </aside>
        </div>
      </section>
    </div>
  </div>
</template>
