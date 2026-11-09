<script setup lang="ts">
import { computed, nextTick, onMounted, onUnmounted, reactive, ref, watch } from 'vue'
import { MessageSquare, X } from 'lucide-vue-next'
import DocThreadCard from './DocThreadCard.vue'
import DocChangeCard from './DocChangeCard.vue'
import {
  act,
  answeredBy,
  clearMarks,
  forEntry,
  fullText,
  locate,
  markRange,
  review,
  sectionElements,
  selectionAnchor,
  shownInMargin,
  stackCards,
  type Anchor,
} from '../../lib/docReview'
import { errorText, notify } from '../../lib/notice'

/**
 * A Markdown file of the instructions with its comments: the text with the quoted passages highlighted and
 * a green bar beside each section a change is proposed for; on computers a margin of cards beside their
 * highlights, on phones a bottom sheet. Selecting text offers «Comentar».
 */
const props = defineProps<{ entryId: string; html: string }>()
const emit = defineEmits<{ asked: [chat: string | null] }>()

const content = ref<HTMLElement>()
const wrap = ref<HTMLElement>()
const margin = ref<HTMLElement>()
const wide = ref(typeof window !== 'undefined' && window.matchMedia?.('(min-width: 1024px)').matches)
const admin = computed(() => review.state?.admin ?? false)
const mine = computed(() => forEntry(review.state, props.entryId))
const open = computed(() => mine.value.threads.filter(t => t.status === 'open'))
const changes = computed(() => mine.value.changes.filter(c => shownInMargin(c)))
const answers = computed(() => answeredBy(review.state))
/** «Ver cambio» on a thread: its change becomes the active card (on phones, the sheet shows it). */
function showChange(seriesId: string) {
  listMode.value = false
  activate(seriesId, { scroll: true })
}
const total = computed(() => open.value.length + mine.value.changes.filter(c => c.status === 'pending').length)

const active = ref<string | null>(null)
/** Where each thread's highlight starts and each change's section, from the top of the text (px). */
const tops = reactive<Record<string, number>>({})
const placed = reactive(new Set<string>())
const bars = ref<{ id: string; top: number; height: number }[]>([])
const heights = reactive<Record<string, number>>({})

// ------------------------------------------------------------------ highlights and bars
function relTop(el: Element) {
  return el.getBoundingClientRect().top - (wrap.value?.getBoundingClientRect().top ?? 0)
}
function paint() {
  const root = content.value
  if (!root) return
  clearMarks(root)
  placed.clear()
  const text = fullText(root)
  for (const t of open.value) {
    const at = locate(text, t)
    if (at < 0) continue
    const marks = markRange(root, at, t.quote.length, t.id)
    if (marks.length) placed.add(t.id)
  }
  measure()
}
function measure() {
  const root = content.value
  if (!root) return
  for (const t of open.value) {
    const mark = root.querySelector(`mark[data-thread="${t.id}"]`)
    if (mark) tops[t.id] = relTop(mark)
  }
  const out: { id: string; top: number; height: number }[] = []
  for (const c of changes.value) {
    const { head, end } = sectionElements(root, c.heading)
    const top = head ? relTop(head) : 0
    const bottom = end ? relTop(end) - 8 : root.getBoundingClientRect().bottom - (wrap.value?.getBoundingClientRect().top ?? 0)
    tops[c.seriesId] = top
    if (head && c.status === 'pending') out.push({ id: c.seriesId, top, height: Math.max(16, bottom - top) })
  }
  bars.value = out
  for (const m of root.querySelectorAll('mark[data-thread]')) m.classList.toggle('is-active', (m as HTMLElement).dataset.thread === active.value)
  requestAnimationFrame(sizeCards)
}
function sizeCards() {
  for (const el of margin.value?.querySelectorAll<HTMLElement>('[data-card]') ?? []) heights[el.dataset.card!] = el.offsetHeight
}
watch(
  () => [props.html, open.value.map(t => t.id).join(), changes.value.map(c => `${c.seriesId}:${c.version}:${c.status}`).join()],
  async () => {
    await nextTick()
    paint()
  },
  { flush: 'post' },
)
watch(active, () => measure())

// ------------------------------------------------------------------ the margin (computers)
/** Placed threads, changes, and the new comment, stacked beside their tops; threads whose text is gone below. */
const items = computed(() => {
  const list: { id: string; kind: 'thread' | 'change' | 'new'; top: number }[] = []
  for (const t of open.value) if (placed.has(t.id)) list.push({ id: t.id, kind: 'thread', top: tops[t.id] ?? 0 })
  for (const c of changes.value) list.push({ id: c.seriesId, kind: 'change', top: tops[c.seriesId] ?? 0 })
  if (composer.anchor && wide.value) list.push({ id: 'new', kind: 'new', top: composer.top })
  return list
})
const detached = computed(() => open.value.filter(t => !placed.has(t.id)))
/** Cards start below the margin's «Comentarios · n» line. */
const FIRST = 32
const layout = computed(() => stackCards(items.value.map(i => ({ id: i.id, top: Math.max(FIRST, i.top), height: heights[i.id] ?? 120 })), active.value))
const marginHeight = computed(() => Math.max(0, ...items.value.map(i => (layout.value[i.id] ?? 0) + (heights[i.id] ?? 120))))
const listMode = ref(false)
const threadOf = (id: string) => mine.value.threads.find(t => t.id === id)!
const changeOf = (id: string) => mine.value.changes.find(c => c.seriesId === id)!

function activate(id: string | null, { scroll = false } = {}) {
  active.value = id
  if (!id) return
  if (!wide.value) sheet.mode = changes.value.some(c => c.seriesId === id) ? 'change' : 'thread'
  if (!wide.value) sheet.id = id
  if (scroll) {
    const target = content.value?.querySelector(`mark[data-thread="${id}"]`) ?? (changeOf(id) && sectionElements(content.value!, changeOf(id).heading).head)
    target?.scrollIntoView({ behavior: 'smooth', block: 'center' })
  }
}
function onContentClick(event: MouseEvent) {
  const mark = (event.target as Element).closest?.('mark[data-thread]') as HTMLElement | null
  if (mark) activate(mark.dataset.thread!)
}

// ------------------------------------------------------------------ selecting and the new comment
const selection = ref<(Anchor & { rect: DOMRect | null }) | null>(null)
const composer = reactive({ anchor: null as Anchor | null, top: 0, text: '', busy: false })
const chip = computed(() => {
  const r = selection.value?.rect
  if (!wide.value || !r || composer.anchor) return null
  return { left: Math.min(window.innerWidth - 110, r.right + 6), top: Math.max(8, r.top - 34) }
})
function onSelection() {
  selection.value = content.value ? selectionAnchor(content.value) : null
}
function startComment() {
  const s = selection.value
  if (!s) return
  const { rect, ...anchor } = s
  composer.anchor = anchor
  composer.top = rect ? rect.top - (wrap.value?.getBoundingClientRect().top ?? 0) : 0
  composer.text = ''
  active.value = 'new'
  if (!wide.value) {
    sheet.mode = 'new'
    sheet.id = null
  }
  nextTick(() => document.querySelector<HTMLTextAreaElement>('[data-new-comment]')?.focus())
}
function cancelComment() {
  composer.anchor = null
  if (active.value === 'new') active.value = null
  if (sheet.mode === 'new') sheet.mode = null
}
async function saveComment() {
  if (!composer.anchor || !composer.text.trim()) return
  composer.busy = true
  try {
    const out = await act<{ thread: { id: string } }>('threads', 'POST', { path: props.entryId, ...composer.anchor, body: composer.text })
    composer.anchor = null
    window.getSelection()?.removeAllRanges()
    selection.value = null
    sheet.mode = null
    await nextTick()
    activate(out.thread.id)
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    composer.busy = false
  }
}

// ------------------------------------------------------------------ the bottom sheet (phones)
const sheet = reactive({ mode: null as 'thread' | 'change' | 'new' | 'list' | null, id: null as string | null })
const sheetThread = computed(() => (sheet.mode === 'thread' && sheet.id ? mine.value.threads.find(t => t.id === sheet.id) : null))
const sheetChange = computed(() => (sheet.mode === 'change' && sheet.id ? mine.value.changes.find(c => c.seriesId === sheet.id) : null))
const resolvedThreads = computed(() => mine.value.threads.filter(t => t.status === 'resolved'))
const allChanges = computed(() => mine.value.changes)
function closeSheet() {
  sheet.mode = null
  active.value = null
}
function pickFromList(id: string) {
  listMode.value = false
  activate(id, { scroll: true })
}

let resize: ResizeObserver | null = null
const media = typeof window !== 'undefined' ? window.matchMedia?.('(min-width: 1024px)') : null
const onMedia = () => (wide.value = !!media?.matches)
onMounted(() => {
  document.addEventListener('selectionchange', onSelection)
  media?.addEventListener?.('change', onMedia)
  if (typeof ResizeObserver !== 'undefined') {
    resize = new ResizeObserver(() => measure())
    if (content.value) resize.observe(content.value)
  }
  paint()
})
onUnmounted(() => {
  document.removeEventListener('selectionchange', onSelection)
  media?.removeEventListener?.('change', onMedia)
  resize?.disconnect()
})
defineExpose({ openList: () => (wide.value ? (listMode.value = true) : (sheet.mode = 'list')) })
</script>

<template>
  <div class="lg:flex lg:items-start lg:gap-6">
    <div ref="wrap" class="relative min-w-0 lg:max-w-[72ch] lg:flex-1">
      <!-- renderMarkdown escapes every text. -->
      <div ref="content" class="md" @click="onContentClick" v-html="html" />
      <span
        v-for="b in bars"
        :key="b.id"
        class="doc-change-bar"
        :style="{ top: `${b.top}px`, height: `${b.height}px` }"
        :title="$t('Cambio propuesto')"
        @click="activate(b.id)"
      />
    </div>

    <!-- The margin (computers): cards beside their highlights. -->
    <aside v-if="wide" ref="margin" class="relative w-80 shrink-0" :style="{ minHeight: `${marginHeight}px` }" @click.self="activate(null)">
      <div class="mb-2 flex justify-end">
        <button v-if="mine.threads.length || mine.changes.length" type="button" class="btn-ghost gap-1 px-2 py-0.5 text-xs" @click="listMode = !listMode">
          <MessageSquare :size="13" /> {{ listMode ? $t('Al margen') : $t('Comentarios · {n}', { n: total }) }}
        </button>
      </div>
      <template v-if="listMode">
        <div class="space-y-2">
          <DocChangeCard v-for="c in allChanges" :key="c.seriesId" :change="c" :admin="admin" @activate="pickFromList(c.seriesId)" @asked="chat => emit('asked', chat)" />
          <DocThreadCard v-for="t in open" :key="t.id" :thread="t" :detached="!placed.has(t.id)" :answered-by="answers[t.id]" @activate="pickFromList(t.id)" @quote="pickFromList(t.id)" @change="showChange" />
          <details v-if="resolvedThreads.length" class="text-sm">
            <summary class="cursor-pointer py-1 text-xs text-stone-500">{{ $t('Resueltos ({n})', { n: resolvedThreads.length }) }}</summary>
            <div class="mt-2 space-y-2">
              <DocThreadCard v-for="t in resolvedThreads" :key="t.id" :thread="t" :active="active === t.id" @activate="active = t.id" />
            </div>
          </details>
        </div>
      </template>
      <template v-else>
        <div v-for="i in items" :key="i.id" class="absolute inset-x-0 transition-[top] duration-150" :style="{ top: `${Math.max(FIRST, layout[i.id] ?? 0)}px` }">
          <DocThreadCard
            v-if="i.kind === 'thread'"
            :thread="threadOf(i.id)"
            :active="active === i.id"
            :answered-by="answers[i.id]"
            @activate="activate(i.id)"
            @quote="activate(i.id, { scroll: true })"
            @change="showChange"
          />
          <DocChangeCard
            v-else-if="i.kind === 'change'"
            :change="changeOf(i.id)"
            :admin="admin"
            :active="active === i.id"
            @activate="activate(i.id)"
            @section="activate(i.id, { scroll: true })"
            @asked="chat => emit('asked', chat)"
          />
          <form v-else data-card="new" class="doc-card doc-card-active rounded-lg border border-brand-300 bg-surface p-3 text-sm" @submit.prevent="saveComment">
            <p class="doc-quote mb-2 line-clamp-3 border-l-2 pl-2 text-xs text-stone-600">{{ composer.anchor?.quote }}</p>
            <textarea v-model="composer.text" data-new-comment rows="3" class="doc-box" :placeholder="$t('Comentario')" @keydown.esc="cancelComment" @keydown.ctrl.enter.prevent="saveComment" />
            <div class="mt-1 flex justify-end gap-2">
              <button type="button" class="btn-ghost px-2 py-0.5 text-xs" @click="cancelComment">{{ $t('Cancelar') }}</button>
              <button class="btn-primary px-2 py-0.5 text-xs" :disabled="composer.busy || !composer.text.trim()">{{ $t('Comentar') }}</button>
            </div>
          </form>
        </div>
        <div v-if="detached.length" class="absolute inset-x-0 space-y-2" :style="{ top: `${marginHeight + 48}px` }">
          <p class="text-xs text-stone-500">{{ $t('El texto cambió') }}</p>
          <DocThreadCard v-for="t in detached" :key="t.id" :thread="t" detached :active="active === t.id" :answered-by="answers[t.id]" @activate="activate(t.id)" @change="showChange" />
        </div>
      </template>
    </aside>

    <!-- «Comentar» by the selection (computers). -->
    <button
      v-if="chip"
      type="button"
      class="btn-primary fixed z-30 px-2.5 py-1 text-xs shadow-md"
      :style="{ left: `${chip.left}px`, top: `${chip.top}px` }"
      @mousedown.prevent
      @click="startComment"
    >
      {{ $t('Comentar') }}
    </button>

    <!-- Phones: a bar above the bottom edge, and a sheet with a thread, a change, a new comment or the list. -->
    <template v-if="!wide">
      <div class="fixed inset-x-0 bottom-0 z-30 flex items-center gap-2 border-t border-stone-200 bg-surface px-4 py-2" style="padding-bottom: max(0.5rem, env(safe-area-inset-bottom))">
        <button type="button" class="btn gap-1 py-1 text-sm" @click="sheet.mode = 'list'">
          <MessageSquare :size="14" /> {{ $t('Comentarios · {n}', { n: total }) }}
        </button>
        <button v-if="selection && !sheet.mode" type="button" class="btn-primary ml-auto py-1 text-sm" @mousedown.prevent @click="startComment">{{ $t('Comentar') }}</button>
      </div>
      <div class="h-14" aria-hidden="true" />
      <div v-if="sheet.mode" class="fixed inset-0 z-40 bg-black/30" @click="closeSheet" />
      <section
        v-if="sheet.mode"
        class="fixed inset-x-0 bottom-0 z-50 flex max-h-[80dvh] flex-col rounded-t-xl border-t border-stone-200 bg-canvas shadow-2xl"
        role="dialog"
        :aria-label="$t('Comentarios')"
      >
        <div class="flex items-center justify-between px-4 pt-3 pb-1">
          <span class="text-sm font-medium text-stone-700">{{ sheet.mode === 'list' ? $t('Comentarios · {n}', { n: total }) : sheet.mode === 'new' ? $t('Comentario') : '' }}</span>
          <button type="button" class="btn-ghost" :aria-label="$t('Cerrar')" @click="closeSheet"><X :size="16" /></button>
        </div>
        <div class="min-h-0 flex-1 overflow-y-auto px-4 pb-4">
          <DocThreadCard v-if="sheetThread" :thread="sheetThread" active :answered-by="answers[sheetThread.id]" @change="showChange" />
          <DocChangeCard v-else-if="sheetChange" :change="sheetChange" :admin="admin" active @asked="chat => emit('asked', chat)" />
          <form v-else-if="sheet.mode === 'new'" @submit.prevent="saveComment">
            <p class="doc-quote mb-2 line-clamp-3 border-l-2 pl-2 text-xs text-stone-600">{{ composer.anchor?.quote }}</p>
            <textarea v-model="composer.text" data-new-comment rows="3" class="doc-box" :placeholder="$t('Comentario')" />
            <div class="mt-2 flex justify-end gap-2">
              <button type="button" class="btn py-1 text-sm" @click="cancelComment">{{ $t('Cancelar') }}</button>
              <button class="btn-primary py-1 text-sm" :disabled="composer.busy || !composer.text.trim()">{{ $t('Comentar') }}</button>
            </div>
          </form>
          <div v-else-if="sheet.mode === 'list'" class="space-y-2">
            <p v-if="!mine.threads.length && !mine.changes.length" class="py-4 text-sm text-stone-500">{{ $t('Sin comentarios') }}</p>
            <button v-for="c in allChanges" :key="c.seriesId" type="button" class="block w-full rounded-md border border-stone-200 bg-surface p-2 text-left text-sm doc-change-edge" @click="sheet.mode = 'change'; sheet.id = c.seriesId">
              <span class="doc-claude-tag">Claude</span> § {{ c.heading || $t('Inicio') }}
              <span v-if="c.status !== 'pending'" class="text-xs text-stone-500"> · {{ c.status === 'applied' ? $t('Aplicado') : c.status === 'undone' ? $t('Deshecho') : '' }}</span>
            </button>
            <button v-for="t in open" :key="t.id" type="button" class="block w-full rounded-md border border-stone-200 bg-surface p-2 text-left text-sm" @click="activate(t.id, { scroll: true }); sheet.mode = 'thread'; sheet.id = t.id">
              <span class="line-clamp-2 border-l-2 pl-2 text-xs" :class="placed.has(t.id) ? 'doc-quote text-stone-600' : 'border-stone-300 text-stone-400'">{{ t.quote }}</span>
              <span class="mt-1 line-clamp-2 block text-stone-800">{{ t.comments[0]?.body }}</span>
              <span class="text-xs text-stone-500">{{ t.comments[0]?.author }}<template v-if="t.comments.length > 1"> · {{ $tn(t.comments.length - 1, '{n} respuesta', '{n} respuestas') }}</template></span>
            </button>
            <details v-if="resolvedThreads.length">
              <summary class="cursor-pointer py-1 text-xs text-stone-500">{{ $t('Resueltos ({n})', { n: resolvedThreads.length }) }}</summary>
              <button v-for="t in resolvedThreads" :key="t.id" type="button" class="mt-2 block w-full rounded-md border border-stone-200 bg-surface p-2 text-left text-sm opacity-80" @click="sheet.mode = 'thread'; sheet.id = t.id">
                <span class="line-clamp-2 border-l-2 border-stone-300 pl-2 text-xs text-stone-500">{{ t.quote }}</span>
                <span class="mt-1 line-clamp-2 block text-stone-700">{{ t.comments[0]?.body }}</span>
              </button>
            </details>
          </div>
        </div>
      </section>
    </template>
  </div>
</template>
