<script setup lang="ts">
import { computed, nextTick, onMounted, onUnmounted, reactive, ref, watch } from 'vue'
import { ChevronLeft, MessageSquare, X } from 'lucide-vue-next'
import DocThreadCard from './DocThreadCard.vue'
import DocChangeCard from './DocChangeCard.vue'
import {
  act,
  answeredBy,
  changeTitle,
  clearMarks,
  forEntry,
  fullText,
  locate,
  markRange,
  review,
  sectionElements,
  selectionAnchor,
  shownInMargin,
  snapToWords,
  stackCards,
  type Anchor,
} from '../../lib/docReview'
import { errorText, notify } from '../../lib/notice'
import { useKeyboard } from '../../composables/usePhone'
import { t } from '../../lib/i18n'

/**
 * A Markdown file of the instructions with its comments: the text with the quoted passages highlighted and
 * a green bar beside each section a change is proposed for; on computers a margin of cards beside their
 * highlights, on phones a bottom sheet. Selecting text offers «Comentar». Highlights and bars take the
 * keyboard too (Tab, Enter).
 */
const props = defineProps<{ entryId: string; html: string }>()
const emit = defineEmits<{ asked: [chat: string | null] }>()

const content = ref<HTMLElement>()
const wrap = ref<HTMLElement>()
const margin = ref<HTMLElement>()
const sheetEl = ref<HTMLElement>()
const sheetBody = ref<HTMLElement>()
const wide = ref(typeof window !== 'undefined' && window.matchMedia?.('(min-width: 1024px)').matches)
/** The phone's keyboard: the sheet and the bar sit above it (Chrome on Android, composables/usePhone.ts). */
const keyboard = useKeyboard()
const admin = computed(() => review.state?.admin ?? false)
const mine = computed(() => forEntry(review.state, props.entryId))
const open = computed(() => mine.value.threads.filter(x => x.status === 'open'))
const changes = computed(() => mine.value.changes.filter(c => shownInMargin(c)))
const pendingChanges = computed(() => mine.value.changes.filter(c => c.status === 'pending'))
const answers = computed(() => answeredBy(review.state))
/** The comments a change answers, for the quote line on its card. */
const quotesOf = (seriesId: string) =>
  (changeOf(seriesId)?.threadIds ?? [])
    .map(id => mine.value.threads.find(x => x.id === id) ?? review.state?.threads.find(x => x.id === id))
    .filter((x): x is NonNullable<typeof x> => !!x)
    // The question it answers, as its first person wrote it (the highlighted words are already in the diff).
    .map(x => {
      const first = x.comments.find(c => c.kind === 'person' && !c.deleted)
      return { id: x.id, quote: first ? `${first.author}: ${first.body}` : x.quote }
    })
/** The quote line on a change card: its thread becomes the active card (on phones, its sheet). */
function showThread(id: string) {
  listMode.value = false
  activate(id, { scroll: true, fromList: sheet.fromList })
}
/** The quote in a phone thread sheet: the sheet closes and the passage shows, highlighted. */
function revealQuote(id: string) {
  closeSheet()
  nextTick(() => {
    active.value = id
    const target = targetOf(id)
    if (target) {
      scrollToFraction(target, 0.3)
      ;(target as HTMLElement).focus?.({ preventScroll: true })
    }
  })
}
/** Resolved from a sheet opened from the list: back to the list. */
function afterResolve() {
  if (sheet.fromList) void backToList()
}
const total = computed(() => open.value.length + pendingChanges.value.length)
/** «Comentarios · n», or «Comentarios» when nothing is waiting. */
const listLabel = computed(() => (total.value ? t('Comentarios · {n}', { n: total.value }) : t('Comentarios')))

const active = ref<string | null>(null)
/** Where each thread's highlight starts and each change's section, from the top of the text (px). */
const tops = reactive<Record<string, number>>({})
const placed = reactive(new Set<string>())
const bars = ref<{ id: string; top: number; height: number; label: string }[]>([])
const heights = reactive<Record<string, number>>({})

// ------------------------------------------------------------------ highlights and bars
function relTop(el: Element) {
  return el.getBoundingClientRect().top - (wrap.value?.getBoundingClientRect().top ?? 0)
}
/** A highlight's name for screen readers: whose comment it is. */
function markName(id: string) {
  const first = mine.value.threads.find(x => x.id === id)?.comments.find(c => c.kind === 'person')
  return first?.author ? t('Comentario de {name}', { name: first.author }) : t('Comentario')
}
function paint() {
  const root = content.value
  if (!root) return
  clearMarks(root)
  placed.clear()
  const text = fullText(root)
  for (const th of open.value) {
    const at = locate(text, th)
    if (at < 0) continue
    const marks = markRange(root, at, th.quote.length, th.id)
    // One keyboard stop per thread: its first mark.
    if (marks[0]) {
      marks[0].tabIndex = 0
      marks[0].setAttribute('role', 'button')
      marks[0].setAttribute('aria-label', markName(th.id))
      placed.add(th.id)
    }
  }
  // Phones: a small «Cambio» tag beside each heading with a change waiting (the bar alone is easy to miss).
  if (!wide.value)
    for (const c of pendingChanges.value) {
      const { head } = sectionElements(root, c.heading)
      if (!head) continue
      const tag = document.createElement('button')
      tag.type = 'button'
      tag.className = 'doc-change-tag'
      tag.dataset.docUi = ''
      tag.dataset.change = c.seriesId
      tag.textContent = t('Cambio')
      head.append(tag)
    }
  measure()
}
function measure() {
  const root = content.value
  if (!root) return
  for (const th of open.value) {
    const mark = root.querySelector(`mark[data-thread="${th.id}"]`)
    if (mark) tops[th.id] = relTop(mark)
  }
  const out: { id: string; top: number; height: number; label: string }[] = []
  for (const c of changes.value) {
    const { head, end } = sectionElements(root, c.heading)
    const top = head ? relTop(head) : 0
    const bottom = end ? relTop(end) - 8 : root.getBoundingClientRect().bottom - (wrap.value?.getBoundingClientRect().top ?? 0)
    tops[c.seriesId] = top
    if (head && c.status === 'pending')
      out.push({ id: c.seriesId, top, height: Math.max(16, bottom - top), label: t('Cambio propuesto: § {heading}', { heading: c.heading || t('Inicio') }) })
  }
  bars.value = out
  for (const m of root.querySelectorAll('mark[data-thread]')) m.classList.toggle('is-active', (m as HTMLElement).dataset.thread === active.value)
  requestAnimationFrame(sizeCards)
}
function sizeCards() {
  for (const el of margin.value?.querySelectorAll<HTMLElement>('[data-card]') ?? []) heights[el.dataset.card!] = el.offsetHeight
}
watch(
  () => [
    props.html,
    wide.value,
    open.value.map(x => `${x.id}:${x.comments.length}`).join(),
    changes.value.map(c => `${c.seriesId}:${c.version}:${c.status}`).join(),
  ],
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
  for (const th of open.value) if (placed.has(th.id)) list.push({ id: th.id, kind: 'thread', top: tops[th.id] ?? 0 })
  for (const c of changes.value) list.push({ id: c.seriesId, kind: 'change', top: tops[c.seriesId] ?? 0 })
  if (composer.anchor && wide.value) list.push({ id: 'new', kind: 'new', top: composer.top })
  return list
})
const detached = computed(() => open.value.filter(x => !placed.has(x.id)))
/** Cards start below the margin's «Comentarios · n» line. */
const FIRST = 32
const layout = computed(() => stackCards(items.value.map(i => ({ id: i.id, top: i.top, height: heights[i.id] ?? 120 })), active.value, 8, FIRST))
const marginHeight = computed(() => Math.max(0, ...items.value.map(i => (layout.value[i.id] ?? 0) + (heights[i.id] ?? 120))))
const listMode = ref(false)
const threadOf = (id: string) => mine.value.threads.find(x => x.id === id)!
const changeOf = (id: string) => mine.value.changes.find(c => c.seriesId === id)

/** The nearest scrolling ancestor of an element (the page's text pane), or null. */
function scroller(el: Element): HTMLElement | null {
  for (let p = el.parentElement; p; p = p.parentElement) if (/(auto|scroll)/.test(getComputedStyle(p).overflowY) && p.scrollHeight > p.clientHeight) return p
  return null
}
/** Scrolls an element to `fraction` of its pane's height from the top (phones: above the sheet). */
function scrollToFraction(el: Element, fraction: number) {
  const pane = scroller(el)
  if (!pane) return el.scrollIntoView?.({ block: 'start' })
  const top = el.getBoundingClientRect().top - pane.getBoundingClientRect().top
  pane.scrollTo?.({ top: pane.scrollTop + top - pane.clientHeight * fraction, behavior: 'smooth' })
}
function targetOf(id: string): Element | null {
  const mark = content.value?.querySelector(`mark[data-thread="${id}"]`)
  if (mark) return mark
  const c = changeOf(id)
  return c && content.value ? sectionElements(content.value, c.heading).head : null
}
function activate(id: string | null, { scroll = false, fromList = false } = {}) {
  active.value = id
  if (!id) return
  if (!wide.value) {
    if (!sheet.mode) opener = document.activeElement as HTMLElement | null
    if (sheet.mode === 'list' && fromList) sheet.listScroll = sheetBody.value?.scrollTop ?? 0
    sheet.fromList = fromList
    sheet.mode = changeOf(id) ? 'change' : 'thread'
    sheet.id = id
  }
  const target = targetOf(id)
  if (!target) return
  if (!wide.value) scrollToFraction(target, 0.15)
  else if (scroll) {
    target.scrollIntoView?.({ behavior: 'smooth', block: 'center' })
    afterScroll(target, () => fitActive(id, target))
  } else if (typeof requestAnimationFrame !== 'undefined') requestAnimationFrame(() => requestAnimationFrame(() => fitActive(id, target)))
}
/** Runs `done` once the pane's smooth scroll has stopped (scrollend, or no scroll event for 150 ms). */
function afterScroll(target: Element, done: () => void) {
  const pane = scroller(target)
  if (!pane) return done()
  let timer = setTimeout(finish, 400)
  function finish() {
    pane!.removeEventListener('scroll', moved)
    pane!.removeEventListener('scrollend', finish)
    clearTimeout(timer)
    done()
  }
  function moved() {
    clearTimeout(timer)
    timer = setTimeout(finish, 150)
  }
  pane.addEventListener('scroll', moved)
  pane.addEventListener('scrollend', finish, { once: true })
}
/**
 * Computers: an active card pushed below the fold (cards above it took its place) comes into view, as far as its
 * highlight stays on screen too.
 */
function fitActive(id: string, target: Element) {
  const card = margin.value?.querySelector(`[data-card="${id}"]`)
  const pane = card && scroller(card)
  if (!card || !pane) return
  const view = pane.getBoundingClientRect()
  const below = card.getBoundingClientRect().bottom - view.bottom + 16
  const room = target.getBoundingClientRect().top - view.top - 16
  const by = Math.min(below, room)
  if (by > 0) pane.scrollBy?.({ top: by, behavior: 'smooth' })
}
/** «Ver cambio» on a thread: its change becomes the active card (on phones, the sheet shows it). */
function showChange(seriesId: string) {
  listMode.value = false
  activate(seriesId, { scroll: true, fromList: sheet.fromList })
}
function anchorOf(target: EventTarget | null): string | null {
  const el = target as Element | null
  const tag = el?.closest?.('[data-change]') as HTMLElement | null
  if (tag) return tag.dataset.change!
  const mark = el?.closest?.('mark[data-thread]') as HTMLElement | null
  return mark?.dataset.thread ?? null
}
function onContentClick(event: MouseEvent) {
  const id = anchorOf(event.target)
  if (id) activate(id)
}
function onContentKey(event: KeyboardEvent) {
  if (event.key !== 'Enter' && event.key !== ' ') return
  const id = anchorOf(event.target)
  if (!id) return
  event.preventDefault()
  openByKeyboard(id)
}
/** Opened from the keyboard: on computers the card's first box takes the focus. */
function openByKeyboard(id: string) {
  activate(id)
  if (wide.value) nextTick(() => margin.value?.querySelector<HTMLElement>(`[data-card="${id}"] textarea, [data-card="${id}"] button`)?.focus())
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
  const { rect, ...picked } = s
  // A touch selection may cut a word: the quote takes whole words.
  const coarse = typeof window !== 'undefined' && window.matchMedia?.('(pointer: coarse)').matches
  const anchor = coarse && content.value ? { ...picked, ...snapToWords(fullText(content.value), picked) } : picked
  composer.anchor = anchor
  composer.top = rect ? rect.top - (wrap.value?.getBoundingClientRect().top ?? 0) : 0
  composer.text = ''
  active.value = 'new'
  if (!wide.value) {
    opener = document.activeElement as HTMLElement | null
    sheet.mode = 'new'
    sheet.id = null
    sheet.fromList = false
  }
  nextTick(() => document.querySelector<HTMLTextAreaElement>('[data-new-comment]')?.focus())
}
function cancelComment() {
  composer.anchor = null
  if (active.value === 'new') active.value = null
  if (sheet.mode === 'new') closeSheet()
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
const sheet = reactive({
  mode: null as 'thread' | 'change' | 'new' | 'list' | null,
  id: null as string | null,
  /** Opened from the list: its header has «‹ Comentarios», back to the list where it was. */
  fromList: false,
  listScroll: 0,
})
/** Where the focus was when the sheet opened: it goes back there when the sheet closes. */
let opener: HTMLElement | null = null
const sheetThread = computed(() => (sheet.mode === 'thread' && sheet.id ? mine.value.threads.find(x => x.id === sheet.id) : null))
const sheetChange = computed(() => (sheet.mode === 'change' && sheet.id ? mine.value.changes.find(c => c.seriesId === sheet.id) : null))
const resolvedThreads = computed(() => mine.value.threads.filter(x => x.status === 'resolved'))
const allChanges = computed(() => mine.value.changes)
function openList() {
  if (wide.value) {
    listMode.value = true
    return
  }
  if (!sheet.mode) opener = document.activeElement as HTMLElement | null
  sheet.mode = 'list'
  sheet.fromList = false
}
async function backToList() {
  sheet.mode = 'list'
  sheet.fromList = false
  active.value = null
  await nextTick()
  if (sheetBody.value) sheetBody.value.scrollTop = sheet.listScroll
}
function closeSheet() {
  sheet.mode = null
  sheet.fromList = false
  active.value = null
  composer.anchor = null
  const back = opener
  opener = null
  nextTick(() => back?.isConnected && back.focus())
}
function pickFromList(id: string) {
  listMode.value = false
  activate(id, { scroll: true })
}
/** The sheet keeps the focus while open (Tab cycles inside it) and closes on Escape. */
function onSheetKey(event: KeyboardEvent) {
  if (event.key === 'Escape') {
    event.preventDefault()
    closeSheet()
    return
  }
  if (event.key !== 'Tab' || !sheetEl.value) return
  const stops = [...sheetEl.value.querySelectorAll<HTMLElement>('button:not([disabled]), textarea, [href], [tabindex]:not([tabindex="-1"])')]
  if (!stops.length) return
  const first = stops[0]
  const last = stops[stops.length - 1]
  if (event.shiftKey && document.activeElement === first) {
    event.preventDefault()
    last.focus()
  } else if (!event.shiftKey && document.activeElement === last) {
    event.preventDefault()
    first.focus()
  }
}
watch(
  () => [sheet.mode, sheet.id],
  async ([mode]) => {
    if (!mode) return
    await nextTick()
    if (!sheetEl.value?.contains(document.activeElement)) sheetEl.value?.querySelector<HTMLElement>('[data-sheet-start]')?.focus()
  },
)
const sheetStyle = computed(() => ({ bottom: `${keyboard.cover.value}px`, maxHeight: `calc(80dvh - ${keyboard.cover.value}px)` }))

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
defineExpose({ openList })
</script>

<template>
  <div class="lg:flex lg:items-start lg:gap-6">
    <div ref="wrap" class="relative min-w-0 lg:max-w-[72ch] lg:flex-1">
      <!-- renderMarkdown escapes every text. -->
      <div ref="content" class="md" @click="onContentClick" @keydown="onContentKey" v-html="html" />
      <span
        v-for="b in bars"
        :key="b.id"
        class="doc-change-bar"
        role="button"
        tabindex="0"
        :aria-label="b.label"
        :title="b.label"
        :style="{ top: `${b.top}px`, height: `${b.height}px` }"
        @click="activate(b.id)"
        @keydown.enter.prevent="openByKeyboard(b.id)"
        @keydown.space.prevent="openByKeyboard(b.id)"
      />
    </div>

    <!-- The margin (computers): cards beside their highlights. -->
    <aside v-if="wide" ref="margin" class="doc-margin relative w-80 shrink-0" :style="{ minHeight: `${marginHeight}px` }" @click.self="activate(null)">
      <div class="mb-2 flex justify-end">
        <button v-if="mine.threads.length || mine.changes.length" type="button" class="btn-ghost gap-1 px-2 py-0.5 text-xs" @click="listMode = !listMode">
          <MessageSquare :size="13" /> {{ listMode ? $t('Al margen') : listLabel }}
        </button>
      </div>
      <template v-if="listMode">
        <div class="space-y-2">
          <DocChangeCard
            v-for="c in allChanges"
            :key="c.seriesId"
            :change="c"
            :admin="admin"
            :quotes="quotesOf(c.seriesId)"
            @activate="pickFromList(c.seriesId)"
            @asked="chat => emit('asked', chat)"
            @thread="showThread"
          />
          <DocThreadCard
            v-for="x in open"
            :key="x.id"
            :thread="x"
            :detached="!placed.has(x.id)"
            :answered-by="answers[x.id]"
            @activate="pickFromList(x.id)"
            @quote="pickFromList(x.id)"
            @change="showChange"
          />
          <details v-if="resolvedThreads.length" class="text-sm">
            <summary class="cursor-pointer py-1 text-xs text-stone-500">{{ $t('Resueltos ({n})', { n: resolvedThreads.length }) }}</summary>
            <div class="mt-2 space-y-2">
              <DocThreadCard v-for="x in resolvedThreads" :key="x.id" :thread="x" :active="active === x.id" @activate="active = x.id" />
            </div>
          </details>
        </div>
      </template>
      <template v-else>
        <div v-for="i in items" :key="i.id" class="absolute inset-x-0 transition-[top] duration-150" :class="{ 'z-[1]': active === i.id }" :style="{ top: `${layout[i.id] ?? FIRST}px` }">
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
            v-else-if="i.kind === 'change' && changeOf(i.id)"
            :change="changeOf(i.id)!"
            :admin="admin"
            :active="active === i.id"
            :quotes="quotesOf(i.id)"
            @activate="activate(i.id)"
            @section="activate(i.id, { scroll: true })"
            @asked="chat => emit('asked', chat)"
            @thread="showThread"
          />
          <form v-else-if="i.kind === 'new'" data-card="new" class="doc-card doc-card-active rounded-lg border border-brand-300 bg-surface p-3 text-sm" @submit.prevent="saveComment">
            <p class="doc-quote mb-2 line-clamp-3 border-l-2 pl-2 text-xs text-stone-600">{{ composer.anchor?.quote }}</p>
            <textarea
              v-model="composer.text"
              data-new-comment
              rows="3"
              class="doc-box"
              :placeholder="$t('Comentario')"
              @keydown.esc="cancelComment"
              @keydown.ctrl.enter.prevent="saveComment"
            />
            <div class="mt-1 flex justify-end gap-2">
              <button type="button" class="btn-ghost px-2 py-0.5 text-xs" @click="cancelComment">{{ $t('Cancelar') }}</button>
              <button class="btn-primary px-2 py-0.5 text-xs" :disabled="composer.busy || !composer.text.trim()">{{ $t('Comentar') }}</button>
            </div>
          </form>
        </div>
        <div v-if="detached.length" class="absolute inset-x-0 space-y-2" :style="{ top: `${marginHeight + 48}px` }">
          <p class="text-xs text-stone-500">{{ $t('El texto cambió') }}</p>
          <DocThreadCard v-for="x in detached" :key="x.id" :thread="x" detached :active="active === x.id" :answered-by="answers[x.id]" @activate="activate(x.id)" @change="showChange" />
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

    <!-- Phones: a bar above the bottom edge (and the keyboard), and a sheet with a thread, a change, a new comment or the list. -->
    <template v-if="!wide">
      <div
        class="fixed inset-x-0 z-30 flex items-center gap-2 border-t border-stone-200 bg-surface px-4 py-2"
        :style="{ bottom: `${keyboard.cover.value}px`, paddingBottom: keyboard.cover.value ? '0.5rem' : 'max(0.5rem, env(safe-area-inset-bottom))' }"
      >
        <button type="button" class="doc-sheet-btn btn gap-1 py-1 text-sm" @click="openList">
          <MessageSquare :size="14" /> {{ listLabel }}
        </button>
        <button v-if="selection && !sheet.mode" type="button" class="doc-sheet-btn btn-primary ml-auto py-1 text-sm" @mousedown.prevent @click="startComment">
          {{ $t('Comentar') }}
        </button>
      </div>
      <div class="h-16" aria-hidden="true" />
      <div v-if="sheet.mode" class="fixed inset-0 z-40 bg-black/30" aria-hidden="true" @click="closeSheet" />
      <section
        v-if="sheet.mode"
        ref="sheetEl"
        class="doc-sheet fixed inset-x-0 z-50 flex flex-col rounded-t-xl border-t border-stone-200 bg-canvas shadow-2xl"
        :style="sheetStyle"
        role="dialog"
        aria-modal="true"
        :aria-label="$t('Comentarios')"
        @keydown="onSheetKey"
      >
        <div class="flex items-center justify-between gap-2 px-2 pt-2 pb-1">
          <button
            v-if="sheet.fromList && sheet.mode !== 'list'"
            type="button"
            class="doc-sheet-btn btn-ghost gap-0.5 text-sm font-medium"
            data-sheet-start
            @click="backToList"
          >
            <ChevronLeft :size="18" /> {{ $t('Comentarios') }}
          </button>
          <span v-else class="px-2 text-sm font-medium text-stone-700">{{ sheet.mode === 'list' ? listLabel : sheet.mode === 'new' ? $t('Comentario') : '' }}</span>
          <button
            type="button"
            class="doc-sheet-close btn-ghost"
            :aria-label="$t('Cerrar')"
            :data-sheet-start="sheet.fromList && sheet.mode !== 'list' ? undefined : ''"
            @click="closeSheet"
          >
            <X :size="18" />
          </button>
        </div>
        <div ref="sheetBody" class="min-h-0 flex-1 overflow-y-auto px-4 pb-4">
          <DocThreadCard
            v-if="sheetThread"
            :thread="sheetThread"
            active
            :answered-by="answers[sheetThread.id]"
            @change="showChange"
            @quote="revealQuote(sheetThread.id)"
            @resolved="afterResolve"
          />
          <DocChangeCard
            v-else-if="sheetChange"
            :change="sheetChange"
            :admin="admin"
            active
            :quotes="quotesOf(sheetChange.seriesId)"
            @asked="chat => emit('asked', chat)"
            @thread="showThread"
          />
          <form v-else-if="sheet.mode === 'new'" @submit.prevent="saveComment">
            <p class="doc-quote mb-2 line-clamp-3 border-l-2 pl-2 text-xs text-stone-600">{{ composer.anchor?.quote }}</p>
            <textarea v-model="composer.text" data-new-comment rows="3" class="doc-box" :placeholder="$t('Comentario')" />
            <div class="mt-2 flex justify-end gap-3">
              <button type="button" class="doc-sheet-btn btn py-1 text-sm" @click="cancelComment">{{ $t('Cancelar') }}</button>
              <button class="doc-sheet-btn btn-primary py-1 text-sm" :disabled="composer.busy || !composer.text.trim()">{{ $t('Comentar') }}</button>
            </div>
          </form>
          <div v-else-if="sheet.mode === 'list'" class="space-y-2">
            <p v-if="!mine.threads.length && !mine.changes.length" class="py-4 text-sm text-stone-500">{{ $t('Sin comentarios') }}</p>
            <button
              v-for="c in allChanges"
              :key="c.seriesId"
              type="button"
              class="doc-change-edge block w-full rounded-md border border-stone-200 bg-surface p-2 text-left text-sm"
              @click="activate(c.seriesId, { fromList: true })"
            >
              <span class="doc-claude-tag">Claude</span> {{ changeTitle(c, t) }}
              <span v-if="c.status !== 'pending'" class="text-xs text-stone-500"> · {{ c.status === 'applied' ? $t('Aplicado') : c.status === 'undone' ? $t('Deshecho') : '' }}</span>
            </button>
            <button
              v-for="x in open"
              :key="x.id"
              type="button"
              class="block w-full rounded-md border border-stone-200 bg-surface p-2 text-left text-sm"
              @click="activate(x.id, { fromList: true })"
            >
              <span class="line-clamp-2 border-l-2 pl-2 text-xs" :class="placed.has(x.id) ? 'doc-quote text-stone-600' : 'border-stone-300 text-stone-400'">{{ x.quote }}</span>
              <span class="mt-1 line-clamp-2 block text-stone-800">{{ x.comments[0]?.body }}</span>
              <span class="text-xs text-stone-500"
                >{{ x.comments[0]?.author }}<template v-if="x.comments.length > 1"> · {{ $tn(x.comments.length - 1, '{n} respuesta', '{n} respuestas') }}</template></span
              >
            </button>
            <details v-if="resolvedThreads.length">
              <summary class="cursor-pointer py-2 text-sm text-stone-500">{{ $t('Resueltos ({n})', { n: resolvedThreads.length }) }}</summary>
              <button
                v-for="x in resolvedThreads"
                :key="x.id"
                type="button"
                class="mt-2 block w-full rounded-md border border-stone-200 bg-surface p-2 text-left text-sm opacity-80"
                @click="activate(x.id, { fromList: true })"
              >
                <span class="line-clamp-2 border-l-2 border-stone-300 pl-2 text-xs text-stone-500">{{ x.quote }}</span>
                <span class="mt-1 line-clamp-2 block text-stone-700">{{ x.comments[0]?.body }}</span>
              </button>
            </details>
          </div>
        </div>
      </section>
    </template>
  </div>
</template>
