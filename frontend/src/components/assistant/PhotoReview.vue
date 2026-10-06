<script setup lang="ts">
import { computed, defineAsyncComponent, onBeforeUnmount, onMounted, ref } from 'vue'
import { Image as ImageIcon, PanelBottom, PanelRight, X } from 'lucide-vue-next'
import type { Proposal } from '../../lib/proposals'
import { panelShare } from '../../lib/proposals'
import { persistentRef } from '../../lib/persist'
import PhotoViewer from './PhotoViewer.vue'

const ProposalGrid = defineAsyncComponent(() => import('../ProposalGrid.vue'))
const RowsTable = defineAsyncComponent(() => import('./RowsTable.vue'))

/**
 * «Revisar con la foto»: a proposal's table and its notebook photo together,
 * over the whole browser tab: the table on top and the photo below, or side
 * by side on a wide screen, each scrolled on its own, the divider dragged to
 * share the room (kept in this browser). Selecting a row in the table brings
 * its photo and says its line; the table works as on its card (edits, Aplicar).
 * A table of rows the assistant shows (show_rows) read from notebook photos
 * comes the same way, still only to read.
 */
const props = defineProps<{ proposal: Proposal; busy?: boolean; photo: number }>()
const emit = defineEmits<{
  close: []
  'update:photo': [n: number]
  apply: [indexes: number[], revision: number | undefined, doubtful?: 'confirm' | 'skip']
  discard: []
  replace: [proposal: Proposal]
}>()

const layout = persistentRef<'below' | 'beside'>('proposal-review:layout', 'below', { lasting: true })
const shares = persistentRef('proposal-review:share', { below: 50, beside: 50 }, { lasting: true })
/** Side by side only on a wide screen; a phone always has the photo below. */
const wide = ref(typeof matchMedia !== 'undefined' && matchMedia('(min-width: 768px)').matches)
const media = typeof matchMedia !== 'undefined' ? matchMedia('(min-width: 768px)') : null
const onMedia = (e: MediaQueryListEvent) => (wide.value = e.matches)
const beside = computed(() => wide.value && layout.value === 'beside')
const split = ref<HTMLElement>()
const dragging = ref<number | null>(null)
/** The table's share of the room, in %. */
const share = computed(() => dragging.value ?? shares.value[beside.value ? 'beside' : 'below'])
function resize(down: PointerEvent) {
  const el = split.value
  if (!el || down.button !== 0) return
  down.preventDefault()
  const handle = down.currentTarget as HTMLElement
  handle.setPointerCapture(down.pointerId)
  const r = el.getBoundingClientRect()
  const at = (e: PointerEvent) =>
    beside.value ? panelShare(e.clientX, r.left, r.width, false) : panelShare(e.clientY, r.top, r.height, false)
  const move = (e: PointerEvent) => (dragging.value = at(e))
  const up = () => {
    handle.removeEventListener('pointermove', move)
    handle.removeEventListener('pointerup', up)
    handle.removeEventListener('pointercancel', up)
    if (dragging.value !== null) shares.value = { ...shares.value, [beside.value ? 'beside' : 'below']: dragging.value }
    dragging.value = null
  }
  handle.addEventListener('pointermove', move)
  handle.addEventListener('pointerup', up)
  handle.addEventListener('pointercancel', up)
}

const photos = computed(() => props.proposal.page?.photos ?? 0)
const url = (n: number, size: 'thumb' | 'view') => `api/proposals/${props.proposal.id}/photos/${n}?size=${size}${props.proposal.page?.photoKey ? `&v=${props.proposal.page.photoKey}` : ''}`
/** The line of the row selected in the table (on the photo shown). */
const line = ref<number | null>(null)
function onRow(photo: number | null, at: number | null) {
  if (photo === null || photo >= photos.value) return (line.value = null)
  if (photo !== props.photo) emit('update:photo', photo)
  line.value = at
}
function pick(n: number) {
  line.value = null
  emit('update:photo', n)
}

/** Esc closes it (not while typing in a cell or a box). */
function onKey(e: KeyboardEvent) {
  if (e.key !== 'Escape' || e.defaultPrevented) return
  const target = e.target as HTMLElement | null
  if (target?.closest('input, textarea, select, [contenteditable], .tabulator')) return
  emit('close')
}
onMounted(() => {
  media?.addEventListener('change', onMedia)
  window.addEventListener('keydown', onKey)
})
onBeforeUnmount(() => {
  media?.removeEventListener('change', onMedia)
  window.removeEventListener('keydown', onKey)
})
</script>

<template>
  <div class="fixed inset-0 z-40 flex flex-col bg-stone-50" role="dialog" :aria-label="$t('Revisar con la foto')">
    <header class="flex items-center gap-1.5 border-b border-stone-200 bg-white px-3 py-1.5">
      <ImageIcon :size="15" class="shrink-0 text-emerald-700" />
      <h2 class="shrink-0 text-sm font-medium">{{ $t('Revisar con la foto') }}</h2>
      <span class="min-w-0 truncate text-xs text-stone-500">{{ proposal.reason }}</span>
      <span class="ml-auto hidden items-center gap-0.5 md:flex">
        <button
          type="button"
          class="btn-ghost"
          :class="{ 'bg-stone-100 text-emerald-800': !beside }"
          :title="$t('La foto debajo de la tabla')"
          :aria-label="$t('La foto debajo de la tabla')"
          @click="layout = 'below'"
        >
          <PanelBottom :size="15" />
        </button>
        <button
          type="button"
          class="btn-ghost"
          :class="{ 'bg-stone-100 text-emerald-800': beside }"
          :title="$t('La foto al lado de la tabla')"
          :aria-label="$t('La foto al lado de la tabla')"
          @click="layout = 'beside'"
        >
          <PanelRight :size="15" />
        </button>
      </span>
      <button type="button" class="btn-ghost max-md:ml-auto" :title="$t('Cerrar (Esc)')" :aria-label="$t('Cerrar')" @click="emit('close')">
        <X :size="15" />
      </button>
    </header>
    <div ref="split" class="flex min-h-0 flex-1" :class="beside ? 'flex-row' : 'flex-col'" :style="{ '--share': `${share}%` }">
      <!-- A size container: the table is at most its height (ProposalSheet), so its column names stay in sight. -->
      <div class="min-h-0 min-w-0 shrink-0 grow-0 basis-(--share) overflow-y-auto px-2 pb-2 [container-type:size]">
        <RowsTable v-if="proposal.kind === 'table'" :table="proposal" @close="emit('discard')" @row="onRow" />
        <ProposalGrid
          v-else
          :proposal="proposal"
          :busy="busy"
          reviewing
          @apply="(indexes, at, doubtful) => emit('apply', indexes, at, doubtful)"
          @discard="emit('discard')"
          @replace="p => emit('replace', p)"
          @photo="pick"
          @row="onRow"
        />
      </div>
      <div
        class="divider shrink-0 touch-none bg-stone-300 hover:bg-emerald-500"
        :class="[beside ? 'w-1.5 cursor-col-resize' : 'h-1.5 cursor-row-resize', { 'bg-emerald-600': dragging !== null }]"
        role="separator"
        :aria-orientation="beside ? 'vertical' : 'horizontal'"
        :aria-valuenow="share"
        :title="$t('Arrastra para repartir el espacio entre la tabla y la foto')"
        @pointerdown="resize"
      />
      <div class="min-h-0 min-w-0 flex-1">
        <PhotoViewer :url="url" :count="photos" :photo="photo" :line="line" @update:photo="pick" />
      </div>
    </div>
  </div>
</template>

<style scoped>
/* A bigger target for a finger, the line itself thin. */
.divider {
  position: relative;
}
.divider::after {
  content: '';
  position: absolute;
  inset: -6px;
}
</style>
