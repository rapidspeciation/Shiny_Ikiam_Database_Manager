<script setup lang="ts">
import { ref } from 'vue'
import type { ProposalPage } from '../../lib/proposals'

/**
 * The notebook photos above a table (a page's proposal, or a table of rows
 * read from the photos): per photo, its thumbnail, the assistant's words on
 * why it is there and its lines in the table. A click on a thumbnail opens
 * «Revisar con la foto» on it (`photo`); Ctrl/⌘ or the middle button, the
 * photo upright in a new tab. A proposal says how many lines change, how many
 * are as the sheet has them and how many are not written; a table, how many rows.
 */
const props = defineProps<{
  /** The proposal's (or table's) id: its photos' address. */
  id: string
  page: Pick<ProposalPage, 'photos' | 'photoKey' | 'photoNotes'>
  photos: { photo: number; from: number; to: number; change?: number; same?: number; other?: number; rows?: number }[]
  /** Shown beside its photo already (PhotoReview): a thumbnail picks the photo there. */
  reviewing?: boolean
}>()
const emit = defineEmits<{ photo: [n: number] }>()

const note = (n: number) => props.page.photoNotes?.[n] ?? ''
const url = (n: number, size: 'thumb' | 'view') =>
  `api/proposals/${props.id}/photos/${n}?size=${size}${props.page.photoKey ? `&v=${props.page.photoKey}` : ''}`
/** A thumbnail that would not load (an old proposal's photo gone): hidden. */
const broken = ref(new Set<number>())
function open(n: number, e: MouseEvent) {
  if (e.ctrlKey || e.metaKey || e.shiftKey || e.button !== 0) return
  e.preventDefault()
  emit('photo', n)
}
</script>

<template>
  <div class="flex flex-wrap gap-2 px-2 pt-1.5">
    <div
      v-for="p in photos"
      :key="p.photo"
      class="flex items-center gap-2 rounded border border-stone-200 bg-stone-50 py-1 pr-2 pl-1 text-[11px] text-stone-600"
      data-photo-thumb
    >
      <a
        v-if="p.photo < page.photos && !broken.has(p.photo)"
        :href="url(p.photo, 'view')"
        target="_blank"
        rel="noopener"
        class="shrink-0"
        :title="reviewing ? $t('Ver esta foto') : $t('Revisar con esta foto (Ctrl+clic: en una pestaña nueva)')"
        @click="open(p.photo, $event)"
      >
        <img
          :src="url(p.photo, 'thumb')"
          :alt="$t('Foto {n} del cuaderno', { n: p.photo + 1 })"
          class="h-14 w-auto max-w-24 rounded border border-stone-300 bg-white object-contain"
          loading="lazy"
          @error="broken = new Set([...broken, p.photo])"
        />
      </a>
      <span>
        <b v-if="photos.length > 1 || note(p.photo)" class="font-medium text-stone-700">{{ $t('Foto {n}', { n: p.photo + 1 }) }}</b>
        <template v-if="note(p.photo)"> · {{ note(p.photo) }}</template>
        <template v-if="p.to"
          ><template v-if="photos.length > 1 || note(p.photo)"> · </template>{{ $t('Líneas {from}–{to}', { from: p.from, to: p.to }) }} ·
          <template v-if="p.rows !== undefined">{{ $tn(p.rows, '{n} fila', '{n} filas') }}</template>
          <template v-else>
            {{ $tn(p.change ?? 0, '{n} cambia', '{n} cambian') }} · {{ $tn(p.same ?? 0, '{n} igual', '{n} iguales') }}
            <template v-if="p.other"> · {{ $tn(p.other, '{n} sin escribir', '{n} sin escribir') }}</template>
          </template>
        </template>
        <template v-else-if="p.rows"
          ><template v-if="photos.length > 1 || note(p.photo)"> · </template>{{ $tn(p.rows, '{n} fila', '{n} filas') }}</template
        >
      </span>
    </div>
  </div>
</template>
