<script setup lang="ts">
/**
 * «Mis identificaciones» (this phone only, lib/identifyLog.ts and lib/identifyHistory.ts): each item loads into the
 * panel when tapped, as if the photo had just been added (its result, its sex, the sex to send). «Reprocesar» when a
 * newer identifier or sex model reads photos now than the one that read it; copy or download the photo. Items kept
 * before photos were kept (no photo) show their line only.
 */
import { Copy, Download, RefreshCw, RotateCcw } from 'lucide-vue-next'
import PhotoThumb from './PhotoThumb.vue'
import { percent } from '../../lib/identifyResult'
import { locale } from '../../lib/i18n'
import type { LogEntry } from '../../lib/identifyLog'

defineProps<{ entries: (LogEntry & { outdated: boolean })[]; current: string | null; busy: boolean }>()
defineEmits<{ load: [at: string]; reprocess: [at: string]; copy: [at: string]; download: [at: string]; forget: [] }>()
const when = (iso: string) => new Date(iso).toLocaleString(locale.value === 'es' ? 'es' : 'en', { day: 'numeric', month: 'short', hour: '2-digit', minute: '2-digit' })
const SEX = { female: '♀', male: '♂', unsure: '' } as const
</script>

<template>
  <details class="rounded-lg border border-border bg-surface p-3 text-sm" data-testid="my-identifications">
    <summary class="cursor-pointer font-medium">{{ $t('Mis identificaciones') }} <span class="font-normal text-fg-muted">({{ entries.length }})</span></summary>
    <ul class="mt-2 divide-y divide-border">
      <li v-for="e in entries" :key="e.at" class="flex items-center gap-2 py-1.5" :class="e.at === current ? 'rounded-md bg-brand-soft' : ''" data-testid="my-identification">
        <button
          v-if="e.saved"
          type="button"
          class="flex min-w-0 flex-1 items-center gap-3 rounded-md py-0.5 pl-1 text-left hover:bg-surface-sunken disabled:cursor-wait"
          :disabled="busy"
          :aria-label="$t('Ver {name}', { name: e.pick ?? e.ai })"
          data-testid="history-load"
          @click="$emit('load', e.at)"
        >
          <PhotoThumb :src="e.thumb" :size="48" :label="e.pick ?? e.ai" :selected="e.at === current" inert />
          <span class="min-w-0 flex-1">
            <i class="font-medium">{{ e.pick ?? e.ai }}</i>
            <span class="block text-xs text-fg-muted">
              {{ when(e.at) }} · {{ percent(e.p) }}<template v-if="e.pick && e.pick !== e.ai"> · {{ $t('la IA sugería {ai}', { ai: e.ai }) }}</template><template v-if="e.sex && e.sex.call !== 'unsure'"> · <span class="whitespace-nowrap">{{ SEX[e.sex.call] }}<template v-if="e.sex.pct != null"> {{ e.sex.pct }} %</template></span></template>
            </span>
          </span>
        </button>
        <span v-else class="flex min-w-0 flex-1 items-center gap-3 pl-1">
          <img v-if="e.thumb" :src="e.thumb" alt="" class="size-12 rounded-md object-cover" />
          <span class="min-w-0 flex-1">
            <i>{{ e.pick ?? e.ai }}</i>
            <span class="block text-xs text-fg-muted">{{ when(e.at) }} · {{ $t('foto no guardada') }}</span>
          </span>
        </span>
        <template v-if="e.saved">
          <button v-if="e.outdated" class="btn min-h-11 shrink-0 px-2" :disabled="busy" data-testid="history-reprocess" :title="$t('Leer de nuevo con el modelo actual')" @click="$emit('reprocess', e.at)">
            <RefreshCw :size="15" /><span class="hidden sm:inline">{{ $t('Reprocesar') }}</span><span class="sr-only sm:hidden">{{ $t('Reprocesar') }}</span>
          </button>
          <button class="btn-ghost size-11 shrink-0" :aria-label="$t('Copiar la foto')" :title="$t('Copiar la foto')" data-testid="history-copy" @click="$emit('copy', e.at)"><Copy :size="16" /></button>
          <button class="btn-ghost size-11 shrink-0" :aria-label="$t('Descargar la foto')" :title="$t('Descargar la foto')" data-testid="history-download" @click="$emit('download', e.at)"><Download :size="16" /></button>
        </template>
      </li>
    </ul>
    <button class="btn mt-3" @click="$emit('forget')"><RotateCcw :size="14" />{{ $t('Borrar la lista') }}</button>
  </details>
</template>
