<script setup lang="ts">
// Descargas: «Nueva versión disponible» for one model (DownloadsPanel): the offered and the installed version with
// their sizes when they are named (not a content hash), and the model's own note (its manifest's measured facts).
import { RefreshCw } from 'lucide-vue-next'
import type { Versions } from './DownloadsPanel.vue'

defineProps<{ versions?: Versions; note?: string | null }>()
/** A version worth showing: a name from the model's metadata, not a content hash (sha256-…). */
const named = (v: string | null | undefined): v is string => !!v && !v.startsWith('sha256-')
</script>

<template>
  <p class="mt-2 flex items-start gap-1.5 rounded-md bg-brand-soft px-2.5 py-2 text-sm">
    <RefreshCw :size="15" class="mt-0.5 shrink-0 text-brand-text" />
    <span>
      <span class="font-medium">{{ $t('Nueva versión disponible') }}</span>
      <span v-if="named(versions?.to)" class="block text-xs text-fg-muted tabular-nums" data-testid="update-versions">
        <span class="block">{{ versions?.toMb ? $t('Nueva: {v}, {mb} MB.', { v: versions.to!, mb: versions.toMb }) : $t('Nueva: {to}.', { to: versions!.to! }) }}</span>
        <span v-if="named(versions?.from)" class="block">
          {{ versions?.fromMb ? $t('Instalada: {v}, {mb} MB.', { v: versions.from!, mb: versions.fromMb }) : $t('Instalada: {v}.', { v: versions!.from! }) }}
        </span>
        <span v-else-if="versions?.fromMb" class="block">{{ $t('Instalada: {mb} MB, sin versión registrada.', { mb: versions.fromMb }) }}</span>
      </span>
      <span v-if="note" class="mt-1 block text-xs text-fg-muted" data-testid="update-note">{{ note }}</span>
    </span>
  </p>
</template>
