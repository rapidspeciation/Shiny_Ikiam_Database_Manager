<script setup lang="ts">
// Identificar → Descargas: everything the phone keeps for use without signal, in one place, reachable before any
// photo: the identifier (either model), the team's sex model (signed in), and the photo pack (PackPanel). Each item
// says its size before downloading; what is on the phone says its size, with an update when the server has a newer
// one, and its removal. The view (views/IdentifyView.vue) holds the models' state and does the work.
import { Check, Cpu, VenusAndMars } from 'lucide-vue-next'
import type { ModelKind } from '../../lib/offlineId'
import PackPanel from './PackPanel.vue'

export interface ModelRow {
  kind: ModelKind
  label: string
  /** On the phone, the version the server lists (or no list known: offline). */
  ready: boolean
  /** On the phone, but the server lists another version. */
  outdated: boolean
  /** MB to download, MB it takes. */
  downloadMb: number
  takesMb: number
  recommended: boolean
}
export interface SexRow {
  ready: boolean
  outdated: boolean
  downloadMb: number
  takesMb: number
  progress: { done: number; total: number } | null
  error: string | null
}
defineProps<{
  models: ModelRow[]
  active: ModelKind | null
  download: { kind: ModelKind; done: number; total: number; retrying?: string } | null
  error: string | null
  lacksSpace: { need: number; free: number } | null
  iosNote: boolean
  fromHf: boolean
  sex: SexRow | null
  online: boolean
}>()
const emit = defineEmits<{ download: [kind: ModelKind]; use: [kind: ModelKind]; remove: [kind: ModelKind]; sexDownload: []; sexRemove: [] }>()
const mb = (b: number) => Math.max(1, Math.round(b / 1048576))
const pct = (p: { done: number; total: number }) => Math.round((100 * p.done) / Math.max(1, p.total))
</script>

<template>
  <div class="max-w-2xl space-y-4" data-testid="downloads">
    <h1 class="text-xl font-semibold">{{ $t('Descargas') }}</h1>

    <!-- The identifier -->
    <section class="rounded-xl border-2 border-border-strong border-l-[6px] border-l-brand bg-surface p-3 sm:p-4" data-testid="dl-models">
      <div class="flex items-start gap-3">
        <Cpu :size="22" class="mt-0.5 shrink-0 text-brand-text" />
        <div class="min-w-0 flex-1">
          <h2 class="text-lg font-semibold">{{ $t('Identificador') }}</h2>
          <p class="text-sm text-fg-muted">{{ $t('Para identificar sin internet. Uno basta.') }}</p>
        </div>
      </div>
      <ul class="mt-3 space-y-2">
        <li
          v-for="m in models"
          :key="m.kind"
          class="rounded-lg border-2 p-3"
          :class="m.ready ? 'border-success-text/50 bg-brand-soft' : 'border-border bg-surface'"
          :data-testid="`dl-model-${m.kind}`"
        >
          <div class="flex flex-wrap items-baseline justify-between gap-x-2">
            <p class="font-medium">
              {{ m.label }}
              <span v-if="m.recommended && !m.ready" class="ml-1 rounded bg-brand-soft px-1.5 py-0.5 text-xs font-medium text-brand-text">{{ $t('recomendado') }}</span>
              <span v-if="m.ready && active === m.kind" class="ml-1 rounded bg-brand px-1.5 py-0.5 text-xs font-medium text-on-brand">{{ $t('en uso') }}</span>
            </p>
            <p class="text-sm text-fg-muted tabular-nums">
              <template v-if="m.ready || m.outdated"><Check :size="14" class="mr-0.5 inline text-success-text" />{{ $t('{mb} MB en el teléfono', { mb: m.takesMb }) }}</template>
              <template v-else>{{ $t('{mb} MB', { mb: m.downloadMb }) }}</template>
            </p>
          </div>
          <div v-if="download?.kind === m.kind" class="mt-2" role="status">
            <div class="h-2.5 overflow-hidden rounded-full bg-surface-sunken" role="progressbar" :aria-valuenow="pct(download)" aria-valuemin="0" aria-valuemax="100">
              <div class="h-full bg-brand transition-[width]" :style="{ width: `${pct(download)}%` }" />
            </div>
            <p class="mt-1 text-sm text-fg-muted tabular-nums">
              {{ download.retrying ? $t('Se cortó la conexión; reintentando…') : $t('{done} de {total} MB', { done: mb(download.done), total: mb(download.total) }) }}
            </p>
          </div>
          <div v-else class="mt-2 flex flex-wrap gap-2">
            <button v-if="!m.ready && !m.outdated" class="btn-primary min-h-11 px-4" :disabled="!!download || !online" :data-testid="`dl-get-${m.kind}`" @click="emit('download', m.kind)">
              {{ $t('Descargar') }} · {{ m.downloadMb }} MB
            </button>
            <button v-if="m.outdated" class="btn-primary min-h-11 px-4" :disabled="!!download || !online" @click="emit('download', m.kind)">
              {{ $t('Actualizar · {mb} MB', { mb: m.downloadMb }) }}
            </button>
            <button v-if="m.ready && active !== m.kind" class="btn min-h-11 px-4" @click="emit('use', m.kind)">{{ $t('Usar este') }}</button>
            <button v-if="m.ready || m.outdated" class="btn-danger min-h-11 px-4" :disabled="!!download" @click="emit('remove', m.kind)">{{ $t('Borrar') }}</button>
          </div>
        </li>
      </ul>
      <p v-if="iosNote" class="mt-3 rounded-md bg-amber-50 p-3 text-sm text-amber-950">
        {{ $t('En iPhone, primero agregue la app a la pantalla de inicio (Compartir → Agregar a inicio) y descargue desde el ícono: lo descargado en Safari no se ve desde el ícono.') }}
      </p>
      <p v-if="lacksSpace" class="mt-3 rounded-md bg-amber-50 p-3 text-sm text-amber-950">{{ $t('Hacen falta unos {need} MB libres; este teléfono tiene {free} MB.', lacksSpace) }}</p>
      <p v-if="error" class="mt-3 rounded-md bg-cell-error p-3 text-sm text-error-text" role="alert">{{ error }}</p>
      <p v-if="!online" class="mt-2 text-xs text-fg-muted">{{ $t('Hace falta conexión a internet para descargar.') }}</p>
      <p v-else-if="fromHf" class="mt-2 text-xs text-fg-subtle">{{ $t('Gratis y sin cuenta: los modelos se descargan de Hugging Face y quedan en este teléfono.') }}</p>
    </section>

    <!-- The team's sex model -->
    <section v-if="sex" class="rounded-xl border-2 border-border-strong border-l-[6px] border-l-fill-pink bg-surface p-3 sm:p-4" data-testid="dl-sex">
      <div class="flex items-start gap-3">
        <VenusAndMars :size="22" class="mt-0.5 shrink-0 text-fill-pink" />
        <div class="min-w-0 flex-1">
          <h2 class="text-lg font-semibold">{{ $t('Sexo estimado') }}</h2>
          <p class="text-sm text-fg-muted">{{ $t('Experimental, solo fotos en mano. Solo para el equipo.') }}</p>
        </div>
        <p class="shrink-0 text-sm text-fg-muted tabular-nums">
          <template v-if="sex.ready || sex.outdated"><Check :size="14" class="mr-0.5 inline text-success-text" />{{ $t('{mb} MB en el teléfono', { mb: sex.takesMb }) }}</template>
          <template v-else>{{ $t('{mb} MB', { mb: sex.downloadMb }) }}</template>
        </p>
      </div>
      <div v-if="sex.progress" class="mt-2" role="status">
        <div class="h-2.5 overflow-hidden rounded-full bg-surface-sunken" role="progressbar" :aria-valuenow="pct(sex.progress)" aria-valuemin="0" aria-valuemax="100">
          <div class="h-full bg-brand transition-[width]" :style="{ width: `${pct(sex.progress)}%` }" />
        </div>
        <p class="mt-1 text-sm text-fg-muted tabular-nums">{{ $t('{done} de {total} MB', { done: mb(sex.progress.done), total: mb(sex.progress.total) }) }}</p>
      </div>
      <div v-else class="mt-2 flex flex-wrap gap-2">
        <button v-if="!sex.ready && !sex.outdated" class="btn-primary min-h-11 px-4" :disabled="!!download || !online" data-testid="sex-download" @click="emit('sexDownload')">
          {{ $t('Descargar') }} · {{ sex.downloadMb }} MB
        </button>
        <button v-if="sex.outdated" class="btn-primary min-h-11 px-4" :disabled="!!download || !online" @click="emit('sexDownload')">{{ $t('Actualizar · {mb} MB', { mb: sex.downloadMb }) }}</button>
        <button v-if="sex.ready || sex.outdated" class="btn-danger min-h-11 px-4" @click="emit('sexRemove')">{{ $t('Borrar') }}</button>
      </div>
      <p v-if="sex.error" class="mt-2 text-sm text-error-text" role="alert">{{ sex.error }}</p>
    </section>

    <PackPanel :online="online" />

    <details class="rounded-lg border border-border bg-surface p-3 text-sm">
      <summary class="min-h-6 cursor-pointer font-medium">{{ $t('Instalar en el teléfono') }}</summary>
      <p class="mt-1 text-fg-muted">{{ $t('Android: menú ⋮ → «Instalar app». iPhone: Compartir → «Agregar a inicio», y abra la app desde el ícono.') }}</p>
    </details>
  </div>
</template>
