<script setup lang="ts">
// Identificar → Descargas: everything the phone keeps for use without signal, in one place, reachable before any
// photo: the identifier (either model), the team's sex model (signed in), and the photo pack (PackPanel). Each item
// says its size before downloading; what is on the phone says its size, and «Nueva versión disponible» with the new
// size and «Actualizar» when the server lists a newer version (the installed one keeps working until the new one is
// downloaded and checked), and its removal. The view (views/IdentifyView.vue) holds the models' state and does the work.
import { Check, Cpu, VenusAndMars } from 'lucide-vue-next'
import type { ModelKind } from '../../lib/offlineId'
import PackPanel from './PackPanel.vue'
import UpdateNotice from './UpdateNotice.vue'

/** The installed and the offered version (null: not known, e.g. a model kept before versions existed). */
export interface Versions {
  from: string | null
  to: string | null
  /** The installed and the offered file's size in MB (the sex model: its size changes with the model). */
  fromMb?: number | null
  toMb?: number | null
}
export interface ModelRow {
  kind: ModelKind
  label: string
  /** On the phone and usable (whatever its version). */
  ready: boolean
  /** On the phone, and the server lists a newer version: «Nueva versión disponible». */
  outdated: boolean
  /** MB to download (the update's when outdated), MB it takes. */
  downloadMb: number
  takesMb: number
  recommended: boolean
  versions?: Versions
  /** The model's own note (its manifest's measured facts), in the page's language; null: none. */
  note?: string | null
}
export interface SexRow {
  /** How many genera the model covers (its calibration's). */
  genera: number
  ready: boolean
  outdated: boolean
  downloadMb: number
  takesMb: number
  progress: { done: number; total: number } | null
  error: string | null
  versions?: Versions
  /** The model's own note (its manifest's: measured cost, speed or memory), in the page's language; null: none. */
  note?: string | null
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
/** MB as phones and the model reports count them (10^6 bytes). */
const mb = (b: number) => Math.max(1, Math.round(b / 1e6))
const pct = (p: { done: number; total: number }) => Math.round((100 * p.done) / Math.max(1, p.total))
/** MB done so far (0 at the start, not the 1 MB floor of a size). */
const mbDone = (b: number) => Math.round(b / 1e6)
/** All bytes in: the file is being checked (its SHA-256) before it replaces the installed one. */
const checking = (p: { done: number; total: number }) => p.total > 1 && p.done >= p.total
/** A version worth showing: a name from the model's metadata, not a content hash (the identifier's files). */
const named = (v: string | null | undefined): v is string => !!v && !v.startsWith('sha256-')
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
            <p v-if="m.ready || m.outdated" class="text-sm text-fg-muted tabular-nums">
              <Check :size="14" class="mr-0.5 inline text-success-text" />{{ $t('{mb} MB en el teléfono', { mb: m.takesMb }) }}
            </p>
          </div>
          <div v-if="download?.kind === m.kind" class="mt-2" role="status">
            <div class="h-2.5 overflow-hidden rounded-full bg-surface-sunken" role="progressbar" :aria-valuenow="pct(download)" aria-valuemin="0" aria-valuemax="100">
              <div class="h-full bg-brand transition-[width]" :style="{ width: `${pct(download)}%` }" />
            </div>
            <p class="mt-1 text-sm text-fg-muted tabular-nums">
              {{
                download.retrying
                  ? $t('Se cortó la conexión; reintentando…')
                  : download.total <= 1
                    ? $t('Preparando la descarga…')
                    : checking(download)
                      ? $t('Comprobando la descarga…')
                      : $t('{done} de {total} MB', { done: mbDone(download.done), total: mb(download.total) })
              }}
            </p>
            <p v-if="m.outdated" class="mt-1 text-xs text-fg-subtle">{{ $t('Mientras tanto se sigue usando la versión instalada.') }}</p>
          </div>
          <UpdateNotice v-if="m.outdated && download?.kind !== m.kind" :versions="m.versions" :note="m.note" :data-testid="`dl-update-${m.kind}`" />
          <!-- A model's note compares it with the version before: shown only with the update (UpdateNotice). -->
          <p v-else-if="m.ready && named(m.versions?.from) && download?.kind !== m.kind" class="mt-1 text-xs text-fg-subtle" :data-testid="`dl-version-${m.kind}`">
            {{ $t('Versión {v}', { v: m.versions!.from! }) }}
          </p>
          <div v-if="download?.kind !== m.kind" class="mt-2 flex flex-wrap gap-2">
            <button v-if="!m.ready && !m.outdated" class="btn-primary min-h-11 px-4" :disabled="!!download || !online" :data-testid="`dl-get-${m.kind}`" @click="emit('download', m.kind)">
              {{ $t('Descargar') }} · {{ m.downloadMb }} MB
            </button>
            <button v-if="m.outdated" class="btn-primary min-h-11 px-4" :disabled="!!download || !online" :data-testid="`dl-refresh-${m.kind}`" @click="emit('download', m.kind)">
              {{ $t('Actualizar · {mb} MB', { mb: m.downloadMb }) }}
            </button>
            <button v-if="m.ready && active !== m.kind" class="btn min-h-11 px-4" @click="emit('use', m.kind)">{{ $t('Usar este') }}</button>
            <button v-if="m.ready || m.outdated" class="btn-danger min-h-11 px-4" :disabled="!!download" @click="emit('remove', m.kind)">{{ $t('Borrar del teléfono') }}</button>
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
          <p class="text-sm text-fg-muted">{{ $t('Experimental, fotos en mano de {n} géneros de Ithomiini. Solo para el equipo.', { n: sex.genera }) }}</p>
          <!-- Under the text, not beside it: at 360 px a column beside it squeezed the description to a few words a line. -->
          <p v-if="sex.ready || sex.outdated" class="mt-1 text-sm text-fg-muted tabular-nums">
            <Check :size="14" class="mr-0.5 inline text-success-text" />{{ $t('{mb} MB en el teléfono', { mb: sex.takesMb }) }}
          </p>
        </div>
      </div>
      <div v-if="sex.progress" class="mt-2" role="status">
        <div class="h-2.5 overflow-hidden rounded-full bg-surface-sunken" role="progressbar" :aria-valuenow="pct(sex.progress)" aria-valuemin="0" aria-valuemax="100">
          <div class="h-full bg-brand transition-[width]" :style="{ width: `${pct(sex.progress)}%` }" />
        </div>
        <p class="mt-1 text-sm text-fg-muted tabular-nums">
          {{
            sex.progress.total <= 1
              ? $t('Preparando la descarga…')
              : checking(sex.progress)
                ? $t('Comprobando la descarga…')
                : $t('{done} de {total} MB', { done: mbDone(sex.progress.done), total: mb(sex.progress.total) })
          }}
        </p>
        <p v-if="sex.outdated" class="mt-1 text-xs text-fg-subtle">{{ $t('Mientras tanto se sigue usando la versión instalada.') }}</p>
      </div>
      <UpdateNotice v-if="sex.outdated && !sex.progress" :versions="sex.versions" :note="sex.note" data-testid="sex-update" />
      <p v-else-if="sex.ready && !sex.progress && named(sex.versions?.from)" class="mt-1 text-xs text-fg-subtle" data-testid="sex-version">{{ $t('Versión {v}', { v: sex.versions!.from! }) }}</p>
      <div v-if="!sex.progress" class="mt-2 flex flex-wrap gap-2">
        <button v-if="!sex.ready && !sex.outdated" class="btn-primary min-h-11 px-4" :disabled="!!download || !online" data-testid="sex-download" @click="emit('sexDownload')">
          {{ $t('Descargar') }} · {{ sex.downloadMb }} MB
        </button>
        <button v-if="sex.outdated" class="btn-primary min-h-11 px-4" :disabled="!!download || !online" data-testid="sex-refresh" @click="emit('sexDownload')">
          {{ $t('Actualizar · {mb} MB', { mb: sex.downloadMb }) }}
        </button>
        <button v-if="sex.ready || sex.outdated" class="btn-danger min-h-11 px-4" @click="emit('sexRemove')">{{ $t('Borrar del teléfono') }}</button>
      </div>
      <p v-if="sex.error" class="mt-2 text-sm text-error-text" role="alert">{{ sex.error }}</p>
    </section>

    <PackPanel :online="online" />

    <details class="rounded-lg border border-border bg-surface p-3 text-sm">
      <summary class="-my-1 cursor-pointer py-3 font-medium">{{ $t('Instalar en el teléfono') }}</summary>
      <p class="mt-1 text-fg-muted">{{ $t('Android: menú ⋮ → «Instalar app». iPhone: Compartir → «Agregar a inicio», y abra la app desde el ícono.') }}</p>
    </details>
  </div>
</template>
