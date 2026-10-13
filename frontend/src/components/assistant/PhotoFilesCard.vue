<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, ref, watch } from 'vue'
import { TabulatorFull as Tabulator } from 'tabulator-tables'
import type { CellComponent, ColumnDefinition } from 'tabulator-tables'
import 'tabulator-tables/dist/css/tabulator_simple.min.css'
import { AlertTriangle, Check, ChevronDown, ChevronRight, FolderCog, X } from 'lucide-vue-next'
import { api } from '../../lib/api'
import { errorText, notify } from '../../lib/notice'
import { locale, t } from '../../lib/i18n'
import type { Proposal } from '../../lib/proposals'
import { fileChangeLabel, fileCounts, fileTarget, progressPercent, type FileRow, type PhotoFiles } from '../../lib/photoLinks'

/**
 * Changes to make to the photo files on Google Drive (the assistant's organize_photos), as a card
 * of Cambios propuestos: how many files are renamed, moved, binned or marked …_todelete (the
 * files of another account, for their owner to delete), the folders and shortcuts made, what was
 * left out and why, and every change in a table. «Aplicar» makes them with the project's Google
 * account, a bar showing how far it is; Photo_links then follows the files changed. The
 * assistant takes a plan's changes back when asked.
 */
const props = defineProps<{ proposal: Proposal }>()
const emit = defineEmits<{ discard: []; started: [] }>()

const plan = computed(() => (props.proposal.sync ?? { total: 0, counts: {}, raw: 0, problems: { count: 0, examples: [] } }) as PhotoFiles)
const status = computed(() => props.proposal.status)
const pending = computed(() => status.value === 'pending')
const progress = computed(() => plan.value.progress)
const base = computed(() => `chat/proposals/${props.proposal.id}/photo-links`)

const busy = ref(false)
async function apply() {
  busy.value = true
  try {
    await api(`${base.value}/apply`, { method: 'POST', body: {} })
    emit('started')
  } catch (e) {
    notify((e as { code?: string }).code === 'stale' ? t('Este plan es de hace más de media hora: pídeselo de nuevo al asistente') : errorText(e), 'error')
  } finally {
    busy.value = false
  }
}

// ------------------------------------------------------------ every change
const showRows = ref(false)
const rows = ref<FileRow[] | null>(null)
const rowsGone = ref(false)
const host = ref<HTMLDivElement>()
let grid: Tabulator | null = null
let built = false
function columns(): ColumnDefinition[] {
  return [
    { title: t('Cambio'), field: 'change', width: 130, formatter: (c: CellComponent) => fileChangeLabel(c.getValue()) },
    { title: t('Carpeta'), field: 'folder', minWidth: 160 },
    {
      title: t('Archivo'),
      field: 'name',
      width: 210,
      formatter: (c: CellComponent) => {
        const row = c.getData() as FileRow
        if (!row.url) return row.name
        const link = document.createElement('a')
        Object.assign(link, { href: row.url, target: '_blank', rel: 'noopener', textContent: row.name, className: 'photo-link' })
        return link
      },
    },
    { title: '→', field: 'to', minWidth: 180, formatter: (c: CellComponent) => fileTarget(c.getData() as FileRow) },
    {
      title: t('Nota'),
      field: 'raw',
      headerSort: false,
      formatter: (c: CellComponent) => {
        const row = c.getData() as FileRow
        return [row.raw ? t('RAW de su foto') : '', row.copy ? t('el mismo archivo está en otra carpeta') : ''].filter(Boolean).join(' · ')
      },
    },
  ] as ColumnDefinition[]
}
async function draw() {
  if (!showRows.value) return
  if (!rows.value && !rowsGone.value)
    try {
      rows.value = (await api<{ rows: FileRow[] }>(`${base.value}/rows`)).rows
    } catch (e) {
      if ((e as { code?: string }).code === 'stale') rowsGone.value = true
      else notify(errorText(e), 'error')
    }
  await nextTick()
  if (!host.value || !rows.value || !showRows.value || grid) return
  const made = (grid = new Tabulator(host.value, {
    data: rows.value,
    columns: columns(),
    layout: 'fitDataStretch',
    maxHeight: '22rem',
    placeholder: t('Sin filas'),
    columnDefaults: { headerSort: true, resizable: 'header' },
  } as unknown as ConstructorParameters<typeof Tabulator>[1]))
  made.on('tableBuilt', () => {
    if (grid === made) built = true
  })
}
function dropGrid() {
  built = false
  grid?.destroy()
  grid = null
}
watch(showRows, on => (on ? void draw() : dropGrid()))
watch(locale, () => built && grid?.setColumns(columns()))
watch(pending, on => {
  if (on) return
  showRows.value = false
  dropGrid()
})
onBeforeUnmount(dropGrid)
</script>

<template>
  <div class="mt-2 rounded-md border border-stone-300 bg-white text-sm" data-photo-files>
    <p class="flex flex-wrap items-center gap-x-2 gap-y-1 border-b border-stone-200 px-3 py-2">
      <FolderCog :size="15" class="shrink-0 text-emerald-700" />
      <strong class="font-medium">{{ $t('Cambios en los archivos de fotos de Google Drive') }}</strong>
      <span class="text-xs text-stone-600">{{ fileCounts(plan.counts) }}</span>
      <span v-if="plan.raw" class="text-xs text-stone-500">{{ $tn(plan.raw, '({n} es el RAW de su foto)', '({n} son los RAW de sus fotos)') }}</span>
    </p>

    <!-- Being made: how far it is. -->
    <div v-if="status === 'applying'" class="px-3 py-3" data-progress>
      <p class="mb-1.5 text-xs text-stone-700">
        <template v-if="progress?.phase === 'sheet'">{{ $t('Archivos listos. Actualizando Photo_links…') }}</template>
        <template v-else>
          {{ $t('Cambiando archivos en Google Drive: {written} de {total}', { written: progress?.written ?? 0, total: progress?.total ?? plan.total }) }}
        </template>
      </p>
      <div
        class="h-2.5 overflow-hidden rounded-full bg-stone-200"
        role="progressbar"
        aria-valuemin="0"
        aria-valuemax="100"
        :aria-valuenow="progressPercent(progress)"
      >
        <div class="h-full rounded-full bg-emerald-600 transition-[width] duration-500" :style="{ width: `${progressPercent(progress)}%` }" />
      </div>
    </div>

    <!-- Made. -->
    <div v-else-if="status === 'applied'" class="space-y-1 px-3 py-2 text-xs" data-done>
      <p class="flex items-start gap-1.5 text-emerald-800">
        <Check :size="14" class="mt-px shrink-0" />
        <span>
          {{ $tn(progress?.written ?? 0, '{n} archivo cambiado en Google Drive.', '{n} archivos cambiados en Google Drive.') }}
          <template v-if="progress?.sheetRows">
            {{ $tn(progress.sheetRows, 'Photo_links: {n} fila actualizada (en Historial).', 'Photo_links: {n} filas actualizadas (en Historial).') }}
          </template>
          {{ $t('El asistente puede deshacer estos cambios si se lo pides.') }}
        </span>
      </p>
      <p v-if="progress?.sheetNote" class="text-amber-900">{{ progress.sheetNote }}</p>
    </div>
    <p v-else-if="status === 'discarded'" class="px-3 py-2 text-xs text-stone-500">{{ $t('Descartada') }}</p>

    <!-- Files that could not be changed. -->
    <div v-if="progress?.failed || progress?.state === 'failed'" class="border-t border-amber-200 bg-amber-50 px-3 py-2 text-xs text-amber-900" data-failed>
      <p class="flex items-start gap-1.5">
        <AlertTriangle :size="14" class="mt-px shrink-0" />
        <span v-if="progress?.state === 'failed'">{{ $t('Se detuvo: {error}', { error: progress.error ?? '' }) }}</span>
        <span v-else>{{ $tn(progress?.failed ?? 0, '{n} archivo no se pudo cambiar:', '{n} archivos no se pudieron cambiar:') }}</span>
      </p>
      <ul class="mt-0.5 ml-5 list-disc">
        <li v-for="e in progress?.errors ?? []" :key="e" class="break-all">{{ e }}</li>
      </ul>
    </div>

    <template v-if="pending">
      <div class="space-y-1 border-b border-stone-200 px-3 py-2 text-xs text-stone-700">
        <p v-if="plan.counts.bin">
          {{ $tn(plan.counts.bin, '{n} archivo va a la papelera de Drive, donde queda 30 días.', '{n} archivos van a la papelera de Drive, donde quedan 30 días.') }}
        </p>
        <p v-if="plan.counts.mark">
          {{
            $tn(
              plan.counts.mark,
              '{n} archivo es de otra cuenta y Drive no deja borrarlo: se renombra …_todelete (sale de Photo_links) para que su dueño lo borre.',
              '{n} archivos son de otra cuenta y Drive no deja borrarlos: se renombran …_todelete (salen de Photo_links) para que su dueño los borre.',
            )
          }}
        </p>
        <p>{{ $t('Después, las filas de Photo_links siguen a los archivos cambiados.') }}</p>
      </div>
      <div v-if="plan.problems?.count" class="border-b border-stone-200 px-3 py-2 text-xs text-amber-900" data-problems>
        <p class="flex items-center gap-1.5">
          <AlertTriangle :size="12" class="shrink-0 text-amber-600" />
          {{ $tn(plan.problems.count, '{n} cosa quedó fuera del plan:', '{n} cosas quedaron fuera del plan:') }}
        </p>
        <ul class="mt-0.5 ml-5 list-disc text-stone-700">
          <li v-for="p in plan.problems.examples" :key="p" class="break-all">{{ p }}</li>
        </ul>
      </div>
      <div class="border-b border-stone-200 px-3 py-1.5 text-xs">
        <button type="button" class="flex items-center gap-1 text-emerald-800 hover:underline" data-show-rows @click="showRows = !showRows">
          <ChevronDown v-if="showRows" :size="12" /><ChevronRight v-else :size="12" />
          {{ showRows ? $t('Ocultar los cambios') : $tn(plan.total, 'Ver el cambio', 'Ver los {n} cambios') }}
        </button>
        <p v-if="showRows && rowsGone" class="py-2 text-stone-500">{{ $t('Este plan es de hace más de media hora: pídeselo de nuevo al asistente') }}</p>
        <div v-show="showRows && rows" class="sheet-grid rows-table photo-links-rows mt-1.5">
          <div ref="host" />
        </div>
      </div>
      <div class="flex flex-wrap items-center gap-2 px-3 py-2">
        <button
          class="btn-primary bg-emerald-700 hover:bg-emerald-800"
          :disabled="busy || !plan.total"
          :title="$t('Hace estos cambios en Google Drive con la cuenta del proyecto')"
          data-apply
          @click="apply"
        >
          <Check :size="15" /> {{ $tn(plan.total, 'Aplicar {n} cambio', 'Aplicar {n} cambios') }}
        </button>
        <button class="btn ml-auto" :disabled="busy" @click="emit('discard')"><X :size="14" /> {{ $t('Descartar') }}</button>
      </div>
    </template>
  </div>
</template>
