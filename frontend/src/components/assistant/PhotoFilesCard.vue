<script setup lang="ts">
import { computed, defineAsyncComponent, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { TabulatorFull as Tabulator } from 'tabulator-tables'
import type { CellComponent, ColumnDefinition } from 'tabulator-tables'
import 'tabulator-tables/dist/css/tabulator_simple.min.css'
import { AlertTriangle, Check, ChevronDown, ChevronRight, ExternalLink, FolderCog, UserRoundCheck, X } from 'lucide-vue-next'
import { api } from '../../lib/api'
import { errorText, notify } from '../../lib/notice'
import { locale, t } from '../../lib/i18n'
import { excludeFileChanges, includeFileChanges, leftOutFileChanges } from '../../lib/photo-files'
import type { Proposal } from '../../lib/proposals'
import { FILE_CHANGES, fileChangeLabel, fileCounts, fileTarget, handoverAccounts, progressPercent, type FileChange, type FileRow, type PhotoFiles } from '../../lib/photoLinks'
import { useRowPhotos } from '../../lib/useRowPhotos'

const CardPhotos = defineAsyncComponent(() => import('./CardPhotos.vue'))

/**
 * Changes to make to the photo files on Google Drive (the assistant's organize_photos), as a card
 * of Cambios propuestos: how many files are renamed, moved, binned or marked …_todelete (the
 * files of another account, for their owner to delete), the folders and shortcuts made, what was
 * left out and why, and every change in a table. «Aplicar» makes them with the project's Google
 * account, a bar showing how far it is; Photo_links then follows the files changed. The
 * assistant takes a plan's changes back when asked. Only this button makes them.
 * What another account has to do afterwards (`handover`: copy a folder's files, or delete the
 * …_todelete ones it owns) is left ready in the team's sheet «Transfer Ownership & Free Up Space»:
 * the card says which account opens it and which button it presses.
 */
const props = defineProps<{ proposal: Proposal }>()
const emit = defineEmits<{ discard: []; started: [] }>()

const plan = computed(() => (props.proposal.sync ?? { total: 0, counts: {}, raw: 0, problems: { count: 0, examples: [] } }) as PhotoFiles)
const status = computed(() => props.proposal.status)
const pending = computed(() => status.value === 'pending')
const progress = computed(() => plan.value.progress)
const base = computed(() => `chat/proposals/${props.proposal.id}/photo-links`)
const handover = computed(() => plan.value.handover)
/** The files changed: the steps made, without the sheet's own. */
const filesDone = computed(() => Math.max(0, (progress.value?.written ?? 0) - (progress.value?.handoverWritten ? 1 : 0)))
/** What the sheet is set to: the folder, with or without those inside it, and where deleted files go. */
const handoverSet = computed(() => {
  const h = handover.value
  if (!h) return ''
  return [
    h.subfolders ? t('con sus subcarpetas') : t('sin sus subcarpetas'),
    ...(h.step === 'delete' ? [h.permanent ? t('borrado definitivo') : t('a la papelera')] : []),
  ].join(' · ')
})

const busy = ref(false)
const selected = ref<Set<number>>(new Set())
const selectedKind = ref<FileChange | ''>('')
async function apply() {
  if (!rows.value || !selected.value.size) return
  busy.value = true
  try {
    await api(`${base.value}/apply`, { method: 'POST', body: { leaveOut: leftOutFileChanges(rows.value, selected.value) } })
    emit('started')
  } catch (e) {
    notify((e as { code?: string }).code === 'stale' ? t('Este plan es de hace más de media hora: pídeselo de nuevo al asistente') : errorText(e), 'error')
  } finally {
    busy.value = false
  }
}
async function retryFailed() {
  busy.value = true
  try {
    await api(`${base.value}/apply`, { method: 'POST', body: { retry: true } })
    emit('started')
  } catch (error) {
    notify(errorText(error), 'error')
  } finally {
    busy.value = false
  }
}

// ------------------------------------------------------------ every change
// open from the start, like a proposal's table; applied, the changes it made
const showRows = ref(true)
const applied = computed(() => status.value === 'applied')
const tableOn = computed(() => pending.value || applied.value)
const picking = useRowPhotos<FileRow>(r => [fileChangeLabel(r.change), fileTarget(r)].filter(Boolean).join(' → '))
const rows = ref<FileRow[] | null>(null)
const rowsGone = ref(false)
const host = ref<HTMLDivElement>()
let grid: Tabulator | null = null
let built = false
function updateSelection(next: Set<number>) {
  selected.value = next
  grid?.redraw(true)
}
function toggleRow(index: number) {
  if (!rows.value) return
  updateSelection(selected.value.has(index)
    ? excludeFileChanges(rows.value, selected.value, [index])
    : includeFileChanges(rows.value, selected.value, [index]))
}
function chooseKind() {
  if (!rows.value || !selectedKind.value) return
  updateSelection(includeFileChanges(rows.value, selected.value, rows.value.flatMap((row, index) => row.change === selectedKind.value ? [index] : [])))
}
async function copyHandover() {
  const h = progress.value?.handover
  if (!h) return
  const message = h.step === 'delete'
    ? t('Cada cuenta dueña ({accounts}) debe abrir la hoja «{sheet}» con su sesión y pulsar «{button}» para borrar sus archivos marcados en {folder}.', {
      sheet: h.sheet, accounts: handoverAccounts(h) || t('la cuenta dueña de los archivos …_todelete'), button: h.button, folder: h.folder,
    })
    : t('Por favor, abre la hoja «{sheet}» con la cuenta que recibirá las fotos y pulsa «{button}» para la carpeta {folder}.', {
      sheet: h.sheet, button: h.button, folder: h.folder,
    })
  try {
    await navigator.clipboard.writeText([message, h.sheetUrl].filter(Boolean).join('\n'))
    notify(t('Mensaje copiado'), 'success')
  } catch (error) {
    notify(errorText(error), 'error')
  }
}
function columns(): ColumnDefinition[] {
  return [
    ...(pending.value ? [{
      title: t('Elegir'), field: '_selected', width: 72, headerSort: false,
      formatter: (c: CellComponent) => {
        const index = (c.getRow().getData() as FileRow & { planIndex: number }).planIndex
        const input = document.createElement('input')
        input.type = 'checkbox'
        input.checked = selected.value.has(index)
        input.setAttribute('aria-label', t('Elegir cambio {n}', { n: index + 1 }))
        input.className = 'h-4 w-4 accent-emerald-700'
        input.addEventListener('click', event => event.stopPropagation())
        input.addEventListener('change', () => toggleRow(index))
        return input
      },
    } as ColumnDefinition] : []),
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
        const copy = !row.copy
          ? ''
          : row.keptIn && row.keptAs
            ? t('el mismo archivo está en {folder} como {name}', { folder: row.keptIn, name: row.keptAs })
            : row.keptIn
              ? t('el mismo archivo está en {folder}', { folder: row.keptIn })
              : t('el mismo archivo está en otra carpeta')
        const lost = row.lookedUp ? t('⚠ las hojas encuentran este archivo, no el que se queda') : ''
        return [row.raw ? t('RAW de su foto') : '', copy, lost].filter(Boolean).join(' · ')
      },
    },
  ] as ColumnDefinition[]
}
async function draw() {
  if (!showRows.value || !tableOn.value) return
  if (!rows.value && !rowsGone.value)
    try {
      rows.value = (await api<{ rows: FileRow[] }>(`${base.value}/rows`)).rows.map((row, planIndex) => ({ ...row, planIndex }))
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
    rowFormatter: picking.rowFormatter,
  } as unknown as ConstructorParameters<typeof Tabulator>[1]))
  picking.attach(made)
  made.on('tableBuilt', () => {
    if (grid === made) built = true
  })
}
function dropGrid() {
  built = false
  grid?.destroy()
  grid = null
  picking.clear()
}
watch([showRows, tableOn], () => (showRows.value && tableOn.value ? void draw() : dropGrid()))
watch(pending, () => {
  rows.value = null
  selected.value = new Set()
  dropGrid()
  void draw()
})
onMounted(() => void draw())
watch(locale, () => built && grid?.setColumns(columns()))
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
          {{ $tn(filesDone, '{n} archivo cambiado en Google Drive.', '{n} archivos cambiados en Google Drive.') }}
          <template v-if="progress?.sheetRows">
            {{ $tn(progress.sheetRows, 'Photo_links: {n} fila actualizada (en Historial).', 'Photo_links: {n} filas actualizadas (en Historial).') }}
          </template>
          <template v-if="plan.counts.delete">{{ $t('Los archivos borrados definitivamente no se pueden recuperar.') }}</template>
          <template v-else>{{ $t('El asistente puede deshacer estos cambios si se lo pides.') }}</template>
        </span>
      </p>
      <p v-if="progress?.sheetNote" class="text-amber-900">{{ progress.sheetNote }}</p>
      <!-- The step left to another account. -->
      <div v-if="progress?.handover" class="mt-1 rounded border border-sky-300 bg-sky-50 px-2 py-1.5 text-sky-950" data-handover-next>
        <p class="flex items-start gap-1.5">
          <UserRoundCheck :size="14" class="mt-px shrink-0 text-sky-700" />
          <span>
            <strong class="font-medium">{{ $t('Falta el paso de otra cuenta.') }}</strong>{{ ' ' }}
            <template v-if="progress.handover.step === 'delete'">
              {{ $t('Cada cuenta dueña abre la hoja «{sheet}» con su sesión y pulsa «{button}»:', { sheet: progress.handover.sheet, button: progress.handover.button }) }}
              <strong class="font-medium break-all">{{ handoverAccounts(progress.handover) || $t('la cuenta dueña de los archivos …_todelete') }}</strong>
            </template>
            <template v-else>
              {{
                $t('La cuenta que se queda con las fotos abre la hoja «{sheet}» con su sesión y pulsa «{button}»; después, pide al asistente actualizar Photo_links.', {
                  sheet: progress.handover.sheet,
                  button: progress.handover.button,
                })
              }}
            </template>
            {{ $t('La hoja ya tiene la carpeta {folder}.', { folder: progress.handover.folder }) }}
          </span>
        </p>
        <a v-if="progress.handover.sheetUrl" :href="progress.handover.sheetUrl" target="_blank" rel="noopener" class="mt-1 ml-5 inline-flex items-center gap-1 text-sky-800 hover:underline">
          <ExternalLink :size="12" /> {{ $t('Abrir la hoja') }}
        </a>
        <button type="button" class="btn ml-5 mt-1 min-h-9 text-xs" data-copy-handover @click="copyHandover">{{ $t('Copiar mensaje para la cuenta dueña') }}</button>
      </div>
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
      <button v-if="applied && progress?.failed" type="button" class="btn mt-2 min-h-9 text-xs" :disabled="busy" data-retry-failed @click="retryFailed">{{ $t('Reintentar cambios fallidos') }}</button>
    </div>

    <template v-if="pending">
      <div class="space-y-1 border-b border-stone-200 px-3 py-2 text-xs text-stone-700">
        <p v-if="plan.counts.bin">
          {{ $tn(plan.counts.bin, '{n} archivo va a la papelera de Drive, donde queda 30 días.', '{n} archivos van a la papelera de Drive, donde quedan 30 días.') }}
        </p>
        <p v-if="plan.counts.delete" class="font-medium text-red-800" data-delete>
          {{
            $tn(
              plan.counts.delete,
              '{n} archivo se borra definitivamente: su espacio queda libre al momento y no se puede recuperar. Su copia está junto a él (mismo nombre y tamaño).',
              '{n} archivos se borran definitivamente: su espacio queda libre al momento y no se pueden recuperar. Sus copias están junto a ellos (mismo nombre y tamaño).',
            )
          }}
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
      <div v-if="handover" class="space-y-1 border-b border-stone-200 bg-sky-50 px-3 py-2 text-xs text-sky-950" data-handover>
        <p class="flex items-start gap-1.5">
          <UserRoundCheck :size="14" class="mt-px shrink-0 text-sky-700" />
          <span>
            {{ $t('La hoja «{sheet}» queda preparada para la carpeta {folder} ({set}).', { sheet: handover.sheet, folder: handover.folder, set: handoverSet }) }}
          </span>
        </p>
        <p v-if="handover.step === 'delete'" class="ml-5">
          {{ $t('Después, cada cuenta dueña abre la hoja con su sesión y pulsa «{button}»:', { button: handover.button }) }}
          <strong class="font-medium break-all">{{ handoverAccounts(handover) || $t('la cuenta dueña de los archivos …_todelete') }}</strong>
        </p>
        <p v-else class="ml-5">
          {{
            $t('Después, la cuenta que se queda con las fotos abre la hoja con su sesión y pulsa «{button}»: copia los archivos de las otras cuentas y renombra los originales …_todelete.', {
              button: handover.button,
            })
          }}
          <template v-if="handover.by?.length">
            {{ $t('Archivos ahora:') }} <span class="break-all">{{ handoverAccounts(handover) }}</span>
          </template>
        </p>
        <p v-if="handover.lastRun" class="ml-5 text-sky-900/70">{{ $t('Último uso de la hoja: {run}', { run: handover.lastRun }) }}</p>
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
    </template>

    <!-- Every change, and beside it the photos of the rows clicked. -->
    <div v-if="tableOn" class="border-b border-stone-200 px-3 py-1.5 text-xs [container-type:inline-size]" data-rows>
      <button type="button" class="flex items-center gap-1 font-medium text-emerald-800 hover:underline" data-show-rows @click="showRows = !showRows">
        <ChevronDown v-if="showRows" :size="12" /><ChevronRight v-else :size="12" />
        <template v-if="applied">{{ $tn(rows?.length ?? 0, 'El cambio hecho', 'Los {n} cambios hechos') }}</template>
        <template v-else>{{ $tn(plan.total, 'El cambio', 'Los {n} cambios') }}</template>
      </button>
      <p v-if="showRows && rowsGone" class="py-2 text-stone-500">
        {{ applied ? $t('Los cambios de este plan ya no se guardan.') : $t('Este plan es de hace más de media hora: pídeselo de nuevo al asistente') }}
      </p>
      <div v-if="pending && rows" class="mt-2 flex flex-wrap items-center gap-2" data-file-selection>
        <label class="sr-only" for="file-change-kind">{{ $t('Tipo de cambio') }}</label>
        <select id="file-change-kind" v-model="selectedKind" class="min-h-9 rounded border border-stone-300 bg-white px-2 text-xs" data-select-kind>
          <option value="">{{ $t('Tipo de cambio') }}</option>
          <option v-for="kind in FILE_CHANGES.filter(kind => rows?.some(row => row.change === kind))" :key="kind" :value="kind">{{ fileChangeLabel(kind) }}</option>
        </select>
        <button type="button" class="btn min-h-9 text-xs" :disabled="!selectedKind" data-add-kind @click="chooseKind">{{ $t('Elegir este tipo') }}</button>
        <button type="button" class="btn min-h-9 text-xs" data-select-all @click="updateSelection(includeFileChanges(rows, new Set(), rows.map((_, i) => i)))">{{ $t('Elegir todos') }}</button>
        <button type="button" class="btn min-h-9 text-xs" :disabled="!selected.size" data-select-none @click="updateSelection(new Set())">{{ $t('Quitar selección') }}</button>
        <span class="text-stone-700" data-selected-count>{{ $t('{n} de {total} cambios elegidos', { n: selected.size, total: rows.length }) }}</span>
      </div>
      <div v-show="showRows && rows" class="card-rows mt-1.5 flex flex-col gap-2">
        <div class="sheet-grid rows-table photo-links-rows min-w-0 flex-1">
          <div ref="host" />
        </div>
        <CardPhotos :card="proposal.id" :photos="picking.photos.value" class="card-photos" />
      </div>
    </div>

    <template v-if="pending">
      <div class="flex flex-wrap items-center gap-2 px-3 py-2">
        <button
          class="btn-primary bg-emerald-700 hover:bg-emerald-800"
          :disabled="busy || !selected.size || !rows"
          :title="$t('Hace estos cambios en Google Drive con la cuenta del proyecto')"
          data-apply
          @click="apply"
        >
          <Check :size="15" /> {{ $tn(selected.size, 'Aplicar {n} cambio', 'Aplicar {n} cambios') }}
        </button>
        <button class="btn ml-auto" :disabled="busy" @click="emit('discard')"><X :size="14" /> {{ $t('Descartar') }}</button>
      </div>
    </template>
  </div>
</template>
