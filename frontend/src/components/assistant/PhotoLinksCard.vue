<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, ref, watch } from 'vue'
import { TabulatorFull as Tabulator } from 'tabulator-tables'
import type { CellComponent, ColumnDefinition } from 'tabulator-tables'
import 'tabulator-tables/dist/css/tabulator_simple.min.css'
import { AlertTriangle, Check, ChevronDown, ChevronRight, FolderPlus, Images, RefreshCw, Sparkles, Wrench, X } from 'lucide-vue-next'
import { askClaude, showCard } from '../../lib/askClaude'
import { fixFor, type PhotoFix } from '../../lib/photoFixes'
import { photoUrl } from '../../lib/review'
import { api } from '../../lib/api'
import { errorText, notify } from '../../lib/notice'
import { intlLocale, locale, t } from '../../lib/i18n'
import type { Proposal } from '../../lib/proposals'
import {
  changeLabel,
  checkLabel,
  filterRows,
  flagText,
  foundChecks,
  progressPercent,
  rowsToWrite,
  rowsWanted,
  type CheckKind,
  type PhotoExample,
  type PhotoRow,
  type PhotoSync,
} from '../../lib/photoLinks'

/**
 * A reading of the photo folders on Drive for the Photo_links sheet (the assistant's
 * sync_photo_links), as a card of Cambios propuestos: the new photos by folder (untick one to
 * leave it for later), the rows that follow a renamed, replaced or deleted file, what looks wrong
 * in the new photos (a name the sheets will not find, an ID the workbook lacks, one out of the
 * order the photos were taken in…), and every row it would write, in a table. «Aplicar» writes
 * it, some hundred rows per save, and the card shows how far it is; «Leer de nuevo» reads the
 * folders again into the same card (its rows are kept for half an hour).
 */
const props = defineProps<{ proposal: Proposal }>()
const emit = defineEmits<{ discard: []; started: [] }>()

const sync = computed(() => (props.proposal.sync ?? {}) as PhotoSync)
const status = computed(() => props.proposal.status)
const pending = computed(() => status.value === 'pending')
const progress = computed(() => sync.value.progress)
const checks = computed(() => foundChecks(sync.value.checks ?? null))
const flagged = computed(() => checks.value.reduce((n, c) => n + c.count, 0))
const changes = computed(() => sync.value.changes ?? { renamed: 0, relinked: 0, removed: 0, moved: 0, details: 0, statusRows: 0 })
const twice = computed(() => sync.value.listedTwice ?? null)
const openTwice = ref(false)

/** What is left for later, by folder: its new rows (files not listed yet), its changed rows (rows already there). */
const skipNew = ref(new Set<string>())
const skipChanges = ref(new Set<string>())
type Part = 'new' | 'changes'
const setOf = (part: Part) => (part === 'new' ? skipNew : skipChanges)
function toggle(part: Part, folder: string) {
  const next = new Set(setOf(part).value)
  if (!next.delete(folder)) next.add(folder)
  setOf(part).value = next
}
/** The folders with something of that part. */
const foldersWith = (part: Part) => (sync.value.byFolder ?? []).filter(f => (part === 'new' ? f.new : f.changed)).map(f => f.folder)
/** Every folder's part ticked, or none. */
const allTicked = (part: Part) => !foldersWith(part).some(f => setOf(part).value.has(f))
function toggleAll(part: Part) {
  setOf(part).value = allTicked(part) ? new Set(foldersWith(part)) : new Set()
}
/** Folders with something left for later. */
const leftFolders = computed(() => new Set([...skipNew.value, ...skipChanges.value]).size)
const KIND_LABEL: Record<string, string> = {
  rename: 'renombrados', move: 'movidos', remove: 'borrados en Drive', relink: 'enlazados al archivo nuevo',
  details: 'dueño o tamaño', duplicate: 'listados dos veces', status: 'estado',
}
/** What changes in a folder, in words (the title of its count). */
/** A folder's name, or what the rows without one are (the sheet script's status lines, mostly). */
const folderName = (f: { folder: string; changed?: number; kinds?: Record<string, number> }) =>
  f.folder || (f.kinds?.status && f.kinds.status === f.changed ? t('(líneas de estado del script)') : t('(sin carpeta)'))
const kindsText = (kinds: Record<string, number> = {}) =>
  Object.entries(kinds).map(([k, n]) => `${n} ${t(KIND_LABEL[k] ?? k)}`).join(' · ')
const writing = computed(() => rowsToWrite(sync.value, skipNew.value, skipChanges.value))

const busy = ref<'' | 'apply' | 'read'>('')
const base = computed(() => `chat/proposals/${props.proposal.id}/photo-links`)
async function readAgain(quiet = false) {
  busy.value = 'read'
  try {
    await api(`${base.value}/refresh`, { method: 'POST', body: {} })
    rows.value = null
    if (!quiet) notify(t('Carpetas leídas de nuevo'))
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = ''
  }
}
async function apply() {
  busy.value = 'apply'
  try {
    await api(`${base.value}/apply`, { method: 'POST', body: { skipNew: [...skipNew.value], skipChanges: [...skipChanges.value] } })
    // The list brings its progress from here on.
    emit('started')
  } catch (e) {
    // Read too long ago for its rows to be kept: read again, to look at before applying.
    if ((e as { code?: string }).code === 'stale') {
      await readAgain(true)
      notify(t('Las carpetas se leyeron de nuevo (la lectura anterior era vieja): revisa y aplica'), 'error')
    } else notify(errorText(e), 'error')
  } finally {
    busy.value = ''
  }
}

// ------------------------------------------------------------ what to look at
const openCheck = ref<CheckKind | null>(null)
/** Whether an example says more than its check's name does. */
const hasDetail = (kind: CheckKind, e: PhotoExample) => kind !== 'unnamed' && !(kind === 'unknownId' && !e.maybe?.length)

// ------------------------------------------------------------ the rows
// open from the start, like a proposal's table: it follows the ticks above
const showRows = ref(true)
const rows = ref<PhotoRow[] | null>(null)
const rowsGone = ref(false)
const show = ref<'all' | 'flagged' | CheckKind>('all')
const host = ref<HTMLDivElement>()
let grid: Tabulator | null = null
const shown = computed(() => filterRows(rowsWanted(rows.value ?? [], skipNew.value, skipChanges.value), show.value))

function columns(): ColumnDefinition[] {
  return [
    { title: t('Cambio'), field: 'change', width: 110, formatter: (c: CellComponent) => changeLabel(c.getValue()) },
    { title: t('Carpeta'), field: 'folder', minWidth: 160 },
    {
      title: t('Nombre'),
      field: 'name',
      width: 190,
      // A new file's name opens it on Drive, in another tab.
      formatter: (c: CellComponent) => {
        const row = c.getData() as PhotoRow
        const text = row.from ? `${row.from} → ${row.name}` : row.detail ? `${row.name} (${row.detail})` : row.name
        if (!row.url) return text
        const link = document.createElement('a')
        Object.assign(link, { href: row.url, target: '_blank', rel: 'noopener', textContent: text, className: 'photo-link' })
        return link
      },
    },
    { title: t('Tomada'), field: 'taken', width: 125 },
    {
      title: t('Para revisar'),
      field: 'checks',
      headerSort: false,
      formatter: (c: CellComponent) => {
        const flags = (c.getData() as PhotoRow).checks ?? []
        c.getElement().classList.toggle('is-flagged', flags.length > 0)
        return flags.map(flagText).join(' · ')
      },
    },
  ] as ColumnDefinition[]
}
async function loadRows() {
  if (rows.value || rowsGone.value) return
  try {
    rows.value = (await api<{ rows: PhotoRow[] }>(`${base.value}/rows`)).rows
  } catch (e) {
    if ((e as { code?: string }).code === 'stale') rowsGone.value = true
    else notify(errorText(e), 'error')
  }
}
let built = false
async function draw() {
  if (!showRows.value) return
  await loadRows()
  await nextTick()
  if (!host.value || !rows.value || !showRows.value) return
  if (grid) {
    // Still being built: it takes the rows shown once it is.
    if (built) void grid.replaceData(shown.value)
    return
  }
  const made = (grid = new Tabulator(host.value, {
    data: shown.value,
    columns: columns(),
    layout: 'fitDataStretch',
    maxHeight: '22rem',
    placeholder: t('Sin filas'),
    columnDefaults: { headerSort: true, resizable: 'header' },
  } as unknown as ConstructorParameters<typeof Tabulator>[1]))
  made.on('tableBuilt', () => {
    if (grid !== made) return
    built = true
    void made.replaceData(shown.value)
  })
}
function dropGrid() {
  built = false
  grid?.destroy()
  grid = null
}
watch([showRows, shown], () => (showRows.value ? void draw() : dropGrid()))
watch(locale, () => built && grid?.setColumns(columns()))
// Read again, or written: the rows it held are another reading's.
watch(
  () => [props.proposal.revision, sync.value.read?.at],
  () => {
    rows.value = null
    rowsGone.value = false
    skipNew.value = new Set()
    skipChanges.value = new Set()
    if (showRows.value && pending.value && !sync.value.stale) void draw()
    else {
      showRows.value = false
      dropGrid()
    }
  },
)
onBeforeUnmount(dropGrid)

// ------------------------------------------------------------ the fixes it offers, and Claude
/** The rows are needed to work the fixes out: read once the card is pending with something to fix. */
watch(
  () => [pending.value, checks.value.length, sync.value.read?.at],
  () => pending.value && !sync.value.stale && checks.value.length && void loadRows(),
  { immediate: true },
)
const fixes = computed(() => new Map(checks.value.map(c => [c.kind, rows.value ? fixFor(c.kind, rows.value) : null])))
const fixing = ref('')
async function runFix(kind: string, fix: PhotoFix) {
  fixing.value = kind
  try {
    if (fix.operations) {
      const out = await api<{ plan: string }>('photo-links/plan', { method: 'POST', body: { operations: fix.operations } })
      showCard(out.plan)
      notify(t('Plan listo: revísalo y pulsa «Aplicar» en su tarjeta'))
    } else
      for (const folder of fix.folders ?? []) {
        const out = await api<{ batch: string }>('drive-photos/start', { method: 'POST', body: { folder } })
        showCard(out.batch)
      }
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    fixing.value = ''
  }
}
const asking = ref('')
async function ask(what: string, text: string) {
  asking.value = what
  try {
    const out = await askClaude(
      `${text} ${t('Es la lectura de Photo_links de la tarjeta {id} (Cambios propuestos): míralo con get_proposal de esa tarjeta o con sync_photo_links. Propón los cambios en tarjetas; no apliques nada sin que yo lo diga.', { id: props.proposal.id })}`,
      t('Photo_links: revisar'),
    )
    if (!out.chat) notify(t('T3 no está disponible: copia el pedido que aparece junto al chat'), 'error')
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    asking.value = ''
  }
}
const fileOf = (url: string) => /\/d\/([\w-]{20,})/.exec(url)?.[1] ?? ''
const listing = ref('')
/** A folder not listed yet, read with the others to start listing it (the same card, read again). */
async function listFolder(folder: string) {
  listing.value = folder
  try {
    const out = await api<{ proposalId: string | null }>('photo-links/read', { method: 'POST', body: { add: [folder], only: sync.value.onlyFolders?.length ? sync.value.only ?? [] : [] } })
    if (out.proposalId && out.proposalId !== props.proposal.id) showCard(out.proposalId)
    notify(t('Leída de nuevo con {folder}: revisa sus filas nuevas', { folder }))
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    listing.value = ''
  }
}

const readAt = computed(() => (sync.value.read?.at ? new Date(sync.value.read.at).toLocaleTimeString(intlLocale(), { hour: '2-digit', minute: '2-digit' }) : ''))
</script>

<template>
  <div class="mt-2 rounded-md border border-stone-300 bg-white text-sm" data-photo-links>
    <p class="flex flex-wrap items-center gap-x-2 gap-y-1 border-b border-stone-200 px-3 py-2">
      <Images :size="15" class="shrink-0 text-emerald-700" />
      <strong class="font-medium">{{ proposal.reason }}</strong>
      <span v-if="sync.read" class="text-xs text-stone-500">
        {{ $t('{files} archivos en {folders} carpetas de Drive, leídos a las {at} en {s} s', { files: sync.read.files, folders: sync.read.folders, at: readAt, s: sync.read.seconds }) }}
      </span>
      <span v-if="sync.onlyFolders?.length" class="rounded bg-stone-100 px-1.5 py-0.5 text-xs text-stone-700">
        {{ $t('solo {folders}', { folders: sync.onlyFolders.join(', ') }) }}
      </span>
      <button
        v-if="pending && !sync.stale"
        type="button"
        class="ml-auto flex items-center gap-1 rounded border border-sky-300 bg-white px-1.5 py-0.5 text-xs text-sky-800 hover:border-sky-600 disabled:opacity-50"
        :disabled="!!asking"
        :title="$t('Abre un chat con Claude al lado que ve todos los problemas de esta lectura y las fotos que necesite, y propone cómo resolverlos')"
        data-ask-claude
        @click="ask('card', $t('Ayúdame con todo lo que encontró esta lectura: revisa cada problema, mira las fotos que hagan falta para decidir (el sobre, las alas) y propón cómo resolverlo, empezando por lo que afecta a más fotos.'))"
      >
        <Sparkles :size="12" /> {{ asking === 'card' ? $t('Abriendo…') : $t('Resolver con Claude') }}
      </button>
      <button
        v-if="pending"
        type="button"
        class="flex items-center gap-1 rounded border border-stone-300 bg-white px-1.5 py-0.5 text-xs text-stone-700 hover:border-emerald-600 hover:text-emerald-800 disabled:opacity-50"
        :class="{ 'ml-auto': sync.stale }"
        :disabled="!!busy"
        :title="$t('Vuelve a leer las carpetas de Drive y compara de nuevo con la hoja')"
        data-read-again
        @click="readAgain()"
      >
        <RefreshCw :size="12" :class="{ 'animate-spin': busy === 'read' }" /> {{ busy === 'read' ? $t('Leyendo…') : $t('Leer de nuevo') }}
      </button>
    </p>

    <!-- Being written: how far it is. -->
    <div v-if="status === 'applying'" class="px-3 py-3" data-progress>
      <p class="mb-1.5 text-xs text-stone-700">
        {{
          $t('Escribiendo en Google Sheets: {written} de {total} filas (guardado {saved} de {saves})', {
            written: progress?.written ?? 0,
            total: progress?.total ?? sync.total,
            saved: Math.min((progress?.saved ?? 0) + 1, progress?.saves ?? 1),
            saves: progress?.saves ?? 1,
          })
        }}
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
      <p class="mt-1.5 text-xs text-stone-500">
        {{ $t('Cada guardado espera a que el libro recalcule; mientras tanto los demás guardados de la app esperan su turno.') }}
      </p>
    </div>

    <!-- Written. -->
    <p v-else-if="status === 'applied'" class="flex items-start gap-1.5 px-3 py-2 text-xs text-emerald-800" data-done>
      <Check :size="14" class="mt-px shrink-0" />
      <span>
        {{ $tn(progress?.written ?? 0, '{n} fila escrita en Photo_links.', '{n} filas escritas en Photo_links.') }}
        {{ $t('Cada guardado está en Historial y se puede deshacer. Photo_dorsal y Photo_ventral muestran las fotos cuando el libro recalcula.') }}
        <template v-if="progress?.leftFolders?.length">
          {{ $t('Quedan para después: {folders}.', { folders: progress.leftFolders.join(', ') }) }}
        </template>
      </span>
    </p>
    <p v-else-if="status === 'discarded'" class="px-3 py-2 text-xs text-stone-500">{{ $t('Descartada') }}</p>

    <!-- Stopped half way, or read too long ago. -->
    <p v-if="pending && (sync.stale || progress)" class="flex items-start gap-1.5 border-b border-amber-200 bg-amber-50 px-3 py-2 text-xs text-amber-900" data-stale>
      <AlertTriangle :size="14" class="mt-px shrink-0" />
      <span>
        <template v-if="progress?.state === 'queued'">
          {{ $t('Google Sheets no responde: el guardado en curso espera en la app y se escribirá solo.') }}
        </template>
        <template v-else-if="progress?.state === 'unconfirmed'">{{ $t('Google aún no confirma el último guardado.') }}</template>
        <template v-else-if="progress?.state === 'failed'">{{ $t('Se detuvo: {error}', { error: progress.error ?? '' }) }}</template>
        <template v-if="progress?.written">
          {{ $tn(progress.written, '{n} fila quedó escrita (está en Historial).', '{n} filas quedaron escritas (están en Historial).') }}
        </template>
        {{ $t('Lee las carpetas de nuevo para ver lo que falta.') }}
      </span>
    </p>
    <p v-if="progress?.skipped" class="border-b border-stone-200 px-3 py-1.5 text-xs text-stone-600">
      {{ $tn(progress.skipped, '{n} fila se dejó fuera porque cambió en la hoja: {why}', '{n} filas se dejaron fuera porque cambiaron en la hoja: {why}', { why: progress.why ?? '' }) }}
    </p>

    <template v-if="pending && !sync.stale">
      <!-- What changes, by folder: its new rows and its changed rows, each applied now or left for later. -->
      <div v-if="sync.byFolder?.length" class="border-b border-stone-200" data-folders>
        <p class="px-3 pt-1.5 text-xs text-stone-600">
          {{ $t('Nuevos: filas que se añaden (archivos que la hoja aún no lista). Cambian: filas que ya están y se corrigen (renombrados, movidos, borrados en Drive…). Desmarca lo que quieras dejar para después: las filas de abajo siguen tus marcas.') }}
        </p>
        <table class="w-full text-xs">
          <thead>
            <tr class="text-left text-stone-500">
              <th class="py-1 pl-3 font-normal">{{ $t('Carpeta') }}</th>
              <th v-for="part in (['new', 'changes'] as const)" :key="part" class="px-2 py-1 text-right font-normal">
                <label class="inline-flex cursor-pointer items-center gap-1" :title="part === 'new' ? $t('Añadir las filas nuevas de todas las carpetas') : $t('Aplicar los cambios de todas las carpetas')">
                  <input type="checkbox" class="rounded border-stone-300 text-emerald-700" :checked="allTicked(part)" :data-all="part" @change="toggleAll(part)" />
                  {{ part === 'new' ? $t('Nuevos') : $t('Cambian') }}
                </label>
              </th>
              <th class="py-1 pr-3 text-right font-normal">{{ $t('En la carpeta') }}</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="f in sync.byFolder" :key="f.folder" class="border-t border-stone-100">
              <td class="py-1 pl-3 break-all" :class="{ 'text-stone-400': (!f.new || skipNew.has(f.folder)) && (!f.changed || skipChanges.has(f.folder)) }">{{ folderName(f) }}</td>
              <td v-for="part in (['new', 'changes'] as const)" :key="part" class="px-2 py-1 text-right tabular-nums">
                <label v-if="part === 'new' ? f.new : f.changed" class="inline-flex cursor-pointer items-center gap-1 font-medium" :class="{ 'text-stone-400 line-through': setOf(part).value.has(f.folder) }"
                  :title="part === 'new' ? $t('Añadir sus filas nuevas ahora') : kindsText(f.kinds)">
                  <input type="checkbox" class="rounded border-stone-300 text-emerald-700" :checked="!setOf(part).value.has(f.folder)"
                    :aria-label="part === 'new' ? $t('Filas nuevas de {folder}', { folder: folderName(f) }) : $t('Cambios de {folder}', { folder: folderName(f) })"
                    @change="toggle(part, f.folder)" />
                  {{ part === 'new' ? f.new : f.changed }}
                </label>
              </td>
              <td class="py-1 pr-3 text-right text-stone-500 tabular-nums">{{ f.files ?? '' }}</td>
            </tr>
          </tbody>
        </table>
      </div>

      <!-- Every row. -->
      <div class="border-b border-stone-200 px-3 py-1.5 text-xs">
        <div class="flex flex-wrap items-center gap-2">
          <button type="button" class="flex items-center gap-1 font-medium text-emerald-800 hover:underline" data-show-rows @click="showRows = !showRows">
            <ChevronDown v-if="showRows" :size="12" /><ChevronRight v-else :size="12" />
            {{ $tn(writing, 'La fila que se escribiría', 'Las {n} filas que se escribirían') }}
          </button>
          <select
            v-if="showRows && rows"
            v-model="show"
            class="rounded border border-stone-200 bg-white py-0.5 pr-6 pl-1.5 text-xs text-stone-800"
            :aria-label="$t('Qué filas mostrar')"
          >
            <option value="all">{{ $t('Todas ({n})', { n: writing }) }}</option>
            <option v-if="flagged" value="flagged">{{ $t('Solo para revisar') }}</option>
            <option v-for="c in checks" :key="c.kind" :value="c.kind">{{ checkLabel(c.kind) }} ({{ c.count }})</option>
          </select>
        </div>
        <p v-if="showRows && rowsGone" class="py-2 text-stone-500">{{ $t('Las carpetas se leyeron hace más de media hora: léelas de nuevo.') }}</p>
        <div v-show="showRows && rows" class="sheet-grid rows-table photo-links-rows mt-1.5">
          <div ref="host" />
        </div>
      </div>
      <!-- Rows that follow their file. -->
      <p
        v-if="changes.renamed || changes.relinked || changes.removed || changes.moved || changes.details"
        class="flex flex-wrap gap-x-3 gap-y-0.5 border-b border-stone-200 px-3 py-1.5 text-xs text-stone-700"
        data-changes
      >
        <span v-if="changes.renamed">{{ $tn(changes.renamed, '{n} archivo renombrado en Drive', '{n} archivos renombrados en Drive') }}</span>
        <span v-if="changes.relinked">{{ $tn(changes.relinked, '{n} fila enlazada al archivo nuevo del mismo nombre', '{n} filas enlazadas al archivo nuevo del mismo nombre') }}</span>
        <span v-if="changes.removed">{{ $tn(changes.removed, '{n} fila vaciada (su archivo se borró)', '{n} filas vaciadas (su archivo se borró)') }}</span>
        <span v-if="changes.moved">{{ $tn(changes.moved, '{n} archivo movido de carpeta', '{n} archivos movidos de carpeta') }}</span>
        <span v-if="changes.details">{{ $tn(changes.details, '{n} fila con otro dueño o tamaño', '{n} filas con otro dueño o tamaño') }}</span>
      </p>

      <!-- Photos the sheet listed twice. -->
      <div v-if="twice && (twice.sameFile || twice.copies || twice.different)" class="space-y-1 border-b border-stone-200 px-3 py-2 text-xs" data-listed-twice>
        <p v-if="twice.sameFile + twice.copies" class="text-stone-700">
          {{
            $tn(
              twice.sameFile + twice.copies,
              '{n} fila vaciada: la misma foto estaba listada dos veces (las hojas y los sitios la toman una vez).',
              '{n} filas vaciadas: la misma foto estaba listada dos veces (las hojas y los sitios la toman una vez).',
            )
          }}
          <span v-if="twice.copyExamples[0]" class="text-stone-500">
            {{ $t('Ej.: {name} se queda en {kept}.', { name: twice.copyExamples[0].name, kept: twice.copyExamples[0].kept.split('/').slice(0, -1).join('/') }) }}
          </span>
        </p>
        <template v-if="twice.different">
          <button
            type="button"
            class="flex w-full items-center gap-1.5 rounded px-1 py-0.5 text-left text-amber-900 hover:bg-amber-50"
            :aria-expanded="openTwice"
            @click="openTwice = !openTwice"
          >
            <ChevronDown v-if="openTwice" :size="12" class="shrink-0" /><ChevronRight v-else :size="12" class="shrink-0" />
            <AlertTriangle :size="12" class="shrink-0 text-amber-600" />
            <span class="font-medium tabular-nums">{{ twice.different }}</span>
            <span>{{ $t('nombres son de dos fotos distintas: compara los sobres; se dejan como están') }}</span>
          </button>
          <!-- Both photos of each name side by side, to compare the envelopes. -->
          <ul v-if="openTwice" class="mt-1 mb-1 ml-7 space-y-2 text-stone-700">
            <li v-for="d in twice.differentExamples" :key="d.name" class="break-all">
              <span class="font-medium">{{ d.name }}</span>
              <div class="mt-1 grid grid-cols-2 gap-1.5">
                <a v-for="r in d.rows" :key="r.row" class="group block rounded border border-stone-200 hover:border-emerald-600" :href="r.url" target="_blank" rel="noopener">
                  <img :src="photoUrl(fileOf(r.url))" :alt="r.path" loading="lazy" class="h-28 w-full rounded-t bg-stone-100 object-contain" />
                  <span class="block px-1 py-0.5 text-[11px]">
                    <span class="text-emerald-800 group-hover:underline">{{ r.path.split('/').slice(0, -1).join('/') }}</span>
                    <span class="text-stone-400"> · {{ $t('fila {row}', { row: r.row }) }}, {{ r.kb }} KB</span>
                  </span>
                </a>
              </div>
            </li>
            <li v-if="twice.different > twice.differentExamples.length" class="text-stone-500">
              {{ $t('y {n} más: pídeselos al asistente', { n: twice.different - twice.differentExamples.length }) }}
            </li>
          </ul>
        </template>
      </div>

      <!-- What looks wrong in the new photos. -->
      <div v-if="sync.checks" class="border-b border-stone-200 px-3 py-2 text-xs" data-checks>
        <p class="text-stone-700">
          {{
            $t('De {photos} fotos nuevas, {linked} ya tienen el nombre que las hojas buscan ({butterflies} mariposas).', {
              photos: sync.checks.photos,
              linked: sync.checks.linked,
              butterflies: sync.checks.butterflies,
            })
          }}
        </p>
        <ul v-if="checks.length" class="mt-1.5 space-y-0.5">
          <li v-for="c in checks" :key="c.kind" :data-check="c.kind">
            <div class="flex items-center gap-1.5">
              <button
                type="button"
                class="flex min-w-0 flex-1 items-center gap-1.5 rounded px-1 py-0.5 text-left text-amber-900 hover:bg-amber-50"
                :aria-expanded="openCheck === c.kind"
                @click="openCheck = openCheck === c.kind ? null : c.kind"
              >
                <ChevronDown v-if="openCheck === c.kind" :size="12" class="shrink-0" /><ChevronRight v-else :size="12" class="shrink-0" />
                <AlertTriangle :size="12" class="shrink-0 text-amber-600" />
                <span class="font-medium tabular-nums">{{ c.count }}</span>
                <span>{{ checkLabel(c.kind) }}</span>
              </button>
              <!-- The fix that is safe without a look, as a plan to check on its own card. -->
              <button
                v-if="pending && fixes.get(c.kind)"
                type="button"
                class="flex shrink-0 items-center gap-1 rounded border border-emerald-300 bg-white px-1.5 py-0.5 text-emerald-800 hover:border-emerald-600 disabled:opacity-50"
                :disabled="!!fixing"
                :title="fixes.get(c.kind)!.title"
                data-fix
                @click="runFix(c.kind, fixes.get(c.kind)!)"
              >
                <Wrench :size="11" /> {{ fixing === c.kind ? $t('Preparando…') : fixes.get(c.kind)!.label }}
              </button>
            </div>
            <ul v-if="openCheck === c.kind" class="mt-0.5 mb-1 ml-7 space-y-0.5 text-stone-700">
              <li v-for="e in c.examples" :key="e.name + (e.in ?? '')" class="break-all">
                <a v-if="e.url" class="font-medium text-emerald-800 hover:underline" :href="e.url" target="_blank" rel="noopener">{{ e.name }}</a>
                <span v-else class="font-medium">{{ e.name }}</span>
                <span v-if="hasDetail(c.kind, e)" class="text-stone-500"> · {{ flagText({ ...e, kind: c.kind }) }}</span>
                <span v-if="e.in" class="text-stone-400"> · {{ e.in }}</span>
              </li>
              <li v-if="c.count > c.examples.length">
                <button type="button" class="text-emerald-800 hover:underline" @click="((show = c.kind), (showRows = true))">
                  {{ $t('Ver las {n} en la tabla', { n: c.count }) }}
                </button>
              </li>
            </ul>
          </li>
        </ul>
      </div>

      <!-- Files left out, folders not listed. -->
      <div
        v-if="sync.sameName?.different || sync.sameName?.copies || sync.notListed?.length || sync.leftOut?.toDelete || sync.leftOut?.temp?.length"
        class="space-y-1 border-b border-stone-200 px-3 py-2 text-xs text-stone-700"
        data-left-out
      >
        <p v-if="sync.sameName?.different" class="text-amber-900">
          <AlertTriangle :size="12" class="mr-1 inline text-amber-600" />{{
            $tn(
              sync.sameName.different,
              '{n} archivo no se lista: otra foto, distinta, ya está listada con su nombre ({examples}).',
              '{n} archivos no se listan: otra foto, distinta, ya está listada con su nombre ({examples}).',
              { examples: sync.sameName.examples.slice(0, 3).map(e => e.name).join(', ') },
            )
          }}
        </p>
        <p v-if="sync.sameName?.copies">
          {{ $tn(sync.sameName.copies, '{n} copia de una foto ya listada no se lista.', '{n} copias de fotos ya listadas no se listan.') }}
        </p>
        <div v-if="sync.notListed?.length">
          <p>{{ $t('Carpetas con fotos de especímenes que no se listan:') }}</p>
          <ul class="mt-0.5 space-y-0.5">
            <li v-for="n in sync.notListed" :key="n.folder" class="flex items-center gap-2">
              <span class="min-w-0 flex-1"><span class="font-medium">{{ n.folder }}</span> ({{ $tn(n.photos, '{n} foto', '{n} fotos') }})</span>
              <button
                v-if="pending"
                type="button"
                class="flex shrink-0 items-center gap-1 rounded border border-emerald-300 bg-white px-1.5 py-0.5 text-emerald-800 hover:border-emerald-600 disabled:opacity-50"
                :disabled="!!listing"
                :title="$t('Lee las carpetas de nuevo con esta, para empezar a listarla (sus filas aparecen en esta tarjeta para aplicar)')"
                data-list-folder
                @click="listFolder(n.folder)"
              >
                <FolderPlus :size="11" /> {{ listing === n.folder ? $t('Leyendo…') : $t('Listar') }}
              </button>
            </li>
          </ul>
        </div>
        <p v-if="sync.leftOut?.toDelete || sync.leftOut?.temp?.length" class="text-stone-500">
          {{ $t('Nunca se listan:') }}
          <template v-if="sync.leftOut.toDelete">{{ $tn(sync.leftOut.toDelete, '{n} archivo …_todelete', '{n} archivos …_todelete') }}</template>
          <template v-for="(f, i) in sync.leftOut.temp" :key="f.folder"
            ><template v-if="i || sync.leftOut.toDelete">; </template>{{ f.folder }} ({{ f.files }})</template
          >
        </p>
      </div>

    </template>

    <div v-if="pending" class="flex flex-wrap items-center gap-2 px-3 py-2">
      <button
        v-if="!sync.stale"
        class="btn-primary bg-emerald-700 hover:bg-emerald-800"
        :disabled="!!busy || !writing"
        :title="$t('Escribe estas filas en Photo_links, unos cientos por guardado')"
        data-apply
        @click="apply"
      >
        <Check :size="15" /> {{ $tn(writing, 'Aplicar {n} fila', 'Aplicar {n} filas') }}
      </button>
      <button v-else class="btn-primary bg-emerald-700 hover:bg-emerald-800" :disabled="!!busy" @click="readAgain()">
        <RefreshCw :size="15" :class="{ 'animate-spin': busy === 'read' }" /> {{ busy === 'read' ? $t('Leyendo…') : $t('Leer de nuevo') }}
      </button>
      <span v-if="leftFolders" class="text-xs text-stone-500">
        {{ $tn(leftFolders, 'Algo de {n} carpeta queda para después', 'Algo de {n} carpetas queda para después') }}
      </span>
      <button class="btn ml-auto" :disabled="!!busy" @click="emit('discard')"><X :size="14" /> {{ $t('Descartar') }}</button>
    </div>
  </div>
</template>

<style>
.photo-links-rows .photo-link {
  color: #065f46;
}
.photo-links-rows .photo-link:hover {
  text-decoration: underline;
}
.photo-links-rows .tabulator-cell.is-flagged {
  background: #fffbeb;
  color: #78350f;
}
</style>
