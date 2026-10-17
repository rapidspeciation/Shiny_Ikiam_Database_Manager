<script setup lang="ts">
import { computed, defineAsyncComponent, onMounted, ref, watch } from 'vue'
import { AlertTriangle, Check, ChevronDown, ChevronRight, FolderPlus, Images, RefreshCw, Sparkles, Wrench, X } from 'lucide-vue-next'
import { askClaude, showCard } from '../../lib/askClaude'
import type { CellEdit } from './ProposalSheet.vue'
import { fixFor, type PhotoFix } from '../../lib/photoFixes'
import { photoUrl } from '../../lib/review'
import { useSession } from '../../stores/session'
import type { CellValue } from '../../lib/types'
import { api } from '../../lib/api'
import { errorText, notify } from '../../lib/notice'
import { intlLocale, t } from '../../lib/i18n'
import type { PersonEdit, Proposal, ProposalChange } from '../../lib/proposals'
import {
  checkLabel,
  driveFileOf,
  flagText,
  foundChecks,
  progressPercent,
  rowsToWrite,
  type CheckKind,
  type PhotoExample,
  type PhotoRow,
  type PhotoSync,
} from '../../lib/photoLinks'

const CardPhotos = defineAsyncComponent(() => import('./CardPhotos.vue'))
/** A proposal's table, as the sheet proposals have it (Tabulator loads apart). */
const ProposalSheet = defineAsyncComponent(() => import('./ProposalSheet.vue'))

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
/** What the wing models read on the photos of the folders read: the other side than the name's d/v, or turned. */
const wingChecks = computed(() => foundChecks(sync.value.wings))
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
// The rows it would write as the sheet has them, in a proposal's table (ProposalSheet): the sheet's
// columns, the reading's values in green, the person's in blue (saved to the reading at once, written
// by «Aplicar»), copied and pasted as in the sheet. Open from the start; they follow the ticks above.
// Applied: the rows it wrote. The photos of the rows selected show beside the table.
const showRows = ref(true)
const applied = computed(() => status.value === 'applied')
const tableOn = computed(() => (pending.value && !sync.value.stale) || applied.value)
/** A reading's row in the sheet's columns, as the server sends it (sheetChanges in server/photo-links-tool.mjs). */
interface SheetRow {
  key: string
  recordId?: string
  row?: number
  kind: string
  folder: string
  create?: boolean
  values: Record<string, CellValue>
  rowValues?: Record<string, CellValue>
  checks?: PhotoRow['checks']
  personEdits?: Record<string, PersonEdit>
}
const rows = ref<PhotoRow[] | null>(null)
const sheetRows = ref<SheetRow[] | null>(null)
const rowsGone = ref(false)
const show = ref<'all' | 'flagged' | CheckKind>('all')
const session = useSession()
const fields = computed(() => session.module('Photo_links')?.fields ?? [])
const types = computed(() => Object.fromEntries(fields.value.map(f => [f.key, f.type])))
const formulas = computed(() => fields.value.filter(f => f.readonly).map(f => f.key))
const NO_FLASH = new Set<string>()
const KIND_NOTE: Record<string, string> = { ...KIND_LABEL, new: 'nueva', folder: 'carpeta nueva' }
/** The rows shown: those the ticks keep, and the filter's. */
const shownRows = computed(() =>
  (sheetRows.value ?? []).filter(r => {
    if (!applied.value && (r.create ? skipNew.value.has(r.folder) : skipChanges.value.has(r.folder))) return false
    if (show.value === 'all') return true
    if (show.value === 'flagged') return !!r.checks?.length
    return !!r.checks?.some(c => c.kind === show.value)
  }),
)
const changesShown = computed<ProposalChange[]>(() =>
  shownRows.value.map((r, index) => ({
    index,
    key: r.key,
    recordId: r.recordId ?? null,
    sheet: 'Photo_links',
    row: r.row ?? null,
    label: String(r.values.Name ?? r.rowValues?.Name ?? ''),
    ...(r.create ? { create: true } : {}),
    values: r.values,
    ...(r.rowValues ? { rowValues: r.rowValues } : {}),
    ...(r.personEdits ? { personEdits: r.personEdits } : {}),
    note: [t(KIND_NOTE[r.kind] ?? r.kind), ...(r.checks ?? []).map(flagText)].join(' · '),
  })),
)
const appliedIndexes = computed(() => (applied.value ? changesShown.value.map(c => c.index) : null))
async function loadRows() {
  if (rows.value || rowsGone.value) return
  try {
    const out = await api<{ rows: PhotoRow[]; changes?: SheetRow[] }>(`${base.value}/rows`)
    rows.value = out.rows
    sheetRows.value = out.changes ?? []
  } catch (e) {
    if ((e as { code?: string }).code === 'stale') rowsGone.value = true
    else notify(errorText(e), 'error')
  }
}
watch([showRows, tableOn], () => showRows.value && tableOn.value && void loadRows())
onMounted(() => tableOn.value && void loadRows())
// Read again, or written: the rows it held are another reading's.
watch(
  () => [props.proposal.revision, sync.value.read?.at],
  () => {
    rows.value = null
    sheetRows.value = null
    rowsGone.value = false
    skipNew.value = new Set()
    skipChanges.value = new Set()
    picked.value = []
    if (tableOn.value) void loadRows()
  },
)
/** A cell typed in the table: shown at once (blue), saved to the reading (what «Aplicar» writes). */
async function onEdit(cells: CellEdit[]) {
  const byKey = new Map((sheetRows.value ?? []).map(r => [r.key, r]))
  const sent = cells.flatMap(c => {
    const r = byKey.get(c.key)
    if (!r) return []
    // «Valor de la hoja»: what the sheet has (a new row: empty); «Valor de la IA»: the reading's.
    const value = c.use === 'sheet' ? (r.rowValues?.[c.field] ?? null) : c.value
    const ai = r.personEdits?.[c.field] ? r.personEdits[c.field].ai : r.values[c.field]
    if (c.use === 'sheet' && !r.create && !(c.field in (r.personEdits ?? {})) && !(c.field in r.values)) return []
    r.values = { ...r.values, [c.field]: value }
    r.personEdits = { ...r.personEdits, [c.field]: ai === undefined ? {} : { ai } }
    if (c.use === 'ai' || (c.use === 'sheet' && !r.create)) {
      const { [c.field]: _gone, ...rest } = r.personEdits
      r.personEdits = rest
      if (c.use === 'sheet') {
        const { [c.field]: _was, ...left } = r.values
        r.values = left
      }
    }
    return [{ key: c.key, field: c.field, value }]
  })
  sheetRows.value = [...(sheetRows.value ?? [])]
  if (!sent.length) return
  try {
    await api(`${base.value}/edit`, { method: 'POST', body: { cells: sent } })
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
// The photos of the rows selected in the table (a cell, or a range of them), up to a dozen.
const picked = ref<string[]>([])
const photos = computed(() => {
  const byKey = new Map((sheetRows.value ?? []).map(r => [r.key, r]))
  return picked.value.slice(0, 12).flatMap(key => {
    const r = byKey.get(key)
    const url = String(r?.values.URL ?? r?.rowValues?.URL ?? '')
    const file = driveFileOf(url)
    if (!r || !file) return []
    const name = String(r.values.Name ?? r.rowValues?.Name ?? '')
    return [{ file, name, link: url, note: [t(KIND_NOTE[r.kind] ?? r.kind), String(r.values['Full Path'] ?? r.rowValues?.['Full Path'] ?? '').split('/').slice(0, -1).join('/')].filter(Boolean).join(' · ') }]
  })
})

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
          {{ $t('Quedan para después: {folders}.', { folders: progress.leftFolders.map(f => folderName({ folder: f })).join(', ') }) }}
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

    </template>

      <!-- Every row, and beside it the photos of the rows clicked. -->
      <div v-if="tableOn" class="border-b border-stone-200 px-3 py-1.5 text-xs [container-type:inline-size]" data-rows>
        <div class="flex flex-wrap items-center gap-2">
          <button type="button" class="flex items-center gap-1 font-medium text-emerald-800 hover:underline" data-show-rows @click="showRows = !showRows">
            <ChevronDown v-if="showRows" :size="12" /><ChevronRight v-else :size="12" />
            <template v-if="applied">{{ $tn(sheetRows?.length ?? 0, 'La fila que se escribió', 'Las {n} filas que se escribieron') }}</template>
            <template v-else>{{ $tn(writing, 'La fila que se escribiría', 'Las {n} filas que se escribirían') }}</template>
          </button>
          <select
            v-if="showRows && rows && !applied"
            v-model="show"
            class="rounded border border-stone-200 bg-white py-0.5 pr-6 pl-1.5 text-xs text-stone-800"
            :aria-label="$t('Qué filas mostrar')"
          >
            <option value="all">{{ $t('Todas ({n})', { n: writing }) }}</option>
            <option v-if="flagged" value="flagged">{{ $t('Solo para revisar') }}</option>
            <option v-for="c in checks" :key="c.kind" :value="c.kind">{{ checkLabel(c.kind) }} ({{ c.count }})</option>
          </select>
        </div>
        <p v-if="showRows && rowsGone" class="py-2 text-stone-500">
          {{ applied ? $t('Las filas de esta lectura ya no se guardan.') : $t('Las carpetas se leyeron hace más de media hora: léelas de nuevo.') }}
        </p>
        <p v-if="showRows && sheetRows && !applied" class="mt-1 text-stone-500">
          {{ $t('Como quedarán en la hoja: en verde lo que cambia; puedes corregir una celda antes de aplicar, o copiar y pegar como en la hoja. Selecciona celdas para ver las fotos de sus filas.') }}
        </p>
        <div v-if="showRows && sheetRows" class="card-rows mt-1.5 flex flex-col gap-2">
          <div class="min-w-0 flex-1">
            <ProposalSheet
              sheet="Photo_links"
              :changes="changesShown"
              :fields="fields.map(f => f.key)"
              :types="types"
              :new-row-formulas="formulas"
              :editable="pending && !sync.stale"
              :applied="appliedIndexes"
              :flash="NO_FLASH"
              :history-key="`${proposal.id}:photo-links`"
              @edit="onEdit"
              @rows="keys => (picked = keys)"
              @notice="m => notify(m)"
            />
          </div>
          <CardPhotos :card="proposal.id" :photos="photos" class="card-photos" />
        </div>
      </div>

    <template v-if="pending && !sync.stale">
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

      <!-- What the wings show, by the wing models, on the photos of the folders read (listed before or new). -->
      <div v-if="wingChecks.length" class="border-b border-stone-200 px-3 py-2 text-xs" data-wings>
        <p class="text-stone-700">{{ $t('Lo que las alas muestran en las fotos de estas carpetas (ya listadas o nuevas):') }}</p>
        <ul class="mt-1.5 space-y-0.5">
          <li v-for="c in wingChecks" :key="c.kind" :data-check="c.kind">
            <button
              type="button"
              class="flex w-full min-w-0 items-center gap-1.5 rounded px-1 py-0.5 text-left text-amber-900 hover:bg-amber-50"
              :aria-expanded="openCheck === c.kind"
              @click="openCheck = openCheck === c.kind ? null : c.kind"
            >
              <ChevronDown v-if="openCheck === c.kind" :size="12" class="shrink-0" /><ChevronRight v-else :size="12" class="shrink-0" />
              <AlertTriangle :size="12" class="shrink-0 text-amber-600" />
              <span class="font-medium tabular-nums">{{ c.count }}</span>
              <span>{{ checkLabel(c.kind) }}</span>
            </button>
            <ul v-if="openCheck === c.kind" class="mt-0.5 mb-1 ml-7 space-y-0.5 text-stone-700">
              <li v-for="e in c.examples" :key="e.name + (e.in ?? '')" class="break-all">
                <a v-if="e.url" class="font-medium text-emerald-800 hover:underline" :href="e.url" target="_blank" rel="noopener">{{ e.name }}</a>
                <span v-else class="font-medium">{{ e.name }}</span>
                <span class="text-stone-500"> · {{ flagText({ ...e, kind: c.kind }) }}</span>
                <span v-if="e.in" class="text-stone-400"> · {{ e.in }}</span>
              </li>
              <li v-if="c.count > c.examples.length" class="text-stone-500">
                {{ $t('y {n} más: pídeselos al asistente', { n: c.count - c.examples.length }) }}
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
.photo-links-rows .tabulator-row.is-picked .tabulator-cell {
  background: #d1fae5;
}
.photo-links-rows .tabulator-row {
  cursor: pointer;
}
/* The photos beside the table when the card is wide, under it when narrow. */
.card-rows .card-photos {
  height: 22rem;
}
@container (min-width: 52rem) {
  .card-rows {
    flex-direction: row;
  }
  .card-rows .card-photos {
    width: 40%;
    flex-shrink: 0;
  }
}
.photo-links-rows .tabulator-cell.is-flagged {
  background: #fffbeb;
  color: #78350f;
}
</style>
