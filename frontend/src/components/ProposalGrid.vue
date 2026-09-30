<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, ref, watch } from 'vue'
import { Check, Plus, Sparkles, X } from 'lucide-vue-next'
import ProposalSheet, { type CellEdit } from './assistant/ProposalSheet.vue'
import { api } from '../lib/api'
import { displayValue } from '../lib/cells'
import { errorText, notify } from '../lib/notice'
import { persistentRef } from '../lib/persist'
import {
  cellId,
  changedCells,
  changedText,
  notApplied,
  rowKey,
  rowsToWrite,
  sheetGroups,
  withLocal,
  type LocalCell,
  type Proposal,
  type ProposalChange,
} from '../lib/proposals'
import type { CellValue } from '../lib/types'
import { useSession } from '../stores/session'
import { t } from '../lib/i18n'

export type { Proposal, ProposalChange } from '../lib/proposals'

/**
 * A proposal of the assistant as an editable sheet (one table per sheet it
 * touches): the assistant's values in green, the person's in blue. The person
 * corrects cells as in Colecta and they are saved to the proposal at once
 * (the assistant sees them and does not overwrite them); what the assistant
 * changes meanwhile flashes. Selected cells go back to the sheet's value (the
 * assistant's kept aside, marked) or take the assistant's again; "Aplicar"
 * writes what the table shows (as does "aplica" in the chat).
 */
const props = defineProps<{ proposal: Proposal; busy?: boolean }>()
const emit = defineEmits<{
  apply: [indexes: number[], revision: number | undefined]
  discard: []
  replace: [proposal: Proposal]
}>()
const session = useSession()

const pending = computed(() => props.proposal.status === 'pending')
const editable = computed(() => pending.value && session.canEdit)

// ------------------------------------------------------------ the person's edits, saved to the proposal
/** Typed and not yet saved (laid over the server's copy). */
const local = ref(new Map<string, LocalCell>())
const shown = computed(() => withLocal(props.proposal, local.value))
const queue = new Map<string, CellEdit>()
let removes: string[] = []
let adds: { sheet: string }[] = []
/** What was sent lately: its echo from the server must not flash as the assistant's change. */
const sent = new Map<string, { value: CellValue; at: number }>()
const saving = ref(false)
const failed = ref(false)
let timer: number | undefined
let running: Promise<void> | null = null

function onEdit(cells: CellEdit[]) {
  const next = new Map(local.value)
  for (const c of cells) {
    const id = cellId(c.key, c.field)
    next.set(id, c.use ? { value: c.value, use: c.use } : { value: c.value })
    // The first "before" is what the person saw.
    queue.set(id, { ...c, before: queue.get(id)?.before ?? c.before })
  }
  local.value = next
  later()
}
function later(ms = 500) {
  window.clearTimeout(timer)
  timer = window.setTimeout(() => void save(), ms)
}
/** Sends what waits, one request at a time. */
async function save(): Promise<void> {
  window.clearTimeout(timer)
  if (running) {
    await running
    return save()
  }
  if (!queue.size && !removes.length && !adds.length) return
  const cells = [...queue.values()]
  const body = { cells, remove: removes, add: adds }
  queue.clear()
  removes = []
  adds = []
  saving.value = true
  running = (async () => {
    try {
      const out = await api<{
        proposal: Proposal
        rejected: { label?: string; field?: string; message: string }[]
        overrode: { label?: string; field: string; ai: CellValue }[]
      }>(`chat/proposals/${props.proposal.id}/edit`, { method: 'POST', body })
      failed.value = false
      const now = Date.now()
      for (const c of cells) sent.set(cellId(c.key, c.field), { value: c.value, at: now })
      dropSaved(cells)
      savedRevision = Math.max(savedRevision, out.proposal.revision ?? 0)
      emit('replace', out.proposal)
      for (const r of out.rejected.slice(0, 3)) {
        const what = [r.field, r.label ? `(${r.label})` : ''].filter(Boolean).join(' ')
        const message = t(r.message)
        notify(what ? t('No se guardó {what}: {message}', { what, message }) : t('No se guardó: {message}', { message }), 'error')
      }
      for (const o of out.overrode.slice(0, 3))
        notify(
          t('Escribiste encima de un cambio de la IA en {field}: proponía {value}', {
            field: `${o.field}${o.label ? ` (${o.label})` : ''}`,
            value: show(o.field, o.ai) || t('vacío'),
          }),
        )
    } catch (e) {
      const code = (e as { code?: string }).code
      if (code === 'OFFLINE' || (e as { status?: number }).status === 0) {
        // Kept, and tried again: nothing typed is lost.
        failed.value = true
        for (const c of cells) if (!queue.has(cellId(c.key, c.field))) queue.set(cellId(c.key, c.field), c)
        removes.push(...body.remove)
        adds.push(...body.add)
        later(5000)
      } else {
        dropSaved(cells)
        notify(errorText(e), 'error')
      }
    } finally {
      saving.value = false
      running = null
    }
  })()
  return running
}
function dropSaved(cells: CellEdit[]) {
  const next = new Map(local.value)
  for (const c of cells) {
    const id = cellId(c.key, c.field)
    const mine = next.get(id)
    if (!queue.has(id) && mine?.use === c.use && JSON.stringify(mine?.value) === JSON.stringify(c.value)) next.delete(id)
  }
  local.value = next
}
onBeforeUnmount(() => {
  // Leaving the page (or the panel) still saves what was typed.
  if (queue.size || removes.length || adds.length) void save()
})

function removeRow(key: string) {
  removes.push(key)
  later(0)
}
function addRow(sheet: string) {
  adds.push({ sheet })
  later(0)
}

// ------------------------------------------------------------ what the assistant changes, live
const flash = ref(new Set<string>())
const flashText = ref('')
let flashTimer: number | undefined
watch(
  () => props.proposal,
  (next, prev) => {
    const now = Date.now()
    for (const [id, s] of sent) if (now - s.at > 30000) sent.delete(id)
    const valueOf = (id: string) => {
      const [key, field] = id.split('\u0000')
      const c = next.changes.find(x => rowKey(x) === key)
      return c && field in c.values ? c.values[field] : undefined
    }
    const cells = changedCells(prev, next).filter(id => {
      if (local.value.has(id)) return false
      const mine = sent.get(id)
      return !mine || JSON.stringify(mine.value ?? null) !== JSON.stringify(valueOf(id) ?? null)
    })
    if (!cells.length) return
    flash.value = new Set(cells)
    flashText.value = changedText(cells.length)
    window.clearTimeout(flashTimer)
    flashTimer = window.setTimeout(() => {
      flash.value = new Set()
      flashText.value = ''
    }, 2600)
  },
)
onBeforeUnmount(() => window.clearTimeout(flashTimer))

// ------------------------------------------------------------ columns, apply
/** The rows "Aplicar" writes: what the table shows (a row set back to the sheet in every cell is left out). */
const chosen = computed(() => rowsToWrite(shown.value))
const setAside = computed(() => notApplied(shown.value))

/** Columns the person added to a sheet's table (kept while the tab is open). */
const extra = persistentRef<Record<string, string[]>>(`proposal-columns:${props.proposal.id}`, {})
const fieldsOf = (sheet: string) => session.module(sheet)?.fields
const groups = computed(() => sheetGroups(shown.value, extra.value, sheet => fieldsOf(sheet)?.map(f => f.key)))
const typesOf = (sheet: string) => ({
  ...Object.fromEntries((fieldsOf(sheet) ?? []).map(f => [f.key, f.type])),
  ...props.proposal.types,
})
/** Columns that can still be added to a sheet's table. */
const addable = (sheet: string, fields: string[]) =>
  (fieldsOf(sheet) ?? []).filter(f => !f.readonly && !f.unavailable && !fields.includes(f.key)).map(f => f.key)
function addColumn(sheet: string, event: Event) {
  const select = event.target as HTMLSelectElement
  if (select.value) extra.value = { ...extra.value, [sheet]: [...(extra.value[sheet] ?? []), select.value] }
  select.value = ''
}

/** The revision the last save of the person's edits produced (the list may not show it yet). */
let savedRevision = 0
async function apply() {
  // What was just typed goes into the proposal first.
  await save()
  await nextTick()
  emit('apply', chosen.value, Math.max(props.proposal.revision ?? 1, savedRevision) || undefined)
}

const show = (field: string, value: CellValue | undefined) =>
  displayValue(value, { key: field, type: (props.proposal.types[field] ?? 'text') as 'text' })
const created = computed(() => props.proposal.changes.filter(c => c.create).length)
const personCells = computed(() => shown.value.changes.reduce((n, c) => n + Object.keys(c.personEdits ?? {}).length, 0))
const statusText = computed(
  () =>
    ({
      applied: t('Aplicado en la hoja ({n} filas)', { n: props.proposal.applied?.length ?? props.proposal.changes.length }),
      needs_review: t('No se pudo aplicar: revisa las filas en la hoja'),
      discarded: t('Descartado'),
      applying: t('Aplicando…'),
    })[props.proposal.status as string] ?? '',
)
</script>

<template>
  <div class="mt-2 rounded-md border border-stone-300 bg-white text-stone-800">
    <p class="flex flex-wrap items-center gap-x-2 border-b border-stone-200 px-2 py-1.5 text-xs font-medium">
      <span>
        {{ $t('Cambios propuestos') }} · {{ (proposal.sheets ?? [proposal.changes[0]?.sheet]).join(', ') }}
        <template v-if="created"> · {{ $tn(created, '{n} fila nueva', '{n} filas nuevas') }}</template>
        <span class="font-normal text-stone-500">— {{ proposal.reason }}</span>
      </span>
      <span
        v-if="flashText"
        class="flex items-center gap-1 rounded bg-amber-100 px-1.5 py-0.5 font-normal text-amber-900"
        role="status"
      >
        <Sparkles :size="12" /> {{ flashText }}
      </span>
    </p>
    <div v-for="g in groups" :key="g.sheet" class="border-b border-stone-100 last:border-b-0">
      <!-- The table and its bar, where ProposalSheet adds the buttons for the selected cells (Valor de la hoja / de la IA). -->
      <ProposalSheet
        :sheet="g.sheet"
        :changes="g.changes"
        :fields="g.fields"
        :types="typesOf(g.sheet)"
        :new-row-formulas="proposal.newRowFormulas?.[g.sheet] ?? []"
        :editable="editable"
        :applied="proposal.status === 'applied' ? (proposal.applied ?? []) : null"
        :flash="flash"
        @edit="onEdit"
        @remove="removeRow"
        @notice="m => notify(m)"
      >
        <template v-if="groups.length > 1 || editable" #default>
          <span v-if="groups.length > 1" class="font-medium text-stone-700">{{ g.sheet }}</span>
          <button
            v-if="editable && g.changes.some(c => c.create)"
            class="flex items-center gap-0.5 hover:text-emerald-800"
            :title="$t('Añadir una fila nueva vacía a esta hoja')"
            @click="addRow(g.sheet)"
          >
            <Plus :size="12" /> {{ $t('Fila') }}
          </button>
          <select
            v-if="editable && addable(g.sheet, g.fields).length"
            class="rounded border border-stone-200 bg-white px-1 py-0.5 text-[11px]"
            :aria-label="$t('Añadir columna')"
            @change="addColumn(g.sheet, $event)"
          >
            <option value="">{{ $t('+ Columna…') }}</option>
            <option v-for="f in addable(g.sheet, g.fields)" :key="f" :value="f">{{ f }}</option>
          </select>
        </template>
        <template v-if="editable" #end>
          <span class="ml-auto flex items-center gap-2">
            <span class="legend is-proposed" :title="$t('Valor de la IA: se escribe al aplicar')">{{ $t('IA') }}</span>
            <span class="legend is-person" :title="$t('Escrito por ti: se escribe al aplicar')">{{ $t('tú') }}</span>
            <span class="legend is-sheet" :title="$t('Valor actual de la hoja: no cambia')">{{ $t('hoja') }}</span>
            <span class="legend is-reverted" :title="$t('Vuelto al valor de la hoja: la sugerencia de la IA no se aplica')">{{
              $t('IA sin aplicar')
            }}</span>
          </span>
        </template>
      </ProposalSheet>
    </div>
    <div class="flex flex-wrap items-center gap-2 px-2 py-1.5">
      <template v-if="pending">
        <button class="btn-primary bg-emerald-700 hover:bg-emerald-800" :disabled="busy || !chosen.length" @click="apply">
          <Check :size="15" /> {{ $tn(chosen.length, 'Aplicar {n} fila', 'Aplicar {n} filas') }}
        </button>
        <button class="btn" :disabled="busy" @click="emit('discard')"><X :size="15" /> {{ $t('Descartar') }}</button>
        <span class="hint">
          <template v-if="saving">{{ $t('Guardando tus cambios…') }}</template>
          <template v-else-if="failed">{{ $t('Sin conexión: tus cambios se guardarán al volver') }}</template>
          <template v-else>
            <template v-if="personCells > setAside"
              >{{ $tn(personCells - setAside, '{n} celda editada por ti', '{n} celdas editadas por ti') }} ·
            </template>
            <template v-if="setAside"
              >{{ $tn(setAside, '{n} sugerencia de la IA sin aplicar', '{n} sugerencias de la IA sin aplicar') }} ·
            </template>
          </template>
          {{ $t('Corrige en la tabla o díselo al asistente; también puedes responder «sí, aplícalo» en el chat.') }}
        </span>
      </template>
      <span v-else class="text-xs" :class="proposal.status === 'applied' ? 'text-brand-700' : 'text-amber-800'">
        {{ statusText }}
      </span>
    </div>
  </div>
</template>
