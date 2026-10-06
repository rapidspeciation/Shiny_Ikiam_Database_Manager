import { defineStore } from 'pinia'
import { api, ApiError, learnItems, requestId } from '../lib/api'
import { mainPurpose, purposeFromHash } from '../lib/history'
import { t } from '../lib/i18n'
import { type CheckSheet, localProblems } from '../lib/saveChecks'
import type { CellValue, TableRow } from '../lib/types'
import { verificationsFor } from '../lib/verifications'
import { useLive } from './live'
import { useSession } from './session'
import { type ServerRecord, useTables } from './tables'
import { stagedSaving } from '../lib/stagedSwitch'

/** Unsaved changes to an existing sheet row. */
export interface PendingEdit {
  module: string
  id: string
  row: number
  label: string
  version: number
  values: Record<string, CellValue>
  before: Record<string, CellValue>
  /** The tab the change was typed in (colecta, muertes…), kept with the save in the Historial. */
  purpose?: string
}

/** A new row that has not been written to the sheet yet. */
export interface PendingCreate {
  clientId: string
  module: string
  label: string
  values: Record<string, CellValue>
  /** Rows that wait for Guardar (a walk's captures, before their species are identified), never saved automatically. */
  manual?: boolean
  /** The tab the row was added in. */
  purpose?: string
}

interface BatchItemError {
  id?: string
  clientId?: string
  field?: string
  code: string
  message: string
}

const same = (a: CellValue | undefined, b: CellValue | undefined) => (a ?? '') === (b ?? '')
/** The data-entry tab open now (#/colecta…), or undefined. */
const currentPurpose = () => (typeof location === 'undefined' ? undefined : purposeFromHash(location.hash))
const itemKey = (item: BatchItemError) => `${item.id || item.clientId}:${item.field || '*'}`

/** What one save did: cells written, and changes still pending afterwards. */
export interface SaveResult {
  saved: number
  left: number
  /** The save's entry in the Historial (what an Undo right after it reverts), when something was written. */
  actionId?: string
  /** Emergidos and Clutches changes kept in the app (server/staged.mjs), and their entry (to undo it). */
  staged?: number
  stagedEntry?: string
  /** Changes kept in the app until Google answers (server/outbox.mjs). */
  queued?: number
}

/** The tabs whose changes are kept in the app until «Guardar en Google Sheets» (server/staged.mjs). */
export const STAGED_PURPOSES = new Set(['emergidos', 'clutches'])
/** A change kept in the app rather than written: typed in one of those tabs, or to a row entered there. */
export const isStaged = (purpose: string | undefined, rowId = '') =>
  rowId.startsWith('staged:') || (stagedSaving() && STAGED_PURPOSES.has(purpose ?? ''))

/** Changes sent and kept by the server until Google answers (server/outbox.mjs), by outbox id. */
interface QueuedSave {
  edits: PendingEdit[]
  creates: PendingCreate[]
  at: string
}
/** What the server answered for a set of changes (records/batch, staged, an outbox item settled). */
interface SaveAnswer {
  status?: string
  records?: ServerRecord[]
  skipped?: BatchItemError[]
  created?: { clientId: string; recordId?: string }[]
  action?: { id: string } | null
  entryId?: string | null
  outboxId?: string
}

/** Pause after the last edit before saving automatically. */
const AUTO_DELAY = 2500
let autoTimer: ReturnType<typeof setTimeout> | null = null
let autoRetries = 0
let checkTimer: ReturnType<typeof setTimeout> | null = null

/**
 * Edits are kept in the browser (and in localStorage, so a closed tab or a
 * dropped connection loses nothing) until the person presses "Guardar".
 * This mirrors the original app's "Guardar en local" → "Subir cambios" flow.
 */
export const usePending = defineStore('pending', {
  state: () => ({
    edits: {} as Record<string, PendingEdit>,
    creates: [] as PendingCreate[],
    /** Why the server did not save a cell ("rowId:field", or "rowId:*" for the whole row). */
    errors: {} as Record<string, string>,
    /** Why a cell is not sent at all: the app's own checks (bad date, strict list, repeated ID). */
    problems: {} as Record<string, string>,
    saving: false,
    /**
     * One request ID per set of changes, kept until the server gives a definite
     * answer, so retrying a save whose outcome was unclear can never write twice.
     * `requestBody` is what that request sent: a different set gets a new ID.
     */
    requestId: null as string | null,
    requestBody: null as string | null,
    /** The same for the changes kept in the app (Emergidos, Clutches: POST /api/staged). */
    stagedId: null as string | null,
    stagedBody: null as string | null,
    /** Changes the server keeps until Google answers, by outbox id: still shown as pending, not sent again. */
    queued: {} as Record<string, QueuedSave>,
    /** Bumped on every cell change, so checks such as repeated IDs follow typing. */
    edited: 0,
    /** Bumped whenever changes happen outside a grid's own editing (save, discard, bulk fills). */
    revision: 0,
    /** The last save: written to Google Sheets, or kept in the app (Emergidos, Clutches) for «Guardar en Google Sheets». */
    lastSaved: null as { count: number; at: string; where?: 'sheet' | 'app' } | null,
    /** Save automatically a moment after the last change (per person, remembered on this device). */
    autoSave: true,
    /** Why automatic saving is waiting (offline, conflict to review…), if it is: Spanish, shown through $t. */
    autoBlocked: '' as string,
  }),
  getters: {
    changeCount: s => Object.values(s.edits).reduce((n, e) => n + Object.keys(e.values).length, 0) + s.creates.length,
    rowCount: s => Object.keys(s.edits).length + s.creates.length,
    /** Cells and new rows sent and waiting for Google ("rowId:field", a new row's clientId). */
    queuedKeys(s): Set<string> {
      const out = new Set<string>()
      for (const q of Object.values(s.queued)) {
        for (const e of q.edits) for (const field of Object.keys(e.values)) out.add(`${e.id}:${field}`)
        for (const c of q.creates) out.add(c.clientId)
      }
      return out
    },
    queuedCount(): number {
      return this.queuedKeys.size
    },
    /**
     * Pending cells that are not being saved, with the reason (server refusals and the app's checks),
     * in the interface language (reasons are kept in Spanish, the key of their English).
     */
    issues(s): Record<string, string> {
      const newRows = new Set(s.creates.map(c => c.clientId))
      const out: Record<string, string> = {}
      for (const [key, message] of [...Object.entries(s.errors), ...Object.entries(s.problems)]) {
        const [id, field] = [key.slice(0, key.lastIndexOf(':')), key.slice(key.lastIndexOf(':') + 1)]
        // A refusal of a cell that is no longer pending (put back, or saved since) no longer counts.
        const stillPending = newRows.has(id) || (!!s.edits[id] && (field === '*' || field in s.edits[id].values))
        if (stillPending || !id || id === 'null' || id === 'undefined') out[key] ??= t(message)
      }
      return out
    },
  },
  actions: {
    storageKey() {
      return `ithomiini:pending:${useSession().user?.username || 'anon'}`
    },
    setAutoSave(on: boolean) {
      this.autoSave = on
      localStorage.setItem(`${this.storageKey()}:auto`, on ? '1' : '0')
      if (on) this.scheduleAutoSave(500)
    },
    /** Saves all pending changes shortly after the last edit (one atomic batch). */
    scheduleAutoSave(delay = AUTO_DELAY) {
      if (autoTimer) clearTimeout(autoTimer)
      autoTimer = null
      if (!this.autoSave || !this.changeCount) return
      autoTimer = setTimeout(() => this.runAutoSave(), delay)
    },
    async runAutoSave() {
      autoTimer = null
      if (!this.autoSave || !this.changeCount) return
      if (this.saving) return this.scheduleAutoSave(1500)
      if (!navigator.onLine) {
        this.autoBlocked = 'Sin conexión: se guardará al volver la conexión'
        return
      }
      try {
        // Cells refused before wait until they are edited again; everything else is saved.
        await this.save('', { retryRefused: false, auto: true })
        autoRetries = 0
        this.autoBlocked = ''
      } catch (e) {
        const code = e instanceof ApiError ? e.code : ''
        if (['WRITE_UNCERTAIN', 'OFFLINE', 'SERVER_ERROR'].includes(code) || (e instanceof ApiError && e.status >= 500)) {
          autoRetries++
          this.autoBlocked = 'No se pudo guardar; se reintentará'
          this.scheduleAutoSave(Math.min(60_000, 5_000 * 2 ** autoRetries))
        } else this.autoBlocked = 'Hay cambios por revisar antes de guardar'
      }
    },
    /** Loads the signed-in person's unsaved changes, and nobody else's. */
    restore() {
      this.edits = {}
      this.creates = []
      this.errors = {}
      this.problems = {}
      this.requestId = null
      this.requestBody = null
      this.stagedId = null
      this.stagedBody = null
      this.queued = {}
      this.autoSave = localStorage.getItem(`${this.storageKey()}:auto`) !== '0'
      this.autoBlocked = ''
      try {
        const saved = JSON.parse(localStorage.getItem(this.storageKey()) || 'null')
        if (saved) {
          this.edits = saved.edits || {}
          this.creates = saved.creates || []
          this.requestId = saved.requestId || null
          this.requestBody = saved.requestBody || null
          this.stagedId = saved.stagedId || null
          this.stagedBody = saved.stagedBody || null
          this.queued = saved.queued || {}
        }
      } catch {
        /* ignore unreadable drafts */
      }
      this.revision++
      this.scheduleCheck()
    },
    /** Called on sign-out: the drafts stay saved for that person, but leave the screen. */
    clear() {
      this.edits = {}
      this.creates = []
      this.errors = {}
      this.problems = {}
      this.requestId = null
      this.requestBody = null
      this.stagedId = null
      this.stagedBody = null
      this.queued = {}
      this.revision++
    },
    persist(changed = true) {
      if (changed) {
        this.requestId = null
        this.requestBody = null
        this.stagedId = null
        this.stagedBody = null
        this.edited++
      }
      localStorage.setItem(
        this.storageKey(),
        JSON.stringify({
          edits: this.edits,
          creates: this.creates,
          requestId: this.requestId,
          requestBody: this.requestBody,
          stagedId: this.stagedId,
          stagedBody: this.stagedBody,
          queued: this.queued,
        }),
      )
      if (changed) {
        this.autoBlocked = ''
        this.scheduleCheck()
        this.scheduleAutoSave()
      }
    },
    /** Runs the app's checks a moment after the last change (a fill or a paste changes many cells at once). */
    scheduleCheck() {
      if (checkTimer) clearTimeout(checkTimer)
      checkTimer = setTimeout(() => {
        checkTimer = null
        this.check()
      }, 200)
    },
    /** The app's own checks on every pending cell (lib/saveChecks.ts). */
    check() {
      const tables = useTables()
      const modules = [...Object.values(this.edits).map(e => e.module), ...this.creates.map(c => c.module)]
      const sheets: Record<string, CheckSheet | undefined> = {}
      // Every loaded sheet takes part: tube IDs must not repeat anywhere in the workbook.
      for (const module of new Set([...modules, ...Object.keys(tables.tables)])) {
        const table = tables.tables[module]
        if (table) sheets[module] = { rows: table.rows, columns: table.columns, rules: verificationsFor(module) }
      }
      this.problems = localProblems(
        [
          ...Object.values(this.edits).map(e => ({ module: e.module, id: e.id, isNew: false, values: e.values })),
          ...this.creates.map(c => ({ module: c.module, id: c.clientId, isNew: true, values: c.values })),
        ],
        sheets,
      )
    },
    value(row: TableRow, field: string): CellValue {
      const edit = this.edits[row.id]
      return edit && field in edit.values ? edit.values[field] : (row.values[field] ?? null)
    },
    isDirty(id: string, field: string) {
      return !!this.edits[id] && field in this.edits[id].values
    },
    setCell(module: string, row: TableRow, label: string, field: string, value: CellValue) {
      const original = row.values[field] ?? null
      let edit = this.edits[row.id]
      if (same(value, original)) {
        if (edit) {
          delete edit.values[field]
          delete edit.before[field]
          if (!Object.keys(edit.values).length) delete this.edits[row.id]
        }
      } else {
        if (!edit)
          edit = this.edits[row.id] = { module, id: row.id, row: row.row, label, version: row.version, values: {}, before: {} }
        edit.values[field] = value
        edit.before[field] = original
        edit.purpose = currentPurpose() ?? edit.purpose
      }
      delete this.errors[`${row.id}:${field}`]
      this.persist()
    },
    addCreate(module: string, label: string, values: Record<string, CellValue>, { manual = false } = {}) {
      const purpose = currentPurpose()
      const item: PendingCreate = {
        clientId: requestId(),
        module,
        label,
        values,
        ...(manual ? { manual } : {}),
        ...(purpose ? { purpose } : {}),
      }
      this.creates.push(item)
      this.persist()
      return item
    },
    updateCreate(clientId: string, field: string, value: CellValue) {
      const item = this.creates.find(c => c.clientId === clientId)
      if (!item) return
      item.values[field] = value
      delete this.errors[`${clientId}:${field}`]
      this.persist()
    },
    removeCreate(clientId: string) {
      this.creates = this.creates.filter(c => c.clientId !== clientId)
      this.persist()
    },
    discard(module?: string) {
      for (const [id, edit] of Object.entries(this.edits)) if (!module || edit.module === module) delete this.edits[id]
      this.creates = this.creates.filter(c => module && c.module !== module)
      this.errors = {}
      this.problems = {}
      this.persist()
      this.revision++
    },
    /** Call after changing many cells programmatically so grids redraw. */
    touch() {
      this.revision++
    },
    /** A cell (or a new row: field omitted) sent and waiting for Google. */
    isQueued(id: string, field?: string) {
      return this.queuedKeys.has(field === undefined ? id : `${id}:${field}`)
    },
    /**
     * Sends every pending change that can be saved; the others stay pending and
     * red with the reason. Cells failing the app's checks are never sent; cells
     * the server refused before are sent again unless `retryRefused` is false
     * (automatic saving waits until they are edited). The server saves what it
     * can and lists what it left out (records/batch with `partial`).
     * Changes typed in Emergidos and Clutches are kept in the app for everyone
     * (POST /api/staged) until «Guardar en Google Sheets»; a save Google cannot
     * take now is kept by the server and written when it answers (`queued`).
     */
    async save(reason: string, { retryRefused = true, auto = false } = {}): Promise<SaveResult> {
      if (this.saving || !this.changeCount) return { saved: 0, left: 0 }
      this.check()
      const held = (key: string) => !!this.problems[key] || (!retryRefused && !!this.errors[key])
      const heldRow = (id: string) =>
        Object.keys(this.problems).some(k => k.startsWith(`${id}:`)) ||
        (!retryRefused && Object.keys(this.errors).some(k => k.startsWith(`${id}:`)))
      // Waiting for Google already: not sent again (a change made to it meanwhile goes once it is written).
      const queued = this.queuedKeys
      const busyRow = (id: string) => [...queued].some(k => k.startsWith(`${id}:`))
      // Snapshot what is sent: people may keep editing while the save runs.
      // (JSON copy: the store's objects are reactive proxies and plain JSON data.)
      const edits = (JSON.parse(JSON.stringify(Object.values(this.edits))) as PendingEdit[])
        .filter(e => !held(`${e.id}:*`) && !busyRow(e.id))
        .map(e => {
          for (const field of Object.keys(e.values))
            if (held(`${e.id}:${field}`)) {
              delete e.values[field]
              delete e.before[field]
            }
          return e
        })
        .filter(e => Object.keys(e.values).length)
      // A new row goes whole or not at all.
      const creates = (JSON.parse(JSON.stringify(this.creates)) as PendingCreate[]).filter(
        c => !heldRow(c.clientId) && !(auto && c.manual) && !queued.has(c.clientId),
      )
      if (!edits.length && !creates.length) return { saved: 0, left: this.changeCount }
      const groups = [
        {
          staged: true,
          edits: edits.filter(e => isStaged(e.purpose, e.id)),
          creates: creates.filter(c => isStaged(c.purpose)),
        },
        {
          staged: false,
          edits: edits.filter(e => !isStaged(e.purpose, e.id)),
          creates: creates.filter(c => !isStaged(c.purpose)),
        },
      ].filter(g => g.edits.length || g.creates.length)
      this.saving = true
      this.persist(false)
      const out: SaveResult = { saved: 0, left: 0 }
      let failure: unknown = null
      try {
        for (const group of groups) {
          try {
            const r = await this.send(group, reason)
            out.saved += r.saved
            if (r.staged) out.staged = (out.staged ?? 0) + r.staged
            if (r.queued) out.queued = (out.queued ?? 0) + r.queued
            if (r.actionId) out.actionId ??= r.actionId
            if (r.stagedEntry) out.stagedEntry ??= r.stagedEntry
          } catch (e) {
            failure ??= e
          }
        }
      } finally {
        this.saving = false
      }
      out.left = this.changeCount
      if (failure) throw failure
      return out
    },
    /** One request: the changes of the staged tabs (POST /api/staged) or the others (records/batch). */
    async send(group: { staged: boolean; edits: PendingEdit[]; creates: PendingCreate[] }, reason: string): Promise<SaveResult> {
      const { edits, creates } = group
      // The tab most of these changes were typed in: the save's purpose in the Historial.
      const purpose =
        mainPurpose([...edits.map(e => e.purpose), ...creates.map(c => c.purpose)]) ??
        (group.staged ? (currentPurpose() && STAGED_PURPOSES.has(currentPurpose()!) ? currentPurpose() : 'emergidos') : currentPurpose())
      const body = {
        reason: reason || null,
        ...(purpose ? { purpose } : {}),
        // Only the changes that conflict are left out; the rest is written.
        partial: true,
        // Each cell is checked against the value the person saw, so edits by
        // others to different cells of the same row do not block the save.
        edits: edits.map(e => ({ id: e.id, values: e.values, expected: e.before })),
        // A typed species replaces the clutch's prediction only where it differs (the server checks).
        creates: creates.map(c => ({
          clientId: c.clientId,
          module: c.module,
          values: c.values,
          ...(c.module === 'Insectary_data'
            ? { replaceFormula: ['SPECIES', 'Collection_location'].filter(f => c.values[f]) }
            : {}),
        })),
      }
      const fingerprint = JSON.stringify(body)
      let id: string
      if (group.staged) {
        if (!this.stagedId || this.stagedBody !== fingerprint) [this.stagedId, this.stagedBody] = [requestId(), fingerprint]
        id = this.stagedId
      } else {
        if (!this.requestId || this.requestBody !== fingerprint) [this.requestId, this.requestBody] = [requestId(), fingerprint]
        id = this.requestId
      }
      this.persist(false)
      const forget = () => {
        if (group.staged) [this.stagedId, this.stagedBody] = [null, null]
        else [this.requestId, this.requestBody] = [null, null]
      }
      try {
        const result = await api<SaveAnswer>(group.staged ? 'staged' : 'records/batch', {
          method: 'POST',
          body: { requestId: id, ...body },
        })
        forget()
        // Google is not answering: the server keeps them and writes them when it does (resolveQueued).
        if (result.status === 'queued' && result.outboxId) {
          this.queued[result.outboxId] = { edits, creates, at: new Date().toISOString() }
          this.persist(false)
          this.revision++
          return { saved: 0, left: this.changeCount, queued: edits.reduce((n, e) => n + Object.keys(e.values).length, 0) + creates.length }
        }
        // The entries everyone sees come with them, so the cells never show the sheet's old value in between.
        if (group.staged) await useLive().loadStaged()
        const saved = this.settle(result, edits, creates, group.staged ? 'app' : 'sheet')
        return {
          saved: group.staged ? 0 : saved,
          left: this.changeCount,
          ...(group.staged ? { staged: saved, ...(result.entryId ? { stagedEntry: result.entryId } : {}) } : {}),
          ...(result.action?.id ? { actionId: result.action.id } : {}),
        }
      } catch (e) {
        const code = e instanceof ApiError ? e.code : ''
        // Only an unclear outcome keeps the request ID for a safe retry.
        if (!['WRITE_UNCERTAIN', 'OFFLINE', 'SERVER_ERROR'].includes(code)) {
          forget()
          this.persist(false)
        }
        if (e instanceof ApiError && Array.isArray((e.details as { items?: unknown })?.items)) {
          this.clearSentErrors(edits, creates)
          for (const item of (e.details as { items: BatchItemError[] }).items) this.errors[itemKey(item)] = item.message
          this.revision++
        }
        throw e
      }
    },
    /** The answer replaces earlier refusals of what was sent. */
    clearSentErrors(edits: PendingEdit[], creates: PendingCreate[]) {
      // Refusals that named no row (e.g. the sheet's columns changed) are answered again by this save.
      for (const key of Object.keys(this.errors)) if (/^(null|undefined)?:/.test(key)) delete this.errors[key]
      for (const e of edits) {
        delete this.errors[`${e.id}:*`]
        for (const field of Object.keys(e.values)) delete this.errors[`${e.id}:${field}`]
      }
      for (const c of creates)
        for (const key of Object.keys(this.errors)) if (key.startsWith(`${c.clientId}:`)) delete this.errors[key]
    },
    /**
     * What was saved (written, or kept in the app) leaves the pending changes, unless it was
     * changed again meanwhile; what was left out stays, red with why. Returns how many were saved.
     */
    settle(result: SaveAnswer, edits: PendingEdit[], creates: PendingCreate[], where: 'sheet' | 'app' = 'sheet'): number {
      useTables().merge(result.records ?? [])
      const skipped = new Set((result.skipped || []).map(itemKey))
      this.clearSentErrors(edits, creates)
      learnItems(result.skipped)
      for (const item of result.skipped || []) this.errors[itemKey(item)] = item.message
      // Remove only what was saved and has not been changed again since.
      let saved = 0
      for (const sent of edits) {
        const current = this.edits[sent.id]
        for (const [field, value] of Object.entries(sent.values)) {
          if (skipped.has(`${sent.id}:${field}`) || skipped.has(`${sent.id}:*`)) continue
          saved++
          if (current && current.values[field] === value) {
            delete current.values[field]
            delete current.before[field]
          } else if (current && field in current.values) current.before[field] = value // changed again: the next save expects what was saved
        }
        if (current && !Object.keys(current.values).length) delete this.edits[sent.id]
      }
      // New rows are written together or left out together (server/batch.mjs).
      const written = new Set((result.created || []).map(c => c.clientId))
      for (const c of creates)
        if (!written.has(c.clientId) && ![...skipped].some(k => k.startsWith(`${c.clientId}:`)))
          this.errors[`${c.clientId}:*`] = 'No se guardó: otra fila nueva de este guardado necesita revisión'
      const sentCreates = new Map(creates.map(c => [c.clientId, JSON.stringify(c.values)]))
      saved += written.size
      this.creates = this.creates.filter(c => !written.has(c.clientId) || sentCreates.get(c.clientId) !== JSON.stringify(c.values))
      this.persist(false)
      this.check()
      if (saved) this.lastSaved = { count: saved, at: new Date().toISOString(), where }
      this.revision++
      return saved
    },
    /**
     * Saves kept by the server until Google answered (server/outbox.mjs): once written, they
     * leave the pending changes as a save does; refused, their cells stay, red with why.
     */
    async resolveQueued() {
      for (const [id, q] of Object.entries(this.queued)) {
        let answer: SaveAnswer & { outbox?: { status: string }; error?: { message: string; details?: { items?: BatchItemError[] } } }
        try {
          answer = await api(`outbox/${encodeURIComponent(id)}`)
        } catch (e) {
          if (e instanceof ApiError && e.status === 404) {
            delete this.queued[id]
            this.persist(false)
          }
          continue
        }
        const status = answer.outbox?.status
        if (!status || status === 'queued' || status === 'writing') continue
        delete this.queued[id]
        if (status === 'done') {
          this.settle(answer, q.edits, q.creates)
          continue
        }
        const items = answer.error?.details?.items ?? []
        learnItems(items)
        this.clearSentErrors(q.edits, q.creates)
        for (const item of items) this.errors[itemKey(item)] = item.message
        if (!items.length)
          for (const e of q.edits) this.errors[`${e.id}:*`] = answer.error?.message || 'No se guardó'
        this.persist(false)
        this.revision++
      }
    },
  },
})
