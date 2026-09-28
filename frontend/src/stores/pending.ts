import { defineStore } from 'pinia'
import { api, ApiError, requestId } from '../lib/api'
import type { CellValue, TableRow } from '../lib/types'
import { useSession } from './session'
import { type ServerRecord, useTables } from './tables'

/** Unsaved changes to an existing sheet row. */
export interface PendingEdit {
  module: string
  id: string
  row: number
  label: string
  version: number
  values: Record<string, CellValue>
  before: Record<string, CellValue>
}

/** A new row that has not been written to the sheet yet. */
export interface PendingCreate {
  clientId: string
  module: string
  label: string
  values: Record<string, CellValue>
}

interface BatchItemError {
  id?: string
  clientId?: string
  field?: string
  code: string
  message: string
}

const same = (a: CellValue | undefined, b: CellValue | undefined) => (a ?? '') === (b ?? '')

/** Pause after the last edit before saving automatically. */
const AUTO_DELAY = 2500
let autoTimer: ReturnType<typeof setTimeout> | null = null
let autoRetries = 0

/**
 * Edits are kept in the browser (and in localStorage, so a closed tab or a
 * dropped connection loses nothing) until the person presses "Guardar".
 * This mirrors the original app's "Guardar en local" → "Subir cambios" flow.
 */
export const usePending = defineStore('pending', {
  state: () => ({
    edits: {} as Record<string, PendingEdit>,
    creates: [] as PendingCreate[],
    errors: {} as Record<string, string>,
    saving: false,
    /**
     * One request ID per set of changes, kept until the server gives a definite
     * answer, so retrying a save whose outcome was unclear can never write twice.
     */
    requestId: null as string | null,
    /** Bumped whenever changes happen outside a grid's own editing (save, discard, bulk fills). */
    revision: 0,
    lastSaved: null as { count: number; at: string } | null,
    /** Save automatically a moment after the last change (per person, remembered on this device). */
    autoSave: true,
    /** Why automatic saving is waiting (offline, conflict to review…), if it is. */
    autoBlocked: '' as string,
  }),
  getters: {
    changeCount: s => Object.values(s.edits).reduce((n, e) => n + Object.keys(e.values).length, 0) + s.creates.length,
    rowCount: s => Object.keys(s.edits).length + s.creates.length,
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
      // Conflicts need a person: automatic saving waits until the flagged cells are edited.
      if (Object.keys(this.errors).length) {
        this.autoBlocked = 'Hay cambios por revisar antes de guardar'
        return
      }
      try {
        await this.save('')
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
      this.requestId = null
      this.autoSave = localStorage.getItem(`${this.storageKey()}:auto`) !== '0'
      this.autoBlocked = ''
      try {
        const saved = JSON.parse(localStorage.getItem(this.storageKey()) || 'null')
        if (saved) {
          this.edits = saved.edits || {}
          this.creates = saved.creates || []
          this.requestId = saved.requestId || null
        }
      } catch {
        /* ignore unreadable drafts */
      }
      this.revision++
    },
    /** Called on sign-out: the drafts stay saved for that person, but leave the screen. */
    clear() {
      this.edits = {}
      this.creates = []
      this.errors = {}
      this.requestId = null
      this.revision++
    },
    persist(changed = true) {
      if (changed) this.requestId = null
      localStorage.setItem(
        this.storageKey(),
        JSON.stringify({ edits: this.edits, creates: this.creates, requestId: this.requestId }),
      )
      if (changed) {
        this.autoBlocked = ''
        this.scheduleAutoSave()
      }
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
      }
      delete this.errors[`${row.id}:${field}`]
      this.persist()
    },
    addCreate(module: string, label: string, values: Record<string, CellValue>) {
      const item = { clientId: requestId(), module, label, values }
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
      this.persist()
      this.revision++
    },
    /** Call after changing many cells programmatically so grids redraw. */
    touch() {
      this.revision++
    },
    async save(reason: string) {
      if (this.saving || !this.changeCount) return
      this.saving = true
      if (!this.requestId) this.requestId = requestId()
      this.persist(false)
      // Snapshot what is sent: people may keep editing while the save runs.
      // (JSON copy: the store's objects are reactive proxies and plain JSON data.)
      const edits: PendingEdit[] = JSON.parse(JSON.stringify(Object.values(this.edits)))
      const creates: PendingCreate[] = JSON.parse(JSON.stringify(this.creates))
      try {
        const result = await api<{ records: ServerRecord[] }>('records/batch', {
          method: 'POST',
          body: {
            requestId: this.requestId,
            reason: reason || null,
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
          },
        })
        useTables().merge(result.records)
        const count = edits.reduce((n, e) => n + Object.keys(e.values).length, 0) + creates.length
        // Remove only what was saved and has not been changed again since.
        for (const sent of edits) {
          const current = this.edits[sent.id]
          if (!current) continue
          for (const [field, value] of Object.entries(sent.values))
            if (current.values[field] === value) {
              delete current.values[field]
              delete current.before[field]
            }
          if (!Object.keys(current.values).length) delete this.edits[sent.id]
        }
        const sentCreates = new Map(creates.map(c => [c.clientId, JSON.stringify(c.values)]))
        this.creates = this.creates.filter(c => sentCreates.get(c.clientId) !== JSON.stringify(c.values))
        this.errors = {}
        this.persist()
        this.lastSaved = { count, at: new Date().toISOString() }
        this.revision++
      } catch (e) {
        const code = e instanceof ApiError ? e.code : ''
        // Only an unclear outcome keeps the request ID for a safe retry.
        if (!['WRITE_UNCERTAIN', 'OFFLINE', 'SERVER_ERROR'].includes(code)) this.persist()
        if (e instanceof ApiError && Array.isArray((e.details as { items?: unknown })?.items)) {
          const items = (e.details as { items: BatchItemError[] }).items
          this.errors = {}
          for (const item of items) this.errors[`${item.id || item.clientId}:${item.field || '*'}`] = item.message
          this.revision++
        }
        throw e
      } finally {
        this.saving = false
      }
    },
  },
})
