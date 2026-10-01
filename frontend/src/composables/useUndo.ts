import { onBeforeUnmount, ref } from 'vue'
import { api, requestId } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import type { UndoPreview } from '../lib/types'
import { useTables } from '../stores/tables'
import { t } from '../lib/i18n'

/** What to undo: whole groups of saves, saves, or single changes (server/history.mjs undoSelection). */
export type UndoBody = { groupIds?: string[]; actionIds?: string[]; changeIds?: string[] }
export interface UndoReview {
  /** What is undone, in words (the dialog's subtitle). */
  title: string
  body: UndoBody
  preview: UndoPreview
  requestId: string
  /** The save (group) it came from, for the list to keep it in view. */
  groupId?: string
}

/**
 * Undoing from the history, as the Historial does it: always a preview first
 * (history/preview: each cell before → after, and what cannot be undone
 * because it changed again since), then one confirmation (history/undo), which
 * writes the old values to Google Sheets as one new save and reloads the sheets
 * it touched. Used by the Historial and by each tab's own history.
 * `done(review)` runs after a confirmed undo (to reload a list).
 */
export function useUndo(done?: (review: UndoReview) => void | Promise<void>) {
  const tables = useTables()
  const undoing = ref<UndoReview | null>(null)
  const reason = ref('')
  const busy = ref(false)

  async function review(body: UndoBody, title: string, groupId?: string) {
    try {
      const preview = await api<UndoPreview>('history/preview', { method: 'POST', body })
      reason.value = ''
      undoing.value = { title, body, preview, requestId: requestId(), groupId }
    } catch (e) {
      notify(errorText(e), 'error')
    }
  }
  function cancel() {
    if (!busy.value) undoing.value = null
  }
  async function confirm() {
    const u = undoing.value
    if (!u || busy.value) return
    busy.value = true
    try {
      const result = await api<{ action: { changes: { sheet: string }[] } | null }>('history/undo', {
        method: 'POST',
        body: { ...u.body, requestId: u.requestId, reason: reason.value || null },
      })
      const sheets = new Set(result.action?.changes.map(c => c.sheet) || [])
      await Promise.all([...sheets].filter(s => tables.tables[s]).map(s => tables.load(s, true)))
      notify(t('Cambios deshechos en Google Sheets'), 'success')
      undoing.value = null
      await done?.(u)
    } catch (e) {
      notify(errorText(e), 'error')
    } finally {
      busy.value = false
    }
  }
  /** Escape closes the confirmation (and nothing behind it: a tab's history panel stays). */
  const closeOnEscape = (e: KeyboardEvent) => {
    if (e.key !== 'Escape' || !undoing.value) return
    e.stopImmediatePropagation()
    cancel()
  }
  window.addEventListener('keydown', closeOnEscape)
  onBeforeUnmount(() => window.removeEventListener('keydown', closeOnEscape))
  return { undoing, reason, busy, review, cancel, confirm }
}
