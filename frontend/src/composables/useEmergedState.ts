import { computed, ref, watch, type Ref } from 'vue'
import { api } from '../lib/api'
import { todayIso } from '../lib/dates'
import type { Draft } from '../lib/emerged'
import { errorText, notify } from '../lib/notice'
import { persistentRef } from '../lib/persist'
import { usePending } from '../stores/pending'
import { useTables } from '../stores/tables'

/**
 * What Emergidos is registering, shared by its two modes (cards and table, see
 * useEntryMode): the day, the clutch, the cards not saved yet (kept in this
 * browser, so a closed tab or a turned phone loses none of them) and which
 * clutches' counts a save leaves alone; and the free pre-made Insectary IDs
 * (server/grid.mjs insectaryIds), loaded again when the sheet changes.
 */
export interface EmergedState {
  /** The emergence day new cards get (ISO). */
  date: Ref<string>
  /** The clutch new cards belong to (CLUTCH NUMBER as text). */
  clutch: Ref<string>
  drafts: Ref<Draft[]>
  /** Clutches whose Insectary_stocks row the next save leaves alone. */
  skipStock: Ref<string[]>
  /** The medium of preserved bodies. */
  medium: Ref<string>
  /** Free pre-made IDs: after the last row used first, then earlier empty rows. */
  freeIds: Ref<string[]>
  /** The same IDs in sheet order (a run of cards follows it: H0B → H1B → H2B). */
  inOrder: Ref<string[]>
  /** Sheet row of each free pre-made ID. */
  rowOf: Ref<Map<string, number>>
  idsLoaded: Ref<boolean>
  loadFreeIds: () => Promise<void>
}

const MODULE = 'Insectary_data'

function create(): EmergedState {
  const pending = usePending()
  const freeIds = ref<string[]>([])
  const inOrder = ref<string[]>([])
  const rowOf = ref(new Map<string, number>())
  const idsLoaded = ref(false)
  let asking: Promise<void> | null = null
  async function loadFreeIds() {
    if (asking) return asking
    asking = (async () => {
      try {
        const result = await api<{ sequence: string[]; rows: { value: string; row: number }[] }>('ids?kind=insectary&count=5000')
        // Rows added in the table and not saved yet hold their IDs too.
        const used = new Set(pending.creates.filter(c => c.module === MODULE).map(c => String(c.values.Insectary_ID).toUpperCase()))
        freeIds.value = result.sequence.filter(id => !used.has(id.toUpperCase()))
        inOrder.value = [...result.rows]
          .sort((a, b) => a.row - b.row)
          .map(r => r.value)
          .filter(id => !used.has(id.toUpperCase()))
        rowOf.value = new Map(result.rows.map(r => [r.value.toUpperCase(), r.row]))
        idsLoaded.value = true
      } catch (e) {
        notify(errorText(e), 'error')
      } finally {
        asking = null
      }
    })()
    return asking
  }
  const tables = useTables()
  watch(() => tables.versions[MODULE], () => void loadFreeIds())
  // The new rows typed in the table take their IDs out of the free ones at once.
  watch(
    () => pending.creates.filter(c => c.module === MODULE).length,
    () => void loadFreeIds(),
  )
  void loadFreeIds()
  return {
    // The same keys the table used before (the clutch and day survive the update).
    date: persistentRef('emerged:date', todayIso()),
    clutch: persistentRef('emerged:clutch', ''),
    drafts: persistentRef<Draft[]>('emerged:drafts', [], { lasting: true }),
    skipStock: persistentRef<string[]>('emerged:skip-stock', []),
    medium: persistentRef('emerged:medium', 'Flash frozen', { lasting: true }),
    freeIds,
    inOrder,
    rowOf,
    idsLoaded,
    loadFreeIds,
  }
}

let shared: EmergedState | null = null
/** The one state both Emergidos modes read and write. */
export function useEmergedState(): EmergedState {
  return (shared ??= create())
}

/** The cards' IDs, upper case. */
export const heldIds = (drafts: Draft[]) => drafts.map(d => d.id.trim().toUpperCase())
export const useHeld = (state: EmergedState) => computed(() => heldIds(state.drafts.value))
