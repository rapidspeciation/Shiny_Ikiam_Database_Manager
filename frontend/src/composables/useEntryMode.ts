import { computed, type Ref } from 'vue'
import { persistentRef } from '../lib/persist'

/**
 * How a data-entry tab shows its rows: `cards` (big touch targets, one butterfly
 * or clutch per card) or `table` (the spreadsheet grid). Cards on every device,
 * computers too (Franz, 1 Oct 2026: the cards work better than the tables); the
 * person can switch on any device, and the choice is kept in this browser per
 * tab (`choice` null = the default). Choices stored before stay as they were.
 */
export type EntryMode = 'cards' | 'table'

export const DEFAULT_MODE: EntryMode = 'cards'

/** The mode shown for what is stored in this browser (null or anything unknown: the default). */
export const modeFor = (choice: unknown): EntryMode => (choice === 'cards' || choice === 'table' ? choice : DEFAULT_MODE)
/** What to store for a mode chosen: the default is stored as null (follow the default). */
export const choiceFor = (mode: EntryMode): EntryMode | null => (mode === DEFAULT_MODE ? null : mode)

export function useEntryMode(tab: string) {
  const choice: Ref<EntryMode | null> = persistentRef<EntryMode | null>(`entry-mode:${tab}`, null, { lasting: true })
  const mode = computed<EntryMode>({
    get: () => modeFor(choice.value),
    set: value => (choice.value = choiceFor(value)),
  })
  return { mode }
}
