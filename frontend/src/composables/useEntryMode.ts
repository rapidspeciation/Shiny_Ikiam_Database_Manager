import { computed, onBeforeUnmount, onMounted, ref, type Ref } from 'vue'
import { persistentRef } from '../lib/persist'

/**
 * How a data-entry tab shows its rows: `cards` (big touch targets, one butterfly
 * or clutch per card: phones and tablets) or `table` (the spreadsheet grid: a PC
 * with a mouse). Each tab defaults by device, so a phone or tablet held either way
 * gets the cards, and the person can switch on any device; the choice is kept in
 * this browser per tab (`choice` null = follow the device).
 */
export type EntryMode = 'cards' | 'table'

/** A touch screen without a mouse (phones and tablets, whichever way they are held). */
export const TOUCH_QUERY = '(pointer: coarse) and (hover: none)'

export function useEntryMode(tab: string) {
  const query = typeof window !== 'undefined' && window.matchMedia ? window.matchMedia(TOUCH_QUERY) : null
  const touch = ref(!!query?.matches)
  const follow = (e: MediaQueryListEvent) => (touch.value = e.matches)
  onMounted(() => query?.addEventListener('change', follow))
  onBeforeUnmount(() => query?.removeEventListener('change', follow))
  const choice: Ref<EntryMode | null> = persistentRef<EntryMode | null>(`entry-mode:${tab}`, null, { lasting: true })
  const mode = computed<EntryMode>({
    get: () => choice.value ?? (touch.value ? 'cards' : 'table'),
    // Choosing what the device would show anyway goes back to following the device.
    set: value => (choice.value = value === (touch.value ? 'cards' : 'table') ? null : value),
  })
  return { mode, touch }
}
