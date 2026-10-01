import { onBeforeUnmount, onMounted, ref } from 'vue'

/**
 * The app's phone width: below Tailwind's `sm` (640 px), where the header puts
 * the tabs on their own row. Follows the screen when it turns.
 */
export const PHONE_QUERY = '(max-width: 639.98px)'
export function usePhoneWidth() {
  const query = typeof window !== 'undefined' && window.matchMedia ? window.matchMedia(PHONE_QUERY) : null
  const phone = ref(!!query?.matches)
  const follow = (e: MediaQueryListEvent) => (phone.value = e.matches)
  onMounted(() => query?.addEventListener('change', follow))
  onBeforeUnmount(() => query?.removeEventListener('change', follow))
  return phone
}

/**
 * The part of the page the on-screen keyboard covers. Chrome on Android shrinks
 * only the visible area (visualViewport) when the keyboard opens, so fixed bars
 * at the bottom would sit under it: `cover` is how many pixels of the layout's
 * bottom are hidden, `open` whether a keyboard is up (as the emulator tests judge it).
 */
export function useKeyboard() {
  const cover = ref(0)
  const open = ref(false)
  const visibleTop = ref(0)
  const visibleBottom = ref(typeof window === 'undefined' ? 0 : window.innerHeight)
  const update = () => {
    const view = window.visualViewport
    if (!view) return
    visibleTop.value = view.offsetTop
    visibleBottom.value = view.offsetTop + view.height
    cover.value = Math.max(0, Math.round(window.innerHeight - visibleBottom.value))
    open.value = view.height < window.innerHeight - 120
  }
  onMounted(() => {
    update()
    window.visualViewport?.addEventListener('resize', update)
    window.visualViewport?.addEventListener('scroll', update)
  })
  onBeforeUnmount(() => {
    window.visualViewport?.removeEventListener('resize', update)
    window.visualViewport?.removeEventListener('scroll', update)
  })
  return { cover, open, visibleTop, visibleBottom, update }
}
