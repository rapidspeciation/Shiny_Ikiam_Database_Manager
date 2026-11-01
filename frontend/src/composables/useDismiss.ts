import { onScopeDispose, watch, type Ref, type WatchSource } from 'vue'

type El = HTMLElement | null | undefined
/**
 * Closes a menu or popover the way people expect: a click or tap outside it, keyboard focus moving out of it, a click
 * into an embedded page (an iframe, whose clicks never reach this document: the window loses focus instead), Escape
 * (focus back on `trigger`, or on whatever had the focus when it opened) and, with `closeOn`, a change of page. `inside` is the menu and its button
 * (one element holding both, or several). The listeners are on only while it is open.
 */
export function useDismiss(
  open: Ref<boolean>,
  inside: Ref<El> | (() => El[]),
  options: { trigger?: Ref<El>; closeOn?: WatchSource } = {},
) {
  const parts = () => (typeof inside === 'function' ? inside() : [inside.value])
  const within = (node: EventTarget | null) => node instanceof Node && parts().some(el => el?.contains(node))
  const close = () => (open.value = false)
  let opener: HTMLElement | null = null
  const pointer = (event: PointerEvent) => {
    if (!within(event.target)) close()
  }
  const focus = (event: FocusEvent) => {
    if (!within(event.target)) close()
  }
  const key = (event: KeyboardEvent) => {
    if (event.key !== 'Escape' || event.defaultPrevented) return
    event.preventDefault()
    close()
    ;(options.trigger?.value ?? opener)?.focus()
  }
  const blur = () => {
    // After the blur the iframe is the active element; a switch to another window leaves the menu as it was.
    setTimeout(() => {
      if (document.activeElement instanceof HTMLIFrameElement && !within(document.activeElement)) close()
    })
  }
  function listen(on: boolean) {
    const method = on ? 'addEventListener' : 'removeEventListener'
    // Capture: a grid or a card that stops the event on its way up still closes the menu.
    document[method]('pointerdown', pointer as EventListener, true)
    document[method]('focusin', focus as EventListener, true)
    document[method]('keydown', key as EventListener)
    window[method]('blur', blur)
  }
  watch(
    open,
    (now, before) => {
      if (now === before) return
      if (now) opener = document.activeElement instanceof HTMLElement ? document.activeElement : null
      listen(now)
    },
    { immediate: true },
  )
  if (options.closeOn) watch(options.closeOn, close)
  onScopeDispose(() => listen(false))
  return { close }
}
