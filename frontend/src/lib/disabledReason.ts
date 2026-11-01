import { notify } from './notice'

/**
 * Why a control is switched off, for touch screens (PRINCIPLES P8): a disabled button's `title` shows on hover with a
 * mouse, and a tap on it shows the same text at the bottom of the screen. Browsers send pointer events to disabled
 * buttons (not clicks), so one listener on the document covers every button that has a title.
 */
export function disabledReason(target: EventTarget | null): string {
  const el = target instanceof Element ? target.closest('button:disabled, [aria-disabled="true"]') : null
  return el?.getAttribute('title')?.trim() ?? ''
}

export function explainDisabledTaps(doc: Document = document) {
  let down: Element | null = null
  doc.addEventListener('pointerdown', e => (down = e.pointerType === 'mouse' ? null : (e.target as Element | null)), true)
  doc.addEventListener(
    'pointerup',
    e => {
      // A tap, not a scroll that started on the button.
      if (e.pointerType === 'mouse' || e.target !== down) return
      const text = disabledReason(e.target)
      if (text) notify(text, 'info', 3500)
    },
    true,
  )
}
