/**
 * Where a menu under a button goes, as its left edge measured from the button's left edge: ending at the
 * button's right edge (the usual place for a menu at the right of a bar), moved right or left as little as
 * needed to keep `margin` pixels from both sides of a page `pageWidth` wide. On phones the header wraps and
 * the button can sit at the screen's left, or in the middle of a row.
 */
export function menuLeft(button: { left: number; right: number }, menuWidth: number, pageWidth: number, margin = 8): number {
  const wanted = button.right - menuWidth
  const left = Math.max(margin, Math.min(wanted, pageWidth - margin - menuWidth))
  return Math.round(left - button.left)
}
