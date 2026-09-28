/**
 * The choice lists of ChoiceField (the app's one dropdown, like the grids'
 * list): which options show for what was typed, and what a commit stores.
 * Kept apart from the component so the rules are tested without a browser.
 */

export interface Choice {
  /** What is stored. */
  value: string
  /** What is shown (and typed to find it). */
  label: string
  /** Grey text after the label (e.g. the row of a suggested ID). */
  hint?: string
  /** Heading the option is listed under (as an <optgroup>). */
  group?: string
}
export type ChoiceOptions = readonly string[] | readonly Choice[]

/** Plain strings are their own label. */
export function toChoices(options: ChoiceOptions): Choice[] {
  return options.map(o => (typeof o === 'string' ? { value: o, label: o } : o))
}

/**
 * The options that match what was typed, in the grid's order (pickChoice): the
 * same text first, then those that start with it, then those that contain it,
 * each in the list's order (most used first). Grouped lists keep their groups
 * together. At most `limit` are returned (a species list has ~10,000); `total`
 * counts them all. Nothing typed: every option.
 */
export function filterChoices(
  choices: readonly Choice[],
  typed: string,
  limit = 100,
  lower: readonly string[] = choices.map(c => c.label.toLowerCase()),
): { items: Choice[]; total: number } {
  const t = typed.trim().toLowerCase()
  let hits: Choice[]
  if (!t) hits = choices as Choice[]
  else {
    const same: Choice[] = []
    const starts: Choice[] = []
    const contains: Choice[] = []
    for (let i = 0; i < choices.length; i++) {
      const label = lower[i]
      if (label === t) same.push(choices[i])
      else if (label.startsWith(t)) starts.push(choices[i])
      else if (label.includes(t)) contains.push(choices[i])
    }
    hits = [...same, ...starts, ...contains]
    if (choices.some(c => c.group)) {
      const order = new Map<string | undefined, number>()
      for (const c of choices) if (!order.has(c.group)) order.set(c.group, order.size)
      hits.sort((a, b) => order.get(a.group)! - order.get(b.group)!)
    }
  }
  return { items: hits.slice(0, limit), total: hits.length }
}

/** The label shown for a stored value (the value itself when it is not an option). */
export function labelOf(choices: readonly Choice[], value: string): string {
  return choices.find(c => c.value === value)?.label ?? value
}

/**
 * What leaving the box stores, when no suggestion is highlighted to take:
 * the option the text picks (first suggestion, compared by label); otherwise
 * the text as typed (free text), empty (when allowed) or null: the value is
 * kept (select-like lists only take their options).
 */
export function commitText(
  typed: string,
  choices: readonly Choice[],
  { freetext = true, allowEmpty = false }: { freetext?: boolean; allowEmpty?: boolean } = {},
): string | null {
  const t = typed.trim()
  if (!t) return freetext || allowEmpty ? '' : null
  const pick = filterChoices(choices, t, 1).items[0]
  if (pick) return pick.value
  return freetext ? t : null
}
