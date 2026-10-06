/**
 * Emergidos' quick taps (+♀, +♂, + larva): each tap adds a card at once with
 * the next free Insectary ID, and the server holds that ID for the card
 * (server/holds.mjs) so nobody else gets it while it is written on the wing.
 * The holds go to the server one at a time, in the order of the taps, so the
 * IDs stay in tap order; when someone else took an ID a moment before, that card
 * (and the ones tapped after it) move on to the next free ones. A card whose ID
 * was written or typed already (`move: false`: a card kept from earlier, or
 * an ID changed to what the wing says) is never moved: it only says it is not
 * held. Nothing here touches the store or the network (`hold` does).
 */
export interface HoldAnswer {
  held: boolean
  value: string
  /** Not held: CLAIMED (someone else holds it: `holder`), USED (a butterfly of the sheet, or no free pre-made row). */
  code?: 'CLAIMED' | 'USED'
  holder?: string
  next?: string | null
}
export type HoldState = 'held' | 'refused' | 'offline'

export interface HoldQueueOptions {
  /** Asks the server to hold `id` for card `key`. Throws when there is no answer (offline). */
  hold: (key: string, id: string) => Promise<HoldAnswer>
  /** The card's ID as it stands, or null when the card is gone. */
  idOf: (key: string) => string | null
  /** The ID a card would take now: the next free one after the cards before it, never one of `refused`. */
  pick: (key: string, refused: ReadonlySet<string>) => string | null
  /**
   * Gives a card another ID: its own was taken a moment before (`holder`), or
   * (`shifted`) a card tapped before it took its ID, so it moves on too.
   */
  assign: (key: string, id: string, from: { id: string; holder?: string; shifted?: boolean }) => void
  /** How a card's hold ended. */
  settled: (key: string, state: HoldState, answer?: HoldAnswer) => void
}

/** At most this many IDs are tried for one card before it is left as not held. */
export const HOLD_TRIES = 8

export class HoldQueue {
  private chain: Promise<void> = Promise.resolve()
  private refused = new Set<string>()
  /** Cards waiting for their turn (or being held now), in tap order. */
  private readonly queue: { key: string; move: boolean }[] = []
  constructor(private readonly options: HoldQueueOptions) {}

  /** Holds the card's ID after the cards tapped before it; `move`: it may take the next free ID when its own was taken. */
  push(key: string, { move = true }: { move?: boolean } = {}): Promise<void> {
    const entry = this.queue.find(e => e.key === key)
    if (entry) entry.move &&= move
    else this.queue.push({ key, move })
    this.chain = this.chain.then(() => this.run(key, move)).catch(() => {})
    return this.chain
  }
  /** Settles once every card pushed so far has its answer. */
  idle(): Promise<void> {
    return this.chain
  }
  /** The cards waiting for their hold (or being held now), in tap order. */
  get waiting(): string[] {
    return this.queue.map(e => e.key)
  }
  /** The free IDs were asked again: an ID refused before may be free now (its card was taken away). */
  forgetRefused() {
    this.refused.clear()
  }

  private async run(key: string, move: boolean) {
    const o = this.options
    try {
      let id = o.idOf(key)
      for (let tries = 0; id && tries < HOLD_TRIES; tries++) {
        let answer: HoldAnswer
        try {
          answer = await o.hold(key, id)
        } catch {
          if (o.idOf(key) !== null) o.settled(key, 'offline')
          return
        }
        // Taken away (or its ID changed) while asking: the newer request says what holds.
        if (o.idOf(key) !== id) return
        if (answer.held) return o.settled(key, 'held', answer)
        this.refused.add(id.toUpperCase())
        const next = move ? o.pick(key, this.refused) : null
        if (!next) return o.settled(key, 'refused', answer)
        o.assign(key, next, { id, holder: answer.holder })
        id = next
        this.shiftAfter(key)
      }
      if (id) o.settled(key, 'refused')
    } finally {
      const at = this.queue.findIndex(e => e.key === key)
      if (at >= 0) this.queue.splice(at, 1)
    }
  }
  /** The cards tapped after one that moved on: each the next free ID after it, so no two show the same one meanwhile. */
  private shiftAfter(key: string) {
    const o = this.options
    for (const e of this.queue.slice(this.queue.findIndex(x => x.key === key) + 1)) {
      if (!e.move) continue
      const now = o.idOf(e.key)
      const next = o.pick(e.key, this.refused)
      if (now && next && next !== now) o.assign(e.key, next, { id: now, shifted: true })
    }
  }
}
