import { ref, type Ref } from 'vue'
import { persistentRef } from '../lib/persist'
import { WHOLE, type OwnChoices, type Typed } from '../lib/tubes'

/**
 * What Tubos is registering, shared by its cards and its table (useEntryMode),
 * so switching mode or turning a phone keeps it: the butterflies chosen (the
 * table's loaded rows, the cards), the tissue, medium and dates, where the CAM
 * and tube runs start, the "unused tubes NA" switch; and, for the cards, each
 * one's own values, the CAMs and tubes typed or scanned, and the selection.
 * The keys are the table's own (kept since before the cards), so nothing
 * chosen there is lost.
 */
export interface TubesState {
  /** The Insectary IDs chosen, in the order added (the order tubes are handed out). */
  picked: Ref<string[]>
  /** The tissue for all: WHOLE_ORGANISM, the wing clip or a part. */
  tissue: Ref<string>
  /** The cards: not preserved after all (CAM and tubes NA); the table keeps its tissue. */
  none: Ref<boolean>
  /** The tissues of a body split in parts (cards). */
  parts: Ref<string[]>
  medium: Ref<string>
  /** The preservation day (ISO), kept on this device. */
  presDate: Ref<string>
  /** The day of the wing clip (ISO), kept on this device. */
  clipDate: Ref<string>
  /** Initials signing the clip note, when not the user's own. */
  initials: Ref<string>
  /** Where the CAM run starts ('' or a suggestion: the app's next free one). */
  camStart: Ref<string>
  /** Where the tube run starts: a rack's next tube, or one typed. */
  tubeStart: Ref<string>
  /** Whether the person picked the rack or typed the first tube (else the app keeps choosing it). */
  rackChosen: Ref<boolean>
  /** After a body, the unused tubes: ID NA, tissue and medium NOT_COLLECTED. */
  closeRest: Ref<boolean>
  own: Ref<OwnChoices>
  /** The CAMs and tubes typed or scanned on each card, by Insectary ID. */
  typed: Ref<Record<string, Typed>>
  /** Tubes and CAMs written as on their label though their form looks wrong (accepted on purpose). */
  accepted: Ref<string[]>
  selected: Ref<string[]>
}

/** A fresh state from the browser's storage (tests; the app uses the one shared state below). */
export function createTubesState(): TubesState {
  return {
    picked: persistentRef<string[]>('tubes:loaded', []),
    tissue: persistentRef('tubes:tissue', WHOLE),
    none: persistentRef('tubes:none', false),
    parts: persistentRef<string[]>('tubes:parts', []),
    medium: persistentRef('tubes:medium', 'Flash frozen'),
    presDate: persistentRef('tubes:date', '', { lasting: true }),
    clipDate: persistentRef('tubes:clip-date', '', { lasting: true }),
    initials: persistentRef('tubes:initials', '', { lasting: true }),
    camStart: persistentRef('tubes:cam', ''),
    tubeStart: persistentRef('tubes:tube', ''),
    rackChosen: persistentRef('tubes:rack-chosen', false),
    closeRest: persistentRef('tubes:na', true),
    own: persistentRef<OwnChoices>('tubes:own', {}),
    typed: persistentRef<Record<string, Typed>>('tubes:typed', {}),
    accepted: persistentRef<string[]>('tubes:accepted', []),
    selected: ref<string[]>([]),
  }
}

let shared: TubesState | null = null
/** The one state both Tubos modes read and write. */
export function useTubesState(): TubesState {
  return (shared ??= createTubesState())
}
