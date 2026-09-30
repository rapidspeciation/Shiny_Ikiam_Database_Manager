/**
 * What the T3 Code frame of the Asistente tab shows, as the script the app adds
 * to T3's page says it (server/t3bridge.mjs): the chat on screen, sent on each
 * navigation. Pure helpers, kept apart so they can be tested.
 */
export interface T3View {
  path: string
  environmentId: string | null
  /** The chat on screen (/<environmentId>/<threadId>). */
  threadId: string | null
  /** A new chat, not sent yet (/draft/<draftId>). */
  draftId: string | null
  visible: boolean
  focused: boolean
}
/** The frame's bridge: not heard yet, speaking, or silent (T3 served without it, or a T3 update broke it). */
export interface T3Seen {
  bridge: 'waiting' | 'on' | 'off'
  view: T3View | null
}

export const HELLO = { type: 'ithomiini-t3-hello' }
/** How long after the frame loaded the bridge has to answer before the app stops waiting for it. */
export const BRIDGE_WAIT_MS = 4000

const ID = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i
const idOrNull = (value: unknown) => (typeof value === 'string' && ID.test(value) ? value.toLowerCase() : null)

/** The origin of T3's address ('' if it is not one). */
export function originOf(url: string) {
  try {
    return new URL(url).origin
  } catch {
    return ''
  }
}

/**
 * The view a message reports, when it comes from the bridge of this frame:
 * sent by the frame's own window, from T3's origin (not another tab, frame or
 * site), in the bridge's shape. null for anything else.
 */
export function bridgeMessage(
  event: { source: unknown; origin: string; data: unknown },
  frame: Window | null | undefined,
  t3Origin: string,
): T3View | null {
  if (!frame || !t3Origin || event.source !== frame || event.origin !== t3Origin) return null
  const d = event.data as Record<string, unknown> | null
  if (!d || typeof d !== 'object' || d.type !== 'ithomiini-t3' || d.v !== 1 || typeof d.path !== 'string') return null
  return {
    path: d.path.slice(0, 300),
    environmentId: idOrNull(d.environmentId),
    threadId: idOrNull(d.threadId),
    draftId: typeof d.draftId === 'string' && d.draftId ? d.draftId.slice(0, 100) : null,
    visible: d.visible !== false,
    focused: d.focused === true,
  }
}

/**
 * The chat the list asks for as open in T3: its thread, 'draft' (a new chat)
 * or 'none' (no chat on screen: settings, T3's home); undefined while the
 * bridge has not spoken or is silent, and the server guesses (chat=auto).
 */
export function seenChat(seen: T3Seen | null | undefined): string | undefined {
  if (!seen || seen.bridge !== 'on' || !seen.view) return undefined
  return seen.view.threadId ?? (seen.view.draftId ? 'draft' : 'none')
}

/** A chat picked by hand stays until the frame moves to another chat (its first report is not a move). */
export function afterMove(chosen: string, before: string | undefined, now: string | undefined) {
  return before !== undefined && now !== before ? 'auto' : chosen
}
