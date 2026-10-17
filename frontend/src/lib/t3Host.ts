import { reactive } from 'vue'
import { api } from './api'
import type { T3Seen } from './t3Bridge'

/**
 * T3 Code's frame lives for the whole session (T3Host, in App.vue), outside the
 * tabs kept alive: a frame taken out of the page and put back reloads, and on a
 * slow connection that is a full T3 load (its assets, its chat lists) each time
 * someone comes back to the Asistente tab. The tab only marks where the frame
 * goes (`slot`) and says what it shows; the frame sits over that place while
 * the tab is on screen and is hidden (still loaded) meanwhile.
 */
export const t3Host = reactive({
  /** T3's address (null: not asked yet, or T3 is not configured), and its chats' environment. */
  url: null as string | null,
  environmentId: null as string | null,
  /** The person's own T3 project (server/t3projects.mjs): the frame opens with only its chats listed. */
  projectKey: null as string | null,
  /** The chat the frame opens on: the person's latest in their own project (server/t3projects.mjs). */
  start: null as string | null,
  /** The place the frame covers (null: the Asistente tab is not on screen). */
  slot: null as HTMLElement | null,
  /** A chat to show (a new object each time a link asks for it); `made`: just made by the app. */
  open: null as { thread: string; made?: boolean } | null,
  /** What the frame shows, as its bridge says it. */
  seen: null as T3Seen | null,
  /** The pointer goes through the frame (the divider beside it is being dragged). */
  passThrough: false,
  /** Raised to sign in to T3 again (Volver a conectar, after a T3 update). */
  reconnects: 0,
  /**
   * A link to a proposal clicked in T3's chat: the tab on screen shows it in its panel (true), or
   * it opens in a new browser tab. Set by the tab that holds the frame.
   */
  onLink: null as ((proposal: string) => boolean) | null,
})

let asking: Promise<void> | null = null
/** T3's address and the person's project, asked once (a tab other than Asistente that shows T3 beside its own work). */
export function ensureT3(): Promise<void> {
  if (t3Host.url) return Promise.resolve()
  asking ??= api<{ url: string | null; environmentId?: string | null; projectKey?: string | null; chat?: string | null }>('t3/status')
    .then(status => {
      t3Host.environmentId = status.environmentId ?? null
      t3Host.projectKey = status.projectKey ?? null
      t3Host.start = status.chat ?? null
      t3Host.url = status.url
    })
    .finally(() => (asking = null))
  return asking
}
