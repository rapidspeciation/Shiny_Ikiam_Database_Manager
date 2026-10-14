import { api } from './api'
import { ensureT3, t3Host } from './t3Host'

/**
 * A card's «Preguntar a Claude»: a chat of the person's T3 project that starts with the request
 * (POST api/t3/ask), opened in the T3 frame; the page holding the card shows its chat panel
 * (event `claude:open`). Without T3 the request comes back to be pasted (`prompt`).
 */
export async function askClaude(text: string, title = ''): Promise<{ chat: string | null; prompt: string }> {
  await ensureT3().catch(() => {})
  const out = await api<{ chat: string | null; prompt: string }>('t3/ask', { method: 'POST', body: { text, title } })
  if (out.chat) t3Host.open = { thread: out.chat }
  window.dispatchEvent(new CustomEvent('claude:open', { detail: out }))
  return out
}

/** A card made from another one (a fix's plan, a batch read on the server): the page shows it beside. */
export function showCard(id: string) {
  window.dispatchEvent(new CustomEvent('photos:card', { detail: id }))
}
