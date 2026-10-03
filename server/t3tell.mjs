// A message from the person into their own T3 Code chat, sent by the app (the
// "Tell the assistant" button of a proposal edited in the sheet meanwhile). T3
// is a stock install: the message goes through its own HTTP API
// (POST /api/orchestration/dispatch, a thread.turn.start command, with the
// broker's admin token), as T3's page sends one, with the chat's own model and
// modes. Only to a chat of the person's T3 project, and only while it is not
// answering (a message then would cut in): otherwise the app copies the
// message for the person to paste.
import { randomUUID } from 'node:crypto';
import { readFile } from 'node:fs/promises';

/**
 * Sends `text` to T3 chat `threadId` of `username`. { sent: true } or
 * { sent: false, reason }: 'no_t3' (T3 or its token not configured), 'no_chat'
 * (not one of the person's chats), 'busy' (answering now), 'failed' (T3 refused).
 */
export async function tellChat({ t3, chats, threadId, username, text, fetchImpl = fetch }) {
  if (!t3?.local || !t3.tokenFile || !chats) return { sent: false, reason: 'no_t3' };
  const chat = threadId ? chats.session(threadId) : null;
  if (!chat || !chats.projectsOf(username).includes(chat.projectId)) return { sent: false, reason: 'no_chat' };
  if (chat.busy) return { sent: false, reason: 'busy' };
  try {
    const token = (await readFile(t3.tokenFile, 'utf8')).trim();
    const response = await fetchImpl(`${t3.local}/api/orchestration/dispatch`, {
      method: 'POST',
      headers: { authorization: `Bearer ${token}`, 'content-type': 'application/json' },
      body: JSON.stringify({
        type: 'thread.turn.start',
        commandId: randomUUID(),
        threadId,
        message: { messageId: randomUUID(), role: 'user', text, attachments: [] },
        runtimeMode: chat.runtimeMode || 'full-access',
        interactionMode: chat.interactionMode || 'default',
        createdAt: new Date().toISOString(),
      }),
      signal: AbortSignal.timeout(10000),
    });
    if (!response.ok) {
      console.error(`T3 refused a message to chat ${threadId}: ${response.status}`);
      return { sent: false, reason: 'failed' };
    }
    return { sent: true };
  } catch (e) {
    console.error(`A message to T3 chat ${threadId} failed:`, e.message);
    return { sent: false, reason: 'failed' };
  }
}
