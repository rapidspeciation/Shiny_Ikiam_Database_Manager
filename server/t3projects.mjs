// Each person's own project in T3 Code. T3 shows every project to everyone signed in
// to it (its permissions cover the whole install), so "own" is the view, not a lock:
// - the first time a person opens the Asistente tab without a project, it is made
//   (scripts/t3-provision.mjs <username>: their brief, their token for the app's
//   tools, "Ithomiini · <name>"), so their chats run as them, never as the person
//   whose project they would otherwise use;
// - the frame opens on it (`projectKey`, server/t3bridge.mjs): T3's chat list filtered
//   to it and it first among the projects. «All projects» in T3's list still shows
//   everyone's chats;
// - and on one of its chats (`chat`): the person's latest, or else an empty "New thread"
//   made in it (as T3 does for a project it starts with). T3's own start page would open
//   a new chat in whichever project anyone used last, and a new chat goes to the project
//   of the chat on screen.
// The live server's service may not write T3's or Claude's files (ProtectSystem), so
// there the script runs as the user unit ithomiini-t3-provision@<username>.service
// (deploy/), as T3's updates do; the lab runs it directly ('direct').
import { execFile } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';

const SCRIPT = fileURLToPath(new URL('../scripts/t3-provision.mjs', import.meta.url));
/** A username a unit's name (and the workspace folder) can carry as it is. */
const PLAIN = /^[A-Za-z0-9._-]{1,64}$/;
/** After a failed try, the next one waits (each opening of the tab would try again). */
const RETRY_MS = 10 * 60_000;

/** T3's key for a project in its list filter and order: <environment>:<folder> (a folder that is not a git repository). */
export function projectKey(environmentId, root) {
  const folder = String(root ?? '').replace(/\/+$/, '');
  return environmentId && folder ? `${environmentId}:${folder}` : null;
}

const run = (bin, args, timeout) =>
  new Promise((resolve, reject) =>
    execFile(bin, args, { timeout }, (error, stdout, stderr) =>
      error ? reject(new Error(String(stderr || error.message).trim().split('\n').slice(-3).join(' '))) : resolve(stdout),
    ),
  );

/** How a project is made: 'systemd' (the live server), 'direct' (the lab) or 'off'. */
export function provisioner(mode, { systemctl = 'systemctl' } = {}) {
  if (mode === 'direct') return username => run(process.execPath, [SCRIPT, username], 120_000);
  if (mode === 'systemd') return username => run(systemctl, ['--user', 'start', `ithomiini-t3-provision@${username}.service`], 150_000);
  return null;
}

/** A new chat's model: as T3's own new chat (Opus 5.5, medium effort, no fast mode); the person can change it before writing. */
const MODEL = {
  instanceId: 'claudeAgent',
  model: 'claude-opus-5-5',
  options: [
    { id: 'effort', value: 'medium' },
    { id: 'fastMode', value: false },
    { id: 'contextWindow', value: '1m' },
  ],
};

/**
 * Makes an empty "New thread" in a project through T3's own command (with the
 * app's T3 token, server/index.mjs's sign-in links use it too): its id, or an error.
 */
export function chatStarter({ local, tokenFile, fetchImpl = fetch } = {}) {
  if (!local || !tokenFile) return null;
  return async project => {
    const threadId = randomUUID();
    const response = await fetchImpl(`${local}/api/orchestration/dispatch`, {
      method: 'POST',
      headers: { authorization: `Bearer ${readFileSync(tokenFile, 'utf8').trim()}`, 'content-type': 'application/json' },
      body: JSON.stringify({
        type: 'thread.create',
        commandId: randomUUID(),
        threadId,
        projectId: project.id,
        title: 'New thread',
        modelSelection: MODEL,
        runtimeMode: 'full-access',
        interactionMode: 'default',
        branch: null,
        worktreePath: null,
        createdAt: new Date().toISOString(),
      }),
      signal: AbortSignal.timeout(10_000),
    });
    if (!response.ok) throw new Error(`T3 answered ${response.status}`);
    return threadId;
  };
}

/**
 * `chats`: server/t3chats.mjs (T3's projects and chats, read-only); `provision(username)`
 * makes the project; `startChat(project)` an empty chat in it. ensure(user) gives
 * { project: { id, root } | null, chat: the chat to open | null }.
 */
export function createT3Projects({ chats, provision, startChat = null, now = Date.now, wait = 500, log = console.error }) {
  const making = new Map();
  const failed = new Map();
  const starting = new Map();
  async function ensure(user) {
    const project = await projectOf(user);
    if (!project) return { project: null, chat: null };
    const latest = chats.chatsOf(user.username, 1)[0]?.id ?? null;
    if (latest || !startChat) return { project, chat: latest };
    // One empty chat, even when the tab asks twice at once.
    if (!starting.has(user.username)) {
      starting.set(
        user.username,
        startChat(project)
          .catch(e => {
            log(`T3 chat for ${user.username}:`, e.message);
            return null;
          })
          .finally(() => starting.delete(user.username)),
      );
    }
    return { project, chat: await starting.get(user.username) };
  }
  async function projectOf(user) {
    if (!chats?.available || !user?.username) return null;
    const own = chats.projectOf(user.username);
    if (own || !provision || !PLAIN.test(user.username)) return own;
    if (now() - (failed.get(user.username) ?? -Infinity) < RETRY_MS) return null;
    if (!making.has(user.username)) {
      failed.delete(user.username);
      making.set(
        user.username,
        provision(user.username)
          .catch(e => {
            failed.set(user.username, now());
            log(`T3 project for ${user.username}:`, e.message);
          })
          .finally(() => making.delete(user.username)),
      );
    }
    await making.get(user.username);
    // T3 lists a project it was just given within a moment.
    for (let i = 0; i < 10; i++) {
      const made = chats.projectOf(user.username, { fresh: true });
      if (made || failed.has(user.username)) return made;
      await new Promise(resolve => setTimeout(resolve, wait));
    }
    return null;
  }
  return { ensure };
}
