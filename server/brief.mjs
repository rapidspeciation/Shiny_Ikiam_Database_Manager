// The brief a T3 Code workspace gets (AGENTS.md, and CLAUDE.md pointing to it):
// a short opening naming the person, assistant/AGENTS.md, and this workspace's
// folders. Written by scripts/t3-provision.mjs; shown in the app's "AI
// instructions" page (server/instructions.mjs) with a generic person.

import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const release = join(dirname(fileURLToPath(import.meta.url)), '..');

/** The live server's folders (scripts/t3-provision.mjs's defaults). */
export const SERVER_PATHS = {
  docs: '/home/ubuntu/ithomiini/current/docs',
  source: '/home/ubuntu/ithomiini/src',
  releases: '/home/ubuntu/ithomiini/releases',
};

/** The lab's rule for changing the app: local only (a bullet of the brief, and the top of the app-dev skill). */
export function labAppDev(labUrl, source) {
  return `- Changing the app: skill \`app-dev\`, but this is the **local test lab**:
  the app at ${labUrl} runs offline on a copy of the workbook (saves never
  reach Google Sheets). Change the source in the git checkout \`${source}\`, run
  the checks, restart the lab app (\`tools/lab/app.sh --stop && setsid -f tools/lab/app.sh --bg\`
  from the source folder, about a minute), ask the person to reload ${labUrl},
  and commit on the current branch. **Never \`git push\` or run
  \`scripts/deploy.sh\`** unless the person explicitly asks.`;
}

/**
 * The workspace's brief. `user`: { display_name, username }; paths: docs,
 * source and releases (SERVER_PATHS on the server); labUrl: the local lab's address.
 */
export function composeBrief(user, { docs, source, releases, labUrl = '', root = release } = SERVER_PATHS) {
  const base = readFileSync(join(root, 'assistant', 'AGENTS.md'), 'utf8');
  const body = base.slice(base.indexOf('\n') + 1).trimStart();
  return `# Ithomiini database assistant (T3 Code)

You are working for **${user.display_name}** (app user \`${user.username}\`).

${body.trimEnd()}

## This workspace

- Project documentation (data-entry audit, monitoring, workbook schema,
  meetings, operations): \`${docs}\`.
- Keep downloads and generated files in \`work/<date>-<topic>/\` here (several
  chats share this folder; don't reuse names).
${labUrl ? labAppDev(labUrl, source) : `- Changing the app: skill \`app-dev\`. The source is the git checkout \`${source}\`;
  never edit the built files in \`${releases}\` or \`current\`.`}
`;
}
