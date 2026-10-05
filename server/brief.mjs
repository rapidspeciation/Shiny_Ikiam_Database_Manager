// The brief a T3 Code workspace gets (AGENTS.md, and CLAUDE.md pointing to it):
// assistant/AGENTS.md with its placeholders filled in: the person
// ({{person}}, {{username}}), the project documentation's folder ({{docs}}), the
// folder of the project's Drive documents ({{knowledge}}) and the sheets' copy
// that `query` reads ({{sheets}}, server/replica.mjs).
// In the local test lab a short note about the lab follows. Written by
// scripts/t3-provision.mjs; shown in the app's "AI instructions" page
// (server/instructions.mjs) with a generic person.

import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const release = join(dirname(fileURLToPath(import.meta.url)), '..');

/** The live server's folders (scripts/t3-provision.mjs's defaults). */
export const SERVER_PATHS = {
  docs: '/home/ubuntu/ithomiini/current/docs',
  knowledge: '/home/ubuntu/ithomiini/shared/knowledge',
  sheets: '/home/ubuntu/ithomiini/shared/sheets.sqlite',
  source: '/home/ubuntu/ithomiini/src',
  releases: '/home/ubuntu/ithomiini/releases',
};

/** The person the instructions page shows the brief for. */
export const GENERIC_PERSON = { display_name: '‹person›', username: '‹username›' };

/** The lab's rule for changing the app: local only (the end of the brief, and the top of the app-dev skill). */
export function labAppDev(labUrl, source) {
  return `This is the **local test lab**: the app at ${labUrl} runs offline on a
copy of the workbook (saves never reach Google Sheets). To change the app
(skill \`app-dev\`), change the source in the git checkout \`${source}\`, run
the checks, restart the lab app (\`tools/lab/app.sh --stop && setsid -f tools/lab/app.sh --bg\`
from the source folder, about a minute), ask the person to reload ${labUrl},
and commit on the current branch. No \`git push\` and no \`scripts/deploy.sh\`
unless the person explicitly asks.`;
}

/**
 * The workspace's brief. `user`: { display_name, username }; `docs`: the
 * project documentation's folder; `knowledge`: the Drive documents' folder; `sheets`: the sheets' copy;
 * `source` and `labUrl`: the local lab's checkout and address (the lab note); `root`: where assistant/AGENTS.md is.
 */
export function composeBrief(
  user,
  { docs, knowledge = SERVER_PATHS.knowledge, sheets = SERVER_PATHS.sheets, source, labUrl = '', root = release } = SERVER_PATHS,
) {
  const brief = readFileSync(join(root, 'assistant', 'AGENTS.md'), 'utf8')
    .replaceAll('{{person}}', user.display_name)
    .replaceAll('{{username}}', user.username)
    .replaceAll('{{docs}}', docs)
    .replaceAll('{{knowledge}}', knowledge)
    .replaceAll('{{sheets}}', sheets)
    .trimEnd();
  return labUrl ? `${brief}\n\n## Local test lab\n\n${labAppDev(labUrl, source)}\n` : `${brief}\n`;
}
