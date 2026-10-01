#!/usr/bin/env node
// Writes server/instructions-history.json: the change history of the assistant's
// instructions (brief, skills, subagents, tool descriptions) for the app's
// "AI instructions" page. Run by `npm run build`, so a release (a tarball
// without .git, scripts/deploy.sh) ships it; a git checkout reads git directly.

import { HISTORY_FILE, REPO_ROOT, isCheckout, writeHistory } from '../server/instructions.mjs';

if (!isCheckout(REPO_ROOT)) {
  console.log('Not a git checkout: the instructions history is left as it is');
} else {
  const history = await writeHistory(REPO_ROOT, HISTORY_FILE);
  const commits = Object.values(history.entries).reduce((n, list) => n + list.length, 0);
  console.log(`Instructions history: ${Object.keys(history.entries).length} files, ${commits} changes → server/instructions-history.json`);
}
