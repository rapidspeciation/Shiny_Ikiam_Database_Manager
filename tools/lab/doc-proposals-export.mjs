#!/usr/bin/env node
// The outside proposer's view of a lab instance (tools/lab/doc-proposals.md), as JSON on stdout: doc_threads with
// its changes (status, comments, revisionRequested and the comments since the last version) and the open threads.
//   node tools/lab/doc-proposals-export.mjs [--pending-revisions] [--url <mcp url>] [--token-file <file>]
// --pending-revisions: only the changes waiting for a new version.
import { fileURLToPath } from 'node:url';
import { DEFAULT_TOKEN_FILE, DEFAULT_URL, mcpClient, option } from './doc-proposals-mcp.mjs';

export async function exportProposals({ call, pendingRevisions = false }) {
  const out = await call('doc_threads', {});
  if (out.error) throw new Error(out.error);
  return pendingRevisions ? { ...out, changes: (out.changes ?? []).filter(c => c.revisionRequested) } : out;
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const argv = process.argv.slice(2);
  const call = mcpClient({ url: option(argv, '--url', DEFAULT_URL), tokenFile: option(argv, '--token-file', DEFAULT_TOKEN_FILE) });
  process.stdout.write(JSON.stringify(await exportProposals({ call, pendingRevisions: argv.includes('--pending-revisions') }), null, 2) + '\n');
}
