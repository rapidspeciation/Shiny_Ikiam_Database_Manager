#!/usr/bin/env node
// The outside proposer's view of a lab instance (tools/lab/doc-proposals.md), as JSON on stdout: doc_threads with
// its changes (status, comments, revisionRequested and the comments since the last version) and the open threads.
//   node tools/lab/doc-proposals-export.mjs [--pending-revisions] [--status <status>] [--path <file>] [--url <mcp url>] [--token-file <file>]
// --pending-revisions: only the changes waiting for a new version. --status: pending, applied, undone or discarded.
// The tool answers in pages (its size limit); the export asks for every page, filtered by the app, until the end.
import { fileURLToPath } from 'node:url';
import { DEFAULT_TOKEN_FILE, DEFAULT_URL, mcpClient, option } from './doc-proposals-mcp.mjs';

export async function exportProposals({ call, pendingRevisions = false, status = null, path = null }) {
  const filters = { ...(pendingRevisions ? { revisionRequested: true } : {}), ...(status ? { status } : {}), ...(path ? { path } : {}) };
  let threads = [];
  const changes = [];
  let total = null;
  for (let offset = 0, pages = 0; ; pages++) {
    if (pages > 1000) throw new Error('doc_threads: no end after 1000 pages');
    const out = await call('doc_threads', { ...filters, offset });
    if (out.error) throw new Error(out.error);
    if (offset === 0) threads = out.threads ?? [];
    changes.push(...(out.changes ?? []));
    total = out.total ?? changes.length;
    if (out.nextOffset == null) {
      // A cut the paging does not cover (the threads were too long for one answer): say so, not a silent half.
      if (out.truncated) throw new Error(`doc_threads cut its answer: ${out.next}`);
      break;
    }
    if (!(out.nextOffset > offset)) throw new Error(`doc_threads: nextOffset ${out.nextOffset} does not move on from ${offset}`);
    offset = out.nextOffset;
  }
  if (changes.length !== total) throw new Error(`doc_threads: ${changes.length} changes read of ${total}`);
  return { threads, changes, total };
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const argv = process.argv.slice(2);
  const call = mcpClient({ url: option(argv, '--url', DEFAULT_URL), tokenFile: option(argv, '--token-file', DEFAULT_TOKEN_FILE) });
  const out = await exportProposals({ call, pendingRevisions: argv.includes('--pending-revisions'), status: option(argv, '--status', null), path: option(argv, '--path', null) });
  process.stdout.write(JSON.stringify(out, null, 2) + '\n');
}
