#!/usr/bin/env node
// Proposes instruction changes from a JSONL file as «cambio de documento» cards in a lab instance
// (tools/lab/doc-proposals.md). One line per change: {"path", "section", "before", "after", "reason"}
// (optional "thread_ids"). Each line is one propose_doc_change call over MCP with the outside proposer's token; the
// tool's answer is printed per line: ok (the change's id, a size growth) or refused (the tool's message).
//   node tools/lab/doc-proposals-import.mjs proposals.jsonl [--dry-run] [--url <mcp url>] [--token-file <file>]
// --dry-run: the same checks, nothing is proposed. Exit code 1 when a line was refused.
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { DEFAULT_TOKEN_FILE, DEFAULT_URL, mcpClient, option } from './doc-proposals-mcp.mjs';

/** The lines of a JSONL text: [{ line, value }] or [{ line, error }]. Blank lines and # comments are skipped. */
export function parseLines(text) {
  return text
    .split('\n')
    .map((raw, i) => ({ raw: raw.trim(), line: i + 1 }))
    .filter(l => l.raw && !l.raw.startsWith('#'))
    .map(({ raw, line }) => {
      try {
        const v = JSON.parse(raw);
        for (const key of ['path', 'before', 'after']) if (typeof v[key] !== 'string' || (key !== 'after' && !v[key])) throw new Error(`"${key}" is missing`);
        if (typeof (v.section ?? v.heading) !== 'string') throw new Error('"section" is missing (\'\' for the text before the first heading)');
        return { line, value: v };
      } catch (e) {
        return { line, error: e.message };
      }
    });
}

export async function importProposals(text, { call, dryRun = false, log = console.log }) {
  let refused = 0;
  for (const l of parseLines(text)) {
    if (l.error) {
      refused++;
      log(`line ${l.line}: refused: ${l.error}`);
      continue;
    }
    const v = l.value;
    const args = { path: v.path, heading: v.section ?? v.heading, before: v.before, after: v.after, ...(v.reason ? { reason: v.reason } : {}), ...(v.thread_ids ? { thread_ids: v.thread_ids } : {}), ...(dryRun ? { dry_run: true } : {}) };
    let out;
    try {
      out = await call('propose_doc_change', args);
    } catch (e) {
      out = { error: e.message };
    }
    if (out.error) {
      refused++;
      log(`line ${l.line}: refused: ${out.error}`);
    } else {
      const grows = out.grows?.length ? ` (grows: ${out.grows.join('; ')})` : '';
      log(`line ${l.line}: ok${dryRun ? ' (dry run)' : ` ${out.changeId} v${out.version}`}${grows}`);
    }
  }
  return { refused };
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const argv = process.argv.slice(2);
  const file = argv.find((a, i) => !a.startsWith('--') && !['--url', '--token-file'].includes(argv[i - 1]));
  if (!file) {
    console.error('Usage: doc-proposals-import.mjs <file.jsonl> [--dry-run] [--url <mcp url>] [--token-file <file>]');
    process.exit(2);
  }
  const call = mcpClient({ url: option(argv, '--url', DEFAULT_URL), tokenFile: option(argv, '--token-file', DEFAULT_TOKEN_FILE) });
  const { refused } = await importProposals(readFileSync(file, 'utf8'), { call, dryRun: argv.includes('--dry-run') });
  process.exitCode = refused ? 1 : 0;
}
