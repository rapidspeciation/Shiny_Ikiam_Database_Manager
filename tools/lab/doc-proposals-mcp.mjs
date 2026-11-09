// The app's MCP endpoint for an outside proposer of instruction changes (tools/lab/doc-proposals.md): the address
// and the token from the command line or their defaults, and one tool call.
import { readFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { join } from 'node:path';

export const DEFAULT_URL = 'http://127.0.0.1:8841/api/ai/mcp';
export const DEFAULT_TOKEN_FILE = join(homedir(), '.cache', 'ithomiini-lab', 'doc-comments-20261108', 'auditor-token.txt');

/** `--name value` from argv. */
export const option = (argv, name, fallback = undefined) => {
  const i = argv.indexOf(name);
  return i >= 0 ? argv[i + 1] : fallback;
};

/** A client for `url` with the token in `tokenFile`: call(tool, args) → the tool's answer (parsed JSON), or throws. */
export function mcpClient({ url = DEFAULT_URL, tokenFile = DEFAULT_TOKEN_FILE } = {}) {
  const token = readFileSync(tokenFile, 'utf8').trim();
  let id = 0;
  return async function call(name, args = {}) {
    const res = await fetch(url, {
      method: 'POST',
      headers: { 'content-type': 'application/json', authorization: `Bearer ${token}` },
      body: JSON.stringify({ jsonrpc: '2.0', id: ++id, method: 'tools/call', params: { name, arguments: args } }),
    });
    const data = await res.json().catch(() => null);
    if (!res.ok || !data?.result) throw new Error(`${name}: HTTP ${res.status} ${data?.error?.message ?? ''}`.trim());
    const text = data.result.content?.[0]?.text ?? '{}';
    try {
      return JSON.parse(text);
    } catch {
      return { text };
    }
  };
}
