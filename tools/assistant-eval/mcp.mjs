// Direct MCP calls to an app (no model): the unit tier of the eval and ground-truth probes.
// The token is read from a T3 workspace's .mcp.json (never printed).
import { readFileSync } from 'node:fs';

export function mcpFrom(workspace = '/home/franz/.t3-ithomiini-lab/workspaces/lab') {
  const cfg = JSON.parse(readFileSync(`${workspace}/.mcp.json`, 'utf8')).mcpServers.db;
  let id = 0;
  async function rpc(method, params = {}) {
    const res = await fetch(cfg.url, {
      method: 'POST',
      headers: { 'content-type': 'application/json', accept: 'application/json, text/event-stream', ...cfg.headers },
      body: JSON.stringify({ jsonrpc: '2.0', id: ++id, method, params }),
    });
    const body = await res.json();
    if (body.error) throw new Error(`${method}: ${body.error.message}`);
    return body.result;
  }
  return {
    url: cfg.url,
    tools: async () => (await rpc('tools/list')).tools,
    /** The server's `initialize` answer (its instructions go into the chat's fixed context). */
    init: async () => rpc('initialize', { protocolVersion: '2025-06-18', capabilities: {}, clientInfo: { name: 'assistant-eval', version: '1' } }),
    /** → { ms, chars, isError, text } */
    call: async (name, args = {}) => {
      const t0 = performance.now();
      const r = await rpc('tools/call', { name, arguments: args });
      const text = r.content.filter(c => c.type === 'text').map(c => c.text).join('\n');
      return { ms: Math.round(performance.now() - t0), chars: text.length, isError: !!r.isError, text, images: r.content.filter(c => c.type === 'image').length };
    },
  };
}
