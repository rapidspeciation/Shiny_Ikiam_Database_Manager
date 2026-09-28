#!/usr/bin/env node
// Stands in for `claude -p`: calls the app's MCP tools like Claude would, then prints a result.
import { readFileSync } from 'node:fs';

const args = process.argv.slice(2);
const config = JSON.parse(args[args.indexOf('--mcp-config') + 1]).mcpServers.ithomiini;
const input = JSON.parse(readFileSync(0, 'utf8').trim());
const prompt = input.message.content.at(-1).text;
let id = 0;
const call = async (method, params) => {
  const response = await fetch(config.url, {
    method: 'POST',
    headers: { 'content-type': 'application/json', ...config.headers },
    body: JSON.stringify({ jsonrpc: '2.0', id: ++id, method, params }),
  });
  return (await response.json()).result;
};
await call('initialize', { protocolVersion: '2025-06-18', capabilities: {} });
const tool = async (name, args) => JSON.parse((await call('tools/call', { name, arguments: args })).content[0].text);
let text = 'Sin cambios';
if (/cuaderno/.test(prompt)) {
  const { found, missing } = await tool('find_records', {
    module: 'Insectary_data',
    field: 'Insectary_ID',
    values: ['5VB', '8VD', '9ZZ'],
  });
  const byId = Object.fromEntries(found.map(r => [r.values.Insectary_ID, r]));
  const out = await tool('propose_changes', {
    reason: 'Cuaderno de insectario, agosto 2025',
    changes: [
      { recordId: byId['5VB'].id, values: { SPECIES: 'Mechanitis messenoides deceptus' }, note: 'emergió deceptus' },
      { recordId: byId['8VD'].id, values: { 'CLUTCH NUMBER': 848 }, note: 'cuaderno: 848' },
    ],
  });
  text = `Propuesta ${out.proposalId} con ${out.rows} filas; no encontrado: ${missing.join(', ')}`;
}
if (/aplica/.test(prompt)) {
  const [, proposalId] = /propuesta (\S+)/.exec(prompt);
  const out = await tool('apply_proposal', { proposalId, indexes: [1] });
  text = `Aplicado: ${out.status} ${out.rows}`;
}
const session = args.includes('--resume') ? args[args.indexOf('--resume') + 1] : args[args.indexOf('--session-id') + 1];
console.log(JSON.stringify({ type: 'system', subtype: 'init', session_id: session }));
console.log(JSON.stringify({ type: 'result', subtype: 'success', is_error: false, result: text, session_id: session }));
