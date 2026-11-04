// Only public assistant output and tool names cross the bridge. Reasoning items,
// tool inputs and results are deliberately outside this allowlist.
export function progressUpdates(items, seen, { personal = false } = {}) {
  const updates = [];
  for (const item of items) {
    if (!['assistant_message', 'dynamic_tool', 'command_execution', 'subagent'].includes(item.type)) continue;
    let p;
    try { p = JSON.parse(item.payload_json); } catch { continue; }
    if (!p || typeof p !== 'object') continue;
    if (item.type === 'assistant_message') {
      if (!personal || typeof p.text !== 'string' || !p.text.trim()) continue;
      // This row changes in place during streaming; ordinal alone is not a cursor.
      const text = p.text.slice(-600);
      if (seen.get(item.ordinal) === text) continue;
      seen.set(item.ordinal, text);
      updates.push({ kind: 'assistant', text });
    } else {
      if (seen.has(item.ordinal)) continue;
      seen.set(item.ordinal, true);
      updates.push({ tool: p.toolName ?? item.type, title: '' });
    }
  }
  return updates;
}
