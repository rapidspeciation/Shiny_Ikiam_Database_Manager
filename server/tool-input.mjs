// One input check for every db tool (MCP tools/call, server/assistant.mjs executeTool).
//
// Chats call a deferred tool before its schema is loaded and guess its keys (retro 16 Nov 2026: about
// 130 failed calls in a week, e.g. host_plants with `species`, show_photos with `cams`, tubes_lookup with `{}`).
// The tool still runs as before (some tools read keys their schema leaves out on purpose, ACCEPTED
// below); its answer then names each key it does not know with the closest one it has, and an empty
// input that fails says which key the tool needs, with an example.

/**
 * Keys a tool reads that its schema leaves out (fewer bytes in the chats' context, or an older call
 * shape): never "unknown", and given they may stand for a required key. From an audit of the handlers (16 Nov).
 */
export const ACCEPTED_KEYS = {
  search_text: ['text', 'q'],
  get_record: ['key', 'recordId'],
  find_records: ['sheet', 'groupBy'],
  describe_sheet: ['sheet'],
  count_records: ['module', 'field', 'fields'],
  host_plants: ['auto_pick'],
  maps_snapshot: ['taxa', 'near'],
  send_sheet_photos: ['sheet'],
  rack_deposit: ['leave_out', 'restore'],
  rack_report: ['racks', 'responsable', 'ultracongelador'],
  update_websites: ['fresh'],
  list_history: ['formulas'],
};

/**
 * Words chats use for a key, by what they mean: a guessed key that is one of a group's words points to
 * the first word of that group the tool has. Shared by every tool (one table, not one per tool).
 */
const SAME_MEANING = [
  ['butterfly', 'species', 'taxon', 'taxa', 'taxa_names', 'name', 'butterflies', 'genus', 'scientific_name'],
  ['ids', 'cams', 'cam_ids', 'camids', 'cam', 'cam_id', 'specimens', 'records', 'id_list', 'recordIds'],
  ['tubes', 'tube_ids', 'tube', 'ids', 'codes'],
  ['id', 'recordId', 'record_id', 'record', 'cam', 'cam_id', 'row', 'key'],
  ['reading', 'query', 'q', 'text', 'search', 'rack', 'rack_reading'],
  ['query', 'q', 'text', 'search', 'sql'],
  ['module', 'sheet', 'table', 'tab', 'module_name'],
  ['sheets', 'sheet', 'sheet_ids', 'sheet_id', 'photo_sheets', 'contact_sheets'],
  ['tiles', 'tile', 'picks', 'chosen', 'numbers', 'photos'],
  ['plant', 'host_plant', 'hostplant', 'plants'],
  ['area', 'region', 'province', 'location', 'place'],
  ['proposalId', 'proposal', 'proposal_id'],
  ['fields', 'columns', 'cols', 'field'],
  ['limit', 'max', 'max_results', 'n', 'count', 'top'],
  ['lang', 'language', 'locale'],
];

/**
 * Example values for "needs …, e.g. …", only for keys that mean the same in every tool that has them
 * (an `id` or `query` differs from tool to tool: no example; `confirmed` never gets one).
 */
const EXAMPLES = {
  ids: ['CAM079891'],
  butterfly: 'Mechanitis messenoides',
  species: ['Mechanitis messenoides'],
  taxon: 'Mechanitis messenoides',
  plant: 'Solanum',
  tubes: ['FS50849233'],
  sheets: ['<a sheet id from photo_sheets>'],
  tiles: [1, 4],
};

const norm = key => String(key).toLowerCase().replace(/[^a-z0-9]/g, '');

/** Edit distance (small strings only: tool keys). */
function distance(a, b) {
  const row = Array.from({ length: b.length + 1 }, (_, i) => i);
  for (let i = 1; i <= a.length; i++) {
    let prev = row[0];
    row[0] = i;
    for (let j = 1; j <= b.length; j++) {
      const keep = row[j];
      row[j] = Math.min(row[j] + 1, row[j - 1] + 1, prev + (a[i - 1] === b[j - 1] ? 0 : 1));
      prev = keep;
    }
  }
  return row[b.length];
}

/** The tool's key closest to a guessed one, or null: same meaning first, then spelling. */
export function closestKey(guess, keys) {
  const g = norm(guess);
  const byNorm = keys.find(k => norm(k) === g);
  if (byNorm) return byNorm;
  for (const group of SAME_MEANING) {
    if (!group.some(w => norm(w) === g)) continue;
    const hit = group.find(w => keys.includes(w) && norm(w) !== g);
    if (hit) return hit;
  }
  let best = null,
    bestD = Infinity;
  for (const k of keys) {
    const n = norm(k);
    const d = n.includes(g) || g.includes(n) ? Math.abs(n.length - g.length) / 2 : distance(g, n);
    if (d < bestD) [best, bestD] = [k, d];
  }
  return best !== null && bestD <= Math.max(1, Math.floor(g.length / 3)) ? best : null;
}

const isEmpty = args => !args || typeof args !== 'object' || Array.isArray(args) || Object.keys(args).length === 0;

/** Example values of some keys, for those that have one (EXAMPLES, an enum): { key: value }, maybe empty. */
function exampleOf(schema, keys) {
  const props = schema?.properties ?? {};
  return Object.fromEntries(
    keys
      .map(k => (Object.hasOwn(EXAMPLES, k) ? [k, EXAMPLES[k]] : props[k]?.enum?.length && k !== 'confirmed' ? [k, props[k].enum[0]] : null))
      .filter(Boolean),
  );
}
const eg = (schema, keys) => {
  const example = exampleOf(schema, keys);
  return Object.keys(example).length ? `, e.g. ${JSON.stringify(example)}` : '';
};

/** "show_photos needs ids, e.g. {"ids":["CAM079891"]}": its required keys, or null without any. */
export function needsText(name, schema) {
  const need = schema?.required ?? [];
  return need.length ? `${name} needs ${need.join(' and ')}${eg(schema, need)}` : null;
}

/** The keys given that the tool does not know, each with the closest it has: [{ key, use }]. */
export function unknownKeys(name, schema, args) {
  if (isEmpty(args)) return [];
  const keys = Object.keys(schema?.properties ?? {});
  const accepted = new Set([...keys, ...(ACCEPTED_KEYS[name] ?? [])]);
  return Object.keys(args)
    .filter(k => !accepted.has(k))
    .map(key => ({ key, use: closestKey(key, keys) }));
}

/** The required keys missing from a non-empty input; none when it has a key the tool reads in their place (ACCEPTED_KEYS). */
const missingKeys = (name, schema, args) =>
  isEmpty(args) || (ACCEPTED_KEYS[name] ?? []).some(k => args[k] !== undefined)
    ? []
    : (schema?.required ?? []).filter(k => args[k] === undefined || args[k] === null);

const unknownLine = (name, schema, unknown) => {
  const keys = Object.keys(schema?.properties ?? {});
  const each = unknown.map(u => (u.use ? `Unknown key '${u.key}': use '${u.use}'` : `Unknown key '${u.key}'`));
  const list = unknown.some(u => !u.use)
    ? keys.length
      ? ` (${name}'s keys: ${keys.join(', ')})`
      : ` (${name} takes no keys)`
    : '';
  return `${each.join('. ')}${list}.`;
};

/**
 * A tool's answer with the input notes: an error starts with what was wrong in the input (unknown
 * keys, missing required keys, or what an empty input needs; then the tool's own error); an
 * answer that worked keeps its content and says in `inputWarning` which keys were not the tool's.
 * `out`: the tool's answer (an object, or text for query).
 */
export function withInputNotes(name, schema, args, out) {
  if (!schema) return out;
  const unknown = unknownKeys(name, schema, args);
  const missing = missingKeys(name, schema, args);
  const failed = out && typeof out === 'object' && out.error !== undefined && out.error !== null;
  if (failed) {
    const own = typeof out.error === 'string' ? out.error : JSON.stringify(out.error);
    if (unknown.length || missing.length) {
      const parts = [];
      if (unknown.length) parts.push(unknownLine(name, schema, unknown));
      if (missing.length) parts.push(`Missing key${missing.length > 1 ? 's' : ''}: ${missing.map(k => `'${k}'`).join(', ')}${eg(schema, missing)}.`);
      return { ...out, error: `${parts.join(' ')} ${own}`.trim() };
    }
    // An empty input fails a tool with required keys for want of them: what it needs goes first, the tool's own
    // error after (undo_edits keeps its "only after they approve"). A tool without (list_invitations) may fail
    // for another reason (only administrators), so its own error stays alone.
    const needs = isEmpty(args) ? needsText(name, schema) : null;
    if (needs) return { ...out, error: `${needs}. ${own}` };
    return out;
  }
  if (!unknown.length) return out;
  // Some tools read a key their schema leaves out (ACCEPTED_KEYS may miss one): the note says the key is not in the schema, not that it was dropped.
  const warning = `Not in ${name}'s schema: ${unknown.map(u => (u.use ? `'${u.key}' (use '${u.use}')` : `'${u.key}'`)).join(', ')}. Check the answer is what you asked for.`;
  if (typeof out === 'string') return `${out}\n\nNote: ${warning}`;
  if (out && typeof out === 'object' && !Array.isArray(out)) return { ...out, inputWarning: warning };
  return out;
}
