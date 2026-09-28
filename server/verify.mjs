// The allowed values of the workbook's dropdown lists (server/verifications.mjs),
// read from the local copy: fixed lists as they are, and lists that come from
// another sheet's column (Lists, Location_data, Taxonomy, Insectary_stocks).
// Cached until one of those sheets changes.

import { LISTS } from './verifications.mjs';
import { moduleMap } from './schema.mjs';

const cache = new Map();

function columnValues(store, sheet, field) {
  const mod = moduleMap.get(sheet);
  if (!mod) return null;
  const revision = store.db
    .prepare('SELECT count(*) n, max(updated_at) u FROM records WHERE sheet=? AND missing=0')
    .get(sheet);
  const key = `${sheet}\u0000${field}`;
  const stamp = `${revision.n}:${revision.u}`;
  const hit = cache.get(key);
  if (hit?.stamp === stamp) return hit.values;
  const values = new Set();
  for (const r of store.db
    .prepare('SELECT values_json FROM records WHERE sheet=? AND missing=0 AND row_num>?')
    .all(sheet, mod.headerRow)) {
    const value = JSON.parse(r.values_json)[field];
    if (value !== null && value !== undefined && String(value).trim() !== '') values.add(String(value).trim());
  }
  values.delete(field); // a header repeated inside the column
  cache.set(key, { stamp, values });
  return values;
}

/** The lists of one sheet: `{ field: { strict, source, values: Set } }`. */
export function listOptions(store, sheet) {
  const out = {};
  for (const [field, rule] of Object.entries(LISTS[sheet] || {})) {
    const values = rule.values ? new Set(rule.values) : columnValues(store, rule.from[0], rule.from[1]);
    if (!values?.size) continue;
    out[field] = {
      strict: !!rule.strict,
      source: rule.values ? 'lista fija de la hoja' : `${rule.from[0]} · ${rule.from[1]}`,
      values,
    };
  }
  return out;
}

/** Why a value is not allowed in a list column, or null. Blank cells are always allowed. */
export function listProblem(options, field, value) {
  const list = options[field];
  if (!list || value === null || value === undefined || typeof value === 'object') return null;
  const text = String(value).trim();
  if (!text || list.values.has(text)) return null;
  return `${field}: «${text}» no está en la lista de la hoja (${list.source})`;
}
