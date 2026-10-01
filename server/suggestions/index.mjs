// Suggested edits for Revisión de datos: each source is a module exporting
// { id, title, describe, async suggest(ctx) } where suggest returns
// [{ sheet, row, recordId, field, current, suggested, certainty, reason }]
// (certainty 'certain' | 'likely' | 'check'). Nothing is written here: a
// person applies the edits they accept through the usual save.
//
// Minimal registry; the Suggested edits tab of Revisión de datos lists the
// sources and runs them with ctx = { store, user }.

import * as wikilocTransects from './wikiloc-transects.mjs';

export const SOURCES = [wikilocTransects];

const CERTAINTIES = new Set(['certain', 'likely', 'check']);

/** Whether a suggestion has the shape every source must return. */
export function validSuggestion(s) {
  return (
    !!s &&
    typeof s.sheet === 'string' &&
    Number.isInteger(s.row) &&
    typeof s.recordId === 'string' &&
    typeof s.field === 'string' &&
    'current' in s &&
    'suggested' in s &&
    CERTAINTIES.has(s.certainty) &&
    typeof s.reason === 'string'
  );
}

/** The suggestions of every source (or of the one named), each tagged with its source. */
export async function runSuggestions(ctx, sourceId = null) {
  const out = [];
  for (const source of SOURCES) {
    if (sourceId && source.id !== sourceId) continue;
    for (const s of await source.suggest(ctx)) if (validSuggestion(s)) out.push({ source: source.id, ...s });
  }
  return out;
}

export const listSources = () => SOURCES.map(s => ({ id: s.id, title: s.title, describe: s.describe }));
