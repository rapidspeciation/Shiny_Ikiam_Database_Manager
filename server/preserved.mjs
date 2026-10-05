// Insectary butterflies preserved without their CAM or tube. A row of
// Insectary_data counts as preserved when one of its preservation cells says
// so: Death_cause Killed_Preserved, Preserved_Dead_Alive Alive or Dead, a
// WHOLE_ORGANISM tube, or a preservation medium (Flash frozen, Ethanol, DMSO).
// Such a row needs a CAM_ID and a Tube_1_id: an ID, or NA when there is none
// on purpose (wings only: a CAM and Tube NA; Tube_1_tissue NOT_COLLECTED).
// An empty cell or a word ("BUSCAR") is missing. Formula cells are the
// sheet's business, and a body whose Location_body says Lost has nothing left
// to number. Notes do not decide anything here.
//
// Shared by Revisión (server/checks.mjs, kinds missing_sample and
// preserved_na), the alerts (server/alerts.mjs) and the proposals table
// (server/assistant.mjs: the cells marked before applying), so all three agree.

import { msg } from './messages.mjs';
import { isIdValue } from './verifications.mjs';

const text = value => (value === null || value === undefined ? '' : String(value).trim());
const same = (value, word) => text(value).toLowerCase() === word.toLowerCase();
const isNA = value => /^(NA|N\/A)$/i.test(text(value));

export const KILLED = 'Killed_Preserved';
export const PRESERVED_STATES = ['Alive', 'Dead'];
export const PRESERVED_MEDIA = ['Flash frozen', 'Ethanol', 'DMSO'];
export const WHOLE = 'WHOLE_ORGANISM';
/** The cells a death writes. */
export const DEATH_FIELDS = ['Death_date', 'Death_cause', 'Preserved_Dead_Alive'];

/** The cells that say an Insectary_data row was preserved, as "column value" ([] when none does). */
export function preservedBy(values) {
  const out = [];
  if (same(values.Death_cause, KILLED)) out.push(`Death_cause ${KILLED}`);
  const state = PRESERVED_STATES.find(s => same(values.Preserved_Dead_Alive, s));
  if (state) out.push(`Preserved_Dead_Alive ${state}`);
  for (const field of ['Tube_1_tissue', 'Tube_2_tissue']) if (same(values[field], WHOLE)) out.push(`${field} ${WHOLE}`);
  const medium = PRESERVED_MEDIA.find(m => same(values.Preservation_medium, m));
  if (medium) out.push(`Preservation_medium ${medium}`);
  return out;
}

/**
 * What a preserved Insectary_data row lacks, or null:
 * - { kind: 'missing_sample', missing: ['CAM_ID', 'Tube_1_id'] (one or both), why }
 * - { kind: 'preserved_na', missing: ['CAM_ID', 'Tube_1_id'], why }: Death_cause
 *   Killed_Preserved, but CAM_ID NA and no tube: the cause says preserved, the
 *   preservation cells say not preserved.
 * `why`: the cells that say it was preserved (preservedBy).
 */
export function sampleGap(values, formulas = {}) {
  const why = preservedBy(values);
  if (!why.length || /^lost/i.test(text(values.Location_body))) return null;
  const missing = [];
  if (!formulas.CAM_ID && !isIdValue(values.CAM_ID) && !isNA(values.CAM_ID)) missing.push('CAM_ID');
  if (
    !formulas.Tube_1_id &&
    !isIdValue(values.Tube_1_id) &&
    !isNA(values.Tube_1_id) &&
    !same(values.Tube_1_tissue, 'NOT_COLLECTED')
  )
    missing.push('Tube_1_id');
  if (missing.length) return { kind: 'missing_sample', missing, why };
  const noTube = [1, 2, 3, 4].every(n => !isIdValue(values[`Tube_${n}_id`]));
  if (same(values.Death_cause, KILLED) && !formulas.CAM_ID && isNA(values.CAM_ID) && noTube)
    return { kind: 'preserved_na', missing: ['CAM_ID', 'Tube_1_id'], why };
  return null;
}

/** Cells whose writing can make a butterfly preserved, or give it its CAM or tube. */
const SAMPLE_FIELDS = new Set([
  ...DEATH_FIELDS,
  'CAM_ID',
  'Tube_1_id',
  'Tube_1_tissue',
  'Tube_2_tissue',
  'Location_body',
]);
/**
 * The cells a proposal row (server/assistant.mjs) would leave missing on a
 * preserved insectary butterfly, each with a msg() saying so: { field: { text,
 * msg } }, or null. Existing rows count only when the row writes one of their
 * death or preservation cells (an old gap elsewhere in the row is Revisión's);
 * new rows always. `record`: the existing row (null for a new one).
 */
export function proposalSampleWarnings(change, record) {
  if (change?.sheet !== 'Insectary_data' || change.context) return null;
  const writes = Object.keys(change.values ?? {});
  if (!change.create && !writes.some(f => SAMPLE_FIELDS.has(f))) return null;
  const old = change.create ? {} : (record ?? {});
  const values = { ...(old.values ?? {}), ...change.values };
  const formulas = Object.fromEntries(Object.entries(old.formulas ?? {}).filter(([f]) => !writes.includes(f)));
  const gap = sampleGap(values, formulas);
  if (!gap) return null;
  return Object.fromEntries(
    gap.missing.map(field => [
      field,
      gap.kind === 'preserved_na'
        ? msg('Death_cause dice Killed_Preserved, pero {field} es NA: pregunta al equipo', { field })
        : msg('Preservada sin {field}: pregunta al equipo', { field }),
    ]),
  );
}
