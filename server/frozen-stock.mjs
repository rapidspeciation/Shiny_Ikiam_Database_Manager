// Flash-frozen stock per species, for the offline identifier page (/prueba-offline/): beside each of the top 5
// species, how many specimens the project already keeps flash frozen, males and females, so a collector in the
// field can decide whether to keep a new one. A small snapshot the page keeps offline and asks for again by ETag.
//
// The rule (docs: .work/offline-id-20261029, report of 31 Oct 2026):
// - Collection_data: Release_Collect Collected_Preserved and a Preservation_medium that says Flash frozen
//   ("Flash frozen", "DMSO buffer & Flash frozen"). Butterflies sent to the insectary count from their
//   Insectary_data row instead.
// - Insectary_data: Preservation_medium, T1_Preservation_medium or T2_Preservation_medium says Flash frozen, and
//   the butterfly is an adult (LIFESTAGE empty, Adult or NA: eggs, larvae and pre-pupae are left out).
// - F1/F2_MutationRate is not read: every flash-frozen row there is also an Insectary_data row (same CAM_ID).
//   Wing_tissue (pupal wing dissections) is not a specimen stock either.
// - Gone from the stock: the body's location (Collection_data Location_WholeBody, or Location_abdomen when the
//   body was split or the cell is empty; Insectary_data Location_body) says Lost, Missing, [Sent for sequencing]
//   or another institution (Paris, Brazil, Cambridge, Sheffield, York). Ikiam, Sanger (the TOL704 freezer),
//   Splitted body, NA, empty and any place not listed count as still there.
// - One specimen per CAM_ID (a CAM in both sheets counts once, and not at all when either row says it is gone);
//   rows without a CAM count on their own.
// - Sex: male -> M, female -> F; uncertain ("male ?", "female_?"), NOT_COLLECTED, NA or empty -> unknown.
// - Species: the SPECIES cell's first two words (a subspecies counts for its species). A cross counts for its
//   species when both sides are the same species ("Mechanitis polymnia werneri x proceriformis"); a cross of two
//   species is not counted. Names are matched to the identifier head's species (head_<sha>.json `species`) by
//   case (ALIASES for two the head names otherwise); names the head does not know count only in their genus
//   when the head knows the genus (and in `unmapped`).

import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { recordsStamp } from './checks.mjs';
import { MODELS } from './offline-id.mjs';
import { moduleMap } from './schema.mjs';

/** Workbook names the head knows under another name (checked against head_b8127dbf on 31 Oct 2026). */
export const ALIASES = {
  'placidina euryanassa': 'Pagyris euryanassa', // the head's genus for it
  'perrybris pamela': 'Perrhybris pamela', // a typo in the workbook
};

const text = value => (value === null || value === undefined ? '' : String(value).trim());
const FLASH = /flash\s*frozen/i;
const ADULT_STAGE = /^(|adult|na|n\/a)$/i;
const GONE_PLACE = /^(lost|missing|\[?sent for sequencing|paris|brazil|cambridge|sheffield|york)\b/i;

/**
 * The binomial a SPECIES cell counts for, or null (empty, a family or tribe, a cross of two species). `isSpecies`
 * (the head's names, lower case): a bare epithet on a cross's second side that is a species of the genus
 * ("Melinaea menophilus x mothone") makes it a cross of two species, not of two subspecies.
 */
export function speciesOf(value, isSpecies = () => false) {
  const sides = text(value)
    .split(/\s*×\s*|\s+(?:x|vs\.?)\s+/i)
    .map(s => s.split(/\s+/).filter(Boolean));
  let name = null;
  for (const words of sides) {
    if (!words.length) continue;
    // "proceriformis" on the second side of a cross: the first side's species.
    let binomial = /^[A-Z]/.test(words[0]) ? (words.length >= 2 ? `${words[0]} ${words[1].toLowerCase()}` : null) : name;
    if (!/^[A-Z]/.test(words[0]) && name && words.length === 1 && isSpecies(`${name.split(' ')[0]} ${words[0]}`.toLowerCase())) binomial = `${name.split(' ')[0]} ${words[0].toLowerCase()}`;
    if (!binomial || /^(sp|spp|cf|aff)\.?$/i.test(binomial.split(' ')[1]) || /^\(/.test(binomial.split(' ')[1])) return null;
    if (name && binomial !== name) return null;
    name = binomial;
  }
  return name;
}

/** The genus a SPECIES cell names ("Dircenna sp." -> Dircenna), or null. */
export function genusOf(value) {
  const first = text(value).split(/\s+/)[0] ?? '';
  return /^[A-Z][a-z]+$/.test(first) && !/(idae|inae|ini)$/.test(first) ? first : null;
}

export function sexOf(value) {
  const v = text(value).toLowerCase();
  return v === 'male' ? 'M' : v === 'female' ? 'F' : 'unknown';
}

/** The body's place for the rule (see above): where the specimen is, or '' when not filled. */
function placeOf(sheet, values) {
  if (sheet === 'Insectary_data') return text(values.Location_body);
  const whole = text(values.Location_WholeBody);
  return !whole || /^splitted body$/i.test(whole) ? text(values.Location_abdomen) : whole;
}

/** Why a row is not in the stock, or null when it is (rows of Collection_data and Insectary_data). */
export function frozenWhyNot(sheet, values) {
  if (sheet === 'Collection_data') {
    if (text(values.Release_Collect) !== 'Collected_Preserved') return 'not_preserved';
    if (!FLASH.test(text(values.Preservation_medium))) return 'not_flash_frozen';
  } else if (sheet === 'Insectary_data') {
    if (![values.Preservation_medium, values.T1_Preservation_medium, values.T2_Preservation_medium].some(v => FLASH.test(text(v)))) return 'not_flash_frozen';
    if (!ADULT_STAGE.test(text(values.LIFESTAGE))) return 'not_adult';
  } else return 'other_sheet';
  if (GONE_PLACE.test(placeOf(sheet, values))) return 'gone';
  return null;
}

const blank = () => ({ M: 0, F: 0, unknown: 0, total: 0 });
const add = (map, key, sex) => {
  const e = map[key] ?? (map[key] = blank());
  e[sex]++;
  e.total++;
};

/**
 * The snapshot from rows of Collection_data and Insectary_data ({ sheet, values }) and the head's species list:
 * { species: { name: {M,F,unknown,total} }, genus: {...}, counted, gone, unmapped: { rows, names: {name: n} } }.
 */
export function countFrozen(rows, headSpecies) {
  const known = new Map(headSpecies.map(s => [s.toLowerCase(), s]));
  const genera = new Set(headSpecies.map(s => s.split(' ')[0]));
  const isSpecies = name => known.has(name);
  const camOf = values => {
    const cam = text(values.CAM_ID).replace(/\s+/g, '').toUpperCase();
    return /^CAM\d+$/.test(cam) ? cam : null;
  };
  // A CAM that any row says is gone is not counted from its other row either.
  const goneCams = new Set();
  const why = rows.map(({ sheet, values }) => {
    const w = frozenWhyNot(sheet, values);
    if (w === 'gone' && camOf(values)) goneCams.add(camOf(values));
    return w;
  });
  const species = {},
    genus = {},
    unmapped = { rows: 0, names: {} };
  const seen = new Set();
  let counted = 0,
    gone = 0;
  rows.forEach(({ values }, i) => {
    const cam = camOf(values);
    if (why[i] === 'gone' || (!why[i] && cam && goneCams.has(cam))) {
      if (!cam || !seen.has(cam)) gone++;
      if (cam) seen.add(cam);
      return;
    }
    if (why[i]) return;
    if (cam) {
      if (seen.has(cam)) return;
      seen.add(cam);
    }
    counted++;
    const sex = sexOf(values.Sex);
    const name = speciesOf(values.SPECIES, isSpecies);
    const head = name && known.get((ALIASES[name.toLowerCase()] ?? name).toLowerCase());
    if (head) add(species, head, sex);
    else {
      unmapped.rows++;
      const label = text(values.SPECIES) || '(vacío)';
      unmapped.names[label] = (unmapped.names[label] ?? 0) + 1;
    }
    const g = head ? head.split(' ')[0] : genusOf(values.SPECIES);
    if (g && genera.has(g)) add(genus, g, sex);
  });
  const sorted = obj => Object.fromEntries(Object.entries(obj).sort(([a], [b]) => a.localeCompare(b)));
  return { species: sorted(species), genus: sorted(genus), counted, gone, unmapped: { rows: unmapped.rows, names: sorted(unmapped.names) } };
}

/** The head's species list (the identifier's offline head, as list.json serves it), read once per file. */
let headMemo = { path: '', species: null };
async function headSpecies(identifierDir) {
  const path = join(identifierDir, MODELS.find(m => m.role === 'head-meta').path);
  if (headMemo.path !== path) headMemo = { path, species: JSON.parse(await readFile(path, 'utf8')).species };
  return headMemo.species;
}

/** The two sheets' rows straight from the local copy (not every sheet: this runs in the app's thread). */
function stockRows(store) {
  const out = [];
  for (const sheet of ['Collection_data', 'Insectary_data']) {
    const header = moduleMap.get(sheet)?.headerRow ?? 1;
    const rows = store.db
      .prepare('SELECT values_json FROM records WHERE sheet=? AND missing=0 AND observed=1 AND row_num>? AND row_num<2000000000 ORDER BY row_num')
      .all(sheet, header);
    for (const r of rows) out.push({ sheet, values: JSON.parse(r.values_json) });
  }
  return out;
}

const cache = new WeakMap();
const pending = new WeakMap();
/**
 * The snapshot for the page, computed again only when the local copy changed:
 * { version, generatedAt, species, genus, counted, gone, unmapped: { rows } }. `version` hashes what is sent
 * (not when), so a change elsewhere in the workbook keeps the page's copy (ETag) valid.
 */
export async function frozenStock(store, identifierDir) {
  const stamp = recordsStamp(store);
  const hit = cache.get(store);
  if (hit?.stamp === stamp && hit.dir === identifierDir) return hit.value;
  const running = pending.get(store);
  if (running?.stamp === stamp) return running.promise;
  const promise = (async () => {
    const counts = countFrozen(stockRows(store), await headSpecies(identifierDir));
    const sent = { species: counts.species, genus: counts.genus, counted: counts.counted, gone: counts.gone, unmapped: { rows: counts.unmapped.rows } };
    const version = createHash('sha256').update(JSON.stringify(sent)).digest('hex').slice(0, 16);
    const value = { version, generatedAt: hit?.value.version === version ? hit.value.generatedAt : new Date().toISOString(), ...sent };
    cache.set(store, { stamp, dir: identifierDir, value });
    return value;
  })().finally(() => pending.delete(store));
  pending.set(store, { stamp, promise });
  return promise;
}

/**
 * When the local copy was last known to match the workbook (the inventory's date), or null: the last sync's
 * check (a sync that finds the workbook unchanged only moves checkedAt), else the last sync that read it
 * (lastSync, also after a restart or when the last sync was stale).
 */
export function syncedAt(store) {
  const status = store.syncStatus ?? {};
  if (status.checkedAt && status.state !== 'stale' && status.state !== 'error') return status.checkedAt;
  return status.lastSync ?? store.getSetting?.('lastSync') ?? null;
}
