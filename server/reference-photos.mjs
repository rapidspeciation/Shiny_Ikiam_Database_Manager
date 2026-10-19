// reference_photos: photos of a species to compare a butterfly with. (a) The collection's own: butterflies
// of that species in the workbook with their dorsal and ventral photos in Photo_links, those whose
// species the Wings Gallery's model also gives first. (b) Field and specimen photos from butterfly sites
// of Ecuador, a small set made on Franz's computer (scripts/reference-photos.mjs: up to 6 field and
// 4 specimen photos a species, 640 px) and copied to the server's shared folder
// (REFERENCE_PHOTOS_DIR, reference-photos/ beside the database): its index.json and the pictures.
// The pictures themselves are put together by server/drive-photos.mjs (the collection's come from Drive).

import { existsSync, readFileSync, statSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { photoIndex, reviewData } from './photodata.mjs';

/** A page: the collection's butterflies (one picture each, dorsal and ventral), field and specimen photos. */
export const PAGE = { collection: 2, field: 3, specimen: 2 };
/** The sites the set is made from, as the answers name them. */
export const SITES = {
  sangay: 'sangay.eu',
  noreste: 'noreste.eu',
  cotacachi: 'cotacachi.eu',
  'loja-zamora': 'loja-zamora.eu',
  'amazonian-butterflies': 'amazonian-butterflies.net',
  neotropicalbutterflies: 'neotropicalbutterflies.com',
};

const text = v => (v === null || v === undefined ? '' : String(v).trim());
const fold = s =>
  text(s)
    .normalize('NFD')
    .replace(/[̀-ͯ]/g, '')
    .toLowerCase()
    .replace(/\s+/g, ' ');
const r2 = n => Math.round(n * 100) / 100;

/** "Mechanitis polymnia proceriformis", "mechanitis  polymnia" → { binomial: 'Mechanitis polymnia', subspecies }; null for a genus alone, "sp." or a hybrid's first name. */
export function speciesOf(name) {
  const words = text(name)
    .replace(/[?()]/g, ' ')
    .split(/\s+/)
    .filter(Boolean);
  if (words.length < 2 || /^(?:sp|spp|cf|aff|x|nr)\.?$/i.test(words[1])) return null;
  const genus = words[0][0].toUpperCase() + words[0].slice(1).toLowerCase();
  const sub = words[2] && !/^(?:x|sp\.?|ssp\.?|f\.?|form)$/i.test(words[2]) ? words[2].toLowerCase() : null;
  return { binomial: `${genus} ${words[1].toLowerCase()}`, subspecies: sub };
}

/** A sex as asked or written in the workbook ("female ?" too): 'female', 'male' or null. */
export const sexOf = v => {
  const s = fold(v).replace(/[\s_]*\?$/, '');
  return /^(?:female|f|hembra|♀)$/.test(s) ? 'female' : /^(?:male|m|macho|♂)$/.test(s) ? 'male' : null;
};

const sets = new Map();
/** The reference set in `dir` ({ species: { name: [entry] }, … } and the folder), read again when its index changes; null without one. */
export function referenceSet(dir) {
  if (!dir) return null;
  const path = join(dir, 'index.json');
  if (!existsSync(path)) return null;
  const stamp = statSync(path).mtimeMs;
  const hit = sets.get(dir);
  if (hit?.stamp === stamp) return hit;
  const index = JSON.parse(readFileSync(path, 'utf8'));
  const byName = new Map(Object.entries(index.species ?? {}).map(([name, list]) => [fold(name), { name, list }]));
  const genera = new Map();
  for (const { name } of byName.values()) {
    const g = fold(name.split(' ')[0]);
    (genera.get(g) ?? genera.set(g, []).get(g)).push(name);
  }
  const out = { stamp, dir: resolve(dir), index, byName, genera };
  sets.set(dir, out);
  return out;
}

/** An entry's picture on disk (only inside the set's folder). */
export function referenceFile(set, entry) {
  const path = resolve(set.dir, String(entry.file ?? ''));
  return path.startsWith(`${set.dir}/`) && existsSync(path) ? path : null;
}

/**
 * The set's photos of a species for one page: { field, specimen } (entries, those of the subspecies and
 * sex asked first), how many there are, and the species of its genus the set has when it has none.
 */
export function referencePage(set, { binomial, subspecies = null }, { sex = null, page = 0 } = {}) {
  const found = set?.byName.get(fold(binomial));
  if (!found) {
    const genus = set?.genera.get(fold(binomial.split(' ')[0])) ?? [];
    return { field: [], specimen: [], total: { field: 0, specimen: 0 }, sameGenus: genus.slice(0, 8) };
  }
  const score = e => (subspecies && fold(e.subspecies) === subspecies ? 2 : 0) + (sex && e.sex === sex ? 1 : 0) - (sex && e.sex && e.sex !== sex ? 1 : 0);
  const of = type => found.list.filter(e => e.type === type).sort((a, b) => score(b) - score(a));
  const field = of('field');
  const specimen = of('specimen');
  return {
    name: found.name,
    field: field.slice(page * PAGE.field, (page + 1) * PAGE.field),
    specimen: specimen.slice(page * PAGE.specimen, (page + 1) * PAGE.specimen),
    total: { field: field.length, specimen: specimen.length },
  };
}

/** One line of what a reference photo is. */
export function referenceLabel(entry) {
  const what = entry.type === 'field' ? 'field photo' : 'specimen photo';
  const name = entry.siteName ? `${entry.species} (${entry.siteName} there)` : [entry.species, entry.subspecies].filter(Boolean).join(' ');
  const details = [entry.sex, entry.view, entry.place].filter(Boolean).join(', ');
  return `${what}, ${SITES[entry.source] ?? entry.source}: ${name}${details ? ` (${details})` : ''}`;
}

const SHEETS = ['Collection_data', 'Insectary_data'];
/**
 * The collection's butterflies of a species with both photos in Photo_links: [{ cam, species, sex,
 * dorsal, ventral, model: [species, confidence] | null, agrees }]: those of the subspecies asked first,
 * then those the model agrees with (the surest first), those it has not read, the rest. `sex` filters by the workbook's sex
 * (else the model's, when supported); `not`: a CAM left out (the butterfly in question).
 */
export function collectionSpecimens(store, { binomial, subspecies = null }, { sex = null, not = null } = {}) {
  const index = photoIndex(store);
  const data = reviewData(store);
  const rows = store.db
    .prepare(
      `SELECT json_extract(values_json, '$.CAM_ID') cam, json_extract(values_json, '$.CAM_ID_insectary') cam2, json_extract(values_json, '$.SPECIES') species,
         json_extract(values_json, '$.Subspecies_Form') sub, json_extract(values_json, '$.Sex') sex
       FROM records WHERE sheet IN (${SHEETS.map(() => '?').join(',')}) AND missing=0 AND observed=1 AND row_num>1
         AND lower(json_extract(values_json, '$.SPECIES')) LIKE ?`,
    )
    .all(...SHEETS, `${binomial.toLowerCase()}%`);
  const skip = text(not).toUpperCase();
  const seen = new Set();
  const out = [];
  for (const r of rows) {
    const named = speciesOf(r.species);
    if (named?.binomial !== binomial) continue;
    for (const raw of [r.cam, r.cam2]) {
      const cam = text(raw).toUpperCase();
      if (!cam || cam === skip || seen.has(cam)) continue;
      const photos = index.byCam.get(cam);
      if (!photos?.dorsal?.length || !photos?.ventral?.length) continue;
      seen.add(cam);
      const prediction = data.predictions.get(cam);
      const top = prediction?.species?.[0] ?? null;
      const sub = named.subspecies ?? (text(r.sub) && !/no subspecies/i.test(r.sub) ? fold(r.sub) : null);
      const butterflySex = sexOf(r.sex) ?? (prediction?.sex?.supported ? sexOf(prediction.sex.sex) : null);
      if (sex && butterflySex !== sex) continue;
      out.push({
        cam,
        species: [binomial, sub].filter(Boolean).join(' '),
        subspecies: sub,
        sex: butterflySex,
        dorsal: photos.dorsal[0],
        ventral: photos.ventral[0],
        model: top ? [top[0], r2(top[1])] : null,
        agrees: top ? speciesOf(top[0])?.binomial === binomial : null,
      });
    }
  }
  const rank = s => (s.agrees ? 2 + s.model[1] : s.agrees === null ? 1 : 0) + (subspecies && s.subspecies === subspecies ? 3 : 0);
  return out.sort((a, b) => rank(b) - rank(a) || a.cam.localeCompare(b.cam));
}

/** One line of what a collection butterfly's picture is. */
export function collectionLabel(s) {
  const model = s.model ? (s.agrees ? `the model agrees (${s.model[1]})` : `the model says ${s.model[0]} (${s.model[1]})`) : 'not read by the model';
  return `collection ${s.cam}: ${s.species}${s.sex ? `, ${s.sex}` : ''} in the workbook; ${model}; dorsal | ventral`;
}
