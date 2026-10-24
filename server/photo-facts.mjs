// photo_facts: what is known of a butterfly from its photos beside what the workbook says, for
// curating the photo archive. Per CAM: the envelope photographed with the wings, read into fields
// (server/envelope-fields.mjs); the workbook's rows with the species, sex and CAM_ID corrections
// their notes hold; the Wings Gallery's prediction; the wing models' side and turn of each photo;
// the Photo_links files. And, across the archive, the places where these disagree, ranked by how
// sure the reading is, without those a note explains or a person already settled in Revisión.
// Read-only; it runs in the tools' workers (server/assistant-host.mjs).

import { createHash } from 'node:crypto';
import { photoIndex, reviewData, wingProblem } from './photodata.mjs';
import { binomialOf, buildLexicon, envelopeOfReadings, fold, noteCorrections } from './envelope-fields.mjs';
import { rankComparison } from './taxonomy.mjs';
import { LISTS } from './verifications.mjs';
import { latestVerdicts } from './review.mjs';
import { galleryOf } from './photo-gallery.mjs';

export const PHOTO_FACTS_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'photo_facts',
      description: [
        "What the archive's photos say about butterflies beside what the workbook says, read-only. The photo archive was read away from the app: the envelope in each photo (full-text OCR of the handwriting, right on about 9 lines in 10, so each field comes with its confidence 0–1), the AI's species and sex prediction, and the wing models' reading of each photo: which side its wings show, how many wings, and whether the photo is turned.",
        '- `cams`: per CAM (up to 25): `envelope` (each field [value, confidence]: species as the workbook writes it, sex, locality (the workbook\'s name; what was read when different), code (the insectary\'s code written on reared butterflies\' envelopes, e.g. F77, 4MT), crispr, camid (the ID written on the envelope); `dates` [kind, date, confidence], kind date (no label: on a field envelope the collection day; on the envelope of a reared butterfly (one with an insectary `code`) the day it died), death, preserved, wingClip (the day it came into the insectary), emerged; `photos` read; `differ`: fields its photos read differently), `workbook` (its Collection_data / Insectary_data rows: species, sex, locality, dates, insectaryId, and `notes`: the species, sex or CAM_ID changes their notes record, with the sentence), `gallery` (the AI\'s prediction for the butterfly\'s photos together: species and subspecies, up to 5 each as "name probability", those under 1 % left out; sex [sex, confidence, supported or why not]; andes: the side of the Andes it assumed; `photos`: the photos whose own prediction puts another species first), `wings` (each photo: the side its wings show with P(dorsal), the wings counted when not 4 and those missing, `turned`, the pair\'s verdict, the models\' review reasons), `files` (Photo_links: name, folder, link), `disagree` and `explained` (disagreements a note or a curation decision accounts for). `lines: true` adds the OCR lines of each photo.',
        '- Without `cams`: the disagreements across the archive, the surest first, paged (`offset`, `limit` up to 100): species (envelope ≠ workbook), sex (envelope ≠ workbook), gallery (the gallery ≥ 90 % sure of another species), aisex (the sex model, supported and ≥ 90 % sure, against the sex in the database, when the envelope does not already disagree), camid (the envelope shows another CAM than the file name), wings (the wings show the other side than the d/v of the name, or the photo is turned). `counts` per kind; `settled`: left out because a note, a curation decision or a verdict in Revisión accounts for them. `kind` (comma-separated) and `q` (text in the CAM, names or locality) narrow it.',
        '- Curating the archive: `summary: true` first, a short answer: how many there are by kind, by tier and by folder and year, the sections to work through. `tier`, `folder`, `year` (with `kind`, `q`) then list one section: each item with its `tier` (clear: independent sources agree against one, the fix is proposed; reason: the text evidence needs weighing; look: only the photo settles it), `why` and the `fix` proposed. `show_photo_fixes` puts a section on a card for the person without reading it here.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          cams: { type: 'array', items: { type: 'string' } },
          lines: { type: 'boolean' },
          kind: { type: 'string' },
          q: { type: 'string' },
          offset: { type: 'integer' },
          limit: { type: 'integer' },
          summary: { type: 'boolean' },
          tier: { type: 'string', description: 'clear, reason, look (comma-separated)' },
          folder: { type: 'string', description: "A photo folder (Photo_links' path, or its start)" },
          year: { type: 'string', description: 'The year the butterfly was collected or brought in, e.g. 2024' },
        },
      },
    },
  },
];
export const PHOTO_FACTS_TOOL_NAMES = new Set(PHOTO_FACTS_TOOLS.map(t => t.function.name));

/** The IDs photos are named after: CAM…, and the M series of Collection_data (M6681). */
const ID = /^(?:CAM\d{5,7}|M\d{4,5})$/;
export const KINDS = ['species', 'sex', 'gallery', 'aisex', 'camid', 'wings'];
/** How sure a reading must be to count as a disagreement. */
const SURE = { species: 0.6, sex: 0.6, gallery: 0.9, aisex: 0.9, camid: 0.9 };
const MAX_CAMS = 25;
const EPOCH = Date.UTC(1899, 11, 30);
const iso = v => (typeof v === 'number' && v > 10000 && v < 80000 ? new Date(EPOCH + Math.floor(v) * 864e5).toISOString().slice(0, 10) : v ?? null);
const text = v => (v === null || v === undefined ? '' : String(v).trim());
const none = v => /^(?:|na|n\/a|none|null|not_found|not_provided|unknown)$/i.test(text(v));
const r2 = n => (typeof n === 'number' ? Math.round(n * 100) / 100 : n);
const pair = f => (f ? [f.value, r2(f.conf)] : undefined);
const sexOf = v => {
  const s = text(v).toLowerCase().replace(/\s*\?$/, '');
  return s === 'female' || s === 'male' ? s : null;
};

// ------------------------------------------------------------ the workbook
const SHEET_FIELDS = {
  Collection_data: ['CAM_ID', 'CAM_ID_insectary', 'Insectary_ID', 'FieldMark_ID', 'Genus', 'SPECIES', 'Subspecies_Form', 'Sex', 'Collection_location', 'Collection_date', 'Death_date', 'Preservation_date', 'Notes_Collection_data'],
  Insectary_data: ['CAM_ID', 'Insectary_ID', 'SPECIES', 'Sex', 'Collection_location', 'Intro2Insectary_date', 'Death_date', 'Preservation_date', 'Notes_Insectary_data'],
};
const DATE_NAMES = { Collection_date: 'collected', Intro2Insectary_date: 'intoInsectary', Death_date: 'death', Preservation_date: 'preserved' };
const taxonomySheet = () => LISTS.Collection_data?.SPECIES?.from ?? ['Taxonomy_v18Jun25', 'species'];
const placesSheet = () => LISTS.Collection_data?.Collection_location?.from ?? ['Location_data', 'Collection_location'];

/** The sheets' state: what the caches below are kept on. */
function workbookStamp(store) {
  const sheets = ['Collection_data', 'Insectary_data', taxonomySheet()[0], placesSheet()[0]];
  return JSON.stringify(
    store.db
      .prepare(`SELECT sheet, count(*) n, max(updated_at) u, sum(version) v FROM records WHERE sheet IN (${sheets.map(() => '?').join(',')}) AND missing=0 GROUP BY sheet ORDER BY sheet`)
      .all(...sheets),
  );
}
const picked = (store, sheet, fields) => {
  const sql = `SELECT id, row_num, ${fields.map((f, i) => `json_extract(values_json, '$."${f}"') AS c${i}`).join(', ')} FROM records WHERE sheet=? AND missing=0 AND observed=1 AND row_num>1 ORDER BY row_num`;
  return store.db
    .prepare(sql)
    .all(sheet)
    .map(r => ({ id: r.id, row: r.row_num, sheet, values: Object.fromEntries(fields.map((f, i) => [f, r[`c${i}`]])) }));
};

const cache = new WeakMap();
/** The workbook's names (lexicon), its rows by CAM, and each CAM's envelope read into fields; rebuilt when the sheets or an import change. */
export function photoFactsData(store) {
  const stamp = `${workbookStamp(store)}|${reviewData(store).stamp}`;
  const hit = cache.get(store);
  if (hit?.stamp === stamp) return hit;
  const rows = Object.entries(SHEET_FIELDS).flatMap(([sheet, fields]) => picked(store, sheet, fields));
  const taxa = new Map();
  const places = new Map();
  const count = (map, name, n = 1) => {
    const key = text(name);
    if (key && !none(key)) map.set(key, (map.get(key) ?? 0) + n);
  };
  const [taxSheet, taxField] = taxonomySheet();
  for (const r of picked(store, taxSheet, [taxField])) count(taxa, r.values[taxField], 0);
  const [placeSheet, placeField] = placesSheet();
  for (const r of picked(store, placeSheet, [placeField])) count(places, r.values[placeField], 0);
  const byCam = new Map();
  for (const r of rows) {
    const species = text(r.values.SPECIES);
    const sub = text(r.values.Subspecies_Form);
    count(taxa, species && sub && !none(sub) && species.split(/\s+/).length === 2 ? `${species} ${sub}` : species);
    count(places, r.values.Collection_location);
    for (const field of ['CAM_ID', 'CAM_ID_insectary']) {
      const cam = text(r.values[field]).toUpperCase();
      if (!ID.test(cam)) continue;
      const list = byCam.get(cam) ?? byCam.set(cam, []).get(cam);
      if (!list.includes(r)) list.push(r);
    }
  }
  // The envelopes are read again only when the workbook's names or an import change (a few seconds), not
  // with every save: how often a name is used (ties between names) stays as it was then.
  const data = reviewData(store);
  const names = createHash('sha1').update([...taxa.keys()].sort().join('\n')).update('\u0000').update([...places.keys()].sort().join('\n')).digest('base64url');
  const parseStamp = `${names}|${data.stamp}`;
  let { lexicon, envelopes } = hit?.parseStamp === parseStamp ? hit : {};
  if (!envelopes) {
    lexicon = buildLexicon({
      taxa: [...taxa].map(([name, n]) => ({ name, n })),
      localities: [...places].map(([name, n]) => ({ name, n })),
    });
    envelopes = new Map();
    for (const [cam, readings] of data.readingsByCam) {
      const usable = readings.filter(r => r.text?.length || r.camid);
      if (usable.length) envelopes.set(cam, { ...envelopeOfReadings(usable, lexicon), readings: usable });
    }
  }
  const out = { stamp, parseStamp, lexicon, byCam, envelopes, corrections: hit?.parseStamp === parseStamp ? hit.corrections : new Map() };
  cache.set(store, out);
  return out;
}

/** A row as the answer shows it, with the corrections its notes record. */
function rowFacts(facts, r) {
  const v = r.values;
  const species = text(v.SPECIES);
  const sub = text(v.Subspecies_Form);
  const dates = {};
  for (const [field, name] of Object.entries(DATE_NAMES)) if (v[field] !== undefined && v[field] !== null && !none(v[field])) dates[name] = iso(v[field]);
  const notes = correctionsOf(facts, r);
  return {
    sheet: r.sheet,
    row: r.row,
    recordId: r.id,
    species: species && sub && !none(sub) && species.split(/\s+/).length === 2 ? `${species} ${sub}` : species || null,
    sex: text(v.Sex) || null,
    locality: text(v.Collection_location) || null,
    ...(Object.keys(dates).length ? { dates } : {}),
    ...(!none(v.Insectary_ID) ? { insectaryId: text(v.Insectary_ID) } : {}),
    ...(notes.length ? { notes } : {}),
  };
}
function correctionsOf(facts, r) {
  const note = r.values.Notes_Collection_data ?? r.values.Notes_Insectary_data;
  if (none(note)) return [];
  const key = `${r.id}|${note}`;
  if (!facts.corrections.has(key)) facts.corrections.set(key, noteCorrections(note, facts.lexicon, text(r.values.SPECIES).split(/\s+/)[0]));
  return facts.corrections.get(key);
}

// ------------------------------------------------------------ one CAM
const shortReasons = s => (s ? String(s).replace(/\(neighbours~[^)]*\)/g, '').slice(0, 160) : undefined);

/** What the wing models say of a CAM's photos, by their names now. */
function wingsOf(data, index, cam) {
  const list = data.wings.byCam.get(cam) ?? [];
  if (!list.length) return null;
  const out = { photos: {} };
  const problems = [];
  for (const w of list) {
    const now = (w.fileId && index.files.get(w.fileId)?.name) || w.name;
    if (w.error) {
      out.photos[now] = `not read: ${String(w.error).slice(0, 80)}`;
      continue;
    }
    const side = w.side ? `${w.side} ${r2(w.pDorsal)}${w.side === 'mixed' && w.foreSide ? ` (forewings ${w.foreSide}, hindwings ${w.hindSide})` : ''}` : 'not checked';
    const missing = w.missing?.length ? ` (no ${w.missing.map(m => m.replace('_', ' ')).join(', ')})` : '';
    out.photos[now] = `${side}${w.wings !== null && w.wings !== undefined && w.wings !== 4 ? `, ${w.wings} wings${missing}` : ''}`;
    const problem = wingProblem(w, now);
    if (problem?.side) problems.push(`${now} shows ${problem.side}`);
    if (problem?.turn) (out.turned ??= {})[now] = `${problem.turn} (turn ${problem.degrees}° clockwise to fix)`;
    if (w.review && w.reasons) (out.review ??= {})[now] = shortReasons(w.reasons);
  }
  const pairs = [...new Set(list.map(w => w.pair).filter(Boolean))];
  if (pairs.length) out.pair = pairs.join('; ');
  if (problems.length) out.otherSide = problems;
  return out;
}

/** A ranked list as one short line: "Mechanitis polymnia 0.9, Melinaea menophilus 0.06" (up to 5, those under 1 % left out). */
const ranked = list =>
  (list ?? [])
    .filter(([, p], i) => i === 0 || p >= 0.01)
    .slice(0, 5)
    .map(([name, p]) => `${name} ${r2(p)}`)
    .join(', ') || undefined;

/** The AI's prediction for a CAM, and the photos alone that give another species first. */
function galleryFacts(prediction, photos = [], index = null) {
  if (!prediction) return null;
  const sex = prediction.sex;
  const why = sex && !sex.supported ? (sex.status && sex.status !== 'species_supported' ? `not supported: ${sex.status}` : 'not supported') : 'supported';
  const first = prediction.species?.[0]?.[0];
  const others = {};
  for (const p of photos) {
    const top = p.species?.[0];
    if (top && first && top[0] !== first) others[(p.fileId && index?.files.get(p.fileId)?.name) || p.name] = `${top[0]} ${r2(top[1])}`;
  }
  return {
    species: ranked(prediction.species),
    ...(prediction.subspecies?.[0] ? { subspecies: ranked(prediction.subspecies) } : {}),
    ...(sex ? { sex: [sex.sex, r2(sex.confidence), why] } : {}),
    ...(prediction.andes ? { andes: prediction.andes } : {}),
    ...(Object.keys(others).length ? { photos: others } : {}),
  };
}

/** The envelope's fields as the answer shows them. */
function envelopeFacts(entry, withLines) {
  if (!entry) return null;
  const e = entry.envelope;
  const out = { photos: e.photos };
  if (e.species) out.species = pair(e.species);
  if (e.sex) out.sex = pair(e.sex);
  if (e.locality) out.locality = fold(e.locality.read) === fold(e.locality.value) ? pair(e.locality) : [e.locality.value, r2(e.locality.conf), e.locality.read];
  if (e.dates.length) out.dates = e.dates.map(d => [d.kind, d.value, r2(d.conf)]);
  for (const f of ['code', 'crispr', 'camid']) if (e[f]) out[f] = pair(e[f]);
  if (e.differ) out.differ = e.differ;
  if (withLines) out.lines = Object.fromEntries(entry.readings.map(r => [r.name, r.text]));
  return out;
}

/**
 * A butterfly as a card of the photo archive review shows it beside its case (server/photo-curation.mjs):
 * the envelope's fields as read, and each database row with its notes, whole (a note may say the CAM_ID or
 * the species was changed after the photo, so the envelope is out of date) and the changes they record.
 */
export function recordFacts(store, cam) {
  const facts = photoFactsData(store);
  const rows = facts.byCam.get(cam) ?? [];
  const envelope = envelopeFacts(facts.envelopes.get(cam), false);
  const database = rows.map(r => {
    const note = r.values.Notes_Collection_data ?? r.values.Notes_Insectary_data;
    return { ...rowFacts(facts, r), ...(!none(note) ? { note: text(note).slice(0, 600) } : {}) };
  });
  return { ...(envelope ? { envelope } : {}), ...(database.length ? { database } : {}) };
}

/** The disagreements of one CAM: [{ kind, … }] and those a note or a decision accounts for. */
function compare(store, facts, data, index, cam, verdicts) {
  const rows = facts.byCam.get(cam) ?? [];
  const entry = facts.envelopes.get(cam);
  const env = entry?.envelope;
  const found = [];
  const explained = [];
  const notes = rows.flatMap(r => correctionsOf(facts, r));
  const flags = data.flags.filter(f => f.cam === cam);
  const readerError = type => flags.some(f => f.type === type && f.decision === 'reader-error');
  const settled = id => ['rejected', 'applied'].includes(verdicts?.get(id)?.verdict);
  const settle = (item, why) => explained.push({ kind: item.kind, why, ...item });
  const prediction = data.predictions.get(cam);

  // Species: the envelope's against each row's (genus only when either has no species).
  if (env?.species && env.species.conf >= SURE.species) {
    const read = env.species;
    for (const r of rows) {
      const recorded = text(r.values.SPECIES);
      if (!recorded || none(recorded)) continue;
      const [rg, rs] = recorded.split(/\s+/);
      const binomial = !rs || /^sp\.?$/i.test(rs) || !read.species ? fold(read.genus) === fold(rg) : binomialOf(read.value) === binomialOf(recorded);
      if (binomial) continue;
      const item = { kind: 'species', cam, envelope: read.value, workbook: recorded, conf: r2(read.conf), seen: read.seen, sheet: r.sheet, row: r.row, recordId: r.id };
      const note = notes.find(n => n.field === 'species' && (n.checked || (n.from && binomialOf(n.from) === binomialOf(read.value))));
      if (note) settle(item, note.checked ? `note: ${note.text}` : `note: changed from ${note.from}${note.to ? ` to ${note.to}` : ''}`);
      else if (readerError('species')) settle(item, 'curation: a reading error');
      else if (settled(`envelope_species:${cam}:${r.sheet}`)) settle(item, 'Revisión: settled');
      else found.push(item);
      break;
    }
  }
  // Sex: the symbol against each row's sex.
  if (env?.sex && env.sex.conf >= SURE.sex && !env.sex.both) {
    for (const r of rows) {
      const recorded = sexOf(r.values.Sex);
      if (!recorded || recorded === env.sex.value) continue;
      const item = { kind: 'sex', cam, envelope: env.sex.value, workbook: text(r.values.Sex), conf: r2(env.sex.conf), seen: env.sex.seen, sheet: r.sheet, row: r.row, recordId: r.id };
      const note = notes.find(n => n.field === 'sex' && (n.from === env.sex.value || (!n.from && n.to === recorded)));
      if (note) settle(item, `note: ${note.text}`);
      else if (readerError('sex')) settle(item, 'curation: a reading error');
      else if (settled(`envelope_sex:${cam}:${r.sheet}`)) settle(item, 'Revisión: settled');
      else found.push(item);
      break;
    }
  }
  // The AI's sex, supported and sure, against the row's, when the envelope's sex is not already against it.
  const sexAi = prediction?.sex;
  if (sexAi?.supported && (sexAi.confidence ?? 0) >= SURE.aisex && ![...found, ...explained].some(f => f.kind === 'sex')) {
    for (const r of rows) {
      const recorded = sexOf(r.values.Sex);
      if (!recorded || recorded === sexAi.sex) continue;
      const item = {
        kind: 'aisex',
        cam,
        gallery: sexAi.sex,
        workbook: text(r.values.Sex),
        conf: r2(sexAi.confidence),
        ...(env?.sex && env.sex.conf >= SURE.sex ? { envelope: env.sex.value } : {}),
        sheet: r.sheet,
        row: r.row,
        recordId: r.id,
      };
      const note = notes.find(n => n.field === 'sex' && (n.from === sexAi.sex || (!n.from && n.to === recorded)));
      if (note) settle(item, `note: ${note.text}`);
      else if (settled(`ai_sex:${cam}`)) settle(item, 'Revisión: settled');
      else found.push(item);
      break;
    }
  }
  // The gallery, sure of another species than the row's.
  if (prediction && rows[0]) {
    const r = rows[0];
    const compared = rankComparison(r.values, prediction, 'species');
    if (compared.status === 'disagreement' && (compared.confidence ?? 0) >= SURE.gallery) {
      const item = {
        kind: 'gallery',
        cam,
        gallery: compared.predicted,
        workbook: compared.recorded,
        conf: r2(compared.confidence),
        ...(env?.species ? { envelope: env.species.value } : {}),
        sheet: r.sheet,
        row: r.row,
        recordId: r.id,
      };
      const note = notes.find(n => n.field === 'species' && n.checked);
      if (note) settle(item, `note: ${note.text}`);
      else if (settled(`ai_species:${cam}`)) settle(item, 'Revisión: settled');
      else found.push(item);
    }
  }
  // The CAM on the envelope against the file name.
  if (entry && ID.test(cam)) {
    const other = entry.readings.filter(r => r.camid && r.camid.toUpperCase() !== cam && (r.conf ?? 0) >= SURE.camid);
    if (other.length) {
      const read = other[0].camid.toUpperCase();
      // The file's own ID also read on the envelope (another line of the full text, or the reader's second
      // guess: an 8 read as 6 on "CAMO78…"): more likely a misreading, ranked lower.
      const own = cam.replace(/O/g, '0');
      const alsoRead = entry.readings.some(
        r => r.text.some(([t]) => String(t).toUpperCase().replace(/\s+/g, '').replace(/O/g, '0').includes(own)) || (r.candidates ?? []).some(([id]) => String(id).toUpperCase() === cam),
      );
      const sure = Math.max(...other.map(r => r.conf));
      const item = {
        kind: 'camid',
        cam,
        envelope: read,
        files: other.map(r => r.name),
        conf: r2(alsoRead ? sure / 2 : sure),
        seen: other.length,
        of: entry.readings.length,
        ...(alsoRead ? { fileIdAlsoRead: true } : {}),
      };
      const note = notes.find(n => n.field === 'camid' && n.from === read);
      const decided = flags.find(f => f.type === 'envelope-not-file' && f.decision);
      if (note) settle(item, `note: ${note.text}`);
      else if (decided?.decision === 'reader-error') settle(item, 'curation: a reading error');
      else if (settled(`photo_camid:${cam}`)) settle(item, 'Revisión: settled');
      else found.push({ ...item, ...(decided ? { curation: decided.decision } : {}) });
    }
  }
  // The wings: the other side than the name says, or turned.
  const wings = data.wings.byCam.get(cam) ?? [];
  const problems = [];
  let conf = 0;
  const sideSettled = settled(`photo_side:${cam}`);
  for (const w of wings) {
    const now = (w.fileId && index.files.get(w.fileId)?.name) || w.name;
    const p = wingProblem(w, now);
    if (!p) continue;
    if (p.side && !sideSettled) {
      problems.push(`${now} shows ${p.side} (P dorsal ${r2(w.pDorsal)})`);
      conf = Math.max(conf, Math.abs(w.pDorsal - 0.5) * 2);
    }
    if (p.turn && !settled(`photo_turn:${String(now).replace(/\.[^.]+$/, '').toUpperCase()}`)) {
      problems.push(`${now} ${p.turn}`);
      conf = Math.max(conf, 0.9);
    }
  }
  if (problems.length) {
    const pairs = [...new Set(wings.map(w => w.pair).filter(Boolean))];
    found.push({ kind: 'wings', cam, wings: problems.join('; '), ...(pairs.length ? { pair: pairs.join('; ') } : {}), conf: r2(conf) });
  }
  return { found, explained };
}

/** The facts of some CAMs. */
export function camFacts(store, cams, { lines = false } = {}) {
  const facts = photoFactsData(store);
  const data = reviewData(store);
  const index = photoIndex(store);
  const verdicts = latestVerdicts(store.db);
  const wanted = [...new Set(cams.map(c => text(c).toUpperCase().replace(/\s+/g, '')).filter(Boolean))].slice(0, MAX_CAMS);
  const files = new Map(galleryOf(store, wanted).map(g => [g.id, g.photos]));
  return wanted.map(cam => {
    const rows = facts.byCam.get(cam) ?? [];
    const entry = facts.envelopes.get(cam);
    const { found, explained } = compare(store, facts, data, index, cam, verdicts);
    const out = {
      cam,
      envelope: envelopeFacts(entry, lines) ?? 'not read (not in the archive scan)',
      workbook: rows.length ? rows.map(r => rowFacts(facts, r)) : 'no row with this CAM_ID',
    };
    const gallery = galleryFacts(data.predictions.get(cam), data.filePredictions?.get(cam), index);
    if (gallery) out.gallery = gallery;
    const wings = wingsOf(data, index, cam);
    if (wings) out.wings = wings;
    const photos = files.get(cam) ?? [];
    out.files = photos.length ? photos.map(p => ({ name: p.name, folder: p.folder, url: p.url })) : 'none in Photo_links';
    if (found.length) out.disagree = found.map(({ cam: _c, recordId: _r, ...f }) => f);
    if (explained.length) out.explained = explained.map(e => `${e.kind}: ${e.why}`);
    return out;
  });
}

const listCache = new WeakMap();
/** Every disagreement of the archive (and those settled), the surest first; kept until the sheets, an import or a verdict change. */
export function allDisagreements(store) {
  const facts = photoFactsData(store);
  const data = reviewData(store);
  const index = photoIndex(store);
  const verdicts = latestVerdicts(store.db);
  const verdictStamp = store.db.prepare('SELECT count(*) n, max(id) m FROM issue_verdicts').get();
  const stamp = `${facts.stamp}|${index.stamp}|${verdictStamp.n}:${verdictStamp.m}`;
  const hit = listCache.get(store);
  if (hit?.stamp === stamp) return hit;
  const cams = new Set([...facts.envelopes.keys(), ...data.predictions.keys(), ...data.wings.byCam.keys()]);
  const items = [];
  const settled = [];
  for (const cam of cams) {
    const { found, explained } = compare(store, facts, data, index, cam, verdicts);
    items.push(...found);
    settled.push(...explained);
  }
  items.sort((a, b) => (b.conf ?? 0) - (a.conf ?? 0) || (b.seen ?? 1) - (a.seen ?? 1) || a.cam.localeCompare(b.cam));
  const out = { stamp, items, settled };
  listCache.set(store, out);
  return out;
}

/** The disagreements across the archive: counts per kind and one page. */
export function disagreementPage(store, { kind, q, offset = 0, limit = 25 } = {}) {
  const kinds = text(kind)
    .split(',')
    .map(k => k.trim())
    .filter(Boolean);
  const unknown = kinds.find(k => !KINDS.includes(k));
  if (unknown) return { error: `kind is one or more of ${KINDS.join(', ')}` };
  const needle = fold(q).trim();
  const { items, settled } = allDisagreements(store);
  const matches = i => !needle || fold(Object.values(i).join(' ')).includes(needle);
  const counts = Object.fromEntries(KINDS.map(k => [k, 0]));
  for (const i of items) if (matches(i)) counts[i.kind]++;
  const settledCounts = {};
  for (const s of settled) if (matches(s)) settledCounts[s.kind] = (settledCounts[s.kind] ?? 0) + 1;
  const chosen = items.filter(i => (!kinds.length || kinds.includes(i.kind)) && matches(i));
  const size = Math.min(Math.max(Number(limit) || 25, 1), 100);
  const start = Math.max(Number(offset) || 0, 0);
  const page = chosen.slice(start, start + size).map(({ recordId: _r, ...i }) => i);
  return {
    counts,
    settled: settledCounts,
    total: chosen.length,
    offset: start,
    items: page,
    ...(start + page.length < chosen.length ? { next: `offset ${start + page.length}` } : {}),
  };
}

/** The tool. */
export function runPhotoFacts(store, args = {}, context = null) {
  const cams = Array.isArray(args.cams) ? args.cams : typeof args.cams === 'string' ? args.cams.split(/[\s,]+/) : [];
  if (cams.length) {
    const out = camFacts(store, cams, { lines: !!args.lines });
    if (context?.sources)
      for (const c of out)
        for (const r of Array.isArray(c.workbook) ? c.workbook : []) context.sources.set(r.recordId, { id: r.recordId, type: 'record', sheet: r.sheet, row: r.row, label: c.cam });
    return { cams: out, ...(cams.length > MAX_CAMS ? { left: `${cams.length - MAX_CAMS} CAMs not answered: ask for them in another call` } : {}) };
  }
  return disagreementPage(store, args);
}
