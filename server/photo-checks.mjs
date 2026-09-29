// The checks that come from the photos (server/photodata.mjs): what the envelope
// photographed with the wings says against the file name and the sheet, photos
// of another butterfly in a CAM folder, preserved butterflies without photos,
// and the Wings Gallery's species prediction against the recorded species.
// The curation's decisions are kept: flags already found to be reading errors
// are not raised again, and its strength (how often such a flag was real in the
// blind review) goes with each issue.

import { photoContext, photoIndex, reviewData } from './photodata.mjs';
import { canonicalTaxon, rankComparison } from './taxonomy.mjs';
import { listOptions } from './verify.mjs';
import { blankOrNA, isIdValue } from './verifications.mjs';

/** Sheet changes are proposed; these are done by hand in Drive (renames, merges). */
export const TASK_KINDS = new Set(['photo_camid', 'photo_extra']);
/** Kinds read from the photos by a model: a person's verdict on them is a training label. */
export const MODEL_KINDS = new Set(['photo_camid', 'photo_extra', 'envelope_sex', 'envelope_species', 'ai_species']);

const text = value => (value === null || value === undefined ? '' : String(value).trim());
const binomial = value => text(value).toLowerCase().split(/\s+/).slice(0, 2).join(' ');
const sexOf = value => {
  const s = text(value)
    .toLowerCase()
    .replace(/[_\s]*\?$/, '')
    .trim();
  return s === 'female' || s === 'male' ? s : null;
};
const SEX_WORD = { male: '♂ macho', female: '♀ hembra' };
/** How often a flag of this stratum was real in the blind sample (strata-summary.csv). */
const STRATUM_STRENGTH = {
  'envelope-not-file:all-photos': 'fuerte',
  'envelope-not-file:high': 'media',
  'envelope-not-file:low': 'baja',
  'sex:both-photos': 'fuerte',
  'sex:one-photo': 'media',
  'species:both-photos': 'fuerte',
  'species:one-photo': 'media',
};
const strengthOf = flag =>
  flag.decision === 'unclear' ? 'dudosa' : flag.strength || STRATUM_STRENGTH[flag.stratum] || 'media';
const stemOf = file => String(file).split('/').pop().split('__')[0];
/** "Episcada sulphurea Episcada sulphurea" (both photos read) → "Episcada sulphurea". */
function once(name) {
  const words = text(name).split(/\s+/).filter(Boolean);
  const half = words.length / 2;
  if (
    Number.isInteger(half) &&
    half &&
    words.slice(0, half).join(' ').toLowerCase() === words.slice(half).join(' ').toLowerCase()
  )
    return words.slice(0, half).join(' ');
  return words.join(' ');
}
const capitalized = name => {
  const t = once(name);
  return t ? t[0].toUpperCase() + t.slice(1) : t;
};

/**
 * @param store the app's store
 * @param sheets Map sheet → loaded rows (checks.mjs load())
 * @param add (kind, row|null, field, problem, extra) as in checks.mjs; row null for issues without a sheet row
 * @param ref (row, field) → the reference of a related row
 * @param today serial day
 */
export function photoIssues(store, { sheets, add, ref, today }) {
  const data = reviewData(store);
  const index = photoIndex(store);
  const byCam = new Map();
  for (const sheet of ['Collection_data', 'Insectary_data'])
    for (const row of sheets.get(sheet) ?? []) {
      if (!row.observed) continue;
      const cam = text(row.values.CAM_ID).toUpperCase();
      if (isIdValue(cam)) (byCam.get(cam) ?? byCam.set(cam, []).get(cam)).push(row);
    }
  const rowOf = cam => byCam.get(cam)?.[0] ?? null;
  const context = (cam, prefer) => photoContext(data, index, cam, prefer) ?? {};
  const speciesOf = cam => {
    const row = rowOf(cam);
    return row ? text(row.values.SPECIES) || 'sin especie' : 'sin fila en la hoja';
  };
  const decided = f => (f.decidedBy ? { decidedBy: f.decidedBy } : {});
  const curation = f => ({
    curation: { type: f.type, stratum: f.stratum, decision: f.decision || null, note: f.action || null, ...decided(f) },
  });

  // ---- Photos filed under one CAM whose envelope shows another (a rename in Drive, not a sheet change).
  const extra = new Map();
  for (const f of data.flags.filter(f => f.type === 'envelope-not-file')) {
    if (f.decision === 'reader-error') continue;
    const target = text(f.target) || text(f.data.read?.[0]) || text(/show (CAM\d+)/.exec(f.action ?? '')?.[1]);
    if (!target) continue;
    const names = f.photos.map(stemOf);
    const row = rowOf(f.cam);
    if (f.decision === 'extra-photos-of-other-specimen') {
      extra.set(`${f.cam}>${target}`, { flag: f, target, names });
      continue;
    }
    const ctx = context(f.cam, names);
    const other = rowOf(target);
    add(
      'photo_camid',
      row,
      'CAM_ID',
      `Las fotos ${names.join(', ')} están guardadas como ${f.cam} (${speciesOf(f.cam)}), pero el sobre dice ${target} (${speciesOf(target)})`,
      {
        id: `photo_camid:${f.cam}`,
        cam: f.cam,
        label: f.cam,
        ...(row ? {} : { sheet: 'Photo_links', value: f.cam }),
        ...ctx,
        envelopeCamid: target,
        strength: f.decision === 'file-name-wrong' ? 'fuerte' : strengthOf(f),
        ...curation(f),
        task: {
          type: 'rename',
          from: f.cam,
          to: target,
          files: names,
          text: `Renombrar en Drive ${names.join(', ')} de ${f.cam} a ${target}${/after/.test(f.action ?? '') ? ' (después de mover las fotos que hoy ocupan ese nombre)' : ''}`,
        },
        ...(other ? { related: [ref(other, 'CAM_ID')] } : {}),
        relatedPhotos: { cam: target, ...(context(target).photos ?? {}) },
      },
    );
  }
  // The other side of the same finding: photos filed under X show Y, which has its own photos.
  for (const f of data.flags.filter(f => f.type === 'duplicate' && f.decision === 'file-name-wrong')) {
    const m = /filed under (CAM\d+) show (CAM\d+)/.exec(f.action ?? '');
    if (!m || extra.has(`${m[1]}>${m[2]}`)) continue;
    extra.set(`${m[1]}>${m[2]}`, {
      flag: { ...f, cam: m[1] },
      target: m[2],
      names: f.photos.map(stemOf).filter(n => n.toUpperCase().startsWith(m[1])),
    });
  }
  for (const { flag: f, target, names } of extra.values()) {
    const row = rowOf(f.cam);
    const other = rowOf(target);
    add(
      'photo_extra',
      row,
      'CAM_ID',
      `Las fotos guardadas como ${f.cam} (${names.join(', ')}) muestran ${target}, que ya tiene sus propias fotos`,
      {
        id: `photo_extra:${f.cam}`,
        cam: f.cam,
        label: f.cam,
        ...(row ? {} : { sheet: 'Photo_links', value: f.cam }),
        ...context(f.cam, names),
        envelopeCamid: target,
        strength: 'fuerte',
        ...curation(f),
        task: {
          type: 'merge',
          from: f.cam,
          to: target,
          files: names,
          text: `Unir o borrar en Drive ${names.join(', ')}: son fotos de ${target}, que ya tiene las suyas; ${f.cam} puede no tener fotos propias`,
        },
        ...(other ? { related: [ref(other, 'CAM_ID')] } : {}),
        relatedPhotos: { cam: target, ...(context(target).photos ?? {}) },
      },
    );
  }

  // ---- The sex symbol on the envelope against the sheet.
  for (const f of data.flags.filter(f => f.type === 'sex')) {
    if (f.decision === 'reader-error') continue;
    const read = sexOf(f.data.envelope);
    if (!read) continue;
    const strength = strengthOf(f);
    for (const row of byCam.get(f.cam) ?? []) {
      const recorded = sexOf(row.values.Sex);
      if (!recorded || recorded === read) continue;
      const fixable = strength === 'fuerte' && f.decision !== 'unclear' && !row.formulas.Sex;
      add(
        'envelope_sex',
        row,
        'Sex',
        `El sobre de ${f.cam} dice ${SEX_WORD[read]}; la hoja dice ${text(row.values.Sex)}`,
        {
          id: `envelope_sex:${f.cam}:${row.sheet}`,
          cam: f.cam,
          ...context(f.cam, f.photos.map(stemOf)),
          ocr: { field: 'Sex', read, lines: f.data.lines ?? [], sheet: text(row.values.Sex) },
          strength,
          ...curation(f),
          ...(fixable ? { fix: { recordId: row.id, values: { Sex: read } }, fixNote: 'sexo del sobre' } : {}),
        },
      );
    }
  }

  // ---- The species on the envelope against the sheet, grouped by batch (many are a whole day's envelopes).
  const lists = {
    Collection_data: listOptions(store, 'Collection_data'),
    Insectary_data: listOptions(store, 'Insectary_data'),
  };
  const species = [];
  for (const f of data.flags.filter(f => f.type === 'species')) {
    if (f.decision === 'reader-error') continue;
    const quoted = /envelope "([^"]+)"/.exec(f.action ?? '')?.[1];
    const read = capitalized(quoted || f.data.envelope?.[0] || '');
    if (!read) continue;
    for (const row of byCam.get(f.cam) ?? []) {
      const recorded = text(row.values.SPECIES);
      if (!recorded || blankOrNA(recorded)) continue;
      if (binomial(recorded) === binomial(read) || recorded.toLowerCase().startsWith(read.toLowerCase())) continue;
      species.push({ f, row, read, recorded });
    }
  }
  const batchKey = s => `${binomial(s.recorded)} → ${binomial(s.read)}`;
  const batches = new Map();
  for (const s of species) batches.set(batchKey(s), (batches.get(batchKey(s)) ?? 0) + 1);
  for (const s of species) {
    const { f, row, read, recorded } = s;
    const options = lists[row.sheet]?.SPECIES;
    const choices = options ? [...options.values].filter(v => binomial(v) === binomial(read)).slice(0, 8) : [];
    const exact =
      choices.find(v => v.toLowerCase() === read.toLowerCase()) ?? (choices.length === 1 ? choices[0] : null);
    const strength = strengthOf(f);
    const key = batchKey(s);
    const size = batches.get(key);
    add('envelope_species', row, 'SPECIES', `El sobre de ${f.cam} dice ${read}; la hoja dice ${recorded}`, {
      id: `envelope_species:${f.cam}:${row.sheet}`,
      cam: f.cam,
      ...context(f.cam, f.photos.map(stemOf)),
      ocr: { field: 'SPECIES', read, lines: f.data.lines ?? [], sheet: recorded },
      strength,
      ...curation(f),
      group: {
        key: `envelope_species:${key}`,
        label: `Hoja ${capitalized(binomial(recorded))} → sobre ${capitalized(binomial(read))}`,
        size,
      },
      ...(choices.length ? { choices } : {}),
      ...(exact &&
      strength !== 'dudosa' &&
      f.decision !== 'unclear' &&
      (!row.formulas.SPECIES || row.sheet === 'Insectary_data')
        ? { fix: { recordId: row.id, values: { SPECIES: exact } }, fixNote: 'especie del sobre' }
        : {}),
    });
  }

  // ---- Preserved butterflies without photos (only once Photo_links is in the app's copy; recent ones are photographed later).
  if (index.linked) {
    const preserved = [
      ['Collection_data', row => text(row.values.Release_Collect) === 'Collected_Preserved', 'Collection_date'],
      ['Insectary_data', row => typeof row.values.Preservation_date === 'number', 'Preservation_date'],
    ];
    const seen = new Set();
    for (const [sheet, isPreserved, dateField] of preserved)
      for (const row of sheets.get(sheet) ?? []) {
        if (!row.observed || !isPreserved(row)) continue;
        const cam = text(row.values.CAM_ID).toUpperCase();
        if (!isIdValue(cam) || !/^CAM\d+$/.test(cam) || seen.has(cam)) continue;
        seen.add(cam);
        const day = row.values[dateField];
        if (typeof day === 'number' && day > today - 30) continue;
        const entry = index.byCam.get(cam);
        const missing = ['dorsal', 'ventral'].filter(v => !entry?.[v]?.length);
        const notFound = ['Photo_dorsal', 'Photo_ventral'].filter(k => /not\s*found/i.test(text(row.values[k])));
        if (!missing.length && !notFound.length) continue;
        const field = missing.length ? `Photo_${missing[0]}` : notFound[0];
        add(
          'photo_missing',
          row,
          field,
          missing.length
            ? `${cam} preservada sin foto ${missing.join(' ni ')} en Photo_links`
            : `${cam}: Photo_links tiene sus fotos, pero ${notFound.join(' y ')} dice Not Found (¿nombre de archivo distinto?)`,
          { cam, ...context(cam), strength: missing.length === 2 ? 'fuerte' : 'media' },
        );
      }
  }

  // ---- The Wings Gallery's species against the recorded one (same rule as the gallery's "differs from recorded").
  // The sheet's species list (~10,000 names) indexed once by genus + species: looking them up
  // for every prediction made this scan take 30 s on the server.
  const speciesIndexes = new Map();
  const speciesIndex = sheet => {
    const options = lists[sheet]?.SPECIES;
    if (!options) return null;
    if (!speciesIndexes.has(sheet)) {
      const index = new Map();
      for (const v of options.values) {
        const key = binomial(v);
        (index.get(key) || index.set(key, []).get(key)).push(v);
      }
      speciesIndexes.set(sheet, index);
    }
    return speciesIndexes.get(sheet);
  };
  for (const [cam, prediction] of data.predictions) {
    const row = rowOf(cam);
    if (!row) continue;
    const compared = rankComparison(row.values, prediction, 'species');
    if (compared.status !== 'disagreement') continue;
    const confidence = compared.confidence ?? 0;
    const byBinomial = speciesIndex(row.sheet);
    const choices = prediction.species
      .slice(0, 3)
      .flatMap(([name]) => (byBinomial ? (byBinomial.get(binomial(canonicalTaxon(name))) ?? []).slice(0, 3) : [name]));
    add(
      'ai_species',
      row,
      'SPECIES',
      `La IA de la galería ve ${compared.predicted} (${Math.round(confidence * 100)} %) en las fotos de ${cam}; la hoja dice ${text(row.values.SPECIES)}`,
      {
        id: `ai_species:${cam}`,
        cam,
        ...context(cam),
        ai: { recorded: compared.recorded, predicted: compared.predicted, confidence },
        strength: confidence >= 0.9 ? 'fuerte' : confidence >= 0.6 ? 'media' : 'baja',
        ...(choices.length ? { choices: [...new Set(choices)].slice(0, 8) } : {}),
      },
    );
  }
}
