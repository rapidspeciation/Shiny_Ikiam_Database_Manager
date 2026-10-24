// The photo archive's disagreements with the database (server/photo-facts.mjs) sorted for curating,
// so the AI reads a summary and a person reviews one section at a time:
// - each disagreement with its evidence beside it (the database, the envelope read in the photos, the
//   models' species and sex, the wings' side and turn), the fix it calls for, and a tier: `clear` when
//   independent sources agree against one (the fix is proposed for review), `reason` when the
//   text evidence needs weighing, `look` when only the photo settles it;
// - where it is (the photos' folder, the year the butterfly was collected or brought in), for sections;
// - a card of them (show_photo_fixes, kind photo_curation): the sure ones come ticked; the person ticks
//   or unticks each fix, or marks a reading wrong (the database keeps its value). «Aplicar» writes the
//   database fixes (Historial), records each verdict (a wrong reading is not raised again and teaches the
//   models, as Revisión's do) and starts the Drive renames and turns as a plan of organize_photos, whose
//   own card follows them; a case whose Drive change was not made comes back to decide, saying why.

import { createHash } from 'node:crypto';
import { applyBatch } from './batch.mjs';
import { binomialOf } from './envelope-fields.mjs';
import { KINDS, allDisagreements, photoFactsData, recordFacts, runPhotoFacts } from './photo-facts.mjs';
import { driveIdOf } from './photo-links.mjs';
import { photoContext, photoIndex, reviewData, wingProblem } from './photodata.mjs';
import { initVerdicts, latestVerdicts } from './review.mjs';
import { listOptions } from './verify.mjs';

export const TIERS = ['clear', 'reason', 'look'];
/** The kinds of a card's entries: photo_facts' kinds, its wings split into a photo's side and its turn. */
export const ENTRY_KINDS = ['species', 'sex', 'gallery', 'aisex', 'camid', 'side', 'turn'];
/** Revisión's kind of each entry's verdict (server/checks.mjs, server/photo-checks.mjs). */
const VERDICT_KIND = {
  species: 'envelope_species',
  sex: 'envelope_sex',
  gallery: 'ai_species',
  aisex: 'ai_sex',
  camid: 'photo_camid',
  side: 'photo_side',
  turn: 'photo_turn',
};
/** Entries a card shows at most, and a section's list for the AI. */
const CARD_MAX = 150;
const LIST_MAX = 100;
/** How sure a model must be to count as agreeing with one side. */
const MODEL_SURE = 0.6;
/** An envelope reading sure enough to write from. */
const ENVELOPE_SURE = 0.8;
const ID_SURE = 0.95;

const EPOCH = Date.UTC(1899, 11, 30);
const text = v => (v === null || v === undefined ? '' : String(v).trim());
const r2 = n => (typeof n === 'number' ? Math.round(n * 100) / 100 : n);
const yearOf = v => (typeof v === 'number' && v > 10000 && v < 80000 ? new Date(EPOCH + Math.floor(v) * 864e5).getUTCFullYear() : null);
const sameSpecies = (a, b) => !!a && !!b && binomialOf(a) === binomialOf(b);
const stem = name => String(name ?? '').replace(/\.[^.]+$/, '');
const filled = o => Object.fromEntries(Object.entries(o).filter(([, v]) => v !== undefined && v !== null && !(Array.isArray(v) && !v.length)));
const camidLineScore = line => {
  const value = String(line?.[0] ?? '').replace(/\s+/g, '');
  const prefix = /^C[A4][MN]?/i.exec(value)?.[0];
  if (!prefix) return 0;
  return (prefix.toUpperCase().startsWith('CAM') ? 30 : 10) + Math.min(6, [...value.slice(prefix.length)].filter(c => /[0-9ODQUILZSGBTBAeilozsbgq]/.test(c)).length);
};

// ------------------------------------------------------------ where each photo is
const folderCache = new WeakMap();
/** Drive ID → the folder Photo_links lists it in. */
function foldersOf(store) {
  const index = photoIndex(store);
  const hit = folderCache.get(store);
  if (hit?.stamp === index.stamp) return hit.folders;
  const folders = new Map();
  for (const r of store.db.prepare("SELECT values_json FROM records WHERE sheet='Photo_links' AND missing=0 AND row_num>1").all()) {
    const v = JSON.parse(r.values_json || '{}');
    const id = driveIdOf(v.URL);
    if (id && v.Type !== 'Folder') folders.set(id, String(v['Full Path'] ?? '').split('/').slice(0, -1).join('/'));
  }
  folderCache.set(store, { stamp: index.stamp, folders });
  return folders;
}

// ------------------------------------------------------------ the entries
/**
 * Every disagreement as entries to curate, the surest first:
 * { id, kind, cam, tier, why: [codes], conf, evidence, fix, choices?, folder, year, sheet?, recordId? }.
 * `fix`: { field, from, to } (a database cell, `recordId`'s) or { drive: [operations of organize_photos] };
 * null when nothing is proposed (a person's look decides). Kept until the data change.
 */
const entryCache = new WeakMap();
export function curationEntries(store) {
  const list = allDisagreements(store);
  const hit = entryCache.get(store);
  if (hit?.list === list) return hit.entries;
  const facts = photoFactsData(store);
  const data = reviewData(store);
  const index = photoIndex(store);
  const folders = foldersOf(store);
  const options = { Collection_data: null, Insectary_data: null };
  const speciesList = sheet => (options[sheet] ??= listOptions(store, sheet)?.SPECIES?.values ?? null);
  const byCam = new Map();
  for (const item of list.items) (byCam.get(item.cam) ?? byCam.set(item.cam, []).get(item.cam)).push(item);

  const entries = [];
  for (const [cam, items] of byCam) {
    const rows = facts.byCam.get(cam) ?? [];
    const row = rows[0];
    const env = facts.envelopes.get(cam)?.envelope;
    const prediction = data.predictions.get(cam);
    const top = prediction?.species?.[0];
    const model = top && top[1] >= MODEL_SURE ? top[0] : null;
    const sexAi = prediction?.sex?.supported ? prediction.sex : null;
    const photos = index.byCam.get(cam);
    const firstFile = photos ? [...photos.dorsal, ...photos.ventral, ...photos.other][0] : null;
    const where = {
      folder: (firstFile && folders.get(firstFile)) || null,
      year: yearOf(row?.values.Collection_date) ?? yearOf(row?.values.Intro2Insectary_date),
    };
    const record = id => (id ? store.getRecord(id) : null);
    /** A database cell the fix may write: a formula only where people type over it (Insectary_data SPECIES). */
    const writable = (r, field) => r && !(r.formulas?.[field] && !(r.sheet === 'Insectary_data' && field === 'SPECIES'));
    /** The species as the database's list writes it, or null. */
    const listed = (sheet, name) => {
      const values = speciesList(sheet);
      if (!values) return text(name).split(/\s+/).slice(0, 2).join(' ') || null;
      const choices = [...values].filter(v => sameSpecies(v, name));
      const folded = v => binomialOf(v) && text(v).toLowerCase();
      return choices.find(v => folded(v) === text(name).toLowerCase()) ?? choices.find(v => binomialOf(v) === folded(v)) ?? (choices.length === 1 ? choices[0] : null);
    };
    const speciesFix = (item, to, why) => {
      const r = record(item.recordId);
      const value = to && listed(item.sheet, to);
      if (!writable(r, 'SPECIES')) return { fix: null, why: [...why, 'formula'] };
      if (!value) return { fix: null, why: [...why, 'notListed'] };
      return { fix: { field: 'SPECIES', from: text(r.values.SPECIES), to: value }, why };
    };
    const sexFix = (item, to, why) => {
      const r = record(item.recordId);
      if (!writable(r, 'Sex')) return { fix: null, why: [...why, 'formula'] };
      return { fix: { field: 'Sex', from: text(r.values.Sex), to }, why };
    };
    const add = (item, kind, tier, { fix = null, why = [], evidence = {}, conf = item.conf, choices, key = cam } = {}) =>
      entries.push(
        filled({
          id: `${VERDICT_KIND[kind]}:${key}${item.sheet && ['species', 'sex'].includes(kind) ? `:${item.sheet}` : ''}`,
          kind,
          cam,
          tier: fix || tier === 'look' ? tier : tier === 'clear' ? 'reason' : tier,
          why,
          conf: r2(conf),
          evidence: filled(evidence),
          fix,
          choices,
          sheet: item.sheet,
          row: item.row,
          recordId: item.recordId,
          ...where,
        }),
      );
    const kinds = new Set(items.map(i => i.kind));
    for (const item of items) {
      if (item.kind === 'species') {
        // The envelope against the database: the model sides with one, or with neither.
        const evidence = { database: item.workbook, envelope: [item.envelope, item.conf, item.seen], model: top ? [top[0], r2(top[1])] : undefined };
        if (sameSpecies(model, item.envelope) && item.conf >= ENVELOPE_SURE) add(item, 'species', 'clear', { ...speciesFix(item, item.envelope, ['agree']), evidence });
        else if (sameSpecies(model, item.workbook)) add(item, 'species', 'look', { ...speciesFix(item, item.envelope, ['modelForDatabase']), evidence });
        else add(item, 'species', 'reason', { ...speciesFix(item, item.envelope, [model ? 'modelOther' : 'modelUnsure']), evidence });
      } else if (item.kind === 'gallery') {
        // The model sure of another species: when the envelope says the same, the species entry has it.
        if (kinds.has('species')) continue;
        const evidence = { database: item.workbook, model: [item.gallery, item.conf], envelope: env?.species ? [env.species.value, r2(env.species.conf)] : undefined };
        const why = env?.species && sameSpecies(env.species.value, item.workbook) ? ['envelopeForDatabase'] : ['modelAlone'];
        const choices = prediction.species.slice(0, 3).map(([name]) => listed(item.sheet, name)).filter(Boolean);
        add(item, 'gallery', 'look', { ...speciesFix(item, item.gallery, why), evidence, choices: [...new Set(choices)] });
      } else if (item.kind === 'sex') {
        const evidence = { database: item.workbook, envelope: [item.envelope, item.conf, item.seen], model: sexAi ? [sexAi.sex, r2(sexAi.confidence)] : undefined };
        if (sexAi?.sex === item.envelope && item.conf >= ENVELOPE_SURE) add(item, 'sex', 'clear', { ...sexFix(item, item.envelope, ['agree']), evidence });
        else if (sexAi && sexAi.sex === text(item.workbook).toLowerCase().replace(/\s*\?$/, '')) add(item, 'sex', 'look', { ...sexFix(item, item.envelope, ['modelForDatabase']), evidence });
        else if (!sexAi && (item.seen ?? 1) >= 2 && item.conf >= 0.9) add(item, 'sex', 'clear', { ...sexFix(item, item.envelope, ['envelopeTwice']), evidence });
        else add(item, 'sex', 'reason', { ...sexFix(item, item.envelope, [sexAi ? 'modelOther' : 'modelUnsure']), evidence });
      } else if (item.kind === 'aisex') {
        const evidence = { database: item.workbook, model: [item.gallery, item.conf], envelope: item.envelope ? [item.envelope] : undefined };
        add(item, 'aisex', 'look', { ...sexFix(item, item.gallery, [item.envelope ? 'envelopeForDatabase' : 'modelAlone']), evidence });
      } else if (item.kind === 'camid') {
        // The envelope shows another CAM than the file name: the files renamed to it.
        const target = item.envelope;
        const other = facts.byCam.get(target)?.[0];
        const targetPhotos = index.byCam.get(target);
        const ownSpecies = text(row?.values.SPECIES);
        const targetSpecies = text(other?.values.SPECIES);
        const why = [];
        if (item.fileIdAlsoRead) why.push('ownIdAlsoRead');
        if ((item.seen ?? 0) < (item.of ?? 0)) why.push('notOnEveryPhoto');
        if (!other) why.push('targetNoRow');
        if (targetPhotos && [...targetPhotos.dorsal, ...targetPhotos.ventral].length) why.push('targetHasPhotos');
        if (model && targetSpecies && !sameSpecies(targetSpecies, ownSpecies)) why.push(sameSpecies(model, targetSpecies) ? 'speciesFitsTarget' : sameSpecies(model, ownSpecies) ? 'speciesFitsFile' : 'speciesFitsNeither');
        const ops = [];
        for (const name of item.files ?? []) {
          const file = index.byName.get(String(name).toUpperCase());
          if (file) ops.push({ action: 'rename', file, name: stem(index.files.get(file)?.name ?? name).replace(new RegExp(`^${cam}`, 'i'), target), from: index.files.get(file)?.name ?? name });
        }
        const sure = !why.some(w => w !== 'speciesFitsTarget') && (item.conf ?? 0) >= ID_SURE;
        const blocked = why.includes('targetHasPhotos') || !ops.length;
        add(item, 'camid', blocked ? 'reason' : sure ? 'clear' : 'look', {
          fix: blocked ? null : { drive: ops, to: target },
          why: sure ? ['idEverywhere', ...why] : why,
          evidence: { file: cam, envelope: [target, item.conf, item.seen, item.of], database: ownSpecies || undefined, target: targetSpecies || undefined, model: top ? [top[0], r2(top[1])] : undefined },
        });
      } else if (item.kind === 'wings') {
        // Split: the side of the pair (a swap of d and v), and each photo turned.
        const wings = data.wings.byCam.get(cam) ?? [];
        const shown = wings.map(w => ({ w, file: w.fileId && index.files.has(w.fileId) ? w.fileId : index.byName.get(String(w.name).toUpperCase()) })).map(x => ({ ...x, name: x.file ? index.files.get(x.file).name : x.w.name }));
        const sides = shown.map(x => ({ ...x, problem: wingProblem(x.w, x.name) })).filter(x => x.problem?.side);
        if (sides.length) {
          const pair = shown.filter(x => /^[dv]$/i.test(x.name.slice(cam.length)));
          const swapped =
            sides.length === 2 && pair.length === 2 && sides.every(x => Math.abs(x.w.pDorsal - 0.5) * 2 >= 0.9) && new Set(sides.map(x => x.name.slice(cam.length).toLowerCase())).size === 2;
          const ops = swapped && sides.every(x => x.file) ? sides.map(x => ({ action: 'rename', file: x.file, name: `${cam}${x.name.slice(cam.length).toLowerCase() === 'd' ? 'v' : 'd'}`, from: x.name })) : [];
          add(item, 'side', swapped ? 'clear' : 'look', {
            fix: ops.length ? { drive: ops } : null,
            why: [swapped ? 'swapped' : sides.length === 1 ? 'oneSide' : 'sameSide'],
            conf: Math.max(...sides.map(x => Math.abs(x.w.pDorsal - 0.5) * 2)),
            evidence: { photos: shown.map(x => [x.name, x.w.side ?? null, r2(x.w.pDorsal)]) },
          });
        }
        for (const x of shown) {
          const problem = wingProblem(x.w, x.name);
          if (!problem?.turn || !x.file || !problem.degrees) continue;
          add(item, 'turn', 'clear', {
            key: stem(x.name).toUpperCase(),
            fix: { drive: [{ action: 'rotate', file: x.file, degrees: problem.degrees, from: x.name }] },
            why: ['turned'],
            conf: x.w.pTurn ?? 0.9,
            evidence: { photo: x.name, turn: problem.turn },
          });
        }
      }
    }
  }
  const rank = { clear: 0, reason: 1, look: 2 };
  entries.sort((a, b) => rank[a.tier] - rank[b.tier] || (b.conf ?? 0) - (a.conf ?? 0) || a.cam.localeCompare(b.cam));
  entryCache.set(store, { list, entries });
  return entries;
}

// ------------------------------------------------------------ sections
const listOf = value =>
  text(value)
    .split(',')
    .map(v => v.trim())
    .filter(Boolean);
/** The entries a section's filters keep, or { error }. */
export function sectionOf(store, { kind, tier, folder, year, q } = {}) {
  const kinds = listOf(kind).flatMap(k => (k === 'wings' ? ['side', 'turn'] : [k]));
  const unknown = kinds.find(k => !ENTRY_KINDS.includes(k));
  if (unknown) return { error: `kind is one or more of ${[...KINDS, 'side', 'turn'].join(', ')}` };
  const tiers = listOf(tier);
  if (tiers.some(t => !TIERS.includes(t))) return { error: `tier is one or more of ${TIERS.join(', ')}` };
  const place = text(folder).toLowerCase();
  const needle = text(q).toLowerCase();
  const entries = curationEntries(store).filter(
    e =>
      (!kinds.length || kinds.includes(e.kind)) &&
      (!tiers.length || tiers.includes(e.tier)) &&
      (!place || (e.folder ?? '').toLowerCase().startsWith(place)) &&
      (!text(year) || String(e.year) === text(year)) &&
      (!needle || JSON.stringify([e.cam, e.evidence, e.fix]).toLowerCase().includes(needle)),
  );
  return { entries };
}

const tally = (entries, key) => {
  const out = new Map();
  for (const e of entries) {
    const k = key(e) ?? 'unknown';
    const t = out.get(k) ?? out.set(k, { n: 0, clear: 0, reason: 0, look: 0 }).get(k);
    t.n++;
    t[e.tier]++;
  }
  return out;
};
/** A short answer: how many, by kind and tier, by folder and by year; the sections to work through. */
export function curationSummary(store, filters = {}) {
  const section = sectionOf(store, filters);
  if (section.error) return section;
  const { entries } = section;
  const tiers = Object.fromEntries(TIERS.map(t => [t, entries.filter(e => e.tier === t).length]));
  const kinds = Object.fromEntries([...tally(entries, e => e.kind)].map(([k, t]) => [k, t]));
  const big = (map, top) => {
    const all = [...map].sort((a, b) => b[1].n - a[1].n);
    const shown = all.slice(0, top).map(([name, t]) => ({ name, ...t }));
    const rest = all.slice(top);
    return rest.length ? [...shown, { name: `${rest.length} more`, n: rest.reduce((s, [, t]) => s + t.n, 0) }] : shown;
  };
  const { settled } = allDisagreements(store);
  return {
    total: entries.length,
    tiers,
    kinds,
    folders: big(tally(entries, e => e.folder ?? 'not in Photo_links'), 15),
    years: [...tally(entries, e => e.year)].sort((a, b) => String(b[0]).localeCompare(String(a[0]))).map(([name, t]) => ({ name, ...t })),
    settledBefore: settled.length,
    tiersMean:
      'clear: independent sources agree against one, the fix comes ticked; reason: the text evidence needs weighing (photo_facts with cams); look: only the photo settles it (photo_files_look view envelope or both).',
    next: 'Tell the person this summary and ask where to start. Then show_photo_fixes with a section (a tier, a kind, a folder or a year): its card, not this chat, is where they decide.',
  };
}

/** photo_facts: the summary or a section with tiers when asked for, otherwise as photo-facts.mjs answers. */
export function runPhotoFactsTool(store, args = {}, context = null) {
  if (args.summary) return curationSummary(store, args);
  const sectioned = ['tier', 'folder', 'year'].some(k => text(args[k])) || listOf(args.kind).some(k => ['side', 'turn'].includes(k));
  if (!sectioned || (Array.isArray(args.cams) ? args.cams.length : text(args.cams))) return runPhotoFacts(store, args, context);
  const section = sectionOf(store, args);
  if (section.error) return section;
  const size = Math.min(Math.max(Number(args.limit) || 25, 1), LIST_MAX);
  const start = Math.max(Number(args.offset) || 0, 0);
  const page = section.entries.slice(start, start + size).map(({ recordId: _r, row: _row, ...e }) => e);
  return {
    total: section.entries.length,
    offset: start,
    items: page,
    ...(start + page.length < section.entries.length ? { next: `offset ${start + page.length}` } : {}),
  };
}

// ------------------------------------------------------------ the card
export const CURATION_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'show_photo_fixes',
      description: [
        "Put a section of the photo archive's disagreements with the database on a card beside the chat, for the person to decide each fix beside its photos. Its rows are not returned here: the card shows each butterfly's photos, envelope and the ID line read, the evidence for and against the fix, and the fix; the `clear` ones come ticked.",
        '- A section: `tier`, `kind`, `folder`, `year`, `q` as in photo_facts (summary first), up to `limit` (default 60, at most 150), the surest first.',
        '- `items` instead: chosen entries, after weighing them (photo_facts with cams, a look at the photos): {cam, kind, value (the right species or sex; for camid the CAM the photos belong to), note (why, one line), check (true: it comes ticked)}. kind: species, sex, gallery, aisex, camid, side, turn (wings: both).',
        "- The person's «Aplicar» on the card writes the database fixes ticked (Historial, undoable), records each verdict (a rejected one is not raised again) and starts the Drive renames and turns as a plan of organize_photos on a card of its own.",
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          title: { type: 'string' },
          tier: { type: 'string' },
          kind: { type: 'string' },
          folder: { type: 'string' },
          year: { type: 'string' },
          q: { type: 'string' },
          limit: { type: 'integer' },
          offset: { type: 'integer', description: 'The next page of a section' },
          items: {
            type: 'array',
            items: {
              type: 'object',
              properties: { cam: { type: 'string' }, kind: { type: 'string' }, value: { type: 'string' }, note: { type: 'string' }, check: { type: 'boolean' } },
              required: ['cam', 'kind'],
            },
          },
        },
      },
    },
  },
];
export const CURATION_TOOL_NAMES = new Set(CURATION_TOOLS.map(t => t.function.name));

/** An entry as the card keeps it: photos, envelope crop and the CAMID line in the source photo. */
function cardEntry(store, data, index, e, extra = {}) {
  const names = e.fix?.drive?.map(o => stem(o.from)) ?? [];
  const context = photoContext(data, index, e.cam, names) ?? {};
  const files = Object.entries(context.photos?.files ?? {}).filter(([, f]) => !/\.(cr2|cr3|orf|nef|arw|rw2|dng|raw)$/i.test(f.name));
  const camid = (data.readingsByCam.get(e.cam) ?? []).filter(r => r.camid && r.fileId).map(r => {
    const line = r.camidLines?.find(l => String(l[0]).replace(/\s+/g, '').toUpperCase() === r.camid) ?? [...(r.camidLines ?? [])].sort((a, b) => camidLineScore(b) - camidLineScore(a))[0];
    const quad = line?.[2];
    const [width, height] = r.size ?? [];
    const points = Array.isArray(quad) && quad.length === 4 && quad.every(p => Array.isArray(p) && p.length === 2 && p.every(Number.isFinite));
    let box;
    if (points && width > 0 && height > 0) {
      const xy = quad.map(([x, y]) => r.turned ? [width - x, height - y] : [x, y]);
      const clamp = n => Math.round(Math.min(1, Math.max(0, n)) * 10000) / 10000;
      box = [clamp(Math.min(...xy.map(p => p[0])) / width), clamp(Math.min(...xy.map(p => p[1])) / height), clamp(Math.max(...xy.map(p => p[0])) / width), clamp(Math.max(...xy.map(p => p[1])) / height)];
    }
    return filled({ fileId: index.byName.get(r.name.toUpperCase()) ?? r.fileId, name: r.name, read: r.camid, conf: r2(r.conf), box, aspect: width > 0 && height > 0 ? r2(width / height) : undefined, turn: line ? ((r.turned ? 180 : 0) + (line[3] ?? 0)) % 360 : undefined });
  });
  // The envelope and the database rows of each butterfly the case involves (a CAMID: the one read too).
  const record = cam => ({ cam, ...recordFacts(store, cam) });
  let records = [record(e.cam)];
  // A CAMID case: the envelope (read on the file's photos) against the butterfly it names, then the file's own row.
  if (e.kind === 'camid' && e.fix?.to && e.fix.to !== e.cam) {
    const [file, named] = [records[0], record(e.fix.to)];
    records = [{ ...named, envelope: file.envelope, named: true }, { cam: file.cam, ...(file.database ? { database: file.database } : {}) }];
  }
  records = records.filter(r => r.envelope || r.database || r.named);
  return filled({
    ...e,
    records,
    photos: files.length || context.photos?.envelope ? { files: files.map(([id, f]) => filled({ id, name: f.name, wings: f.wings })), envelope: context.photos?.envelope } : undefined,
    idLines: camid,
    // The surest come ticked; the AI's choices as it marked them.
    checked: extra.check ?? e.tier === 'clear',
    note: extra.note,
  });
}

/**
 * show_photo_fixes and the card's «Aplicar» for one app. `show(card, context)` keeps a card (its link back);
 * `organize(operations, user, chat)` prepares Drive changes ({ status, plan, link } or { error });
 * `update(id, card, status)` changes a kept card.
 */
export function createCuration({ store, show, organize = null, update = null, getProposal = null, planAvailable = null }) {
  function run(args = {}, user, context) {
    const data = reviewData(store);
    const index = photoIndex(store);
    let chosen;
    let left = 0;
    let offset = 0;
    const problems = [];
    if (Array.isArray(args.items) && args.items.length) {
      const all = curationEntries(store);
      chosen = [];
      for (const it of args.items.slice(0, CARD_MAX)) {
        const cam = text(it.cam).toUpperCase().replace(/\s+/g, '');
        const kinds = text(it.kind) === 'wings' ? ['side', 'turn'] : [text(it.kind)];
        const found = all.filter(e => e.cam === cam && kinds.includes(e.kind));
        if (!found.length) {
          problems.push(`${cam} ${text(it.kind)}: no such disagreement now (photo_facts with cams says what there is)`);
          continue;
        }
        for (const e of found) {
          const value = text(it.value);
          let entry = e;
          if (value && ['species', 'gallery'].includes(e.kind)) entry = { ...e, fix: { field: 'SPECIES', from: e.fix?.from ?? text(e.evidence.database), to: value } };
          else if (value && ['sex', 'aisex'].includes(e.kind)) entry = { ...e, fix: { field: 'Sex', from: e.fix?.from ?? text(e.evidence.database), to: value.toLowerCase() } };
          else if (value && e.kind === 'camid' && e.fix?.drive) {
            const to = value.toUpperCase();
            entry = { ...e, fix: { to, drive: e.fix.drive.map(o => ({ ...o, name: stem(o.from).replace(new RegExp(`^${cam}`, 'i'), to) })) } };
          }
          chosen.push(cardEntry(store, data, index, entry, { note: text(it.note).slice(0, 300) || undefined, check: it.check === true }));
        }
      }
    } else {
      const section = sectionOf(store, args);
      if (section.error) return section;
      const size = Math.min(Math.max(Number(args.limit) || 60, 1), CARD_MAX);
      offset = Math.min(Math.max(Number(args.offset) || 0, 0), Math.max(section.entries.length - 1, 0));
      chosen = section.entries.slice(offset, offset + size).map(e => cardEntry(store, data, index, e));
      left = section.entries.length - offset - chosen.length;
    }
    if (!chosen.length) return { error: problems.length ? problems.join('; ') : 'Nothing in that section.' };
    const counts = {};
    for (const e of chosen) counts[e.kind] = (counts[e.kind] ?? 0) + 1;
    // Without a title the card names its section itself, in the person's language.
    const title = text(args.title).slice(0, 120);
    const shown = show({ type: 'curation', title, entries: chosen, offset, totalInSection: offset + chosen.length + left, leftInSection: left, filters: filled({ tier: args.tier, kind: args.kind, folder: args.folder, year: args.year }) }, context);
    return filled({
      shown: chosen.length,
      ticked: chosen.filter(e => e.checked).length,
      kinds: counts,
      ...shown,
      leftInSection: left || undefined,
      notFound: problems,
      next: 'The person decides on the card. Tell them what the section holds and what to look at; do not list the rows again here.',
    });
  }

  /** Apply only the decisions explicitly submitted on this card. The card stays open for unresolved rows. */
  async function apply(proposal, body, user) {
    const card = JSON.parse(proposal.sync_json || '{}');
    if (card.type !== 'curation') return { error: 'Not a card of photo fixes', code: 'not_found' };
    const byId = new Map(card.entries.map(e => [e.id, e]));
    const selected = key => {
      const ids = body[key] ?? (key === 'keep' ? body.reject : null);
      return [...new Set((Array.isArray(ids) ? ids : []).map(String))];
    };
    const groups = ['accept', 'keep', 'defer'].map(key => [key, selected(key)]);
    const requested = groups.flatMap(([, ids]) => ids);
    if (!requested.length) return { error: 'Choose at least one change', code: 'nothing' };
    if (requested.some(id => !byId.has(id)) || new Set(requested).size !== requested.length)
      return { error: 'A selected change is unknown or has more than one decision; reload the card', code: 'invalid_selection' };
    const values = body.values && typeof body.values === 'object' && !Array.isArray(body.values) ? body.values : {};
    const result = { written: 0, kept: 0, deferred: 0, queued: 0, planned: 0, skipped: [], failed: [], drive: [] };
    const decisions = new Map();
    const driveCandidates = [];
    const fresh = new Map(curationEntries(store).map(e => [e.id, e]));
    const previous = latestVerdicts(store.db);
    initVerdicts(store.db);
    const insert = store.db.prepare(
      'INSERT INTO issue_verdicts(issue_id,kind,verdict,value,comment,user_id,user_name,at,proposal_id,snapshot_json) VALUES(?,?,?,?,?,?,?,?,?,?)',
    );
    const name = user.displayName || user.username || String(user.id);
    const verdict = (e, v, value = null, progress = null) => {
      insert.run(e.id, VERDICT_KIND[e.kind], v, value, e.note ?? null, String(user.id), name, new Date().toISOString(), proposal.id, JSON.stringify({ ...snapshotOf(e), ...(progress ? { progress } : {}) }));
      previous.set(e.id, { verdict: v, proposal_id: proposal.id, value, snapshot_json: JSON.stringify({ progress }) });
    };
    const current = e => {
      const now = fresh.get(e.id);
      return now && now.kind === e.kind && now.recordId === e.recordId && JSON.stringify(now.evidence) === JSON.stringify(e.evidence) &&
        JSON.stringify(now.fix?.drive ?? null) === JSON.stringify(e.fix?.drive ?? null) && now.fix?.from === e.fix?.from;
    };
    const pending = e => {
      const v = previous.get(e.id);
      if (v?.proposal_id !== proposal.id || v.verdict !== 'pending') return null;
      try { return JSON.parse(v.snapshot_json).progress ?? null; } catch { return null; }
    };
    for (const [choice, ids] of groups) for (const id of ids) {
      const e = byId.get(id);
      const prior = previous.get(id);
      if (prior?.proposal_id === proposal.id && ['applied', 'rejected'].includes(prior.verdict)) {
        decisions.set(id, prior.verdict === 'applied' ? 'accepted' : 'kept');
        continue;
      }
      const held = pending(e);
      const failedQueue = ['sheet', 'sheet_failed'].includes(held?.type) ? store.outbox?.byRequest?.(held.requestId) : null;
      const inProgress = ['failed', 'conflict'].includes(failedQueue?.status) ? null : held;
      if (['drive', 'sheet'].includes(inProgress?.type) && choice !== 'accept') {
        result.skipped.push(`${e.cam}: this change is already ${inProgress.type === 'drive' ? 'planned on Drive' : 'queued for the sheet'}`);
        decisions.set(id, inProgress.type === 'drive' ? 'planned' : 'queued');
        continue;
      }
      if (choice === 'defer') {
        decisions.set(id, 'deferred');
        result.deferred++;
        continue;
      }
      if (!current(e) && !['drive', 'sheet'].includes(inProgress?.type)) {
        result.skipped.push(`${e.cam}: evidence changed since this card was made`);
        continue;
      }
      if (choice === 'keep') {
        verdict(e, 'rejected', e.fix?.from ?? null);
        decisions.set(id, 'kept');
        result.kept++;
        continue;
      }
      const to = text(values[id]) || e.fix?.to;
      if (e.fix?.field && e.recordId) {
        const r = store.getRecord(e.recordId);
        const progress = inProgress?.type === 'sheet' ? inProgress : null;
        if (!progress && (!r || text(r.values?.[e.fix.field]) !== text(e.fix.from))) {
          result.skipped.push(`${e.cam}: ${e.fix.field} changed since this card was made`);
          continue;
        }
        if (e.fix.field === 'Sex' && !['male', 'female', 'unknown'].includes(text(to).toLowerCase())) {
          result.skipped.push(`${e.cam}: invalid sex`);
          continue;
        }
        if (e.fix.field === 'SPECIES' && !listOptions(store, r?.sheet ?? e.sheet)?.SPECIES?.values?.has(to)) {
          result.skipped.push(`${e.cam}: species is not in the database list`);
          continue;
        }
        if (text(to).toLowerCase() === text(e.fix.from).toLowerCase()) {
          verdict(e, 'rejected', e.fix.from);
          decisions.set(id, 'kept');
          result.kept++;
          continue;
        }
        const baseId = `photo-fixes-${createHash('sha256').update(JSON.stringify([proposal.id, id, e.fix.from, to])).digest('hex')}`;
        const requestId = failedQueue ? `${baseId}-${createHash('sha256').update(`${failedQueue.id}:${failedQueue.updated_at}`).digest('hex').slice(0, 12)}` : baseId;
        if (progress && (progress.requestId !== requestId || progress.type !== 'sheet')) {
          result.skipped.push(`${e.cam}: another save is pending for this change`);
          continue;
        }
        try {
          const out = await applyBatch(store, { requestId, edits: [{ id: e.recordId, values: { [e.fix.field]: to }, expected: { [e.fix.field]: e.fix.from || null } }], reason: `Photo curation: ${e.cam} ${e.fix.field}` }, user, { source: 'ai_approved' });
          if (out.status === 'verified') {
            verdict(e, 'applied', to);
            decisions.set(id, 'accepted');
            result.written++;
          } else if (out.status === 'queued') {
            if (!progress) verdict(e, 'pending', to, { type: 'sheet', requestId, outboxId: out.outboxId });
            decisions.set(id, 'queued');
            result.queued++;
          } else result.failed.push(`${e.cam}: save was ${out.status}`);
        } catch (error) { result.failed.push(`${e.cam}: ${error.message}`); }
      } else if (e.fix?.drive?.length) {
        const progress = inProgress?.type === 'drive' ? inProgress : null;
        if (progress?.type === 'drive' && progress.plan) {
          result.drive.push({ id, plan: progress.plan, link: progress.link });
          decisions.set(id, 'planned');
          result.planned++;
          continue;
        }
        driveCandidates.push(e);
      } else result.skipped.push(`${e.cam}: no fix is available for this change`);
    }
    if (driveCandidates.length) {
      const ops = [...new Map(driveCandidates.flatMap(e => e.fix.drive).map(({ from: _from, ...op }) => [JSON.stringify(op), op])).values()];
      try {
        const out = organize ? await organize(ops, user, proposal) : { error: 'This app cannot plan Drive changes' };
        if (out.error || !out.plan) result.failed.push(`Drive plan: ${out.error ?? 'not confirmed'}`);
        else {
          if (out.changes?.leftOut?.count) result.failed.push(`Drive plan left out ${out.changes.leftOut.count} requested operation(s); check the plan before applying`);
          for (const e of driveCandidates) {
            verdict(e, 'pending', e.fix.to ?? null, { type: 'drive', plan: out.plan, link: out.link });
            result.drive.push({ id: e.id, plan: out.plan, link: out.link });
            decisions.set(e.id, 'planned');
            result.planned++;
          }
        }
      } catch (error) { result.failed.push(`Drive plan: ${error.message}`); }
    }
    const after = {
      ...card,
      entries: card.entries.map(e => decisions.has(e.id) ? { ...e, decision: decisions.get(e.id), ...(values[e.id] ? { chosen: text(values[e.id]) } : {}) } : e),
      result,
    };
    const complete = after.entries.every(e => ['accepted', 'kept'].includes(e.decision));
    update?.(proposal.id, after, complete ? 'applied' : 'pending');
    return { status: complete ? 'applied' : 'partial', ...result, entries: after.entries };
  }

  /** A separate Drive plan has finished. Only all required file operations completing settles its issue. */
  function reconcileDrive(planId, progress, planCard = null) {
    if (!['done', 'failed'].includes(progress?.state)) return { settled: 0, unresolved: 0 };
    initVerdicts(store.db);
    const completed = store.db.prepare("SELECT ops_json FROM drive_changes WHERE id=? AND undone_at IS NULL").get(planId);
    const done = completed ? JSON.parse(completed.ops_json || '[]') : [];
    const verdicts = [...latestVerdicts(store.db).values()].filter(v => {
      if (v.verdict !== 'pending') return false;
      try { const p = JSON.parse(v.snapshot_json)?.progress; return p?.type === 'drive' && p.plan === planId; } catch { return false; }
    });
    const updates = new Map();
    for (const v of verdicts) {
      const snapshot = JSON.parse(v.snapshot_json);
      const expected = snapshot.fix?.drive ?? [];
      const found = op => done.some(d => d.id === op.file && d.action === op.action &&
        (op.action === 'rename' ? stem(d.to).toUpperCase() === stem(op.name).toUpperCase() : op.action !== 'rotate' || d.degrees === op.degrees));
      const applied = expected.length > 0 && expected.every(found);
      const next = { ...snapshot, progress: applied ? { type: 'drive_done', plan: planId } : { type: 'drive_failed', plan: planId, reason: progress.error ?? 'Some file changes were left out or failed' } };
      store.db.prepare('INSERT INTO issue_verdicts(issue_id,kind,verdict,value,comment,user_id,user_name,at,proposal_id,snapshot_json) VALUES(?,?,?,?,?,?,?,?,?,?)')
        .run(v.issue_id, v.kind, applied ? 'applied' : 'pending', v.value, v.comment, v.user_id, v.user_name, new Date().toISOString(), v.proposal_id, JSON.stringify(next));
      const list = updates.get(v.proposal_id) ?? updates.set(v.proposal_id, []).get(v.proposal_id);
      list.push({ id: v.issue_id, applied });
    }
    for (const [id, changes] of updates) {
      const proposal = getProposal?.(id);
      if (!proposal) continue;
      const card = JSON.parse(proposal.sync_json || '{}');
      if (card.type !== 'curation') continue;
      const decisions = new Map(changes.map(c => [c.id, c.applied ? 'accepted' : null]));
      // One not made comes back to decide, saying why (the plan's own card lists each file's error).
      const why = e =>
        progress.errors?.find(x => (e.fix?.drive ?? []).some(o => String(x).toUpperCase().startsWith(stem(o.from).toUpperCase()))) ?? progress.error ?? 'the Drive change was not made';
      const entries = card.entries.map(e => (decisions.has(e.id) ? { ...e, decision: decisions.get(e.id), ...(decisions.get(e.id) ? { problem: undefined } : { problem: why(e) }) } : e));
      const complete = entries.every(e => ['accepted', 'kept'].includes(e.decision));
      update?.(id, { ...card, entries, result: { ...card.result, driveProgress: { plan: planId, state: progress.state, written: progress.written, total: progress.total, failed: progress.failed ?? 0, leftOut: planCard?.leftOut?.count ?? 0, errors: progress.errors?.slice(0, 3) } } }, complete ? 'applied' : 'pending');
    }
    return { settled: [...updates.values()].flat().filter(x => x.applied).length, unresolved: [...updates.values()].flat().filter(x => !x.applied).length };
  }

  /** Refresh queued sheet saves from local outbox/action state, without starting a write. */
  function refreshQueued(proposal) {
    let card = JSON.parse(proposal.sync_json || '{}');
    if (card.type !== 'curation') return { error: 'Not a card of photo fixes', code: 'not_found' };
    const latest = latestVerdicts(store.db);
    const decisions = new Map();
    const result = { confirmed: 0, failed: 0, queued: 0 };
    const insert = store.db.prepare('INSERT INTO issue_verdicts(issue_id,kind,verdict,value,comment,user_id,user_name,at,proposal_id,snapshot_json) VALUES(?,?,?,?,?,?,?,?,?,?)');
    for (const e of card.entries) {
      const v = latest.get(e.id);
      if (v?.proposal_id !== proposal.id || v.verdict !== 'pending') continue;
      const snapshot = JSON.parse(v.snapshot_json);
      const progress = snapshot.progress;
      if (progress?.type === 'drive' && planAvailable && !planAvailable(progress.plan)) {
        const plan = getProposal?.(progress.plan);
        const planCard = plan ? JSON.parse(plan.sync_json || '{}') : null;
        if (['done', 'failed'].includes(planCard?.progress?.state)) {
          reconcileDrive(progress.plan, planCard.progress, planCard);
          card = JSON.parse(getProposal?.(proposal.id)?.sync_json ?? proposal.sync_json);
        } else if (plan?.status !== 'applying' && planCard?.progress?.state !== 'writing') {
          const next = { ...snapshot, progress: { type: 'drive_failed', plan: progress.plan, reason: 'The Drive plan expired; prepare it again' } };
          insert.run(v.issue_id, v.kind, 'pending', v.value, v.comment, v.user_id, v.user_name, new Date().toISOString(), v.proposal_id, JSON.stringify(next));
          decisions.set(e.id, null);
          result.failed++;
        }
        continue;
      }
      if (progress?.type !== 'sheet') continue;
      const outbox = store.outbox?.byRequest?.(progress.requestId);
      const action = store.actionByRequest(progress.requestId);
      const answer = outbox?.result_json ? JSON.parse(outbox.result_json) : null;
      const verified = (outbox?.status === 'done' && (answer?.status ?? 'verified') === 'verified') || (!outbox && action?.status === 'verified');
      const failed = ['failed', 'conflict'].includes(outbox?.status) || (outbox?.status === 'done' && !verified) || (!outbox && action?.status === 'failed');
      if (!verified && !failed) {
        result.queued++;
        continue;
      }
      const next = { ...snapshot, progress: verified ? { type: 'sheet_done', requestId: progress.requestId, outboxId: progress.outboxId } : { type: 'sheet_failed', requestId: progress.requestId, outboxId: progress.outboxId, reason: outbox?.error_json ? JSON.parse(outbox.error_json)?.message : 'The queued save failed' } };
      insert.run(v.issue_id, v.kind, verified ? 'applied' : 'pending', v.value, v.comment, v.user_id, v.user_name, new Date().toISOString(), v.proposal_id, JSON.stringify(next));
      decisions.set(e.id, verified ? 'accepted' : null);
      result[verified ? 'confirmed' : 'failed']++;
    }
    const entries = card.entries.map(e => decisions.has(e.id) ? { ...e, decision: decisions.get(e.id) } : e);
    const complete = entries.every(e => ['accepted', 'kept'].includes(e.decision));
    if (decisions.size) update?.(proposal.id, { ...card, entries, result: { ...card.result, queueRefresh: result } }, complete ? 'applied' : 'pending');
    return { status: complete ? 'applied' : 'partial', entries, result };
  }

  return { run, apply, reconcileDrive, refreshQueued };
}

/** What a verdict keeps of an entry: enough to show it later, and to learn from (server/review.mjs trainingLabels). */
function snapshotOf(e) {
  const ev = e.evidence ?? {};
  return filled({
    kind: VERDICT_KIND[e.kind],
    cam: e.cam,
    sheet: e.sheet,
    row: e.row,
    recordId: e.recordId,
    label: e.cam,
    field: e.fix?.field,
    value: ev.database,
    fix: e.fix,
    tier: e.tier,
    why: e.why,
    evidence: ev,
    photos: e.photos?.envelope ? { envelope: e.photos.envelope } : undefined,
    envelopeCamid: e.kind === 'camid' ? ev.envelope?.[0] : undefined,
    ocr: ['species', 'sex'].includes(e.kind) && ev.envelope ? { field: e.fix?.field ?? (e.kind === 'sex' ? 'Sex' : 'SPECIES'), read: ev.envelope[0], sheet: ev.database } : undefined,
    ai: ['gallery', 'aisex'].includes(e.kind) && ev.model ? { recorded: ev.database, predicted: ev.model[0], confidence: ev.model[1] } : undefined,
  });
}
