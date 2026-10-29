// A deposit of racks at the LBI freezer, as the assistant's «Cambios propuestos» shows it
// (docs/assistant-tools-rack-reader.md): one proposal per chat holding every rack read there, each rack with
// its wells (A1..H12, from rack_read_photos' reading placed by the corner its code sits beside), the deposit
// sheet's fields (Anexo1: responsable, freezer, shelf, rack row, box) and what the workbook's tube-locations
// sheet would need: new rows for tubes it does not have, the rack or the well of those it has elsewhere.
// Nothing here writes: server/assistant.mjs saves the changes as a proposal (the person presses «Aplicar»).
//
// The page_json of such a proposal holds { photos, deposit }: deposit = { responsable, ultracongelador, batch,
// racks: [entry], xlsx?, drive? }, an entry = { reading, rack_id, code_beside, confirmed, estanteria,
// fila_de_rack, caja, fill, empty } as given, plus what depositState works out for the table (wells …).

import { COLS, RACK_ID, ROW_LETTERS, ROWS, WELLS, fillOrder, sequenceFit, wellOf } from './rack-reader.mjs';
import { moduleMap } from './schema.mjs';

export const FREEZER = 'LBI062';
const TUBE = /^[A-Z]{2}\d{6,10}$/;
const text = v => (v === null || v === undefined ? '' : String(v).trim());
const upper = v => text(v).toUpperCase();
const blank = v => ['', 'NA', 'N/A', 'NOT_COLLECTED', 'NOT_PROVIDED'].includes(upper(v));

/** The workbook's tube-locations sheet (Ithomiini_tube_locations_18Jun26), or null. */
export const tubeSheet = () => [...moduleMap.keys()].find(s => /tube_locations/i.test(s)) ?? null;

/** The well name, tidy (a1 → A1), or null. */
export function wellName(value) {
  const m = /^([A-Ha-h])\s*0?(\d{1,2})$/.exec(text(value));
  return m && Number(m[2]) >= 1 && Number(m[2]) <= COLS ? `${m[1].toUpperCase()}${Number(m[2])}` : null;
}

/** The fields a rack of the deposit sheet needs, and the deposit's own (said empty to the person until given). */
export const RACK_FIELDS = ['estanteria', 'fila_de_rack', 'caja'];
export const DEPOSIT_FIELDS = ['responsable', 'ultracongelador'];

/**
 * Columns of the tube-locations sheet a deposit also fills when the sheet has them (it has none of these on
 * 29 Oct 2026: Sort, Tube_ID, Rack_ID, Tube_location, Batch, permits, Sanger's IDs, Exclude).
 */
const PLACE_COLUMNS = [
  [/^(freezer|ultracongelador)$/i, (d, r) => d.ultracongelador],
  [/^(shelf|estanter[ií]a)$/i, (d, r) => r.estanteria],
  [/^(rack[ _]?row|fila[ _]de[ _]rack)$/i, (d, r) => r.fila_de_rack],
  [/^(box|caja|nro\.? ?caja)$/i, (d, r) => r.caja],
];

/**
 * Where the tubes of a reading are with its code beside `codeBeside` ('A12' or 'H12'), or as the reading
 * placed them (null): { mapping, wells: Map well → tube, disputed: [wells the photos disagree on], seen: Set of
 * wells with a code square not read } — null when the reading has no corner to place it by.
 */
export function placeReading(reading, codeBeside = null) {
  const corner = reading.code_corner;
  let mapping = reading.mapping ?? null;
  if (codeBeside && corner) mapping = ['m1', 'm2'].find(m => wellOf(m, ...corner) === codeBeside) ?? mapping;
  if (!mapping) return null;
  // Readings made before 29 Oct kept their wells only (as read).
  if (!reading.frame_cells) {
    if (mapping !== reading.mapping || !reading.wells) return null;
    const disputed = (reading.conflicts ?? []).flatMap(c => c.wells ?? []);
    return { mapping, wells: new Map(Object.entries(reading.wells)), disputed, seen: new Set() };
  }
  const wells = new Map();
  for (const [k, id] of Object.entries(reading.frame_cells)) wells.set(wellOf(mapping, ...k.split(',').map(Number)), id);
  const asWell = name => {
    const m = /^R(\d+)C(\d+)$/.exec(name ?? '');
    return m ? wellOf(mapping, Number(m[2]) - 1, Number(m[1]) - 1) : null;
  };
  const disputed = [...new Set((reading.conflicts ?? []).flatMap(c => [c.photo_position, c.also_at, c.kept_at].map(asWell).filter(Boolean)))];
  const seen = new Set((reading.seen ?? []).map(k => wellOf(mapping, ...k.split(',').map(Number))));
  return { mapping, wells, disputed, seen };
}

/** The two wells the tube beside the code can be in these photos (one per mirror: A12 or H1 on FK00274230). */
export const codeOptions = reading => (reading.code_corner ? ['m1', 'm2'].map(m => wellOf(m, ...reading.code_corner)) : []);
/** The well beside which a reading's code sits under its own placing (A12 on FK00274230), or null. */
export const codeBesideOf = reading => (reading.code_corner && reading.mapping ? wellOf(reading.mapping, ...reading.code_corner) : null);

/**
 * One rack's wells as the deposit has them: the reading placed by the entry's corner, the person's `fill`
 * (wells the photos did not read, from the tubes' side labels) and `empty`. Returns { wells: Map, empty: Set,
 * missing: [wells nobody has said], problems, code_beside, checked_by_ids }; wells null when A1 is not known.
 */
export function rackWells(reading, entry) {
  const options = codeOptions(reading);
  if (entry.code_beside && options.length && !options.includes(entry.code_beside))
    return { wells: null, empty: new Set(), missing: [...WELLS], problems: [`In these photos the rack code can only be beside ${options.join(' or ')}, not ${entry.code_beside}.`], code_beside: null, checked_by_ids: false };
  const placed = placeReading(reading, entry.code_beside ?? null);
  if (!placed) return { wells: null, empty: new Set(), missing: [...WELLS], problems: ['A1 is not known for this rack: ' + (reading.orientation?.note ?? 'ask the person.')], code_beside: null, checked_by_ids: false };
  const wells = new Map(placed.wells);
  const problems = [];
  // Wells the photos disagree on: the person says which tube (fill) or that it is empty.
  for (const w of placed.disputed) wells.delete(w);
  const empty = new Set((Array.isArray(entry.empty) ? entry.empty : []).map(wellName).filter(Boolean));
  for (const [w, id] of Object.entries(entry.fill && typeof entry.fill === 'object' ? entry.fill : {})) {
    const well = wellName(w);
    const tube = upper(id);
    if (!well || !TUBE.test(tube)) problems.push(`${w} = ${id} is not a well and a tube ID.`);
    else if (wells.has(well) && wells.get(well) !== tube && !placed.disputed.includes(well)) problems.push(`${well} was read as ${wells.get(well)}, not ${tube}.`);
    else wells.set(well, tube);
  }
  for (const w of empty) wells.delete(w);
  const twice = [...wells.values()].filter((id, n, all) => all.indexOf(id) !== n);
  if (twice.length) problems.push(`${[...new Set(twice)].join(', ')} on two wells.`);
  const missing = WELLS.filter(w => !wells.has(w) && !empty.has(w)).sort((a, b) => fillOrder(a) - fillOrder(b));
  const fit = sequenceFit(wells);
  const code_beside = reading.code_corner ? wellOf(placed.mapping, ...reading.code_corner) : null;
  return { wells, empty, missing, disputed: placed.disputed.filter(w => missing.includes(w)), problems, code_beside, checked_by_ids: fit.pairs >= 10 && fit.fit >= 0.8 * fit.pairs, seen: placed.seen };
}

/**
 * Tube IDs looked up in every tube column of the workbook (tubes_lookup): per ID the rows holding it (CAM,
 * species, sex, tissue, medium) and its place in the tube-locations sheet; `not_found`: IDs no row has.
 */
const TUBE_FIELD = /^(?:[FM]_)?Tube_\d(?:_id)?(?:_LEGS)?$|^Tube_ID$|^Tube$/i;
const SKIP_SHEETS = /tube_locations|manifest/i;
export function lookupTubes(store, list) {
  const ids = [...new Set(list.map(upper).filter(Boolean))].slice(0, 2000);
  const rows = ids.length
    ? store.db
        .prepare(
          `SELECT DISTINCT r.sheet, r.row_num, r.values_json FROM records r, json_each(r.values_json) j
           WHERE r.missing = 0 AND j.type = 'text' AND upper(trim(j.value)) IN (SELECT value FROM json_each(?))`,
        )
        .all(JSON.stringify(ids))
    : [];
  const found = new Map(ids.map(id => [id, { rows: [], location: null }]));
  for (const row of rows) {
    const v = JSON.parse(row.values_json);
    if (/tube_locations/i.test(row.sheet)) {
      const id = upper(v.Tube_ID);
      if (found.has(id)) found.get(id).location = { rack: text(v.Rack_ID) || null, well: text(v.Tube_location) || null, sheet: row.sheet };
      continue;
    }
    if (SKIP_SHEETS.test(row.sheet)) continue;
    for (const [field, value] of Object.entries(v)) {
      const id = upper(value);
      if (!TUBE_FIELD.test(field) || !found.has(id)) continue;
      const stem = field.replace(/_id(_LEGS)?$/i, '').replace(/^Tube_ID$/i, 'Tube');
      const n = /Tube_(\d)/i.exec(field)?.[1];
      const tissue = /_LEGS$/i.test(field) ? 'LEGS' : text(v[`${stem}_tissue`]) || null;
      const medium = [n && v[`T${n}_Preservation_medium`], v.Preservation_medium].find(x => !blank(x));
      found.get(id).rows.push({
        sheet: row.sheet,
        row: row.row_num,
        field,
        cam_id: text(v.CAM_ID) || null,
        species: text(v.SPECIES ?? v.Species) || null,
        subspecies: text(v.Subspecies_Form) || null,
        sex: text(v.Sex) || null,
        tissue,
        medium: medium ? text(medium) : null,
        sample_type: [medium && text(medium), tissue].filter(Boolean).join(' · ') || null,
      });
    }
  }
  const tubes = [...found].map(([id, f]) => ({ tube_id: id, rows: f.rows, ...(f.location ? { location: f.location } : {}) }));
  return {
    asked: ids.length,
    found: tubes.filter(t => t.rows.length).length,
    not_found: tubes.filter(t => !t.rows.length).map(t => t.tube_id),
    tubes: tubes.filter(t => t.rows.length || t.location),
  };
}

/** What a rack entry keeps as given (the rest depositState works out again each time). */
export const RACK_INPUT = ['reading', 'photos', 'rack_id', 'code_beside', 'confirmed', 'estanteria', 'fila_de_rack', 'caja', 'fill', 'empty'];
export const rackInput = entry => Object.fromEntries(RACK_INPUT.filter(k => entry?.[k] !== undefined).map(k => [k, entry[k]]));

/** A new deposit for the person signed in: the freezer LBI062 and their name as responsable (said, to confirm). */
export const newDeposit = user => ({
  responsable: text(user?.displayName ?? user?.display_name ?? user?.username) || '',
  responsable_default: true,
  ultracongelador: FREEZER,
  racks: [],
});

/**
 * The deposit as the table shows it, and the changes it implies in the tube-locations sheet: per rack its
 * wells (`wells[well] = { t: tube, s: 'new' | 'moved' | 'same' | 'check', r: recordId, was: { rack, well } }`;
 * `s: 'empty'` for a well said empty; wells nobody said are in `missing`), the tubes no sheet of the workbook
 * has (`need_rows`: they need their rows, never invented), the sheet's tubes on this rack the photos do not
 * show there (`elsewhere`), tubes with two rows in the sheet (`duplicates`). `readingOf(token)` reads a reading
 * (its owner's); `changes` / `newRows` as propose_changes takes them; `reason` the proposal's.
 */
export function depositState(store, deposit, readingOf) {
  const sheet = tubeSheet();
  const columns = new Set(moduleMap.get(sheet)?.fields.map(f => f.key) ?? []);
  const racks = [];
  for (const entry of deposit.racks ?? []) {
    const reading = readingOf(entry.reading);
    if (!reading) {
      racks.push({ ...entry, wells: {}, missing: [...WELLS], problems: ['Its reading is gone: read the photos again.'] });
      continue;
    }
    const placed = rackWells(reading, entry);
    const rack_id = upper(entry.rack_id || reading.rack_id) || null;
    racks.push({
      ...entry,
      rack_id,
      rack_id_read: !!reading.rack_id && upper(reading.rack_id) === rack_id,
      code_beside: placed.code_beside,
      code_read: !!reading.code_corner,
      code_options: codeOptions(reading),
      // The code's corner taken as the team's racks have it, not yet confirmed by the person.
      assumed: !entry.confirmed && !!reading.orientation?.assumed && placed.code_beside === codeBesideOf(reading),
      checked_by_ids: placed.checked_by_ids,
      tubes_read: Object.keys(reading.frame_cells ?? reading.wells ?? {}).length,
      _wells: placed.wells,
      _empty: placed.empty,
      missing: placed.missing,
      disputed: placed.disputed ?? [],
      problems: [...placed.problems, ...(rack_id && !RACK_ID.test(rack_id) ? [`${rack_id} is not a rack code (FK…, FH…, FE…, SA… and 8 digits).`] : []), ...(rack_id ? [] : ['The rack code was not read: ask the person for it (printed beside a corner).'])],
    });
  }
  // The same tube on two racks of the deposit.
  const onRack = new Map();
  for (const r of racks) for (const id of r._wells?.values() ?? []) onRack.set(id, [...(onRack.get(id) ?? []), r.rack_id ?? '?']);
  for (const r of racks)
    for (const id of r._wells?.values() ?? []) if (onRack.get(id).length > 1) r.problems.push(`${id} is on ${onRack.get(id).join(' and ')}.`);
  const ids = [...onRack.keys()];
  const rackIds = racks.map(r => r.rack_id).filter(Boolean);
  const byTube = new Map();
  const byRack = new Map();
  if (sheet && (ids.length || rackIds.length))
    for (const row of store.db
      .prepare(
        `SELECT id, row_num, values_json FROM records WHERE sheet = ? AND missing = 0 AND (
           upper(trim(json_extract(values_json, '$.Tube_ID'))) IN (SELECT value FROM json_each(?))
           OR upper(trim(json_extract(values_json, '$.Rack_ID'))) IN (SELECT value FROM json_each(?)))`,
      )
      .all(sheet, JSON.stringify(ids), JSON.stringify(rackIds))) {
      const v = JSON.parse(row.values_json);
      const r = { id: row.id, row: row.row_num, tube: upper(v.Tube_ID), rack: upper(v.Rack_ID), well: wellName(v.Tube_location) ?? text(v.Tube_location) };
      byTube.set(r.tube, [...(byTube.get(r.tube) ?? []), r]);
      if (r.rack) byRack.set(r.rack, [...(byRack.get(r.rack) ?? []), r]);
    }
  let sort = null;
  if (sheet && columns.has('Sort'))
    sort = Number(store.db.prepare("SELECT max(CAST(json_extract(values_json, '$.Sort') AS INTEGER)) n FROM records WHERE sheet = ? AND missing = 0").get(sheet).n) || 0;
  const need = new Set(ids.length ? lookupTubes(store, ids).not_found : []);
  const changes = [];
  const newRows = [];
  const out = racks.map(r => {
    const { _wells, _empty, ...entry } = r;
    const wells = {};
    for (const w of _empty ?? []) wells[w] = { s: 'empty' };
    const counts = { new: 0, moved: 0, same: 0, check: 0 };
    const duplicates = [];
    const ready = !!_wells && !!r.rack_id && RACK_ID.test(r.rack_id) && !r.problems.length;
    for (const [well, tube] of [...(_wells ?? [])].sort((a, b) => fillOrder(a[0]) - fillOrder(b[0]))) {
      const rows = byTube.get(tube) ?? [];
      const here = rows.find(x => x.rack === r.rack_id && x.well === well);
      let cell;
      if (here) cell = { t: tube, s: 'same', r: here.id };
      else if (rows.length > 1) {
        cell = { t: tube, s: 'check', was: rows.map(x => ({ rack: x.rack, well: x.well })) };
        duplicates.push(tube);
      } else if (rows.length === 1) {
        const old = rows[0];
        cell = { t: tube, s: 'moved', r: old.id, was: { rack: old.rack || null, well: old.well || null } };
        if (ready) {
          const values = {};
          if (old.rack !== r.rack_id) values.Rack_ID = r.rack_id;
          if (old.well !== well) values.Tube_location = well;
          changes.push({ recordId: old.id, values, note: `Antes: ${old.rack || '—'} ${old.well || '—'}; las fotos del rack: ${r.rack_id} ${well}` });
        }
      } else {
        cell = { t: tube, s: 'new' };
        if (ready) {
          const values = { Tube_ID: tube, Rack_ID: r.rack_id, Tube_location: well };
          if (sort !== null) values.Sort = ++sort;
          if (text(deposit.batch) && columns.has('Batch')) values.Batch = text(deposit.batch);
          for (const [pattern, of] of PLACE_COLUMNS) {
            const column = [...columns].find(c => pattern.test(c));
            const value = column && text(of(deposit, r));
            if (value) values[column] = value;
          }
          newRows.push({ sheet, values, note: `Rack ${r.rack_id}, ${well}: leído de las fotos` });
        }
      }
      if (need.has(tube)) cell.n = 1;
      counts[cell.s]++;
      wells[well] = cell;
    }
    const placedTubes = new Set(_wells?.values() ?? []);
    const elsewhere = (byRack.get(r.rack_id) ?? [])
      .filter(x => !placedTubes.has(x.tube))
      .map(x => ({ tube: x.tube, well: x.well }))
      .sort((a, b) => (fillOrder(a.well ?? 'A1') || 0) - (fillOrder(b.well ?? 'A1') || 0));
    return {
      ...entry,
      wells,
      counts: { ...counts, empty: _empty?.size ?? 0, missing: r.missing.length },
      need_rows: [...(_wells?.values() ?? [])].filter(t => need.has(t)),
      elsewhere,
      duplicates,
      unset: RACK_FIELDS.filter(f => !text(r[f])),
    };
  });
  const unset = [...DEPOSIT_FIELDS.filter(f => !text(deposit[f])), ...(deposit.responsable_default && text(deposit.responsable) ? ['responsable (assumed: the person signed in)'] : [])];
  const codes = out.map(r => r.rack_id ?? '?');
  const told = [
    newRows.length ? `${newRows.length} ${newRows.length === 1 ? 'tubo nuevo' : 'tubos nuevos'}` : null,
    changes.length ? `${changes.length} con otra posición` : null,
  ].filter(Boolean);
  const reason = `Depósito LBI · ${codes.length === 1 ? 'rack' : 'racks'} ${codes.join(', ')}${told.length ? `: ${told.join(', ')}` : ': sin cambios en la hoja'}`;
  return { sheet, deposit: { ...deposit, sheet, racks: out, unset }, changes, newRows, reason };
}

/** Whether a deposit can be written as the Anexo1 sheet: every well of every rack read, filled or said empty. */
export function depositProblems(deposit) {
  const problems = [];
  if (!text(deposit.responsable)) problems.push('responsable: the person depositing the samples.');
  for (const [k, r] of (deposit.racks ?? []).entries()) {
    const name = `Rack ${k + 1} (${r.rack_id ?? 'code not read'})`;
    for (const p of r.problems ?? []) problems.push(`${name}: ${p}`);
    const disputed = r.disputed ?? [];
    if (disputed.length) problems.push(`${name}: the photos disagree on ${disputed.join(', ')}: ask the person which tube is there (fill) or whether it is empty.`);
    const open = (r.missing ?? []).filter(w => !disputed.includes(w));
    if (open.length) problems.push(`${name}: no tube for ${open.join(', ')}: give each (its side label) or say it is empty.`);
  }
  return problems;
}

/** A rack's wells as the deposit sheet writes them (Map well → tube). */
export const sheetWells = rack => new Map(Object.entries(rack.wells ?? {}).filter(([, c]) => c.t).map(([w, c]) => [w, c.t]));

export { ROWS, ROW_LETTERS };
