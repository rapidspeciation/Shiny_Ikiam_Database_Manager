// The assistant's tools for tube racks (docs/assistant-tools-rack-reader.md):
//   rack_read_photos  photos of a rack's bottom (chat attachments) → its code, the tubes on A1..H12, what is missing
//   rack_report       the readings as the LBI's deposit sheet (Anexo1: one sheet per rack), a download link
//   tubes_lookup      tube IDs → the workbook's rows (CAM, species, tissue, medium), and the IDs it does not have
// Nothing here changes the workbook (SELECT only) or Drive. Readings and reports are files of the app's own
// (<rackReportsDir>, beside the database by default), each kept with the person who made it; a report is served
// at /api/rack-reports/<token>/<file> to that person (or an admin) signed in to the app.

import { randomBytes } from 'node:crypto';
import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { crc32, deflateRawSync } from 'node:zlib';
import { attachmentFile } from './proposal-photos.mjs';
import { COLS, ROWS, ROW_LETTERS, WELLS, fillOrder, loadPhoto, mergeReadings, photoName, readOne, wellOf } from './rack-reader.mjs';

export const MAX_PHOTOS = 6;
const TOKEN = /^[a-f0-9]{24}$/;
const FILE = /^[\w.-]{1,120}\.xlsx$/;
const TUBE = /^[A-Z]{2}\d{6,10}$/;

export const RACK_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'rack_read_photos',
      description: [
        "Reads a 96-tube rack (8 × 12, FluidX-style tubes) from photos of its bottom: every tube's 2D code, the rack's own code, and where each tube is (A1..H12). For unloading samples into a rack and listing what was left.",
        '- `photos`: 1 to 6 chat attachments (file names from "[Attached image … saved at …]"), the same rack from slightly different angles: the photos are merged by tube ID, so a tube hidden by glare in one is read in another.',
        "- A1: a photo of the bottom is a mirror of the rack seen from above, so A1 is at one of two corners. The tool proves which from the tubes' own IDs when the rack holds pre-loaded sequential tubes; otherwise ask the person, then call again with `a1_tube` (the ID of the tube in A1: A1 is printed on the rack's top) or `rack_code_beside` ('A12' or 'H12': the well the rack's code is next to). Never guess it.",
        'Returns `reading` (give it to rack_report), the rack ID, `orientation`, the wells with their tubes, `missing` wells (tube not read: ask for another photo, or the ID from its side label) and conflicts.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          photos: { type: 'array', items: { type: 'string' } },
          a1_tube: { type: 'string' },
          rack_code_beside: { type: 'string', enum: ['A12', 'H12'] },
        },
        required: ['photos'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'rack_report',
      description: [
        "The deposit sheet of the LBI freezer (Alex's Anexo1): an .xlsx with one sheet per rack, «Caja <rack code> (8x12)», the tubes on their wells A1..H12, and a download link to give the person as a Markdown link.",
        '- `racks`: one entry per rack: `reading` (from rack_read_photos, A1 known), `estanteria`, `fila_de_rack`, `caja` (box number); `fill` {well: tube ID} for tubes the photos did not read (from the tube\'s side label); `empty` [wells] the person says are empty.',
        '- `responsable` (the person depositing), `ultracongelador` (default LBI062).',
        'Refused while a well of a rack is neither read, filled nor said empty: ask the person.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          racks: {
            type: 'array',
            items: {
              type: 'object',
              properties: {
                reading: { type: 'string' },
                estanteria: { type: 'string' },
                fila_de_rack: { type: 'string' },
                caja: { type: 'string' },
                fill: { type: 'object', additionalProperties: { type: 'string' } },
                empty: { type: 'array', items: { type: 'string' } },
              },
              required: ['reading'],
            },
          },
          responsable: { type: 'string' },
          ultracongelador: { type: 'string' },
        },
        required: ['racks', 'responsable'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'tubes_lookup',
      description: [
        "Tube IDs (FS50849233…) looked up in every tube column of the workbook: the rows holding each, with CAM, species, sex, the tube's tissue and preservation medium, and the tube's rack position in the tube-locations sheet. `not_found`: IDs no row has. A check before a deposit (every tube should be in the database); give `reading` instead of `tubes` for a rack read by rack_read_photos.",
      ].join(' '),
      parameters: {
        type: 'object',
        properties: { tubes: { type: 'array', items: { type: 'string' } }, reading: { type: 'string' } },
      },
    },
  },
];
export const RACK_TOOL_NAMES = new Set(RACK_TOOLS.map(t => t.function.name));

const text = v => (v === null || v === undefined ? '' : String(v).trim());
const blank = v => ['', 'NA', 'N/A', 'NOT_COLLECTED', 'NOT_PROVIDED'].includes(text(v).toUpperCase());
const upper = v => text(v).toUpperCase();

/** The well name, tidy (a1 → A1), or null. */
export function wellName(value) {
  const m = /^([A-Ha-h])\s*0?(\d{1,2})$/.exec(text(value));
  return m && Number(m[2]) >= 1 && Number(m[2]) <= COLS ? `${m[1].toUpperCase()}${Number(m[2])}` : null;
}

// ---------------------------------------------------------------- .xlsx

const xml = s =>
  String(s)
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;')
    .replace(/[\u0000-\u0008\u000b\u000c\u000e-\u001f]/g, '');

/** A .zip of { name: Buffer|string } (deflated). */
export function zip(files) {
  const locals = [];
  const central = [];
  let offset = 0;
  for (const [name, content] of Object.entries(files)) {
    const raw = Buffer.isBuffer(content) ? content : Buffer.from(content, 'utf8');
    const data = deflateRawSync(raw);
    const nameBuf = Buffer.from(name, 'utf8');
    const crc = crc32(raw);
    const head = Buffer.alloc(30);
    head.writeUInt32LE(0x04034b50, 0);
    head.writeUInt16LE(20, 4);
    head.writeUInt16LE(0x0800, 6);
    head.writeUInt16LE(8, 8);
    head.writeUInt32LE(0x00210000, 10); // 1 Jan 1980
    head.writeUInt32LE(crc, 14);
    head.writeUInt32LE(data.length, 18);
    head.writeUInt32LE(raw.length, 22);
    head.writeUInt16LE(nameBuf.length, 26);
    locals.push(head, nameBuf, data);
    const dir = Buffer.alloc(46);
    dir.writeUInt32LE(0x02014b50, 0);
    dir.writeUInt16LE(20, 4);
    dir.writeUInt16LE(20, 6);
    dir.writeUInt16LE(0x0800, 8);
    dir.writeUInt16LE(8, 10);
    dir.writeUInt32LE(0x00210000, 12);
    dir.writeUInt32LE(crc, 16);
    dir.writeUInt32LE(data.length, 20);
    dir.writeUInt32LE(raw.length, 24);
    dir.writeUInt16LE(nameBuf.length, 28);
    dir.writeUInt32LE(offset, 42);
    central.push(dir, nameBuf);
    offset += head.length + nameBuf.length + data.length;
  }
  const size = central.reduce((s, b) => s + b.length, 0);
  const end = Buffer.alloc(22);
  end.writeUInt32LE(0x06054b50, 0);
  end.writeUInt16LE(Object.keys(files).length, 8);
  end.writeUInt16LE(Object.keys(files).length, 10);
  end.writeUInt32LE(size, 12);
  end.writeUInt32LE(offset, 16);
  return Buffer.concat([...locals, ...central, end]);
}

const colName = n => (n < 26 ? String.fromCharCode(65 + n) : colName(Math.floor(n / 26) - 1) + String.fromCharCode(65 + (n % 26)));
// Styles: 0 plain, 1 bordered, 2 bordered bold, 3 bordered centred (the merged rack cells), 4 bordered bold centred.
const STYLES = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><fonts count="2"><font><sz val="11"/><name val="Calibri"/></font><font><b/><sz val="11"/><name val="Calibri"/></font></fonts><fills count="2"><fill><patternFill patternType="none"/></fill><fill><patternFill patternType="gray125"/></fill></fills><borders count="2"><border/><border><left style="thin"/><right style="thin"/><top style="thin"/><bottom style="thin"/></border></borders><cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs><cellXfs count="5"><xf xfId="0"/><xf xfId="0" borderId="1" applyBorder="1"/><xf xfId="0" fontId="1" borderId="1" applyFont="1" applyBorder="1"/><xf xfId="0" borderId="1" applyBorder="1" applyAlignment="1"><alignment horizontal="center" vertical="center"/></xf><xf xfId="0" fontId="1" borderId="1" applyFont="1" applyBorder="1" applyAlignment="1"><alignment horizontal="center" vertical="center"/></xf></cellXfs><cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles></styleSheet>`;

/**
 * One rack's sheet as Anexo1 lays it out: A1 «Responsable: …», G1:R1 the diagram's title, row 2 the headers
 * and columns 1..12, rows 3..10 the rows A..H (letter in F, tubes in G..R), A3:E10 merged per column with the
 * freezer, shelf, rack code, rack row and box number.
 */
export function sheetXml({ responsable, ultracongelador, estanteria, rack, fila, caja, wells }) {
  const rows = [];
  const cell = (col, row, value, style) => {
    const ref = `${colName(col)}${row}`;
    if (value === null || value === undefined || value === '') return `<c r="${ref}" s="${style}"/>`;
    if (typeof value === 'number') return `<c r="${ref}" s="${style}"><v>${value}</v></c>`;
    return `<c r="${ref}" s="${style}" t="inlineStr"><is><t>${xml(value)}</t></is></c>`;
  };
  const num = v => (/^\d+$/.test(text(v)) ? Number(v) : text(v));
  const last = 6 + COLS - 1;
  rows.push(`<row r="1">${cell(0, 1, `Responsable: ${responsable}`, 2)}${cell(6, 1, 'Diagrama de posición de tubos por caja', 4)}${Array.from({ length: COLS - 1 }, (_, k) => cell(7 + k, 1, null, 4)).join('')}</row>`);
  const heads = ['Ultracongelador', 'Estanteria', 'Rack', 'Fila de rack', 'Nro. Caja'];
  rows.push(`<row r="2">${heads.map((h, k) => cell(k, 2, h, 4)).join('')}${cell(5, 2, null, 2)}${Array.from({ length: COLS }, (_, k) => cell(6 + k, 2, k + 1, 2)).join('')}</row>`);
  for (let r = 0; r < ROWS; r++) {
    const row = 3 + r;
    const left = [ultracongelador, text(estanteria), rack, num(fila), num(caja)].map((v, k) => cell(k, row, r === 0 ? v : null, 3)).join('');
    const tubes = Array.from({ length: COLS }, (_, k) => cell(6 + k, row, wells.get(`${ROW_LETTERS[r]}${k + 1}`) ?? null, 1)).join('');
    rows.push(`<row r="${row}">${left}${cell(5, row, ROW_LETTERS[r], 2)}${tubes}</row>`);
  }
  const widths = [30.29, 10.71, 11.71, 10.71, 10.71, 4.71, ...Array(COLS).fill(12.71)];
  const merges = [`G1:${colName(last)}1`, ...[0, 1, 2, 3, 4].map(k => `${colName(k)}3:${colName(k)}${2 + ROWS}`)];
  return `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><cols>${widths.map((w, k) => `<col min="${k + 1}" max="${k + 1}" width="${w}" customWidth="1"/>`).join('')}</cols><sheetData>${rows.join('')}</sheetData><mergeCells count="${merges.length}">${merges.map(m => `<mergeCell ref="${m}"/>`).join('')}</mergeCells></worksheet>`;
}

/** The workbook of sheets [{ name, xml }]. */
export function workbook(sheets) {
  const files = {
    '[Content_Types].xml': `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/><Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/><Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>${sheets.map((_, k) => `<Override PartName="/xl/worksheets/sheet${k + 1}.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>`).join('')}</Types>`,
    '_rels/.rels': `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>`,
    'xl/workbook.xml': `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets>${sheets.map((s, k) => `<sheet name="${xml(s.name)}" sheetId="${k + 1}" r:id="rId${k + 1}"/>`).join('')}</sheets></workbook>`,
    'xl/_rels/workbook.xml.rels': `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">${sheets.map((_, k) => `<Relationship Id="rId${k + 1}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet${k + 1}.xml"/>`).join('')}<Relationship Id="rId${sheets.length + 1}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/></Relationships>`,
    'xl/styles.xml': STYLES,
  };
  sheets.forEach((s, k) => (files[`xl/worksheets/sheet${k + 1}.xml`] = s.xml));
  return zip(files);
}

// ---------------------------------------------------------------- files kept

export const reportsDir = config => config.rackReportsDir ?? (config.databasePath && config.databasePath !== ':memory:' ? join(dirname(config.databasePath), 'rack-reports') : null);

function save(dir, owner, name, body) {
  const token = randomBytes(12).toString('hex');
  const at = join(dir, token);
  mkdirSync(at, { recursive: true, mode: 0o700 });
  writeFileSync(join(at, 'owner.json'), JSON.stringify({ owner, created: new Date().toISOString() }));
  writeFileSync(join(at, name), body, { mode: 0o600 });
  return token;
}

/** A kept file for a signed-in user: { path, name } when theirs (or they are an admin), else null. */
export function reportFile(dir, token, name, user) {
  if (!dir || !TOKEN.test(token ?? '') || !(FILE.test(name ?? '') || name === 'reading.json')) return null;
  const path = join(dir, token, name);
  if (!existsSync(path)) return null;
  try {
    const { owner } = JSON.parse(readFileSync(join(dir, token, 'owner.json'), 'utf8'));
    if (owner !== user?.id && user?.role !== 'admin') return null;
  } catch {
    return null;
  }
  return { path, name };
}

// ---------------------------------------------------------------- the tools

const TUBE_FIELD = /^(?:[FM]_)?Tube_\d(?:_id)?(?:_LEGS)?$|^Tube_ID$|^Tube$/i;
const SKIP_SHEETS = /tube_locations|manifest/i;

/**
 * The tools for one app. `store`: the app's database (SELECT only); config: `rackReportsDir`, `publicUrl`,
 * `t3.home` (the chat attachments); `readPhoto` (tests) reads a file into readOne's result.
 */
export function createRackTools({ store, config = {}, readPhoto = async path => readOne(await loadPhoto(path)) }) {
  const dir = reportsDir(config);
  const base = String(config.publicUrl || '').replace(/\/+$/, '');

  async function readPhotos(args, user) {
    if (!dir) return { error: 'This app has no folder for rack readings (rackReportsDir).' };
    const given = (Array.isArray(args.photos) ? args.photos : [args.photos]).filter(Boolean).slice(0, MAX_PHOTOS);
    if (!given.length) return { error: 'Give the photos: the chat attachments of the rack seen from below.' };
    const files = given.map(g => attachmentFile(config.t3?.home, String(g)));
    const refused = given.filter((_, k) => !files[k]);
    if (refused.length) return { error: `Not chat attachments of this server: ${refused.join(', ')}` };
    const readings = [];
    const photos = [];
    for (const f of files) {
      try {
        const r = await readPhoto(f.path);
        readings.push(r);
        photos.push({ photo: f.name, tubes_read: r.tubes.size, rack_code: r.rack?.text ?? null, ...(r.lattice ? {} : { grid: 'not found', codes: r.loose ?? [] }) });
      } catch (e) {
        photos.push({ photo: f.name, error: e.message });
      }
    }
    const a1 = args.a1_tube ? upper(args.a1_tube) : null;
    const m = mergeReadings(readings, { a1Tube: a1, codeAt: args.rack_code_beside ?? null });
    const known = m.orientation.mapping;
    const wells = {};
    const byPhoto = {};
    const missing = [];
    const photoTimes = {};
    for (let r = 0; r < (m.frame ? ROWS : 0); r++)
      for (let c = 0; c < COLS; c++) {
        const v = m.cells?.get?.(`${c},${r}`);
        const name = known ? wellOf(known, c, r) : photoName([c, r]);
        if (v?.id) {
          (known ? wells : byPhoto)[name] = v.id;
          photoTimes[v.id] = v.photos;
        } else missing.push({ [known ? 'well' : 'photo_position']: name, square_seen: !!v?.seen });
      }
    const order = known ? Object.keys(wells).sort((a, b) => fillOrder(a) - fillOrder(b)) : null;
    // Conflicts by well: the photos disagree there, so the person confirms those wells (rack_report's fill).
    const asWell = name => {
      const m = /^R(\d+)C(\d+)$/.exec(name ?? '');
      return m && known ? wellOf(known, Number(m[2]) - 1, Number(m[1]) - 1) : null;
    };
    const conflicts = (m.conflicts ?? []).map(c => ({
      ...c,
      ...(known ? { wells: [c.photo_position, c.also_at, c.kept_at].map(asWell).filter(Boolean) } : {}),
    }));
    const reading = {
      rack_id: m.rack_id,
      orientation: m.orientation,
      wells: known ? Object.fromEntries(order.map(w => [w, wells[w]])) : null,
      photo_positions: known ? null : byPhoto,
      missing,
      conflicts,
      photos,
    };
    const token = save(dir, user?.id ?? null, 'reading.json', JSON.stringify(reading));
    const status = known ? (m.orientation.basis.some(b => b === 'a1_tube' || b === 'rack_code') ? 'confirmed by the person' : 'proven by the tube IDs') : 'unknown';
    return {
      reading: token,
      rack_id: m.rack_id,
      tubes_read: Object.keys(photoTimes).length,
      orientation: {
        a1: status,
        ...(known ? { a1_photo_position: m.orientation.a1_photo_position, rack_code_beside: m.orientation.rack_code_beside, basis: m.orientation.basis } : {}),
        ...(m.orientation.problems.length ? { problems: m.orientation.problems } : {}),
        ...(m.orientation.note ? { ask: m.orientation.note } : {}),
      },
      ...(known ? { wells: reading.wells } : { photo_positions: byPhoto, photo_position_note: 'R1C1: the tube beside the rack code when it was read, else the top-left tube of the first photo.' }),
      missing,
      tubes_seen_in_one_photo_only: Object.entries(photoTimes).filter(([, n]) => n === 1).length,
      conflicts,
      photos,
      ...(m.notes?.length ? { notes: m.notes } : {}),
    };
  }

  function loadReading(token, user) {
    const f = reportFile(dir, String(token ?? ''), 'reading.json', user);
    return f ? JSON.parse(readFileSync(f.path, 'utf8')) : null;
  }

  function report(args, user) {
    if (!dir) return { error: 'This app has no folder for rack reports (rackReportsDir).' };
    const racks = Array.isArray(args.racks) ? args.racks.slice(0, 20) : [];
    if (!racks.length) return { error: 'Give `racks`: one entry per rack with its `reading` from rack_read_photos.' };
    const responsable = text(args.responsable);
    if (!responsable) return { error: 'Give `responsable`: the person depositing the samples.' };
    const sheets = [];
    const summary = [];
    const problems = [];
    const names = new Set();
    for (const [k, r] of racks.entries()) {
      const reading = loadReading(r.reading, user);
      if (!reading) {
        problems.push(`Rack ${k + 1}: reading ${r.reading} not found (read the photos again with rack_read_photos).`);
        continue;
      }
      if (!reading.wells) {
        problems.push(`Rack ${k + 1} (${reading.rack_id ?? 'code not read'}): A1 is not known. ${reading.orientation?.note ?? ''}`.trim());
        continue;
      }
      const wells = new Map(Object.entries(reading.wells));
      const fill = r.fill && typeof r.fill === 'object' ? r.fill : {};
      const empty = new Set((Array.isArray(r.empty) ? r.empty : []).map(wellName).filter(Boolean));
      // Wells the photos disagree on: confirmed by the person (fill or empty), the read value is not trusted.
      const disputed = new Set((reading.conflicts ?? []).flatMap(c => c.wells ?? []));
      for (const w of disputed) wells.delete(w);
      for (const [w, id] of Object.entries(fill)) {
        const well = wellName(w);
        const tube = upper(id);
        if (!well || !TUBE.test(tube)) problems.push(`Rack ${k + 1}: fill ${w} = ${id} is not a well and a tube ID.`);
        else if (wells.has(well) && wells.get(well) !== tube) problems.push(`Rack ${k + 1}: ${well} was read as ${wells.get(well)}, not ${tube}.`);
        else wells.set(well, tube);
      }
      for (const w of empty) wells.delete(w);
      const unsure = [...disputed].filter(w => !wells.has(w) && !empty.has(w));
      if (unsure.length) problems.push(`Rack ${k + 1}: the photos disagree on ${unsure.join(', ')}: ask the person which tube is there (fill) or whether it is empty.`);
      const open = WELLS.filter(w => !wells.has(w) && !empty.has(w) && !disputed.has(w));
      if (open.length) problems.push(`Rack ${k + 1} (${reading.rack_id ?? 'code not read'}): no tube for ${open.join(', ')}: give each in \`fill\` (its side label) or in \`empty\`.`);
      const twice = [...wells.values()].filter((id, n, all) => all.indexOf(id) !== n);
      if (twice.length) problems.push(`Rack ${k + 1}: ${[...new Set(twice)].join(', ')} on two wells.`);
      const rack = reading.rack_id ?? '';
      let name = `Caja ${rack || k + 1} (8x12)`.slice(0, 31);
      for (let n = 2; names.has(name); n++) name = `Caja ${rack || k + 1}-${n} (8x12)`.slice(0, 31);
      names.add(name);
      sheets.push({
        name,
        xml: sheetXml({
          responsable,
          ultracongelador: text(args.ultracongelador) || 'LBI062',
          estanteria: text(r.estanteria),
          rack,
          fila: text(r.fila_de_rack),
          caja: text(r.caja) || String(k + 1),
          wells,
        }),
      });
      summary.push({ sheet: name, rack_id: rack || null, tubes: wells.size, empty: [...empty], filled_by_hand: Object.keys(fill).map(wellName).filter(Boolean), a1: reading.orientation?.basis ?? [] });
    }
    if (problems.length) return { error: 'Not made yet.', problems };
    const day = new Date().toISOString().slice(0, 10);
    const file = `Anexo1-deposito-${day}.xlsx`;
    const token = save(dir, user?.id ?? null, file, workbook(sheets));
    const url = `${base}/api/rack-reports/${token}/${file}`;
    return { file, download: url, markdown: `[${file}](${url})`, racks: summary, note: 'The link opens for the person who asked (signed in to the app) and admins.' };
  }

  function lookup(args, user) {
    let ids = Array.isArray(args.tubes) ? args.tubes : typeof args.tubes === 'string' ? args.tubes.split(/[\s,;]+/) : [];
    if (args.reading) {
      const reading = loadReading(args.reading, user);
      if (!reading) return { error: `Reading ${args.reading} not found.` };
      ids = [...ids, ...Object.values(reading.wells ?? reading.photo_positions ?? {})];
    }
    ids = [...new Set(ids.map(upper).filter(Boolean))].slice(0, 500);
    if (!ids.length) return { error: 'Give `tubes` (IDs) or a `reading`.' };
    const rows = store.db
      .prepare(
        `SELECT DISTINCT r.sheet, r.row_num, r.values_json FROM records r, json_each(r.values_json) j
         WHERE r.missing = 0 AND j.type = 'text' AND upper(trim(j.value)) IN (SELECT value FROM json_each(?))`,
      )
      .all(JSON.stringify(ids));
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

  return {
    async run(name, args = {}, user = null) {
      try {
        if (name === 'rack_read_photos') return await readPhotos(args, user);
        if (name === 'rack_report') return report(args, user);
        if (name === 'tubes_lookup') return lookup(args, user);
      } catch (e) {
        return { error: e.message };
      }
      return { error: `Unknown tool ${name}` };
    },
  };
}
