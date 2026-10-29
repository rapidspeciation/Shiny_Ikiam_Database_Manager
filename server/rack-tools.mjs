// The assistant's tools for tube racks (docs/assistant-tools-rack-reader.md):
//   rack_read_photos  photos of a rack's bottom (chat attachments) → its code, the tubes on A1..H12, what is missing;
//                     server/assistant.mjs then puts the rack into the chat's deposit proposal (server/rack-deposit.mjs)
//   rack_deposit      (server/assistant.mjs) the deposit's fields, wells and corner changed; the rows follow
//   rack_report       the deposit as the LBI's deposit sheet (Anexo1: one sheet per rack), a download link, and on
//                     request a copy on the project account's Drive
//   tubes_lookup      tube IDs → the workbook's rows (CAM, species, tissue, medium), and the IDs it does not have
// Nothing here writes the workbook (SELECT only). Readings and reports are files of the app's own
// (<rackReportsDir>, beside the database by default), each kept with the person who made it; a report is served
// at /api/rack-reports/<token>/<file> to that person (or an admin) signed in to the app, and copied, when asked, to
// the project account's Drive (folder «Depósitos LBI (racks)»: config.rackDriveFolder, else found or made in
// config.rackDriveParent, else in its My Drive).

import { randomBytes } from 'node:crypto';
import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { crc32, deflateRawSync } from 'node:zlib';
import { driveWriterFor } from './drive.mjs';
import { attachmentFile } from './proposal-photos.mjs';
import { FREEZER, depositProblems, depositState, lookupTubes, rackWells, sheetWells } from './rack-deposit.mjs';
import { COLS, ROWS, ROW_LETTERS, WELLS, fillOrder, loadPhoto, mergeReadings, photoName, readOne, wellOf } from './rack-reader.mjs';

export const MAX_PHOTOS = 6;
/** The folder of the deposit sheets on the project account's Drive. */
export const DRIVE_FOLDER = 'Depósitos LBI (racks)';
const XLSX = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet';
const EDITORS = ['editor', 'reviewer', 'admin'];
const TOKEN = /^[a-f0-9]{24}$/;
const FILE = /^[\w.-]{1,120}\.xlsx$/;

export const RACK_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'rack_read_photos',
      description: [
        "Reads a 96-tube rack (8 × 12, FluidX-style tubes) from photos of its bottom: every tube's 2D code, the rack's own code, and where each tube is (A1..H12). The rack goes at once into this chat's deposit proposal in «Cambios propuestos» (one section per rack, its 8 × 12 grid, the deposit sheet's fields and the tube-locations rows it implies); nothing is written until the person presses «Aplicar».",
        '- `photos`: 1 to 6 chat attachments (file names from "[Attached image … saved at …]"), the same rack from slightly different angles: the photos are merged by tube ID, so a tube hidden by glare in one is read in another.',
        "- A1: a photo of the bottom is a mirror of the rack seen from above. The rack's printed code (the square beside a corner) places it: by default the code sits beside A12, as on the team's racks (FK00274230). That is an assumption: `orientation.assumed`; tell the person and ask them to confirm (seen from above, the code beside A12?). The photos allow two wells for it (the deposit's `code_options`, A12 or H1 on FK00274230): if the person says the other, rack_deposit with that `code_beside` turns the rack. Tubes go into racks in any order, so their IDs seldom tell; when they do follow a sequence it only checks the corner (`orientation.checked_by_ids`).",
        "- No code read in any photo: ask for a photo where the code shows, or the ID of the tube in A1 (`a1_tube`) or at another corner (`corner_tube` {well, id}), then call again.",
        'Returns `reading`, the rack ID, `orientation`, `missing` wells (tube not read: ask for another photo, or the ID from its side label, or whether it is empty: rack_deposit `fill` / `empty`), conflicts, and `deposit`: the proposal, its link and what it still needs (`ask`).',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          photos: { type: 'array', items: { type: 'string' } },
          a1_tube: { type: 'string' },
          corner_tube: {
            type: 'object',
            description: 'A tube the person names at a corner well, when the A1 tube was not read.',
            properties: { well: { type: 'string', enum: ['A12', 'H1', 'H12', 'A1'] }, id: { type: 'string' } },
            required: ['well', 'id'],
          },
          rack_code_beside: { type: 'string', enum: ['A12', 'H12'] },
        },
        required: ['photos'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'rack_deposit',
      description: [
        "Changes a rack deposit of this chat (the proposal rack_read_photos made, «Cambios propuestos»): the deposit sheet's fields, the tubes of wells the photos did not read, which corner the rack's code is beside. The grid and the tube-locations rows follow at once; the person can change the same in the sidebar.",
        '- `responsable` (default: the person signed in), `ultracongelador` (default LBI062), `batch` (the sheet\'s Batch for new rows, only when the person gives one).',
        '- `racks`: per rack (`rack`: its code, or its number in the deposit from 1): `estanteria`, `fila_de_rack`, `caja`; `fill` {well: tube ID} (from the side label); `empty` [wells]; `code_beside` (one of the rack\'s `code_options`: the person confirming the corner, or turning the rack); `rack_id` when the code was not read; `remove: true` drops the rack.',
        "Answers what it still needs (`ask`), the tubes no sheet has (`need_rows`: they need their rows, never invented), the sheet's tubes on the rack that the photos do not show there (`elsewhere`), and `protected` when the app's Google account cannot write the sheet («Aplicar» is then refused until its owner allows it).",
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          proposal: { type: 'string', description: "The deposit's proposalId (default: this chat's pending deposit)." },
          responsable: { type: 'string' },
          ultracongelador: { type: 'string' },
          batch: { type: 'string' },
          racks: {
            type: 'array',
            items: {
              type: 'object',
              properties: {
                rack: { type: 'string' },
                estanteria: { type: 'string' },
                fila_de_rack: { type: 'string' },
                caja: { type: 'string' },
                fill: { type: 'object', additionalProperties: { type: 'string' } },
                empty: { type: 'array', items: { type: 'string' } },
                code_beside: { type: 'string', enum: ['A1', 'A12', 'H1', 'H12'] },
                rack_id: { type: 'string' },
                remove: { type: 'boolean' },
              },
              required: ['rack'],
            },
          },
        },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'rack_report',
      description: [
        "The deposit sheet of the LBI freezer (Alex's Anexo1) for this chat's rack deposit: an .xlsx with one sheet per rack, «Caja <rack code> (8x12)», the tubes on their wells A1..H12, and its download link (`markdown`: give it as it is). The sidebar has the same as «Descargar .xlsx».",
        "- `drive: true` (only when the person asks for it, or presses «Guardar en Google Drive» in the sidebar): a copy in the project's Drive folder «Depósitos LBI (racks)», `drive.markdown`. `drive.error`: Drive did not take it; the download still works, say so.",
        '- `proposal`: the deposit (default: this chat\'s pending one).',
        'Refused while a well of a rack is neither read, filled nor said empty, or a field is missing: ask the person.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: { proposal: { type: 'string' }, drive: { type: 'boolean' } },
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
const upper = v => text(v).toUpperCase();

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
  if (!dir || !TOKEN.test(token ?? '') || !(FILE.test(name ?? '') || name === 'reading.json' || name === 'report.json')) return null;
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

/** What the answer says of how A1 was placed. */
function orientationStatus(o) {
  if (!o.mapping) return 'unknown';
  if (o.basis.some(b => b === 'a1_tube' || b === 'corner_tube' || b === 'rack_code')) return 'confirmed by the person';
  return `assumed: the rack code beside ${o.rack_code_beside ?? 'A12'} (the team's racks)${o.checked_by_ids ? ', and the tubes\' IDs agree' : ''}: confirm with the person`;
}

/**
 * The tools for one app. `store`: the app's database (SELECT only); config: `rackReportsDir`, `publicUrl`,
 * `t3.home` (the chat attachments), the Drive copy's `rackDriveFolder` / `rackDriveParent` and the project
 * account (`driveCredentialsFile`, server/drive.mjs; tests: `drive`); `readPhoto` (tests) reads a file into readOne's result.
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
    const corner = args.corner_tube && typeof args.corner_tube === 'object' ? [{ well: upper(args.corner_tube.well), id: upper(args.corner_tube.id) }] : [];
    const m = mergeReadings(readings, { a1Tube: a1, codeAt: args.rack_code_beside ?? null, cornerTubes: corner });
    const known = m.orientation.mapping;
    const wells = {};
    const byPhoto = {};
    const missing = [];
    const photoTimes = {};
    const frameCells = {};
    const seen = [];
    for (let r = 0; r < (m.frame ? ROWS : 0); r++)
      for (let c = 0; c < COLS; c++) {
        const v = m.cells?.get?.(`${c},${r}`);
        const name = known ? wellOf(known, c, r) : photoName([c, r]);
        if (v?.id) {
          (known ? wells : byPhoto)[name] = v.id;
          frameCells[`${c},${r}`] = v.id;
          photoTimes[v.id] = v.photos;
        } else {
          missing.push({ [known ? 'well' : 'photo_position']: name, square_seen: !!v?.seen });
          if (v?.seen) seen.push(`${c},${r}`);
        }
      }
    const order = known ? Object.keys(wells).sort((a, b) => fillOrder(a) - fillOrder(b)) : null;
    // Conflicts by well: the photos disagree there, so the person confirms those wells (rack_deposit's fill).
    const asWell = name => {
      const m = /^R(\d+)C(\d+)$/.exec(name ?? '');
      return m && known ? wellOf(known, Number(m[2]) - 1, Number(m[1]) - 1) : null;
    };
    const conflicts = (m.conflicts ?? []).map(c => ({
      ...c,
      ...(known ? { wells: [c.photo_position, c.also_at, c.kept_at].map(asWell).filter(Boolean) } : {}),
    }));
    // Kept as photo positions too, so the rack can be turned (its code beside H12, not A12) without reading it again.
    const reading = {
      rack_id: m.rack_id,
      orientation: m.orientation,
      mapping: known,
      code_corner: m.frame?.code_corner ?? null,
      frame_cells: m.frame ? frameCells : null,
      seen,
      wells: known ? Object.fromEntries(order.map(w => [w, wells[w]])) : null,
      photo_positions: known ? null : byPhoto,
      missing,
      conflicts,
      photos,
    };
    const token = save(dir, user?.id ?? null, 'reading.json', JSON.stringify(reading));
    return {
      reading: token,
      rack_id: m.rack_id,
      tubes_read: Object.keys(photoTimes).length,
      orientation: {
        a1: orientationStatus(m.orientation),
        ...(known
          ? {
              a1_photo_position: m.orientation.a1_photo_position,
              rack_code_beside: m.orientation.rack_code_beside,
              basis: m.orientation.basis,
              assumed: m.orientation.assumed,
              checked_by_ids: m.orientation.checked_by_ids,
            }
          : {}),
        ...(m.orientation.problems.length ? { problems: m.orientation.problems } : {}),
        ...(m.orientation.not_read?.length ? { not_read: m.orientation.not_read } : {}),
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

  /** A reading of `user`'s (null: not found or someone else's). */
  function loadReading(token, user) {
    const f = reportFile(dir, String(token ?? ''), 'reading.json', user);
    return f ? JSON.parse(readFileSync(f.path, 'utf8')) : null;
  }

  /** The deposit (depositState's) as the Anexo1 .xlsx, kept for `user`: { file, token, download, markdown } or { error, problems }. */
  function depositReport(deposit, user, { fields = true } = {}) {
    if (!dir) return { error: 'This app has no folder for rack reports (rackReportsDir).' };
    const problems = depositProblems(deposit, { fields });
    if (!deposit.racks?.length) problems.push('No rack in this deposit yet: read its photos (rack_read_photos).');
    if (problems.length) return { error: 'Not made yet.', problems };
    const names = new Set();
    const sheets = deposit.racks.map((r, k) => {
      const rack = r.rack_id ?? '';
      let name = `Caja ${rack || k + 1} (8x12)`.slice(0, 31);
      for (let n = 2; names.has(name); n++) name = `Caja ${rack || k + 1}-${n} (8x12)`.slice(0, 31);
      names.add(name);
      return {
        name,
        xml: sheetXml({
          responsable: text(deposit.responsable),
          ultracongelador: text(deposit.ultracongelador) || FREEZER,
          estanteria: text(r.estanteria),
          rack,
          fila: text(r.fila_de_rack),
          caja: text(r.caja),
          wells: sheetWells(r),
        }),
      };
    });
    const codes = deposit.racks.map(r => r.rack_id).filter(Boolean);
    const day = new Date().toISOString().slice(0, 10);
    const file = `Anexo1-deposito-${day}${codes.length && codes.length <= 3 ? `-${codes.join('-')}` : ''}.xlsx`;
    const token = save(dir, user?.id ?? null, file, workbook(sheets));
    const url = `${base}/api/rack-reports/${token}/${file}`;
    return { file, token, download: url, markdown: `[${file}](${url})`, sheets: sheets.map(s => s.name), note: 'The link opens for the person who asked (signed in to the app) and admins.' };
  }

  /** The legacy call: racks given as readings with their fields (before the deposit proposal). */
  function report(args, user) {
    const racks = Array.isArray(args.racks) ? args.racks.slice(0, 20) : [];
    if (!racks.length) return { error: 'Give `racks`: one entry per rack with its `reading` from rack_read_photos.' };
    const deposit = { responsable: text(args.responsable), ultracongelador: text(args.ultracongelador) || FREEZER, racks: [] };
    for (const r of racks) {
      const reading = loadReading(r.reading, user);
      const placed = reading ? rackWells(reading, r) : null;
      deposit.racks.push({
        ...r,
        rack_id: reading?.rack_id ?? null,
        wells: Object.fromEntries([...(placed?.wells ?? [])].map(([w, t]) => [w, { t }])),
        missing: placed?.missing ?? [...WELLS],
        disputed: placed?.disputed ?? [],
        problems: !reading ? [`reading ${r.reading} not found (read the photos again with rack_read_photos).`] : placed.problems,
      });
    }
    // The call before the deposit proposal: its fields were asked for by the assistant, not required here.
    return depositReport(deposit, user, { fields: false });
  }

  // ------------------------------------------------------------ the copy on Drive
  let driveFolder = null;
  /** The folder «Depósitos LBI (racks)»: the one configured, else found or made in the configured parent (else My Drive). */
  async function folderOnDrive(drive) {
    if (config.rackDriveFolder) return config.rackDriveFolder;
    if (driveFolder) return driveFolder;
    const parent = config.rackDriveParent || 'root';
    const found = (await drive.children(parent, { foldersOnly: true })).find(f => f.name === DRIVE_FOLDER);
    driveFolder = (found ?? (await drive.createFolder(DRIVE_FOLDER, parent))).id;
    return driveFolder;
  }
  /**
   * A kept report (its token and file) copied to the project's Drive: { file, link, markdown, folder } or
   * { error } (Drive not set up or not answering: the download link still works).
   */
  async function copyToDrive(token, file, user) {
    if (!EDITORS.includes(user?.role)) return { error: 'Only editors copy deposit sheets to the project Drive.' };
    const kept = reportFile(dir, token, file, user);
    if (!kept) return { error: 'That report is not kept here any more: make it again.' };
    const drive = driveWriterFor(store, config);
    if (!drive?.upload) return { error: 'This app has no Google account for Drive set up: the download link still works.' };
    try {
      const folder = await folderOnDrive(drive);
      const made = await drive.upload(file, readFileSync(kept.path), XLSX, folder);
      const link = made.webViewLink ?? `https://drive.google.com/file/d/${made.id}/view`;
      return { file, id: made.id, link, markdown: `[${file} en Drive](${link})`, folder: `https://drive.google.com/drive/folders/${folder}` };
    } catch (e) {
      driveFolder = null;
      return { error: `Google Drive did not take the copy (${String(e?.message || e).slice(0, 160)}): the download link still works.` };
    }
  }

  return {
    async run(name, args = {}, user = null) {
      try {
        if (name === 'rack_read_photos') return await readPhotos(args, user);
        if (name === 'rack_report') return report(args, user);
        if (name === 'tubes_lookup') {
          let ids = Array.isArray(args.tubes) ? args.tubes : typeof args.tubes === 'string' ? args.tubes.split(/[\s,;]+/) : [];
          if (args.reading) {
            const reading = loadReading(args.reading, user);
            if (!reading) return { error: `Reading ${args.reading} not found.` };
            ids = [...ids, ...Object.values(reading.wells ?? reading.photo_positions ?? {})];
          }
          if (!ids.length) return { error: 'Give `tubes` (IDs) or a `reading`.' };
          return lookupTubes(store, ids.slice(0, 500));
        }
      } catch (e) {
        return { error: e.message };
      }
      return { error: `Unknown tool ${name}` };
    },
    /** The deposit as the table shows it and the changes it implies (rack-deposit.mjs), its readings read as `owner`'s. */
    state: (deposit, owner) => depositState(store, deposit, token => loadReading(token, owner)),
    loadReading,
    depositReport,
    copyToDrive,
  };
}

export { wellName } from './rack-deposit.mjs';
