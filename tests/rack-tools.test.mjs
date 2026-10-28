import test from 'node:test';
import assert from 'node:assert/strict';
import { copyFileSync, mkdirSync, mkdtempSync, readFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { inflateRawSync } from 'node:zlib';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { mcpTools } from '../server/assistant.mjs';
import { loadPhoto, mergeReadings, readOne, wellOf } from '../server/rack-reader.mjs';
import { RACK_TOOL_NAMES, createRackTools, reportFile, wellName } from '../server/rack-tools.mjs';

const RACK_PHOTO = new URL('./fixtures/rack-bottom-fk00274230.jpg', import.meta.url).pathname;
const TUBE_PHOTO = new URL('./fixtures/tube-bottom-fs50849233.jpg', import.meta.url).pathname;
// The rack photographed on 28 Oct: pre-loaded tubes FS50849233 (A1) … FS50849328 (H12), column by column.
const expected = well => `FS${50849233 + (Number(well.slice(1)) - 1) * 8 + 'ABCDEFGH'.indexOf(well[0])}`;

/** The entries of an .xlsx (our own zip: deflated, sizes in the local headers). */
function unzip(buf) {
  const out = {};
  let at = 0;
  while (buf.readUInt32LE(at) === 0x04034b50) {
    const size = buf.readUInt32LE(at + 18);
    const nameLength = buf.readUInt16LE(at + 26);
    const name = buf.subarray(at + 30, at + 30 + nameLength).toString();
    const start = at + 30 + nameLength;
    out[name] = inflateRawSync(buf.subarray(start, start + size)).toString();
    at = start + size;
  }
  return out;
}
const cellsOf = sheet => Object.fromEntries([...sheet.matchAll(/<c r="([A-Z]+\d+)"[^>]*?(?:\/>|>(?:<v>([^<]*)<\/v>|<is><t>([^<]*)<\/t><\/is>)<\/c>)/g)].map(m => [m[1], m[2] ?? m[3] ?? null]));

const seed = {
  Collection_data: [
    { row: 2, values: { CAM_ID: 'CAM079877', SPECIES: 'Ithomia salapia', Sex: 'female', Tube_1_id: 'FS50849233', Tube_1_tissue: 'WHOLE_ORGANISM', Preservation_medium: 'Flash frozen' } },
  ],
  Insectary_data: [
    { row: 2, values: { CAM_ID: 'CAM080001', SPECIES: 'Mechanitis menapis', Tube_1_id: 'FS50849234', Tube_1_tissue: '**OTHER_SOMATIC_ANIMAL_TISSUE** | WING CLIP', T1_Preservation_medium: 'Ethanol', Preservation_medium: 'NOT_COLLECTED' } },
  ],
};

async function setup({ readPhoto } = {}) {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets(seed) });
  await store.sync({ sheets: Object.keys(seed) });
  const home = mkdtempSync(join(tmpdir(), 'rack-t3-'));
  mkdirSync(join(home, 'userdata', 'attachments'), { recursive: true });
  copyFileSync(RACK_PHOTO, join(home, 'userdata', 'attachments', 'thread-1-rack.jpg'));
  const rackReportsDir = mkdtempSync(join(tmpdir(), 'rack-reports-'));
  const config = { t3: { home }, rackReportsDir, publicUrl: 'https://app.test' };
  return { store, config, tools: createRackTools({ store, config, ...(readPhoto ? { readPhoto } : {}) }), rackReportsDir };
}
const franz = { id: 'u1', username: 'franz', role: 'editor' };

test('the rack tools are offered to the chats', () => {
  const names = new Set(mcpTools().map(t => t.name));
  for (const n of RACK_TOOL_NAMES) assert.ok(names.has(n), n);
});

test("Franz's rack photo: at least 92 of 96 tubes, every one where its ID says, the rack code, A1 from the IDs", async () => {
  const one = await readOne(await loadPhoto(RACK_PHOTO));
  assert.equal(one.rack?.text, 'FK00274230');
  const m = mergeReadings([one]);
  assert.equal(m.orientation.mapping, 'm1');
  assert.deepEqual(m.orientation.basis, ['tube_sequence']);
  assert.equal(m.orientation.rack_code_beside, 'A12');
  const wells = [...m.cells].filter(([, v]) => v.id).map(([k, v]) => [wellOf('m1', ...k.split(',').map(Number)), v.id]);
  assert.ok(wells.length >= 92, `${wells.length} read`);
  for (const [well, id] of wells) assert.equal(id, expected(well), well);
});

test('a photo of one tube: its code, without a grid', async () => {
  const one = await readOne(await loadPhoto(TUBE_PHOTO));
  assert.equal(one.lattice, false);
  assert.ok(one.loose.includes('FS50849233'));
});

test('rack_read_photos → rack_report: the Anexo1 sheet, refused while a well is open, served to its maker', async () => {
  const { tools, rackReportsDir } = await setup();
  const read = await tools.run('rack_read_photos', { photos: ['/somewhere/thread-1-rack.jpg'] }, franz);
  assert.equal(read.rack_id, 'FK00274230');
  assert.equal(read.orientation.a1, 'proven by the tube IDs');
  assert.ok(read.tubes_read >= 92);
  for (const [well, id] of Object.entries(read.wells)) assert.equal(id, expected(well));
  const open = read.missing.map(m => m.well);
  const refused = tools.run('rack_report', { responsable: 'Franz Chandi', racks: [{ reading: read.reading, estanteria: '2', fila_de_rack: '1', caja: '1' }] }, franz);
  assert.match((await refused).problems[0], new RegExp(open[0]));
  const fill = Object.fromEntries(open.map(w => [w, expected(w)]));
  const made = await tools.run('rack_report', { responsable: 'Franz Chandi', racks: [{ reading: read.reading, estanteria: '2', fila_de_rack: '1', caja: '1', fill }] }, franz);
  assert.equal(made.racks[0].sheet, 'Caja FK00274230 (8x12)');
  assert.equal(made.racks[0].tubes, 96);
  const [, token, file] = /\/api\/rack-reports\/([a-f0-9]{24})\/(.+)$/.exec(made.download);
  assert.equal(reportFile(rackReportsDir, token, file, { id: 'someone else', role: 'editor' }), null);
  assert.equal(reportFile(rackReportsDir, token, file, { id: 'admin', role: 'admin' })?.name, file);
  const xlsx = unzip(readFileSync(reportFile(rackReportsDir, token, file, franz).path));
  assert.match(xlsx['xl/workbook.xml'], /<sheet name="Caja FK00274230 \(8x12\)"/);
  const sheet = xlsx['xl/worksheets/sheet1.xml'];
  const c = cellsOf(sheet);
  assert.equal(c.A1, 'Responsable: Franz Chandi');
  assert.equal(c.G1, 'Diagrama de posición de tubos por caja');
  assert.deepEqual([c.A2, c.B2, c.C2, c.D2, c.E2, c.G2, c.R2], ['Ultracongelador', 'Estanteria', 'Rack', 'Fila de rack', 'Nro. Caja', '1', '12']);
  assert.deepEqual([c.A3, c.B3, c.C3, c.D3, c.E3, c.F3, c.F10], ['LBI062', '2', 'FK00274230', '1', '1', 'A', 'H']);
  assert.equal(c.G3, 'FS50849233'); // A1
  assert.equal(c.R3, 'FS50849321'); // A12
  assert.equal(c.G10, 'FS50849240'); // H1
  assert.equal(c.R10, 'FS50849328'); // H12
  for (const m of ['G1:R1', 'A3:A10', 'E3:E10']) assert.match(sheet, new RegExp(`<mergeCell ref="${m}"/>`));
});

test('A1 unknown (tubes not in sequence): positions stay photo positions and the report asks; a1_tube settles it', async () => {
  // A deposit rack filled by hand: IDs in no order. Photo frame (c, r) → the ID; no rack code read.
  const ids = Array.from({ length: 96 }, (_, k) => `FS${63886000 + ((k * 37) % 96) * 3}`);
  const reading = { tubes: new Map(ids.map((id, k) => [`${k % 12},${Math.floor(k / 12)}`, id])), seen: new Set(), rack: null, lattice: true };
  const { tools } = await setup({ readPhoto: async () => reading });
  const read = await tools.run('rack_read_photos', { photos: ['thread-1-rack.jpg'] }, franz);
  assert.equal(read.orientation.a1, 'unknown');
  assert.match(read.orientation.ask, /which tube is in A1/);
  assert.equal(read.photo_positions.R1C1, ids[0]);
  const refused = await tools.run('rack_report', { responsable: 'X', racks: [{ reading: read.reading }] }, franz);
  assert.match(refused.problems[0], /A1 is not known/);
  // The person: "A1 is the tube at the photo's bottom-left" → its ID.
  const a1 = ids[7 * 12];
  const again = await tools.run('rack_read_photos', { photos: ['thread-1-rack.jpg'], a1_tube: a1 }, franz);
  assert.equal(again.orientation.a1, 'confirmed by the person');
  assert.equal(again.wells.A1, a1);
  assert.equal(again.wells.H12, ids[11]);
  // A tube not at a corner a bottom photo allows is refused, not guessed.
  const wrong = await tools.run('rack_read_photos', { photos: ['thread-1-rack.jpg'], a1_tube: ids[5] }, franz);
  assert.equal(wrong.orientation.a1, 'unknown');
  assert.match(wrong.orientation.problems[0], /not a corner/);
});

test('tubes_lookup: rows with CAM, species, tissue and medium; IDs not in the workbook', async () => {
  const { tools } = await setup();
  const out = await tools.run('tubes_lookup', { tubes: ['fs50849233', 'FS50849234', 'FS50849300'] }, franz);
  assert.deepEqual(out.not_found, ['FS50849300']);
  const [a, b] = out.tubes;
  assert.deepEqual([a.rows[0].cam_id, a.rows[0].species, a.rows[0].sample_type], ['CAM079877', 'Ithomia salapia', 'Flash frozen · WHOLE_ORGANISM']);
  assert.equal(b.rows[0].sample_type, 'Ethanol · **OTHER_SOMATIC_ANIMAL_TISSUE** | WING CLIP');
});

test('wells and attachments: names tidied, other files refused', async () => {
  assert.equal(wellName('a01'), 'A1');
  assert.equal(wellName('I1'), null);
  const { tools } = await setup();
  const out = await tools.run('rack_read_photos', { photos: ['../../etc/passwd'] }, franz);
  assert.match(out.error, /Not chat attachments/);
});

// A photo's reading as readOne gives it: frame (c, r) → ID, the 8 × 12 rack at (0, 0)..(11, 7).
const seq = (c, r) => `FS${50849233 + (11 - c) * 8 + r}`; // the photo as Franz's: A1 top right
const readingOf = (cells, rack = null) => ({ tubes: new Map(cells.map(([c, r, id]) => [`${c},${r}`, id ?? seq(c, r)])), seen: new Set(), rack, lattice: true });
const allCells = filter => Array.from({ length: 96 }, (_, k) => [k % 12, Math.floor(k / 12)]).filter(filter ?? (() => true));

test('a partly filled rack without its code: the IDs prove the mirror, not where row A is; no wells', async () => {
  // Rows B..H only (row A empty): shifting would call B "A".
  const { tools } = await setup({ readPhoto: async () => readingOf(allCells(([, r]) => r > 0)) });
  const read = await tools.run('rack_read_photos', { photos: ['thread-1-rack.jpg'] }, franz);
  assert.equal(read.orientation.a1, 'unknown');
  assert.match(read.orientation.problems.join(' '), /not filled edge to edge/);
  assert.equal(read.wells, undefined);
  const refused = await tools.run('rack_report', { responsable: 'X', racks: [{ reading: read.reading }] }, franz);
  assert.match(refused.problems[0], /A1 is not known/);
  // With the rack's code beside the top-left tube (R1C1), its row is the rack's first: the wells are known.
  const coded = await setup({ readPhoto: async () => readingOf(allCells(([, r]) => r > 0), { text: 'FK00274230', at: [-0.95, 0], corner: [0, 0] }) });
  const again = await coded.tools.run('rack_read_photos', { photos: ['thread-1-rack.jpg'] }, franz);
  assert.equal(again.orientation.a1, 'proven by the tube IDs');
  assert.equal(again.wells.B1, 'FS50849234');
  assert.deepEqual(again.missing.map(m => m.well).sort(), Array.from({ length: 12 }, (_, k) => `A${k + 1}`).sort());
});

test('photos without a grid: their codes come back, no error', async () => {
  const { tools } = await setup({ readPhoto: async () => ({ tubes: new Map(), seen: new Set(), rack: null, loose: ['FS50849233'], codes: 1, lattice: false }) });
  const read = await tools.run('rack_read_photos', { photos: ['thread-1-rack.jpg'] }, franz);
  assert.equal(read.error, undefined);
  assert.deepEqual(read.photos[0].codes, ['FS50849233']);
  assert.equal(read.orientation.a1, 'unknown');
});

test('two photos disagreeing on a well: the report waits for the person to say which tube is there', async () => {
  const photos = [readingOf(allCells()), readingOf(allCells().map(([c, r]) => [c, r, c === 0 && r === 0 ? 'FS99999999' : null]))];
  let n = 0;
  const { tools } = await setup({ readPhoto: async () => photos[n++ % 2] });
  const read = await tools.run('rack_read_photos', { photos: ['thread-1-rack.jpg', 'thread-1-rack.jpg'] }, franz);
  assert.deepEqual(read.conflicts.map(c => c.wells).flat(), ['A12']);
  const base = { reading: read.reading };
  const refused = await tools.run('rack_report', { responsable: 'X', racks: [base] }, franz);
  assert.match(refused.problems.join(' '), /disagree on A12/);
  const made = await tools.run('rack_report', { responsable: 'X', racks: [{ ...base, fill: { A12: 'FS50849321' } }] }, franz);
  assert.equal(made.racks[0].tubes, 96);
});
