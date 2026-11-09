import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { parseDateText } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { signIn, until } from './helpers/assistant.mjs';

// Collection_data's DATE_OF_COLLECTION and DATE_OF_PRESERVATION are formulas of the Sanger manifest block,
// not the dates the team recorded: for a row dated before 14 Apr 2023 whose genus is not in Lists'
// Genera_0176, they give Collection_date and Preservation_date plus 451 days. Río Pusuno as the sheet has it
// (9 Nov 2026): collected on 21 Jan and 18 Jul 2022; DATE_OF_COLLECTION shows 17 Apr and 12 Oct 2023 for
// the genera outside that list. Every tool that shows those columns says which column holds the date.

const d = text => parseDateText(text);
const pusuno = (row, CAM_ID, Genus, collected, manifest) => ({
  row,
  values: {
    CAM_ID,
    Genus,
    SPECIES: `${Genus} sp.`,
    Collection_location: 'Rio Pusuno',
    Collection_date: d(collected),
    Preservation_date: d(collected),
    DATE_OF_COLLECTION: d(manifest),
    DATE_OF_PRESERVATION: d(manifest),
  },
});
const SEED = {
  Collection_data: [
    pusuno(2, 'CAM070476', 'Mechanitis', '2022-01-21', '2022-01-21'),
    pusuno(3, 'CAM070477', 'Ithomia', '2022-01-21', '2023-04-17'),
    pusuno(4, 'CAM070541', 'Oleria', '2022-07-18', '2022-07-18'),
    pusuno(5, 'CAM070542', 'Tithorea', '2022-07-18', '2023-10-12'),
  ],
};

async function setup() {
  const dir = mkdtempSync(join(tmpdir(), 'manifest-dates-'));
  const store = new Store({ localMode: true, databasePath: join(dir, 'app.sqlite') }, { sheets: new LocalSheets(structuredClone(SEED)) });
  await store.sync();
  const assistant = createAssistant({
    store,
    config: { sheetsCopyPath: join(dir, 'sheets.sqlite'), sheetsCopy: { startMs: 0, delayMs: 50, log: { log() {}, error: console.error } } },
  });
  const ana = signIn(store, assistant, { id: 'u-ana', username: 'ana', role: 'editor' });
  const query = async sql => {
    const out = await ana.result('query', { sql });
    const { text } = out.content[0];
    return out.isError ? JSON.parse(text) : text;
  };
  // The copy is built a moment after the app starts.
  await until(async () => !String((await query('SELECT count(*) FROM Collection_data')).error ?? '').includes('not built'), 15000);
  const close = () => {
    assistant.close();
    store.close();
    rmSync(dir, { recursive: true, force: true });
  };
  return { ana, query, close };
}

const SAYS_COLLECTION_DATE = /DATE_OF_COLLECTION[^\n]*not the date[^\n]*Collection_date/;

test('query: a result with DATE_OF_COLLECTION says that Collection_date is the collection date', async () => {
  const { query, close } = await setup();
  try {
    // The statement a chat ran for "en qué fechas se ha colectado en Río Pusuno".
    const text = await query(
      "SELECT Collection_location, DATE_OF_COLLECTION, COUNT(*) n FROM Collection_data WHERE fold(Collection_location) LIKE '%pusuno%' GROUP BY 1,2 ORDER BY 2",
    );
    assert.match(text, SAYS_COLLECTION_DATE, text);
    assert.match(text, /451 days/);
    // The rows are still the sheet's, unchanged.
    assert.match(text, /Rio Pusuno\t2023-10-12\t1/);
    // SELECT * shows the column too.
    assert.match(await query('SELECT * FROM Collection_data LIMIT 1'), SAYS_COLLECTION_DATE);
    assert.match(await query('SELECT max(DATE_OF_PRESERVATION) FROM Collection_data'), /DATE_OF_PRESERVATION[^\n]*Preservation_date/);

    // The dates as recorded: two, and no note.
    const recorded = await query(
      "SELECT Collection_date, COUNT(*) n FROM Collection_data WHERE fold(Collection_location) LIKE '%pusuno%' GROUP BY 1 ORDER BY 1",
    );
    assert.deepEqual(recorded.split('\n').slice(0, 3), ['Collection_date\tn', '2022-01-21\t2', '2022-07-18\t2']);
    assert.doesNotMatch(recorded, /DATE_OF_COLLECTION/);
  } finally {
    close();
  }
});

test('query: a date column not found offers the recorded dates first, the manifest ones marked as such', async () => {
  const { query, close } = await setup();
  try {
    const wrong = await query("SELECT MIN(Date), MAX(Date) FROM Collection_data WHERE fold(Collection_location) LIKE '%pusuno%'");
    assert.match(wrong.error, /no such column: Date/);
    assert.equal(wrong.near[0], 'Collection_data.Collection_date');
    const manifest = wrong.near.filter(n => /DATE_OF_/.test(n));
    assert.ok(manifest.length, wrong.near.join(' | '));
    for (const name of manifest) assert.match(name, /\(manifest formula, not the recorded date: [A-Za-z_]+_date\)$/, name);
    const firstManifest = wrong.near.findIndex(n => /DATE_OF_/.test(n));
    assert.ok(wrong.near.slice(firstManifest).every(n => /DATE_OF_/.test(n)), `recorded dates before manifest ones: ${wrong.near.join(' | ')}`);
  } finally {
    close();
  }
});

test('find_records, get_record and describe_sheet: DATE_OF_COLLECTION comes with the same note', async () => {
  const { ana, close } = await setup();
  try {
    const found = await ana.call('find_records', { module: 'Collection_data', values: ['CAM070542'], field: 'CAM_ID', fields: ['Collection_date', 'DATE_OF_COLLECTION'] });
    assert.deepEqual(found.rows[0].slice(3), ['2022-07-18', '2023-10-12']);
    assert.match(found.dateNote, SAYS_COLLECTION_DATE);
    // Without the column: no note.
    const plain = await ana.call('find_records', { module: 'Collection_data', values: ['CAM070542'], field: 'CAM_ID', fields: ['Collection_date'] });
    assert.equal(plain.dateNote, undefined);

    const record = await ana.call('get_record', { sheet: 'Collection_data', id: 'CAM070542' });
    assert.equal(record.values.Collection_date, '2022-07-18');
    assert.match(record.dateNote, SAYS_COLLECTION_DATE);

    const sheet = await ana.call('describe_sheet', { module: 'Collection_data', columns: ['Collection_date', 'DATE_OF_COLLECTION'] });
    assert.match(sheet.dateNote, SAYS_COLLECTION_DATE);
  } finally {
    close();
  }
});
