import { test } from 'node:test';
import assert from 'node:assert/strict';
import { countFrozen, frozenWhyNot, genusOf, sexOf, speciesOf } from '../server/frozen-stock.mjs';

const HEAD = ['Pagyris euryanassa', 'Mechanitis polymnia', 'Mechanitis lysimnia', 'Melinaea menophilus', 'Melinaea mothone', 'Oleria onega'];
const C = values => ({ sheet: 'Collection_data', values: { Release_Collect: 'Collected_Preserved', Preservation_medium: 'Flash frozen', ...values } });
const I = values => ({ sheet: 'Insectary_data', values: { T1_Preservation_medium: 'Flash frozen', Location_body: 'Ikiam', ...values } });

test('speciesOf: subspecies, crosses within a species, crosses of two species, genus-only and higher names', () => {
  assert.equal(speciesOf('Mechanitis polymnia proceriformis'), 'Mechanitis polymnia');
  assert.equal(speciesOf('Mechanitis polymnia werneri x proceriformis'), 'Mechanitis polymnia');
  assert.equal(speciesOf('Mechanitis polymnia werneri X Mechanitis polymnia eurydice'), 'Mechanitis polymnia');
  assert.equal(speciesOf('Melinaea menophilus zaneka VS Melinaea mothone'), null);
  assert.equal(speciesOf('Dircenna sp.'), null);
  assert.equal(speciesOf('Haeterini (tribe)'), null);
  assert.equal(speciesOf('Riodinidae'), null);
  assert.equal(speciesOf(''), null);
  assert.equal(genusOf('Dircenna sp.'), 'Dircenna');
  assert.equal(genusOf('Riodinidae (family)'), null);
  assert.equal(genusOf('Satyrini (tribe)'), null);
});

test('sexOf: only a certain male or female is a sex', () => {
  assert.deepEqual(['male', 'female', ' Female ', 'male ?', 'female_?', 'NOT_COLLECTED', 'NA', ''].map(sexOf), ['M', 'F', 'F', 'unknown', 'unknown', 'unknown', 'unknown', 'unknown']);
});

test('frozenWhyNot: preserved flash frozen adults still in the project', () => {
  assert.equal(frozenWhyNot('Collection_data', C({}).values), null);
  assert.equal(frozenWhyNot('Collection_data', C({ Preservation_medium: 'DMSO buffer & Flash frozen' }).values), null);
  assert.equal(frozenWhyNot('Collection_data', C({ Preservation_medium: 'Ethanol' }).values), 'not_flash_frozen');
  assert.equal(frozenWhyNot('Collection_data', C({ Release_Collect: 'Collected_Sent2Insectary' }).values), 'not_preserved');
  assert.equal(frozenWhyNot('Collection_data', C({ Location_WholeBody: 'Sanger - TOL704 freezer' }).values), null);
  assert.equal(frozenWhyNot('Collection_data', C({ Location_WholeBody: 'Splitted body', Location_abdomen: 'Ikiam' }).values), null);
  assert.equal(frozenWhyNot('Collection_data', C({ Location_WholeBody: 'Splitted body', Location_abdomen: 'Missing' }).values), 'gone');
  assert.equal(frozenWhyNot('Collection_data', C({ Location_abdomen: '[Sent for sequencing]' }).values), 'gone');
  assert.equal(frozenWhyNot('Collection_data', C({ Location_abdomen: 'Cambridge' }).values), 'gone');
  assert.equal(frozenWhyNot('Insectary_data', I({}).values), null);
  assert.equal(frozenWhyNot('Insectary_data', I({ T1_Preservation_medium: 'Ethanol', Preservation_medium: 'Flash frozen' }).values), null);
  assert.equal(frozenWhyNot('Insectary_data', I({ T1_Preservation_medium: 'Ethanol' }).values), 'not_flash_frozen');
  assert.equal(frozenWhyNot('Insectary_data', I({ LIFESTAGE: '3rd instar larva' }).values), 'not_adult');
  assert.equal(frozenWhyNot('Insectary_data', I({ LIFESTAGE: 'Adult' }).values), null);
  assert.equal(frozenWhyNot('Insectary_data', I({ Location_body: 'Lost - see notes' }).values), 'gone');
  assert.equal(frozenWhyNot('F1/F2_MutationRate', {}), 'other_sheet');
});

test('countFrozen: per species and genus, one per CAM, unmapped names reported', () => {
  const rows = [
    C({ CAM_ID: 'CAM001', SPECIES: 'Mechanitis polymnia proceriformis', Sex: 'male' }),
    I({ CAM_ID: 'CAM001', SPECIES: 'Mechanitis polymnia', Sex: 'male' }), // the same butterfly
    I({ CAM_ID: 'CAM002', SPECIES: 'Mechanitis polymnia werneri x proceriformis', Sex: 'female' }),
    I({ CAM_ID: 'CAM003', SPECIES: 'Mechanitis Lysimnia', Sex: 'female ?' }),
    C({ CAM_ID: 'NA', SPECIES: 'Mechanitis lysimnia', Sex: 'female' }),
    C({ CAM_ID: 'NA', SPECIES: 'Mechanitis lysimnia', Sex: 'female' }), // no CAM: each row is one
    C({ CAM_ID: 'CAM004', SPECIES: 'Mechanitis sp.', Sex: 'male' }),
    C({ CAM_ID: 'CAM005', SPECIES: 'Melinaea menophilus zaneka VS Melinaea mothone', Sex: 'male' }),
    C({ CAM_ID: 'CAM006', SPECIES: 'Oleria onega', Sex: 'male', Location_abdomen: 'Missing' }),
    C({ CAM_ID: 'CAM007', SPECIES: 'Oleria onega', Sex: 'male', Preservation_medium: 'Ethanol' }),
  ];
  const out = countFrozen(rows, HEAD);
  assert.deepEqual(out.species, {
    'Mechanitis lysimnia': { M: 0, F: 2, unknown: 1, total: 3 },
    'Mechanitis polymnia': { M: 1, F: 1, unknown: 0, total: 2 },
  });
  assert.deepEqual(out.genus, {
    Mechanitis: { M: 2, F: 3, unknown: 1, total: 6 },
    Melinaea: { M: 1, F: 0, unknown: 0, total: 1 },
  });
  assert.equal(out.counted, 7);
  assert.equal(out.gone, 1);
  assert.deepEqual(out.unmapped, { rows: 2, names: { 'Mechanitis sp.': 1, 'Melinaea menophilus zaneka VS Melinaea mothone': 1 } });
});

test('countFrozen: workbook names the head knows under another name', () => {
  const out = countFrozen([C({ CAM_ID: 'CAM010', SPECIES: 'Placidina euryanassa', Sex: 'female' })], HEAD);
  assert.deepEqual(out.species, { 'Pagyris euryanassa': { M: 0, F: 1, unknown: 0, total: 1 } });
  assert.deepEqual(Object.keys(out.genus), ['Pagyris']);
});

test('GET /api/frozen-stock: signed in only, ETag, 304, a new version after a change that counts', async t => {
  const { mkdtempSync, mkdirSync, rmSync, writeFileSync } = await import('node:fs');
  const { tmpdir } = await import('node:os');
  const { join } = await import('node:path');
  const { createApp } = await import('../server/index.mjs');
  const { LocalSheets } = await import('../server/sheets.mjs');
  const { setPasswordHashCost } = await import('../server/auth.mjs');
  setPasswordHashCost(16);
  const dir = mkdtempSync(join(tmpdir(), 'frozen-stock-'));
  mkdirSync(join(dir, 'identifier', 'offline'), { recursive: true });
  writeFileSync(join(dir, 'identifier', 'offline', 'head_b8127dbf.json'), JSON.stringify({ species: HEAD }));
  const row = (n, values) => ({ row: n, values: { Release_Collect: 'Collected_Preserved', Preservation_medium: 'Flash frozen', ...values } });
  const app = await createApp(
    { databasePath: join(dir, 'app.sqlite'), identifierDir: join(dir, 'identifier'), localMode: true, secureCookies: false, syncIntervalMs: 0, setupToken: 'setup-token-123' },
    {
      sheets: new LocalSheets({
        Collection_data: [
          row(2, { CAM_ID: 'CAM070918', SPECIES: 'Oleria onega', Sex: 'female' }),
          row(3, { CAM_ID: 'CAM070919', SPECIES: 'Oleria onega', Sex: 'male' }),
        ],
      }),
      skipInitialSync: true,
    },
  );
  t.after(async () => {
    await app.close();
    rmSync(dir, { recursive: true, force: true });
  });
  await app.store.sync({ sheets: ['Collection_data'] });
  const address = await app.listen(0, '127.0.0.1');
  const base = `http://127.0.0.1:${address.port}`;
  assert.equal((await fetch(`${base}/api/frozen-stock`)).status, 401);
  const setup = await fetch(`${base}/api/auth/setup`, {
    method: 'POST',
    headers: { 'content-type': 'application/json', origin: base },
    body: JSON.stringify({ token: 'setup-token-123', username: 'ana', displayName: 'Ana', password: 'secret12' }),
  });
  const cookie = setup.headers.getSetCookie().map(c => c.split(';')[0]).join('; ');
  const first = await fetch(`${base}/api/frozen-stock`, { headers: { cookie } });
  assert.equal(first.status, 200);
  const stock = await first.json();
  assert.deepEqual(stock.species, { 'Oleria onega': { M: 1, F: 1, unknown: 0, total: 2 } });
  assert.match(stock.generatedAt, /^\d{4}-\d\d-\d\dT/);
  const etag = first.headers.get('etag');
  assert.equal(etag, `"${stock.version}"`);
  assert.equal((await fetch(`${base}/api/frozen-stock`, { headers: { cookie, 'if-none-match': etag } })).status, 304);
  // A change that counts: a new version.
  const record = app.store.getRecordBySheetRow('Collection_data', 3);
  app.store.db
    .prepare('UPDATE records SET values_json=?, version=version+1, updated_at=? WHERE id=?')
    .run(JSON.stringify({ ...record.values, Sex: 'female' }), new Date(Date.now() + 1000).toISOString(), record.id);
  const after = await fetch(`${base}/api/frozen-stock`, { headers: { cookie, 'if-none-match': etag } });
  assert.equal(after.status, 200);
  assert.deepEqual((await after.json()).species['Oleria onega'], { M: 0, F: 2, unknown: 0, total: 2 });
});
