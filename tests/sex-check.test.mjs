import test from 'node:test';
import assert from 'node:assert/strict';
import { fileURLToPath } from 'node:url';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { UNRELIABLE, createSexCheck, decideSex, unreliableFor } from '../server/sex-check.mjs';

// ------------------------------------------------------------ the rule (docs/sex-check.md)
const db = (...sexes) => sexes.map((sex, i) => ({ sheet: i ? 'Insectary_data' : 'Collection_data', row: i + 2, sex, raw: sex ?? '' }));
const sure = (pMale, supported = true) => ({ pMale, supported, status: supported ? 'species_supported' : 'below_confidence' });
const env = (value, conf = 0.95, more = {}) => ({ value, conf, lines: [value === 'male' ? 'Oleria ♂' : 'Oleria ♀'], ...more });

test('a Supported model call against the database is flagged, with what the envelope says', () => {
  const a = decideSex({ model: sure(0.99), envelope: null, database: db('female') });
  assert.equal(a.flag, 'disagree');
  assert.deepEqual(a.reasons, ['model_vs_database']);
  assert.equal(a.suggest, 'male');
  assert.deepEqual(a.model, { sex: 'male', p: 0.99, supported: true, status: 'species_supported' });
  assert.deepEqual(decideSex({ model: sure(0.02), envelope: env('female'), database: db('male') }).reasons, ['model_vs_database', 'envelope_agrees_with_model']);
  assert.deepEqual(decideSex({ model: sure(0.02), envelope: env('male'), database: db('male') }).reasons, ['model_vs_database', 'envelope_agrees_with_database']);
});

test('a call that is not Supported is a leaning, never an error', () => {
  const a = decideSex({ model: sure(0.99, false), envelope: null, database: db('female') });
  assert.equal(a.flag, 'uncertain');
  assert.deepEqual(a.reasons, ['model_not_supported']);
  assert.equal(a.model.sex, 'male');
  assert.equal(decideSex({ model: { status: 'not_scored:no_wings' }, database: db('female') }).flag, 'uncertain');
  assert.deepEqual(decideSex({ model: { status: 'not_scored:no_wings' }, database: db('female') }).reasons, ['model_not_scored']);
});

test('the envelope alone against the database is a weaker flag, and none when a Supported call agrees with the database', () => {
  assert.equal(decideSex({ model: sure(0.6, false), envelope: env('male'), database: db('female') }).flag, 'check');
  assert.deepEqual(decideSex({ model: null, envelope: env('male'), database: db('female') }).reasons, ['envelope_vs_database']);
  // The model sure and agreeing with the database: the reader probably invented the sign.
  const a = decideSex({ model: sure(0.01), envelope: env('male'), database: db('female') });
  assert.equal(a.flag, 'agree');
  assert.deepEqual(a.reasons, ['envelope_misread_likely']);
  // Weak envelope readings do not count: both signs on one envelope, or a low confidence (a lone sign is halved by the parser).
  assert.equal(decideSex({ model: null, envelope: env('male', 0.9, { both: true }), database: db('female') }).flag, 'uncertain');
  assert.equal(decideSex({ model: null, envelope: env('male', 0.45, { alone: true }), database: db('female') }).flag, 'uncertain');
  assert.equal(decideSex({ model: null, envelope: env('male', 0.45, { alone: true }), database: db('female') }).envelope.sure, false);
});

test('agreement, an empty database sex, and rows that differ among themselves', () => {
  assert.deepEqual(decideSex({ model: sure(0.97), envelope: env('male'), database: db('male', 'male') }).flag, 'agree');
  const empty = decideSex({ model: sure(0.97), envelope: null, database: db(null) });
  assert.equal(empty.flag, 'check');
  assert.deepEqual(empty.reasons, ['database_empty']);
  assert.equal(empty.suggest, 'male');
  assert.deepEqual(decideSex({ model: null, envelope: env('female'), database: [] }).reasons, ['database_empty', 'only_envelope']);
  assert.deepEqual(decideSex({ model: sure(0.97), envelope: env('female'), database: [] }).reasons, ['model_vs_envelope', 'database_empty']);
  assert.equal(decideSex({ model: sure(0.6, false), envelope: null, database: [] }).flag, 'uncertain');
  assert.equal(decideSex({ model: null, envelope: null, database: [] }).flag, 'none');
  const differ = decideSex({ model: sure(0.97), envelope: null, database: db('male', 'female') });
  assert.equal(differ.flag, 'check');
  assert.deepEqual(differ.reasons, ['database_rows_differ']);
  assert.equal(differ.database.differ, true);
  assert.equal(differ.database.sex, null);
});

test('a species the model is not trusted on (server/sex-model-unreliable.json): its call never flags nor agrees', () => {
  const methona = unreliableFor('Methona confusa confusa');
  assert.ok(methona?.why, 'Methona confusa is on the list, matched by genus and species');
  assert.equal(unreliableFor('methona  CONFUSA'), methona);
  assert.equal(unreliableFor('Methona curvifascia'), null);
  assert.equal(unreliableFor(null), null);
  assert.ok(Object.values(UNRELIABLE).every(v => v.why && v.source), 'each entry says why and where it was found');
  // A sure-looking call against the database: shown, not flagged.
  const a = decideSex({ model: sure(0.99), envelope: null, database: db('female'), unreliable: methona });
  assert.equal(a.flag, 'uncertain');
  assert.deepEqual(a.reasons, ['model_unreliable_species']);
  assert.deepEqual([a.model.sex, a.model.supported, a.model.unreliable], ['male', false, methona.why]);
  assert.equal(a.suggest, undefined);
  // Agreeing is not an agreement either, and an empty database gets no suggestion from it.
  assert.equal(decideSex({ model: sure(0.99), database: db('male'), unreliable: methona }).flag, 'uncertain');
  const empty = decideSex({ model: sure(0.99), database: db(null), unreliable: methona });
  assert.deepEqual([empty.flag, empty.suggest, empty.reasons], ['uncertain', undefined, ['database_empty', 'model_unreliable_species']]);
  // The envelope against the database still counts as before.
  const e = decideSex({ model: sure(0.99), envelope: env('male'), database: db('female'), unreliable: methona });
  assert.deepEqual([e.flag, e.reasons], ['check', ['envelope_vs_database', 'model_unreliable_species']]);
  // Without a model call there is nothing to call unreliable.
  assert.deepEqual(decideSex({ model: { status: 'not_scored:no_wings' }, database: db('female'), unreliable: methona }).reasons, ['model_not_scored']);
});

// ------------------------------------------------------------ the check on a small workbook, with stand-in children
const FAKE = fileURLToPath(new URL('./fixtures/fake-sex-worker.mjs', import.meta.url));
const row = (row, cam, SPECIES, Sex) => ({ row, values: { Release_Collect: 'Collected_Preserved', CAM_ID: cam, SPECIES, Sex, Collection_location: 'Ikiam' } });
async function setup() {
  const seed = {
    Collection_data: [row(2, 'CAM000201', 'Oleria baizana', 'male'), row(3, 'CAM000202', 'Hypothyris euclea', 'female'), row(4, 'CAM000203', 'Oleria baizana', ''), row(5, 'CAM000204', 'Methona confusa', 'female')],
    Taxonomy_v18Jun25: ['Oleria baizana', 'Hypothyris euclea', 'Methona confusa'].map((species, i) => ({ row: i + 2, values: { species } })),
  };
  const store = new Store({ localMode: true }, { sheets: new LocalSheets(seed) });
  await store.sync({ sheets: Object.keys(seed) });
  return createSexCheck({ store, workerArgs: [FAKE], readerArgs: [FAKE, '--reader'] });
}
const photo = o => Buffer.from(JSON.stringify(o));

test('a butterfly: the model and the envelope read from its photos, the database from its CAM, the flag', async () => {
  const check = await setup();
  try {
    assert.deepEqual(check.status(), { available: true });
    // The database says male; the model (Supported) and the envelope say female.
    const a = await check.check({ cam: 'cam000201', d: photo({ pMale: 0.01, supported: true, line: ['Oleria baizana ♀', 0.97] }), v: photo({ line: ['CAM000201', 0.99] }) });
    assert.equal(a.cam, 'CAM000201');
    assert.equal(a.flag, 'disagree');
    assert.deepEqual(a.reasons, ['model_vs_database', 'envelope_agrees_with_model']);
    assert.equal(a.species, 'Oleria baizana');
    assert.equal(a.model.status, 'species:Oleria baizana', 'the database species goes to the Supported screen');
    assert.equal(a.envelope.sex, 'female');
    assert.deepEqual(a.database.rows, [{ sheet: 'Collection_data', row: 2, value: 'male' }]);
    // Agreement; a sex missing from the database; no wings found.
    assert.equal((await check.check({ cam: 'CAM000202', d: photo({ pMale: 0.02, supported: true }), v: photo({}) })).flag, 'agree');
    const empty = await check.check({ cam: 'CAM000203', d: photo({ pMale: 0.93, supported: true }), v: photo({}) });
    assert.deepEqual([empty.flag, empty.suggest], ['check', 'male']);
    const none = await check.check({ cam: 'CAM000202', d: photo({}), v: photo({}) });
    assert.deepEqual([none.flag, none.model.status], ['uncertain', 'not_scored:no_wings']);
    const methona = await check.check({ cam: 'CAM000204', d: photo({ pMale: 0.99, supported: true }), v: photo({}) });
    assert.deepEqual([methona.flag, methona.reasons, methona.model.supported], ['uncertain', ['model_unreliable_species'], false]);
    await assert.rejects(check.check({ cam: 'IMG_0001', d: photo({}), v: photo({}) }), /Not a CAM ID/);
  } finally {
    check.close();
  }
});

test('without the photo reader or the sex model it says why', async () => {
  const check = createSexCheck({ store: null, readerDir: '/nowhere', identifierDir: '/nowhere' });
  assert.deepEqual(check.status(), { available: false, why: 'no photo reader with the wing chain (scripts/build-rename-app.sh)' });
  await assert.rejects(check.check({ cam: 'CAM000201', d: Buffer.from('x'), v: Buffer.from('x') }), e => e.status === 503);
});
