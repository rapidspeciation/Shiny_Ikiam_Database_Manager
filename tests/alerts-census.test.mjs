// Alerts that are not news and alerts about censuses (server/alerts.mjs), on the cases of
// 04/11/2026: the CRISPR pool (1 CAM left, last used 19/11/2025) is information, not a call to
// ask PAS or AA; the M. polymnia (06/10, 400 expected) and M. messenoides (13/10, 142 expected)
// censuses open in the Censo tab with no marks; the 6 «Actualizar censos» proposals of 06/10 still
// pending. As production had them that day (CENSUS-FACTCHECK, 04/11/2026): the polymnia census
// of 01/10 was entered through Muertes on 12/10 (14 M. p. proceriformis disappearances dated
// 01/10), so the Censo-tab session of 06/10 is information; nothing was entered for messenoides,
// so its session is «abierto sin usar»; the census proposals mark 14 butterflies (each in two of
// them: a first draft and its five pages), 4 of which got a death date since, so they are stale.
import test, { mock } from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { alerts, alertsStamp, idleCensuses, staleCensusProposals } from '../server/alerts.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { addUser } from './helpers/assistant.mjs';

const EPOCH = Date.UTC(1899, 11, 30);
const serial = iso => Math.round((Date.parse(`${iso}T00:00:00Z`) - EPOCH) / 864e5);
const crispr = n => `CAM${String(78500 + n).padStart(6, '0')}`;
const insectary = n => `CAM${String(78000 + n).padStart(6, '0')}`;

/** Ecuador, 04/11/2026 at noon. */
function at(iso = '2026-11-04T17:00:00Z') {
  mock.timers.enable({ apis: ['Date'], now: Date.parse(iso) });
}
test.afterEach(() => mock.timers.reset());

/**
 * The CRISPR pool of Lists, CAM078500–CAM078549, used up to CAM078548 on 19/11/2025; and the
 * insectary pool, CAM078000–CAM078099, used up to CAM078060, last on `insectaryDay`.
 */
async function fixture({ insectaryDay = '2026-10-30', crisprDay = '2025-11-19', extra = [] } = {}) {
  const rows = [];
  let row = 2;
  for (let i = 0; i <= 48; i++)
    rows.push({ row: row++, values: { Insectary_ID: `C${i}X`, SPECIES: 'Mechanitis polymnia', CAM_ID: crispr(i), Death_date: serial(crisprDay) - 48 + i } });
  for (let i = 0; i <= 60; i++)
    rows.push({ row: row++, values: { Insectary_ID: `I${i}X`, SPECIES: 'Mechanitis messenoides', CAM_ID: insectary(i), Death_date: serial(insectaryDay) - 60 + i } });
  for (let i = 0; i < 5; i++) rows.push({ row: row++, values: { Insectary_ID: `P${i}Y`, SPECIES: 'Mechanitis polymnia proceriformis', Intro2Insectary_date: serial('2026-08-01') } });
  for (const values of extra) rows.push({ row: row++, values });
  const sheets = new LocalSheets({
    Insectary_data: rows,
    // One row per Lists line: each pool is a column.
    Lists: Array.from({ length: 100 }, (_, i) => ({
      row: 2 + i,
      values: { 'InsectaryWild&Reared_CAMid': insectary(i), ...(i < 50 ? { CRISPRonly_CAMid: crispr(i) } : {}) },
    })),
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Lists'] });
  addUser(store, { id: 'u-franz', username: 'franz', displayName: 'Franz' });
  addUser(store, { id: 'u-kim', username: 'kim', displayName: 'Kim' });
  // The assistant's tables (ai_proposals).
  createAssistant({ store, config: {} });
  return store;
}

function addCensus(store, { species, day, expected, createdAt = `${day}T15:00:00.000Z`, status = 'open', marks = 0, by = 'u-franz' }) {
  const id = randomUUID();
  store.db
    .prepare('INSERT INTO censuses(id, request_id, species, day, status, expected, created_by, created_at) VALUES (?,?,?,?,?,?,?,?)')
    .run(id, randomUUID(), species, day, status, expected, by, createdAt);
  for (let i = 0; i < marks; i++)
    store.db
      .prepare('INSERT INTO census_marks(id, request_id, census_id, record_id, insectary_id, kind, actor, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?)')
      .run(randomUUID(), randomUUID(), id, randomUUID(), `M${i}Z`, 'seen', 'u-franz', createdAt, createdAt);
  return id;
}

let threadRow = 0;
/**
 * A proposal of the chat `t3` (its T3 thread): the butterflies `gone` (records, or how many made-up
 * ones) marked disappeared on 02/10, `others` other changes, `context` lines already in the sheet.
 */
function addProposal(store, { gone, others = 0, createdAt, status = 'pending', t3 = 't3-censos', title = 'Actualizar censos', context = 0 }) {
  const thread = `thread-${++threadRow}`;
  store.db.prepare('INSERT INTO ai_threads(id, owner_id, title, created_at, updated_at) VALUES (?,?,?,?,?)').run(thread, 'u-franz', 'T3 Code', createdAt, createdAt);
  const butterflies = Array.isArray(gone) ? gone : Array.from({ length: gone }, () => ({ id: `r-${randomUUID()}` }));
  const changes = [
    ...butterflies.map(b => ({ recordId: b.id, expectedVersion: b.version, values: { Death_cause: 'Disappearance', Death_date: serial('2026-10-02') } })),
    ...Array.from({ length: context }, (_, i) => ({ context: true, recordId: `c${i}`, values: {} })),
    ...Array.from({ length: others }, (_, i) => ({ recordId: `o${threadRow}-${i}`, values: { Sex: 'female' } })),
  ];
  const id = randomUUID();
  store.db
    .prepare('INSERT INTO ai_proposals(id, thread_id, owner_id, changes_json, reason, status, created_at, t3_thread, t3_title) VALUES (?,?,?,?,?,?,?,?,?)')
    .run(id, thread, 'u-franz', JSON.stringify(changes), 'Censo 2/10/26', status, createdAt, t3, title);
  return id;
}
/** Insectary_data rows by Insectary_ID: { id (record), version }. */
const recordsOf = (store, ids) =>
  ids.map(label => store.db.prepare("SELECT id, version FROM records WHERE sheet = 'Insectary_data' AND label = ?").get(label));
/** A record changed since (another route wrote it): its values merged, its version up. */
function changeRecord(store, recordId, values) {
  const r = store.db.prepare('SELECT values_json, version FROM records WHERE id = ?').get(recordId);
  store.db
    .prepare('UPDATE records SET values_json = ?, version = ?, updated_at = ? WHERE id = ?')
    .run(JSON.stringify({ ...JSON.parse(r.values_json), ...values }), r.version + 1, new Date().toISOString(), recordId);
}

test('a CAM range not used for six months is information with its date, not a call to ask PAS or AA', async () => {
  at();
  const store = await fixture();
  const data = alerts(store);
  const pool = data.camPools.find(p => p.pool === 'CRISPRonly_CAMid');
  assert.deepEqual(
    pool.ranges.map(r => [r.first, r.last, r.used, r.left, r.highest, r.lastUsed?.date, r.level]),
    [['CAM078500', 'CAM078549', 49, 1, 'CAM078548', '2025-11-19', 'idle']],
  );
  assert.equal(pool.level, 'ok');
  const cam = data.alerts.find(a => a.id === 'cam:CRISPRonly_CAMid:CAM078500');
  assert.equal(cam.level, 'info');
  assert.equal(cam.text, 'CRISPRonly_CAMid: queda 1 CAM en CAM078500–CAM078549, sin uso desde 19/11/2025 (último CAM078548).');
  assert.doesNotMatch(cam.text, /Pidan/);
  // The insectary pool, used last week, still warns.
  const used = data.alerts.find(a => a.id === 'cam:InsectaryWild&Reared_CAMid:CAM078000');
  assert.equal(used.level, 'warn');
  assert.match(used.text, /quedan 39 CAM .*Pidan a PAS o AA un rango nuevo\./);
  store.close();
});

test('a CAM range last used five months ago still warns (the idle line is about six months)', async () => {
  at();
  const store = await fixture({ crisprDay: '2026-06-04' });
  const cam = alerts(store).alerts.find(a => a.id === 'cam:CRISPRonly_CAMid:CAM078500');
  assert.equal(cam.level, 'warn');
  assert.match(cam.text, /Pidan a PAS o AA un rango nuevo\./);
  store.close();
});

test('a census open in the Censo tab with no marks after a week is an alert; one with marks or recent is not', async () => {
  at();
  const store = await fixture();
  const polymnia = addCensus(store, { species: 'Mechanitis polymnia', day: '2026-10-06', expected: 400, createdAt: '2026-10-07T03:23:24.042Z' });
  const messenoides = addCensus(store, { species: 'Mechanitis messenoides', day: '2026-10-13', expected: 142, by: 'u-kim' });
  // Not alerts: marked, opened 5 days ago, finished, exactly 7 days old.
  addCensus(store, { species: 'Mechanitis lysimnia', day: '2026-10-06', expected: 30, marks: 3 });
  addCensus(store, { species: 'Ithomia salapia', day: '2026-10-30', expected: 20 });
  addCensus(store, { species: 'Oleria baizana', day: '2026-10-01', expected: 10, status: 'finished' });
  addCensus(store, { species: 'Hypothyris anastasia', day: '2026-10-28', expected: 12 });
  const data = alerts(store);
  assert.deepEqual(
    data.openCensuses.map(c => [c.id, c.species, c.day, c.expected, c.days, c.by, c.covered]),
    [
      [polymnia, 'Mechanitis polymnia', '2026-10-06', 400, 29, 'Franz', null],
      [messenoides, 'Mechanitis messenoides', '2026-10-13', 142, 22, 'Kim', null],
    ],
  );
  const shown = data.alerts.filter(a => a.id.startsWith('census-open:'));
  assert.deepEqual(
    shown.map(a => [a.id, a.level, a.link, a.action]),
    [
      [`census-open:${polymnia}`, 'warn', '#/censo', { kind: 'cancel-census', census: polymnia }],
      [`census-open:${messenoides}`, 'warn', '#/censo', { kind: 'cancel-census', census: messenoides }],
    ],
  );
  assert.match(shown[1].text, /^Censo de Mechanitis messenoides del 13\/10\/2026: abierto sin usar\. .*que abrió Kim .*\(142 mariposas esperadas\)/);
  assert.deepEqual(shown[0].textMsg.vars.species, 'Mechanitis polymnia');

  // A first mark or a cancel ends it, and the kept alerts are computed again.
  const before = alertsStamp(store);
  store.db
    .prepare('INSERT INTO census_marks(id, request_id, census_id, record_id, insectary_id, kind, actor, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?)')
    .run(randomUUID(), randomUUID(), polymnia, randomUUID(), 'P0Y', 'seen', 'u-franz', '2026-11-04T17:00:00Z', '2026-11-04T17:00:00Z');
  store.db.prepare("UPDATE censuses SET status = 'cancelled' WHERE id = ?").run(messenoides);
  assert.notEqual(alertsStamp(store), before);
  assert.deepEqual(alerts(store).openCensuses, []);
  assert.ok(!alerts(store).alerts.some(a => a.id.startsWith('census-open:')));
  store.close();
});

/** Production on 04/11/2026: the 14 polymnia disappearances of 01/10 and the 14 butterflies of the 02/10 census proposals. */
const OCT = [
  // Entered by Alejandra through Muertes on 12/10, dated 01/10 (the polymnia census).
  ...Array.from({ length: 14 }, (_, i) => ({ Insectary_ID: `A${i}D`, SPECIES: 'Mechanitis polymnia proceriformis', Death_date: serial('2026-10-01'), Death_cause: 'Disappearance' })),
  // Two weeks before: not this census.
  { Insectary_ID: 'Q1D', SPECIES: 'Mechanitis polymnia eurydice', Death_date: serial('2026-09-16'), Death_cause: 'Disappearance' },
  // A messenoides that died (not disappeared) that week.
  { Insectary_ID: 'M1D', SPECIES: 'Mechanitis messenoides', Death_date: serial('2026-10-14'), Death_cause: 'Natural' },
  // The 02/10 census: 7 + 1 + 1 + 1 still alive, 3 + 1 with a death date entered since.
  ...['salapia salapia', 'salapia salapia', 'salapia salapia', 'salapia salapia', 'salapia salapia', 'salapia salapia', 'salapia salapia'].map((sp, i) => ({ Insectary_ID: `S${i}D`, SPECIES: `Ithomia ${sp}` })),
  { Insectary_ID: 'L1D', SPECIES: 'Mechanitis lysimnia' },
  { Insectary_ID: 'L2D', SPECIES: 'Mechanitis lysimnia' },
  { Insectary_ID: 'E1D', SPECIES: 'Mechanitis polymnia proceriformis' },
  { Insectary_ID: 'E2D', SPECIES: 'Mechanitis polymnia eurydice' },
  { Insectary_ID: 'S7D', SPECIES: 'Ithomia salapia salapia' },
  { Insectary_ID: 'S8D', SPECIES: 'Ithomia salapia salapia' },
  { Insectary_ID: 'S9D', SPECIES: 'Ithomia salapia salapia' },
];
const STILL_ALIVE = ['S0D', 'S1D', 'S2D', 'S3D', 'S4D', 'S5D', 'S6D', 'L1D', 'E1D', 'E2D'];
const DEAD_SINCE = ['S7D', 'S8D', 'S9D', 'L2D'];

test('04/11/2026 replayed: the polymnia session is covered by the 01/10 census entered in Muertes; messenoides is open unused; the 02/10 proposals are stale', async () => {
  at();
  const store = await fixture({ extra: OCT });
  const polymnia = addCensus(store, { species: 'Mechanitis polymnia', day: '2026-10-06', expected: 400, createdAt: '2026-10-07T03:23:24.042Z' });
  const messenoides = addCensus(store, { species: 'Mechanitis messenoides', day: '2026-10-13', expected: 142, by: 'u-kim' });
  // The first draft (all 14) and its five pages (7 + 3 + 2 + 1 + 1), each with Emergidos context lines.
  const all = recordsOf(store, [...STILL_ALIVE, ...DEAD_SINCE]);
  const pages = [all.slice(0, 7), all.slice(7, 10), all.slice(10, 12), all.slice(12, 13), all.slice(13)];
  const ids = [
    addProposal(store, { gone: all, createdAt: '2026-10-06T19:37:05.307Z' }),
    ...pages.map((page, i) => addProposal(store, { gone: page, context: 20, createdAt: `2026-10-06T20:12:${String(40 + i).padStart(2, '0')}.000Z` })),
  ];
  // Since then other routes gave 4 of them a death date (Muertes, Sheets).
  for (const r of recordsOf(store, DEAD_SINCE)) changeRecord(store, r.id, { Death_date: serial('2026-10-11'), Death_cause: 'Disappearance' });

  const data = alerts(store);
  const [p, m] = data.openCensuses;
  assert.deepEqual([p.id, p.covered], [polymnia, { count: 14, dates: ['2026-10-01'] }]);
  assert.deepEqual([m.id, m.covered], [messenoides, null]);
  const open = Object.fromEntries(data.alerts.filter(a => a.id.startsWith('census-open:')).map(a => [a.id, a]));
  const covered = open[`census-open:${polymnia}`];
  assert.equal(covered.level, 'info');
  assert.equal(
    covered.text,
    'Censo de Mechanitis polymnia del 06/10/2026: ya está en la base por otra vía (14 desapariciones de Mechanitis polymnia del 01/10/2026). La sesión de la pestaña Censo que abrió Franz quedó sin marcas; se puede cancelar.',
  );
  assert.deepEqual(covered.action, { kind: 'cancel-census', census: polymnia });
  const unused = open[`census-open:${messenoides}`];
  assert.equal(unused.level, 'warn');
  assert.equal(
    unused.text,
    'Censo de Mechanitis messenoides del 13/10/2026: abierto sin usar. La sesión que abrió Kim en la pestaña Censo no tiene marcas (142 mariposas esperadas) y la base no tiene desapariciones de Mechanitis messenoides de esa semana. Si se contó en papel, pasen las marcas; si no se hizo, cancélenlo.',
  );
  assert.deepEqual(unused.action, { kind: 'cancel-census', census: messenoides });

  const [chat] = data.censusProposals;
  assert.deepEqual(
    { ...chat, proposals: chat.proposals.length },
    { chat: 't3-censos', title: 'Actualizar censos', by: 'Franz', proposals: 6, censusDays: ['2026-10-02'], disappearances: 14, open: 10, rows: 14, changed: 4, stale: true, since: '2026-10-06', days: 29, link: '#/propuestas?chat=t3-censos' },
  );
  assert.deepEqual(chat.proposals, ids);
  const shown = data.alerts.find(a => a.id === 'census-proposals:t3-censos');
  assert.equal(shown.level, 'warn');
  assert.equal(
    shown.text,
    'Censo del 02/10/2026 (propuestas del 06/10/2026, chat «Actualizar censos» de Franz): 10 de las 14 mariposas que sus propuestas dan por desaparecidas siguen sin fecha de muerte y cuentan como vivas. Las propuestas quedaron viejas (4 de sus 14 filas cambiaron desde entonces): no las apliquen; rehagan esas desapariciones desde el cuaderno.',
  );
  assert.doesNotMatch(shown.text, /aplíquen/);
  assert.equal(shown.action, undefined);

  // Once the 10 get their death date the alert is only information: discard the proposals.
  mock.timers.tick(60_000);
  for (const r of recordsOf(store, STILL_ALIVE)) changeRecord(store, r.id, { Death_date: serial('2026-10-02'), Death_cause: 'Disappearance' });
  const after = alerts(store).alerts.find(a => a.id === 'census-proposals:t3-censos');
  assert.equal(after.level, 'info');
  assert.match(after.text, /: las 14 mariposas de sus 6 propuestas ya tienen fecha de muerte\. .*descártenlas\.$/);
  store.close();
});

test('a census session is covered only by disappearances of its species within a week of its day', async () => {
  at();
  const store = await fixture({
    extra: [
      { Insectary_ID: 'X1D', SPECIES: 'Mechanitis lysimnia', Death_date: serial('2026-10-13'), Death_cause: 'Disappearance' },
      { Insectary_ID: 'X2D', SPECIES: 'Mechanitis lysimnia', Death_date: serial('2026-09-28'), Death_cause: 'Disappearance' },
      { Insectary_ID: 'X3D', SPECIES: 'Ithomia salapia', Death_date: serial('2026-10-06'), Death_cause: 'Disappearance' },
      { Insectary_ID: 'X4D', SPECIES: 'Hypothyris anastasia', Death_date: serial('2026-10-06'), Death_cause: 'Natural' },
    ],
  });
  const lysimnia = addCensus(store, { species: 'Mechanitis lysimnia', day: '2026-10-06', expected: 30 });
  addCensus(store, { species: 'Hypothyris anastasia', day: '2026-10-06', expected: 12 });
  const [l, h] = alerts(store).openCensuses;
  // 13/10 is 7 days after (in), 28/09 8 days before (out); another species and a natural death do not count.
  assert.deepEqual([l.id, l.covered], [lysimnia, { count: 1, dates: ['2026-10-13'] }]);
  assert.equal(h.covered, null);
  assert.match(alerts(store).alerts.find(a => a.id === `census-open:${lysimnia}`).text, /\(1 desaparición de Mechanitis lysimnia del 13\/10\/2026\)/);
  store.close();
});

test('census proposals pending more than two weeks whose rows did not change are one alert per chat, to apply or discard', async () => {
  at();
  const store = await fixture({ extra: OCT });
  const all = recordsOf(store, [...STILL_ALIVE, ...DEAD_SINCE]);
  const ids = [all.slice(0, 9), all.slice(9)].map((gone, i) => addProposal(store, { gone, context: 3, createdAt: `2026-10-06T2${i}:10:00.000Z` }));
  // Not alerts: applied, discarded, drafted 10 days ago, no disappearances, exactly 14 days old (Ecuador day).
  addProposal(store, { gone: 5, createdAt: '2026-09-20T12:00:00.000Z', status: 'applied' });
  addProposal(store, { gone: 5, createdAt: '2026-09-20T12:00:00.000Z', status: 'discarded' });
  addProposal(store, { gone: 4, createdAt: '2026-10-25T12:00:00.000Z', t3: 't3-new', title: 'Censo nuevo' });
  addProposal(store, { gone: 0, others: 4, createdAt: '2026-09-01T12:00:00.000Z', t3: 't3-other', title: 'Otra cosa' });
  addProposal(store, { gone: 2, createdAt: '2026-10-21T12:00:00.000Z', t3: 't3-edge', title: 'Borde' });
  // Context lines only (already in the sheet): nothing to apply.
  addProposal(store, { gone: 0, context: 4, createdAt: '2026-09-01T12:00:00.000Z', t3: 't3-context', title: 'Contexto' });
  const data = alerts(store);
  assert.deepEqual(data.censusProposals, [
    {
      chat: 't3-censos',
      title: 'Actualizar censos',
      by: 'Franz',
      proposals: ids,
      censusDays: ['2026-10-02'],
      disappearances: 14,
      open: 14,
      rows: 14,
      changed: 0,
      stale: false,
      since: '2026-10-06',
      days: 29,
      link: '#/propuestas?chat=t3-censos',
    },
  ]);
  const shown = data.alerts.filter(a => a.id.startsWith('census-proposals:'));
  assert.equal(shown.length, 1);
  assert.equal(shown[0].level, 'warn');
  assert.equal(shown[0].link, '#/propuestas?chat=t3-censos');
  assert.equal(
    shown[0].text,
    'Censo: 2 propuestas con 14 desaparecidas esperan desde el 06/10/2026 sin aplicar (chat «Actualizar censos» de Franz). Revísenlas y aplíquenlas o descártenlas; mientras tanto esas mariposas cuentan como vivas.',
  );

  // Applying them ends the alert (the stamp follows the pending proposals).
  store.db.prepare(`UPDATE ai_proposals SET status = 'applied' WHERE id IN (${ids.map(() => '?').join(',')})`).run(...ids);
  assert.deepEqual(staleCensusProposals(store), []);
  assert.ok(!alerts(store).alerts.some(a => a.id.startsWith('census-proposals:')));
  store.close();
});

test('a proposal made outside the T3 chats links to itself; a single one reads in the singular', async () => {
  at();
  const store = await fixture({ extra: OCT });
  const id = addProposal(store, { gone: recordsOf(store, ['S0D']), createdAt: '2026-10-01T12:00:00.000Z', t3: null, title: null });
  const [chat] = staleCensusProposals(store);
  assert.equal(chat.link, `#/propuestas/${id}`);
  assert.equal(chat.title, 'T3 Code');
  const shown = alerts(store).alerts.find(a => a.id.startsWith('census-proposals:'));
  assert.match(shown.text, /^Censo: 1 propuesta con 1 desaparecidas espera desde el 01\/10\/2026 sin aplicar \(chat «T3 Code» de Franz\)\. Revísenla/);
  store.close();
});

test('without the census or assistant tables (an older database) there are no census alerts', async () => {
  at();
  const store = await fixture();
  store.db.exec('DROP TABLE census_marks');
  assert.deepEqual(idleCensuses(store), []);
  assert.ok(alertsStamp(store));
  store.close();
});
