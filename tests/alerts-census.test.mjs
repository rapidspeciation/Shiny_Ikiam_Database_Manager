// Alerts that are not news and alerts about censuses (server/alerts.mjs), on the cases of
// 04/11/2026: the CRISPR pool (1 CAM left, last used 19/11/2025) is information, not a call to
// ask PAS or AA; the M. polymnia (06/10, 400 expected) and M. messenoides (13/10, 142 expected)
// censuses open in the Censo tab with no marks; the 6 «Actualizar censos» proposals of 06/10 with
// 28 disappearances still pending.
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
async function fixture({ insectaryDay = '2026-10-30', crisprDay = '2025-11-19' } = {}) {
  const rows = [];
  let row = 2;
  for (let i = 0; i <= 48; i++)
    rows.push({ row: row++, values: { Insectary_ID: `C${i}X`, SPECIES: 'Mechanitis polymnia', CAM_ID: crispr(i), Death_date: serial(crisprDay) - 48 + i } });
  for (let i = 0; i <= 60; i++)
    rows.push({ row: row++, values: { Insectary_ID: `I${i}X`, SPECIES: 'Mechanitis messenoides', CAM_ID: insectary(i), Death_date: serial(insectaryDay) - 60 + i } });
  for (let i = 0; i < 5; i++) rows.push({ row: row++, values: { Insectary_ID: `P${i}Y`, SPECIES: 'Mechanitis polymnia proceriformis', Intro2Insectary_date: serial('2026-08-01') } });
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
  // The assistant's tables (ai_proposals).
  createAssistant({ store, config: {} });
  return store;
}

function addCensus(store, { species, day, expected, createdAt = `${day}T15:00:00.000Z`, status = 'open', marks = 0 }) {
  const id = randomUUID();
  store.db
    .prepare('INSERT INTO censuses(id, request_id, species, day, status, expected, created_by, created_at) VALUES (?,?,?,?,?,?,?,?)')
    .run(id, randomUUID(), species, day, status, expected, 'u-franz', createdAt);
  for (let i = 0; i < marks; i++)
    store.db
      .prepare('INSERT INTO census_marks(id, request_id, census_id, record_id, insectary_id, kind, actor, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?)')
      .run(randomUUID(), randomUUID(), id, randomUUID(), `M${i}Z`, 'seen', 'u-franz', createdAt, createdAt);
  return id;
}

let threadRow = 0;
/** A proposal of the chat `t3` (its T3 thread) with `gone` disappearances and `others` other changes. */
function addProposal(store, { gone, others = 0, createdAt, status = 'pending', t3 = 't3-censos', title = 'Actualizar censos', context = 0 }) {
  const thread = `thread-${++threadRow}`;
  store.db.prepare('INSERT INTO ai_threads(id, owner_id, title, created_at, updated_at) VALUES (?,?,?,?,?)').run(thread, 'u-franz', 'T3 Code', createdAt, createdAt);
  const changes = [
    ...Array.from({ length: gone }, (_, i) => ({ recordId: `r${i}`, values: { Death_cause: 'Disappearance', Death_date: serial('2026-10-02') } })),
    ...Array.from({ length: context }, (_, i) => ({ context: true, recordId: `c${i}`, values: { Death_cause: 'Disappearance' } })),
    ...Array.from({ length: others }, (_, i) => ({ recordId: `o${i}`, values: { Sex: 'female' } })),
  ];
  const id = randomUUID();
  store.db
    .prepare('INSERT INTO ai_proposals(id, thread_id, owner_id, changes_json, reason, status, created_at, t3_thread, t3_title) VALUES (?,?,?,?,?,?,?,?,?)')
    .run(id, thread, 'u-franz', JSON.stringify(changes), 'Censo 2/10/26', status, createdAt, t3, title);
  return id;
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
  const messenoides = addCensus(store, { species: 'Mechanitis messenoides', day: '2026-10-13', expected: 142 });
  // Not alerts: marked, opened 5 days ago, finished, exactly 7 days old.
  addCensus(store, { species: 'Mechanitis lysimnia', day: '2026-10-06', expected: 30, marks: 3 });
  addCensus(store, { species: 'Ithomia salapia', day: '2026-10-30', expected: 20 });
  addCensus(store, { species: 'Oleria baizana', day: '2026-10-01', expected: 10, status: 'finished' });
  addCensus(store, { species: 'Hypothyris anastasia', day: '2026-10-28', expected: 12 });
  const data = alerts(store);
  assert.deepEqual(
    data.openCensuses.map(c => [c.id, c.species, c.day, c.expected, c.days, c.by]),
    [
      [polymnia, 'Mechanitis polymnia', '2026-10-06', 400, 29, 'Franz'],
      [messenoides, 'Mechanitis messenoides', '2026-10-13', 142, 22, 'Franz'],
    ],
  );
  const shown = data.alerts.filter(a => a.id.startsWith('census-open:'));
  assert.deepEqual(
    shown.map(a => [a.id, a.level, a.link]),
    [
      [`census-open:${polymnia}`, 'warn', '#/censo'],
      [`census-open:${messenoides}`, 'warn', '#/censo'],
    ],
  );
  assert.equal(
    shown[0].text,
    'Censo de Mechanitis polymnia del 06/10/2026: sigue abierto en la pestaña Censo y sin marcas (400 mariposas esperadas). Si se contó en papel, pasen las marcas y termínenlo; si no se hizo, cancélenlo.',
  );
  assert.match(shown[1].text, /^Censo de Mechanitis messenoides del 13\/10\/2026: .*\(142 mariposas esperadas\)/);
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

test('census proposals pending more than two weeks are one alert per chat, with how many disappearances and a link to them', async () => {
  at();
  const store = await fixture();
  // The 6 proposals of 06/10 in «Actualizar censos»: 14 + 6 + 3 + 2 + 2 + 1 = 28 disappearances.
  const ids = [14, 6, 3, 2, 2, 1].map((gone, i) =>
    addProposal(store, { gone, others: i ? 20 : 1, context: i ? 3 : 0, createdAt: `2026-10-06T${19 + (i ? 1 : 0)}:${String(10 + i).padStart(2, '0')}:00.000Z` }),
  );
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
      disappearances: 28,
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
    'Censo: 6 propuestas con 28 desaparecidas esperan desde el 06/10/2026 sin aplicar (chat «Actualizar censos» de Franz). Revísenlas y aplíquenlas o descártenlas; mientras tanto esas mariposas cuentan como vivas.',
  );

  // Applying them ends the alert (the stamp follows the pending proposals).
  store.db.prepare(`UPDATE ai_proposals SET status = 'applied' WHERE id IN (${ids.map(() => '?').join(',')})`).run(...ids);
  assert.deepEqual(staleCensusProposals(store), []);
  assert.ok(!alerts(store).alerts.some(a => a.id.startsWith('census-proposals:')));
  store.close();
});

test('a proposal made outside the T3 chats links to itself; a single one reads in the singular', async () => {
  at();
  const store = await fixture();
  const id = addProposal(store, { gone: 1, createdAt: '2026-10-01T12:00:00.000Z', t3: null, title: null });
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
