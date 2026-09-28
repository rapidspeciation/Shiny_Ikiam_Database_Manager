import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash, randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { allIssues, checkData } from '../server/checks.mjs';
import { createAssistant } from '../server/assistant.mjs';

const EPOCH = Date.UTC(1899, 11, 30);
const serial = iso => Math.round((Date.parse(`${iso}T00:00:00Z`) - EPOCH) / 864e5);
const isoOf = n => new Date(EPOCH + n * 864e5).toISOString().slice(0, 10);
const today = serial(new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date()));
// Typed a year ahead by mistake: 20 days ago, one year later.
const ago = new Date(EPOCH + (today - 20) * 864e5);
const slipped = Math.round((Date.UTC(ago.getUTCFullYear() + 1, ago.getUTCMonth(), ago.getUTCDate()) - EPOCH) / 864e5);
const formulaCell = (formula, value) => ({
  userEnteredValue: { formulaValue: formula },
  effectiveValue: { stringValue: value },
});

async function fixture() {
  const sheets = new LocalSheets({
    Collection_data: [
      // Sent to the insectary as N5D, whose insectary row says another species and sex.
      {
        row: 2,
        values: {
          Release_Collect: 'Collected_Sent2Insectary',
          Insectary_ID: 'N5D',
          SPECIES: 'Ithomia salapia',
          Sex: 'female',
          Collection_date: serial('2026-09-01'),
        },
      },
      // Sent to the insectary, but N9Z was never registered there.
      { row: 3, values: { Release_Collect: 'Collected_Sent2Insectary', Insectary_ID: 'N9Z', SPECIES: 'Ithomia salapia' } },
      // Sent without an Insectary_ID.
      { row: 4, values: { Release_Collect: 'Collected_Sent2Insectary', Insectary_ID: 'NA', SPECIES: 'Ithomia salapia' } },
      // Preserved: a sex outside the list, the CAM of N5D, no tube.
      {
        row: 5,
        values: {
          Release_Collect: 'Collected_Preserved',
          SPECIES: 'Ithomia salapia',
          Sex: 'female_?',
          CAM_ID: 'CAM000010',
          Collection_date: serial('2026-09-02'),
        },
      },
      // Preserved without CAM or tube, dated a year ahead.
      { row: 6, values: { Release_Collect: 'Collected_Preserved', SPECIES: 'Oleria gunilla', Collection_date: slipped } },
      // Mark B40 on two species.
      {
        row: 7,
        values: {
          Release_Collect: 'Mark_Released',
          FieldMark_ID: 'B40',
          SPECIES: 'Oleria gunilla',
          CAM_ID: 'NA',
          Tube_1_id: 'NA',
          Collection_date: serial('2026-05-10'),
        },
      },
      {
        row: 8,
        values: {
          Release_Collect: 'Mark_Released',
          FieldMark_ID: 'b40',
          SPECIES: 'Hyposcada illinissa',
          Collection_date: serial('2026-08-12'),
        },
      },
      // The tube of N5D used again here.
      { row: 9, values: { Release_Collect: 'Mark_Released', Tube_1_id: 'FS00000010', SPECIES: 'Oleria gunilla' } },
      // Sent to the insectary as N7D, whose pre-made row is still empty.
      { row: 10, values: { Release_Collect: 'Collected_Sent2Insectary', Insectary_ID: 'N7D', SPECIES: 'Oleria gunilla' } },
    ],
    Insectary_data: [
      {
        row: 2,
        values: {
          Insectary_ID: 'N5D',
          Wild_Reared: 'Wild-caught',
          SPECIES: 'Mechanitis lysimnia',
          Sex: 'male',
          Intro2Insectary_date: serial('2026-08-30'),
          CAM_ID: 'CAM000010',
          Tube_1_id: 'FS00000010',
          Preservation_date: serial('2026-09-05'),
        },
      },
      // Wild, without a collection row; died "a year before" it entered.
      {
        row: 3,
        values: {
          Insectary_ID: 'N6D',
          Wild_Reared: 'Wild-caught',
          SPECIES: 'Mechanitis lysimnia',
          Intro2Insectary_date: serial('2026-09-01'),
          Death_date: serial('2025-09-03'),
        },
      },
      { row: 4, cells: [formulaCell('=(ROW()+3)&"D"', 'N7D')] },
    ],
    SamplingDay_data: [
      // 375004 typed for 21/9/2026; the note says the day.
      {
        row: 2,
        values: {
          Date: 375004,
          Location: 'Ikiam',
          Purpose: 'Monitoring',
          Collectors_initials: 'AA',
          Notes: '21/9/2026 AA: Sunny',
        },
      },
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Collection_data', 'Insectary_data', 'SamplingDay_data'] });
  return store;
}
const find = (out, kind, sheet, row, field) =>
  out.issues.find(i => i.kind === kind && i.sheet === sheet && i.row === row && (!field || i.field === field));

test('check_data finds each kind of inconsistency, with the row, the value and the obvious fix', async () => {
  const store = await fixture();
  const out = checkData(store, { limit: 500 });
  assert.deepEqual(out.counts, {
    repeat: 2,
    cam_cross: 2,
    list: 1,
    insectary_link: 4,
    link_mismatch: 2,
    date_order: 2,
    future_date: 1,
    bad_date: 1,
    missing_sample: 3,
    mark_reuse: 1,
  });
  // A number that is no date, flagged once (not as a date in the future), with the day its note gives.
  const broken = find(out, 'bad_date', 'SamplingDay_data', 2, 'Date');
  assert.match(broken.problem, /375004 \(año 2926\), que no es una fecha/);
  assert.deepEqual(broken.fix.values, { Date: '2026-09-21' });
  // Tubes are unique across the workbook.
  assert.match(find(out, 'repeat', 'Collection_data', 9, 'Tube_1_id').problem, /FS00000010 también está en Insectary_data fila 2/);
  // A CAM given to a field butterfly and to an insectary butterfly.
  assert.match(find(out, 'cam_cross', 'Collection_data', 5, 'CAM_ID').problem, /Insectary_data fila 2 \(N5D\)/);
  // Outside the strict Sex list, with the obvious spelling as fix.
  const sex = find(out, 'list', 'Collection_data', 5, 'Sex');
  assert.equal(sex.value, 'female_?');
  assert.deepEqual(sex.fix, { recordId: sex.recordId, values: { Sex: 'female ?' } });
  // Both directions of the Collection ↔ Insectary link.
  assert.match(find(out, 'insectary_link', 'Collection_data', 3).problem, /N9Z no tiene fila en Insectary_data/);
  assert.match(find(out, 'insectary_link', 'Collection_data', 4).problem, /sin Insectary_ID/);
  assert.match(find(out, 'insectary_link', 'Collection_data', 10).problem, /N7D en Insectary_data \(fila 4\) está vacía/);
  assert.match(find(out, 'insectary_link', 'Insectary_data', 3).problem, /N6D sin fila en Collection_data/);
  // The same butterfly described differently in the two sheets.
  const species = find(out, 'link_mismatch', 'Insectary_data', 2, 'SPECIES');
  assert.equal(species.related[0].row, 2);
  assert.equal(species.related[0].sheet, 'Collection_data');
  assert.ok(find(out, 'link_mismatch', 'Insectary_data', 2, 'Sex'));
  // Entered the insectary before it was caught; died a year before it entered (a year typed wrong).
  assert.ok(find(out, 'date_order', 'Insectary_data', 2, 'Intro2Insectary_date'));
  const death = find(out, 'date_order', 'Insectary_data', 3, 'Death_date');
  assert.deepEqual(death.fix.values, { Death_date: '2026-09-03' });
  const future = find(out, 'future_date', 'Collection_data', 6, 'Collection_date');
  assert.equal(future.value, isoOf(slipped));
  assert.deepEqual(future.fix.values, { Collection_date: isoOf(today - 20) });
  assert.ok(find(out, 'missing_sample', 'Collection_data', 5, 'Tube_1_id'));
  assert.ok(find(out, 'missing_sample', 'Collection_data', 6, 'CAM_ID'));
  // The later butterfly with B40 is the one listed; marks are compared without case.
  const mark = find(out, 'mark_reuse', 'Collection_data', 8, 'FieldMark_ID');
  assert.match(mark.problem, /B40 ya se usó para Oleria gunilla \(fila 7, 2026-05-10\)/);
  store.close();
});

test('check_data pages and filters by sheet and kind, and is computed once per state of the copy', async () => {
  const store = await fixture();
  const first = checkData(store, { kind: 'insectary_link,link_mismatch', limit: 2 });
  assert.equal(first.total, 6);
  assert.equal(first.issues.length, 2);
  const second = checkData(store, { kind: 'insectary_link,link_mismatch', limit: 2, offset: 2 });
  assert.notDeepEqual(second.issues[0], first.issues[0]);
  const insectary = checkData(store, { sheet: 'Insectary_data' });
  assert.ok(insectary.issues.every(i => i.sheet === 'Insectary_data'));
  assert.equal(insectary.sheets.Collection_data, 11);
  assert.throws(() => checkData(store, { kind: 'nonsense' }), { code: 'INVALID_KIND' });
  assert.throws(() => checkData(store, { sheet: 'Nope' }), { code: 'MODULE_NOT_FOUND' });
  // Cached until the local copy changes.
  assert.equal(allIssues(store), allIssues(store));
  const user = { id: 'e1', username: 'editor', role: 'editor' };
  const sex = checkData(store, { kind: 'list' }).issues[0];
  await store.applyProposal([{ recordId: sex.recordId, values: sex.fix.values }], { user, requestId: randomUUID() });
  assert.equal(checkData(store, { kind: 'list' }).total, 0);
  store.close();
});

test('the assistant follows check_data with propose_changes, and the person applies the fix', async () => {
  const store = await fixture();
  const assistant = createAssistant({ store, config: { claude: {} } });
  store.db
    .prepare(
      "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','ana','Ana','editor','s','h',1,'2026-01-01')",
    )
    .run();
  const token = 'token-for-ana';
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update(token).digest('hex'), 'u1');
  const call = async (name, args) => {
    const out = await assistant.mcp(
      { authorization: `Bearer ${token}` },
      { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } },
    );
    return JSON.parse(out.body.result.content[0].text);
  };
  const tools = (await assistant.mcp({ authorization: `Bearer ${token}` }, { jsonrpc: '2.0', id: 1, method: 'tools/list' }))
    .body.result.tools;
  assert.ok(['check_data', 'queue_wikiloc', 'get_walk'].every(n => tools.some(t => t.name === n)));

  const found = await call('check_data', { kind: 'date_order,list' });
  const fixes = found.issues.filter(i => i.fix);
  assert.equal(fixes.length, 2);
  const proposed = await call('propose_changes', {
    reason: 'Arreglos obvios de check_data',
    changes: fixes.map(i => ({ ...i.fix, note: i.problem })),
  });
  assert.equal(proposed.rows, 2);
  const user = { id: 'u1', username: 'ana', role: 'editor' };
  const listed = await assistant.handle({ method: 'GET', path: '/api/chat/proposals', user, query: {} });
  assert.equal(listed.body.proposals[0].source, 'T3 Code');
  const applied = await assistant.handle({
    method: 'POST',
    path: `/api/chat/proposals/${proposed.proposalId}/apply`,
    user,
    body: { requestId: randomUUID() },
  });
  assert.equal(applied.body.status, 'applied');
  assert.equal(store.getRecordBySheetRow('Insectary_data', 3).values.Death_date, serial('2026-09-03'));
  assert.equal(store.getRecordBySheetRow('Collection_data', 5).values.Sex, 'female ?');
  assert.equal((await call('check_data', { kind: 'list' })).total, 0);

  // The same from the app: "Proponer arreglos" in Revisión de datos.
  const future = checkData(store, { kind: 'future_date' }).issues[0];
  const fromChecks = await assistant.handle({
    method: 'POST',
    path: '/api/chat/proposals/from-checks',
    user,
    body: { ids: [future.id] },
  });
  assert.equal(fromChecks.status, 201);
  const pending = await assistant.handle({ method: 'GET', path: '/api/chat/proposals', user, query: {} });
  assert.equal(pending.body.proposals[0].source, 'Revisión de datos');
  assert.deepEqual(pending.body.proposals[0].changes[0].values, { Collection_date: serial(isoOf(today - 20)) });
  store.close();
});
