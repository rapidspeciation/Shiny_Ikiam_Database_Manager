import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash, randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { allIssues, checkData } from '../server/checks.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { linkCapture, saveTrack } from '../server/monitoring.mjs';
import { alerts } from '../server/alerts.mjs';
import { asCell, moduleMap } from '../server/schema.mjs';
import { proposalSampleWarnings, sampleGap } from '../server/preserved.mjs';

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
/** A descriptor (server/messages.mjs) filled in Spanish, as the app does in Spanish: it must give the problem's text. */
const spanish = m => {
  const value = v => (Array.isArray(v) ? v.map(value).join(', ') : v && typeof v === 'object' ? spanish(v) : String(v ?? ''));
  return m.key.replace(/\{(\w+)\}/g, (all, k) => (m.vars && k in m.vars ? value(m.vars[k]) : all));
};
const sameTexts = issues => {
  for (const i of issues) {
    assert.ok(i.problemMsg, i.id);
    assert.equal(spanish(i.problemMsg), i.problem);
    if (i.fixNote) assert.equal(spanish(i.fixNoteMsg), i.fixNote);
  }
};

const find = (out, kind, sheet, row, field) =>
  out.issues.find(i => i.kind === kind && i.sheet === sheet && i.row === row && (!field || i.field === field));

test('check_data finds each kind of inconsistency, with the row, the value and the obvious fix', async () => {
  const store = await fixture();
  const out = checkData(store, { limit: 500 });
  // Each problem comes with its descriptor for the interface language, built from the same template.
  sameTexts(out.issues);
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
    preserved_na: 0,
    stage_adult: 0,
    mark_reuse: 1,
    walk_doubt: 0,
    photo_camid: 0,
    photo_extra: 0,
    envelope_sex: 0,
    envelope_species: 0,
    photo_missing: 0,
    ai_species: 0,
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
  const assistant = createAssistant({ store, config: {} });
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

test('walk_doubt lists the Wikiloc points stored without a row, until a person pairs them in Dudas', async () => {
  const day = { Collection_date: serial('2025-09-08'), Collector: 'AA - Alex Arias', Purpose: 'Monitoring', Collection_location: 'Ikiam' };
  const sheets = new LocalSheets({
    Collection_data: [
      { row: 2, values: { ...day, Collection_time: 554 / 1440, SPECIES: 'Hypothyris euclea', Sex: 'female', FieldMark_ID: 'NA' } },
      { row: 3, values: { ...day, Collection_time: 554 / 1440, SPECIES: 'Eresia eunice', Sex: 'female', FieldMark_ID: 'NA' } },
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Collection_data'] });
  const editor = { id: 'e1', username: 'editor', role: 'editor' };
  // As Pasar al mapa stores them: two notes that tie for the two rows, and one with no row.
  const point = (text, lon, minutes, sex) => ({ lat: -0.95, lon, text, minutes, sex, doubt: true });
  const { track } = saveTrack(
    store,
    {
      requestId: randomUUID(),
      date: '2025-09-08',
      collector: 'AA - Alex Arias',
      name: 'Monitoreo 8/9/2025',
      track: [],
      captures: [point('9:14 sol female 0.3m', -77.861, 554, 'female'), point('9:14 sol female 1m', -77.862, 554, 'female'), point('Planta', -77.863, null, null)],
    },
    editor,
  );
  const out = checkData(store, { kind: 'walk_doubt' });
  sameTexts(out.issues);
  assert.equal(out.total, 3);
  assert.equal(out.counts.walk_doubt, 3);
  const [tie, , plant] = out.issues;
  assert.equal(tie.sheet, 'Collection_data');
  assert.equal(tie.value, '9:14 sol female 0.3m');
  assert.equal(tie.label, 'Wikiloc 08/09/2025 AA');
  assert.equal(tie.link, '#/monitoreo?vista=dudas');
  assert.deepEqual(tie.walk, { trackId: track.id, index: 0, date: '2025-09-08', collector: 'AA - Alex Arias', name: 'Monitoreo 8/9/2025', wikiloc: null });
  assert.match(tie.problem, /del recorrido del 08\/09\/2025 \(AA - Alex Arias\) guardado sin fila: empate con otra fila\. Puede ser la fila \d \(/);
  assert.match(tie.problem, /Emparéjalo en Monitoreo → Dudas/);
  assert.equal(tie.fix, undefined);
  assert.deepEqual(tie.related.map(r => r.row).sort(), [2, 3]);
  // Nothing fits "Planta": no row, the free rows of the day to choose from.
  assert.equal(plant.row, null);
  assert.equal(plant.recordId, null);
  assert.match(plant.problem, /ninguna fila encaja\. Puede ser la fila 2 .* o la fila 3/);
  // Paired in Dudas: gone from the list at once (the scan follows the stored walks too).
  linkCapture(store, track.id, { index: 2, recordId: null });
  assert.deepEqual(
    checkData(store, { kind: 'walk_doubt' }).issues.map(i => i.value),
    ['9:14 sol female 0.3m', '9:14 sol female 1m'],
  );
  store.close();
});

test('a tube listed in another sheet for the same butterfly is a reference, not a repeat', async () => {
  const sheets = new LocalSheets({
    Collection_data: [
      { row: 2, values: { CAM_ID: 'CAM070918', Tube_1_id: 'FD30881820', SPECIES: 'Thyridia psidii' } },
      { row: 3, values: { CAM_ID: 'CAM070919', Tube_1_id: 'FD30881999', SPECIES: 'Thyridia psidii' } },
    ],
    Barcoding_DNA: [
      // The same butterfly's tube: fine.
      { row: 2, values: { CAM_ID: 'CAM070918', Tube_1_id: 'FD30881820' } },
      // Another butterfly's tube under a different CAM: a real repeat.
      { row: 3, values: { CAM_ID: 'CAM070111', Tube_1_id: 'FD30881999' } },
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Collection_data', 'Barcoding_DNA'] });
  const repeats = allIssues(store).issues.filter(i => i.kind === 'repeat').map(i => i.value);
  assert.ok(!repeats.includes('FD30881820'));
  assert.ok(repeats.includes('FD30881999'));
  store.close();
});

/** A seed row from field values, some cells as formulas ({ field: [formula, value] }). */
const seedRow = (sheet, row, values, formulas = {}) => {
  const cells = [];
  for (const f of moduleMap.get(sheet).fields) {
    if (f.key in formulas) cells[f.column] = formulaCell(...formulas[f.key]);
    else if (f.key in values) cells[f.column] = asCell(values[f.key]);
  }
  return { row, cells };
};
async function preservedFixture() {
  const insectary = (row, values, formulas) =>
    seedRow('Insectary_data', row, { SPECIES: 'Mechanitis lysimnia', Sex: 'male', ...values }, formulas);
  const sheets = new LocalSheets({
    Insectary_data: [
      // Killed for pheromones and preserved: no CAM, no tube (it came through a proposal).
      insectary(2, {
        Insectary_ID: 'D5D',
        Death_date: today - 10,
        Death_cause: 'Killed_Preserved',
        Preserved_Dead_Alive: 'Alive',
        Notes_Insectary_data: '1/10/26 FCH: Pheromones. Killed / preserved',
      }),
      // Wings only: its CAM, and Tube NA on purpose.
      insectary(3, {
        Insectary_ID: 'W1W',
        Death_date: today - 300,
        Death_cause: 'Unknown',
        Preserved_Dead_Alive: 'Dead',
        CAM_ID: 'CAM000100',
        Tube_1_id: 'NA',
      }),
      // Its body was lost: nothing left to number.
      insectary(4, {
        Insectary_ID: 'L1L',
        Death_date: today - 30,
        Preserved_Dead_Alive: 'Dead',
        Tube_1_tissue: 'WHOLE_ORGANISM',
        Location_body: 'Lost',
      }),
      // Not preserved: NA and NOT_COLLECTED.
      insectary(5, {
        Insectary_ID: 'N1N',
        Death_date: today - 15,
        Death_cause: 'Unknown',
        Preserved_Dead_Alive: 'NA',
        CAM_ID: 'NA',
        Tube_1_id: 'NA',
        Tube_1_tissue: 'NOT_COLLECTED',
        Preservation_medium: 'NOT_COLLECTED',
      }),
      // Died; its cause is written later, in the app.
      insectary(6, {
        Insectary_ID: 'K1K',
        Death_date: today - 20,
        Death_cause: 'Unknown',
        Preserved_Dead_Alive: 'NA',
        CAM_ID: 'NA',
        Tube_1_id: 'NA',
      }),
      // A tube still to find, long ago.
      insectary(7, {
        Insectary_ID: 'B1B',
        Death_date: today - 400,
        Preserved_Dead_Alive: 'Dead',
        CAM_ID: 'CAM000101',
        Tube_1_id: 'BUSCAR ',
        Tube_1_tissue: 'WHOLE_ORGANISM',
        Notes_Insectary_data: '13-12-24 MJS: Individual preserved, put the tube inside the shipper before registering it',
      }),
      // The CAM is a formula: the sheet's business.
      insectary(
        8,
        { Insectary_ID: 'F1F', Death_date: today - 40, Preserved_Dead_Alive: 'Alive', Preservation_medium: 'Flash frozen', Tube_1_id: 'NA' },
        { CAM_ID: ['=IF(TRUE,"BUSCAR")', 'BUSCAR'] },
      ),
      // Only a preservation date: not enough to say it was preserved.
      insectary(9, { Insectary_ID: 'P1P', Death_date: today - 100, Preservation_date: today - 100 }),
      // Alive.
      insectary(10, { Insectary_ID: 'A1A', Intro2Insectary_date: today - 5 }),
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data'] });
  store.db
    .prepare(
      "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','ana','Ana','editor','s','h',1,'2026-01-01')",
    )
    .run();
  return store;
}
const ana = { id: 'u1', username: 'ana', displayName: 'Ana', role: 'editor' };
const dayFirst = serial => isoOf(serial).split('-').reverse().join('/');

test('missing_sample and preserved_na: an insectary butterfly preserved by its cells, without CAM or tube, and the team is asked', async () => {
  const store = await preservedFixture();
  const K1K = store.getRecordBySheetRow('Insectary_data', 6);
  await store.applyProposal([{ recordId: K1K.id, values: { Death_cause: 'Killed_Preserved' } }], { user: ana, requestId: randomUUID() });

  const out = checkData(store, { kind: 'missing_sample,preserved_na', limit: 50 });
  sameTexts(out.issues);
  assert.deepEqual(out.issues.map(i => `${i.kind} ${i.label} ${i.field}`).sort(), [
    'missing_sample B1B Tube_1_id',
    'missing_sample D5D CAM_ID',
    'missing_sample D5D Tube_1_id',
    'preserved_na K1K Death_cause',
  ]);
  const d5d = find(out, 'missing_sample', 'Insectary_data', 2, 'CAM_ID');
  assert.equal(d5d.problem, 'Preservada (Death_cause Killed_Preserved, Preserved_Dead_Alive Alive) sin CAM_ID');
  // Nobody is named: the issue says what is missing, not whom to ask.
  assert.equal(d5d.ask, undefined);
  const k1k = find(out, 'preserved_na', 'Insectary_data', 6);
  assert.match(k1k.problem, /CAM_ID y los tubos dicen NA \(no preservada\)$/);

  // Alerts: the recent ones, asking the team, with a link to the row; the full list apart.
  const data = alerts(store);
  assert.deepEqual(
    data.missingSamples.map(s => [s.id, s.kind, s.missing]),
    [
      ['D5D', 'missing_sample', ['CAM_ID', 'Tube_1_id']],
      ['K1K', 'preserved_na', ['CAM_ID', 'Tube_1_id']],
      ['B1B', 'missing_sample', ['Tube_1_id']],
    ],
  );
  const D5D = store.getRecordBySheetRow('Insectary_data', 2);
  const alert = data.alerts.find(a => a.id === `sample:${D5D.id}`);
  assert.equal(alert.level, 'warn');
  assert.equal(alert.text, `D5D (Mechanitis lysimnia) preservada el ${dayFirst(today - 10)} sin CAM/tubo — pregunta al equipo`);
  assert.equal(alert.textMsg.key, '{id} ({species}) preservada el {date} sin CAM/tubo — pregunta al equipo');
  assert.equal(alert.link, '#/tablas?hoja=Insectary_data&buscar=D5D');
  assert.match(data.alerts.find(a => a.id === `sample:${K1K.id}`).text, /^K1K .*Killed_Preserved.*NA — pregunta al equipo$/);
  // Older than 180 days: in the list, not an alert.
  assert.ok(!data.alerts.some(a => a.text.startsWith('B1B')));

  // Filled: the notice goes.
  await store.applyProposal(
    [{ recordId: D5D.id, values: { CAM_ID: 'CAM000103', Tube_1_id: 'FS00000103', Tube_1_tissue: 'WHOLE_ORGANISM' } }],
    { user: ana, requestId: randomUUID() },
  );
  assert.ok(!alerts(store).alerts.some(a => a.id === `sample:${D5D.id}`));
  assert.equal(checkData(store, { kind: 'missing_sample' }).total, 1);
  store.close();
});

test('sampleGap: what counts as preserved, and what counts as missing', () => {
  const gap = values => sampleGap(values)?.kind ?? null;
  assert.equal(gap({ Death_cause: 'Killed_Preserved', CAM_ID: 'CAM000001', Tube_1_id: 'FS00000001' }), null);
  assert.equal(gap({ Preserved_Dead_Alive: 'Dead', CAM_ID: 'CAM000001', Tube_1_id: '' }), 'missing_sample');
  assert.equal(gap({ Tube_2_tissue: 'WHOLE_ORGANISM', CAM_ID: 'CAM000001', Tube_1_tissue: 'NOT_COLLECTED' }), null);
  assert.equal(gap({ Preservation_medium: 'Ethanol', CAM_ID: '', Tube_1_id: 'NA' }), 'missing_sample');
  assert.equal(gap({ Preservation_medium: 'NOT_COLLECTED', Preservation_date: 46000 }), null);
  // Killed_Preserved with NA everywhere: the cause and the cells disagree; with a tube, it is the wings-only kind.
  assert.equal(gap({ Death_cause: 'Killed_Preserved', CAM_ID: 'NA', Tube_1_id: 'NA' }), 'preserved_na');
  assert.equal(gap({ Death_cause: 'Killed_Preserved', CAM_ID: 'NA', Tube_1_id: 'NA', Tube_2_id: 'FS00000002' }), null);
  // Notes never decide it.
  assert.equal(gap({ Notes_Insectary_data: '1/10/26 FCH: Killed / preserved' }), null);
});

test('a proposal that would leave a butterfly preserved without CAM or tube marks those cells before it is applied', async () => {
  const store = await preservedFixture();
  const A1A = store.getRecordBySheetRow('Insectary_data', 10);
  const killed = {
    sheet: 'Insectary_data',
    recordId: A1A.id,
    values: { Death_date: today, Death_cause: 'Killed_Preserved', Preserved_Dead_Alive: 'Alive' },
  };
  assert.deepEqual(Object.keys(proposalSampleWarnings(killed, A1A)), ['CAM_ID', 'Tube_1_id']);
  assert.equal(proposalSampleWarnings(killed, A1A).CAM_ID.text, 'Preservada sin CAM_ID: pregunta al equipo');
  const filled = { ...killed, values: { ...killed.values, CAM_ID: 'CAM000104', Tube_1_id: 'FS00000104' } };
  assert.equal(proposalSampleWarnings(filled, A1A), null);
  // A note on an old row with a gap: that gap is Revisión's, not this proposal's.
  const B1B = store.getRecordBySheetRow('Insectary_data', 7);
  assert.equal(proposalSampleWarnings({ sheet: 'Insectary_data', recordId: B1B.id, values: { Notes_Insectary_data: 'x' } }, B1B), null);
  assert.ok(proposalSampleWarnings({ sheet: 'Insectary_data', create: true, values: { Death_cause: 'Killed_Preserved' } }, null));

  // Through the assistant: its answer says so, and the table shows those cells, marked.
  const assistant = createAssistant({ store, config: {} });
  const token = 'token-for-ana';
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update(token).digest('hex'), 'u1');
  const out = await assistant.mcp(
    { authorization: `Bearer ${token}` },
    {
      jsonrpc: '2.0',
      id: 1,
      method: 'tools/call',
      params: {
        name: 'propose_changes',
        arguments: {
          reason: 'A1A killed for pheromones',
          changes: [{ recordId: A1A.id, values: { Death_date: isoOf(today), Death_cause: 'Killed_Preserved', Preserved_Dead_Alive: 'Alive' } }],
        },
      },
    },
  );
  const proposed = JSON.parse(out.body.result.content[0].text);
  assert.deepEqual(proposed.preservedWithoutSample, [{ index: 0, label: 'A1A', missing: ['CAM_ID', 'Tube_1_id'] }]);
  const listed = await assistant.handle({ method: 'GET', path: '/api/chat/proposals', user: ana, query: {} });
  const view = listed.body.proposals.find(p => p.id === proposed.proposalId);
  assert.deepEqual(Object.keys(view.changes[0].warnings), ['CAM_ID', 'Tube_1_id']);
  assert.ok(view.fields.includes('CAM_ID') && view.fields.includes('Tube_1_id'));
  store.close();
});

test('stage_adult: a date in Intro2Insectary_date with an egg, larva or pupa stage; an empty stage or an Adult is not flagged', async () => {
  const day = serial('2026-10-04');
  const sheets = new LocalSheets({
    Insectary_data: [
      { row: 2, values: { Insectary_ID: 'A1A', Intro2Insectary_date: day, LIFESTAGE: '3rd instar larva' } },
      { row: 3, values: { Insectary_ID: 'A2A', Intro2Insectary_date: day, LIFESTAGE: 'Pupa day 4' } },
      { row: 4, values: { Insectary_ID: 'A3A', Intro2Insectary_date: day, LIFESTAGE: 'Egg' } },
      { row: 5, values: { Insectary_ID: 'A4A', Intro2Insectary_date: day } },
      { row: 6, values: { Insectary_ID: 'A5A', Intro2Insectary_date: day, LIFESTAGE: 'Adult' } },
      { row: 7, values: { Insectary_ID: 'A6A', Intro2Insectary_date: 'NA', LIFESTAGE: 'Pre-pupa' } },
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  try {
    await store.sync({ sheets: ['Insectary_data'] });
    const out = checkData(store, { kind: 'stage_adult', limit: 50 });
    sameTexts(out.issues);
    assert.deepEqual(out.issues.map(i => `${i.label} ${i.field}`).sort(), ['A1A LIFESTAGE', 'A2A LIFESTAGE', 'A3A LIFESTAGE']);
    assert.match(out.issues.find(i => i.label === 'A2A').problem, /Pupa day 4.*2026-10-04/);
  } finally {
    store.close();
  }
});
