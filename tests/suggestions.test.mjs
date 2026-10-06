import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash, randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { moduleMap } from '../server/schema.mjs';
import { allIssues } from '../server/checks.mjs';
import { allSuggestions, registerSource, suggestionPage, suggestionsCsv } from '../server/suggestions/index.mjs';
import { solvedFindings } from '../server/findings.mjs';
import { alerts } from '../server/alerts.mjs';
import { createAssistant } from '../server/assistant.mjs';

const EPOCH = Date.UTC(1899, 11, 30);
const serial = iso => Math.round((Date.parse(`${iso}T00:00:00Z`) - EPOCH) / 864e5);
const today = serial(new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date()));
const isoOf = n => new Date(EPOCH + n * 864e5).toISOString().slice(0, 10);
const column = (sheet, key) => moduleMap.get(sheet).fields.find(f => f.key === key).column;
const formulaCell = (formula, value) => ({
  userEnteredValue: { formulaValue: formula },
  effectiveValue: typeof value === 'number' ? { numberValue: value } : { stringValue: value },
});
const cam = n => `CAM${String(78000 + n).padStart(6, '0')}`;

/**
 * A small workbook with one case of each source: 60 decided Pedigree rows that
 * F1/F2_MutationRate lists (so "same Insectary_ID and CAM" is measured as Yes),
 * an undecided one, a "YES"; a run of tubes typed without their 0; a
 * SamplingDay row with year 2926; a wild butterfly whose two rows disagree on
 * sex; a Collection row without the lookups of the rows above; a CAM range of
 * Lists nearly used up; and a species with 31 preserved at Ikiam.
 */
async function fixture() {
  const insectary = [];
  const pedigree = [];
  for (let i = 0; i < 60; i++) {
    insectary.push({
      row: 2 + i,
      values: {
        Insectary_ID: `P${i}A`,
        Wild_Reared: 'Reared',
        SPECIES: 'Mechanitis lysimnia',
        Death_date: today - 30,
        Research_purpose: 'F1/F2 mutation rate',
        Pedigree: 'Yes',
        CAM_ID: cam(i),
        Tube_1_id: `FS${50848900 + i}`,
      },
    });
    pedigree.push({ row: 2 + i, values: { Insectary_ID: `P${i}A`, Generation: 'F2', CAM_ID: cam(i) } });
  }
  pedigree.push({ row: 62, values: { Insectary_ID: 'Q1A', Generation: 'F2', CAM_ID: cam(60) } });
  insectary.push(
    // Undecided, and listed in F1/F2_MutationRate with its CAM: likely Yes.
    {
      row: 62,
      values: {
        Insectary_ID: 'Q1A',
        Wild_Reared: 'Reared',
        Death_date: today - 5,
        Research_purpose: 'F1/F2 mutation rate',
        Pedigree: 'YES or NO',
        CAM_ID: cam(60),
        Tube_1_id: 'FS50848960',
      },
    },
    // Undecided, nothing about it anywhere: no value.
    {
      row: 63,
      values: { Insectary_ID: 'Q2A', Wild_Reared: 'Reared', Death_date: today - 5, Research_purpose: 'F1/F2 mutation rate', Pedigree: 'YES or NO' },
    },
    // "YES" typed for Yes.
    { row: 64, values: { Insectary_ID: 'Q3A', Wild_Reared: 'Reared', Death_date: today - 5, Pedigree: 'YES' } },
    // Tubes of one day typed without the 0 of FS50848…, between tubes of that rack.
    { row: 65, values: { Insectary_ID: 'Q4A', Tube_1_id: 'FS5848961', SPECIES: 'Mechanitis lysimnia' } },
    { row: 66, values: { Insectary_ID: 'Q5A', Tube_1_id: 'FS5848962', SPECIES: 'Mechanitis lysimnia' } },
    { row: 67, values: { Insectary_ID: 'Q6A', Tube_1_id: 'FS5848963', SPECIES: 'Mechanitis lysimnia' } },
    { row: 68, values: { Insectary_ID: 'Q7A', Tube_1_id: 'FS50848964', SPECIES: 'Mechanitis lysimnia' } },
    // A wild butterfly whose collection row says female.
    {
      row: 69,
      values: {
        Insectary_ID: 'K1A',
        Wild_Reared: 'Wild-caught',
        SPECIES: 'Ithomia salapia',
        Sex: 'male',
        Intro2Insectary_date: today - 3,
        Death_date: today - 1,
        Preservation_medium: 'Flash frozen',
      },
    },
  );
  const collection = [
    // Sent to the insectary: the Insectary row above disagrees on sex.
    {
      row: 2,
      values: {
        Release_Collect: 'Collected_Sent2Insectary',
        Insectary_ID: 'K1A',
        SPECIES: 'Ithomia salapia',
        Sex: 'female',
        Collection_date: today - 3,
      },
    },
    // Sent to the insectary after it, without the lookups row 2 has (added below as formulas).
    { row: 3, values: { Release_Collect: 'Collected_Sent2Insectary', Insectary_ID: 'K1A', SPECIES: 'Ithomia salapia', Sex: 'female', Collection_date: today - 3 } },
  ];
  // 31 Oleria tigilla preserved at Ikiam (the 30th ten days ago), 27 Napeogenes inachia.
  for (let i = 0; i < 31; i++)
    collection.push({
      row: 4 + i,
      values: {
        Release_Collect: 'Collected_Preserved',
        SPECIES: 'Oleria tigilla',
        Tribe: 'Ithomiini',
        Collection_location: i % 5 ? 'Ikiam' : 'Casa de Lin',
        Collection_date: today - 40 + i,
      },
    });
  for (let i = 0; i < 27; i++)
    collection.push({
      row: 35 + i,
      values: {
        Release_Collect: 'Collected_Preserved',
        SPECIES: 'Napeogenes inachia',
        Tribe: 'Ithomiini',
        Collection_location: 'Mariposario Ikiam',
        Collection_date: today - 100 + i,
      },
    });
  // Heliconius are not Ithomiini: never counted.
  for (let i = 0; i < 35; i++)
    collection.push({
      row: 62 + i,
      values: { Release_Collect: 'Collected_Preserved', SPECIES: 'Heliconius numata', Tribe: 'Heliconiini', Collection_location: 'Ikiam', Collection_date: today - 50 },
    });
  const sheets = new LocalSheets({
    Insectary_data: insectary,
    Collection_data: collection,
    'F1/F2_MutationRate': pedigree,
    // 2926 typed for 2026, among days of September 2026.
    SamplingDay_data: [
      { row: 2, values: { Date: serial('2026-09-19'), Location: 'Ikiam' } },
      { row: 3, values: { Date: serial('2926-09-21'), Location: 'Ikiam' } },
      { row: 4, values: { Date: serial('2026-09-23'), Location: 'Ikiam' } },
    ],
    // The insectary's CAM range: 100 CAMs, 61 used.
    Lists: Array.from({ length: 100 }, (_, i) => ({ row: 2 + i, values: { 'InsectaryWild&Reared_CAMid': cam(i) } })),
  });
  // Row 2 reads Death_date… from Insectary_data; row 3 (typed later) does not.
  const row2 = sheets.rows.get('Collection_data').find(r => r.row === 2);
  for (const [field, letter] of [
    ['Death_date', 'I'],
    ['Preservation_date', 'M'],
    ['Preservation_medium', 'AA'],
    ['Preserved_dead_alive', 'AB'],
  ])
    row2.cells[column('Collection_data', field)] = formulaCell(
      `=XLOOKUP(D2, Insectary_data!A:A, Insectary_data!${letter}:${letter},"")`,
      field === 'Death_date' ? today - 1 : '',
    );
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Collection_data', 'F1/F2_MutationRate', 'SamplingDay_data', 'Lists'] });
  store.db
    .prepare(
      "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','ana','Ana','editor','s','h',1,'2026-01-01')",
    )
    .run();
  return store;
}
const one = (items, source, sheet, row, field) =>
  items.find(s => s.source === source && s.sheet === sheet && s.row === row && (!field || s.field === field));

test('each source suggests what the workbook shows, with an honest certainty and the reason', async () => {
  const store = await fixture();
  const { items } = await allSuggestions(store);
  for (const s of items) {
    assert.ok(['certain', 'likely', 'check'].includes(s.certainty), s.key);
    assert.ok(s.reason && s.reasonMsg, s.key);
    assert.equal(s.key, `${s.source}:${s.recordId}:${s.field}`);
  }

  // Pedigree: the class "listed in F1/F2 with its CAM" was decided Yes 60 times out of 60.
  const listed = one(items, 'pedigree', 'Insectary_data', 62);
  assert.equal(listed.suggested, 'Yes');
  assert.equal(listed.certainty, 'likely');
  assert.match(listed.reason, /Yes en 60 de 60/);
  assert.equal(listed.related[0].sheet, 'F1/F2_MutationRate');
  const unknown = one(items, 'pedigree', 'Insectary_data', 63);
  assert.equal(unknown.suggested, null);
  assert.equal(unknown.certainty, 'check');
  assert.deepEqual([one(items, 'pedigree', 'Insectary_data', 64).suggested, one(items, 'pedigree', 'Insectary_data', 64).certainty], [
    'Yes',
    'certain',
  ]);
  // Decided rows get nothing.
  assert.equal(one(items, 'pedigree', 'Insectary_data', 2), undefined);

  // The run of tubes, read together: the 0 goes back, next to FS50848960 and FS50848964.
  const tubes = items.filter(s => s.source === 'tubes');
  assert.deepEqual(
    tubes.map(s => [s.row, s.current, s.suggested, s.certainty]),
    [
      [65, 'FS5848961', 'FS50848961', 'likely'],
      [66, 'FS5848962', 'FS50848962', 'likely'],
      [67, 'FS5848963', 'FS50848963', 'likely'],
    ],
  );

  // 2926 → 2026, the only reading near the rows around.
  const date = one(items, 'dates', 'SamplingDay_data', 3, 'Date');
  assert.deepEqual([date.suggested, date.certainty], ['2026-09-21', 'likely']);

  // The twin rows: nothing says which is right, so check; the insectary follows the collection.
  const twin = one(items, 'twins', 'Insectary_data', 69, 'Sex');
  assert.deepEqual([twin.current, twin.suggested, twin.certainty], ['male', 'female', 'check']);

  // Two rows with the lookups are too few to call them the team's formula (tests/formula-patterns.test.mjs
  // has the rows that are).
  assert.equal(one(items, 'formulas', 'Collection_data', 3, 'Preservation_medium'), undefined);
  store.close();
});

test('the page filters by source, certainty and sheet, counts each, and the CSV has every row', async () => {
  const store = await fixture();
  const all = await suggestionPage(store, {});
  const pedigree = all.sources.find(s => s.id === 'pedigree').counts;
  assert.deepEqual(pedigree, { certain: 1, likely: 1, check: 1, total: 3 });
  const likely = await suggestionPage(store, { source: 'tubes,pedigree', certainty: 'likely' });
  assert.ok(likely.items.every(s => ['tubes', 'pedigree'].includes(s.source) && s.certainty === 'likely'));
  assert.equal(likely.total, 4);
  // Surest first inside a source; first seen is kept for each.
  assert.ok(likely.items.every(s => s.firstSeen));
  const paged = await suggestionPage(store, { limit: 2, offset: 2 });
  assert.equal(paged.items.length, 2);
  await assert.rejects(suggestionPage(store, { source: 'nonsense' }), { code: 'INVALID_SOURCE' });
  await assert.rejects(suggestionPage(store, { certainty: 'maybe' }), { code: 'INVALID_CERTAINTY' });
  const csv = await suggestionsCsv(store, { source: 'pedigree' });
  const lines = csv.trim().split('\r\n');
  assert.equal(lines[0], 'source,certainty,sheet,row,label,field,current,suggested,manual,reason,recordId');
  assert.equal(lines.length, 4);
  // A formula's commas and quotes are quoted as CSV, and the formulas are counted by group: tests/formula-patterns.test.mjs.
  const tsv = await suggestionsCsv(store, { source: 'pedigree' }, { tsv: true });
  assert.equal(tsv.trim().split('\r\n')[1].split('\t').length, 11);
  store.close();
});

test('a problem fixed in the sheet moves to Resueltos with when and who; one that comes back opens again', async () => {
  const store = await fixture();
  const before = allIssues(store).issues.find(i => i.kind === 'link_mismatch' && i.field === 'Sex');
  assert.ok(before);
  await allSuggestions(store);
  assert.equal(solvedFindings(store).total, 0);
  const user = { id: 'u1', username: 'ana', role: 'editor', displayName: 'Ana' };
  await store.applyProposal([{ recordId: before.recordId, values: { Sex: 'female' } }], { user, requestId: randomUUID() });

  allIssues(store);
  await allSuggestions(store);
  const solved = solvedFindings(store);
  const check = solved.items.find(i => i.type === 'check' && i.key === before.id);
  assert.ok(check, 'the check is solved');
  assert.equal(check.solved.user, 'Ana');
  assert.deepEqual([check.solved.field, check.solved.before, check.solved.after], ['Sex', 'male', 'female']);
  assert.ok(check.solved.actionId);
  assert.ok(check.firstSeen <= check.solvedAt);
  // The suggestion for the same cell is solved too.
  assert.ok(solved.items.some(i => i.type === 'suggestion' && i.kind === 'twins' && i.solved.user === 'Ana'));
  assert.equal(solvedFindings(store, { type: 'suggestion' }).items.every(i => i.type === 'suggestion'), true);
  assert.equal(solvedFindings(store, { q: 'K1A' }).total >= 1, true);

  // Back to male: open again, gone from the solved list.
  await store.applyProposal([{ recordId: before.recordId, values: { Sex: 'male' } }], { user, requestId: randomUUID() });
  allIssues(store);
  assert.equal(
    solvedFindings(store, { type: 'check' }).items.some(i => i.key === before.id),
    false,
  );
  store.close();
});

test('alerts: a CAM range in use running low, a species at 30 preserved, one close to it', async () => {
  const store = await fixture();
  const data = alerts(store);
  const pool = data.camPools.find(p => p.pool === 'InsectaryWild&Reared_CAMid');
  assert.equal(pool.ranges.length, 1);
  assert.deepEqual(
    [pool.ranges[0].first, pool.ranges[0].last, pool.ranges[0].used, pool.ranges[0].highest, pool.ranges[0].left, pool.level],
    ['CAM078000', 'CAM078099', 61, 'CAM078060', 39, 'low'],
  );
  assert.ok(data.alerts.some(a => a.id === 'cam:InsectaryWild&Reared_CAMid:CAM078000' && a.level === 'warn'));
  assert.match(data.alerts.find(a => a.id.startsWith('cam:')).text, /quedan 39 CAM/);

  const rule = data.preserveRule;
  const tigilla = rule.reached.find(s => s.species === 'Oleria tigilla');
  assert.equal(tigilla.preserved, 31);
  assert.equal(tigilla.reachedOn, isoOf(today - 40 + 29));
  assert.equal(tigilla.after, 1);
  assert.equal(tigilla.afterRows[0].row, 34);
  assert.ok(data.alerts.some(a => a.id === 'thirty:Oleria tigilla' && a.level === 'warn'));
  // Mariposario Ikiam counts; Heliconius (not Ithomiini) never does.
  assert.deepEqual(rule.close, [{ species: 'Napeogenes inachia', preserved: 27, left: 3, lastPreserved: isoOf(today - 74) }]);
  assert.ok(!rule.reached.some(s => /Heliconius/.test(s.species)));
  // Computed once per state of the copy.
  assert.equal(alerts(store), data);
  store.close();
});

test('the assistant reads the suggestions and the alerts; neither writes anything', async () => {
  const store = await fixture();
  const assistant = createAssistant({ store, config: {} });
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
  assert.ok(['list_suggested_edits', 'get_alerts'].every(n => tools.some(t => t.name === n)));
  const versions = () => store.db.prepare('SELECT sum(version) v FROM records').get().v;
  const before = versions();
  const tubes = await call('list_suggested_edits', { source: 'tubes' });
  assert.equal(tubes.total, 3);
  assert.equal(tubes.items[0].reasonMsg, undefined, 'the assistant reads the Spanish text');
  assert.ok(tubes.sources.some(s => s.id === 'pedigree' && s.describe));
  const got = await call('get_alerts', {});
  assert.ok(got.alerts.some(a => a.id === 'thirty:Oleria tigilla'));
  assert.equal(versions(), before);
  store.close();
});

test('a new source plugs into the registry; a malformed one is refused', async () => {
  assert.throws(() => registerSource({ id: 'bad id', suggest() {} }), /id/);
  assert.throws(() => registerSource({ id: 'tubes', suggest() {} }), /already/);
  registerSource({
    id: 'demo',
    title: 'Demo',
    describe: 'Demo',
    async suggest(ctx) {
      const row = ctx.observed('SamplingDay_data')[0];
      return [{ sheet: row.sheet, row: row.row, recordId: row.id, field: 'Location', current: 'Ikiam', suggested: null, certainty: 'check', reason: 'demo' }];
    },
  });
  const store = await fixture();
  const page = await suggestionPage(store, { source: 'demo' });
  assert.equal(page.total, 1);
  assert.equal(page.items[0].key, `demo:${page.items[0].recordId}:Location`);
  assert.equal(page.items[0].label, page.items[0].label || '');
  store.close();
});
