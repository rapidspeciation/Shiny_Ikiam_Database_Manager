import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { signIn } from './helpers/assistant.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { FIND_BUDGET, findRecords } from '../server/records-tool.mjs';
import { noteText } from '../server/notebook.mjs';

// The same values for many rows (propose_changes' bulk), the row limit of a proposal (500), and
// row queries that stay small enough for an MCP client.

const today = () => new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date());
/** 120 preserved larvae (rows 2–121; the first 5 with their Sex already) and 10 adults (rows 122–131). */
const LARVAE = Array.from({ length: 130 }, (_, i) => {
  const row = i + 2;
  const larva = i < 120;
  return {
    row,
    values: {
      Insectary_ID: `L${i}E`,
      SPECIES: 'Mechanitis lysimnia',
      ...(larva ? { LIFESTAGE: '3rd instar larva' } : { Sex: 'female' }),
      ...(larva && i < 5 ? { Sex: 'NOT_COLLECTED' } : {}),
      ...(i === 6 ? { Notes_Insectary_data: '1/10/26 BP: found on leaf' } : {}),
    },
  };
});

async function setup(seed = { Insectary_data: LARVAE }) {
  const sheets = new LocalSheets(seed);
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: Object.keys(seed) });
  const assistant = createAssistant({ store, config: {} });
  const { call } = signIn(store, assistant, { id: 'u-franz', displayName: 'Franz Chandi' });
  const row = n => store.getRecordBySheetRow('Insectary_data', n);
  const saved = id => {
    const p = store.db.prepare('SELECT * FROM ai_proposals WHERE id = ?').get(id);
    return { ...p, changes: JSON.parse(p.changes_json) };
  };
  return { store, assistant, call, row, saved };
}

test('bulk: the same values for the rows a filter picks, in one proposal of up to 500 rows', async () => {
  const { store, call, row, saved } = await setup();
  try {
    const out = await call('propose_changes', {
      reason: 'Larvas preservadas: Sex NOT_COLLECTED',
      bulk: [
        {
          sheet: 'Insectary_data',
          filters: { LIFESTAGE: { empty: false }, Sex: { empty: true } },
          set: { Sex: 'NOT_COLLECTED' },
          note: 'Larva preservada: sin sexo',
        },
      ],
    });
    assert.ok(!out.error, out.error);
    // 115 rows: one proposal with its link, the bulk preview instead of a long table.
    assert.equal(out.rows, 115);
    assert.ok(out.link.includes(out.proposalId));
    assert.ok(!out.proposals && !out.split && !out.table, 'not split, no long table');
    const [summary] = out.bulk;
    assert.equal(summary.matched, 115);
    assert.equal(summary.changed, 115);
    assert.equal(summary.preview.length, 5);
    assert.deepEqual(summary.preview[0], { row: 7, label: summary.preview[0].label, before: { Sex: null }, after: { Sex: 'NOT_COLLECTED' } });

    const proposal = saved(out.proposalId);
    assert.equal(proposal.reason, 'Larvas preservadas: Sex NOT_COLLECTED');
    assert.equal(proposal.changes.length, 115);
    // Ordinary rows: what the sheet had and the row's version, checked when applied.
    const change = proposal.changes[0];
    assert.deepEqual([change.recordId, change.before, change.values, change.note], [row(7).id, { Sex: null }, { Sex: 'NOT_COLLECTED' }, 'Larva preservada: sin sexo']);
    assert.equal(change.expectedVersion, row(7).version);

    const applied = await call('apply_proposal', { proposalId: out.proposalId });
    assert.equal(applied.rows, 115);
    assert.equal(row(7).values.Sex, 'NOT_COLLECTED');
    assert.equal(row(121).values.Sex, 'NOT_COLLECTED');
    assert.equal(row(123).values.Sex, 'female', 'adults are not picked');
  } finally {
    store.close();
  }
});

test('bulk: recordIds with filters, notes in the team form, rows listed in changes keep their values', async () => {
  const { store, call, row, saved } = await setup();
  try {
    const ids = [2, 3, 7, 8, 9, 125].map(n => row(n).id);
    const out = await call('propose_changes', {
      reason: 'Larvas del clutch',
      changes: [{ recordId: row(9).id, values: { Sex: 'NA' }, note: 'Visto en la foto' }],
      bulk: [{ sheet: 'Insectary_data', recordIds: [...ids, 'nope'], filters: { LIFESTAGE: '3rd instar larva' }, set: { Sex: 'NOT_COLLECTED', Notes_Insectary_data: 'sin sexo' } }],
    });
    assert.ok(!out.error, out.error);
    // Rows 2 and 3 already say NOT_COLLECTED but take the note; 125 is an adult (the filter leaves it out).
    assert.equal(out.rows, 5);
    assert.ok(out.proposalId);
    assert.deepEqual(out.bulk[0].notInSheet, ['nope']);
    assert.equal(out.bulk[0].matched, 5);
    assert.equal(out.table.length, 5, 'a short proposal still lists its rows');
    const changes = saved(out.proposalId).changes;
    const of = n => changes.find(c => c.recordId === row(n).id);
    const dated = text => noteText(text, { today: today(), initials: 'FC' });
    assert.equal(of(2).values.Notes_Insectary_data, dated('sin sexo'));
    assert.equal(of(8).values.Notes_Insectary_data, `1/10/26 BP: found on leaf | ${dated('sin sexo')}`);
    assert.equal(of(9).values.Sex, 'NA', 'the listed row keeps its own value');
    assert.equal(of(9).values.Notes_Insectary_data, dated('sin sexo'));
    assert.equal(of(9).note, 'Visto en la foto');

    // Rows that already hold every value are counted apart.
    const again = await call('propose_changes', {
      reason: 'x',
      bulk: [{ sheet: 'Insectary_data', recordIds: [row(2).id, row(7).id], set: { Sex: 'NOT_COLLECTED' } }],
    });
    assert.deepEqual([again.bulk[0].matched, again.bulk[0].changed, again.bulk[0].alreadySet], [2, 1, 1]);
  } finally {
    store.close();
  }
});

test('bulk and row limits: mistakes are said before any row is drafted', async () => {
  const { store, call, row } = await setup();
  try {
    const bulk = (group, extra = {}) => call('propose_changes', { reason: 'x', bulk: [{ sheet: 'Insectary_data', ...group }], ...extra });
    assert.match((await bulk({ filters: { LIFESTAGE: { empty: false } }, set: { Sex: 'hembra' } })).error, /^bulk\[0\]: Sex: «hembra» no está en la lista/);
    assert.match((await bulk({ filters: { Especie: 'x' }, set: { Sex: 'NA' } })).error, /^bulk\[0\]: filters: Unknown column Especie in Insectary_data; did you mean SPECIES\?/);
    assert.match((await bulk({ filters: { LIFESTAGE: { empty: false } }, set: { Sexo: 'NA' } })).error, /^bulk\[0\]: set: Unknown column Sexo in Insectary_data; did you mean Sex\?/);
    assert.match((await bulk({ set: { Sex: 'NA' } })).error, /Give recordIds and\/or filters/);
    assert.match((await bulk({ filters: { LIFESTAGE: { empty: false } }, set: { Sex: null } })).error, /null means no change/);
    // Rows listed one by one: at most 500, and the answer says what to do.
    const listed = Array.from({ length: 130 }, (_, i) => ({ recordId: row(i + 2).id, values: { Sex: 'NA' } }));
    const all = await call('propose_changes', { reason: 'x', changes: listed });
    assert.equal(all.rows, 130);
    assert.match(all.table, /get_proposal with full: true/, 'a long proposal says where its indexes are');
    assert.equal(store.db.prepare("SELECT count(*) n FROM ai_proposals WHERE status = 'pending'").get().n, 1);
    // Several groups picking the same rows count them once.
    const same = { sheet: 'Insectary_data', filters: { SPECIES: 'Mechanitis lysimnia' }, set: { Sex: 'NA' } };
    assert.equal((await call('propose_changes', { reason: 'x', bulk: [same, same, same, same] })).rows, 130);
  } finally {
    store.close();
  }
});

test('more than 500 rows: one proposal takes them only after those already holding the values are left out', async () => {
  const rows = Array.from({ length: 560 }, (_, i) => ({
    row: i + 2,
    values: { Insectary_ID: `R${i}E`, SPECIES: 'Oleria onega', ...(i < 100 ? { Sex: 'NOT_COLLECTED' } : {}) },
  }));
  const { store, call, row } = await setup({ Insectary_data: rows });
  try {
    const tooMany = await call('propose_changes', { reason: 'x', changes: rows.slice(0, 501).map(r => ({ recordId: row(r.row).id, values: { Sex: 'NA' } })) });
    assert.match(tooMany.error, /^At most 500 rows per proposal \(here 501\)/);
    const bulk = set => call('propose_changes', { reason: 'x', bulk: [{ sheet: 'Insectary_data', filters: { SPECIES: 'Oleria onega' }, set }] });
    assert.match((await bulk({ Sex: 'NA' })).error, /^560 rows to change\. One proposal takes at most 500 rows: narrow the filters/);
    // 100 rows already say NOT_COLLECTED: 460 to change.
    const out = await bulk({ Sex: 'NOT_COLLECTED' });
    assert.ok(!out.error, out.error);
    assert.deepEqual([out.rows, out.bulk[0].matched, out.bulk[0].alreadySet], [460, 560, 100]);
  } finally {
    store.close();
  }
});

test('a 500-row proposal goes to the page once: then, while it holds, only its digest', async () => {
  const rows = Array.from({ length: 520 }, (_, i) => ({ row: i + 2, values: { Insectary_ID: `R${i}E`, SPECIES: 'Oleria onega', CAM_ID: `CAM0${79000 + i}` } }));
  const { store, assistant, call, row } = await setup({ Insectary_data: rows });
  try {
    const out = await call('propose_changes', {
      reason: 'Sexo',
      changes: rows.slice(0, 500).map(r => ({ recordId: row(r.row).id, values: { Sex: 'NA' } })),
    });
    assert.equal(out.rows, 500);
    const user = { id: 'u-franz', username: 'franz', displayName: 'Franz Chandi', role: 'editor' };
    const list = async query =>
      (await assistant.handle({ method: 'GET', path: '/api/chat/proposals', body: {}, user, query: { all: '1', ...query } })).body;
    const first = await list();
    const [p] = first.proposals;
    assert.equal(p.changes.length, 500, 'every row; none between (they are all changed)');
    assert.match(p.digest, /^[\w-]{12}$/);
    const again = await list({ have: p.digest });
    assert.deepEqual(again.proposals, [{ id: p.id, digest: p.digest, same: true }]);
    assert.ok(JSON.stringify(again).length < 1000);
    // Changed: sent whole again, with another digest.
    await call('update_proposal', { proposalId: p.id, rows: [{ index: 0, values: { Sex: 'female' } }] });
    const changed = (await list({ have: p.digest })).proposals[0];
    assert.equal(changed.changes.length, 500);
    assert.notEqual(changed.digest, p.digest);
  } finally {
    store.close();
  }
});

test('find_records stays small: rows cut at the size budget, idsOnly for long lists; search_text too', async () => {
  const long = 'x'.repeat(3000);
  const seed = {
    Insectary_data: LARVAE.map(r => ({ ...r, values: { ...r.values, Notes_Insectary_data: `larva con hongos ${long}` } })),
  };
  const { store, call } = await setup(seed);
  try {
    assert.ok(FIND_BUDGET <= 40000);
    const all = await call('find_records', { module: 'Insectary_data', filters: { SPECIES: 'Mechanitis lysimnia' }, limit: 500 });
    assert.equal(all.total, 130);
    assert.ok(all.returned < 130);
    assert.equal(all.truncated, true);
    assert.match(all.next, /size limit.*offset=.*idsOnly/);
    assert.ok(JSON.stringify(all).length < 40000, String(JSON.stringify(all).length));

    const ids = await call('find_records', { module: 'Insectary_data', filters: { SPECIES: 'Mechanitis lysimnia' }, idsOnly: true });
    assert.equal(ids.returned, 130);
    assert.deepEqual(Object.keys(ids.found[0]).sort(), ['id', 'label', 'row']);
    assert.equal(ids.found[0].label, store.getRecordBySheetRow('Insectary_data', 2).label);
    assert.ok(!('formulaColumns' in ids));
    assert.equal(findRecords(store.db, { module: 'Insectary_data', field: 'Insectary_ID', values: ['L1E'], idsOnly: true }).found[0].row, 3);

    const search = await call('search_text', { query: 'hongos' });
    assert.ok(search.records.length >= 1 && search.records.length < 12);
    assert.equal(search.truncated, true);
    assert.match(search.next, /more rows not shown \(size limit\): use find_records/);
    assert.ok(JSON.stringify(search).length < 40000);
  } finally {
    store.close();
  }
});
