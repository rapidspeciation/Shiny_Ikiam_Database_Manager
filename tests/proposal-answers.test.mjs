// What the assistant's tools answer about proposals, kept short: a proposal the sheet caught up
// with said in one line, rows given the same said once, a Photo_links reading's examples only on
// request, what a discard of a partly applied proposal kept; and get_record, get_walk.
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { claimJob, finishJob, saveWalk } from '../server/monitoring.mjs';
import { signIn } from './helpers/assistant.mjs';

const SENT = 'Collected_Sent2Insectary';

async function setup(seed, config = {}) {
  const sheets = new LocalSheets(seed);
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: Object.keys(seed) });
  const assistant = createAssistant({ store, config });
  const { call, http, user } = signIn(store, assistant);
  /** Someone types in Google Sheets; the sheet hook reports the row. */
  const typeInSheet = async (sheet, row, changes) => {
    await sheets.externalEdit(sheet, row, changes);
    await store.refreshRows(sheet, [row]);
  };
  const record = (sheet, row) => store.getRecordBySheetRow(sheet, row);
  return { sheets, store, assistant, call, http, user, typeInSheet, record };
}

const insectary = n => ({
  Insectary_data: Array.from({ length: n }, (_, i) => ({
    row: 2 + i,
    values: { Insectary_ID: `A${i}A`, Wild_Reared: 'Reared', SPECIES: 'Oleria onega', Sex: 'male' },
  })),
});

test('a proposal the sheet caught up with: the cells as proposed counted, one line for the whole of it, and so in overlaps', async () => {
  const { store, call, record, typeInSheet } = await setup(insectary(3));
  try {
    const { proposalId } = await call('propose_changes', {
      reason: 'Página 12',
      changes: [2, 3].map(row => ({ recordId: record('Insectary_data', row).id, values: { Sex: 'female', Notes_Insectary_data: 'ala rota' } })),
    });
    // The team typed the page by hand meanwhile, one note in other words.
    const note = (await call('get_proposal', { proposalId, full: true })).rows[0].values.Notes_Insectary_data;
    assert.match(note, /ala rota$/);
    await typeInSheet('Insectary_data', 2, { Sex: 'female', Notes_Insectary_data: note });
    await typeInSheet('Insectary_data', 3, { Sex: 'female', Notes_Insectary_data: 'wing broken' });
    const got = await call('get_proposal', { proposalId });
    assert.deepEqual(
      got.attention.map(a => [a.label, a.sheetChanged.asProposed, Object.keys(a.sheetChanged.cells ?? {})]),
      [
        ['A0A', 2, []],
        ['A1A', 1, ['Notes_Insectary_data']],
      ],
    );
    assert.deepEqual(got.attention[1].sheetChanged.cells.Notes_Insectary_data, { read: null, now: 'wing broken', proposed: note });
    assert.match(got.inSheet, /^2 of its 2 rows edited in the sheet since it was drafted \(by Google Sheets\): 3 of its 4 cells hold what it proposes, 1 another value\.$/);

    // Another proposal on those rows: how much of the first the sheet holds.
    const other = await call('propose_changes', { changes: [{ recordId: record('Insectary_data', 2).id, values: { Sex: 'NA' } }] });
    assert.deepEqual(
      other.overlaps.map(o => [o.proposalId, o.cells, o.alreadyInSheet]),
      [[proposalId, 4, 3]],
    );

    // All of it there: said so.
    await typeInSheet('Insectary_data', 3, { Notes_Insectary_data: note });
    assert.match((await call('get_proposal', { proposalId })).inSheet, /^All 4 cells it writes are in the sheet already \(by Google Sheets\): applying it changes nothing\.$/);
  } finally {
    store.close();
  }
});

test('get_proposal full: rows given the same cells as one sameChange entry, the others listed', async () => {
  const { store, call, record } = await setup(insectary(5));
  try {
    const { proposalId } = await call('propose_changes', {
      changes: [
        ...[2, 3, 4, 5].map(row => ({ recordId: record('Insectary_data', row).id, values: { Sex: 'female' }, note: 'página 3' })),
        { recordId: record('Insectary_data', 6).id, values: { Sex: 'NA' }, note: 'página 3' },
      ],
    });
    const full = await call('get_proposal', { proposalId, full: true });
    assert.equal(full.total, 5);
    assert.equal(full.sheet, 'Insectary_data');
    assert.deepEqual(full.sameChange, [
      { rows: 4, values: { Sex: 'female' }, note: 'página 3', indexes: [0, 1, 2, 3], labels: ['A0A', 'A1A', 'A2A', 'A3A'] },
    ]);
    assert.deepEqual(
      full.rows.map(r => [r.index, r.label, r.values]),
      [[4, 'A4A', { Sex: 'NA' }]],
    );
  } finally {
    store.close();
  }
});

test('get_proposal of a Photo_links reading: counts and a few examples, every example with full', async () => {
  const { store, call, user } = await setup(insectary(1));
  try {
    const different = Array.from({ length: 20 }, (_, i) => ({ name: `CAM0000${10 + i}d.JPG`, rows: [{ row: 100 + i, path: `A/CAM0000${10 + i}d.JPG` }] }));
    const card = {
      read: { at: '2026-10-15T19:48:32.494Z', folders: 3, files: 40, seconds: 2, rows: 60 },
      total: 2,
      newFiles: 2,
      newFolders: [],
      byFolder: [{ folder: 'A', new: 2, files: 40, changed: 0, kinds: {} }],
      changes: { renamed: 0, relinked: 0, removed: 0, moved: 0, details: 0, statusRows: 0 },
      examples: { renamed: [], relinked: [], removed: [] },
      checks: null,
      sameName: { copies: 0, different: 0, examples: [] },
      listedTwice: { sameFile: 0, copies: 0, copyExamples: [], different: 25, differentExamples: different },
      notListed: [],
      leftOut: { toDelete: 0, temp: [] },
      turned: [],
      wings: null,
      untouched: { outside: 0, unchecked: 0, repeated: 0, unread: 0 },
      notFound: [],
    };
    const id = randomUUID();
    store.db.prepare("INSERT INTO ai_threads (id,owner_id,title,created_at,updated_at) VALUES ('t1',?,'Photo_links','2026-10-15','2026-10-15')").run(user.id);
    store.db
      .prepare(
        "INSERT INTO ai_proposals (id,thread_id,owner_id,changes_json,reason,status,created_at,updated_at,last_by,sync_json) VALUES (?,?,?,'[]','Photo_links','pending',?,?,'ai',?)",
      )
      .run(id, 't1', user.id, '2026-10-15T19:48:32.494Z', '2026-10-15T19:48:32.494Z', JSON.stringify(card));
    const plain = await call('get_proposal', { proposalId: id });
    assert.equal(plain.listedTwice.differentPhotosOfOneName.count, 25);
    assert.equal(plain.listedTwice.differentPhotosOfOneName.examples.length, 6);
    assert.match(plain.next, /sync_photo_links with scan/);
    assert.match(plain.more, /full: true/);
    const full = await call('get_proposal', { proposalId: id, full: true });
    assert.equal(full.listedTwice.differentExamples.length, 20);
    assert.ok(JSON.stringify(full).length > JSON.stringify(plain).length);
  } finally {
    store.close();
  }
});

test('discarding a proposal applied one sheet at a time says what was written and what was left out', async () => {
  const { store, call, http, record } = await setup({
    Collection_data: [{ row: 2, values: { Release_Collect: SENT, Insectary_ID: 'A0T', SPECIES: 'Oleria onega' } }, { row: 3, values: {} }, { row: 4, values: {} }],
    Insectary_data: [
      { row: 2, values: { Insectary_ID: 'A0T', Wild_Reared: 'Wild-caught', SPECIES: 'Oleria onega' } },
      { row: 3, values: { Insectary_ID: 'A1T', Wild_Reared: 'Wild-caught', SPECIES: 'Oleria onega' } },
    ],
  });
  try {
    const { proposalId } = await call('propose_changes', {
      reason: 'Página con silvestres',
      changes: [2, 3].map(row => ({ recordId: record('Insectary_data', row).id, values: { Sex: 'female' } })),
      newRows: [{ sheet: 'Collection_data', values: { Release_Collect: SENT, Insectary_ID: 'A1T', SPECIES: 'Oleria onega', Sex: 'female' } }],
    });
    const applied = await http('POST', `/api/chat/proposals/${proposalId}/apply`, { body: { requestId: randomUUID(), sheets: ['Insectary_data'] } });
    assert.equal(applied.status, 200, JSON.stringify(applied.body));
    const out = await call('update_proposal', { proposalId, discard: true });
    assert.equal(out.status, 'applied');
    assert.deepEqual(out.written, { rows: 2, sheets: ['Insectary_data'], labels: ['A0T', 'A1T'] });
    assert.deepEqual(out.notWritten, { rows: 1, sheets: ['Collection_data'], labels: ['A1T'] });
    // Asked again, the same.
    const again = await call('update_proposal', { proposalId, rows: [{ index: 0, values: { Sex: 'male' } }] });
    assert.match(again.error, /^Applied in part: its Insectary_data rows are in the sheet, its Collection_data rows were left out/);
    assert.deepEqual(again.notWritten.sheets, ['Collection_data']);
    assert.deepEqual((await call('get_proposal', { proposalId })).written.rows, 2);
  } finally {
    store.close();
  }
});

test('get_record gives formula text only when asked', async () => {
  const { store, call, record } = await setup({
    Insectary_stocks: [{ row: 2, values: { 'CLUTCH NUMBER': 838, SPECIES: 'Oleria onega' } }],
    Insectary_data: [{ row: 2, values: { Insectary_ID: '5VB', 'CLUTCH NUMBER': 838 } }],
  });
  try {
    const id = record('Insectary_data', 2).id;
    store.db.prepare('UPDATE records SET formulas_json = ? WHERE id = ?').run(JSON.stringify({ SPECIES: '=XLOOKUP(C2,Insectary_stocks!A:A,Insectary_stocks!C:C,"")' }), id);
    const plain = await call('get_record', { id });
    assert.equal(plain.formulas, undefined);
    assert.deepEqual(plain.formulaColumns, ['SPECIES']);
    const asked = await call('get_record', { id, formulas: true });
    assert.match(asked.formulas.SPECIES, /^=XLOOKUP/);
  } finally {
    store.close();
  }
});

const URL_ = 'https://es.wikiloc.com/rutas-senderismo/monitoreo-ithomidos-26-sep-2026-288058472';

test('get_walk waits while the home computer reads the walk; a Cloudflare check points to the GPX import', async () => {
  const { store, call, user } = await setup(
    { Collection_data: [{ row: 2, values: {} }], SamplingDay_data: [] },
    { walkWaitMs: 3000, walkPollMs: 10 },
  );
  try {
    store.setSetting('wikilocWorkerSeen', new Date().toISOString());
    assert.equal((await call('queue_walk', { url: URL_ })).status, 'queued');
    const asked = call('get_walk', { url: URL_ });
    // The home computer reads it a moment later: the answer is the walk, not its queue status.
    setTimeout(async () => {
      const { job } = claimJob(store);
      await saveWalk(store, { url: job.target, jobId: job.id, date: '2026-09-26', name: 'Monitoreo 26 sep 2026', author: '1', track: [], waypoints: [] }, user);
      finishJob(store, job.id, { status: 'done' });
    }, 50);
    const draft = await asked;
    assert.equal(draft.walk?.date, '2026-09-26', JSON.stringify(draft));

    const other = 'https://es.wikiloc.com/rutas-senderismo/monitoreo-ithomidos-27-sep-2026-288058473';
    await call('queue_walk', { url: other });
    const { job } = claimJob(store);
    finishJob(store, job.id, { status: 'failed', message: 'TimeoutError: Cloudflare challenge did not clear' });
    const failed = await call('get_walk', { url: other });
    assert.equal(failed.status, 'failed');
    assert.match(failed.next, /GPX .*Monitoreo → Importar/);
  } finally {
    store.close();
  }
});
