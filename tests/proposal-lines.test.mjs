// A notebook page's proposal saved as a notebook-reader's file (get_proposal saveLines), in the
// person's own workspace, and matched again from it (match_notebook linesFile + replaceProposalId)
// to the same proposal: how a chat gathers older chats' pages without the app's database.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { mkdirSync, mkdtempSync, readFileSync, rmSync, symlinkSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { parseDateText } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';

const d = text => parseDateText(text);
const THREAD = '11111111-2222-4333-8444-555555555555';

async function setup() {
  const root = mkdtempSync(join(tmpdir(), 'ithomiini-lines-'));
  const workspaces = join(root, 'workspaces');
  mkdirSync(join(workspaces, 'franz'), { recursive: true });
  mkdirSync(join(workspaces, 'other', 'work'), { recursive: true });
  const attachments = join(root, 't3', 'userdata', 'attachments');
  mkdirSync(attachments, { recursive: true });
  writeFileSync(join(attachments, `${THREAD}-aaaa.jpg`), 'jpeg');
  const sheets = new LocalSheets({
    Insectary_stocks: [
      { row: 2, values: { 'CLUTCH NUMBER': 838, SPECIES: 'Mechanitis messenoides intermedia', NOTES: 'old' } },
      { row: 3, values: { 'CLUTCH NUMBER': 848, Generation: 'NA', SPECIES: 'Mechanitis messenoides messenoides', 'DATE LAID': d('2025-08-08') } },
    ],
    Insectary_data: [{ row: 2, values: { Insectary_ID: '5VB', 'CLUTCH NUMBER': 838 } }],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_stocks', 'Insectary_data'] });
  const assistant = createAssistant({ store, config: { t3Workspaces: workspaces, t3: { home: join(root, 't3') } } });
  store.db
    .prepare(
      "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u-franz','franz','Franz Chandi','editor','s','h',1,'2026-01-01')",
    )
    .run();
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update('franz-token').digest('hex'), 'u-franz');
  const call = async (name, args) =>
    JSON.parse(
      (await assistant.mcp({ authorization: 'Bearer franz-token' }, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } }))
        .body.result.content[0].text,
    );
  const stored = id => store.db.prepare('SELECT * FROM ai_proposals WHERE id = ?').get(id);
  // The rows as the proposal keeps them (a new row's client id is new each time).
  const rows = id => JSON.parse(stored(id).changes_json).map(({ clientId, ...c }) => c);
  const own = join(workspaces, 'franz');
  const close = () => {
    store.close();
    rmSync(root, { recursive: true, force: true });
  };
  return { store, call, stored, rows, own, workspaces, close };
}

const STOCKS = {
  kind: 'stocks',
  year: 2025,
  title: 'posturas 838–1000',
  photo: `${THREAD}-aaaa.jpg`,
  rotate: 90,
  includeUnchanged: true,
  lines: [
    { raw: '838 interm. disec 2+1 larvas enfermas', values: { 'CLUTCH NUMBER': '838', dissections: '2+1', NOTES: 'larvas enfermas' } },
    { raw: '848 messen. 8/8', values: { 'CLUTCH NUMBER': '848', SPECIES: 'Mechanitis messenoides messenoides', 'DATE LAID': '8/8' } },
    {
      raw: '999 lys (F1) 1/9 [roto]',
      values: { 'CLUTCH NUMBER': '999', SPECIES: 'Mechanitis lysimnia (F1)', 'DATE LAID': '1/9', 'NUMBER OF EGGS': null },
      confidence: { 'DATE LAID': 0.5 },
      alternatives: { 'DATE LAID': ['7/9'], 'NUMBER OF EGGS': ['1?'] },
      reasons: { 'DATE LAID': 'smudged', 'NUMBER OF EGGS': 'torn' },
    },
    { raw: '1000 tachado', values: { 'CLUTCH NUMBER': '1000' }, crossedOut: true },
  ],
};

test("get_proposal saveLines: a page's proposal as a reader's file, matched again to the same rows", async () => {
  const { call, stored, rows, own, close } = await setup();
  try {
    const out = await call('match_notebook', STOCKS);
    assert.ok(out.proposalId, JSON.stringify(out));
    const before = rows(out.proposalId);

    const saved = await call('get_proposal', { proposalId: out.proposalId, saveLines: 'work/gathered/p1.json' });
    assert.deepEqual(saved, { proposalId: out.proposalId, linesFile: 'work/gathered/p1.json', lines: 4 });
    const file = JSON.parse(readFileSync(join(own, 'work', 'gathered', 'p1.json'), 'utf8'));
    assert.equal(file.kind, 'stocks');
    assert.equal(file.title, 'posturas 838–1000');
    assert.deepEqual([file.photo, file.rotate], [[`${THREAD}-aaaa.jpg`], [90]]);
    assert.deepEqual(file.lines[0], {
      n: 1,
      raw: '838 interm. disec 2+1 larvas enfermas',
      // The sum as written; the note without the sheet's old note and the date and initials before it.
      values: { 'CLUTCH NUMBER': '838', NOTES: 'larvas enfermas', 'NUMBER OF PUPAE/LARVAE FOR DISECTIONS': '2+1' },
    });
    assert.deepEqual(file.lines[1], { n: 2, raw: '848 messen. 8/8', values: { 'CLUTCH NUMBER': '848' } }, 'a line already in the sheet: its key');
    // Dates as a reader writes them, the doubtful and unreadable cells with their readings and reasons.
    assert.deepEqual(file.lines[2], {
      n: 3,
      raw: '999 lys (F1) 1/9 [roto]',
      values: { 'CLUTCH NUMBER': '999', SPECIES: 'Mechanitis lysimnia', 'DATE LAID': '1/9/2025', Generation: 'F1', 'NUMBER OF EGGS': null },
      confidence: { 'DATE LAID': 0.5 },
      alternatives: { 'DATE LAID': ['7/9/2025'], 'NUMBER OF EGGS': ['1?'] },
      reasons: { 'DATE LAID': 'smudged', 'NUMBER OF EGGS': 'torn' },
    });
    assert.deepEqual(file.lines[3], { n: 4, raw: '1000 tachado', values: { 'CLUTCH NUMBER': '1000' }, crossedOut: true });

    // Read again from the file in place of the proposal: the same rows, the same page and photos.
    const page = stored(out.proposalId).page_json;
    const again = await call('match_notebook', { linesFile: 'work/gathered/p1.json', replaceProposalId: out.proposalId, includeUnchanged: true });
    assert.equal(again.proposalId, out.proposalId, JSON.stringify(again));
    assert.deepEqual(rows(out.proposalId), before);
    assert.equal(stored(out.proposalId).page_json, page);
    assert.equal(stored(out.proposalId).reason, 'Cuaderno Posturas (Insectary_stocks): posturas 838–1000');
  } finally {
    close();
  }
});

test('saveLines: implied columns left out, pages kept before page_json from their rows, only in work/', async () => {
  const { store, call, rows, own, workspaces, close } = await setup();
  try {
    const out = await call('match_notebook', {
      kind: 'emergence',
      year: 2025,
      lines: [
        { raw: '5VB ♀ 4/8', values: { Insectary_ID: '5VB', Sex: 'female', Intro2Insectary_date: '4/8' } },
        { raw: '6VB ♂', values: { Insectary_ID: '6VB', Sex: 'male' } },
      ],
    });
    const before = rows(out.proposalId);
    assert.deepEqual(before[0].inferred, ['Stock_of_origin', 'LIFESTAGE']);
    await call('get_proposal', { proposalId: out.proposalId, saveLines: join(own, 'work', 'e.json') });
    const file = JSON.parse(readFileSync(join(own, 'work', 'e.json'), 'utf8'));
    assert.deepEqual(file.lines[0].values, { Insectary_ID: '5VB', Sex: 'female', Intro2Insectary_date: '4/8/2025' }, 'Stock_of_origin and LIFESTAGE are implied again');
    assert.deepEqual(file.lines[1], { n: 2, raw: '6VB ♂', values: { Insectary_ID: '6VB' } }, 'not found in the sheet: its ID');
    await call('match_notebook', { linesFile: 'work/e.json', replaceProposalId: out.proposalId });
    assert.deepEqual(rows(out.proposalId), before);

    // A proposal from before pages were kept: its rows in order, each line as its note quotes it.
    store.db.prepare('UPDATE ai_proposals SET page_json = NULL WHERE id = ?').run(out.proposalId);
    const old = await call('get_proposal', { proposalId: out.proposalId, saveLines: 'work/old.json' });
    assert.equal(old.lines, 1);
    const oldFile = JSON.parse(readFileSync(join(own, 'work', 'old.json'), 'utf8'));
    assert.equal(oldFile.kind, 'emergence', 'the kind from the reason');
    assert.deepEqual(oldFile.lines, [{ n: 1, raw: '5VB ♀ 4/8', values: { Insectary_ID: '5VB', Sex: 'female', Intro2Insectary_date: '4/8/2025' } }]);

    // Only a .json file under this person's work/ folder.
    writeFileSync(join(workspaces, 'other', 'work', 'x.json'), '{}');
    symlinkSync(join(workspaces, 'other', 'work'), join(own, 'work', 'out'));
    for (const [saveLines, why] of [
      ['page.json', /not in this workspace's work\/ folder/],
      ['work/../../other/work/p.json', /not in this workspace's work\/ folder/],
      [join(workspaces, 'other', 'work', 'p.json'), /not in this workspace's work\/ folder/],
      ['work/out/p.json', /not in this workspace's work\/ folder/],
      ['work/out/x.json', /not in this workspace's work\/ folder/],
      ['work/p.txt', /\.json/],
      ['work', /\.json/],
    ]) {
      const refused = await call('get_proposal', { proposalId: out.proposalId, saveLines });
      assert.match(refused.error ?? '', why, saveLines);
    }
    assert.equal(readFileSync(join(workspaces, 'other', 'work', 'x.json'), 'utf8'), '{}');

    // A proposal that is no notebook page.
    const plain = await call('propose_changes', {
      reason: 'Sexo',
      changes: [{ recordId: store.getRecordBySheetRow('Insectary_data', 2).id, values: { Sex: 'male' } }],
    });
    assert.match((await call('get_proposal', { proposalId: plain.proposalId, saveLines: 'work/x.json' })).error, /not a notebook page/);
  } finally {
    close();
  }
});
