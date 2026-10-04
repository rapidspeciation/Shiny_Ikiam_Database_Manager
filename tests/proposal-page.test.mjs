// A notebook page's proposal: every line of the page in its order (context
// and placeholder rows), what the SPECIES formula will give, the page's photos
// (only attachments of the proposal's own T3 chat) and the lean payload.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { mkdirSync, mkdtempSync, rmSync, symlinkSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { moduleMap, parseDateText } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { attachmentFile, photosOf, rightAngle } from '../server/proposal-photos.mjs';

const d = text => parseDateText(text);
const THREAD = '11111111-2222-4333-8444-555555555555';
const OTHER = '99999999-2222-4333-8444-555555555555';
// A 4×2 px photo (wider than tall).
const JPEG = Buffer.from(
  '/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDABALDA4MChAODQ4SERATGCgaGBYWGDEjJR0oOjM9PDkzODdASFxOQERXRTc4UG1RV19iZ2hnPk1xeXBkeFxlZ2P/2wBDARESEhgVGC8aGi9jQjhCY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2P/wAARCAACAAQDASIAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREAAgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwDHooorhPqD/9k=',
  'base64',
);
/** A JPEG's width and height (its SOF marker). */
function jpegSize(data) {
  for (let i = 2; i < data.length - 9; ) {
    if (data[i] !== 0xff) return null;
    const marker = data[i + 1];
    if (marker >= 0xc0 && marker <= 0xc2) return [data.readUInt16BE(i + 7), data.readUInt16BE(i + 5)];
    i += 2 + data.readUInt16BE(i + 2);
  }
  return null;
}

async function setup() {
  const home = mkdtempSync(join(tmpdir(), 'ithomiini-t3-'));
  const attachments = join(home, 'userdata', 'attachments');
  mkdirSync(attachments, { recursive: true });
  writeFileSync(join(attachments, `${THREAD}-aaaa.jpg`), JPEG);
  writeFileSync(join(attachments, `${OTHER}-bbbb.jpg`), JPEG);
  writeFileSync(join(home, 'secret.jpg'), JPEG);
  symlinkSync(join(home, 'secret.jpg'), join(attachments, `${THREAD}-link.jpg`));

  const species = moduleMap.get('Insectary_data').fields.find(f => f.key === 'SPECIES').column;
  const sheets = new LocalSheets({
    Insectary_stocks: [
      { row: 2, values: { 'CLUTCH NUMBER': 838, SPECIES: 'Mechanitis messenoides intermedia' } },
      { row: 3, values: { 'CLUTCH NUMBER': 848, SPECIES: 'Mechanitis messenoides messenoides' } },
    ],
    Insectary_data: [
      { row: 2, values: { Insectary_ID: '5VB', 'CLUTCH NUMBER': 838, Sex: 'female', Intro2Insectary_date: d('2025-08-04') } },
      { row: 3, values: { Insectary_ID: '8VD', 'CLUTCH NUMBER': 848, Wild_Reared: 'Reared' } },
      // A pre-made row: only its ID, its SPECIES formula gives nothing yet.
      { row: 4, values: { Insectary_ID: '2AB' } },
      { row: 5, values: { Insectary_ID: '3AB', 'CLUTCH NUMBER': 848 } },
    ],
  });
  for (const [row, value] of [
    [2, 'Mechanitis messenoides intermedia'],
    [3, 'Mechanitis messenoides messenoides'],
    [4, null],
    [5, 'Mechanitis messenoides messenoides'],
  ])
    sheets.rows.get('Insectary_data').find(r => r.row === row).cells[species] = {
      userEnteredValue: { formulaValue: `=XLOOKUP(C${row},Insectary_stocks!A:A,Insectary_stocks!C:C,"")` },
      ...(value ? { effectiveValue: { stringValue: value } } : {}),
    };
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_stocks', 'Insectary_data'] });
  // T3 Code's chats as the assistant reads them: every call of this test comes from THREAD.
  const t3Chats = {
    available: true,
    threads: ids => new Map(ids.map(id => [id, { title: 'Cuaderno' }])),
    threadOfToolUse: () => THREAD,
    onlyRunning: () => THREAD,
    open: () => null,
    chatsOf: () => [],
    findProposals: async () => new Map(),
  };
  const assistant = createAssistant({ store, config: { t3: { home }, t3Chats } });
  for (const [id, name] of [
    ['u-franz', 'franz'],
    ['u-ana', 'ana'],
  ]) {
    store.db
      .prepare(
        "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES(?,?,?,'editor','s','h',1,'2026-01-01')",
      )
      .run(id, name, name);
    store.db
      .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
      .run(createHash('sha256').update(`token-${name}`).digest('hex'), id);
  }
  const call = async (name, args) =>
    JSON.parse(
      (
        await assistant.mcp(
          { authorization: 'Bearer token-franz' },
          { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args, _meta: { 'claudecode/toolUseId': 'toolu_1' } } },
        )
      ).body.result.content[0].text,
    );
  const franz = { id: 'u-franz', username: 'franz', displayName: 'Franz', role: 'editor' };
  const ana = { id: 'u-ana', username: 'ana', displayName: 'Ana', role: 'editor' };
  const http = (method, path, body = {}, user = franz, headers = {}) => assistant.handle({ method, path, body, user, query: {}, headers });
  const get = (path, user = franz, query = {}, headers = {}) => assistant.handle({ method: 'GET', path, body: {}, user, query, headers });
  const proposals = async () => (await http('GET', '/api/chat/proposals')).body.proposals;
  const close = () => {
    store.close();
    rmSync(home, { recursive: true, force: true });
  };
  return { store, home, attachments, call, http, get, proposals, close, ana };
}

const PAGE = {
  kind: 'emergence',
  year: 2025,
  title: 'emergidos 5VB–3AB',
  lines: [
    { raw: '5VB interm ♀ 838 4/8 dead 9/8', values: { Insectary_ID: '5VB', Sex: 'female', 'CLUTCH NUMBER': '838', Intro2Insectary_date: '4/8', Death_date: '9/8' } },
    // On the second photo (its order goes after the first photo's lines).
    { raw: '8VD 848', values: { Insectary_ID: '8VD', 'CLUTCH NUMBER': '848' }, photo: 1 },
    // A pre-made row: the clutch goes in, the species is the clutch's (the formula gives it).
    { raw: '2AB interm ♀ 838', values: { Insectary_ID: '2AB', SPECIES: 'Mechanitis messenoides intermedia', Sex: 'female', 'CLUTCH NUMBER': '838' } },
    { raw: '7ZZ ♀', values: { Insectary_ID: '7ZZ', Sex: 'female' } },
    { raw: '4AB tachado', values: { Insectary_ID: '4AB' }, crossedOut: true },
  ],
};

test('a photo is an attachment of the proposal chat, directly in the T3 attachments folder', async () => {
  const { home, close } = await setup();
  try {
    assert.equal(attachmentFile(home, `${THREAD}-aaaa.jpg`, THREAD)?.name, `${THREAD}-aaaa.jpg`);
    // The path the chat gives: only its name counts.
    assert.equal(attachmentFile(home, `/somewhere/else/${THREAD}-aaaa.jpg`, THREAD)?.name, `${THREAD}-aaaa.jpg`);
    assert.equal(attachmentFile(home, `${OTHER}-bbbb.jpg`, THREAD), null, 'another chat');
    assert.equal(attachmentFile(home, `${OTHER}-bbbb.jpg`)?.name, `${OTHER}-bbbb.jpg`, 'the chat not known yet: only the folder counts');
    assert.equal(attachmentFile(home, `${THREAD}-link.jpg`, THREAD), null, 'a link out of the folder');
    assert.equal(attachmentFile(home, '../secret.jpg'), null);
    assert.equal(attachmentFile(home, '..'), null);
    assert.equal(attachmentFile(home, `${THREAD}-none.jpg`, THREAD), null, 'no such file');
    assert.equal(attachmentFile(null, `${THREAD}-aaaa.jpg`), null, 'T3 not configured');
    assert.deepEqual([0, 90, 180, 270, 360, -90, 'x'].map(rightAngle), [0, 90, 180, 270, 0, 270, 0]);
    assert.deepEqual(photosOf(home, { photo: [`${THREAD}-aaaa.jpg`, `${OTHER}-bbbb.jpg`], rotate: 90 }, THREAD), {
      photos: [{ file: `${THREAD}-aaaa.jpg`, rotate: 90 }],
      refused: [`${OTHER}-bbbb.jpg`],
    });
  } finally {
    close();
  }
});

test("a page's proposal shows every line in the notebook's order, with its photo, and what the SPECIES formula will give", async () => {
  const { store, call, http, get, proposals, close, ana } = await setup();
  try {
    const out = await call('match_notebook', {
      ...PAGE,
      photo: [`/home/ubuntu/.t3/userdata/attachments/${THREAD}-aaaa.jpg`, `${THREAD}-aaaa.jpg`, `${OTHER}-bbbb.jpg`, '../../etc/passwd'],
      rotate: [90, 0, 0, 0],
    });
    assert.ok(out.proposalId, JSON.stringify(out));
    assert.deepEqual(out.photoNotShown, [`${OTHER}-bbbb.jpg`, '../../etc/passwd']);
    let [p] = await proposals();
    assert.deepEqual(p.page, { kind: 'emergence', sheet: 'Insectary_data', columns: p.page.columns, keys: ['Insectary_ID'], photos: 2 });
    assert.deepEqual(p.page.columns.slice(0, 4), ['Insectary_ID', 'SPECIES', 'Sex', 'CLUTCH NUMBER']);
    // Photo 0's lines (1, 3, 4, 5), then photo 1's (2).
    assert.deepEqual(
      p.changes.map(c => [c.page.photo, c.page.line, c.label, c.index >= 0 ? 'row' : c.placeholder ? 'as written' : 'sheet', c.page.status ?? '']),
      [
        [0, 1, '5VB', 'row', ''],
        [0, 3, '2AB', 'row', ''],
        [0, 4, '7ZZ', 'as written', 'missing'],
        [0, 5, '4AB', 'as written', 'crossed'],
        [1, 2, '8VD', 'sheet', 'match'],
      ],
    );
    const [, premade, missing, crossed, same] = p.changes;
    assert.deepEqual([missing.page.raw, crossed.page.raw], ['7ZZ ♀', '4AB tachado']);
    assert.ok(missing.context && same.context && !same.placeholder, 'never written');
    assert.equal(same.rowValues['CLUTCH NUMBER'], 848, 'the sheet row as it is');
    // The species is the clutch's: not written (the formula gives it once the clutch is in), but shown.
    assert.ok(!('SPECIES' in premade.values));
    assert.equal(premade.values['CLUTCH NUMBER'], 838);
    assert.deepEqual(premade.formulaGives, { SPECIES: 'Mechanitis messenoides intermedia' });
    const stored = JSON.parse(store.db.prepare('SELECT changes_json FROM ai_proposals WHERE id = ?').get(out.proposalId).changes_json);
    assert.deepEqual(stored.find(c => c.label === '2AB').formulaGives, { SPECIES: 'Mechanitis messenoides intermedia' });

    // The photos: upright copies of this chat's attachment, for its owner only.
    const thumb = await get(`/api/proposals/${out.proposalId}/photos/0`);
    assert.equal(thumb.status, 200);
    assert.equal(thumb.headers['content-type'], 'image/jpeg');
    assert.match(thumb.headers['cache-control'], /^private/);
    if (!thumb.raw.equals(JPEG)) assert.deepEqual(jpegSize(thumb.raw), [2, 4], 'turned 90° clockwise');
    assert.equal((await get(`/api/proposals/${out.proposalId}/photos/0`, undefined, {}, { 'if-none-match': thumb.headers.etag })).status, 304);
    assert.equal((await get(`/api/proposals/${out.proposalId}/photos/1`, undefined, { size: 'view' })).status, 200);
    assert.equal((await get(`/api/proposals/${out.proposalId}/photos/2`)).status, 404);
    // Someone else on the team who finishes the chat sees them too; someone who only looks does not.
    assert.equal((await get(`/api/proposals/${out.proposalId}/photos/0`, ana)).status, 200, "another editor, the chat handed over");
    assert.equal((await get(`/api/proposals/${out.proposalId}/photos/0`, { ...ana, role: 'observer' })).status, 404, 'an observer');
    // The proposal said to come from another chat: its photos are not that chat's.
    store.db.prepare('UPDATE ai_proposals SET t3_thread = ? WHERE id = ?').run(OTHER, out.proposalId);
    assert.equal((await get(`/api/proposals/${out.proposalId}/photos/0`)).status, 404);
    store.db.prepare('UPDATE ai_proposals SET t3_thread = ? WHERE id = ?').run(THREAD, out.proposalId);

    // Franz's chat handed to Ana (open in her T3 frame, or asked for): its proposals are in her table, live,
    // and she can edit them; her own list (no chat) does not take them in.
    const listed = async query => (await get('/api/chat/proposals', ana, query)).body;
    const onScreen = await listed({ chat: 'auto', seen: THREAD });
    assert.deepEqual(onScreen.proposals.map(x => x.id), [out.proposalId]);
    assert.deepEqual((await listed({ chat: THREAD })).proposals.map(x => x.id), [out.proposalId]);
    assert.deepEqual((await listed({ only: out.proposalId })).proposals.map(x => x.id), [out.proposalId]);
    assert.deepEqual((await listed({})).proposals, []);
    // Her page follows Franz's revision: a change by the assistant there wakes it.
    assert.equal(onScreen.revision, (await get('/api/chat/proposals', undefined, { chat: THREAD })).body.revision);
    const edited = await http('POST', `/api/chat/proposals/${out.proposalId}/edit`, { cells: [] }, ana);
    assert.equal(edited.status, 200, JSON.stringify(edited.body));
    assert.equal((await http('POST', `/api/chat/proposals/${out.proposalId}/edit`, { cells: [] }, { ...ana, role: 'observer' })).status, 403);

    // A clutch changed later (update_proposal): the formula's species follows it.
    const index = p.changes.find(c => c.label === '2AB').index;
    await call('update_proposal', { proposalId: out.proposalId, rows: [{ index, values: { 'CLUTCH NUMBER': '848' } }] });
    [p] = await proposals();
    assert.deepEqual(p.changes.find(c => c.label === '2AB').formulaGives, { SPECIES: 'Mechanitis messenoides messenoides' });

    // A row added later takes the line of its ID; one not on the page goes after it.
    const rowOf = id => store.getRecordBySheetRow('Insectary_data', { '8VD': 3, '3AB': 5 }[id]);
    await call('update_proposal', {
      proposalId: out.proposalId,
      changes: [
        { recordId: rowOf('3AB').id, values: { Sex: 'male' } },
        { recordId: rowOf('8VD').id, values: { Sex: 'male' } },
      ],
    });
    [p] = await proposals();
    assert.deepEqual(
      p.changes.map(c => [c.label, c.page?.line ?? null, c.index >= 0]),
      [
        ['5VB', 1, true],
        ['2AB', 3, true],
        ['7ZZ', 4, false],
        ['4AB', 5, false],
        ['8VD', 2, true],
        ['3AB', null, true],
      ],
    );

    // A row the person removes stays on the page, grey (as the sheet has it).
    const first = p.changes[0];
    await http('POST', `/api/chat/proposals/${out.proposalId}/edit`, { cells: [], remove: [first.key] });
    [p] = await proposals();
    assert.deepEqual([p.changes[0].label, p.changes[0].index < 0, p.changes[0].context, p.changes[0].page.status], ['5VB', true, true, 'match']);
    // Typed into that grey row: it becomes a row of the proposal again.
    const typed = await http('POST', `/api/chat/proposals/${out.proposalId}/edit`, {
      cells: [{ key: p.changes[0].key, field: 'Death_cause', value: 'Spider' }],
    });
    assert.equal(typed.status, 200, JSON.stringify(typed.body));
    const row = typed.body.proposal.changes[0];
    assert.deepEqual([row.label, row.index >= 0, !!row.context, row.values.Death_cause, row.page.line], ['5VB', true, false, 'Spider', 1]);

    // A line the save refused shows with why.
    const page = JSON.parse(store.db.prepare('SELECT page_json FROM ai_proposals WHERE id = ?').get(out.proposalId).page_json);
    page.lines[3].error = 'Tube_1_id FD1 ya está en Insectary_data fila 9';
    store.db.prepare('UPDATE ai_proposals SET page_json = ? WHERE id = ?').run(JSON.stringify(page), out.proposalId);
    [p] = await proposals();
    assert.equal(p.changes.find(c => c.label === '7ZZ').page.error, 'Tube_1_id FD1 ya está en Insectary_data fila 9');
  } finally {
    close();
  }
});

test('the proposal goes lean: hints once, formula columns once per sheet, no drafting leftovers', async () => {
  const { call, proposals, close } = await setup();
  try {
    await call('match_notebook', {
      kind: 'deaths',
      year: 2025,
      lines: [
        { raw: '9/8 5VB unk', values: { Insectary_ID: '5VB', Death_date: '9/8', Death_cause: 'Unknown' } },
        { raw: '9/8 3AB unk', values: { Insectary_ID: '3AB', Death_date: '9/8', Death_cause: 'Unknown' } },
      ],
    });
    const [p] = await proposals();
    const rows = p.changes.filter(c => c.index >= 0);
    assert.equal(rows.length, 2);
    for (const c of rows) {
      assert.ok(!('before' in c) && !('expectedVersion' in c) && !('current' in c), Object.keys(c).join());
      assert.ok(c.rowValues);
      // The template's hints: an index into the proposal's table, the same for both rows.
      for (const i of Object.values(c.hints ?? {})) assert.ok(p.hintTable[i]);
    }
    assert.deepEqual(rows[0].hints, rows[1].hints);
    assert.ok(Object.keys(rows[0].hints).length > 3, 'a death fills its template');
    assert.equal(new Set(p.hintTable.map(h => JSON.stringify(h))).size, p.hintTable.length, 'each hint once');
    assert.ok(p.hintTable.every(h => h.msg && !h.text), 'the descriptor only (the interface words it)');
    assert.ok(p.sheetFormulas.Insectary_data.length >= 0 && rows.every(c => !c.formulas));
  } finally {
    close();
  }
});

test('a proposal made before pages were kept gets its photo later with update_proposal', async () => {
  const { store, call, get, proposals, close } = await setup();
  try {
    const row = store.getRecordBySheetRow('Insectary_data', 3);
    const out = await call('propose_changes', {
      reason: 'Cuaderno Emergidos (Insectary_data): emergidos 8VD',
      changes: [{ recordId: row.id, values: { Sex: 'male' } }],
    });
    let [p] = await proposals();
    assert.equal(p.page?.photos ?? 0, 0);

    // Another chat's photo, or none by that name: refused, nothing changes.
    const refused = await call('update_proposal', { proposalId: out.proposalId, photo: `${OTHER}-bbbb.jpg` });
    assert.match(refused.error, /No photo of this chat/);

    const done = await call('update_proposal', { proposalId: out.proposalId, photo: `${THREAD}-aaaa.jpg`, rotate: 90 });
    assert.equal(done.photos, 1);
    assert.equal(done.revision, 2, 'the table refreshes');
    [p] = await proposals();
    assert.equal(p.page.photos, 1);
    assert.equal(p.page.sheet, 'Insectary_data');
    assert.equal(p.changes[0].values.Sex, 'male', 'its rows as they were');
    const thumb = await get(`/api/proposals/${out.proposalId}/photos/0`);
    assert.equal(thumb.status, 200);
    if (!thumb.raw.equals(JPEG)) assert.deepEqual(jpegSize(thumb.raw), [2, 4], 'turned 90° clockwise');
  } finally {
    close();
  }
});
