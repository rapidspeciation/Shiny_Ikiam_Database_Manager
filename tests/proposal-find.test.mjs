// Finding proposals from a chat without reading the app's database: list_proposals through all
// the person's chats (and the team's, read-only) by status, sheet, row ID, photo, date and chat,
// each entry saying what get_proposal would be read for; a proposal taken out of review from
// the chat (update_proposal discard), and a clear answer when one is no longer pending;
// propose_changes with its photo.
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { signIn } from './helpers/assistant.mjs';

const THREAD = '11111111-2222-4333-8444-555555555555';
const OTHER = '99999999-2222-4333-8444-555555555555';
const PHOTO = `${THREAD}-aaaa.jpg`;

async function setup() {
  const home = mkdtempSync(join(tmpdir(), 'ithomiini-t3-'));
  mkdirSync(join(home, 'userdata', 'attachments'), { recursive: true });
  writeFileSync(join(home, 'userdata', 'attachments', PHOTO), 'not really a jpeg');
  const store = new Store(
    { localMode: true },
    {
      sheets: new LocalSheets({
        Insectary_stocks: [
          { row: 2, values: { 'CLUTCH NUMBER': 900, SPECIES: 'Mechanitis lysimnia' } },
          { row: 3, values: { 'CLUTCH NUMBER': 901, SPECIES: 'Mechanitis lysimnia' } },
          { row: 4, values: { 'CLUTCH NUMBER': 902, SPECIES: 'Mechanitis lysimnia' } },
        ],
      }),
    },
  );
  await store.sync({ sheets: ['Insectary_stocks'] });
  // T3 Code's chats as the assistant reads them: the calls come from `chat.now`.
  const chat = { now: THREAD };
  const t3Chats = {
    available: true,
    threads: ids => new Map(ids.map(id => [id, { title: 'Cuaderno' }])),
    threadOfToolUse: () => chat.now,
    onlyRunning: () => chat.now,
    open: () => null,
    chatsOf: () => [],
    findProposals: async () => new Map(),
  };
  const assistant = createAssistant({ store, config: { t3: { home }, t3Chats } });
  const franz = signIn(store, assistant, { id: 'u-franz', displayName: 'Franz Chandi' });
  const ana = signIn(store, assistant, { id: 'u-ana', username: 'ana', displayName: 'Ana' });
  const guest = signIn(store, assistant, { id: 'u-guest', username: 'guest', displayName: 'Guest', role: 'viewer' });
  const stock = row => store.getRecordBySheetRow('Insectary_stocks', row);
  const close = () => {
    store.close();
    rmSync(home, { recursive: true, force: true });
  };
  return { store, chat, franz, ana, guest, stock, close };
}

const ids = list => list.proposals.map(p => p.proposalId ?? p.tableId).sort();

test('list_proposals: any of the person\'s proposals by status, sheet, row ID, photo, date and chat, each with its state', async () => {
  const { chat, franz, stock, close } = await setup();
  const { call, http } = franz;
  try {
    // Drafted in this chat: one with the photo it was read from, one the person then edits in the table.
    const withPhoto = await call('propose_changes', {
      reason: 'Posturas 900',
      changes: [{ sheet: 'Insectary_stocks', id: '900', values: { NOTES: 'larvas sanas' } }],
      photo: PHOTO,
      rotate: 90,
    });
    assert.equal(withPhoto.photos, 1, JSON.stringify(withPhoto));
    const edited = await call('propose_changes', { reason: 'Posturas 901', changes: [{ recordId: stock(3).id, values: { NOTES: 'dos' } }] });
    const typed = await http('POST', `/api/chat/proposals/${edited.proposalId}/edit`, { body: { cells: [{ key: stock(3).id, field: 'NOTES', value: 'tres' }] } });
    assert.equal(typed.status, 200, JSON.stringify(typed.body));
    // Another chat's, applied there.
    chat.now = OTHER;
    const applied = await call('propose_changes', { reason: 'Posturas 902', changes: [{ recordId: stock(4).id, values: { NOTES: 'cuatro' } }] });
    const shown = await call('get_proposal', { proposalId: applied.proposalId });
    const done = await http('POST', `/api/chat/proposals/${applied.proposalId}/apply`, { body: { requestId: randomUUID(), revision: shown.revision } });
    assert.equal(done.body.status, 'applied', JSON.stringify(done.body));
    chat.now = THREAD;

    // This chat: its own two, the other chat's not.
    const here = await call('list_proposals', {});
    assert.equal(here.chat, THREAD);
    assert.deepEqual(ids(here), [withPhoto.proposalId, edited.proposalId].sort());
    // What a get_proposal would have been read for: who changed it last and the person's cells.
    const revised = here.proposals.find(p => p.proposalId === edited.proposalId);
    assert.equal(revised.lastChangedBy, 'person');
    assert.equal(revised.personEdits, 1);
    assert.deepEqual(revised.sheets, ['Insectary_stocks']);

    // Any status, every chat; the applied one says when and by whom, and which chat holds it.
    assert.deepEqual(ids(await call('list_proposals', { status: 'any' })), [withPhoto.proposalId, edited.proposalId, applied.proposalId].sort());
    const closed = await call('list_proposals', { status: 'applied' });
    assert.deepEqual(ids(closed), [applied.proposalId]);
    assert.equal(closed.proposals[0].chat, OTHER);
    assert.ok(closed.proposals[0].appliedAt);
    assert.equal(closed.proposals[0].appliedBy, 'Franz Chandi');
    // Open ones only, by default, once filtered.
    assert.deepEqual(ids(await call('list_proposals', { sheet: 'insectary_stocks' })), [withPhoto.proposalId, edited.proposalId].sort());
    assert.deepEqual(ids(await call('list_proposals', { sheet: 'Collection_data', status: 'any' })), []);
    // A row's ID among the rows, a photo's name, a chat by the start of its id, a date.
    assert.deepEqual(ids(await call('list_proposals', { id: '901' })), [edited.proposalId]);
    assert.deepEqual(ids(await call('list_proposals', { id: '902', status: 'any' })), [applied.proposalId]);
    assert.deepEqual(ids(await call('list_proposals', { photo: 'AAAA' })), [withPhoto.proposalId]);
    assert.deepEqual(ids(await call('list_proposals', { chat: OTHER.slice(0, 8), status: 'any' })), [applied.proposalId]);
    assert.equal((await call('list_proposals', { since: '1/1/2020', status: 'any' })).found, 3);
    assert.equal((await call('list_proposals', { since: '2999-01-01', status: 'any' })).found, 0);
    assert.match((await call('list_proposals', { since: 'ayer' })).error, /since/);
    assert.match((await call('list_proposals', { status: 'done' })).error, /Unknown status done/);
    // A long list: the newest within the limit, and how many more there are.
    const one = await call('list_proposals', { status: 'any', limit: 1 });
    assert.equal(one.proposals.length, 1);
    assert.equal(one.found, 3);
    assert.match(one.next, /^2 more/);

    // The photo goes with the proposal: get_proposal names it with its turn.
    const read = await call('get_proposal', { proposalId: withPhoto.proposalId });
    assert.deepEqual(read.photos, [{ file: PHOTO, rotate: 90 }]);
    assert.ok(read.createdAt);
    // A list given as JSON text is read as a list; a name that is no attachment is refused.
    const asText = await call('propose_changes', {
      reason: 'x',
      changes: [{ recordId: stock(2).id, values: { NOTES: 'cinco' } }],
      photo: JSON.stringify([{ name: PHOTO, note: 'página 900' }]),
    });
    assert.equal(asText.photos, 1, JSON.stringify(asText));
    const missing = await call('propose_changes', { reason: 'x', changes: [{ recordId: stock(2).id, values: { NOTES: 'seis' } }], photo: 'nope.jpg' });
    assert.equal(missing.error, 'No photo by that name');
  } finally {
    close();
  }
});

test('update_proposal discard: out of review from the chat; a proposal no longer pending says what became of it', async () => {
  const { franz, stock, close } = await setup();
  const { call, http } = franz;
  try {
    const first = await call('propose_changes', { reason: 'a', changes: [{ recordId: stock(2).id, values: { NOTES: 'uno' } }] });
    assert.match((await call('update_proposal', { proposalId: first.proposalId, discard: true, reason: 'b' })).error, /give it alone \(reason given too\)/);
    assert.deepEqual(await call('update_proposal', { proposalId: first.proposalId, discard: true }), { proposalId: first.proposalId, status: 'discarded' });
    // Revised afterwards: what became of it, when and by whom.
    const late = await call('update_proposal', { proposalId: first.proposalId, rows: [{ index: 0, values: { NOTES: 'dos' } }] });
    assert.match(late.error, /^Discarded: nothing of it was written/);
    assert.equal(late.status, 'discarded');
    assert.equal(late.closedBy, 'assistant');
    assert.ok(late.closedAt);
    assert.equal((await call('update_proposal', { proposalId: first.proposalId, discard: true })).status, 'discarded');
    assert.equal((await call('get_proposal', { proposalId: first.proposalId })).closedBy, 'assistant');
    // Discarded by the person from the table.
    const second = await call('propose_changes', { reason: 'c', changes: [{ recordId: stock(3).id, values: { NOTES: 'tres' } }] });
    assert.equal((await http('POST', `/api/chat/proposals/${second.proposalId}/discard`)).status, 200);
    const listed = await call('list_proposals', { status: 'discarded' });
    assert.deepEqual(
      listed.proposals.map(p => [p.proposalId, p.closedBy]).sort(),
      [
        [first.proposalId, 'assistant'],
        [second.proposalId, 'person'],
      ].sort(),
    );
    // A table shown with show_rows is closed the same way.
    const table = await call('show_rows', { title: 'Posturas', sheet: 'Insectary_stocks', recordIds: [stock(2).id] });
    assert.deepEqual(await call('update_proposal', { proposalId: table.tableId, discard: true }), { proposalId: table.tableId, status: 'closed' });
  } finally {
    close();
  }
});

test("a teammate's proposals: found with team, read with get_proposal, changed only by them", async () => {
  const { franz, ana, guest, stock, close } = await setup();
  try {
    const hers = await ana.call('propose_changes', { reason: 'Posturas de Ana', changes: [{ recordId: stock(2).id, values: { NOTES: 'de Ana' } }] });
    const mine = await franz.call('propose_changes', { reason: 'Mías', changes: [{ recordId: stock(3).id, values: { NOTES: 'de Franz' } }] });
    assert.deepEqual(ids(await franz.call('list_proposals', { allChats: true })), [mine.proposalId]);
    const team = await franz.call('list_proposals', { team: true });
    assert.deepEqual(ids(team), [hers.proposalId, mine.proposalId].sort());
    const listed = team.proposals.find(p => p.proposalId === hers.proposalId);
    assert.equal(listed.by, 'Ana');
    assert.equal(listed.readOnly, true);
    assert.equal(team.proposals.find(p => p.proposalId === mine.proposalId).by, undefined);
    // Read as she sees it, marked read-only.
    const read = await franz.call('get_proposal', { proposalId: hers.proposalId, full: true });
    assert.equal(read.by, 'Ana');
    assert.equal(read.readOnly, true);
    assert.match(read.rows[0].values.NOTES, /: de Ana$/);
    // Not revised, discarded or applied from someone else's chat.
    assert.match((await franz.call('update_proposal', { proposalId: hers.proposalId, rows: [{ index: 0, values: { NOTES: 'x' } }] })).error, /^Ana's proposal: read-only/);
    assert.match((await franz.call('update_proposal', { proposalId: hers.proposalId, discard: true })).error, /^Ana's proposal: read-only/);
    assert.match((await franz.call('apply_proposal', { proposalId: hers.proposalId })).error, /not found/);
    assert.equal((await ana.call('get_proposal', { proposalId: hers.proposalId })).status, 'pending');
    // Someone who cannot propose edits sees only their own.
    assert.equal((await guest.call('get_proposal', { proposalId: hers.proposalId })).error, 'Proposal not found');
    assert.deepEqual(ids(await guest.call('list_proposals', { team: true })), []);
  } finally {
    close();
  }
});
