import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { FEW_BETWEEN, PEEK_ROWS, showBetween } from '../server/proposal-view.mjs';

// The review table reads like the sheet: rows by row number, and the sheet's rows between them that the
// proposal leaves alone shown greyed for context (never written), so nothing is hidden in between. A
// notebook page's lines too, whatever order its photos came in; lines the sheet has the other way round
// within a photo are told.

async function setup(rows) {
  const sheets = new LocalSheets({ Insectary_data: rows });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data'] });
  const assistant = createAssistant({ store, config: {} });
  store.db
    .prepare(
      "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','franz','Franz','editor','s','h',1,'2026-01-01')",
    )
    .run();
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update('franz-token').digest('hex'), 'u1');
  const call = async (name, args) => {
    const out = await assistant.mcp(
      { authorization: 'Bearer franz-token' },
      { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } },
    );
    return JSON.parse(out.body.result.content[0].text);
  };
  const user = { id: 'u1', username: 'franz', displayName: 'Franz', role: 'editor' };
  const list = async (query = {}) =>
    (
      await assistant.handle({
        method: 'GET',
        path: '/api/chat/proposals',
        body: {},
        user,
        query: { all: '1', ...query },
      })
    ).body;
  const at = row => store.getRecordBySheetRow('Insectary_data', row).id;
  return { store, assistant, call, list, at };
}

test('a pending proposal shows its rows in sheet order, with the rows in between for context', async () => {
  const { store, call, list, at } = await setup(
    [2, 3, 4, 5, 6].map(row => ({ row, values: { Insectary_ID: `K${row}B`, Sex: 'NA' } })),
  );
  try {
    const out = await call('propose_changes', {
      reason: 'Emergidos',
      changes: [
        { recordId: at(6), values: { Sex: 'female' } },
        { recordId: at(3), values: { Sex: 'male' } },
      ],
    });
    assert.ok(!out.error, out.error);
    const shown = (await list()).proposals[0].changes;
    assert.deepEqual(
      shown.map(c => [c.row, !!c.gap]),
      [
        [3, false],
        [4, true],
        [5, true],
        [6, false],
      ],
    );
    const gap = shown[1];
    assert.equal(gap.context, true);
    assert.deepEqual(gap.values, {});
    // A pending proposal sends a row's sheet values once, as rowValues (as for its own rows).
    assert.equal(gap.rowValues.Sex, 'NA');
    // The proposal's own rows keep their index (what applying and editing refer to).
    assert.deepEqual(
      shown.filter(c => !c.gap).map(c => c.index),
      [1, 0],
    );
  } finally {
    store.close?.();
  }
});

test('the rows in between: shown when few by default, as the view says otherwise', async () => {
  const { store, call, list, at } = await setup(
    Array.from({ length: 80 }, (_, i) => ({
      row: i + 2,
      values: { Insectary_ID: `${String.fromCharCode(65 + Math.floor(i / 10))}${i % 10}B`, Sex: 'NA' },
    })),
  );
  try {
    assert.equal(showBetween(null, { paged: false, between: FEW_BETWEEN, rows: 2 }), true);
    assert.equal(showBetween(null, { paged: false, between: FEW_BETWEEN + 1, rows: 2 }), false);
    assert.equal(showBetween(null, { paged: false, between: 60, rows: 60 }), true, 'no more than the rows changed');
    assert.equal(showBetween(null, { paged: true, between: 1, rows: 2 }), false, 'not on a notebook page');
    assert.equal(showBetween({ between: true }, { paged: true, between: 400, rows: 2 }), true);

    // Rows 2 and 70: 67 rows between two changed rows, too many by default.
    const far = await call('propose_changes', {
      reason: 'Lejos',
      changes: [2, 70].map(row => ({ recordId: at(row), values: { Sex: 'male' } })),
    });
    const rowsOf = async id => (await list()).proposals.find(p => p.id === id).changes;
    assert.deepEqual(
      (await rowsOf(far.proposalId)).map(c => c.row),
      [2, 70],
    );
    // Asked for: they come in.
    let revised = await call('update_proposal', { proposalId: far.proposalId, view: { between: true } });
    assert.ok(!revised.error, revised.error);
    assert.equal(revised.unchanged, undefined, 'a new view is a new revision');
    assert.equal((await rowsOf(far.proposalId)).length, 69);
    // null drops it: the default again.
    revised = await call('update_proposal', { proposalId: far.proposalId, view: { between: null } });
    assert.equal((await rowsOf(far.proposalId)).length, 2);

    // Few rows between: shown by default, left out when the view says so.
    const near = await call('propose_changes', {
      reason: 'Cerca',
      changes: [10, 14].map(row => ({ recordId: at(row), values: { Sex: 'female' } })),
      view: { between: false },
    });
    assert.deepEqual(
      (await rowsOf(near.proposalId)).map(c => c.row),
      [10, 14],
    );
    await call('update_proposal', { proposalId: near.proposalId, view: { between: null } });
    assert.deepEqual(
      (await rowsOf(near.proposalId)).map(c => c.row),
      [10, 11, 12, 13, 14],
    );
  } finally {
    store.close?.();
  }
});

test('a new Insectary_data row takes the place of the row it will be written to', async () => {
  // A1E–A5E in rows 2–6; A6E is the next ID of the series: row 7, past the last row.
  const { store, call, list, at } = await setup(
    [1, 2, 3, 4, 5].map(n => ({ row: n + 1, values: { Insectary_ID: `A${n}E`, SPECIES: 'Oleria onega', Sex: 'NA' } })),
  );
  try {
    const out = await call('propose_changes', {
      reason: 'Emergidos',
      newRows: [{ sheet: 'Insectary_data', values: { Insectary_ID: 'A6E', Sex: 'female' } }],
      changes: [
        { recordId: at(5), values: { Sex: 'male' } },
        { recordId: at(2), values: { Sex: 'male' } },
      ],
      view: { between: false },
    });
    assert.ok(!out.error, out.error);
    const shown = (await list()).proposals[0].changes;
    assert.deepEqual(
      shown.map(c => [c.label, c.index]),
      [
        ['A1E', 2],
        ['A4E', 1],
        ['A6E', 0],
      ],
    );
  } finally {
    store.close?.();
  }
});

test("a notebook page in the sheet's order, its photos in any order; lines a photo has the other way round are told", async () => {
  const { store, call, list } = await setup(
    [1, 2, 3, 4, 5, 6, 7].map(n => ({ row: n + 1, values: { Insectary_ID: `A${n}E`, SPECIES: 'Oleria onega' } })),
  );
  try {
    const out = await call('match_notebook', {
      kind: 'emergence',
      year: 2026,
      lines: [
        // Photo 0 (sent first): A3E, A5E, then A4E, which the sheet has before A5E.
        { raw: 'A3E ♀', values: { Insectary_ID: 'A3E', Sex: 'female' } },
        { raw: 'A5E ♂', values: { Insectary_ID: 'A5E', Sex: 'male' } },
        { raw: 'A4E ♀', values: { Insectary_ID: 'A4E', Sex: 'female' } },
        // Photo 1: A6E, then A1E (back in the sheet); between the photos the order does not count.
        { raw: 'A6E ♂', values: { Insectary_ID: 'A6E', Sex: 'male' }, photo: 1 },
        { raw: 'A1E ♂', values: { Insectary_ID: 'A1E', Sex: 'male' }, photo: 1 },
      ],
    });
    assert.ok(out.proposalId, JSON.stringify(out));
    const told = [
      { photo: 0, line: 3, id: 'A4E', after: { line: 2, id: 'A5E' } },
      { photo: 1, line: 5, id: 'A1E', after: { line: 4, id: 'A6E' } },
    ];
    // The assistant is told (match_notebook, get_proposal), to check the IDs or tell the person.
    assert.deepEqual(out.orderDiffers, told);
    assert.match(out.orderNote, /comes after `after` on the page but before it in the sheet/);
    assert.deepEqual((await call('get_proposal', { proposalId: out.proposalId })).orderDiffers, told);

    const [p] = (await list()).proposals;
    assert.deepEqual(
      p.changes.map(c => [c.row, c.label, c.page.photo, c.page.line]),
      [
        [2, 'A1E', 1, 5],
        [4, 'A3E', 0, 1],
        [5, 'A4E', 0, 3],
        [6, 'A5E', 0, 2],
        [7, 'A6E', 1, 4],
      ],
      'by row, each with its photo and line (no rows in between on a page)',
    );
    assert.deepEqual(p.outOfOrder, told);
    // The rows told are marked in the table.
    assert.deepEqual(
      p.changes.filter(c => c.outOfOrder).map(c => [c.label, c.outOfOrder]),
      [
        ['A1E', { line: 4, id: 'A6E' }],
        ['A4E', { line: 2, id: 'A5E' }],
      ],
    );

    // The page with the rows between, as the view asks (A2E, row 3).
    await call('match_notebook', {
      kind: 'emergence',
      year: 2026,
      replaceProposalId: out.proposalId,
      view: { between: true },
      lines: [
        { raw: 'A1E ♂', values: { Insectary_ID: 'A1E', Sex: 'male' } },
        { raw: 'A3E ♀', values: { Insectary_ID: 'A3E', Sex: 'female' } },
      ],
    });
    const [again] = (await list()).proposals;
    assert.deepEqual(
      again.changes.map(c => [c.row, !!c.gap]),
      [
        [2, false],
        [3, true],
        [4, false],
      ],
    );
    assert.equal(again.outOfOrder, undefined, 'in step with the sheet');
  } finally {
    store.close?.();
  }
});

test('repeated IDs: a line finds the repeat (A0E.1) over its empty pre-made row; rows added by hand take their base line; the table is told', async () => {
  // A0E–A2E pre-made and empty high up; the butterflies were typed as repeats after Z9D.
  const used = (id, row) => ({ row, values: { Insectary_ID: id, Sex: 'female', 'CLUTCH NUMBER': 800 + row } });
  const { store, call, list, at } = await setup([
    { row: 2, values: { Insectary_ID: 'A0E' } },
    { row: 3, values: { Insectary_ID: 'A1E' } },
    { row: 4, values: { Insectary_ID: 'A2E' } },
    used('Z8D', 5),
    used('Z9D', 6),
    used('A0E.1', 7),
    used('A1E.1', 8),
    used('A2E.1', 9),
  ]);
  try {
    const out = await call('match_notebook', {
      kind: 'deaths',
      year: 2026,
      lines: [
        { raw: 'Z9D 2/10', values: { Insectary_ID: 'Z9D', Death_date: '2/10' } },
        { raw: 'A0E 2/10', values: { Insectary_ID: 'A0E', Death_date: '2/10' } },
        { raw: 'A1E 3/10', values: { Insectary_ID: 'A1E', Death_date: '3/10' } },
        // Only its ID on the page: nothing tells it is the repeat, it stays with its row.
        { raw: 'A2E', values: { Insectary_ID: 'A2E' } },
      ],
    });
    assert.ok(out.proposalId, JSON.stringify(out));
    let [p] = (await list()).proposals;
    assert.deepEqual(
      p.changes.map(c => [c.row, c.label, c.page?.line ?? null, !!c.context]),
      [
        [4, 'A2E', 4, true],
        [6, 'Z9D', 1, false],
        [7, 'A0E.1', 2, false],
        [8, 'A1E.1', 3, false],
      ],
    );
    const repeat = p.changes.find(c => c.label === 'A0E.1');
    assert.deepEqual(repeat.repeatOf, { id: 'A0E', row: 2, empty: true, above: 'Z9D' });
    assert.deepEqual(p.changes.find(c => c.label === 'A1E.1').repeatOf, { id: 'A1E', row: 3, empty: true, above: 'A0E.1' });
    assert.equal(p.changes.find(c => c.label === 'Z9D').repeatOf, undefined);

    // A death added by hand for A2E.1: it takes the line of A2E (its base), in its place on the page.
    const added = await call('update_proposal', {
      proposalId: out.proposalId,
      changes: [{ recordId: at(9), values: { Death_date: '2026-10-04' } }],
    });
    assert.ok(!added.error, added.error);
    [p] = (await list()).proposals;
    const hand = p.changes.find(c => c.label === 'A2E.1');
    assert.deepEqual(hand.page && [hand.page.photo, hand.page.line], [0, 4]);
    assert.ok(!p.changes.some(c => c.label === 'A2E'), "the line's own row gives way to the change made for it");
  } finally {
    store.close?.();
  }
});

test("a marker's sheet rows, opened from the table: by row range, up to PEEK_ROWS, for whoever may see the proposal", async () => {
  const { store, assistant, call, at } = await setup(
    Array.from({ length: 119 }, (_, i) => ({
      row: i + 2,
      values: { Insectary_ID: `R${i + 2}`, Sex: 'NA', Tube_1_rack: 'A1' },
    })),
  );
  const rows = (user, id, query) => assistant.handle({ method: 'GET', path: `/api/chat/proposals/${id}/rows`, user, query });
  try {
    const out = await call('propose_changes', {
      reason: 'Lejos',
      changes: [2, 120].map(row => ({ recordId: at(row), values: { Sex: 'male' } })),
    });
    assert.ok(!out.error, out.error);
    const franz = { id: 'u1', username: 'franz', role: 'editor' };
    const got = await rows(franz, out.proposalId, { sheet: 'Insectary_data', from: '3', to: '119' });
    assert.equal(got.status, 200);
    assert.equal(got.body.rows.length, PEEK_ROWS);
    assert.deepEqual([got.body.rows[0].row, got.body.rows.at(-1).row], [3, 2 + PEEK_ROWS]);
    assert.deepEqual(got.body.rest, { from: 3 + PEEK_ROWS, to: 119 });
    const first = got.body.rows[0];
    assert.equal(first.label, 'R3');
    assert.equal(first.recordId, at(3));
    assert.equal(first.values.Sex, 'NA');
    assert.ok(!('Tube_1_rack' in first.values), 'the columns proposals never show stay out');
    // A short range: all of it, nothing more.
    const few = await rows(franz, out.proposalId, { sheet: 'Insectary_data', from: '10', to: '12' });
    assert.deepEqual(
      few.body.rows.map(r => r.row),
      [10, 11, 12],
    );
    assert.equal(few.body.rest, undefined);
    // Another editor may (a chat handed over); a viewer who does not own it may not.
    const ana = { id: 'u2', username: 'ana', role: 'editor' };
    assert.equal((await rows(ana, out.proposalId, { sheet: 'Insectary_data', from: '10', to: '12' })).status, 200);
    const vera = { id: 'u3', username: 'vera', role: 'viewer' };
    assert.equal((await rows(vera, out.proposalId, { sheet: 'Insectary_data', from: '10', to: '12' })).status, 404);
    // Only the proposal's sheets, and a range that reads as one.
    assert.equal((await rows(franz, out.proposalId, { sheet: 'Collection_data', from: '3', to: '5' })).status, 404);
    assert.equal((await rows(franz, out.proposalId, { sheet: 'Insectary_data', from: '9', to: '5' })).status, 400);
    assert.equal((await rows(franz, out.proposalId, { sheet: 'Insectary_data', from: 'x', to: '5' })).status, 400);
    assert.equal((await rows(null, out.proposalId, { sheet: 'Insectary_data', from: '3', to: '5' })).status, 401);
  } finally {
    store.close?.();
  }
});

test("a page line shown as the sheet has it takes the person's edits in place (a death moved onto it), and goes back to context when undone", async () => {
  const { store, assistant, call, list, at } = await setup([2, 3, 4].map(row => ({ row, values: { Insectary_ID: `L${row}C`, Sex: 'female' } })));
  const user = { id: 'u1', username: 'franz', displayName: 'Franz', role: 'editor' };
  const edit = (id, cells) => assistant.handle({ method: 'POST', path: `/api/chat/proposals/${id}/edit`, body: { cells }, user, query: {} });
  try {
    // The death of L3C read on L4C's line.
    const out = await call('match_notebook', {
      kind: 'deaths',
      year: 2026,
      lines: [
        { raw: 'L3C', values: { Insectary_ID: 'L3C' } },
        { raw: 'L4C 2/10 unk', values: { Insectary_ID: 'L4C', Death_date: '2/10', Death_cause: 'Unknown' } },
      ],
    });
    assert.ok(out.proposalId, JSON.stringify(out));
    let [p] = (await list()).proposals;
    const l3 = p.changes.find(c => c.label === 'L3C');
    const l4 = p.changes.find(c => c.label === 'L4C');
    assert.deepEqual([l3.context, l3.index < 0, l3.key], [true, true, at(3)]);
    const moved = Object.keys(l4.values);
    assert.ok(moved.includes('Death_date') && moved.includes('CAM_ID'), moved.join());

    // Moved up as the table moves it: L3C typed, L4C back to the sheet's values.
    const done = await edit(out.proposalId, [
      ...moved.map(field => ({ key: l3.key, field, value: l4.values[field], before: null })),
      ...moved.map(field => ({ key: l4.key, field, value: null, before: l4.values[field], use: 'sheet' })),
    ]);
    assert.equal(done.status, 200, JSON.stringify(done.body));
    assert.deepEqual(done.body.rejected, []);
    [p] = (await list()).proposals;
    const now = p.changes.find(c => c.key === l3.key);
    assert.ok(!now.context && now.index >= 0, 'a row of the proposal now');
    assert.equal(now.recordId, at(3));
    assert.deepEqual([now.row, now.page?.line, now.page?.photo], [3, 1, 0], 'same row, same line of the page');
    assert.equal(now.values.Death_date, l4.values.Death_date);
    assert.deepEqual(
      p.changes.map(c => [c.row, c.label, c.page?.line ?? null, !!c.context]),
      [
        [3, 'L3C', 1, false],
        [4, 'L4C', 2, false],
      ],
      'in its place in the sheet and on the page',
    );
    assert.deepEqual(p.changes.find(c => c.label === 'L4C').values, {});

    // Undone: L3C has nothing proposed again and is the page's line as the sheet has it, as before.
    await edit(out.proposalId, [
      ...moved.map(field => ({ key: l3.key, field, value: null, before: l4.values[field], use: 'sheet' })),
      ...moved.map(field => ({ key: l4.key, field, value: l4.values[field], before: null })),
    ]);
    [p] = (await list()).proposals;
    const back = p.changes.find(c => c.key === l3.key);
    assert.deepEqual([back.context, back.values, back.personEdits, back.page?.line], [true, {}, undefined, 1]);
    assert.equal(p.changes.find(c => c.label === 'L4C').values.Death_date, l4.values.Death_date);
    // Applying writes L4C only.
    const applied = await call('apply_proposal', { proposalId: out.proposalId });
    assert.equal(applied.rows, 1, JSON.stringify(applied));
    assert.equal(store.getRecordBySheetRow('Insectary_data', 3).values.Death_date ?? null, null);
  } finally {
    store.close?.();
  }
});

test('typing into a page line shown as the sheet has it makes it a change; the sheet rows between rows of other proposals stay read-only', async () => {
  const { store, assistant, call, list, at } = await setup([2, 3, 4, 5].map(row => ({ row, values: { Insectary_ID: `M${row}C`, Sex: 'NA' } })));
  const user = { id: 'u1', username: 'franz', displayName: 'Franz', role: 'editor' };
  const edit = (id, cells) => assistant.handle({ method: 'POST', path: `/api/chat/proposals/${id}/edit`, body: { cells }, user, query: {} });
  try {
    const page = await call('match_notebook', {
      kind: 'deaths',
      year: 2026,
      lines: [
        { raw: 'M2C', values: { Insectary_ID: 'M2C' } },
        { raw: 'M3C 2/10 unk', values: { Insectary_ID: 'M3C', Death_date: '2/10', Death_cause: 'Unknown' } },
      ],
    });
    const typed = await edit(page.proposalId, [{ key: at(2), field: 'Sex', value: 'male', before: null }]);
    assert.deepEqual(typed.body.rejected, []);
    const row = typed.body.proposal.changes.find(c => c.key === at(2));
    assert.deepEqual([!!row.context, row.values, row.page?.line, row.row], [false, { Sex: 'male' }, 1, 2]);

    // A proposal without a page: the sheet's rows between its rows are only to read.
    const plain = await call('propose_changes', {
      reason: 'Sexos',
      changes: [2, 5].map(r => ({ recordId: at(r), values: { Sex: 'female' } })),
    });
    const p = (await list()).proposals.find(x => x.id === plain.proposalId);
    const gap = p.changes.find(c => c.gap);
    assert.ok(gap, 'a row in between is shown');
    const refused = await edit(plain.proposalId, [{ key: gap.key, field: 'Sex', value: 'male', before: null }]);
    assert.equal(refused.body.rejected.length, 1);
    assert.ok(!refused.body.proposal.changes.some(c => c.key === gap.key && !c.context));
  } finally {
    store.close?.();
  }
});
