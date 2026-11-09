import test from 'node:test';
import assert from 'node:assert/strict';
import { DatabaseSync } from 'node:sqlite';
import { execFileSync } from 'node:child_process';
import { mkdirSync, mkdtempSync, readFileSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import {
  DOC_TOOLS,
  budgetChanges,
  budgetGrowth,
  createDocReview,
  createGitSync,
  docChecks,
  effectiveFrom,
  findSection,
  hashText,
  locate,
  overlaidFiles,
  overlayReader,
  replacePart,
  sections,
  threadSection,
} from '../server/doc-review.mjs';
import { mcpTools } from '../server/assistant.mjs';
import { composeBrief, GENERIC_PERSON, SERVER_PATHS } from '../server/brief.mjs';

// Comments on the instructions and Claude's changes to them (server/doc-review.mjs): anchoring, permissions,
// the overlay the page, the brief and the workspaces read, stale changes, undo, git, the MCP tools.

const SKILL = `---
name: demo
description: A demo skill.
---

# Demo

Intro line.

## Datos

Usa NA para el sexo cuando no se sabe.

### Detalle

Una línea más.

## Fotos

Dos fotos por lado.
`;

function fixture() {
  const root = mkdtempSync(join(tmpdir(), 'doc-review-'));
  const put = (rel, text) => {
    mkdirSync(dirname(join(root, rel)), { recursive: true });
    writeFileSync(join(root, rel), text);
  };
  put('assistant/AGENTS.md', '# Brief\n\nHola {{person}}.\n\n## Reglas\n\nUna regla.\n');
  put('assistant/voice/app.md', '## Tono\n\nCorto.\n');
  put('assistant/voice/core.md', 'Sin emojis.\n');
  put('assistant/skills/demo/SKILL.md', SKILL);
  put('assistant/skills/app-guide/SKILL.md', '---\nname: app-guide\ndescription: Guide.\n---\n\n# Guide\n\nSee [r](reference/r.md) and #/tablas.\n');
  put('assistant/skills/app-guide/reference/r.md', '# R\n\nOpen #/tablas.\n');
  put('frontend/src/router.ts', "{ path: '/tablas' }\n");
  put('frontend/src/tabs.ts', '\n');
  const db = new DatabaseSync(':memory:');
  db.exec('CREATE TABLE users(id TEXT PRIMARY KEY, username TEXT, display_name TEXT, role TEXT, active INTEGER, email TEXT)');
  db.prepare("INSERT INTO users VALUES ('u-admin','franz','Franz Chandi','admin',1,'franz@example.org')").run();
  db.prepare("INSERT INTO users VALUES ('u-obs','ana','Ana Pérez','observer',1,'ana@example.org')").run();
  db.prepare("INSERT INTO users VALUES ('u-ed','luis','Luis','editor',1,'luis@example.org')").run();
  return { root, db, put };
}
const admin = { id: 'u-admin', username: 'franz', role: 'admin' };
const observer = { id: 'u-obs', username: 'ana', role: 'observer' };
const editor = { id: 'u-ed', username: 'luis', role: 'editor' };
const SKILL_PATH = 'assistant/skills/demo/SKILL.md';

test('sections run to the next heading of their level; the start before the first heading is its own; fences do not count', () => {
  const text = `${SKILL}\n\`\`\`\n## not a heading\n\`\`\`\n`;
  const all = sections(text);
  assert.deepEqual(
    all.map(s => [s.heading, s.level]),
    [
      ['Demo', 1],
      ['Datos', 2],
      ['Detalle', 3],
      ['Fotos', 2],
    ],
  );
  const datos = findSection(text, 'Datos');
  assert.match(datos.text, /^## Datos\n[\s\S]*### Detalle[\s\S]*Una línea más\.\n\n$/);
  assert.equal(findSection('Intro.\n\n# A\n\nx\n', '').text, 'Intro.\n\n');
  assert.equal(findSection(text, '`Datos`').heading, 'Datos', 'compared without Markdown marks');
  assert.equal(findSection(text, 'Nada'), null);
});

test('a quote is placed by its prefix and suffix, ties to the nearest offset; a quote that is gone is -1', () => {
  const text = 'el sexo NA aquí. Otra vez el sexo NA allá.';
  const second = text.lastIndexOf('sexo NA');
  assert.equal(locate(text, { quote: 'sexo NA', prefix: 'Otra vez el ', suffix: ' allá' }), second);
  assert.equal(locate(text, { quote: 'sexo NA', offset: 30 }), second);
  assert.equal(locate(text, { quote: 'sexo NA', offset: 0 }), text.indexOf('sexo NA'));
  assert.equal(locate(text, { quote: 'machos' }), -1);
});

test('a part replaces once; a part found twice or not at all is refused; the whole section keeps its blank line', () => {
  assert.equal(replacePart('## A\n\nx y x\n\n', 'y', 'z'), '## A\n\nx z x\n\n');
  assert.equal(replacePart('## A\n\nx y x\n\n', 'x', 'z'), null);
  assert.equal(replacePart('## A\n\nx\n\n', 'w', 'z'), null);
  assert.equal(replacePart('## A\n\nx\n\n', '## A\n\nx', '## A\n\nw'), '## A\n\nw\n\n');
});

test('the overlay: the shipped file until a chain starts at its hash; a release that caught up goes quiet; a conflicting one stops it', () => {
  const A = 'a\n';
  const B = 'b\n';
  const C = 'c\n';
  const r1 = { id: 1, parent_hash: hashText(A), hash: hashText(B), text: B };
  const r2 = { id: 2, parent_hash: hashText(B), hash: hashText(C), text: C };
  assert.equal(effectiveFrom(A, []).text, A);
  assert.equal(effectiveFrom(A, [r1]).text, B);
  assert.equal(effectiveFrom(A, [r1, r2]).text, C);
  // A release with r1's commit: r2 still applies on it; with both, nothing does.
  assert.deepEqual(effectiveFrom(B, [r1, r2]).used, [2]);
  assert.deepEqual(effectiveFrom(C, [r1, r2]).used, []);
  // A release that changed the file otherwise: its text wins, the revisions do not apply.
  assert.equal(effectiveFrom('x\n', [r1, r2]).text, 'x\n');
  // Undo and apply again: back and forth along the chain.
  const r3 = { id: 3, parent_hash: hashText(B), hash: hashText(A), text: A };
  const r4 = { id: 4, parent_hash: hashText(A), hash: hashText(B), text: B };
  assert.equal(effectiveFrom(A, [r1, r3]).text, A);
  assert.equal(effectiveFrom(A, [r1, r3, r4]).text, B);
});

test('any signed-in person comments, replies, resolves and reopens; only the author edits or deletes; names, never usernames or emails', () => {
  const { root, db } = fixture();
  const d = createDocReview({ db, root });
  const t = d.createThread(observer, { path: SKILL_PATH, quote: 'Usa NA para el sexo', prefix: 'Datos', suffix: ' cuando', offset: 20, heading: 'Datos', body: '¿NA o NOT_COLLECTED?' });
  assert.equal(t.comments[0].author, 'Ana Pérez');
  assert.equal(t.comments[0].own, true);
  const replied = d.reply(editor, t.id, 'NOT_COLLECTED');
  assert.equal(replied.comments.length, 2);
  assert.throws(() => d.editComment(editor, t.comments[0].id, 'x'), /Only your own/);
  assert.throws(() => d.deleteComment(admin, t.comments[0].id), /Only your own/);
  d.editComment(observer, t.comments[0].id, '¿NA o NOT_COLLECTED? (sexo)');
  assert.equal(d.resolve(editor, t.id).status, 'resolved');
  assert.equal(d.reopen(observer, t.id).status, 'open');
  // A deleted comment with replies stays as «Comentario borrado»; a thread with none left goes.
  d.deleteComment(observer, t.comments[0].id);
  const state = d.state(editor);
  const shown = state.threads.find(x => x.id === t.id);
  assert.equal(shown.comments[0].deleted, true);
  assert.equal(shown.comments[0].body, '');
  assert.equal(shown.comments[1].own, true);
  const text = JSON.stringify(state);
  for (const secret of ['ana', 'luis', 'franz', '@example.org']) assert.ok(!text.includes(`"${secret}"`) && !text.includes(secret + '@'), `no ${secret}`);
  assert.doesNotMatch(text, /example\.org/);
  const lone = d.createThread(editor, { path: SKILL_PATH, quote: 'Dos fotos', heading: 'Fotos', body: 'x' });
  d.deleteComment(editor, lone.comments[0].id);
  assert.equal(d.state(editor).threads.some(x => x.id === lone.id), false);
  assert.equal(state.counts[SKILL_PATH], 1);
  // Only files under assistant/ that exist.
  assert.throws(() => d.createThread(editor, { path: 'server/index.mjs', quote: 'x', body: 'x' }), /Not an instructions file/);
  assert.throws(() => d.createThread(editor, { path: 'assistant/../x.md', quote: 'x', body: 'x' }), /Not an instructions file/);
  assert.throws(() => d.createThread(null, { path: SKILL_PATH, quote: 'x', body: 'x' }), /Sign in/);
});

test('MCP: doc_threads gives the section source; propose refuses a before that is not there, and non-admin chats; apply resolves its threads', async () => {
  const { root, db } = fixture();
  const refreshed = [];
  const d = createDocReview({ db, root, refresh: () => refreshed.push(1) });
  const t = d.createThread(observer, { path: SKILL_PATH, quote: 'Usa NA para el sexo', heading: 'Datos', body: 'Debería ser NOT_COLLECTED' });
  const threads = await d.runTool('doc_threads', {}, { user: admin });
  assert.equal(threads.threads[0].id, t.id);
  assert.match(threads.threads[0].section, /^## Datos\n\nUsa NA para el sexo cuando no se sabe\./);
  assert.deepEqual(threads.threads[0].comments, [{ by: 'Ana Pérez', text: 'Debería ser NOT_COLLECTED' }]);

  assert.match((await d.runTool('propose_doc_change', { path: SKILL_PATH, heading: 'Datos', before: 'Usa NA para el sexo si', after: 'x' }, { user: admin })).error, /does not match/);
  assert.match((await d.runTool('propose_doc_change', { path: SKILL_PATH, heading: 'Datos', before: 'Usa NA', after: 'x' }, { user: editor })).error, /administrator/);
  assert.match((await d.runTool('propose_doc_change', { path: 'server/x.md', heading: '', before: 'a', after: 'b' }, { user: admin })).error, /Not an instructions file/);
  assert.match((await d.runTool('propose_doc_change', { path: SKILL_PATH, heading: 'Nada', before: 'a', after: 'b' }, { user: admin })).error, /No section/);

  assert.deepEqual(d.openThreads(), [t.id]);
  const made = await d.runTool('propose_doc_change', { path: SKILL_PATH, heading: 'Datos', before: 'Usa NA para el sexo', after: 'Usa NOT_COLLECTED para el sexo', thread_ids: [t.id] }, { user: admin, chat: 'chat-1' });
  assert.equal(made.version, 1);
  assert.deepEqual(d.openThreads(), [], 'a thread a pending change answers is not asked again');
  const shown = d.state(observer).changes[0];
  assert.equal(shown.status, 'pending');
  assert.match(shown.diff, /^@@/);
  assert.match(shown.diff, /^-NA$/m);
  assert.match(shown.diff, /^\+NOT_COLLECTED$/m);
  assert.equal(shown.chat, 'chat-1');
  assert.equal(shown.stale, false);

  assert.equal((await d.runTool('reply_doc_thread', { id: t.id, text: 'Propuesto.' }, { user: admin })).ok, true);
  assert.equal(d.state(observer).threads[0].comments.at(-1).kind, 'claude');
  assert.equal(d.state(observer).threads[0].comments.at(-1).author, null);

  // Only admins apply, discard and undo.
  assert.throws(() => d.apply(editor, made.changeId), /Only administrators/);
  assert.throws(() => d.discard(observer, made.changeId), /Only administrators/);
  const out = d.apply(admin, made.changeId);
  assert.equal(out.change.status, 'applied');
  assert.match(d.effectiveText(SKILL_PATH), /Usa NOT_COLLECTED para el sexo cuando no se sabe/);
  assert.equal(readFileSync(join(root, SKILL_PATH), 'utf8'), SKILL, 'the shipped file is not written');
  const thread = d.state(observer).threads[0];
  assert.equal(thread.status, 'resolved');
  assert.equal(thread.resolvedByChange, made.changeId);
  assert.equal(thread.comments.at(-1).kind, 'system');
  await d.idle();
  assert.equal(refreshed.length, 1, 'the workspaces are refreshed');
  // The other readers: a reader of the database (t3-provision, the budget) and the files the overlay changes.
  assert.match(overlayReader(db, root)(SKILL_PATH), /NOT_COLLECTED/);
  assert.deepEqual(overlaidFiles(db, root).map(f => f.path), [SKILL_PATH]);
});

test('a change whose section changed since is stale and cannot be applied; revise makes v2 on the text now', async () => {
  const { root, db } = fixture();
  const d = createDocReview({ db, root });
  const one = await d.runTool('propose_doc_change', { path: SKILL_PATH, heading: 'Fotos', before: 'Dos fotos', after: 'Tres fotos' }, { user: admin });
  const two = await d.runTool('propose_doc_change', { path: SKILL_PATH, heading: 'Fotos', before: 'por lado', after: 'por cada lado' }, { user: admin });
  d.apply(admin, two.changeId);
  const stale = d.state(admin).changes.find(c => c.seriesId === one.changeId);
  assert.equal(stale.stale, true);
  assert.throws(() => d.apply(admin, one.changeId), /changed since/);
  // Claude redoes it on the section now: v2 replaces v1 on the card, v1 one click away.
  const v2 = await d.runTool('revise_doc_change', { change_id: one.changeId, after: 'Tres fotos' }, { user: admin });
  assert.equal(v2.version, 2);
  // Aplicar from a screen that still shows v1: refused, nothing applied.
  assert.throws(() => d.apply(admin, one.changeId, { version: 1 }), e => e.code === 'NOT_LATEST');
  const card = d.state(admin).changes.find(c => c.seriesId === one.changeId);
  assert.equal(card.version, 2);
  assert.equal(card.stale, false);
  assert.deepEqual(card.versions.map(v => [v.version, v.status]), [
    [1, 'superseded'],
    [2, 'pending'],
  ]);
  d.apply(admin, one.changeId, { version: 2 });
  assert.match(d.effectiveText(SKILL_PATH), /Tres fotos por cada lado/);
});

test('undo writes the section back, apply again redoes it; undo on a section changed since becomes a new proposal', async () => {
  const { root, db } = fixture();
  const d = createDocReview({ db, root });
  const t = d.createThread(observer, { path: SKILL_PATH, quote: 'Usa NA', heading: 'Datos', body: 'NOT_COLLECTED' });
  const c = await d.runTool('propose_doc_change', { path: SKILL_PATH, heading: 'Datos', before: 'Usa NA', after: 'Usa NOT_COLLECTED', thread_ids: [t.id] }, { user: admin });
  d.apply(admin, c.changeId);
  const system = () => d.state(observer).threads[0].comments.filter(x => x.kind === 'system').map(x => x.body);
  assert.equal(d.state(observer).threads[0].status, 'resolved');
  await assert.rejects(d.undo(editor, c.changeId), /Only administrators/);
  const undone = await d.undo(admin, c.changeId);
  assert.equal(undone.change.status, 'undone');
  assert.equal(d.effectiveText(SKILL_PATH), SKILL);
  // Its thread is open again, with a line saying why; applying again resolves it again.
  assert.equal(d.state(observer).threads[0].status, 'open');
  assert.deepEqual(system(), ['resolved_by_change', 'reopened_by_undo']);
  assert.deepEqual(d.openThreads(), [t.id]);
  d.apply(admin, c.changeId);
  assert.match(d.effectiveText(SKILL_PATH), /Usa NOT_COLLECTED/);
  assert.equal(d.state(observer).threads[0].status, 'resolved');
  assert.deepEqual(system(), ['resolved_by_change', 'reopened_by_undo', 'resolved_by_change']);
  assert.deepEqual(d.revisions(SKILL_PATH).map(r => [r.kind, r.author]), [
    ['apply', 'Franz Chandi'],
    ['undo', 'Franz Chandi'],
    ['apply', 'Franz Chandi'],
  ]);
  assert.match((await d.revisionDiff(SKILL_PATH, d.revisions(SKILL_PATH)[1].revision)).diff, /^\+NA$/m);
  // Another change of the same section after it: undoing the first is a proposal now.
  const other = await d.runTool('propose_doc_change', { path: SKILL_PATH, heading: 'Datos', before: 'cuando no se sabe', after: 'si no se sabe' }, { user: admin });
  d.apply(admin, other.changeId);
  const proposed = await d.undo(admin, c.changeId);
  assert.equal(proposed.proposed.status, 'pending');
  assert.match(proposed.proposed.after, /Usa NA para el sexo si no se sabe/);
});

test('a release that changed the file without the change: the overlay stops and the change says so; «Volver a proponer»', async () => {
  const { root, db, put } = fixture();
  const d = createDocReview({ db, root });
  const c = await d.runTool('propose_doc_change', { path: SKILL_PATH, heading: 'Fotos', before: 'Dos fotos', after: 'Tres fotos' }, { user: admin });
  d.apply(admin, c.changeId);
  // Deployed with the commit: same text, nothing left to overlay, the change is not «lost».
  put(SKILL_PATH, d.effectiveText(SKILL_PATH));
  assert.equal(d.state(admin).changes[0].lost, false);
  // Deployed from a branch without it (someone else changed the file).
  put(SKILL_PATH, SKILL.replace('Intro line.', 'Intro line, edited.'));
  assert.match(d.effectiveText(SKILL_PATH), /Dos fotos/);
  assert.match(d.effectiveText(SKILL_PATH), /edited/);
  assert.equal(d.state(admin).changes[0].lost, true);
  const again = await d.repropose(admin, c.changeId);
  assert.equal(again.change.status, 'pending');
  assert.equal(again.change.version, 2);
  d.apply(admin, c.changeId);
  assert.match(d.effectiveText(SKILL_PATH), /edited[\s\S]*Tres fotos/);
});

test('the checks of assistant/ run on apply: a broken front matter, an unknown placeholder, a guide link or route', async () => {
  const { root, db } = fixture();
  const read = rel => readFileSync(join(root, rel), 'utf8');
  assert.deepEqual(docChecks(SKILL_PATH, read, root), []);
  assert.deepEqual(docChecks('assistant/AGENTS.md', read, root), []);
  assert.deepEqual(docChecks('assistant/skills/app-guide/SKILL.md', read, root), []);
  const broken = rel => (rel === SKILL_PATH ? SKILL.replace('name: demo', 'name: other') : read(rel));
  assert.deepEqual(docChecks(SKILL_PATH, broken, root), [{ code: 'front_matter', file: SKILL_PATH }]);
  const brief = rel => (rel === 'assistant/AGENTS.md' ? read(rel).replace('Una regla.', 'Una {{regla}}.') : read(rel));
  assert.deepEqual(docChecks('assistant/AGENTS.md', brief, root), [{ code: 'placeholder', text: '{{regla}}' }]);
  const guide = rel => (rel === 'assistant/skills/app-guide/SKILL.md' ? read(rel).replace('[r](reference/r.md)', 'r').replace('#/tablas', '#/nada') : read(rel));
  assert.deepEqual(docChecks('assistant/skills/app-guide/SKILL.md', guide, root), [
    { code: 'guide_link', file: 'reference/r.md' },
    { code: 'route', route: 'nada' },
  ]);
  // Over MCP a change the checks refuse is not made.
  const d = createDocReview({ db, root });
  const out = await d.runTool('propose_doc_change', { path: 'assistant/AGENTS.md', heading: 'Reglas', before: 'Una regla.', after: 'Una {{regla}}.' }, { user: admin });
  assert.match(out.error, /Refused by the checks.*placeholder/);
  assert.equal(d.state(admin).changes.length, 0);
});

test('the context-size ratchet: a growth past the recorded bytes is shown and needs «Aplicar igualmente»', async () => {
  const { root, db } = fixture();
  assert.deepEqual(budgetGrowth({ brief: { bytes: 100 } }, { brief: 100 }, { brief: 130, 'skills (total)': 999 }), [{ part: 'brief', bytes: 30 }]);
  // Already over (an earlier change applied anyway): only this change's own growth counts.
  assert.deepEqual(budgetGrowth({ brief: { bytes: 100 } }, { brief: 120 }, { brief: 130 }), [{ part: 'brief', bytes: 10 }]);
  assert.deepEqual(budgetGrowth({ brief: { bytes: 100 } }, { brief: 100 }, { brief: 90 }), []);
  // The brief measured with the overlay, as scripts/context-budget.mjs does (composeBrief with a reader).
  const measure = read => ({ brief: Buffer.byteLength(composeBrief(GENERIC_PERSON, { ...SERVER_PATHS, root, read })) });
  const recorded = measure(rel => readFileSync(join(root, rel), 'utf8')).brief;
  const d = createDocReview({ db, root, measure, budget: () => ({ brief: { bytes: recorded } }) });
  const grows = await d.runTool('propose_doc_change', { path: 'assistant/AGENTS.md', heading: 'Reglas', before: 'Una regla.', after: 'Una regla más larga.' }, { user: admin });
  assert.deepEqual(grows.grows, ['brief +11 bytes: an admin applies it anyway or not']);
  assert.deepEqual(d.state(admin).changes[0].budget, [{ part: 'brief', bytes: 11 }]);
  assert.throws(() => d.apply(admin, grows.changeId), e => e.code === 'GROWS' && e.details.budget[0].bytes === 11);
  d.apply(admin, grows.changeId, { anyway: true });
  assert.match(d.effectiveText('assistant/AGENTS.md'), /más larga/);
  // A change that shrinks it passes.
  const shrink = await d.runTool('propose_doc_change', { path: 'assistant/AGENTS.md', heading: 'Reglas', before: 'Una regla más larga.', after: 'Regla.' }, { user: admin });
  assert.equal(shrink.grows, undefined);
  d.apply(admin, shrink.changeId);
});

test('the budget file after a change: only the parts it moved, a growth with its reason, a shrink keeps its own', () => {
  const budget = { about: 'x', parts: { brief: { bytes: 100, reason: 'old' }, 'skill:a': { bytes: 5, reason: 'drift' } } };
  const grown = budgetChanges(budget, { brief: 90, 'skill:a': 7 }, { brief: 95, 'skill:a': 7 }, 'change 1');
  assert.deepEqual(grown.parts, { brief: { bytes: 105, reason: 'change 1' }, 'skill:a': { bytes: 5, reason: 'drift' } });
  assert.equal(grown.about, 'x');
  assert.deepEqual(budgetChanges(budget, { brief: 90 }, { brief: 80 }, 'change 2').parts.brief, { bytes: 90, reason: 'old' });
  assert.equal(budgetChanges(budget, { brief: 90 }, { brief: 90 }, 'r'), null);
});

test('the doc tools are deferred and carry the writing rules', () => {
  const tools = mcpTools().filter(t => DOC_TOOLS.some(d => d.function.name === t.name));
  assert.equal(tools.length, 4);
  for (const t of tools) assert.equal(t._meta?.['anthropic/alwaysLoad'], undefined, `${t.name} deferred`);
  for (const name of ['propose_doc_change', 'revise_doc_change']) {
    const d = tools.find(t => t.name === name).description;
    assert.match(d, /as it is now/);
    assert.match(d, /smallest change/);
    assert.match(d, /no history/);
    assert.match(d, /past mistakes/);
    assert.match(d, /never\/always/);
    assert.ok(d.length < 420, `${name} stays short (${d.length})`);
  }
});

test('the brief: the page and the workspaces read AGENTS.md and the voice through the overlay', async () => {
  const { root, db } = fixture();
  const d = createDocReview({ db, root });
  const c = await d.runTool('propose_doc_change', { path: 'assistant/voice/app.md', heading: 'Tono', before: 'Corto.', after: 'Corto y claro.' }, { user: admin });
  d.apply(admin, c.changeId);
  const brief = composeBrief(GENERIC_PERSON, { ...SERVER_PATHS, root, read: d.read });
  assert.match(brief, /Hola ‹person›\.[\s\S]*Corto y claro\./);
  // A thread on the brief's voice part finds its section in the voice file.
  const s = threadSection(d.read, 'assistant/AGENTS.md', { quote: 'Corto y claro', heading: 'Tono' });
  assert.equal(s.path, 'assistant/voice/app.md');
});

// ------------------------------------------------------------------ git

function repoPair() {
  const base = mkdtempSync(join(tmpdir(), 'doc-git-'));
  const g = (cwd, ...args) => execFileSync('git', ['-C', cwd, ...args], { encoding: 'utf8' });
  const remote = join(base, 'remote.git');
  execFileSync('git', ['init', '-q', '--bare', '-b', 'main', remote]);
  const seed = join(base, 'seed');
  execFileSync('git', ['clone', '-q', remote, seed]);
  mkdirSync(join(seed, 'assistant', 'skills', 'demo'), { recursive: true });
  writeFileSync(join(seed, 'assistant', 'skills', 'demo', 'SKILL.md'), SKILL);
  for (const cwd of [seed]) {
    g(cwd, 'config', 'user.email', 'test@example.invalid');
    g(cwd, 'config', 'user.name', 'Test');
  }
  g(seed, 'add', '.');
  g(seed, 'commit', '-q', '-m', 'seed');
  g(seed, 'push', '-q', 'origin', 'HEAD:main');
  const work = join(base, 'work');
  execFileSync('git', ['clone', '-q', remote, work]);
  return { base, remote, seed, work, g };
}

test('git: each revision is one commit in the worktree; pushed only when enabled; a conflict is «failed»; only assistant/**/*.md', async () => {
  const { root, db } = fixture();
  const { remote, seed, work, g } = repoPair();
  const local = createGitSync({ worktree: work, push: false, author: { name: 'App', email: 'app@example.invalid' } });
  const d = createDocReview({ db, root, git: local });
  const c = await d.runTool('propose_doc_change', { path: SKILL_PATH, heading: 'Datos', before: 'Usa NA', after: 'Usa NOT_COLLECTED' }, { user: admin });
  const out = d.apply(admin, c.changeId);
  const synced = await out.synced;
  assert.equal(synced.state, 'committed');
  assert.equal(g(work, 'log', '-1', '--format=%s').trim(), 'Instrucciones: skills/demo/SKILL.md § Datos');
  assert.match(readFileSync(join(work, SKILL_PATH), 'utf8'), /Usa NOT_COLLECTED/);
  assert.equal(g(remote, 'log', '-1', '--format=%s', 'main').trim(), 'seed', 'not pushed');
  assert.equal(d.state(admin).changes[0].revision.gitState, 'committed');
  // The undo is a commit too.
  const undone = await d.undo(admin, c.changeId);
  assert.equal((await undone.synced).state, 'committed');
  assert.match(g(work, 'log', '-1', '--format=%s').trim(), /\(deshecho\)$/);

  // Pushing: the remote got ahead meanwhile (another file): fetched and fast-forwarded first.
  writeFileSync(join(seed, 'README.md'), 'x\n');
  g(seed, 'add', '.');
  g(seed, 'commit', '-q', '-m', 'other');
  g(seed, 'push', '-q', 'origin', 'HEAD:main');
  const work2 = join(dirname(work), 'work2');
  execFileSync('git', ['clone', '-q', remote, work2]);
  const pushing = createGitSync({ worktree: work2, push: true, author: { name: 'App', email: 'app@example.invalid' } });
  const pushed = await pushing.commit({ path: SKILL_PATH, text: SKILL.replace('Dos', 'Tres'), parentHash: hashText(SKILL), sectionBefore: '', sectionAfter: '', message: 'Instrucciones: x' });
  assert.equal(pushed.state, 'pushed');
  assert.equal(g(remote, 'log', '-1', '--format=%s', 'main').trim(), 'Instrucciones: x');
  // The same section changed in git meanwhile: failed, nothing pushed.
  g(seed, 'pull', '-q', '--rebase', 'origin', 'main');
  writeFileSync(join(seed, SKILL_PATH), SKILL.replace('Dos fotos por lado', 'Cuatro fotos por lado'));
  g(seed, 'commit', '-q', '-am', 'other change');
  g(seed, 'push', '-q', 'origin', 'HEAD:main');
  const conflict = await pushing.commit({ path: SKILL_PATH, text: SKILL.replace('Dos fotos por lado', 'Cinco'), parentHash: hashText(SKILL), sectionBefore: 'Dos fotos por lado', sectionAfter: 'Cinco', message: 'Instrucciones: y' });
  assert.equal(conflict.state, 'failed');
  assert.equal(conflict.error, 'conflict');
  assert.equal((await pushing.commit({ path: 'server/index.mjs', text: 'x' })).state, 'failed');
  assert.equal((await pushing.commit({ path: 'assistant/x.json', text: 'x' })).state, 'failed');
});
