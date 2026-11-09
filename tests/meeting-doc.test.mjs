import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, mkdirSync, writeFileSync, readFileSync, statSync } from 'node:fs';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { DatabaseSync } from 'node:sqlite';
import {
  TEMPLATE_TITLE,
  agendaLines,
  cleanAgenda,
  createMeetingDocs,
  docsRequests,
  italics,
  minutesIndex,
  nextMeetingDate,
  paperHtml,
  parseTitle,
  simBackend,
  ukTime,
  whatsappText,
} from '../server/meeting-doc.mjs';

const AGENDA = {
  items: ['Infrastructure: water for the new insectary (Ángel and Kim)', { text: 'Aula 11 (Ángel)', children: ['Lab name', "Lin's grants"] }, '_Tithorea_ host plants from Cavernas (Naty)'],
  followUps: ['Sanger cluster access (Joana)'],
  other: [],
};

function knowledge() {
  const dir = mkdtempSync(join(tmpdir(), 'md-know-'));
  mkdirSync(join(dir, 'drive'));
  const md = (id, title, date) =>
    writeFileSync(join(dir, 'drive', `${id}.md`), `---\ntitle: ${title}\nsourceUrl: https://docs.google.com/document/d/${id}/edit\ndriveId: ${id}\nfolder: Meetings\nkind: meeting\ndate: ${date}\n---\n# ${title}\n`);
  md('doc142', '142 Meeting-22/10/2026', '2026-10-22');
  md('doc143', '143 Meeting-29/10/2026', '2026-10-29');
  md('doc9', '9 Meeting-16/3/23', '2023-03-16');
  md('tpl', TEMPLATE_TITLE, '');
  return dir;
}

function setup({ folders = null } = {}) {
  const dir = mkdtempSync(join(tmpdir(), 'md-sim-'));
  const backend = simBackend({ dir, templateId: 'tpl', folders: ['meetings'], publicUrl: 'http://lab' });
  const db = new DatabaseSync(':memory:');
  const docs = createMeetingDocs({
    db,
    backend,
    picture: async (_b, docId, _p, out) => (writeFileSync(out, `png of ${docId}`), out),
    config: { templateId: 'tpl', folderId: 'meetings', folderName: 'Meetings', folders: folders ?? ['meetings'], knowledgeRoots: [knowledge()], publicUrl: 'http://lab', pictureDir: join(dir, 'pictures'), today: () => '2026-11-04' },
  });
  return { docs, backend, db, dir };
}
const users = {
  admin: { username: 'franz', role: 'admin' },
  kim: { username: 'kim', role: 'editor' },
  obs: { username: 'visitor', role: 'observer' },
};

test('dates, titles and UK time', () => {
  assert.equal(nextMeetingDate('2026-11-04'), '2026-11-05');
  assert.equal(nextMeetingDate('2026-11-05'), '2026-11-12');
  assert.equal(nextMeetingDate('2026-11-05', { sameDay: true }), '2026-11-05');
  assert.equal(ukTime('2026-10-22'), '3:00pm'); // BST
  assert.equal(ukTime('2026-11-05'), '2:00pm'); // GMT after 25 Oct
  assert.deepEqual(parseTitle('143 Meeting-29/10/2026'), { number: 143, date: '2026-10-29' });
  assert.deepEqual(parseTitle('9 Meeting-16/3/23'), { number: 9, date: '2023-03-16' });
  assert.equal(parseTitle(TEMPLATE_TITLE), null);
});

test('minutes index reads the documents copy: newest first, the template found', () => {
  const { minutes, template } = minutesIndex([knowledge()]);
  assert.equal(minutes[0].title, '143 Meeting-29/10/2026');
  assert.equal(minutes[1].number, 142);
  assert.equal(template.id, 'tpl');
});

test('agenda lines follow the template: sections always present, empty point when none', () => {
  const lines = agendaLines(cleanAgenda(AGENDA));
  assert.deepEqual(
    lines.map(l => [l.depth, l.text, l.bold]),
    [
      [0, 'Infrastructure: water for the new insectary (Ángel and Kim)', false],
      [0, 'Aula 11 (Ángel)', false],
      [1, 'Lab name', false],
      [1, "Lin's grants", false],
      [0, '_Tithorea_ host plants from Cavernas (Naty)', false],
      [0, 'Follow-ups:', true],
      [1, 'Sanger cluster access (Joana)', false],
      [0, 'Other/various:', true],
      [1, '', false],
    ],
  );
  assert.throws(() => cleanAgenda({ items: [] }), /no points/);
  assert.deepEqual(italics('_Methona_ broods'), { plain: 'Methona broods', ranges: [[0, 7]] });
  assert.deepEqual(italics('snake_case_name stays'), { plain: 'snake_case_name stays', ranges: [] });
});

test('context: next number and date from the last minutes', () => {
  const { docs } = setup();
  const c = docs.context();
  assert.equal(c.meeting.date, '2026-11-05');
  assert.equal(c.meeting.number, 144);
  assert.equal(c.meeting.title, '144 Meeting-05/11/2026');
  assert.equal(c.meeting.ukTime, '2:00pm');
  assert.equal(c.last.title, '143 Meeting-29/10/2026');
  assert.equal(c.previous.title, '142 Meeting-22/10/2026');
  assert.equal(c.configured, true);
});

test('propose, revise, then only an allowed person applies; the copy is made in the allowed folder', async () => {
  const { docs, backend } = setup();
  assert.match((await docs.run('meeting_doc_propose', { agenda: AGENDA }, users.obs)).error, /editors/);
  const a = await docs.run('meeting_doc_propose', { agenda: AGENDA, sources: ['Ale, underground 4/11: asks for points'] }, users.kim);
  assert.equal(a.created, true);
  assert.equal(a.status, 'pending');
  assert.equal(a.title, '144 Meeting-05/11/2026');
  assert.match(a.link, /^http:\/\/lab\/api\/meeting-docs\/md-[\w]+\/preview$/);
  assert.match(a.whatsapp, /tomorrow's 🇪🇨🇬🇧 meeting \(05\/11\/2026\)\.\. 9:00am 🇪🇨, 2:00pm 🇬🇧/);
  assert.match(a.whatsapp, /\n☐ Aula 11 \(Ángel\)\n    ◦ Lab name\n/);
  assert.match(a.whatsapp, /☐ \*Follow-ups:\*/);
  const b = await docs.run('meeting_doc_propose', { agenda: { ...AGENDA, other: ['Peter Holland brings a 5 TB drive and flowcells (Joana)'] } }, users.kim);
  assert.equal(b.revised, true);
  assert.equal(b.id, a.id);
  assert.equal(b.revision, 2);
  // Nothing on "Drive" yet: only the template.
  assert.equal(await backend.findByName('144 Meeting-05/11/2026', 'meetings'), null);
  await assert.rejects(docs.apply(a.id, users.kim), /Only an administrator/);
  assert.match((await docs.run('meeting_doc_apply', { id: a.id }, users.kim)).error, /Only an administrator/);
  const done = await docs.run('meeting_doc_apply', { id: a.id }, users.admin);
  assert.equal(done.status, 'applied');
  assert.match(done.docLink, /\/api\/meeting-docs\/sim\/sim-/);
  const made = backend.get(done.docLink.split('/').pop());
  assert.equal(made.name, '144 Meeting-05/11/2026');
  assert.deepEqual(made.parents, ['meetings']);
  assert.equal(made.copiedFrom, 'tpl');
  assert.match(done.picture, /pictures\/md-\w+-\d+\.png$/);
  assert.equal(readFileSync(done.picture, 'utf8'), `png of ${made.id}`);
  assert.match((await docs.run('meeting_doc_picture', { id: a.id }, users.kim)).picture, /\.png$/);
  assert.equal(made.paragraphs[0].text, 'Meeting-05/11/2026');
  assert.deepEqual(made.paragraphs.slice(1, 5).map(p => p.text), ['Agenda', 'Infrastructure: water for the new insectary (Ángel and Kim)', 'Aula 11 (Ángel)', 'Lab name']);
  assert.ok(made.paragraphs.some(p => p.text === 'Peter Holland brings a 5 TB drive and flowcells (Joana)' && p.depth === 1));
  assert.ok(made.paragraphs.some(p => p.text === 'Resumen de la reunión'));
  await assert.rejects(docs.apply(a.id, users.admin), /applied/);
  // After apply, a new proposal for the same meeting is refused.
  assert.match((await docs.run('meeting_doc_propose', { agenda: AGENDA }, users.kim)).error, /made already/);
  assert.equal((await docs.run('meeting_doc_get', {}, users.kim)).status, 'applied');
});

test('apply refuses a folder outside the allowlist and a title that exists; the proposal stays pending', async () => {
  const off = setup({ folders: ['another-folder'] });
  const p = await off.docs.run('meeting_doc_propose', { agenda: AGENDA }, users.kim);
  await assert.rejects(off.docs.apply(p.id, users.admin), /not one the app may write in/);
  assert.equal(off.docs.get(p.id).status, 'pending');

  const { docs, backend } = setup();
  await backend.copy('tpl', '144 Meeting-05/11/2026', 'meetings');
  const q = await docs.run('meeting_doc_propose', { agenda: AGENDA }, users.kim);
  await assert.rejects(docs.apply(q.id, users.admin), /exists already/);
  const after = docs.get(q.id);
  assert.equal(after.status, 'pending');
  assert.match(after.error, /exists already/);
});

test('discard: the proposer or an allowed person; then nothing can be applied', async () => {
  const { docs } = setup();
  const p = await docs.run('meeting_doc_propose', { agenda: AGENDA }, users.kim);
  assert.throws(() => docs.discard(p.id, { username: 'other', role: 'editor' }), /Not allowed/);
  assert.equal(docs.discard(p.id, users.kim).status, 'discarded');
  await assert.rejects(docs.apply(p.id, users.admin), /discarded/);
});

test('minutes that exist already for that date: no proposal', async () => {
  const { docs } = setup();
  const r = await docs.run('meeting_doc_propose', { date: '2026-10-29', agenda: AGENDA }, users.kim);
  assert.match(r.error, /exist already/);
});

test('preview page: buttons only for those allowed; the paper shows the checklist', async () => {
  const { docs } = setup();
  const p = await docs.run('meeting_doc_propose', { agenda: AGENDA }, users.kim);
  const call = async user => {
    let out = null;
    await docs.route({ method: 'GET', path: `/api/meeting-docs/${p.id}/preview`, user, res: {}, sendHtml: (_r, html) => (out = html), json: () => {} });
    return out;
  };
  assert.match(await call(users.admin), /id="apply"/);
  assert.doesNotMatch(await call(users.kim), /id="apply"/);
  assert.match(await call(users.kim), /Solo un administrador puede aplicarla/);
  const paper = paperHtml({ date: '2026-11-05', agenda: cleanAgenda(AGENDA) });
  assert.match(paper, /Meeting-05\/11\/2026/);
  assert.match(paper, /<i>Tithorea<\/i> host plants/);
});

test('a fill that fails keeps the copy: the error and preview name it, and applying again finishes that same copy', async () => {
  const { docs, backend, db, dir } = setup();
  const fill = backend.fill;
  let fail = true;
  backend.fill = async (...a) => {
    if (fail) throw new Error('Docs 500');
    return fill(...a);
  };
  const p = await docs.run('meeting_doc_propose', { agenda: AGENDA }, users.kim);
  const r1 = await docs.run('meeting_doc_apply', { id: p.id }, users.admin);
  assert.match(r1.error, /was made but its Agenda could not be filled \(Docs 500\)/);
  const after = docs.get(p.id);
  assert.equal(after.status, 'pending');
  assert.ok(after.doc_id);
  let html = null;
  await docs.route({ method: 'GET', path: `/api/meeting-docs/${p.id}/preview`, user: users.admin, res: {}, sendHtml: (_r, h) => (html = h), json: () => {} });
  assert.match(html, /ya existe pero su Agenda no se llenó/);
  assert.match(html, /Último intento: .*Docs 500/);
  fail = false;
  const r2 = await docs.run('meeting_doc_apply', { id: p.id }, users.admin);
  assert.equal(r2.status, 'applied');
  assert.equal(docs.get(p.id).doc_id, after.doc_id);
  const copies = Object.values(JSON.parse(readFileSync(join(dir, 'files.json'), 'utf8'))).filter(f => f.name === '144 Meeting-05/11/2026');
  assert.equal(copies.length, 1);
  assert.equal(db.prepare('SELECT count(*) n FROM meeting_doc_proposals').get().n, 1);
});

test('after a failed fill: the number cannot change; a binned copy is not reused, the next apply makes a new one', async () => {
  const { docs, backend, dir } = setup();
  const fill = backend.fill;
  let fail = true;
  backend.fill = async (...a) => {
    if (fail) throw new Error('Docs 500');
    return fill(...a);
  };
  const p = await docs.run('meeting_doc_propose', { agenda: AGENDA }, users.kim);
  await docs.run('meeting_doc_apply', { id: p.id }, users.admin);
  assert.match((await docs.run('meeting_doc_propose', { agenda: AGENDA, number: 150 }, users.kim)).error, /number cannot change now/);
  assert.equal((await docs.run('meeting_doc_propose', { agenda: { ...AGENDA, other: ['Two dry shippers (Joana)'] } }, users.kim)).revision, 2);
  // Someone bins the copy.
  const files = JSON.parse(readFileSync(join(dir, 'files.json'), 'utf8'));
  const first = docs.get(p.id).doc_id;
  files[first].trashed = true;
  writeFileSync(join(dir, 'files.json'), JSON.stringify(files));
  fail = false;
  const done = await docs.run('meeting_doc_apply', { id: p.id }, users.admin);
  assert.equal(done.status, 'applied');
  assert.notEqual(docs.get(p.id).doc_id, first);
});

test('a «404» from a fill on a copy that is still there keeps the copy; it is finished on the next apply', async () => {
  const { docs, backend } = setup();
  const fill = backend.fill;
  let fail = true;
  backend.fill = async (...a) => {
    if (fail) throw new Error('Google 404: Requested entity was not found.');
    return fill(...a);
  };
  const p = await docs.run('meeting_doc_propose', { agenda: AGENDA }, users.kim);
  assert.match((await docs.run('meeting_doc_apply', { id: p.id }, users.admin)).error, /could not be filled .* finishes that copy/);
  const kept = docs.get(p.id).doc_id;
  assert.ok(kept);
  fail = false;
  assert.equal((await docs.run('meeting_doc_apply', { id: p.id }, users.admin)).status, 'applied');
  assert.equal(docs.get(p.id).doc_id, kept);
});

test('POST apply answers without the picture\'s server path', async () => {
  const { docs } = setup();
  const p = await docs.run('meeting_doc_propose', { agenda: AGENDA }, users.kim);
  let body = null;
  await docs.route({ method: 'POST', path: `/api/meeting-docs/${p.id}/apply`, user: users.admin, res: {}, sendHtml: () => {}, json: (_r, _s, v) => (body = v) });
  assert.equal(body.status, 'applied');
  assert.equal('picture' in body, false);
});

test('while a proposal is being applied no other one is made for that date; a restart puts it back to pending', async () => {
  const { docs, db, backend } = setup();
  const p = await docs.run('meeting_doc_propose', { agenda: AGENDA }, users.kim);
  db.prepare("UPDATE meeting_doc_proposals SET status = 'applying' WHERE id = ?").run(p.id);
  assert.match((await docs.run('meeting_doc_propose', { agenda: AGENDA }, users.kim)).error, /being created right now/);
  const again = createMeetingDocs({ db, backend, config: { templateId: 'tpl', folderId: 'meetings', today: () => '2026-11-04' } });
  assert.equal(again.get(p.id).status, 'pending');
  assert.match(again.get(p.id).error, /restart/);
});

test('picture: editors only; served to the preview; its server path is not in the JSON', async () => {
  const { docs } = setup();
  const p = await docs.run('meeting_doc_propose', { agenda: AGENDA }, users.kim);
  await docs.run('meeting_doc_apply', { id: p.id }, users.admin);
  assert.match((await docs.run('meeting_doc_picture', { id: p.id }, users.obs)).error, /Only editors/);
  let body = null;
  await docs.route({ method: 'GET', path: `/api/meeting-docs/${p.id}`, user: users.obs, res: {}, sendHtml: () => {}, json: (_r, _s, v) => (body = v) });
  assert.equal(body.status, 'applied');
  assert.equal('picture' in body, false);
  const res = { head: null, writeHead(s, h) { this.head = [s, h]; }, end(b) { this.body = b; } };
  await docs.route({ method: 'GET', path: `/api/meeting-docs/${p.id}/picture.png`, user: users.obs, res, sendHtml: () => {}, json: () => {} });
  assert.equal(res.head[0], 200);
  assert.equal(res.head[1]['content-type'], 'image/png');
  let html = null;
  await docs.route({ method: 'GET', path: `/api/meeting-docs/${p.id}/preview`, user: users.admin, res: {}, sendHtml: (_r, h) => (html = h), json: () => {} });
  assert.match(html, /picture\.png/);
});

// ------------------------------------------------------------ Docs batchUpdate on the template's layout
// A tiny Docs: the body as text from index 1, paragraphs split at newlines, bullets kept per paragraph.
function docFrom(paragraphs) {
  return { text: paragraphs.map(p => p.text + '\n').join(''), meta: paragraphs.map(p => ({ ...p })) };
}
function asApi(d) {
  const content = [];
  let i = 1;
  for (const line of d.text.split('\n').slice(0, -1)) {
    const m = d.meta?.[content.length];
    content.push({ startIndex: i, endIndex: i + line.length + 1, paragraph: { elements: [{ textRun: { content: line + '\n' } }], ...(m?.list ? { bullet: { listId: m.list, ...(m.level ? { nestingLevel: m.level } : {}) } } : {}) } });
    i += line.length + 1;
  }
  return { body: { content }, lists: { L1: { listProperties: { nestingLevels: [0, 1, 2].map(n => ({ indentStart: { magnitude: 36 * (n + 1) }, indentFirstLine: { magnitude: 36 * (n + 1) - 18 } })) } } } };
}
function applyRequests(d, requests) {
  let text = d.text;
  const at = idx => idx - 1;
  let bulleted = null;
  for (const r of requests) {
    if (r.deleteContentRange) {
      const { startIndex, endIndex } = r.deleteContentRange.range;
      assert.ok(endIndex <= text.length + 1, 'delete inside the body');
      text = text.slice(0, at(startIndex)) + text.slice(at(endIndex));
    } else if (r.insertText) {
      const i = at(r.insertText.location.index);
      text = text.slice(0, i) + r.insertText.text + text.slice(i);
    } else if (r.updateTextStyle) {
      const { startIndex, endIndex } = r.updateTextStyle.range;
      assert.ok(startIndex >= 1 && endIndex <= text.length + 1 && startIndex < endIndex, 'style range inside the body');
      if (r.updateTextStyle.textStyle.bold) (d.bold ??= []).push(text.slice(at(startIndex), at(endIndex)));
      if (r.updateTextStyle.textStyle.italic) (d.italic ??= []).push(text.slice(at(startIndex), at(endIndex)));
    } else if (r.createParagraphBullets) {
      const { startIndex, endIndex } = r.createParagraphBullets.range;
      const seg = text.slice(at(startIndex), at(endIndex));
      const lines = seg.split('\n');
      bulleted = lines.filter((_, k) => k < lines.length - 1 || seg.endsWith('\n') === false).map(l => ({ depth: /^\t*/.exec(l)[0].length, text: l.replace(/^\t+/, '') }));
      text = text.slice(0, at(startIndex)) + lines.map(l => l.replace(/^\t+/, '')).join('\n') + text.slice(at(endIndex));
    } else if (r.updateParagraphStyle) {
      const { startIndex, endIndex } = r.updateParagraphStyle.range;
      const para = text.slice(at(startIndex), at(endIndex));
      assert.ok(para.endsWith('\n') && !para.slice(0, -1).includes('\n'), 'one whole paragraph');
      (d.indents ??= []).push([para.slice(0, -1), r.updateParagraphStyle.paragraphStyle.indentStart.magnitude]);
    } else if (r.replaceAllText) {
      text = text.split(r.replaceAllText.containsText.text).join(r.replaceAllText.replaceText);
    }
  }
  return { text, bulleted, bold: d.bold ?? [], italic: d.italic ?? [], indents: d.indents ?? [] };
}

test('docsRequests: the template copy gets the title and the checklist, the rest untouched', () => {
  const tpl = docFrom([
    { text: 'Meeting-##/##/2026' },
    { text: '' },
    { text: 'Agenda' },
    { text: '' },
    { text: '', list: 'L1' },
    { text: 'Follow-ups:', list: 'L1' },
    { text: '', list: 'L1', level: 1 },
    { text: 'Other/various:', list: 'L1' },
    { text: '   ', list: 'L2', level: 1 },
    { text: 'Resumen de la reunión' },
    { text: ' ' },
    { text: 'Follow-ups:' },
    { text: '[Add items here]' },
    { text: 'Next week(s)' },
  ]);
  const agenda = cleanAgenda(AGENDA);
  const reqs = docsRequests(asApi(tpl), { date: '2026-11-05', agenda });
  assert.equal(reqs.at(-1).replaceAllText.replaceText, 'Meeting-05/11/2026');
  const out = applyRequests(tpl, reqs);
  const lines = out.text.split('\n');
  assert.deepEqual(lines.slice(0, 4), ['Meeting-05/11/2026', '', 'Agenda', '']); // the blank line under «Agenda» stays
  const agendaEnd = lines.indexOf('Resumen de la reunión');
  assert.deepEqual(lines.slice(4, agendaEnd), [
    'Infrastructure: water for the new insectary (Ángel and Kim)',
    'Aula 11 (Ángel)',
    'Lab name',
    "Lin's grants",
    'Tithorea host plants from Cavernas (Naty)',
    'Follow-ups:',
    'Sanger cluster access (Joana)',
    'Other/various:',
    '',
  ]);
  assert.deepEqual(lines.slice(agendaEnd), ['Resumen de la reunión', ' ', 'Follow-ups:', '[Add items here]', 'Next week(s)', '']);
  assert.deepEqual(out.bold, ['Follow-ups:', 'Other/various:']);
  assert.deepEqual(out.italic, ['Tithorea']);
  assert.deepEqual(out.bulleted.map(b => b.depth), [0, 0, 1, 1, 0, 0, 1, 0, 1]);
  assert.deepEqual(out.indents.map(x => x[1]), [36, 36, 72, 72, 36, 36, 72, 36, 72]); // the template's levels, not the replaced paragraph's
  assert.equal(out.indents[2][0], 'Lab name');
});

test('docsRequests refuses a document without the template headings', () => {
  assert.throws(() => docsRequests(asApi(docFrom([{ text: 'Something else' }])), { date: '2026-11-05', agenda: cleanAgenda(AGENDA) }), /template/);
});

test('whatsapp text without intro, with the document link', () => {
  const t = whatsappText({ date: '2026-11-05', agenda: cleanAgenda(AGENDA), intro: false, docUrl: 'https://docs.google.com/document/d/x/edit' });
  assert.ok(t.startsWith('*Meeting-05/11/2026*\n☐ Infrastructure'));
  assert.ok(t.endsWith('\nhttps://docs.google.com/document/d/x/edit'));
  assert.doesNotMatch(t, /Other\/various:\*\n\s+◦ \n/); // the empty point is not sent
});

test('the sim backend keeps its files private (owner only, 0600) and readable again', () => {
  // A server's usual umask (022) would leave a plain new file world-readable.
  const umask = process.umask(0o022);
  let dir;
  try {
    ({ dir } = setup());
  } finally {
    process.umask(umask);
  }
  assert.equal(statSync(join(dir, 'files.json')).mode & 0o777, 0o600, 'files.json readable by its owner only');
  const files = JSON.parse(readFileSync(join(dir, 'files.json'), 'utf8'));
  assert.equal(files.tpl.name, TEMPLATE_TITLE);
  // Opened again on the same folder: the same template, not a new one.
  assert.equal(simBackend({ dir, templateId: 'tpl' }).get('tpl').createdTime, files.tpl.createdTime);
});
