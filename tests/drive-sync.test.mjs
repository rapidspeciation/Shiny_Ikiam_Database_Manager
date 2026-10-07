import test from 'node:test';
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { chmodSync, existsSync, readFileSync, statSync, writeFileSync } from 'node:fs';
import { mkdtemp, rm, writeFile } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { crc32 } from 'node:zlib';
import { fileURLToPath } from 'node:url';
import { createKnowledge } from '../server/knowledge.mjs';
import { syncDrive, vttText, withoutCallLinks, xmlText } from '../scripts/drive-sync.mjs';

const script = fileURLToPath(new URL('../scripts/drive-sync.mjs', import.meta.url));
const config = JSON.parse(readFileSync(new URL('../deploy/drive-sync.json', import.meta.url), 'utf8'));
const folder = name => config.folders.find(f => f.name === name).id;
const ROOT = folder('Ithomiini_IKIAM');
const MEETINGS = folder('Meetings');
const PROTOCOLS = folder('Protocols');
const REPORTS = folder('Reports');
const RECORDINGS = 'folderRecordings0000000000000';
const ADMIN = 'folderAdmin00000000000000000000';
const BIBLIO = 'folderBiblio0000000000000000000';

/** A zip without compression (enough for unzip). */
function zip(files) {
  const parts = [],
    central = [];
  let offset = 0;
  for (const [name, text] of Object.entries(files)) {
    const data = Buffer.from(text);
    const nameBuf = Buffer.from(name);
    const crc = crc32(data);
    const local = Buffer.alloc(30);
    local.writeUInt32LE(0x04034b50, 0);
    local.writeUInt16LE(20, 4);
    local.writeUInt32LE(crc, 14);
    local.writeUInt32LE(data.length, 18);
    local.writeUInt32LE(data.length, 22);
    local.writeUInt16LE(nameBuf.length, 26);
    const head = Buffer.alloc(46);
    head.writeUInt32LE(0x02014b50, 0);
    head.writeUInt16LE(20, 4);
    head.writeUInt16LE(20, 6);
    head.writeUInt32LE(crc, 16);
    head.writeUInt32LE(data.length, 20);
    head.writeUInt32LE(data.length, 24);
    head.writeUInt16LE(nameBuf.length, 28);
    head.writeUInt32LE(offset, 42);
    parts.push(local, nameBuf, data);
    central.push(head, nameBuf);
    offset += local.length + nameBuf.length + data.length;
  }
  const dir = Buffer.concat(central);
  const end = Buffer.alloc(22);
  end.writeUInt32LE(0x06054b50, 0);
  end.writeUInt16LE(Object.keys(files).length, 8);
  end.writeUInt16LE(Object.keys(files).length, 10);
  end.writeUInt32LE(dir.length, 12);
  end.writeUInt32LE(offset, 16);
  return Buffer.concat([...parts, dir, end]);
}

const slide = text => `<p:sld><p:cSld><p:spTree><p:sp><p:txBody><a:p><a:r><a:t>${text}</a:t></a:r></a:p></p:txBody></p:sp></p:spTree></p:cSld></p:sld>`;
// Slide 2 is shown first: the order comes from presentation.xml.
const pptx = zip({
  'ppt/presentation.xml': '<p:presentation><p:sldIdLst><p:sldId id="256" r:id="rId3"/><p:sldId id="257" r:id="rId2"/></p:sldIdLst></p:presentation>',
  'ppt/_rels/presentation.xml.rels':
    '<Relationships><Relationship Id="rId2" Target="slides/slide1.xml"/><Relationship Id="rId3" Target="slides/slide2.xml"/></Relationships>',
  'ppt/slides/slide1.xml': slide('Cr&#237;a de Mechanitis &amp; Melinaea'),
  'ppt/slides/slide2.xml': slide('Portada del informe'),
  'ppt/slides/_rels/slide1.xml.rels': '<Relationships><Relationship Id="rId1" Target="../notesSlides/notesSlide1.xml"/></Relationships>',
  'ppt/notesSlides/notesSlide1.xml': '<p:notes><a:p><a:r><a:t>Decir los números de 2025</a:t></a:r></a:p><a:p><a:r><a:t>1</a:t></a:r></a:p></p:notes>',
});
const docx = zip({
  'word/document.xml':
    '<w:document><w:body><w:p><w:pPr><w:pStyle w:val="Heading1"/></w:pPr><w:r><w:t>Limpieza</w:t></w:r></w:p><w:p><w:r><w:t xml:space="preserve">Lavar las jaulas </w:t></w:r><w:r><w:t>cada semana.</w:t></w:r></w:p></w:body></w:document>',
});

const file = (id, name, mimeType, extra = {}) => ({
  id,
  name,
  mimeType,
  modifiedTime: '2026-09-01T10:00:00.000Z',
  webViewLink: `https://docs.google.com/x/d/${id}/edit`,
  ...extra,
});
const FOLDER = 'application/vnd.google-apps.folder';
const ids = {
  meeting: 'docMeeting137aaaaaaaaaaaaaaaaaa',
  meetingOld: 'docMeeting012aaaaaaaaaaaaaaaaaa',
  vtt: 'vttRecording0aaaaaaaaaaaaaaaaaa',
  slides: 'slidesRootaaaaaaaaaaaaaaaaaaaaa',
  slidesBig: 'slidesBigaaaaaaaaaaaaaaaaaaaaaa',
  bigPptx: 'pptxHugeaaaaaaaaaaaaaaaaaaaaaaa',
  pptx: 'pptxReportaaaaaaaaaaaaaaaaaaaaa',
  pdf: 'pdfReportaaaaaaaaaaaaaaaaaaaaaa',
  docx: 'docxProtocolaaaaaaaaaaaaaaaaaaa',
  secret: 'docSecretaaaaaaaaaaaaaaaaaaaaaa',
  sheet: 'sheetaaaaaaaaaaaaaaaaaaaaaaaaaa',
};

function fixture() {
  return {
    folders: {
      [ROOT]: [
        file(ADMIN, 'Admin', FOLDER),
        file(BIBLIO, 'Useful bibliography', FOLDER),
        file(MEETINGS, 'Meetings', FOLDER),
        file(ids.slides, 'Presentación general', 'application/vnd.google-apps.presentation', { size: '21870102' }),
        file(ids.slidesBig, 'Charla con fotos', 'application/vnd.google-apps.presentation', { size: '91870102' }),
        file(ids.bigPptx, 'Charla larga.pptx', 'application/vnd.openxmlformats-officedocument.presentationml.presentation', { size: '48838415' }),
      ],
      [MEETINGS]: [
        file(ids.meeting, '137 Meeting-10/09/2026', 'application/vnd.google-apps.document'),
        file(ids.meetingOld, '12 Meeting-05/03/2025', 'application/vnd.google-apps.document'),
        file(RECORDINGS, 'Recordings from meetings', FOLDER),
        file(ids.sheet, 'Asistencia', 'application/vnd.google-apps.spreadsheet'),
      ],
      [RECORDINGS]: [file(ids.vtt, 'Meeting 137 transcript.vtt', 'text/vtt', { size: '300' })],
      [PROTOCOLS]: [
        file(ids.docx, 'Protocolo limpieza.docx', 'application/vnd.openxmlformats-officedocument.wordprocessingml.document', { size: String(docx.length) }),
        file(ids.secret, 'InfoAccess cuentas', 'application/vnd.google-apps.document'),
      ],
      [REPORTS]: [
        file(ids.pptx, 'Informe 2025.pptx', 'application/vnd.openxmlformats-officedocument.presentationml.presentation', { size: String(pptx.length) }),
        file(ids.pdf, 'Informe final.pdf', 'application/pdf', { size: '5000' }),
      ],
      [ADMIN]: [file('adminDocaaaaaaaaaaaaaaaaaaaaaaaa', 'Contrato', 'application/vnd.google-apps.document')],
      [BIBLIO]: [file('biblioDocaaaaaaaaaaaaaaaaaaaaaa', 'Paper', 'application/pdf')],
    },
    content: {
      [ids.meeting]:
        '# 137 Meeting-10/09/2026\n\nAcordamos revisar las larvas de Mechanitis cada mañana. ![][image1]\n\nEnlace: https://meet.google.com/abc-defg-hij\n\n[image1]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUg==>\n',
      [ids.meetingOld]: 'Reunión antigua sobre huevos.',
      [ids.vtt]:
        'WEBVTT\n\n1\n00:00:01.000 --> 00:00:03.000\n<v Ana Pérez>Buenos días a todos\n\n2\n00:00:03.000 --> 00:00:05.000\n<v Ana Pérez>empezamos con las crías\n\n3\n00:00:05.000 --> 00:00:07.000\n<v Luis>Hay 40 pupas\n',
    },
    binary: { [ids.pptx]: pptx, [ids.docx]: docx, [ids.slides]: pptx },
    tooLarge: [ids.slidesBig],
    decks: {
      [ids.slidesBig]: [
        { objectId: 'p1', number: 1, textElements: [{ text: 'Fotos de campo' }], notes: 'Hablar del transecto T2', tables: [] },
        { objectId: 'p2', number: 2, textElements: [], notes: '', tables: [{ rows: [{ cells: [{ text: 'Especie' }, { text: 'N' }] }] }] },
      ],
    },
  };
}

// The stub gog, in process: lists folders and "downloads" files from the fixture, and logs every call.
function stubGog(fx, calls) {
  return async (bin, args) => {
    calls.push(args);
    const at = name => args[args.indexOf(name) + 1];
    const i = args.indexOf('drive');
    if (i >= 0 && args[i + 1] === 'ls') {
      if ((fx.fail ?? []).includes(at('--parent'))) throw new Error('Google API error 500');
      return JSON.stringify({ files: fx.folders[at('--parent')] ?? [] });
    }
    if (i >= 0 && args[i + 1] === 'download') {
      const id = args[i + 2];
      if ((fx.tooLarge ?? []).includes(id)) throw new Error('Google API error (403 exportSizeLimitExceeded): This file is too large to be exported.');
      if (fx.binary[id]) writeFileSync(at('--out'), fx.binary[id]);
      else if (fx.content[id] !== undefined) writeFileSync(at('--out'), fx.content[id]);
      else throw new Error('not found');
      return JSON.stringify({ path: at('--out') });
    }
    if (args.includes('list-slides'))
      return JSON.stringify({ slides: fx.decks[at('list-slides')].map(({ objectId, number }) => ({ objectId, number, isSkipped: false })) });
    if (args.includes('read-slide')) return JSON.stringify(fx.decks[at('read-slide')].find(s => s.objectId === args[args.indexOf('read-slide') + 2]));
    throw new Error(`unexpected gog call ${args.join(' ')}`);
  };
}

async function setup() {
  const dir = await mkdtemp(join(tmpdir(), 'ithomiini-drive-sync-'));
  const fx = fixture();
  const env = {
    KNOWLEDGE_DIR: join(dir, 'knowledge'),
    ITHOMIINI_GOG_BIN: 'gog',
    GOG_ACCOUNT: 'project@example.test',
    ITHOMIINI_PDFTOTEXT: join(dir, 'no-pdftotext'),
  };
  /** One run; its gog calls are in .calls. */
  const sync = async () => {
    const calls = [];
    const summary = await syncDrive({ env, exec: stubGog(fx, calls) });
    return { ...summary, calls };
  };
  const drive = join(env.KNOWLEDGE_DIR, 'drive');
  const manifest = () => JSON.parse(readFileSync(join(drive, 'manifest.json'), 'utf8'));
  return { dir, fx, sync, drive, manifest };
}
const downloads = calls => calls.filter(c => c.includes('download')).map(c => c[c.indexOf('download') + 1]);
const listed = calls => calls.filter(c => c.includes('ls')).map(c => c[c.indexOf('--parent') + 1]);
test('office and transcript text extraction', () => {
  assert.equal(xmlText('<a:p><a:r><a:t>Hola &amp; adiós</a:t></a:r><a:br/><a:r><a:t>fin</a:t></a:r></a:p>'), 'Hola & adiós\nfin');
  assert.equal(withoutCallLinks('Enlace: https://meet.google.com/abc-defg-hij.'), 'Enlace: [enlace de videollamada omitido].');
  assert.equal(vttText('WEBVTT\n\n00:00.000 --> 00:01.000\nLuis: uno\n\n00:01.000 --> 00:02.000\nLuis: dos'), 'Luis: uno dos');
});

test('drive sync exports the included folders read-only, never the excluded ones, and only changes after that', async () => {
  const { dir, fx, sync, drive, manifest } = await setup();
  try {
    const first = await sync();
    assert.equal(first.total, 9);
    // Every gog call is read-only, with the account and client.
    for (const call of first.calls) {
      assert.equal(call[0], '--readonly');
      assert.deepEqual(call.slice(1, 5), ['--account', 'project@example.test', '--client', 'ithomiini']);
    }
    // Excluded folders and names, other root folders and Sheets are never opened.
    const opened = listed(first.calls);
    assert.ok(opened.includes(RECORDINGS));
    assert.ok(!opened.includes(ADMIN) && !opened.includes(BIBLIO));
    const got = downloads(first.calls);
    assert.deepEqual(new Set(got), new Set([ids.meeting, ids.meetingOld, ids.slides, ids.slidesBig, ids.vtt, ids.docx, ids.pptx]));
    assert.ok(!got.includes(ids.secret) && !got.includes(ids.sheet) && !got.includes(ids.bigPptx) && !got.includes(ids.pdf));
    assert.ok(first.calls.some(c => c.includes(ids.meeting) && c.includes('--format') && c.includes('md')));
    assert.ok(first.calls.some(c => c.includes(ids.slides) && c.includes('pptx')));
    // A deck Drive will not export (images over 10 MB) is read slide by slide.
    assert.equal(first.calls.filter(c => c.includes('read-slide')).length, 2);
    const big = readFileSync(join(drive, `${ids.slidesBig}.md`), 'utf8');
    assert.match(big, /## Diapositiva 1\n\nFotos de campo\n\nNotas: Hablar del transecto T2\n\n## Diapositiva 2\n\nEspecie\nN\n/);
    assert.match(readFileSync(join(drive, `${ids.slides}.md`), 'utf8'), /## Diapositiva 2\n\nCría de Mechanitis/);

    const files = manifest().files;
    assert.equal(files[ids.pdf].status, 'sin texto');
    assert.equal(files[ids.bigPptx].status, 'muy grande');
    assert.equal(files[ids.meeting].kind, 'meeting');
    assert.equal(files[ids.vtt].kind, 'transcript');
    assert.equal(files[ids.vtt].folder, 'Meetings/Recordings from meetings');
    assert.equal(files[ids.docx].kind, 'protocol');
    assert.equal(files[ids.pptx].kind, 'report');
    assert.equal(files[ids.slides].kind, 'presentation');
    assert.equal(files[ids.slidesBig].status, 'ok');
    assert.equal(files[ids.secret], undefined);
    assert.equal(statSync(join(drive, `${ids.meeting}.md`)).mode & 0o777, 0o600);

    const meeting = readFileSync(join(drive, `${ids.meeting}.md`), 'utf8');
    assert.match(meeting, /^---\ntitle: 137 Meeting-10\/09\/2026\n/);
    assert.match(meeting, /\ndate: 2026-09-10\n/);
    assert.match(meeting, /\nkind: meeting\n/);
    assert.match(meeting, new RegExp(`\\ndriveId: ${ids.meeting}\\n`));
    assert.equal(meeting.match(/^# 137 Meeting/gm).length, 1);
    // Embedded images and video-call links are left out.
    assert.doesNotMatch(meeting, /base64|image1|meet\.google/);
    assert.match(meeting, /Enlace: \[enlace de videollamada omitido\]/);
    const deck = readFileSync(join(drive, `${ids.pptx}.md`), 'utf8');
    assert.match(deck, /## Diapositiva 1\n\nPortada del informe\n\n## Diapositiva 2\n\nCría de Mechanitis & Melinaea\n\nNotas: Decir los números de 2025\n/);
    assert.match(readFileSync(join(drive, `${ids.docx}.md`), 'utf8'), /## Limpieza\nLavar las jaulas cada semana\./);
    assert.match(readFileSync(join(drive, `${ids.vtt}.md`), 'utf8'), /Ana Pérez: Buenos días a todos empezamos con las crías\n\nLuis: Hay 40 pupas/);

    // The assistant finds them.
    const knowledge = createKnowledge({ knowledgeRoots: [join(dir, 'knowledge')] });
    const [hit] = await knowledge.search({ query: 'larvas mechanitis manana', kind: 'meeting' });
    assert.equal(hit.id, ids.meeting);
    assert.equal(hit.date, '2026-09-10');

    // Second run: one Doc edited, one removed (trashed), one moved into an excluded name.
    fx.folders[MEETINGS][0].modifiedTime = '2026-09-12T08:00:00.000Z';
    fx.content[ids.meeting] = 'Texto corregido.';
    fx.folders[MEETINGS].splice(1, 1);
    fx.folders[REPORTS][0].trashed = true;
    const second = await sync();
    assert.deepEqual(downloads(second.calls), [ids.meeting]);
    assert.match(readFileSync(join(drive, `${ids.meeting}.md`), 'utf8'), /Texto corregido/);
    assert.ok(!existsSync(join(drive, `${ids.meetingOld}.md`)));
    assert.ok(!existsSync(join(drive, `${ids.pptx}.md`)));
    assert.equal(manifest().files[ids.meetingOld], undefined);
    assert.equal(second.deleted, 2);

    // Third run: nothing changed, nothing downloaded.
    assert.deepEqual(downloads((await sync()).calls), []);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
});

test('a failed listing stops the sync without deleting anything; a running sync blocks another', async () => {
  const { dir, fx, sync, drive } = await setup();
  try {
    await sync();
    fx.fail = [REPORTS];
    await assert.rejects(sync(), /Google API error 500/);
    assert.ok(existsSync(join(drive, `${ids.pptx}.md`)));
    assert.ok(!existsSync(join(drive, '.sync.lock')));

    // A lock held by a live process.
    fx.fail = [];
    await writeFile(join(drive, '.sync.lock'), `${process.pid} now\n`);
    await assert.rejects(sync(), e => e.locked === true);
    // A stale lock (process gone) is taken over.
    await writeFile(join(drive, '.sync.lock'), '999999999 old\n');
    await sync();
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
});

test('a failed export is reported and retried next run', async () => {
  const { dir, fx, sync, manifest } = await setup();
  try {
    delete fx.binary[ids.slides];
    assert.equal((await sync()).failed, 1);
    assert.equal(manifest().files[ids.slides].status, 'error');
    fx.binary[ids.slides] = pptx;
    const again = await sync();
    assert.equal(again.failed, 0);
    assert.deepEqual(downloads(again.calls), [ids.slides]);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
});

// The command itself (what the timer runs), with a gog program: its summary, a failed file's exit
// code, and a held lock's.
test('the drive-sync command prints its summary and exits 1 on a failed file, 75 while another sync runs', async () => {
  const dir = await mkdtemp(join(tmpdir(), 'ithomiini-drive-sync-cli-'));
  try {
    const doc = 'application/vnd.google-apps.document';
    const listing = { [MEETINGS]: [file(ids.meeting, '137 Meeting-10/09/2026', doc), file(ids.meetingOld, '12 Meeting-05/03/2025', doc)] };
    writeFileSync(join(dir, 'config.json'), JSON.stringify({ folders: [{ id: MEETINGS, name: 'Meetings', kind: 'meeting' }] }));
    writeFileSync(
      join(dir, 'gog'),
      `#!${process.execPath}
const { appendFileSync, writeFileSync } = require('node:fs');
const args = process.argv.slice(2);
appendFileSync(${JSON.stringify(join(dir, 'calls.log'))}, JSON.stringify(args) + '\\n');
const at = name => args[args.indexOf(name) + 1];
if (args.includes('ls')) process.stdout.write(JSON.stringify({ files: (${JSON.stringify(listing)})[at('--parent')] ?? [] }));
else if (at('download') === ${JSON.stringify(ids.meeting)}) writeFileSync(at('--out'), 'Acta');
else { console.error('not found'); process.exit(4); }
`,
    );
    chmodSync(join(dir, 'gog'), 0o755);
    const env = {
      PATH: process.env.PATH,
      HOME: dir,
      KNOWLEDGE_DIR: join(dir, 'knowledge'),
      ITHOMIINI_GOG_BIN: join(dir, 'gog'),
      ITHOMIINI_DRIVE_SYNC_CONFIG: join(dir, 'config.json'),
    };
    const run = () => spawnSync(process.execPath, [script], { env, encoding: 'utf8' });
    const out = run();
    assert.equal(out.status, 1, out.stderr);
    assert.match(out.stdout, /Drive sync: 2 documents .*exported 1, .*failed 1/);
    assert.match(out.stderr, /1 file\(s\) failed/);
    const calls = readFileSync(join(dir, 'calls.log'), 'utf8').trim().split('\n').map(line => JSON.parse(line));
    assert.ok(calls.every(c => c[0] === '--readonly'));
    assert.match(readFileSync(join(dir, 'knowledge', 'drive', `${ids.meeting}.md`), 'utf8'), /Acta/);

    writeFileSync(join(dir, 'knowledge', 'drive', '.sync.lock'), `${process.pid} now\n`);
    const blocked = run();
    assert.equal(blocked.status, 75);
    assert.match(blocked.stderr, /another drive sync is running/);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
});
