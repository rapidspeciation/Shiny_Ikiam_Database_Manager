// Photos uploaded to Drive without being renamed (the camera's names), read on this server: each
// photo is downloaded, its envelope's CAM ID read by the Rename Photos app's reader (the same models,
// photo-reader/ beside the server: scripts/build-rename-app.sh), a small copy of it and of its envelope
// kept, and the photo deleted at once, so the server's disk holds a few photos at a time. The batch is
// then put in order the way the «Fotos» tab does (server/rename-photos.mjs checkBatch): its IDs in the
// order the photos were taken, two photos each (d then v), one turn per shooting session; what does not
// add up is marked for a look, and only that is shown to the assistant. Applied from its card, it
// becomes a plan of changes on Drive (server/photo-files.mjs: renames with their RAW files, turns,
// removals), made with the card of that plan, after which Photo_links lists the photos.
//
// A batch is a card of Cambios propuestos (server/assistant.mjs, `sync_json` of type 'photos'); its rows
// are in drive_photo_rows, its pictures under the cache folder (DRIVE_PHOTOS_CACHE).

import { spawn } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { existsSync, mkdirSync, readFileSync, readdirSync, rmSync, statSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createInterface } from 'node:readline';
import { FOLDER, driveFor, driveTree, driveWriterFor } from './drive.mjs';
import { driveIdOf, knownId, photoLinkRows, specimenName, specimens } from './photo-links.mjs';
import { IMAGES, checkBatch } from './rename-photos.mjs';

export const DRIVE_PHOTO_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'drive_photos',
      description: [
        "Photos uploaded to a Drive folder without being renamed (the camera's names, IMG_…): the server reads the CAM ID on each envelope with the Rename Photos reader, keeps a small copy, and puts the batch in order (two photos per ID, d then v, in the order taken; one turn per shooting session). It shows as a card beside the chat, where the person checks it and presses «Aplicar».",
        '- `folder` (a Drive link, or a folder of Photo_links by its path): starts reading it (about a second a photo; answers at once with the batch). `all: true`: the photos already named too.',
        '- `batch`: how it goes, then the IDs in the order taken (`order`), the counts, the checks (IDs without exactly two photos, IDs out of the order taken, IDs the workbook lacks, IDs with photos listed already, turns that differ from their session) and the rows to look at (`toLook`). `rows: "all"`: every row.',
        'Look only at the rows marked (drive_photos_look), correct the clear ones (drive_photos_edit), and tell the person what is left for them and which IDs need photos taken again.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: { folder: { type: 'string' }, all: { type: 'boolean' }, batch: { type: 'string' }, rows: { type: 'string', enum: ['marked', 'all'] } },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'drive_photos_look',
      description:
        "Pictures of photos of a batch of drive_photos, as the server keeps them: `what: \"envelope\"` (default: the envelope, to read its ID) or `\"photo\"` (the whole photo, small: which side, wings, turn). `files`: the rows' `file` (up to 12).",
      parameters: {
        type: 'object',
        properties: { batch: { type: 'string' }, files: { type: 'array', items: { type: 'string' } }, what: { type: 'string', enum: ['envelope', 'photo'] } },
        required: ['batch', 'files'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'drive_photos_edit',
      description:
        "Corrects rows of a batch of drive_photos (the card shows each change as the assistant's): `changes` [{ file, id (the CAM ID), view (d, v, d2…), remove (true: a photo not wanted, removed when applied), rotation (0, 90, 180, 270 clockwise, from the photo as shown), note (what a person should know) }]. Views are worked out again for the IDs changed unless given.",
      parameters: {
        type: 'object',
        properties: {
          batch: { type: 'string' },
          changes: {
            type: 'array',
            items: {
              type: 'object',
              properties: {
                file: { type: 'string' },
                id: { type: 'string' },
                view: { type: 'string' },
                remove: { type: 'boolean' },
                rotation: { type: 'number' },
                note: { type: 'string' },
              },
              required: ['file'],
            },
          },
        },
        required: ['batch', 'changes'],
      },
    },
  },
];
export const DRIVE_PHOTO_TOOL_NAMES = new Set(DRIVE_PHOTO_TOOLS.map(t => t.function.name));

const EDITORS = ['editor', 'reviewer', 'admin'];
const PHOTO = /\.(jpe?g)$/i;
const RAW = /\.(cr2|cr3|orf|nef|arw|dng|raf|rw2|pef)$/i;
/** Below this the reader's ID is a guess to look at. */
const SURE = 0.9;
/** Photos taken further apart than this are of another session (one turn each). */
const SESSION_MS = 30 * 60_000;
const MAX_PHOTOS = 3000;
const DOWNLOADS = 2;
/** The pictures kept, in all: the oldest batches' go first. */
const CACHE_BYTES = 1024 ** 3;
const KEPT_DAYS = 14;
const LOOK_MAX = 12;
/** How long a call on a batch being read waits for it to end before answering how far it is. */
const WAIT_MS = 90_000;
const PAGE = 150;

const text = v => String(v ?? '').trim();
const filled = object =>
  Object.fromEntries(Object.entries(object).filter(([, v]) => !(v === 0 || v === null || v === undefined || v === false || v === '' || (typeof v === 'object' && !Object.keys(v).length))));
const stem = name => String(name).replace(/\.[^.]+$/, '').toLowerCase();
const norm = d => (((Math.round(Number(d) / 90) * 90) % 360) + 360) % 360;
/** The camera's capture time ("2026:07:20 10:15:30") as an ISO moment (read as UTC, as Photo_links does). */
const takenOf = item => {
  const m = /(\d{4}):(\d{2}):(\d{2}) (\d{2}):(\d{2}):(\d{2})/.exec(String(item.imageMediaMetadata?.time ?? ''));
  return m ? `${m[1]}-${m[2]}-${m[3]}T${m[4]}:${m[5]}:${m[6]}Z` : item.createdTime || null;
};
const byNumber = new Intl.Collator('en', { numeric: true }).compare;

/** The views of an ID's photos in the order taken: d, v, d2, v2… */
export const viewsFor = count => Array.from({ length: count }, (_, i) => `${i % 2 ? 'v' : 'd'}${i >= 2 ? Math.floor(i / 2) + 1 : ''}`);

/**
 * A batch's rows put in order (they come in the order taken): each ID's views, the turn of each
 * session (the most common among its photos read), and what to look at (`review`). `known`: the
 * workbook's IDs; `listed`: the IDs whose photos Photo_links lists. Rows a person or the assistant
 * set keep their view and turn.
 */
export function arrange(rows, { known = null, listed = null } = {}) {
  const live = rows.filter(r => !r.remove);
  // Views, by ID in the order taken.
  const byId = new Map();
  for (const r of live) if (r.camid) (byId.get(r.camid) ?? byId.set(r.camid, []).get(r.camid)).push(r);
  for (const list of byId.values()) {
    const auto = list.filter(r => !r.viewSet);
    const used = new Set(list.filter(r => r.viewSet).map(r => r.view));
    const free = viewsFor(list.length + used.size).filter(v => !used.has(v));
    auto.forEach((r, i) => (r.view = free[i]));
  }
  for (const r of live) if (!r.camid && !r.viewSet) r.view = '';
  // One turn per session: the most common turn the reader found among its photos.
  const sessions = [];
  let last = NaN;
  for (const r of live) {
    const t = r.taken ? Date.parse(r.taken) : NaN;
    if (!sessions.length || t - last >= SESSION_MS) sessions.push([]);
    sessions.at(-1).push(r);
    if (!Number.isNaN(t)) last = t;
  }
  for (const list of sessions) {
    const votes = new Map();
    for (const r of list) if (r.readRotation !== null && r.readRotation !== undefined) votes.set(r.readRotation, (votes.get(r.readRotation) ?? 0) + 1);
    const [common] = [...votes].sort((a, b) => b[1] - a[1])[0] ?? [0];
    for (const r of list) {
      if (!r.rotationSet) r.rotation = common;
      r.turnedApart = r.readRotation !== null && r.readRotation !== undefined && r.readRotation !== r.rotation;
    }
  }
  // What to look at.
  const shaped = live.map((r, i) => ({
    file: r.file,
    n: i + 1,
    id: r.camid ?? '',
    view: r.view,
    taken: r.taken,
    review: [
      ...(!r.camid ? ['no ID read'] : []),
      ...(r.camid && r.setBy === 'reader' && (r.conf ?? 0) < SURE ? ['unsure reading'] : []),
      ...(r.turnedApart ? ['rotation-outlier'] : []),
    ],
    skip: false,
    status: '',
    byHost: [],
  }));
  const checked = checkBatch(shaped, { known, listed });
  const marked = new Set(checked.flagged.map(r => r.file));
  for (const r of rows) {
    const s = shaped.find(x => x.file === r.file);
    r.review = s ? [...s.review, ...(marked.has(r.file) && !s.review.length ? ['checks'] : [])] : [];
    r.marked = !!s && marked.has(r.file);
  }
  return { order: checked.order, counts: checked.counts, checks: checked.checks };
}

/** The name a row's photo takes: <ID><view>.JPG (its RAW file the same, with its own ending). */
export const nameFor = (r, ext = '.JPG') => (r.camid && r.view ? `${r.camid}${r.view}${ext}` : null);

/**
 * Drive photo batches for one app. `organize(operations, user)`: a plan of changes to the files made
 * and started (server/photo-links-tool.mjs), answering { plan, link } or { error }. `show(batch)`,
 * `tell(batch)`: its card made, and changed. `readerDir`, `cacheDir`, `reader` (a fake for tests).
 */
export function createDrivePhotos({ store, config = {}, readerDir = null, cacheDir = null, organize = null, show = null, tell = null, reader = null, waitMs = WAIT_MS }) {
  const db = store.db;
  db.exec(`CREATE TABLE IF NOT EXISTS drive_photo_batches (id TEXT PRIMARY KEY, owner TEXT NOT NULL, folder_id TEXT NOT NULL, folder TEXT NOT NULL,
    status TEXT NOT NULL, created_at TEXT NOT NULL, updated_at TEXT NOT NULL, summary_json TEXT NOT NULL DEFAULT '{}', plan TEXT, error TEXT)`);
  db.exec(`CREATE TABLE IF NOT EXISTS drive_photo_rows (batch_id TEXT NOT NULL, file TEXT NOT NULL, n INTEGER NOT NULL, row_json TEXT NOT NULL,
    PRIMARY KEY (batch_id, file))`);
  const cache = cacheDir ?? config.drivePhotosCache ?? join(tmpdir(), 'ithomiini-drive-photos');
  mkdirSync(cache, { recursive: true, mode: 0o700 });
  const now = () => new Date().toISOString();

  const batchOf = id => db.prepare('SELECT * FROM drive_photo_batches WHERE id = ?').get(String(id ?? ''));
  const rowsOf = id =>
    db
      .prepare('SELECT row_json FROM drive_photo_rows WHERE batch_id = ? ORDER BY n')
      .all(id)
      .map(r => JSON.parse(r.row_json));
  const saveRow = (id, r) =>
    db.prepare('INSERT OR REPLACE INTO drive_photo_rows (batch_id, file, n, row_json) VALUES (?,?,?,?)').run(id, r.file, r.n, JSON.stringify(r));
  const setBatch = (id, values) => {
    const keys = Object.keys(values);
    db.prepare(`UPDATE drive_photo_batches SET ${keys.map(k => `${k} = ?`).join(', ')}, updated_at = ? WHERE id = ?`).run(...keys.map(k => values[k]), now(), id);
  };
  const picture = (id, file, what) => join(cache, id, `${file}-${what === 'photo' ? 'p' : 'c'}.jpg`);

  const lookups = () => {
    const known = new Set(specimens(store).keys());
    const listed = new Set();
    for (const r of photoLinkRows(store)) {
      const parsed = specimenName(r.values.Name);
      const id = parsed ? knownId(parsed.id, known) : null;
      if (id) listed.add(id);
    }
    return { known, listed };
  };

  /** The batch put in order again and its summary saved; the card told. */
  function settle(id) {
    const rows = rowsOf(id);
    const { known, listed } = lookups();
    const done = rows.filter(r => r.read);
    const arranged = arrange(done, { known, listed });
    for (const r of done) saveRow(id, r);
    const batch = batchOf(id);
    const summary = {
      ...JSON.parse(batch.summary_json || '{}'),
      photos: rows.length,
      read: done.length,
      ids: new Set(done.filter(r => r.camid && !r.remove).map(r => r.camid)).size,
      marked: done.filter(r => r.marked).length,
      toRemove: done.filter(r => r.remove).length,
      turned: done.filter(r => !r.remove && r.rotation).length,
      ...arranged,
    };
    setBatch(id, { summary_json: JSON.stringify(summary) });
    tell?.(batchOf(id));
    return summary;
  }

  // ------------------------------------------------------------ reading
  let running = null;
  let runningId = null;
  const queue = [];
  function startReader(knownFile) {
    if (reader) return reader;
    const dir = readerDir ?? config.photoReaderDir;
    if (!dir || !existsSync(join(dir, 'photo-reader.mjs'))) throw new Error('This server has no photo reader installed (photo-reader/: scripts/build-rename-app.sh).');
    const child = spawn(process.execPath, [join(dir, 'photo-reader.mjs'), '--models', join(dir, 'models'), '--known', knownFile, '--threads', '2'], {
      stdio: ['pipe', 'pipe', 'pipe'],
    });
    const waiting = new Map();
    let ready;
    const isReady = new Promise((resolve, reject) => {
      ready = resolve;
      child.once('error', reject);
    });
    child.stderr.on('data', d => console.error('Photo reader:', String(d).slice(0, 300)));
    child.on('exit', code => {
      for (const [, w] of waiting) w.reject(new Error(`the photo reader stopped (${code})`));
      waiting.clear();
    });
    createInterface({ input: child.stdout }).on('line', line => {
      let out;
      try {
        out = JSON.parse(line);
      } catch {
        return;
      }
      if (out.ready) return ready();
      waiting.get(out.id)?.resolve(out);
      waiting.delete(out.id);
    });
    return {
      async read(job) {
        await isReady;
        return new Promise((resolve, reject) => {
          waiting.set(job.id, { resolve, reject });
          child.stdin.write(`${JSON.stringify(job)}\n`);
        });
      },
      close: () => child.stdin.end(),
    };
  }

  async function readBatch(id) {
    const batch = batchOf(id);
    const writer = driveWriterFor(store, config);
    if (!writer?.download) throw new Error('This app has no Google account set up that can read the photos (GOOGLE_DRIVE_CREDENTIALS_FILE).');
    const work = join(tmpdir(), `ithomiini-drive-photos-${id}`);
    mkdirSync(work, { recursive: true, mode: 0o700 });
    mkdirSync(join(cache, id), { recursive: true, mode: 0o700 });
    const knownFile = join(work, 'known.json');
    writeFileSync(knownFile, JSON.stringify([...lookups().known]));
    const read = startReader(knownFile);
    try {
      const todo = rowsOf(id).filter(r => !r.read);
      let done = rowsOf(id).length - todo.length;
      let next = 0;
      // A few photos downloaded ahead of the reader; each deleted once read.
      const one = async () => {
        while (next < todo.length) {
          const r = todo[next++];
          const path = join(work, `${r.file}.jpg`);
          try {
            await writer.download(r.file, path);
            const out = await read.read({ id: r.file, path, preview: picture(id, r.file, 'photo'), crop: picture(id, r.file, 'envelope') });
            if (out.error) throw new Error(out.error);
            Object.assign(r, {
              read: true,
              // An ID the workbook lacks is kept, and marked (a butterfly not typed yet, or a misreading).
              camid: out.camid ?? null,
              readAs: out.camid ?? null,
              conf: out.conf,
              candidates: (out.candidates ?? []).map(c => c.id),
              readRotation: out.rotation === null || out.rotation === undefined ? null : norm(out.rotation),
              orientation: out.orientation,
              setBy: 'reader',
            });
          } catch (e) {
            Object.assign(r, { read: true, camid: null, error: String(e.message || e).slice(0, 200), setBy: 'reader', readRotation: null });
          } finally {
            rmSync(path, { force: true });
          }
          saveRow(id, r);
          done++;
          if (done % 5 === 0 || done === todo.length) {
            setBatch(id, { summary_json: JSON.stringify({ ...JSON.parse(batchOf(id).summary_json || '{}'), photos: done + (todo.length - next), read: done }) });
            tell?.(batchOf(id));
          }
        }
      };
      await Promise.all(Array.from({ length: DOWNLOADS }, one));
    } finally {
      read.close?.();
      rmSync(work, { recursive: true, force: true });
    }
    settle(id);
    setBatch(id, { status: 'ready' });
    tell?.(batchOf(id));
    prune();
    return batch;
  }
  /** One batch read at a time (each takes two of the server's cores); the others wait their turn. */
  function pump() {
    if (running || !queue.length) return;
    const id = queue.shift();
    runningId = id;
    running = readBatch(id)
      .catch(e => {
        setBatch(id, { status: 'failed', error: String(e.message || e).slice(0, 300) });
        tell?.(batchOf(id));
      })
      .finally(() => {
        running = null;
        runningId = null;
        pump();
      });
  }
  // A restart in the middle of a reading: it goes on where it was.
  for (const b of db.prepare("SELECT id FROM drive_photo_batches WHERE status = 'reading' ORDER BY created_at").all()) queue.push(b.id);
  setImmediate(pump);

  /** The pictures of batches done with or old, and the oldest beyond the space allowed. */
  function prune() {
    const batches = db.prepare('SELECT id, status, updated_at FROM drive_photo_batches ORDER BY updated_at').all();
    const old = Date.now() - KEPT_DAYS * 86_400_000;
    let total = 0;
    const sizes = new Map();
    for (const b of batches) {
      const dir = join(cache, b.id);
      if (!existsSync(dir)) continue;
      let bytes = 0;
      for (const f of readdirSync(dir)) bytes += statSync(join(dir, f)).size;
      sizes.set(b.id, bytes);
      total += bytes;
    }
    for (const b of batches) {
      if (!sizes.has(b.id) || b.status === 'reading') continue;
      if (Date.parse(b.updated_at) < old || total > CACHE_BYTES) {
        rmSync(join(cache, b.id), { recursive: true, force: true });
        total -= sizes.get(b.id);
      }
    }
  }

  // ------------------------------------------------------------ starting
  /** The folder asked for: a Drive link or ID, or a folder of Photo_links by its path or name. */
  async function folderOf(asked, drive) {
    const wanted = text(asked);
    const id = driveIdOf(wanted) ?? (/^[\w-]{20,}$/.test(wanted) ? wanted : null);
    if (id) {
      const item = await drive.get(id);
      return item && !item.gone && item.mimeType === FOLDER ? { id, name: item.name } : null;
    }
    const lower = wanted.toLowerCase().replace(/^\/+|\/+$/g, '');
    const folders = photoLinkRows(store).filter(r => r.values.Type === 'Folder');
    const hit =
      folders.find(r => String(r.values['Full Path']).toLowerCase() === lower) ??
      folders.filter(r => String(r.values['Full Path']).toLowerCase().endsWith(`/${lower}`))[0] ??
      folders.find(r => String(r.values.Name).toLowerCase() === lower);
    if (hit) return { id: driveIdOf(hit.values.URL), name: hit.values['Full Path'] };
    // A folder not listed yet (a new upload): among the folders inside the listed ones, by its name or path.
    const roots = folders.filter(r => !String(r.values['Full Path']).includes('/')).map(r => driveIdOf(r.values.URL)).filter(Boolean);
    const tree = await driveTree(drive, roots, { foldersOnly: true });
    const pathOf = item => {
      const parts = [];
      for (let f = item; f; f = f.parent ? tree.get(f.parent) : null) parts.unshift(f.name);
      return parts.join('/');
    };
    const found = [...tree.values()].filter(f => f.mimeType === FOLDER && (f.name.toLowerCase() === lower || pathOf(f).toLowerCase().endsWith(lower)));
    return found.length === 1 ? { id: found[0].id, name: pathOf(found[0]) } : null;
  }

  async function start({ folder, all = false }, user, context) {
    const drive = driveFor(store, config);
    if (!drive) return { error: 'This copy of the app has no Google Drive to read.' };
    const where = await folderOf(folder, drive);
    if (!where) return { error: `No folder "${text(folder).slice(0, 120)}" on Drive: give its link, or its path as Photo_links lists it.` };
    const items = await driveTree(drive, [where.id]);
    const { known } = lookups();
    const files = [...items.values()].filter(f => f.mimeType !== FOLDER && f.parent);
    const named = f => {
      const parsed = specimenName(f.name);
      return !!(parsed && knownId(parsed.id, known));
    };
    const photos = files.filter(f => PHOTO.test(f.name) && !/_todelete$/i.test(f.name) && (all || !named(f)));
    if (!photos.length) return { error: `No photo to read in ${where.name}${all ? '' : ' (all its photos have names of the workbook: all: true reads them too)'}.` };
    if (photos.length > MAX_PHOTOS) return { error: `${photos.length} photos: at most ${MAX_PHOTOS} at a time; give one of its folders.` };
    const raws = new Map();
    for (const f of files) if (RAW.test(f.name)) (raws.get(stem(f.name)) ?? raws.set(stem(f.name), []).get(stem(f.name))).push(f);
    photos.sort((a, b) => (takenOf(a) ?? '').localeCompare(takenOf(b) ?? '') || byNumber(a.name, b.name));
    const id = randomUUID();
    const at = now();
    db.prepare('INSERT INTO drive_photo_batches (id, owner, folder_id, folder, status, created_at, updated_at, summary_json) VALUES (?,?,?,?,?,?,?,?)').run(
      id,
      String(user.id ?? user.username),
      where.id,
      where.name,
      'reading',
      at,
      at,
      JSON.stringify({ photos: photos.length, read: 0, all }),
    );
    photos.forEach((f, i) => {
      const raw = raws.get(stem(f.name))?.[0];
      saveRow(id, {
        file: f.id,
        n: i + 1,
        name: f.name,
        url: f.webViewLink ?? null,
        folder: items.get(f.parent)?.name ?? '',
        taken: takenOf(f),
        size: Number(f.size) || 0,
        ...(raw ? { raw: raw.id, rawName: raw.name } : {}),
        read: false,
      });
    });
    const shown = show?.(batchOf(id), context) ?? {};
    queue.push(id);
    pump();
    const ahead = queue.length - 1 + (running ? 1 : 0);
    return filled({
      batch: id,
      folder: where.name,
      photos: photos.length,
      withRawFile: photos.filter(f => raws.has(stem(f.name))).length,
      ...shown,
      waitingFor: ahead ? `${ahead} other batch(es) first` : null,
      next: `Reading takes about ${Math.max(1, Math.round((photos.length * 1.3) / 60))} min. Call drive_photos with batch "${id}": it answers once the batch is read (or after a minute and a half). The card beside the chat shows it too.`,
    });
  }

  // ------------------------------------------------------------ what the assistant is told
  const compact = r =>
    filled({
      file: r.file,
      n: r.n,
      name: r.name,
      id: r.camid,
      readAs: r.readAs !== r.camid ? r.readAs : null,
      conf: r.setBy === 'reader' && r.conf !== undefined ? Math.round(r.conf * 100) / 100 : null,
      also: r.setBy === 'reader' ? r.candidates?.slice(1) : null,
      view: r.view,
      taken: r.taken?.slice(11, 19),
      rotation: r.rotation,
      remove: r.remove,
      look: r.review,
      setBy: r.setBy !== 'reader' ? r.setBy : null,
      note: r.note,
      error: r.error,
    });
  function status(id, { rows: which = 'marked' } = {}) {
    const batch = batchOf(id);
    if (!batch) return { error: 'No batch by that ID.' };
    const summary = JSON.parse(batch.summary_json || '{}');
    if (batch.status === 'reading')
      return { batch: id, status: 'reading', folder: batch.folder, read: `${summary.read ?? 0} of ${summary.photos}`, next: 'Still reading: call drive_photos with this batch again (it waits for the end).' };
    if (batch.status === 'failed') return { batch: id, status: 'failed', error: batch.error };
    const rows = rowsOf(id);
    const shown = which === 'all' ? rows : rows.filter(r => r.marked || r.error || r.remove || (r.setBy && r.setBy !== 'reader'));
    return filled({
      batch: id,
      status: batch.status,
      folder: batch.folder,
      counts: summary.counts,
      order: summary.order,
      checks: summary.checks,
      toLook: shown.slice(0, PAGE).map(compact),
      more: shown.length > PAGE ? shown.length - PAGE : null,
      plan: batch.plan,
    });
  }

  function look({ batch, files, what = 'envelope' }) {
    const b = batchOf(batch);
    if (!b) return { error: 'No batch by that ID.' };
    const rows = new Map(rowsOf(batch).map(r => [r.file, r]));
    const out = { batch, pictures: [] };
    const images = [];
    for (const file of (Array.isArray(files) ? files : [files]).slice(0, LOOK_MAX)) {
      const r = rows.get(String(file)) ?? [...rows.values()].find(x => x.name === file);
      const path = r && picture(batch, r.file, what === 'photo' ? 'photo' : 'envelope');
      if (!r || !existsSync(path)) {
        out.pictures.push({ file, missing: r ? `no ${what} picture kept` : 'no such row' });
        continue;
      }
      out.pictures.push(filled({ file: r.file, name: r.name, id: r.camid, view: r.view }));
      images.push({ data: readFileSync(path).toString('base64'), mimeType: 'image/jpeg' });
    }
    return Object.assign(out, { [IMAGES]: images });
  }

  /** Changes to rows (`by`: 'assistant' or 'person'); the batch put in order again. */
  function edit({ batch, changes }, by) {
    const b = batchOf(batch);
    if (!b) return { error: 'No batch by that ID.' };
    if (b.status !== 'ready') return { error: b.status === 'reading' ? 'Still reading: wait for it.' : `The batch is ${b.status}.` };
    const rows = new Map(rowsOf(batch).map(r => [r.file, r]));
    const problems = [];
    let changed = 0;
    for (const c of Array.isArray(changes) ? changes : []) {
      const r = rows.get(String(c.file)) ?? [...rows.values()].find(x => x.name === c.file);
      if (!r) {
        problems.push(`${c.file}: no such row`);
        continue;
      }
      if (c.id !== undefined) {
        const id = text(c.id).toUpperCase().replace(/\s+/g, '');
        if (id && !/^[A-Z]*\d+$/.test(id)) {
          problems.push(`${r.name}: ${c.id} is not an ID`);
          continue;
        }
        r.camid = id || null;
        if (c.view === undefined) r.viewSet = false;
      }
      if (c.view !== undefined) {
        const view = text(c.view).toLowerCase();
        if (view && !/^[dv]\d*$/.test(view)) {
          problems.push(`${r.name}: view is d, v, d2, v2…`);
          continue;
        }
        r.view = view;
        r.viewSet = !!view;
      }
      if (c.remove !== undefined) r.remove = c.remove === true;
      if (c.rotation !== undefined) {
        r.rotation = norm(c.rotation);
        r.rotationSet = true;
      }
      if (c.note !== undefined) r.note = text(c.note).slice(0, 300) || undefined;
      r.setBy = by;
      saveRow(batch, r);
      changed++;
    }
    const summary = settle(batch);
    return filled({ changed, problems, counts: summary.counts, checks: summary.checks, order: summary.order });
  }

  /** The batch as changes to the files: renames (their RAW files with them), turns, removals. */
  function operations(id) {
    const rows = rowsOf(id).filter(r => r.read);
    const ops = [];
    const left = [];
    const names = new Set();
    for (const r of rows) {
      if (r.remove) {
        ops.push({ action: 'remove', file: `https://drive.google.com/file/d/${r.file}/view` });
        continue;
      }
      const to = nameFor(r);
      if (!to) {
        left.push(`${r.name}: no ID`);
        continue;
      }
      if (names.has(to)) {
        left.push(`${r.name}: ${to} is given to another photo of the batch too`);
        continue;
      }
      names.add(to);
      if (to !== r.name) ops.push({ action: 'rename', file: `https://drive.google.com/file/d/${r.file}/view`, name: to });
      if (r.rotation) ops.push({ action: 'rotate', file: `https://drive.google.com/file/d/${r.file}/view`, degrees: r.rotation });
    }
    return { ops, left };
  }

  async function apply(id, user) {
    const b = batchOf(id);
    if (!b) return { error: 'No batch by that ID.', code: 'not_found' };
    if (b.status !== 'ready') return { error: `The batch is ${b.status}.`, code: 'state' };
    if (!organize) return { error: 'This app cannot change files on Drive.', code: 'unavailable' };
    const { ops, left } = operations(id);
    if (!ops.length) return { error: 'Nothing to change: every photo has its name already.', code: 'nothing' };
    const out = await organize(ops, user);
    if (out.error) return { ...out, code: 'plan' };
    setBatch(id, { status: 'applied', plan: out.plan });
    tell?.(batchOf(id));
    return filled({ status: 'applying', plan: out.plan, link: out.link, leftOut: left });
  }

  return {
    async run(name, args = {}, user, context) {
      if (!EDITORS.includes(user?.role)) return { error: 'Your role cannot change the photos.' };
      try {
        if (name === 'drive_photos' && args.folder && !args.batch) return await start({ folder: args.folder, all: args.all === true }, user, context);
        // Being read: the answer waits for it to end, a minute and a half at most.
        if (name === 'drive_photos' && runningId === String(args.batch) && running)
          await Promise.race([running, new Promise(resolve => setTimeout(resolve, waitMs).unref?.())]);
        if (name === 'drive_photos') return status(args.batch, args);
        if (name === 'drive_photos_look') return look(args);
        if (name === 'drive_photos_edit') return edit(args, 'assistant');
      } catch (e) {
        return { error: String(e.message || e) };
      }
      return { error: `No tool ${name}` };
    },
    start,
    status,
    edit,
    apply,
    operations,
    batch: batchOf,
    rows: rowsOf,
    discard(id) {
      const b = batchOf(id);
      if (!b || b.status === 'reading') return false;
      setBatch(id, { status: 'discarded' });
      rmSync(join(cache, id), { recursive: true, force: true });
      tell?.(batchOf(id));
      return true;
    },
    /** A kept picture's path ('photo' or 'envelope'), or null. */
    picture: (id, file, what) => {
      const path = picture(id, String(file).replace(/[^\w-]/g, ''), what);
      return existsSync(path) ? path : null;
    },
    /** Waits for the batch being read (tests). */
    idle: async () => {
      while (running || queue.length) await (running ?? new Promise(r => setImmediate(r)));
    },
  };
}
