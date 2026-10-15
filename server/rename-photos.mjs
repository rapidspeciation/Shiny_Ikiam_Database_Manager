// The «Fotos» tab as the assistant reaches it. The photos are on the person's computer,
// read and renamed in their browser by the Rename Photos app shown in the tab
// (github.com/Fr4nzz/rename_photos_AI, frontend RenameHost.vue); the assistant runs on the server.
// So the open tab is its hands: it tells the app the batch as it changes (the rows: each photo's
// file, the ID read on its envelope, its flags, its rotation) and asks for work to do; a tool call
// leaves a command for it (small copies of photos to look at, corrections to rows) and waits for
// the tab's answer. Nothing is kept on disk, and no file is renamed from here: renaming is the
// person's own click in the tab.
//
//   rename_photos_read   the batch: counts, the IDs in the order photographed, what does not add up
//   rename_photos_look   photos as pictures (the envelope, or the whole photo)
//   rename_photos_edit   corrections to rows (ID, skip, turn, leave for the person), or all taken back
//
// HTTP, for the tab (RenameHost.vue): POST rename/state, GET rename/commands (waits for one),
// POST rename/result, GET rename/ids (the workbook's IDs, for the reader's database rule),
// GET rename/upload (where the batch's files go on Drive, for the tab's own bar).

import { createHash, randomUUID } from 'node:crypto';
import { photoLinkRows, specimenName, specimens } from './photo-links.mjs';

// ------------------------------------------------------------ the tools
export const RENAME_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'rename_photos_read',
      description: [
        "The batch of wing photos open in the person's «Rename photos» tab: the photos of a folder on their computer, each with the ID the app read on its envelope, before they are renamed after it.",
        '- `order`: the IDs in the order the photos were taken, each with its number of photos (`CAM077001×2`); `!` marks one with a photo waiting for a person, `?` photos without an ID.',
        '- `checks`: what does not add up. `notTwoPhotos`: an ID with one photo, or more than two (each butterfly has a dorsal and a ventral photo, taken one after the other) · `apart`: an ID on photos not taken one after the other · `notInWorkbook`: an ID no row of the workbook has (`maybe`: workbook IDs one character away) · `alreadyPhotographed`: an ID whose photos Photo_links already lists · `turned`: photos whose envelope pointed another way than the rest of their session · `turnUnsure`: the turn of the batch is not clear from the envelopes (laid sideways, or pointing both ways): two photos of each run to look at (`lookAt`); the photos are landscape with the forewings above the hindwings, and one turn fits them all · `wings`: photos whose wings disagree with their name or turn (`says`).',
        '- `wingCheck`: the wing check, run by the app on each photo after the reading: the side the wings show against d/v, the wings of a pair compared, which way up. Absent when it has run on every photo.',
        "- `upload`: where the files go once renamed: the Drive folders that hold the photos (`photos`) and the RAW files (`rawFiles`) of the IDs nearest these, each with its link and its place in Drive for desktop; the folder's size on the computer and the space left in the project's Drive (`projectDrive`).",
        '- `rows`: the photos to look at (those flagged and those in `checks`), or with `show: "all"` every photo, `from` and `limit` paging through them; `files` picks photos by name. A row: its number in the batch `n`, `file`, `id`, `view` (d, v: by the order taken), `taken`, `review` (why the reader left it for a person: no-reading, not-in-database, out-of-sequence, repeated-id, unreadable-photo, rotation-outlier, turn-unsure; wing-side, wing-count, wing-turn from the wing check), `suggest`, `conf` (how sure the reader was, 0–1), `rotation` (clockwise degrees chosen for it), `wings` (the side its wings show, dorsal being upper side up, with P(dorsal), the wings found and the turn that puts the forewings on top), `wingNotes` (what the wings say against its name or turn).',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          show: { type: 'string', enum: ['problems', 'all'] },
          files: { type: 'array', items: { type: 'string' } },
          from: { type: 'integer' },
          limit: { type: 'integer' },
        },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'rename_photos_look',
      description: [
        "Photos of the batch as pictures, made by the person's browser from the files on their computer: up to 24 `files` a call.",
        '- `part`: "envelope" (default: the envelope with its ID, upright, large enough to read the handwriting) or "photo" (the whole photo as it will be once turned: the wings, the envelope, the colour card).',
        '- Several photos come six to a picture, each with its number, file and ID written under it (`sheet: false`: one picture each, larger).',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          files: { type: 'array', items: { type: 'string' } },
          part: { type: 'string', enum: ['envelope', 'photo'] },
          sheet: { type: 'boolean' },
        },
        required: ['files'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'rename_photos_edit',
      description: [
        "Corrects rows of the batch in the person's tab, as they would in its review; each shows there as changed by you, and the person renames the files themselves afterwards.",
        '- `edits`: {file, id} the ID on the envelope (the photo then no longer waits for a person; `confirm: false` keeps it waiting) · {file, confirm: true} the ID read is right · {file, skip: true} the photo is left out of the renaming (a repeated or blurred one) · {file, rotate: 90 | 180 | 270} turned clockwise from how it shows now · {file, leave: "why"} left for the person, with the reason on its card · {file, view: "d" | "v"} instead of the view its order gives.',
        '- `undo: true` takes back every change you made to this batch.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          edits: {
            type: 'array',
            items: {
              type: 'object',
              properties: {
                file: { type: 'string' },
                id: { type: 'string' },
                confirm: { type: 'boolean' },
                skip: { type: 'boolean' },
                rotate: { type: 'integer', enum: [90, 180, 270] },
                leave: { type: 'string' },
                view: { type: 'string' },
                note: { type: 'string' },
              },
              required: ['file'],
            },
          },
          undo: { type: 'boolean' },
        },
      },
    },
  },
];
export const RENAME_TOOL_NAMES = new Set(RENAME_TOOLS.map(t => t.function.name));
/** Pictures a tool answers with ({ data: base64, mimeType }), beside its JSON (server/assistant.mjs). */
export const IMAGES = Symbol('images');

const EDITORS = ['editor', 'reviewer', 'admin'];
/** The tab asks for work again within this long; silent for longer, it counts as closed. */
const POLL_MS = 25_000;
const ALIVE_MS = 45_000;
const ANSWER_MS = { view: 90_000, edit: 30_000, undo: 30_000, state: 15_000 };
const MAX_ROWS = 6000;
const PAGE = 120;
const LOOKUPS_MS = 60_000;
const EXAMPLES = 12;

const text = v => String(v ?? '').trim();
const filled = object =>
  Object.fromEntries(
    Object.entries(object).filter(
      ([, v]) => !(v === 0 || v === null || v === undefined || v === false || v === '' || (typeof v === 'object' && !Object.keys(v).length)),
    ),
  );

/** A row as the tab tells it, kept to what the tools use. */
function cleanRow(r) {
  const list = v => (Array.isArray(v) ? v.map(x => text(x).slice(0, 60)).filter(Boolean).slice(0, 12) : []);
  return {
    file: text(r?.file).slice(0, 200),
    now: text(r?.now).slice(0, 200) || undefined,
    n: Number(r?.n) || 0,
    id: text(r?.id).toUpperCase().slice(0, 40),
    view: text(r?.view).slice(0, 8),
    to: text(r?.to).slice(0, 200),
    taken: typeof r?.taken === 'string' && !Number.isNaN(Date.parse(r.taken)) ? r.taken : null,
    status: text(r?.status).slice(0, 20),
    skip: r?.skip === true,
    review: list(r?.review),
    suggest: list(r?.suggest),
    otherIdsRead: list(r?.otherIdsRead),
    note: text(r?.note).slice(0, 300),
    conf: typeof r?.conf === 'number' ? r.conf : undefined,
    readFrom: text(r?.readFrom).slice(0, 20),
    rotation: Number(r?.rotation) || 0,
    rotationPending: Number(r?.rotationPending) || 0,
    rotationFrom: text(r?.rotationFrom).slice(0, 20),
    byHost: list(r?.byHost),
    wings: wingsOf(r?.wings),
    wingNotes: Array.isArray(r?.wingNotes) ? r.wingNotes.map(x => text(x).slice(0, 200)).filter(Boolean).slice(0, 6) : [],
  };
}
const SIDES = new Set(['dorsal', 'ventral', 'unsure', 'mixed', 'absent']);
/** The wing check of a photo, in a few words: the side its wings show, how many, which turn they ask for. */
function wingsOf(w) {
  if (!w || typeof w !== 'object' || !SIDES.has(w.side)) return undefined;
  const p = typeof w.pDorsal === 'number' ? ` p=${w.pDorsal.toFixed(2)}` : '';
  const count = Number(w.count) || 0;
  const missing = Array.isArray(w.missing) && w.missing.length ? ` (no ${w.missing.map(x => text(x).split('_').reverse().join(' ')).join(', ')})` : '';
  const turn = [90, 180, 270].includes(w.turn) ? `, upright after ${w.turn}°` : w.turn === 0 ? ', upright' : '';
  const groups = w.side === 'mixed' ? ` (forewings ${text(w.forewings)}, hindwings ${text(w.hindwings)})` : '';
  return `${w.side}${groups}${p}, ${count} wing${count === 1 ? '' : 's'}${missing}${turn}`.slice(0, 200);
}
export function cleanState(state) {
  return {
    folder: text(state?.folder).slice(0, 200) || null,
    canWrite: state?.canWrite === true,
    files: Number(state?.files) || 0,
    folderBytes: Number(state?.folderBytes) > 0 ? Number(state.folderBytes) : null,
    reading: { on: state?.reading?.on === true, percent: Number(state?.reading?.percent) || 0, message: text(state?.reading?.message).slice(0, 200) },
    wingCheck: {
      on: state?.wingCheck?.on === true,
      done: Number(state?.wingCheck?.done) || 0,
      total: Number(state?.wingCheck?.total) || 0,
      checked: Number(state?.wingCheck?.checked) || 0,
    },
    rows: (Array.isArray(state?.rows) ? state.rows.slice(0, MAX_ROWS) : []).map(cleanRow).filter(r => r.file),
  };
}

/** The rows in the order the photos were taken (those without a time after, by their number). */
const inOrder = rows =>
  [...rows].sort((a, b) => (a.taken ? Date.parse(a.taken) : Infinity) - (b.taken ? Date.parse(b.taken) : Infinity) || a.n - b.n);
// A photo flagged only for which way up it is waits as its session does (checks.turnUnsure), not by itself.
const waits = r => r.review.some(x => x !== 'turn-unsure');
/** Photos taken further apart than this are of another session (one setup, one turn). */
const SESSION_MS = 30 * 60_000;

/** IDs one character away from `id` among `known` (a misread digit), a few. */
function near(id, known) {
  const out = [];
  const digits = '0123456789';
  for (let i = 0; i < id.length && out.length < 4; i++) {
    if (!digits.includes(id[i])) continue;
    for (const d of digits) {
      if (d === id[i]) continue;
      const other = `${id.slice(0, i)}${d}${id.slice(i + 1)}`;
      if (known.has(other)) out.push(other);
    }
  }
  return out.slice(0, 4);
}

/**
 * What does not add up in a batch: { order, counts, checks, flagged } from its rows, the workbook's
 * IDs (`known`: a Set, or null without them) and the IDs whose photos Photo_links lists (`listed`).
 */
export function checkBatch(rows, { known = null, listed = null } = {}) {
  const ordered = inOrder(rows.filter(r => !r.skip));
  // Runs of one ID, as photographed.
  const runs = [];
  for (const r of ordered) {
    const last = runs.at(-1);
    if (last && last.id === r.id && r.id) last.rows.push(r);
    else if (last && !last.id && !r.id) last.rows.push(r);
    else runs.push({ id: r.id, rows: [r] });
  }
  const order = runs.map(run => `${run.id || '?'}×${run.rows.length}${run.rows.some(waits) ? '!' : ''}`).join(' ');
  const byId = new Map();
  for (const r of ordered) if (r.id) (byId.get(r.id) ?? byId.set(r.id, []).get(r.id)).push(r);
  const runsOf = new Map();
  for (const run of runs) if (run.id) runsOf.set(run.id, (runsOf.get(run.id) ?? 0) + 1);

  const show = r => filled({ n: r.n, file: r.now || r.file, view: r.view, taken: r.taken?.slice(11, 19) });
  const some = list => ({ count: list.length, examples: list.slice(0, EXAMPLES) });
  const notTwo = [...byId].filter(([, list]) => list.length !== 2).map(([id, list]) => ({ id, photos: list.map(show) }));
  const apart = [...runsOf].filter(([, n]) => n > 1).map(([id]) => ({ id, photos: byId.get(id).map(show) }));
  const unknown = known ? [...byId].filter(([id]) => !known.has(id)).map(([id, list]) => filled({ id, photos: list.length, maybe: near(id, known) })) : [];
  const already = listed ? [...byId].filter(([id]) => listed.has(id)).map(([id, list]) => ({ id, photos: list.length })) : [];
  const turned = ordered.filter(r => r.review.includes('rotation-outlier')).map(show);
  // Which way up: the sessions whose envelopes lay sideways, with two of their photos to look at.
  const unsure = [];
  let last = NaN;
  for (const r of ordered) {
    const t = r.taken ? Date.parse(r.taken) : NaN;
    if (r.review.includes('turn-unsure')) {
      const open = unsure.at(-1);
      if (open && !(t - last >= SESSION_MS)) open.rows.push(r);
      else unsure.push({ rows: [r] });
    }
    if (!Number.isNaN(t)) last = t;
  }
  const wingsSay = ordered.filter(r => r.wingNotes?.length).map(r => ({ ...show(r), id: r.id, says: r.wingNotes }));
  const turnUnsure = unsure.map(u => ({ photos: u.rows.length, from: u.rows[0].n, to: u.rows.at(-1).n, rotation: u.rows[0].rotation, lookAt: [u.rows[0], u.rows[Math.floor(u.rows.length / 2)]].filter((r, i, a) => a.indexOf(r) === i).map(show) }));
  const checks = filled({
    notTwoPhotos: notTwo.length ? some(notTwo) : null,
    apart: apart.length ? some(apart) : null,
    notInWorkbook: unknown.length ? some(unknown) : null,
    alreadyPhotographed: already.length ? some(already) : null,
    turned: turned.length ? some(turned) : null,
    turnUnsure: turnUnsure.length ? some(turnUnsure) : null,
    wings: wingsSay.length ? some(wingsSay) : null,
  });
  // The photos to look at: those waiting for a person, and those of the IDs the checks name.
  const named = new Set([...notTwo, ...apart, ...unknown].map(x => x.id));
  const samples = new Set(turnUnsure.flatMap(u => u.lookAt.map(x => x.file)));
  const flagged = ordered.filter(r => waits(r) || !r.id || named.has(r.id) || samples.has(r.now || r.file));
  return {
    order,
    counts: filled({
      photos: rows.length,
      ids: byId.size,
      waitingForAPerson: rows.filter(r => !r.skip && waits(r)).length,
      withoutId: ordered.filter(r => !r.id).length,
      skipped: rows.filter(r => r.skip).length,
      renamed: rows.filter(r => r.status === 'Renamed').length,
      changedByYou: rows.filter(r => r.byHost.length).length,
    }),
    checks,
    flagged,
  };
}

const PHOTO_FILE = /\.(jpe?g|png|heic|heif)$/i;
const RAW_FILE = /\.(cr2|cr3|orf|nef|arw|dng|raf|rw2|pef)$/i;
/** How far apart two IDs' numbers may be for one's folder to say where the other's photos go. */
const NEAR = 400;
/** The folder everything is under, as Drive for desktop shows it. */
const DRIVE_HOME = 'Ithomiini_IKIAM › Photos';
const idParts = id => /^([A-Z]+)(\d+)$/.exec(String(id ?? '').toUpperCase());

/**
 * Where the photos Photo_links lists are, by their ID: { jpg, raw } (each [{ prefix, num, folder }]
 * in the order of the IDs) and the folders' links by their path.
 */
export function listedPlaces(rows) {
  const out = { jpg: [], raw: [], folders: new Map() };
  for (const { values: v } of rows) {
    const path = text(v['Full Path']);
    if (v.Type === 'Folder') out.folders.set(path, text(v.URL));
    if (v.Type !== 'File') continue;
    const kind = PHOTO_FILE.test(path) ? out.jpg : RAW_FILE.test(path) ? out.raw : null;
    const parts = kind && idParts(specimenName(v.Name)?.id);
    if (parts && path.includes('/')) kind.push({ prefix: parts[1], num: Number(parts[2]), folder: path.slice(0, path.lastIndexOf('/')) });
  }
  for (const list of [out.jpg, out.raw]) list.sort((a, b) => (a.prefix < b.prefix ? -1 : a.prefix > b.prefix ? 1 : a.num - b.num));
  return out;
}

/** The folder most of `ids` point to: the one holding the photos of the listed IDs nearest each. */
function folderFor(ids, list, folders) {
  // A folder weighs by the files near each ID; `idsFor`: how many IDs have one of its files near.
  const votes = new Map();
  const idsFor = new Map();
  let voters = 0;
  for (const id of ids) {
    const parts = idParts(id);
    if (!parts) continue;
    const [, prefix, digits] = parts;
    const num = Number(digits);
    // The first listed ID not below this one, then its neighbours on both sides.
    let lo = 0;
    let hi = list.length;
    while (lo < hi) {
      const mid = (lo + hi) >> 1;
      const e = list[mid];
      if (e.prefix < prefix || (e.prefix === prefix && e.num < num)) lo = mid + 1;
      else hi = mid;
    }
    const near = [];
    for (const at of [lo - 2, lo - 1, lo, lo + 1]) {
      const e = list[at];
      if (e && e.prefix === prefix && Math.abs(e.num - num) <= NEAR) near.push(e.folder);
    }
    if (!near.length) continue;
    voters++;
    for (const folder of near) votes.set(folder, (votes.get(folder) ?? 0) + 1);
    for (const folder of new Set(near)) idsFor.set(folder, (idsFor.get(folder) ?? 0) + 1);
  }
  if (!voters) return null;
  const [[folder], second] = [...votes].sort((a, b) => b[1] - a[1]);
  const count = idsFor.get(folder);
  return filled({
    folder,
    link: folders.get(folder),
    inDriveForDesktop: `${DRIVE_HOME} › ${folder.split('/').join(' › ')}`,
    because: `the photos of the IDs nearest ${count} of these ${ids.length} IDs are there`,
    alsoNear: second && idsFor.get(second[0]) >= voters / 4 ? second[0] : null,
  });
}

/**
 * Where a batch's files go on Drive: the folders that hold the photos, and the RAW files, of the IDs
 * nearest the batch's; or, when none is near, what the batch's butterflies are (to name a new folder).
 */
export function uploadPlace(rows, places, butterflies = null) {
  const ids = [...new Set(rows.filter(r => !r.skip && r.id).map(r => r.id))];
  if (!ids.length) return null;
  const jpg = folderFor(ids, places.jpg, places.folders);
  const raw = folderFor(ids, places.raw, places.folders);
  const found = ids.map(id => butterflies?.get(id)).filter(Boolean);
  const count = values => {
    const tally = new Map();
    for (const v of values) if (v) tally.set(v, (tally.get(v) ?? 0) + 1);
    return Object.fromEntries([...tally].sort((a, b) => b[1] - a[1]).slice(0, 4));
  };
  // A sheet's date number as its year.
  const year = date => (typeof date === 'number' ? new Date(Date.UTC(1899, 11, 30) + date * 86_400_000).getUTCFullYear() : null);
  return filled({
    photos: jpg,
    rawFiles: raw,
    ...(jpg
      ? {}
      : {
          noFolderYet: 'No photo of an ID near these is listed: a new collection. Its folder is named after the country and year (PERU_2024), with JPG and RAW inside, in «Photos wings of collected butterflies» (organize_photos makes them).',
          butterflies: filled({ countries: count(found.map(b => b.country)), years: count(found.map(b => year(b.date))) }),
        }),
  });
}

/** A row as the model is told it. */
const told = r =>
  filled({
    n: r.n,
    file: r.now || r.file,
    id: r.id,
    view: r.view,
    taken: r.taken?.slice(11, 19),
    review: r.review,
    suggest: r.suggest,
    otherIdsRead: r.otherIdsRead,
    conf: r.conf,
    readFrom: r.readFrom === 'envelope' ? null : r.readFrom,
    rotation: r.rotation,
    skip: r.skip,
    note: r.note,
    wings: r.wings,
    wingNotes: r.wingNotes,
    changedByYou: r.byHost,
  });

/** A folder of photos found again: its name and the names its photos had when first read (they keep them as `file`). */
export const folderKey = state =>
  createHash('sha1')
    .update(`${state.folder}\n${state.rows.map(r => r.file).sort().join('\n')}`)
    .digest('hex')
    .slice(0, 24);

/** The prompt of the chat the tab's «Review with Claude» starts. */
export const REVIEW_PROMPT = [
  'Revisa el lote de fotos que tengo abierto en la pestaña «Fotos» (skill `photos`, «Reviewing a batch before it is renamed»).',
  'Comprueba las fotos marcadas, los IDs que no tienen exactamente dos fotos y una muestra de las demás mirando los sobres. Corrige lo que esté claro y déjame señalado lo que no puedas resolver.',
  'El control de alas ya comparó las alas de cada foto con su d/v y su giro: mira las que señala. Si el giro sigue sin estar claro (`turnUnsure`), compruébalo mirando dos o tres fotos enteras: deben quedar horizontales, con las alas anteriores arriba y las posteriores abajo; si no, gíralas todas igual.',
  'Al terminar dime qué cambiaste, qué queda para mí y si ya puedo pulsar «Rename».',
].join(' ');

/**
 * The batches open in people's «Fotos» tabs, for one app.
 *  - run(name, args, user): a tool.
 *  - handle({ method, path, body, query, user }): the tab's HTTP (null: not one of its paths).
 *  - active(user): whether the person's tab is open with a batch; folderOf(user): its folder, and the key that finds it again.
 * `space()`: the project account's space on Drive (server/photo-links-tool.mjs), for where the batch
 * goes next. `pollMs`, `answerMs`: for tests.
 */
export function createRenamePhotos({ store, pollMs = POLL_MS, answerMs = null, now = Date.now, space = null } = {}) {
  /** By person: { tab, at, state, queue, wake, pending: Map id → { resolve, timer } }. */
  const sessions = new Map();
  const keyOf = user => String(user?.id ?? user?.username ?? '');
  const sessionOf = (user, tab) => {
    const key = keyOf(user);
    let s = sessions.get(key);
    // Another tab of the same person takes over: the earlier one's commands are answered as lost.
    if (!s || (tab && s.tab !== tab)) {
      if (s) drop(s, 'The tab was opened again: ask once more.');
      s = { tab, at: now(), state: null, queue: [], wake: null, pending: new Map() };
      sessions.set(key, s);
    }
    return s;
  };
  function drop(s, why) {
    for (const p of s.pending.values()) {
      clearTimeout(p.timer);
      p.resolve({ error: why });
    }
    s.pending.clear();
    s.wake?.([]);
  }
  const live = user => {
    const s = sessions.get(keyOf(user));
    return s && now() - s.at < ALIVE_MS ? s : null;
  };
  const NOT_OPEN = "The «Rename photos» tab is not open in the person's browser (or was closed): they open it, choose the folder with the photos and press «Run PaddleOCR».";

  /** Leaves a command for the tab and waits for its answer. */
  function ask(s, name, args) {
    const id = randomUUID();
    return new Promise(resolve => {
      const timer = setTimeout(() => {
        s.pending.delete(id);
        s.queue = s.queue.filter(c => c.id !== id);
        resolve({ error: "The person's tab did not answer in time (the browser may be busy reading photos, or the tab asleep): ask again in a moment." });
      }, answerMs ?? ANSWER_MS[name] ?? 30_000);
      timer.unref?.();
      s.pending.set(id, { resolve, timer });
      s.queue.push({ id, name, args });
      const wake = s.wake;
      s.wake = null;
      wake?.(s.queue.splice(0));
    });
  }

  /** The workbook's IDs a photo can be named after, and the IDs whose photos Photo_links lists; read again after a minute. */
  let lookups = null;
  function fresh() {
    if (lookups && now() - lookups.at < LOOKUPS_MS) return lookups;
    const butterflies = specimens(store);
    const known = new Set(butterflies.keys());
    const listed = new Set();
    const links = photoLinkRows(store);
    for (const r of links) {
      const photo = r.values.Type === 'File' ? specimenName(r.values.Name) : null;
      if (photo) listed.add(photo.id.toUpperCase());
    }
    lookups = { at: now(), known, listed, butterflies, places: listedPlaces(links) };
    return lookups;
  }

  /** After the renaming: where the files go on Drive, and whether they fit. */
  async function afterRenaming(state) {
    const { places, butterflies } = fresh();
    const upload = uploadPlace(state.rows, places, butterflies);
    if (!upload) return null;
    const drive = await Promise.resolve(space?.()).catch(() => null);
    const batchGb = state.folderBytes ? Math.round((state.folderBytes / 2 ** 30) * 10) / 10 : null;
    return filled({
      ...upload,
      folderOnTheComputerGb: batchGb,
      projectDrive: drive
        ? filled({
            freeGb: drive.freeGb,
            usedPercent: drive.percent,
            nearlyFull: drive.nearlyFull,
            fits: batchGb !== null && drive.freeGb !== undefined && batchGb > drive.freeGb ? 'no: the folder is larger than the space left' : null,
          })
        : null,
      then: 'Once the files are uploaded: sync_photo_links lists them, then update_websites brings the Wings Gallery and the maps up to date.',
    });
  }

  /** Where the batch's wing check is (null: done on every photo). */
  function wingCheckNow(state) {
    const w = state.wingCheck;
    if (w?.on) return `checking the wings: ${w.done}/${w.total} photos; read again when it has finished`;
    const photos = state.rows.filter(r => !r.skip).length;
    if (!w?.checked) return 'not run on this batch (the tab has a «Check wings» button)';
    return w.checked < photos ? `run on ${w.checked} of ${photos} photos` : null;
  }

  async function read(args, s) {
    const state = s.state;
    if (!state?.rows.length)
      return filled({
        folder: state?.folder,
        photosInFolder: state?.files,
        reading: state?.reading.on ? `${state.reading.percent}% ${state.reading.message}`.trim() : null,
        note: state?.reading.on
          ? 'The app is still reading the photos: read again when it has finished.'
          : 'No photo has been read yet: the person chooses the folder in the tab and presses «Run PaddleOCR».',
      });
    const { known, listed } = fresh();
    const checked = checkBatch(state.rows, { known, listed });
    const picked = Array.isArray(args.files) && args.files.length ? new Set(args.files.map(f => text(f).toLowerCase())) : null;
    const pool = picked
      ? state.rows.filter(r => picked.has(r.file.toLowerCase()) || picked.has((r.now ?? '').toLowerCase()))
      : args.show === 'all'
        ? inOrder(state.rows)
        : checked.flagged;
    const from = Math.max(0, Number(args.from) || 0);
    const limit = Math.min(Math.max(1, Number(args.limit) || PAGE), 400);
    const page = pool.slice(from, from + limit);
    return filled({
      folder: state.folder,
      reading: state.reading.on ? `${state.reading.percent}% ${state.reading.message}`.trim() : null,
      canRenameInPlace: state.canWrite ? null : 'no: the folder was opened read-only (a browser other than Chrome or Edge)',
      wingCheck: wingCheckNow(state),
      counts: checked.counts,
      order: checked.order,
      checks: checked.checks,
      rows: page.map(told),
      more: pool.length > from + limit ? { from: from + limit, of: pool.length } : null,
      upload: picked || from ? null : await afterRenaming(state),
    });
  }

  async function look(args, s) {
    const files = (Array.isArray(args.files) ? args.files : args.files ? [args.files] : []).map(f => text(f)).filter(Boolean).slice(0, 24);
    if (!files.length) return { error: 'Give the `files` to look at (their names in the batch).' };
    const out = await ask(s, 'view', { files, part: args.part === 'photo' ? 'photo' : 'envelope', sheet: args.sheet ?? files.length > 1 });
    if (out.error) return out;
    const images = (out.images ?? []).filter(i => typeof i?.jpeg === 'string' && i.jpeg.length < 3_000_000).slice(0, 24);
    return {
      pictures: images.map((i, k) => ({ picture: k + 1, shows: (i.files ?? []).map(f => text(f).slice(0, 200)) })),
      ...(out.missing?.length ? { notInTheBatch: out.missing.slice(0, 24) } : {}),
      ...(images.length ? {} : { error: 'The tab could not make the pictures (the folder may need opening again).' }),
      [IMAGES]: images.map(i => ({ data: i.jpeg, mimeType: 'image/jpeg' })),
    };
  }

  async function edit(args, s) {
    if (args.undo) {
      const out = await ask(s, 'undo', {});
      return out.error ? out : { status: 'undone', rowsAsTheyWere: out.rowsRestored ?? 0 };
    }
    const edits = (Array.isArray(args.edits) ? args.edits : []).slice(0, 2000);
    if (!edits.length) return { error: 'Give `edits` (or `undo: true`).' };
    const out = await ask(s, 'edit', { edits, by: 'Claude' });
    if (out.error) return out;
    return filled({
      changed: (out.changed ?? []).length,
      rows: (out.rows ?? []).map(cleanRow).map(told),
      notDone: out.problems,
      next: 'The person sees each change on its card and renames the files with «Rename» in the tab.',
    });
  }

  return {
    active: user => !!live(user)?.state?.rows.length,
    /** The folder open in the person's tab, as a key that finds it again (its name and its photos' first names), or null. */
    folderOf: user => {
      const state = live(user)?.state;
      return state?.folder && state.rows.length ? { folder: state.folder, key: folderKey(state) } : null;
    },
    async run(name, args = {}, user) {
      if (!EDITORS.includes(user?.role)) return { error: 'Your role cannot rename photos' };
      const s = live(user);
      if (!s) return { error: NOT_OPEN };
      if (name === 'rename_photos_read') return read(args, s);
      if (name === 'rename_photos_look') return look(args, s);
      return edit(args, s);
    },
    async handle({ method, path, body = {}, query = {}, user }) {
      const m = /^\/api\/rename\/(state|commands|result|ids|upload)$/.exec(path);
      if (!m) return null;
      if (!EDITORS.includes(user?.role)) return { status: 403, body: { code: 'forbidden', message: 'Your role cannot rename photos' } };
      const what = m[1];
      if (what === 'ids' && method === 'GET') return { status: 200, body: { ids: [...fresh().known] } };
      const tab = text(method === 'GET' ? query.tab : body.tab).slice(0, 64);
      if (!tab) return { status: 400, body: { code: 'bad_request', message: 'tab is required' } };
      const s = sessionOf(user, tab);
      s.at = now();
      // Where the batch's files go, as the tab's bar says it (null before any photo has an ID).
      if (what === 'upload' && method === 'GET') return { status: 200, body: { upload: s.state?.rows.length ? await afterRenaming(s.state) : null } };
      if (what === 'state' && method === 'POST') {
        s.state = cleanState(body.state);
        return { status: 200, body: { ok: true } };
      }
      if (what === 'commands' && method === 'GET') {
        if (s.queue.length) return { status: 200, body: { commands: s.queue.splice(0) } };
        // One wait at a time: an earlier one (the page asked again) is let go.
        s.wake?.([]);
        const commands = await new Promise(resolve => {
          const timer = setTimeout(() => {
            if (s.wake === wake) s.wake = null;
            resolve([]);
          }, pollMs);
          timer.unref?.();
          const wake = list => {
            clearTimeout(timer);
            resolve(list);
          };
          s.wake = wake;
        });
        s.at = now();
        return { status: 200, body: { commands } };
      }
      if (what === 'result' && method === 'POST') {
        const waiting = s.pending.get(text(body.id));
        if (waiting) {
          s.pending.delete(text(body.id));
          clearTimeout(waiting.timer);
          waiting.resolve(body.ok === false ? { error: text(body.error).slice(0, 300) || 'The tab could not do it.' } : (body.data ?? {}));
        }
        return { status: 200, body: { ok: true } };
      }
      return { status: 405, body: { code: 'method', message: 'Method not allowed' } };
    },
    close() {
      for (const s of sessions.values()) drop(s, 'The app is restarting: ask again in a minute.');
      sessions.clear();
    },
  };
}
