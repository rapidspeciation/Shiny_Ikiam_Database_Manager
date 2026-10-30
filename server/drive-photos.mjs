// Photos uploaded to Drive without being renamed (the camera's names), read on this server: each
// photo is downloaded, its envelope's CAM ID read by the Rename Photos app's reader (the same models,
// photo-reader/ beside the server: scripts/build-rename-app.sh), and a small copy of it and of its
// envelope kept. The photo itself is kept too, in a cache of whole photos (5 GB by default, the least
// used dropped first; none while the disk has little room), for the assistant to look closer at the odd
// ones (drive_photos_look with a `box`: a part at full resolution); one dropped is downloaded again. The batch is
// then put in order the way the «Fotos» tab does (server/rename-photos.mjs checkBatch): its IDs in the
// order the photos were taken, two photos each (d then v), each photo's turn; what does not
// add up is marked for a look, and only that is shown to the assistant. Applied from its card, it
// becomes a plan of changes on Drive (server/photo-files.mjs: renames with their RAW files, turns,
// removals), made with the card of that plan, after which Photo_links lists the photos.
//
// A batch is a card of Cambios propuestos (server/assistant.mjs, `sync_json` of type 'photos'); its rows
// are in drive_photo_rows, its pictures under the cache folder (DRIVE_PHOTOS_CACHE).

import { spawn } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { createHash } from 'node:crypto';
import { existsSync, mkdirSync, readFileSync, readdirSync, renameSync, rmSync, statSync, statfsSync, utimesSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createInterface } from 'node:readline';
import { FOLDER, driveFor, driveTree, driveWriterFor } from './drive.mjs';
import { driveIdOf, knownId, photoLinkRows, specimenName, specimens } from './photo-links.mjs';
import { IMAGES, checkBatch } from './rename-photos.mjs';
import { attachmentFile } from './proposal-photos.mjs';
import { photoBoxes, startViewer, viewPlan } from './photo-views.mjs';
import { PAGE as REFERENCE_PAGE, collectionLabel, collectionSpecimens, referenceFile, referenceLabel, referencePage, referenceSet, sexOf, speciesOf } from './reference-photos.mjs';

export const DRIVE_PHOTO_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'drive_photos',
      description: [
        "Photos uploaded to a Drive folder without being renamed (the camera's names, IMG_…): the server reads the CAM ID on each envelope with the Rename Photos reader, keeps a small copy, and puts the batch in order (two photos per ID, d then v, in the order taken; each photo turned so its forewings are above its hindwings, by its own wings or envelope when they are sure, else as the other photos of its camera). It shows as a card beside the chat, where the person checks it and presses «Aplicar».",
        '- `folder` (a Drive link, or a folder of Photo_links by its path): starts reading it (about a second a photo; answers at once with the batch). `all: true`: the photos already named too.',
        '- `batch`: how it goes, then the IDs in the order taken (`order`), the counts, the checks (IDs without exactly two photos, IDs out of the order taken, IDs the workbook lacks, IDs with photos listed already, `turnUnsure`: photos with no sure turn of their own whose camera has no clear usual turn either, two photos to look at) and the rows to look at (`toLook`; `look` turn-disagree: its envelope points another way than its sure wings, turned as the wings say; turn-camera: turned by its envelope alone, unlike the other photos of its camera; `turnBy`: wings, envelope or camera). `rows: "all"`: every row.',
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
        "Pictures of photos of a batch of drive_photos: `what: \"envelope\"` (default: the envelope, to read its ID), `\"photo\"` (the whole photo, small: which side, wings, turn) or `\"full\"` (the whole photo, 1600 px). `box` [x0, y0, x1, y1], fractions of the photo as shown (0,0 its top left): that part at full resolution (an envelope hard to read, a wing; 1 to 4 files). `files`: the rows' `file` (up to 12).",
      parameters: {
        type: 'object',
        properties: {
          batch: { type: 'string' },
          files: { type: 'array', items: { type: 'string' } },
          what: { type: 'string', enum: ['envelope', 'photo', 'full'] },
          box: { type: 'array', items: { type: 'number' } },
        },
        required: ['batch', 'files'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'photo_files_look',
      description:
        "Pictures of any photo files on Google Drive, from Drive's resized copies (as the Wings Gallery loads them: quick, not the whole file): what a Photo_links reading or a person points to. `files` (up to 8): a Drive file ID, its link, or a file name or path (with or without its extension: the files of that name in Photo_links, else on Drive, …_todelete ones too; with a path, only those in that folder, the others named in `otherFolders`). Each picture says its real `path`, `sizeKb` and when it was taken; a file asked twice comes once (`sameAs`). `view`, for a specimen's photo: `wings` (the wings only, 640 px), `envelope` (the envelope only, 560 px, turned upright), `both` (the two side by side, 360 px high: the species and sex against what the envelope says), `full` (default: the whole photo); cut with the boxes read from the photo archive, and the whole photo, with `note`, when its box is not known. `size` (the whole photo): 800 (default: the wings, which side, the turn) or 1600 (an envelope's writing). `box` [x0, y0, x1, y1], fractions of the picture (0,0 its top left): that part, from the 1600 px copy.",
      parameters: {
        type: 'object',
        properties: {
          files: { type: 'array', items: { type: 'string' } },
          view: { type: 'string', enum: ['full', 'wings', 'envelope', 'both'] },
          size: { type: 'number', enum: [800, 1600] },
          box: { type: 'array', items: { type: 'number' } },
        },
        required: ['files'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'identify_species',
      description: [
        "The species a butterfly photo shows, by the team's AI Identifier (BioCLIP 2.5-H and the head trained on 15,347 names; since 29 Oct 2026 the alias head head_hier_b8127dbf.pt, same names), run on this server: about 3 s a photo, 10 s for the first after a quiet while (it loads).",
        "- `files` (up to 6): a Drive file ID, link, or name or path as photo_files_look takes them (read from Drive's 1600 px copy); a chat attachment's file name; with `batch`, rows of a drive_photos batch (the whole photo).",
        '- `side`: "East" or "West", the side of the Andes in Ecuador where it was photographed (Ikiam: East): names not recorded there weigh 2 %.',
        '- `box` [x0, y0, x1, y1], fractions of the photo as shown: that butterfly only, for a photo of several; otherwise the wings are found on their own (`crop` says when none were and the centre of the photo was used).',
        "Each photo: `species` and `subspecies` (up to 5, with probabilities, those under 1 % left out), `genus`, and `confidence`. On 303 labelled photos (258 of the team's), the first species was right 89 % of the time (the team's Ithomiini 92 %), within the five 97 %, the first subspecies 94 %; \"high\" (first species at 60 % or more, three photos in four) was right 96 %, \"low\" about two times in three. Those figures were measured with the earlier head (14 Oct); the current one was not scored on these photos (on 2,936 iNaturalist butterflies, with the full-precision encoder, its first species was right 0.24 points less often than the earlier head's, a difference within chance).",
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          files: { type: 'array', items: { type: 'string' } },
          batch: { type: 'string' },
          side: { type: 'string', enum: ['East', 'West'] },
          box: { type: 'array', items: { type: 'number' } },
        },
        required: ['files'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'reference_photos',
      description: [
        "Photos of a species to compare a butterfly with, each picture with a line saying what it is: the collection's own butterflies of that species (dorsal and ventral wings in one picture; those whose species the Wings Gallery's model also gives come first), then field and specimen photos from butterfly sites of Ecuador (sangay.eu, noreste.eu, cotacachi.eu, loja-zamora.eu, a few from amazonian-butterflies.net and neotropicalbutterflies.com).",
        "It helps in the doubtful cases: the identifier unsure of a photo, or naming another species than the workbook; then the photos of both species beside the butterfly's own settle most of them. For the rest, identify_species and the photo itself are enough.",
        '- `species`: a name as the workbook writes it (with its subspecies, those come first); `sex`: "female" or "male"; `cam`: the butterfly in question, left out; `page`: 1, 2… for more.',
        "Species of other countries (Colombia, Brazil…) have no field or specimen photos yet: only the collection's, when it has them.",
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          species: { type: 'string' },
          sex: { type: 'string', enum: ['female', 'male'] },
          cam: { type: 'string' },
          page: { type: 'integer' },
        },
        required: ['species'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'drive_photos_edit',
      description:
        "Corrects rows of a batch of drive_photos (the card shows each change as the assistant's): `changes` [{ file, id (the CAM ID), view (d, v, d2…), remove (true: a photo not wanted, removed when applied), rotation (0, 90, 180, 270 clockwise, from the photo as shown), note (what a person should know) }]. Views are worked out again for the IDs changed unless given. `turnAll` (0, 90, 180, 270): the same turn, from how each shows, for every photo of the batch (when all were shot from one camera setup); one whose orientation tag flipped takes the same turn of the camera's frame.",
      parameters: {
        type: 'object',
        properties: {
          batch: { type: 'string' },
          turnAll: { type: 'number' },
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
        required: ['batch'],
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
const MAX_PHOTOS = 3000;
const DOWNLOADS = 2;
/** The pictures kept, in all: the oldest batches' go first. */
const CACHE_BYTES = 1024 ** 3;
/** Whole photos kept (DRIVE_PHOTOS_ORIGINALS_GB), the least used dropped first; none kept below FREE_BYTES of disk. */
const ORIGINALS_BYTES = 5 * 1024 ** 3;
const FREE_BYTES = 10 * 1024 ** 3;
const KEPT_DAYS = 14;
const LOOK_MAX = 12;
/** drive_photo_look: files a call, names' files each, and the copies kept (the least used dropped first). */
const VIEW_MAX = 8;
const VIEW_PER_NAME = 4;
const VIEWS_BYTES = 300 * 1024 ** 2;
const VIEW_TIMEOUT_MS = 15_000;
/** reference_photos: the height of a collection butterfly's picture (its dorsal and ventral wings side by side). */
const COLLECTION_HEIGHT = 300;
/** identify_species: photos a call (about 3 s each). */
const IDENTIFY_MAX = 6;
/** The first species' probability from which an identification is told as high: right 96 % of the time above, 67 % below (17 Oct 2026, 303 photos). */
const HIGH = 0.6;
/** What the identifier cropped, when it is not the wings it found. */
const CROPS = {
  centre_crop_fallback: 'no wings found: the centre of the photo',
  reviewed_full_image_override: 'the whole photo',
  manual_bbox_square_letterbox: 'the box given',
};
/** Drive's resized copies of a file shared by link (the Wings Gallery's); the first is usually the quicker. */
const driveCopies = (id, size) => [`https://drive.google.com/thumbnail?id=${id}&sz=w${size}`, `https://lh3.googleusercontent.com/d/${id}=w${size}`];
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
 * A batch's rows put in order (they come in the order taken): each ID's views, each photo's turn (its
 * own wings' or envelope's when sure, else its camera's usual one; `turnBy`), and what to look at (`review`). `known`: the
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
  // Each photo's turn (as the Rename Photos app: rotationPlan planTurns). A photo with a sure reading of its
  // own takes it, in any direction: its wings' turn when they are sure (`wingRotation`: 3 wings or more, 90 %
  // sure), else its envelope's (`envelopeRotation`: its CAM ID line read sure, else its whole text). Both sure
  // and disagreeing: the wings (they show the butterfly; an envelope may lie any way beside it), with a look
  // unless that is also the camera's sure usual turn. Only a photo with neither takes its camera's usual turn,
  // in the camera's own frame (the orientation tag ignored: on a copy stand the camera stays put and the tag
  // flips between shots), from the photos with their own that stay landscape with it; kept landscape, and
  // looked at unless 3 photos or more say it, 80 % of them alike. A photo turned by its envelope alone
  // against that usual turn is looked at too.
  const TAG_CW = { 1: 0, 6: 90, 3: 180, 8: 270 };
  const frameWide = r => (r.wide === undefined || TAG_CW[r.orientation ?? 1] === undefined ? null : (TAG_CW[r.orientation ?? 1] % 180 === 0) === r.wide);
  const fits = (turn, wide) => (norm(turn) % 180 === 0) === wide;
  const isTurn = v => v !== null && v !== undefined && Number.isFinite(Number(v));
  // Rows read before the reader gave the envelope's sure turn: their CAM ID line's, read sure, upright or upside down.
  const envelopeOf = r =>
    r.envelopeRotation !== undefined
      ? (isTurn(r.envelopeRotation) ? norm(r.envelopeRotation) : null)
      : isTurn(r.readRotation) && (r.conf ?? 0) >= 0.95 && frameWide(r) !== null && fits(TAG_CW[r.orientation ?? 1] + r.readRotation, frameWide(r)) ? norm(r.readRotation) : null;
  const cameras = new Map();
  for (const r of live) (cameras.get(r.camera ?? '') ?? cameras.set(r.camera ?? '', []).get(r.camera ?? '')).push(r);
  for (const group of cameras.values()) {
    const own = new Map();
    for (const r of group) {
      if (r.rotationSet) continue;
      const envelope = envelopeOf(r);
      const wings = isTurn(r.wingRotation) ? norm(r.wingRotation) : null;
      if (wings !== null) own.set(r, { cw: wings, by: 'wings', disagree: envelope !== null && envelope !== wings });
      else if (envelope !== null) own.set(r, { cw: envelope, by: 'envelope', disagree: false });
    }
    const votes = new Map();
    for (const [r, o] of own) {
      const wide = frameWide(r);
      if (wide === null) continue;
      const turn = norm(TAG_CW[r.orientation ?? 1] + o.cw);
      if (fits(turn, wide)) votes.set(turn, (votes.get(turn) ?? 0) + 1);
    }
    const total = [...votes.values()].reduce((a, b) => a + b, 0);
    const [best, most] = [...votes].sort((a, b) => b[1] - a[1])[0] ?? [0, 0];
    const sure = total >= 3 && most / total >= 0.8;
    const usual = r => {
      const wide = frameWide(r);
      return wide === null ? null : norm((fits(best, wide) ? best : best + 90) - TAG_CW[r.orientation ?? 1]);
    };
    for (const r of group) {
      r.turnUnsure = false;
      r.turnLook = null;
      if (r.rotationSet) continue;
      const o = own.get(r);
      if (o) {
        const u = sure ? usual(r) : null;
        r.rotation = o.cw;
        r.turnBy = o.by;
        r.turnLook = o.disagree && u !== o.cw ? 'turn-disagree' : o.by === 'envelope' && u !== null && u !== o.cw ? 'turn-camera' : null;
        continue;
      }
      const u = usual(r);
      if (u === null) continue;
      r.rotation = u;
      r.turnBy = 'camera';
      r.turnUnsure = !sure;
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
      ...(r.turnUnsure ? ['turn-unsure'] : []),
      ...(r.turnLook ? [r.turnLook] : []),
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
 * `identifier`: the AI Identifier's process (server/identifier.mjs), for identify_species. `viewer`: what
 * makes the views of a photo (server/photo-views.mjs startViewer; a fake for tests).
 */
export function createDrivePhotos({ store, config = {}, readerDir = null, cacheDir = null, organize = null, show = null, tell = null, reader = null, viewer = null, identifier = null, waitMs = WAIT_MS, freeBytes = FREE_BYTES, fetchImpl = fetch }) {
  const db = store.db;
  db.exec(`CREATE TABLE IF NOT EXISTS drive_photo_batches (id TEXT PRIMARY KEY, owner TEXT NOT NULL, folder_id TEXT NOT NULL, folder TEXT NOT NULL,
    status TEXT NOT NULL, created_at TEXT NOT NULL, updated_at TEXT NOT NULL, summary_json TEXT NOT NULL DEFAULT '{}', plan TEXT, error TEXT)`);
  db.exec(`CREATE TABLE IF NOT EXISTS drive_photo_rows (batch_id TEXT NOT NULL, file TEXT NOT NULL, n INTEGER NOT NULL, row_json TEXT NOT NULL,
    PRIMARY KEY (batch_id, file))`);
  const cache = cacheDir ?? config.drivePhotosCache ?? join(tmpdir(), 'ithomiini-drive-photos');
  mkdirSync(cache, { recursive: true, mode: 0o700 });
  const originals = join(cache, 'originals');
  mkdirSync(originals, { recursive: true, mode: 0o700 });
  const originalsBytes = config.drivePhotosOriginalsGb !== undefined ? Number(config.drivePhotosOriginalsGb) * 1024 ** 3 : ORIGINALS_BYTES;
  const originalOf = file => join(originals, `${String(file).replace(/[^\w-]/g, '')}.jpg`);
  /** Whether a whole photo may be kept: the cache allows some, and the disk has room. */
  const roomForOriginals = () => {
    if (!(originalsBytes > 0)) return false;
    try {
      const fs = statfsSync(originals);
      return fs.bavail * fs.bsize > freeBytes;
    } catch {
      return false;
    }
  };
  /** The whole photos beyond the space allowed: the least recently used go. */
  function trimOriginals() {
    const files = readdirSync(originals)
      .filter(f => f.endsWith('.jpg'))
      .map(f => {
        const s = statSync(join(originals, f));
        return { f, size: s.size, at: s.mtimeMs };
      })
      .sort((a, b) => a.at - b.at);
    let total = files.reduce((n, x) => n + x.size, 0);
    for (const x of files) {
      if (total <= originalsBytes) break;
      rmSync(join(originals, x.f), { force: true });
      total -= x.size;
    }
  }
  /** A photo's whole file on this server: the one kept (marked used), or downloaded again. */
  async function original(file) {
    const path = originalOf(file);
    if (existsSync(path)) {
      const now = new Date();
      utimesSync(path, now, now);
      return path;
    }
    const writer = driveWriterFor(store, config);
    if (!writer?.download) throw new Error('This app has no Google account set up that can read the photos.');
    await writer.download(file, `${path}.part`);
    renameSync(`${path}.part`, path);
    trimOriginals();
    return path;
  }
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
  function startReader(knownFile = null) {
    if (reader) return reader;
    const dir = readerDir ?? config.photoReaderDir;
    if (!dir || !existsSync(join(dir, 'photo-reader.mjs'))) throw new Error('This server has no photo reader installed (photo-reader/: scripts/build-rename-app.sh).');
    const args = [join(dir, 'photo-reader.mjs'), '--models', join(dir, 'models'), ...(knownFile ? ['--known', knownFile] : []), '--threads', '2', '--wings'];
    const child = spawn(process.execPath, args, {
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
          // Downloaded where whole photos are kept; deleted after reading when there is no room for it.
          const path = originalOf(r.file);
          const keep = roomForOriginals();
          try {
            if (!existsSync(path)) {
              await writer.download(r.file, `${path}.part`);
              renameSync(`${path}.part`, path);
            }
            const out = await read.read({ id: r.file, path, preview: picture(id, r.file, 'photo'), crop: picture(id, r.file, 'envelope') });
            if (out.error) throw new Error(out.error);
            Object.assign(r, {
              read: true,
              // An ID the workbook lacks is kept, and marked (a butterfly not typed yet, or a misreading).
              camid: out.camid ?? null,
              readAs: out.camid ?? null,
              conf: out.conf,
              ...(out.idLine ? { idLine: out.idLine } : {}),
              candidates: (out.candidates ?? []).map(c => c.id),
              readRotation: out.rotation === null || out.rotation === undefined ? null : norm(out.rotation),
              // the envelope's turn when the reading is sure of it, and the wings' (absent: a reader before them)
              ...(out.envelopeRotation !== undefined ? { envelopeRotation: out.envelopeRotation === null ? null : norm(out.envelopeRotation) } : {}),
              ...(out.wingRotation !== undefined ? { wingRotation: out.wingRotation === null ? null : norm(out.wingRotation) } : {}),
              ...(out.wings ? { wings: out.wings } : {}),
              orientation: out.orientation,
              ...(out.width && out.height ? { wide: out.width >= out.height } : {}),
              setBy: 'reader',
            });
          } catch (e) {
            Object.assign(r, { read: true, camid: null, error: String(e.message || e).slice(0, 200), setBy: 'reader', readRotation: null });
          } finally {
            rmSync(`${path}.part`, { force: true });
            if (!keep) rmSync(path, { force: true });
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
    trimOriginals();
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
        camera: [f.imageMediaMetadata?.cameraMake, f.imageMediaMetadata?.cameraModel].filter(Boolean).join(' ').trim(),
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
      turnBy: r.rotationSet ? null : r.turnBy,
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

  async function look({ batch, files, what = 'envelope', box = null }) {
    const b = batchOf(batch);
    if (!b) return { error: 'No batch by that ID.' };
    const rows = new Map(rowsOf(batch).map(r => [r.file, r]));
    const out = { batch, pictures: [] };
    const images = [];
    // A part of the whole photo, or all of it larger: cut from the photo kept (downloaded again if dropped).
    const whole = what === 'full' || (Array.isArray(box) && box.length === 4);
    const cutter = whole ? startReader() : null;
    try {
      for (const file of (Array.isArray(files) ? files : [files]).slice(0, whole ? 4 : LOOK_MAX)) {
        const r = rows.get(String(file)) ?? [...rows.values()].find(x => x.name === file);
        if (!r) {
          out.pictures.push({ file, missing: 'no such row' });
          continue;
        }
        let path = picture(batch, r.file, what === 'photo' ? 'photo' : 'envelope');
        if (whole) {
          const area = Array.isArray(box) && box.length === 4 ? box.map(Number) : [0, 0, 1, 1];
          path = join(cache, batch, `${r.file}-z${createHash('sha1').update(JSON.stringify(area)).digest('hex').slice(0, 10)}.jpg`);
          if (!existsSync(path)) {
            const cut = await cutter.read({ id: `${r.file}-cut`, op: 'cut', path: await original(r.file), box: area, out: path, side: 1600 });
            if (cut.error) {
              out.pictures.push({ file, missing: cut.error });
              continue;
            }
          }
        }
        if (!existsSync(path)) {
          out.pictures.push({ file, missing: `no ${what} picture kept` });
          continue;
        }
        out.pictures.push(filled({ file: r.file, name: r.name, id: r.camid, view: r.view, box: whole ? (box ?? 'whole') : null }));
        images.push({ data: readFileSync(path).toString('base64'), mimeType: 'image/jpeg' });
      }
    } finally {
      if (cutter && cutter !== reader) cutter.close?.();
    }
    return Object.assign(out, { [IMAGES]: images });
  }

  // ------------------------------------------------------------ any photo on Drive
  const views = join(cache, 'views');
  /** The copies kept beyond their space: the least recently used go. */
  function trimViews() {
    const files = readdirSync(views)
      .map(f => ({ f, ...statSync(join(views, f)) }))
      .sort((a, b) => a.mtimeMs - b.mtimeMs);
    let total = files.reduce((n, x) => n + x.size, 0);
    for (const x of files) {
      if (total <= VIEWS_BYTES) break;
      rmSync(join(views, x.f), { force: true });
      total -= x.size;
    }
  }
  /**
   * The files a name, path, link or ID stands for: [{ id, name }]. A name (or a path: its folders
   * put the files there first) is looked up in Photo_links, else on Drive.
   */
  /**
   * The files a name or path points to: { found: [{ id, name, row, path, sizeKb, taken }], elsewhere }.
   * A path gives only the files in its folder (Photo_links, else Drive itself), never one of the same
   * name elsewhere: those are told in `elsewhere`. A file renamed …_todelete is found by its name too.
   */
  async function filesNamed(given) {
    const id = driveIdOf(given) ?? (/^[\w-]{25,}$/.test(given) ? given : null);
    if (id) return { found: [{ id, name: null }] };
    const cut = given.lastIndexOf('/');
    const folder = cut >= 0 ? given.slice(0, cut).toLowerCase().replace(/^\/+|\/+$/g, '') : '';
    const base = given.slice(cut + 1);
    const wanted = stem(base);
    const exact = /\.[a-z0-9]{2,4}$/i.test(base) ? base.toLowerCase() : null;
    const plain = name => String(name ?? '').toLowerCase().replace(/_todelete$/, '');
    const matches = name => (exact ? plain(name) === exact : stem(plain(name)) === wanted);
    const dirOf = path => String(path ?? '').split('/').slice(0, -1).join('/').toLowerCase();
    const inFolder = path => !folder || dirOf(path) === folder || dirOf(path).endsWith(`/${folder}`);
    const listed = photoLinkRows(store)
      .filter(r => matches(r.values.Name))
      .map(r => ({ id: driveIdOf(r.values.URL), name: r.values.Name, row: r.row, path: r.values['Full Path'], sizeKb: r.values['Size KB'], taken: r.values['Capture Date'] }))
      .filter(f => f.id);
    const here = listed.filter(f => inFolder(f.path));
    if (here.length) return { found: here.slice(0, VIEW_PER_NAME) };
    // Not listed there: Drive itself (the files of that name, …_todelete ones too), in that folder.
    const drive = driveFor(store, config);
    const onDrive = [];
    if (drive?.named) {
      if (exact) for (const n of [base, `${base}_todelete`]) onDrive.push(...((await drive.named(n)) ?? []));
      else onDrive.push(...((await drive.named(null, base)) ?? []));
    }
    const kb = f => (f.size ? Math.round((Number(f.size) / 1024) * 10) / 10 : undefined);
    if (!folder) {
      const ids = new Set(listed.map(f => f.id));
      return { found: [...listed, ...onDrive.filter(f => !ids.has(f.id)).map(f => ({ id: f.id, name: f.name, sizeKb: kb(f) }))].slice(0, VIEW_PER_NAME) };
    }
    const leaf = folder.split('/').pop();
    const parents = new Map();
    for (const f of onDrive)
      for (const p of f.parents ?? []) if (!parents.has(p)) parents.set(p, String((await drive.get(p).catch(() => null))?.name ?? '').toLowerCase());
    const there = onDrive.filter(f => (f.parents ?? []).some(p => parents.get(p) === leaf));
    if (there.length) return { found: there.slice(0, VIEW_PER_NAME).map(f => ({ id: f.id, name: f.name, path: `${given.slice(0, cut)}/${f.name}`, sizeKb: kb(f) })) };
    const elsewhere = [...new Set([...listed.map(f => f.path), ...onDrive.map(f => `${(f.parents ?? []).map(p => parents.get(p)).filter(Boolean).join(', ') || '?'}/${f.name}`)])];
    return { found: [], elsewhere: elsewhere.slice(0, 8) };
  }
  /** A file's resized copy on this server: Drive's own, else (not shared by link) made from the whole file. */
  async function viewOf(id, size) {
    mkdirSync(views, { recursive: true, mode: 0o700 });
    const path = join(views, `${id.replace(/[^\w-]/g, '')}-${size}.jpg`);
    if (existsSync(path)) {
      const now = new Date();
      utimesSync(path, now, now);
      return path;
    }
    for (const url of driveCopies(id, size)) {
      try {
        const response = await fetchImpl(url, { redirect: 'follow', signal: AbortSignal.timeout(VIEW_TIMEOUT_MS) });
        // A file not shared by link answers with Google's sign-in page.
        if (!response.ok || !/^image\//.test(response.headers.get('content-type') ?? '')) continue;
        writeFileSync(path, Buffer.from(await response.arrayBuffer()), { mode: 0o600 });
        trimViews();
        return path;
      } catch {
        // the other address, or the whole file
      }
    }
    const cutter = startReader();
    try {
      const cut = await cutter.read({ id: `${id}-view`, op: 'cut', path: await original(id), box: [0, 0, 1, 1], out: path, side: size });
      if (cut.error) throw new Error(cut.error);
    } finally {
      if (cutter !== reader) cutter.close?.();
    }
    trimViews();
    return path;
  }
  /**
   * A picture made by the views' process (server/photo-views.mjs), kept with the copies under a name of
   * what it shows: { path } or { error, message }. `session.viewer`: the process, started with the first.
   */
  async function madePicture(session, key, job) {
    mkdirSync(views, { recursive: true, mode: 0o700 });
    const path = join(views, `${key.replace(/[^\w-]/g, '')}-${createHash('sha1').update(JSON.stringify(job)).digest('hex').slice(0, 10)}.jpg`);
    if (existsSync(path)) {
      const now = new Date();
      utimesSync(path, now, now);
      return { path };
    }
    try {
      session.viewer ??= viewer ?? startViewer(readerDir ?? config.photoReaderDir);
    } catch (e) {
      return { error: 'no viewer', message: String(e.message || e) };
    }
    const made = await session.viewer.make({ ...job, out: path });
    if (made.error) return { error: made.error, message: made.error };
    trimViews();
    return { path };
  }
  const closeViews = session => session.viewer && session.viewer !== viewer && session.viewer.close?.();

  async function lookAtDrive({ files, size = 800, box = null, view = 'full' }) {
    const side = Number(size) >= 1200 ? 1600 : 800;
    const area = Array.isArray(box) && box.length === 4 ? box.map(Number) : null;
    // A view of a specimen's photo (a `box` takes its place).
    const wanted = !area && ['wings', 'envelope', 'both'].includes(view) ? view : null;
    const out = { pictures: [] };
    const images = [];
    const seen = new Map();
    const session = {};
    let cutter = null;
    try {
      for (const given of (Array.isArray(files) ? files : [files]).map(text).filter(Boolean).slice(0, VIEW_MAX)) {
        const named = await filesNamed(given).catch(e => (out.pictures.push({ file: given, missing: String(e.message || e) }), null));
        if (!named) continue;
        const found = named.found;
        if (!found.length)
          out.pictures.push(
            filled({
              file: given,
              missing: named.elsewhere?.length ? 'no file of that name in that folder (Photo_links and Drive)' : 'no file of that name in Photo_links or on Drive',
              otherFolders: named.elsewhere,
            }),
          );
        for (const f of found) {
          // the same file asked twice in a call (two names or paths for it): one picture
          if (seen.has(f.id)) {
            out.pictures.push({ file: given, sameAs: seen.get(f.id) });
            continue;
          }
          seen.set(f.id, given);
          if (images.length >= VIEW_MAX) break;
          try {
            let path = null;
            let shown = null;
            let note = null;
            if (wanted) {
              const plan = viewPlan(wanted, photoBoxes(store, { id: f.id, name: f.name }));
              if (plan.parts) {
                const src = await viewOf(f.id, 1600);
                const made = await madePicture(session, `${f.id}-${wanted}`, { parts: plan.parts.map(p => ({ ...p, src })), side: plan.side, height: plan.height, wide: plan.wide });
                if (made.path) [path, shown] = [made.path, wanted];
                else if (made.error === 'shape') note = 'the photo was turned after its boxes were read: the whole photo';
                else if (made.error === 'no viewer') note = `${made.message} The whole photo.`;
                else throw new Error(made.message);
              } else note = `${plan.missing}: the whole photo`;
            }
            path ??= await viewOf(f.id, area ? 1600 : side);
            if (area) {
              const part = join(views, `${f.id.replace(/[^\w-]/g, '')}-z${createHash('sha1').update(JSON.stringify(area)).digest('hex').slice(0, 10)}.jpg`);
              if (!existsSync(part)) {
                cutter ??= startReader();
                const cut = await cutter.read({ id: `${f.id}-part`, op: 'cut', path, box: area, out: part, side: 1600 });
                if (cut.error) throw new Error(cut.error);
              }
              path = part;
            }
            out.pictures.push(
              filled({ file: given, id: f.id, name: f.name, path: f.path, sizeKb: f.sizeKb, taken: f.taken, photoLinksRow: f.row, link: `https://drive.google.com/file/d/${f.id}/view`, box: area, view: shown, note }),
            );
            images.push({ data: readFileSync(path).toString('base64'), mimeType: 'image/jpeg' });
          } catch (e) {
            out.pictures.push(filled({ file: given, id: f.id, name: f.name, missing: String(e.message || e).slice(0, 200) }));
          }
        }
      }
    } finally {
      if (cutter && cutter !== reader) cutter.close?.();
      closeViews(session);
    }
    return Object.assign(out, { [IMAGES]: images });
  }

  // ------------------------------------------------------------ photos of a species to compare with
  /**
   * reference_photos: a page of the collection's butterflies of a species (dorsal and ventral wings in
   * one picture, from Drive's 800 px copies) and of the reference set's field and specimen photos, each
   * picture with one line saying what it is.
   */
  async function references({ species, sex = null, cam = null, page = 1 }) {
    const asked = speciesOf(species);
    if (!asked) return { error: '`species`: a genus and its species, e.g. "Mechanitis lysimnia" (its subspecies after them).' };
    const wantedSex = sex ? sexOf(sex) : null;
    const n = Math.max(1, Math.floor(Number(page) || 1)) - 1;
    const out = { species: [asked.binomial, asked.subspecies].filter(Boolean).join(' '), ...(wantedSex ? { sex: wantedSex } : {}), pictures: [] };
    const images = [];
    const session = {};
    const label = line => out.pictures.push(`${images.length}. ${line}`);
    try {
      // The collection's.
      const specimens = collectionSpecimens(store, asked, { sex: wantedSex, not: cam });
      for (const s of specimens.slice(n * REFERENCE_PAGE.collection, (n + 1) * REFERENCE_PAGE.collection)) {
        try {
          const parts = [];
          for (const fileId of [s.dorsal, s.ventral]) {
            const plan = viewPlan('wings', photoBoxes(store, { id: fileId }));
            parts.push({ src: await viewOf(fileId, 800), box: plan.parts?.[0].box ?? [0, 0, 1, 1] });
          }
          const made = await madePicture(session, `${s.cam}-dv`, { parts, height: COLLECTION_HEIGHT });
          if (!made.path) throw new Error(made.message);
          images.push({ data: readFileSync(made.path).toString('base64'), mimeType: 'image/jpeg' });
          label(collectionLabel(s));
        } catch (e) {
          out.pictures.push(`collection ${s.cam}: no picture (${String(e.message || e).slice(0, 120)})`);
        }
      }
      // The field and specimen photos.
      const set = referenceSet(config.referencePhotosDir);
      const found = set ? referencePage(set, asked, { sex: wantedSex, page: n }) : null;
      for (const entry of [...(found?.field ?? []), ...(found?.specimen ?? [])]) {
        const path = referenceFile(set, entry);
        if (!path) continue;
        images.push({ data: readFileSync(path).toString('base64'), mimeType: 'image/jpeg' });
        label(referenceLabel(entry));
      }
      out.counts = { collection: specimens.length, field: found?.total.field ?? 0, specimen: found?.total.specimen ?? 0 };
      if (!set) out.reference = "The field and specimen photos are not installed on this server: only the collection's.";
      else if (!found.name)
        Object.assign(out, {
          reference: `No field or specimen photos of ${asked.binomial}: the set holds the species of Ecuador's butterfly sites; species of other countries (Colombia, Brazil…) have none yet.`,
          ...(found.sameGenus.length ? { sameGenus: found.sameGenus } : {}),
        });
      if (!specimens.length) out.collection = `No butterfly of ${asked.binomial}${wantedSex ? ` (${wantedSex})` : ''} in the workbook with both photos in Photo_links.`;
      const more = (n + 1) * REFERENCE_PAGE.collection < specimens.length || (n + 1) * REFERENCE_PAGE.field < out.counts.field || (n + 1) * REFERENCE_PAGE.specimen < out.counts.specimen;
      if (more) out.more = `page ${n + 2}`;
    } finally {
      closeViews(session);
    }
    return Object.assign(out, { [IMAGES]: images });
  }

  // ------------------------------------------------------------ the species in a photo
  /**
   * identify_species: each photo (a file on Drive, a chat's attachment, a batch's row) put on this
   * server, then read by the identifier one after the other; its lists cut to what counts.
   */
  async function identify({ files, batch = null, side = null, box = null }) {
    if (!identifier) return { error: 'The identifier is not set up on this server.' };
    const rows = batch ? batchOf(batch) && new Map(rowsOf(batch).map(r => [r.file, r])) : null;
    if (batch && !rows) return { error: 'No batch by that ID.' };
    const where = /^east$/i.test(side ?? '') ? 'East' : /^west$/i.test(side ?? '') ? 'West' : null;
    const area = Array.isArray(box) && box.length === 4 ? box.map(Number) : null;
    if (box !== null && box !== undefined && !(area?.every(v => v >= 0 && v <= 1) && area[0] < area[2] && area[1] < area[3]))
      return { error: '`box` is [x0, y0, x1, y1], fractions of the photo (0 to 1), x0 < x1 and y0 < y1.' };
    const listed = list => {
      const kept = list.filter(([, p], i) => i === 0 || p >= 0.01).map(([name, p]) => [name, Math.round(p * 100) / 100]);
      return kept.length ? kept : undefined;
    };
    const out = { photos: [] };
    const seen = new Map();
    for (const given of (Array.isArray(files) ? files : [files]).map(text).filter(Boolean).slice(0, IDENTIFY_MAX)) {
      // [{ about: what the answer says of the file, key, local(): its path on this server }]
      let targets = [];
      try {
        if (rows) {
          const r = rows.get(given) ?? [...rows.values()].find(x => x.name === given);
          if (r) targets = [{ about: { file: r.file, name: r.name, id: r.camid, view: r.view }, key: r.file, local: () => original(r.file) }];
          else out.photos.push({ file: given, missing: 'no such row' });
        } else {
          const attached = attachmentFile(config.t3?.home, given);
          if (attached) targets = [{ about: { file: given }, key: attached.path, local: async () => attached.path }];
          else {
            const named = await filesNamed(given);
            if (!named.found.length)
              out.photos.push(
                filled({
                  file: given,
                  missing: named.elsewhere?.length ? 'no file of that name in that folder (Photo_links and Drive)' : 'no file of that name in Photo_links or on Drive',
                  otherFolders: named.elsewhere,
                }),
              );
            targets = named.found.map(f => ({
              about: { file: given, name: f.name, path: f.path, link: `https://drive.google.com/file/d/${f.id}/view` },
              key: f.id,
              local: () => viewOf(f.id, 1600),
            }));
          }
        }
      } catch (e) {
        out.photos.push({ file: given, missing: String(e.message || e).slice(0, 200) });
        continue;
      }
      for (const t of targets) {
        if (seen.has(t.key)) {
          out.photos.push({ file: given, sameAs: seen.get(t.key) });
          continue;
        }
        seen.set(t.key, given);
        try {
          const r = await identifier.identify({ path: await t.local(), side: where, box: area });
          if (r.error) {
            out.photos.push(filled({ ...t.about, error: r.error }));
            continue;
          }
          const first = r.species?.[0]?.[1] ?? 0;
          out.photos.push(
            filled({
              ...t.about,
              species: listed(r.species ?? []),
              subspecies: listed(r.subspecies ?? []),
              genus: listed(r.genus ?? []),
              confidence: first >= HIGH ? 'high' : 'low',
              crop: CROPS[r.crop] ?? (r.crop === 'success' ? null : r.crop),
              side: r.prior,
            }),
          );
        } catch (e) {
          out.photos.push(filled({ ...t.about, error: String(e.message || e).slice(0, 200) }));
        }
      }
    }
    return out;
  }

  /** Changes to rows (`by`: 'assistant' or 'person'); the batch put in order again. */
  function edit({ batch, changes = [], turnAll }, by) {
    const b = batchOf(batch);
    if (!b) return { error: 'No batch by that ID.' };
    if (b.status !== 'ready') return { error: b.status === 'reading' ? 'Still reading: wait for it.' : `The batch is ${b.status}.` };
    const rows = new Map(rowsOf(batch).map(r => [r.file, r]));
    const problems = [];
    let changed = 0;
    // One turn for all, as the photos with the batch's usual orientation tag show; one whose tag flipped
    // takes the same turn of the camera's frame.
    if (turnAll !== undefined && turnAll !== null) {
      const cw = r => ({ 1: 0, 6: 90, 3: 180, 8: 270 })[r.orientation ?? 1] ?? 0;
      const tally = new Map();
      for (const r of rows.values()) if (r.read) tally.set(cw(r), (tally.get(cw(r)) ?? 0) + 1);
      const usual = [...tally].sort((a, b) => b[1] - a[1])[0]?.[0] ?? 0;
      for (const r of rows.values()) {
        if (!r.read) continue;
        Object.assign(r, { rotation: norm(Number(turnAll) + usual - cw(r)), rotationSet: true, setBy: by });
        saveRow(batch, r);
        changed++;
      }
    }
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
      // It only reads: anyone in a chat.
      if (name === 'identify_species') return await identify(args).catch(e => ({ error: String(e.message || e) }));
      if (name === 'reference_photos') return await references(args).catch(e => ({ error: String(e.message || e) }));
      if (!EDITORS.includes(user?.role)) return { error: 'Your role cannot change the photos.' };
      try {
        if (name === 'drive_photos' && args.folder && !args.batch) return await start({ folder: args.folder, all: args.all === true }, user, context);
        // Being read: the answer waits for it to end, a minute and a half at most.
        if (name === 'drive_photos' && runningId === String(args.batch) && running)
          await Promise.race([running, new Promise(resolve => setTimeout(resolve, waitMs).unref?.())]);
        if (name === 'drive_photos') return status(args.batch, args);
        if (name === 'drive_photos_look') return await look(args);
        if (name === 'photo_files_look') return await lookAtDrive(args);
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
