// The assistant's tool for Photo_links (sync_photo_links): reads the photo folders on Google
// Drive, says what the sheet lacks and what looks wrong in the new photos (server/photo-links.mjs),
// shows it beside the chat as a card of Cambios propuestos (server/assistant.mjs keeps the card), and
// writes it once the person agrees, in the chat or with the card's «Aplicar», as ordinary saves
// (server/batch.mjs: each in Historial, each undoable), telling the card how far it is.
// Only the app's thread runs it (server/assistant-host.mjs MAIN_TOOLS): it reads Google and writes the sheet.

import { randomUUID } from 'node:crypto';
import { applyBatch } from './batch.mjs';
import { driveFor, driveWriterFor } from './drive.mjs';
import { SHEET, driveIdOf, photoLinkRows, scanPhotoLinks } from './photo-links.mjs';
import { describeChanges, kindOf, planFileChanges, runFileChanges, settleChanges, undoFileChanges } from './photo-files.mjs';

// ------------------------------------------------------------ the tools
export const PHOTO_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'sync_photo_links',
      description: [
        "Bring the Photo_links sheet up to date with the photo folders on Google Drive, so Photo_dorsal and Photo_ventral find the photos uploaded since (what the sheet's menu «📷 Photo Database Tools» does).",
        '- Without `confirmed`: reads Drive (up to about half a minute) and answers what would change; nothing is written. It also shows beside the chat (`link`), with every row and an «Apply» button. Tell the person what matters in it:',
        '  - `changes`: new files per listed folder; rows whose file was renamed, replaced by one of the same name (`relinked`) or deleted (`removed`: the row is emptied).',
        "  - `newPhotos`: of the new photos (RAW files apart), `linked` are named <ID>d.JPG or <ID>v.JPG with an ID of the workbook, which is what the sheets look up. The rest are things to look at, each with examples: `rename` (found under the name in `to`), `unknownId` (the workbook lacks the ID; `maybe`: likely the ID meant), `unnamed` (the camera's name), `outOfOrder` (the ID is far from those of the photos taken just before and after, `around`: how a misread ID looks), `otherFolder` (the same ID has photos in another folder, `other`: another photo, or the same one filed twice, `copy`), `unpaired` (the other view is missing), `beforeCollection` (taken before the butterfly was collected), `otherCountry` (a butterfly of another country than its folder's).",
        '  - `sameName`: files not listed because a listed file already has their name: `copies` of that photo, or `different` photos (one of the two is misnamed, or the ID was used twice).',
        '  - `notListed`: folders with specimen photos beside the listed ones. `leftOut`: files named …_todelete and folders named …_temp, which are never listed.',
        '- `only`: just these folders (a `byFolder` name, or the folder above them), which is quicker: `["Guyana_2025"]`. `add`: folders to start listing, by their `notListed` name or a Drive link.',
        "- `confirmed: true` with that answer's `scan`, only after the person's latest message approves it: writes it, some hundred rows per save (`skip`: folders of `byFolder` to leave for later). A long one answers `writing`: call again with the same arguments to follow it.",
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          only: { type: 'array', items: { type: 'string' } },
          add: { type: 'array', items: { type: 'string' } },
          scan: { type: 'string' },
          confirmed: { type: 'boolean' },
          skip: { type: 'array', items: { type: 'string' } },
        },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'organize_photos',
      description: [
        "Put the photo folders on Google Drive in order with the project's Google account: rename files, move them to another folder, remove them, make folders, gather shortcuts to a set of photos. Photo_links then follows the files changed (their rows renamed, linked to the file kept, or emptied).",
        '- Without `confirmed`: works the plan out and shows it beside the chat (`link`), every change listed; nothing is changed. Tell the person what it would do and what was left out (`leftOut`).',
        '- `operations`: {action: "rename", file, name} · {action: "move", file, to} (a file or a folder into the folder `to`) · {action: "remove", file} · {action: "folder", name, in} · {action: "shortcut", ids, to} (shortcuts to the dorsal and ventral photos of those specimens, e.g. to share a project\'s photos). `file` and the folders: a Drive link, a path ("PERU_2024/JPG/CAM077740d.JPG") or a name no other has; a folder this plan makes can be named in its later operations.',
        '- `fix`: what a check of sync_photo_links calls for, in the folder `in` and inside it: {check: "rename", in} every photo whose name the sheets do not find takes the name they look up · {check: "copies", in} every file of `in` that is also, the same file, in another listed folder is removed · {check: "copied", in} every …_todelete file whose copy is beside it is binned.',
        "- A photo renamed or removed takes its RAW file with it (`withRaw: false`: not). Removing bins the files the project's account owns (30 days in Drive's bin); Drive lets nobody bin someone else's file, so those are renamed …_todelete (out of Photo_links, for their owner to delete).",
        "- `confirmed: true` with that answer's `plan`, only after the person's latest message approves it: makes the changes. A long one answers `writing`: call again with the same arguments. `undo` with a plan's ID takes its changes back.",
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          operations: {
            type: 'array',
            items: {
              type: 'object',
              properties: {
                action: { type: 'string', enum: ['rename', 'move', 'remove', 'folder', 'shortcut'] },
                file: { type: 'string' },
                name: { type: 'string' },
                to: { type: 'string' },
                in: { type: 'string' },
                ids: { type: 'array', items: { type: 'string' } },
              },
              required: ['action'],
            },
          },
          fix: {
            type: 'array',
            items: {
              type: 'object',
              properties: { check: { type: 'string', enum: ['rename', 'copies', 'copied'] }, in: { type: 'string' } },
              required: ['check'],
            },
          },
          withRaw: { type: 'boolean' },
          plan: { type: 'string' },
          confirmed: { type: 'boolean' },
          undo: { type: 'string' },
        },
      },
    },
  },
];
export const PHOTO_TOOL_NAMES = new Set(PHOTO_TOOLS.map(t => t.function.name));

const EDITORS = ['editor', 'reviewer', 'admin'];
const EXAMPLES = 6;
/** Rows per save (server/batch.mjs takes 500 at most). */
const CHUNK = 400;
/** A reading older than this is made again before writing: the folders may have changed. */
const FRESH_MS = 30 * 60_000;
/** How long a call waits for the writing before answering `writing`. */
const WAIT_MS = 100_000;
/** Readings kept (each holds its rows). */
const KEPT = 6;
/** What a reading of every folder knows of the files is used again this long (organize_photos). */
const INDEX_MS = 10 * 60_000;
const FILES_TITLE = 'Google Drive: photo files';

const filled = object =>
  Object.fromEntries(
    Object.entries(object).filter(
      ([, v]) => !(v === 0 || v === null || v === undefined || v === false || (typeof v === 'object' && !Object.keys(v).length)),
    ),
  );
const list = value => (Array.isArray(value) ? value : value ? [value] : []).map(a => String(a).slice(0, 300)).slice(0, 40);

/** A reading as its card keeps it (and, without what only the card needs, as the model gets it). */
export function cardOf(scan, { add = [], only = [] } = {}) {
  const s = scan.summary;
  return {
    add,
    only,
    onlyFolders: scan.only,
    read: { at: new Date().toISOString(), folders: scan.read.folders, files: scan.read.files, seconds: scan.read.seconds, rows: scan.read.rows },
    total: scan.edits.length + scan.creates.length,
    newFiles: s.newFiles,
    newFolders: s.newFolders,
    byFolder: s.byFolder,
    changes: {
      renamed: s.renamed.length,
      relinked: s.relinked.length,
      removed: s.removed.length,
      moved: s.moved,
      details: s.details,
      statusRows: s.statusRows,
    },
    examples: { renamed: s.renamed.slice(0, EXAMPLES), relinked: s.relinked.slice(0, EXAMPLES), removed: s.removed.slice(0, EXAMPLES) },
    checks: s.names?.photos ? s.names : null,
    sameName: s.sameName,
    notListed: s.notListed.map(({ id: _id, ...rest }) => rest),
    leftOut: { toDelete: s.leftOut, temp: s.setAside },
    untouched: { outside: s.outside + s.elsewhere, unchecked: s.unchecked, repeated: s.repeated, unread: s.unread },
    notFound: scan.notFound,
  };
}

/** A card as the model gets it. */
function brief(id, card, shown = {}) {
  const c = card.changes;
  const count = (n, examples) => (n > examples.length ? { count: n, examples } : examples);
  return filled({
    scan: id,
    ...shown,
    drive: { folders: card.read.folders, files: card.read.files, seconds: card.read.seconds },
    sheetRows: card.read.rows,
    onlyFolders: card.onlyFolders,
    upToDate: card.total ? null : true,
    changes: card.total
      ? filled({
          newFiles: card.newFiles,
          newFolders: card.newFolders,
          byFolder: card.byFolder,
          renamed: count(c.renamed, card.examples.renamed),
          relinked: count(c.relinked, card.examples.relinked),
          removed: count(c.removed, card.examples.removed),
          movedInDrive: c.moved,
          ownerOrSizeChanged: c.details,
          scriptStatusRowsCleared: c.statusRows,
        })
      : null,
    newPhotos: card.checks ? filled(Object.fromEntries(Object.entries(card.checks).map(([k, v]) => [k, v?.count === 0 ? null : v]))) : null,
    sameName: card.sameName.copies || card.sameName.different ? card.sameName : null,
    notListed: card.notListed,
    leftOut: filled({ toDeleteFiles: card.leftOut.toDelete, tempFolders: card.leftOut.temp }),
    // Rows left as they are.
    untouched: filled({
      fileNowOutsideTheListedFolders: card.untouched.outside,
      fileNotFoundButUnchecked: card.untouched.unchecked,
      rowsRepeatingAFile: card.untouched.repeated,
      rowsWithoutALink: card.untouched.unread,
    }),
    foldersNotFound: card.notFound,
    next: card.total
      ? `Nothing is written yet. After the person agrees (they can also press «Apply» beside the chat): sync_photo_links with scan "${id}" and confirmed: true. It writes every row of \`changes\`, the photos with something to look at too.`
      : null,
  });
}

/**
 * sync_photo_links for one app.
 *  - run(args, user, context): the tool.
 *  - apply(id, user, { skip }), refresh(id, params, user), rows(id): the card's «Aplicar», «Leer de nuevo» and its table.
 * `show({ id, card }, context)` keeps the card of a reading (`id`: the card it replaces) and answers
 * { id, …what the model is told, its link }; `tell(id, progress)` is called as the writing goes.
 * `drive` (in config), `waitMs`, `chunk`: for tests.
 */
export function createPhotoLinksTool({ store, config = {}, waitMs = WAIT_MS, chunk = CHUNK, show = null, tell = null }) {
  /** The readings kept, by their card's ID: { id, at, add, only, edits, creates, display, card }. */
  const plans = new Map();
  /** The writings, by the same ID: { state, saves, saved, total, written, skipped, error, promise }. */
  const jobs = new Map();
  let reading = null;

  async function timeZone() {
    if (config.photoLinksTimeZone) return config.photoLinksTimeZone;
    try {
      const out = await store.sheets.request?.('?fields=properties(timeZone)', { background: true });
      return out?.properties?.timeZone || 'Europe/London';
    } catch {
      return 'Europe/London';
    }
  }

  async function read({ add = [], only = [] }, context = null, replace = null) {
    const drive = driveFor(store, config);
    if (!drive) return { error: 'This copy of the app has no Google Drive to read.' };
    const scan = await scanPhotoLinks({ store, drive, add, only, timeZone: await timeZone() });
    if (!only.length) known = { at: Date.now(), index: scan.index };
    const card = cardOf(scan, { add, only });
    const shown = show?.({ id: replace, card }, context) ?? { id: randomUUID().slice(0, 8) };
    const plan = { id: shown.id, shown, at: Date.now(), add, only, edits: scan.edits, creates: scan.creates, display: scan.display, card };
    plans.delete(plan.id);
    plans.set(plan.id, plan);
    jobs.delete(plan.id);
    for (const id of plans.keys()) {
      if (plans.size <= KEPT) break;
      plans.delete(id);
    }
    return plan;
  }

  /** One save; rows that cannot be saved (changed since the reading) are left out and counted. */
  async function save(body, user) {
    const requestId = `photo-links-${randomUUID()}`;
    try {
      return { out: await applyBatch(store, { requestId, ...body }, user, { source: 'ai_approved' }), skipped: 0 };
    } catch (e) {
      const items = e?.details?.items;
      if (!items?.length) throw e;
      const ids = new Set(items.map(i => i.id).filter(Boolean));
      const indexes = new Set(items.filter(i => !i.id && i.index !== null).map(i => i.index));
      const rest = {
        ...body,
        ...(body.edits ? { edits: body.edits.filter(x => !ids.has(x.id)) } : {}),
        ...(body.creates ? { creates: body.creates.filter((_, i) => !indexes.has(i)) } : {}),
      };
      const left = (rest.edits?.length ?? 0) + (rest.creates?.length ?? 0);
      const skipped = (body.edits?.length ?? 0) + (body.creates?.length ?? 0) - left;
      if (!left || !skipped) throw e;
      return {
        out: await applyBatch(store, { ...rest, requestId: `photo-links-${randomUUID()}` }, user, { source: 'ai_approved' }),
        skipped,
        why: items[0].message,
      };
    }
  }

  const progressOf = job =>
    filled({
      state: job.state,
      saved: job.saved,
      saves: job.saves,
      written: job.written,
      total: job.total,
      skipped: job.skipped,
      why: job.skippedWhy,
      error: job.error,
      leftFolders: job.left,
    });

  function write(plan, user, skip) {
    const edits = plan.edits.map(({ id, values, expected }) => ({ id, values, expected }));
    const creates = plan.creates.filter(c => !skip.has(c.folder)).map(({ module, values }) => ({ module, values }));
    const reason = `Photo_links brought up to date with Google Drive (${creates.length} new rows, ${edits.length} rows changed)`;
    const saves = [];
    for (let i = 0; i < edits.length; i += chunk) saves.push({ edits: edits.slice(i, i + chunk) });
    for (let i = 0; i < creates.length; i += chunk) saves.push({ creates: creates.slice(i, i + chunk) });
    const job = { state: 'writing', saves: saves.length, saved: 0, total: edits.length + creates.length, written: 0, skipped: 0, left: [...skip] };
    const say = () => {
      try {
        tell?.(plan.id, progressOf(job));
      } catch (e) {
        console.error('Photo_links progress:', e.message);
      }
    };
    say();
    job.promise = (async () => {
      for (const body of saves) {
        const rows = (body.edits?.length ?? 0) + (body.creates?.length ?? 0);
        const { out, skipped, why } = await save({ ...body, reason }, user);
        if (skipped) Object.assign(job, { skipped: job.skipped + skipped, skippedWhy: why });
        // Google not answering: the save is kept and written when it does; the rest waits for another reading.
        if (out.status === 'queued') {
          job.state = 'queued';
          return;
        }
        // Google took the save but has not confirmed it: new rows wait until it is (a second copy otherwise).
        if (out.status !== 'verified') {
          job.state = 'unconfirmed';
          return;
        }
        job.saved++;
        job.written += rows - skipped;
        if (job.saved < job.saves) say();
        // The app answers the pages between two saves (a local copy's save never waits for anything).
        await new Promise(resolve => setImmediate(resolve));
      }
      job.state = 'done';
    })()
      .catch(e => {
        job.state = 'failed';
        job.error = e?.message || String(e);
      })
      .finally(say);
    return job;
  }

  /** Starts writing a reading (or making a plan's changes on Drive): its job, or { error, code }. */
  function start(id, user, skip = []) {
    if (!EDITORS.includes(user?.role)) return { error: 'Your role cannot change the sheet', code: 'forbidden' };
    const running = [...jobs.values()].find(j => j.state === 'writing');
    if (running) return { error: 'Photo_links is being written already: wait for it to finish.', code: 'busy' };
    const plan = plans.get(id);
    if (!plan || Date.now() - plan.at > FRESH_MS)
      return {
        error: 'That reading is no longer kept (the folders may have changed since): read them again, look at the result, then confirm that one.',
        code: 'stale',
      };
    if (plan.type === 'files') {
      plans.delete(id);
      const job = change(plan, user);
      jobs.set(id, job);
      return { job };
    }
    const leave = new Set(skip);
    const total = plan.edits.length + plan.creates.filter(c => !leave.has(c.folder)).length;
    if (!total) return { error: `${SHEET} is already up to date.`, code: 'nothing' };
    plans.delete(id);
    const job = write(plan, user, leave);
    jobs.set(id, job);
    return { job };
  }

  const answer = (id, job) => {
    if (job.type === 'files') return changed(id, job);
    const base = filled({
      scan: id,
      rowsWritten: job.written,
      rowsToWrite: job.total,
      saves: `${job.saved} of ${job.saves}`,
      rowsLeftOut: job.skipped,
      leftOutBecause: job.skippedWhy,
      foldersLeftForLater: job.left,
    });
    if (job.state === 'writing')
      return { status: 'writing', ...base, next: 'Still writing: call sync_photo_links again with the same arguments to follow it.' };
    if (job.state === 'done')
      return {
        status: 'done',
        ...base,
        note: `Written to ${SHEET}; each save is in Historial and can be undone. Photo_dorsal and Photo_ventral show the new photos once the workbook has recalculated.`,
      };
    if (job.state === 'queued')
      return {
        status: 'queued',
        ...base,
        note: 'Google Sheets is not answering: the save in progress is kept in the app and written when it answers. Run sync_photo_links again later for the rest.',
      };
    if (job.state === 'unconfirmed')
      return {
        status: 'unconfirmed',
        ...base,
        note: 'Google has not confirmed the last save yet (the app keeps checking it). Run sync_photo_links again in a few minutes for the rest.',
      };
    return { status: 'failed', ...base, error: job.error, note: 'What was written stays (Historial). Read the folders again to see what is left.' };
  };
  const waited = async (id, job) => {
    await Promise.race([job.promise, new Promise(resolve => setTimeout(resolve, waitMs).unref?.())]);
    return answer(id, job);
  };

  async function run(args = {}, user, context = null) {
    if (!EDITORS.includes(user?.role)) return { error: 'Your role cannot change the sheet' };
    const add = list(args.add);
    const only = list(args.only);
    const running = [...jobs].find(([, j]) => j.state === 'writing');
    if (running) return waited(...running);
    if (args.confirmed) {
      const id = String(args.scan ?? '');
      if (jobs.has(id)) return answer(id, jobs.get(id));
      const started = start(id, user, list(args.skip));
      if (started.error) return started.code === 'nothing' ? { status: 'done', note: started.error } : { error: started.error };
      return waited(id, started.job);
    }
    // One reading at a time: a call for the same folders shares the one in progress.
    const key = JSON.stringify([add, only]);
    if (reading && reading.key !== key) await reading.promise.catch(() => {});
    if (!reading) {
      const mine = { key, promise: read({ add, only }, context) };
      reading = mine;
      const over = () => reading === mine && (reading = null);
      mine.promise.then(over, over);
    }
    const plan = await reading.promise;
    if (plan.error) return plan;
    const { id: _id, ...shown } = plan.shown;
    return brief(plan.id, plan.card, shown);
  }

  // ------------------------------------------------------------ the folders put in order (organize_photos)
  /** What the latest reading of every folder knows of the files: { at, index }. */
  let known = null;
  async function wholeIndex() {
    if (known && Date.now() - known.at < INDEX_MS) return known.index;
    const drive = driveFor(store, config);
    if (!drive) return null;
    const scan = await scanPhotoLinks({ store, drive, timeZone: await timeZone() });
    known = { at: Date.now(), index: scan.index };
    return known.index;
  }
  // The changes made on Drive, kept to take them back: a plan's ops as they went, and the saves that followed in Photo_links.
  store.db.exec(
    "CREATE TABLE IF NOT EXISTS drive_changes (id TEXT PRIMARY KEY, actor TEXT NOT NULL, created_at TEXT NOT NULL, ops_json TEXT NOT NULL, actions_json TEXT NOT NULL DEFAULT '[]', undone_at TEXT)",
  );
  const kept = op => ({ action: op.action, id: op.id, name: op.name, to: op.to, from: op.from, toId: op.toId, how: op.how, made: op.made, done: op.done });

  /** A plan's changes as its card keeps them. */
  function filesCard({ ops, problems }) {
    const counts = {};
    for (const op of ops) counts[kindOf(op)] = (counts[kindOf(op)] || 0) + 1;
    return {
      type: 'files',
      total: ops.length,
      counts,
      raw: ops.filter(o => o.raw).length,
      problems: { count: problems.length, examples: problems.slice(0, 8).map(p => [p.file ?? p.at, p.error].filter(Boolean).join(': ')) },
    };
  }
  const shownOp = op =>
    filled({ change: kindOf(op), folder: op.folder, name: op.name, to: op.action === 'folder' ? null : op.to, url: op.url, raw: op.raw, copy: op.copy || op.copied });

  /** Makes a plan's changes on Drive, then has Photo_links follow the files touched. */
  function change(plan, user) {
    const writer = driveWriterFor(store, config);
    const job = { type: 'files', state: 'writing', total: plan.ops.length, written: 0, failed: 0, sheetRows: 0 };
    const say = () => {
      try {
        tell?.(
          plan.id,
          filled({
            state: job.state,
            written: job.written,
            total: job.total,
            failed: job.failed,
            errors: job.errors,
            phase: job.phase,
            sheetRows: job.sheetRows,
            sheetNote: job.sheetNote,
            error: job.error,
          }),
        );
      } catch (e) {
        console.error('Drive changes progress:', e.message);
      }
    };
    say();
    job.promise = (async () => {
      let last = 0;
      await runFileChanges(writer, plan.ops, {
        onStep: (done, failed) => {
          Object.assign(job, { written: done, failed });
          if (Date.now() - last < 700) return;
          last = Date.now();
          say();
        },
      });
      known = null;
      job.errors = plan.ops.filter(o => o.error).slice(0, EXAMPLES).map(o => `${o.name}: ${o.error}`);
      const done = plan.ops.filter(o => o.done);
      store.db
        .prepare('INSERT OR REPLACE INTO drive_changes (id, actor, created_at, ops_json) VALUES (?,?,?,?)')
        .run(plan.id, String(user.id ?? user.username), new Date().toISOString(), JSON.stringify(done.map(kept)));
      // Photo_links follows: the rows of the files renamed, moved or removed.
      const touched = new Set(done.filter(o => ['rename', 'move', 'remove'].includes(o.action)).map(o => o.id));
      const drive = driveFor(store, config);
      if (touched.size && drive) {
        job.phase = 'sheet';
        say();
        const fileOf = new Map(photoLinkRows(store).map(r => [r.id, driveIdOf(r.values.URL)]));
        const scan = await scanPhotoLinks({ store, drive, timeZone: await timeZone() });
        known = { at: Date.now(), index: scan.index };
        const edits = scan.edits.filter(e => touched.has(fileOf.get(e.id))).map(({ id, values, expected }) => ({ id, values, expected }));
        const actions = [];
        for (let i = 0; i < edits.length; i += chunk) {
          const body = { edits: edits.slice(i, i + chunk), reason: `Photo_links follows the files changed on Google Drive (${edits.length} rows)` };
          const { out, skipped } = await save(body, user);
          if (out.status !== 'verified') {
            job.sheetNote = 'Google Sheets did not confirm the save: sync_photo_links brings the rest of the rows up to date later.';
            break;
          }
          job.sheetRows += body.edits.length - skipped;
          if (out.action?.id) actions.push(out.action.id);
          await new Promise(resolve => setImmediate(resolve));
        }
        store.db.prepare('UPDATE drive_changes SET actions_json = ? WHERE id = ?').run(JSON.stringify(actions), plan.id);
      }
      job.state = 'done';
    })()
      .catch(e => {
        job.state = 'failed';
        job.error = e?.message || String(e);
      })
      .finally(say);
    return job;
  }
  const changed = (id, job) => {
    const base = filled({
      plan: id,
      filesChanged: job.written,
      of: job.total,
      failed: job.failed,
      failedExamples: job.errors,
      photoLinksRowsUpdated: job.sheetRows,
      note: job.sheetNote,
    });
    if (job.state === 'writing') return { status: 'writing', ...base, next: 'Still changing files: call organize_photos again with the same arguments to follow it.' };
    if (job.state === 'failed') return { status: 'failed', ...base, error: job.error, undo: `organize_photos with undo: "${id}" takes back what was changed.` };
    return {
      status: 'done',
      ...base,
      undo: `organize_photos with undo: "${id}" takes these changes back (the files, and Photo_links' rows).`,
      next: 'New files in the listed folders, if any, still need sync_photo_links.',
    };
  };

  /** Takes a plan's changes back: the files as they were, and the saves that followed in Photo_links undone. */
  async function undo(id, user, writer) {
    const batch = store.db.prepare('SELECT * FROM drive_changes WHERE id = ?').get(id);
    if (!batch) return { error: 'No changes are kept under that ID.' };
    if (batch.undone_at) return { error: `Those changes were taken back already (${batch.undone_at.slice(0, 16)}).` };
    const out = await undoFileChanges(writer, JSON.parse(batch.ops_json));
    known = null;
    let saves = 0;
    let sheetNote = null;
    for (const actionId of JSON.parse(batch.actions_json || '[]').reverse())
      try {
        await store.undo({ actionIds: [actionId], requestId: `photo-files-undo-${randomUUID()}` }, user);
        saves++;
      } catch (e) {
        sheetNote = `A save of Photo_links could not be undone (${e?.message || e}): sync_photo_links brings the sheet up to date.`;
      }
    store.db.prepare('UPDATE drive_changes SET undone_at = ? WHERE id = ?').run(new Date().toISOString(), id);
    return filled({ status: 'undone', filesAsTheyWere: out.undone, failed: out.failed, photoLinksSavesUndone: saves, note: sheetNote });
  }

  async function organize(args = {}, user, context = null) {
    if (!EDITORS.includes(user?.role)) return { error: 'Your role cannot change files' };
    const writer = driveWriterFor(store, config);
    if (!writer) return { error: 'This app has no Google account set up to change files on Drive.' };
    const running = [...jobs].find(([, j]) => j.state === 'writing');
    if (running) return waited(...running);
    if (args.undo) return undo(String(args.undo), user, writer);
    if (args.confirmed) {
      const id = String(args.plan ?? '');
      if (jobs.has(id)) return answer(id, jobs.get(id));
      const started = start(id, user);
      return started.error ? { error: started.error } : waited(id, started.job);
    }
    const asked = value => (Array.isArray(value) ? value : value ? [value] : []).slice(0, 5000);
    const index = await wholeIndex();
    if (!index) return { error: 'This copy of the app has no Google Drive to read.' };
    const planned = await settleChanges(writer, planFileChanges({ index, operations: asked(args.operations), fix: asked(args.fix), withRaw: args.withRaw !== false }));
    if (!planned.ops.length) return { error: 'Nothing to change.', ...describeChanges([], planned.problems) };
    const card = filesCard(planned);
    const shown = show?.({ id: null, card, title: FILES_TITLE }, context) ?? { id: randomUUID() };
    plans.set(shown.id, { id: shown.id, shown, type: 'files', at: Date.now(), ops: planned.ops, display: planned.ops.map(shownOp), card });
    jobs.delete(shown.id);
    const { id, ...links } = shown;
    return {
      plan: id,
      ...links,
      changes: describeChanges(planned.ops, planned.problems),
      next: `Nothing is changed yet. After the person agrees (they can also press «Apply» beside the chat): organize_photos with plan "${id}" and confirmed: true.`,
    };
  }

  return {
    run,
    organize,
    /** The card's «Aplicar»: { status: 'applying' } once it has started, or { error, code }. */
    apply(id, user, { skip = [] } = {}) {
      const started = start(id, user, list(skip));
      return started.error ? started : { status: 'applying' };
    },
    /** The card's «Leer de nuevo»: the folders read again into the same card. */
    async refresh(id, params, user) {
      if (!EDITORS.includes(user?.role)) return { error: 'Your role cannot change the sheet', code: 'forbidden' };
      if ([...jobs.values()].some(j => j.state === 'writing')) return { error: 'Photo_links is being written: wait for it to finish.', code: 'busy' };
      const plan = await read({ add: list(params?.add), only: list(params?.only) }, null, id);
      return plan.error ? { error: plan.error, code: 'no_drive' } : { status: 'pending' };
    },
    /** The rows of a reading as the card's table lists them (null once written, or too old). */
    rows: id => plans.get(id)?.display ?? null,
    writing: () => [...jobs.values()].some(j => j.state === 'writing'),
  };
}
