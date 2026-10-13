// The assistant's tool for Photo_links (sync_photo_links): reads the photo folders on Google
// Drive, says what the sheet lacks and what looks wrong in the new photos (server/photo-links.mjs),
// shows it beside the chat as a card of Cambios propuestos (server/assistant.mjs keeps the card), and
// writes it once the person agrees, in the chat or with the card's «Aplicar», as ordinary saves
// (server/batch.mjs: each in Historial, each undoable), telling the card how far it is.
// Only the app's thread runs it (server/assistant-host.mjs MAIN_TOOLS): it reads Google and writes the sheet.

import { randomUUID } from 'node:crypto';
import { applyBatch } from './batch.mjs';
import { driveFor } from './drive.mjs';
import { SHEET, scanPhotoLinks } from './photo-links.mjs';

export const PHOTO_LINKS_TOOL = {
  type: 'function',
  function: {
    name: 'sync_photo_links',
    description: [
      "Bring the Photo_links sheet up to date with the photo folders on Google Drive, so Photo_dorsal and Photo_ventral find the photos uploaded since (what the sheet's menu «📷 Photo Database Tools» does).",
      '- Without `confirmed`: reads Drive (up to about half a minute) and answers what would change; nothing is written. It also shows beside the chat (`link`), with every row and an «Apply» button. Tell the person what matters in it:',
      '  - `changes`: new files per listed folder; rows whose file was renamed, replaced by one of the same name (`relinked`) or deleted (`removed`: the row is emptied).',
      "  - `newPhotos`: of the new photos (RAW files apart), `linked` are named <ID>d.JPG or <ID>v.JPG with an ID of the workbook, which is what the sheets look up. The rest are things to look at, each with examples: `rename` (found under the name in `to`), `unknownId` (the workbook lacks the ID; `maybe`: likely the ID meant), `unnamed` (the camera's name), `outOfOrder` (the ID is far from those of the photos taken just before and after, `around`: how a misread ID looks), `unpaired` (the other view is missing), `beforeCollection` (taken before the butterfly was collected), `otherCountry` (a butterfly of another country than its folder's).",
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
};

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

  /** Starts writing a reading: its job, or { error, code }. */
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
    const leave = new Set(skip);
    const total = plan.edits.length + plan.creates.filter(c => !leave.has(c.folder)).length;
    if (!total) return { error: `${SHEET} is already up to date.`, code: 'nothing' };
    plans.delete(id);
    const job = write(plan, user, leave);
    jobs.set(id, job);
    return { job };
  }

  const answer = (id, job) => {
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

  return {
    run,
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
