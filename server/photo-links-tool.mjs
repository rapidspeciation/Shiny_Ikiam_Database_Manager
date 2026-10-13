// The assistant's tool for Photo_links (sync_photo_links): reads the photo folders on Google
// Drive, says what the sheet lacks (server/photo-links.mjs), and writes it once the person agrees,
// as ordinary saves (server/batch.mjs: each in Historial, each undoable). Only the app's thread
// runs it (server/assistant-host.mjs MAIN_TOOLS): it reads Google and writes the sheet.

import { randomUUID } from 'node:crypto';
import { applyBatch } from './batch.mjs';
import { driveFor } from './drive.mjs';
import { SHEET, scanPhotoLinks } from './photo-links.mjs';

export const PHOTO_LINKS_TOOL = {
  type: 'function',
  function: {
    name: 'sync_photo_links',
    description: [
      'Bring the Photo_links sheet up to date with the photo folders on Google Drive, so Photo_dorsal and Photo_ventral find the photos uploaded since (what the sheet\'s menu «📷 Photo Database Tools» does).',
      '- Without `confirmed`: reads Drive (up to about a minute) and answers what would change; nothing is written. Tell the person what matters in it:',
      '  - `changes`: new files per listed folder; rows whose file was renamed, replaced by one of the same name (`relinked`) or deleted (`removed`: the row is emptied).',
      '  - `newPhotoNames`: of the new photos (RAW files apart), `linked` are named <ID>d.JPG or <ID>v.JPG with an ID of the database, which is what the sheets look up; `rename` would be found under the name in `to`; `unknownId` carry an ID the database lacks; `unnamed` still have the camera\'s name.',
      '  - `sameName`: files not listed because a listed file already has their name: `copies` of that photo, or `different` photos (one of the two is misnamed, or the ID was used twice).',
      '  - `notListed`: folders with specimen photos beside the listed ones. `leftOut`: files named …_todelete and folders named …_temp, which are never listed.',
      '- `add`: folders to start listing, by their `notListed` name or a Drive link.',
      "- `confirmed: true` with that answer's `scan`, only after the person's latest message approves it: writes it, some hundred rows per save. A long one answers `writing`: call again with the same arguments to follow it.",
    ].join('\n'),
    parameters: {
      type: 'object',
      properties: {
        add: { type: 'array', items: { type: 'string' } },
        scan: { type: 'string' },
        confirmed: { type: 'boolean' },
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

const cut = list => (list.length > EXAMPLES ? { count: list.length, examples: list.slice(0, EXAMPLES) } : list);
const filled = object =>
  Object.fromEntries(
    Object.entries(object).filter(
      ([, v]) => !(v === 0 || v === null || v === undefined || (typeof v === 'object' && !Object.keys(v).length)),
    ),
  );

/** A reading as the model gets it. */
function brief(scan) {
  const s = scan.summary;
  const pending = scan.edits.length + scan.creates.length;
  return filled({
    scan: scan.id,
    drive: { folders: scan.read.folders, files: scan.read.files, seconds: scan.read.seconds },
    sheetRows: scan.read.rows,
    upToDate: pending ? null : true,
    changes: pending
      ? filled({
          newFiles: s.newFiles,
          newFolders: s.newFolders,
          byFolder: s.byFolder,
          renamed: cut(s.renamed),
          relinked: cut(s.relinked),
          removed: cut(s.removed),
          movedInDrive: s.moved,
          ownerOrSizeChanged: s.details,
          scriptStatusRowsCleared: s.statusRows,
        })
      : null,
    newPhotoNames: s.names?.photos ? s.names : null,
    sameName: s.sameName.copies || s.sameName.different ? s.sameName : null,
    notListed: s.notListed.map(({ id: _id, ...rest }) => rest),
    leftOut: filled({ toDeleteFiles: s.leftOut, tempFolders: s.setAside }),
    // Rows left as they are.
    untouched: filled({
      fileNowOutsideTheListedFolders: s.outside + s.elsewhere,
      fileNotFoundButUnchecked: s.unchecked,
      rowsRepeatingAFile: s.repeated,
      rowsWithoutALink: s.unread,
    }),
    foldersNotFound: scan.notFound,
    next: pending
      ? `Nothing is written yet. After the person agrees: sync_photo_links with scan "${scan.id}" and confirmed: true.`
      : null,
  });
}

/**
 * sync_photo_links for one app: run(args, user). `drive`, `waitMs`, `chunk`: for tests.
 */
export function createPhotoLinksTool({ store, config = {}, waitMs = WAIT_MS, chunk = CHUNK }) {
  let last = null; // The latest reading: { id, at, add, edits, creates, summary, read }
  let reading = null;
  let job = null; // The writing of a reading: { scan, state, saves, done, total, written, skipped, error, promise }

  async function timeZone() {
    if (config.photoLinksTimeZone) return config.photoLinksTimeZone;
    try {
      const out = await store.sheets.request?.('?fields=properties(timeZone)', { background: true });
      return out?.properties?.timeZone || 'Europe/London';
    } catch {
      return 'Europe/London';
    }
  }

  async function read(add) {
    const drive = driveFor(store, config);
    if (!drive) return { error: 'This copy of the app has no Google Drive to read.' };
    const scan = await scanPhotoLinks({ store, drive, add, timeZone: await timeZone() });
    last = { ...scan, id: randomUUID().slice(0, 8), at: Date.now(), add };
    return last;
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

  function write(scan, user) {
    const reason = `Photo_links brought up to date with Google Drive (${scan.creates.length} new rows, ${scan.edits.length} rows changed)`;
    const saves = [];
    const edits = scan.edits.map(({ id, values, expected }) => ({ id, values, expected }));
    for (let i = 0; i < edits.length; i += chunk) saves.push({ edits: edits.slice(i, i + chunk) });
    for (let i = 0; i < scan.creates.length; i += chunk) saves.push({ creates: scan.creates.slice(i, i + chunk) });
    const state = {
      scan: scan.id,
      state: 'writing',
      saves: saves.length,
      saved: 0,
      total: edits.length + scan.creates.length,
      written: 0,
      skipped: 0,
      actions: [],
    };
    state.promise = (async () => {
      for (const body of saves) {
        const rows = (body.edits?.length ?? 0) + (body.creates?.length ?? 0);
        const { out, skipped, why } = await save({ ...body, reason }, user);
        if (skipped) Object.assign(state, { skipped: state.skipped + skipped, skippedWhy: why });
        // Google not answering: the save is kept and written when it does; the rest waits for another reading.
        if (out.status === 'queued') {
          state.state = 'queued';
          return;
        }
        // Google took the save but has not confirmed it: new rows wait until it is (a second copy otherwise).
        if (out.status !== 'verified') {
          state.state = 'unconfirmed';
          return;
        }
        state.saved++;
        state.written += rows - skipped;
        if (out.action?.id) state.actions.push(out.action.id);
      }
      state.state = 'done';
    })().catch(e => {
      state.state = 'failed';
      state.error = e?.message || String(e);
    });
    return state;
  }

  const progress = () => {
    const base = filled({
      scan: job.scan,
      rowsWritten: job.written,
      rowsToWrite: job.total,
      saves: `${job.saved} of ${job.saves}`,
      rowsLeftOut: job.skipped,
      leftOutBecause: job.skippedWhy,
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

  async function run(args = {}, user) {
    if (!EDITORS.includes(user?.role)) return { error: 'Your role cannot change the sheet' };
    const add = (Array.isArray(args.add) ? args.add : args.add ? [args.add] : []).map(a => String(a).slice(0, 300)).slice(0, 20);
    if (job?.state === 'writing') {
      await Promise.race([job.promise, new Promise(resolve => setTimeout(resolve, waitMs).unref?.())]);
      return progress();
    }
    if (args.confirmed) {
      if (job && job.scan === args.scan) return progress();
      if (!last || last.id !== args.scan || Date.now() - last.at > FRESH_MS)
        return { error: 'That reading is no longer kept (the folders may have changed since): call sync_photo_links without `confirmed`, show the person the result, then confirm that one.' };
      if (!last.edits.length && !last.creates.length) return { status: 'done', note: `${SHEET} is already up to date.` };
      job = write(last, user);
      last = null;
      await Promise.race([job.promise, new Promise(resolve => setTimeout(resolve, waitMs).unref?.())]);
      return progress();
    }
    // One reading at a time: a call for the same folders shares the one in progress.
    const key = JSON.stringify(add);
    if (reading && reading.key !== key) await reading.promise.catch(() => {});
    if (!reading) {
      const mine = { key, promise: read(add) };
      reading = mine;
      mine.promise.then(
        () => reading === mine && (reading = null),
        () => reading === mine && (reading = null),
      );
    }
    const scan = await reading.promise;
    return scan.error ? scan : brief(scan);
  }

  return { run, state: () => ({ last, job }) };
}
