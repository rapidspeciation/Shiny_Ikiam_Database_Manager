// The photo folders on Google Drive put in order: files renamed, moved to another folder or removed,
// folders made, shortcuts to a set of photos gathered in a folder, as the assistant's organize_photos
// asks (server/photo-links-tool.mjs shows the plan as a card and runs it once the person agrees;
// Photo_links then follows the files it touched).
//
// - planFileChanges works the request out against a reading of the folders (server/photo-links.mjs
//   `index`): which file each operation means, its RAW file with it, and what cannot be done.
// - settleChanges asks the account that changes files what it may do with them.
// - runFileChanges makes the changes with that account (server/drive.mjs driveWriterFor) and says
//   how each went; undoFileChanges takes them back.
//
// Removing a file: the account bins the files it owns (they stay 30 days in Drive's bin, and count
// against its space until the bin is emptied). Nobody can bin someone else's file, so those are
// renamed …_todelete instead, which takes them out of Photo_links and leaves them for their owner to
// delete. That is the mark the team's sheet «Transfer Ownership & Free Up Space» uses: its «Copy
// files», run by the account that takes the photos over, copies each file and renames the original
// …_todelete; its «Delete copied files», run by the first owner, deletes those. The fix `copied`
// does that second step for the account's own files, and only where the copy is there.

import { FOLDER } from './drive.mjs';
import { driveIdOf, rightName } from './photo-links.mjs';

const PHOTO = /\.(jpe?g|png|heic|heif|jfif)$/i;
const RAW = /\.(cr2|cr3|orf|nef|arw|dng|raf|rw2|pef|tiff?)$/i;
const TO_DELETE = /_todelete$/i;
/** Changes of one plan, at most. */
export const MAX_OPERATIONS = 5000;
const EXAMPLES = 6;

const lower = v => String(v ?? '').toLowerCase();
const isFolder = item => item?.mimeType === FOLDER;
const stemOf = name => (String(name).lastIndexOf('.') > 0 ? String(name).slice(0, String(name).lastIndexOf('.')) : String(name));
const extOf = name => (String(name).lastIndexOf('.') > 0 ? String(name).slice(String(name).lastIndexOf('.')) : '');
const idIn = text => driveIdOf(text) ?? (/^[\w-]{20,}$/.test(String(text ?? '').trim()) ? String(text).trim() : null);

/**
 * The changes a request means: { ops, problems }.
 *  - index: a reading of every listed folder ({ items, home, folderPath, info }).
 *  - operations: [{ action, … }]
 *      rename   { file, name }     a file's new name (without an ending: it keeps its own)
 *      move     { file, to }       a file or a folder into the folder `to`
 *      remove   { file }           a file out of the photos (binned, or renamed …_todelete)
 *      folder   { name, in }       a new folder `name` inside the folder `in`
 *      shortcut { ids, to }        shortcuts to the dorsal and ventral photos of those specimens, in the folder `to`
 *    `file` and the folders: a Drive link or ID, a path ("PERU_2024/JPG/CAM077740d.JPG") or a name no
 *    other has. A folder made by this plan can be named by its path in later operations; `to` may be
 *    the link of a folder outside the photo folders.
 *  - fix: [{ check, in }] the changes a check of the photos calls for, in the folder `in` and inside it:
 *      rename  every photo whose name the sheets do not find takes the name they look up
 *      copies  every file of `in` that is also, the same file, in another listed folder is removed
 *      copied  every …_todelete file whose copy is beside it is binned (the account's own files)
 *  - withRaw: a photo renamed or removed takes its RAW file with it (the file of its name in the
 *    RAW folder beside its own).
 * An op: { action, id, name, folder, to, … }: `to` is the new name, or the folder gone into (`toId`,
 * or `toNew`: the op that makes it); a RAW file following its photo has `raw: true`.
 */
export function planFileChanges({ index, operations = [], fix = [], withRaw = true }) {
  const { items, home, folderPath, info } = index;
  const ops = [];
  const problems = [];
  const touched = new Map();
  const made = new Map();

  const wholePath = item => {
    const parts = [];
    for (let f = item; f && f.id !== home; f = f.parent ? items.get(f.parent) : null) parts.unshift(f.name);
    return parts.join('/');
  };
  const pathOfFolder = id => folderPath(id) ?? (items.has(id) ? wholePath(items.get(id)) : '');
  const listedFile = item => !isFolder(item) && !!item.parent && !!folderPath(item.parent);
  const all = [...items.values()].filter(f => f.id !== home);
  let paths = null;
  /** The items a path or a name stands for. */
  const named = (text, want) => {
    paths ??= all.map(item => [lower(wholePath(item)), item]);
    const wanted = lower(String(text).trim().replace(/^\/+|\/+$/g, ''));
    return paths.filter(([path, item]) => want(item) && (path === wanted || path.endsWith(`/${wanted}`))).map(([, item]) => item);
  };
  const find = (text, want, what) => {
    if (text === undefined || text === null || String(text).trim() === '') return { error: `give the ${what}` };
    const id = idIn(text);
    if (id && items.has(id) && want(items.get(id))) return { item: items.get(id) };
    const found = named(text, want);
    if (found.length === 1) return { item: found[0] };
    if (!found.length) return { error: `no ${what} "${String(text).slice(0, 120)}" in the photo folders` };
    return { error: `${found.length} ${what}s are named "${String(text).slice(0, 120)}": give its folder too, or its link (${found.slice(0, 3).map(wholePath).join('; ')})` };
  };
  /** A folder to put things in: one of the photo folders, one this plan makes (`fresh`), or another by its link (`outside`). */
  const findFolder = text => {
    const wanted = lower(String(text ?? '').trim().replace(/^\/+|\/+$/g, ''));
    const fresh = [...made].find(([path]) => path === wanted || path.endsWith(`/${wanted}`));
    if (fresh) return { fresh: fresh[1] };
    const id = idIn(text);
    return id && !items.has(id) ? { outside: id } : find(text, isFolder, 'folder');
  };

  // A photo's RAW file: the file of its name in a folder beside its own (JPG and RAW side by side), or in it.
  let raws = null;
  const rawsOf = item => {
    if (!withRaw || !PHOTO.test(item.name)) return [];
    if (!raws) {
      raws = new Map();
      for (const f of all) {
        if (isFolder(f) || !RAW.test(f.name)) continue;
        const key = lower(stemOf(f.name));
        if (!raws.has(key)) raws.set(key, []);
        raws.get(key).push(f);
      }
    }
    const above = items.get(item.parent)?.parent;
    return (raws.get(lower(stemOf(item.name))) ?? []).filter(f => f.parent === item.parent || (above && items.get(f.parent)?.parent === above));
  };
  /** A folder's items by name, as they are once the plan is done: two files never get one name. */
  const folders = new Map();
  const inFolder = folderId => {
    if (!folders.has(folderId)) folders.set(folderId, new Map(all.filter(f => f.parent === folderId).map(f => [lower(f.name), f])));
    return folders.get(folderId);
  };
  const taken = (folderId, name, self) => {
    const holder = inFolder(folderId).get(lower(name));
    return holder !== undefined && holder.id !== self;
  };
  const add = (op, item, key = item.id) => {
    if (touched.has(key)) return problems.push({ file: wholePath(item), error: `two changes of it (${touched.get(key)} and ${op.action})` }) && false;
    if (ops.length >= MAX_OPERATIONS) return false;
    touched.set(key, op.action);
    ops.push({ ...op, id: item.id, name: item.name, folder: pathOfFolder(item.parent), ...(item.webViewLink ? { url: item.webViewLink } : {}) });
    return true;
  };
  const rename = (item, name, extra = {}) => {
    const to = extOf(name) ? name : `${name}${extOf(item.name)}`;
    if (to === item.name) return false;
    if (taken(item.parent, to, item.id)) return problems.push({ file: wholePath(item), error: `${to} already exists in its folder` }) && false;
    if (!add({ action: 'rename', to, ...extra }, item)) return false;
    inFolder(item.parent).delete(lower(item.name));
    inFolder(item.parent).set(lower(to), item);
    return true;
  };
  const remove = (item, extra = {}) => !TO_DELETE.test(item.name) && add({ action: 'remove', ...extra }, item);
  /** Where a `to` leads: { toId | toNew, to (its path) }, or { error }. */
  const target = text => {
    const to = findFolder(text);
    if (to.error) return to;
    if (to.fresh !== undefined) return { toNew: to.fresh, to: `${ops[to.fresh].folder}/${ops[to.fresh].name}` };
    if (to.outside) return { toId: to.outside, to: String(text).slice(0, 200), outside: true };
    return { toId: to.item.id, to: pathOfFolder(to.item.id), item: to.item };
  };

  for (const [i, o] of operations.entries()) {
    const at = `operations[${i}]`;
    const action = String(o?.action ?? '');
    if (action === 'folder') {
      const name = String(o.name ?? '').trim();
      if (!name || /[/\\]/.test(name)) {
        problems.push({ at, error: 'folder: give its name (no slashes) and the folder it goes in (`in`)' });
        continue;
      }
      const parent = find(o.in, isFolder, 'folder');
      if (parent.error) {
        problems.push({ at, error: parent.error });
        continue;
      }
      const path = lower(`${wholePath(parent.item)}/${name}`);
      if (made.has(path) || taken(parent.item.id, name, null)) {
        problems.push({ at, error: `the folder ${name} already exists in ${wholePath(parent.item)}` });
        continue;
      }
      made.set(path, ops.length);
      ops.push({ action: 'folder', name, folder: pathOfFolder(parent.item.id), toId: parent.item.id });
      continue;
    }
    if (action === 'shortcut') {
      const where = target(o.to);
      const ids = (Array.isArray(o.ids) ? o.ids : o.ids ? [o.ids] : []).map(id => String(id).trim().toUpperCase()).filter(Boolean);
      if (where.error || !ids.length) {
        problems.push({ at, error: where.error ?? 'shortcut: give the specimens (`ids`) and the folder (`to`)' });
        continue;
      }
      // Each specimen's dorsal and ventral photo as Photo_links would list it: the first of that name.
      const photos = new Map();
      for (const item of all) if (listedFile(item) && PHOTO.test(item.name) && !TO_DELETE.test(item.name) && !photos.has(lower(item.name))) photos.set(lower(item.name), item);
      const none = [];
      let already = 0;
      for (const id of ids)
        for (const view of ['d', 'v']) {
          const photo = photos.get(lower(`${id}${view}.jpg`));
          if (!photo) none.push(`${id}${view}`);
          else if (where.item && taken(where.item.id, photo.name, null)) already++;
          else if (add({ action: 'shortcut', to: where.to, ...(where.toId ? { toId: where.toId } : { toNew: where.toNew }), ...(where.outside ? { outside: true } : {}) }, photo, `shortcut:${photo.id}`) && where.item)
            inFolder(where.item.id).set(lower(photo.name), photo);
        }
      if (none.length) problems.push({ at, error: `no listed photo of ${none.length} views: ${none.slice(0, 12).join(', ')}${none.length > 12 ? '…' : ''}` });
      if (already) problems.push({ at, error: `${already} are in that folder already` });
      continue;
    }
    if (!['rename', 'move', 'remove'].includes(action)) {
      problems.push({ at, error: `action is rename, move, remove, folder or shortcut (${String(o?.action).slice(0, 30)} given)` });
      continue;
    }
    const found = find(o.file, action === 'move' ? () => true : f => !isFolder(f), 'file');
    if (found.error) {
      problems.push({ at, error: found.error });
      continue;
    }
    const item = found.item;
    if (action === 'rename') {
      const name = String(o.name ?? '').trim();
      if (!name || /[/\\]/.test(name)) {
        problems.push({ at, error: 'rename: give the new name' });
        continue;
      }
      const to = extOf(name) ? name : `${name}${extOf(item.name)}`;
      if (rename(item, to)) for (const raw of rawsOf(item)) rename(raw, `${stemOf(to)}${extOf(raw.name)}`, { raw: true });
    } else if (action === 'remove') {
      if (remove(item)) for (const raw of rawsOf(item)) remove(raw, { raw: true });
    } else {
      const where = target(o.to);
      if (where.error) {
        problems.push({ at, error: where.error });
        continue;
      }
      if (where.item && (where.item.id === item.parent || where.item.id === item.id)) {
        problems.push({ at, error: `${item.name} is already in ${where.to}` });
        continue;
      }
      if (where.item && taken(where.item.id, item.name, item.id)) {
        problems.push({ at, error: `${item.name} already exists in ${where.to}` });
        continue;
      }
      const moved = add({ action: 'move', to: where.to, from: item.parent, ...(where.toId ? { toId: where.toId } : { toNew: where.toNew }), ...(where.outside ? { outside: true } : {}) }, item);
      if (moved && where.item) inFolder(where.item.id).set(lower(item.name), item);
    }
  }

  for (const [i, f] of fix.entries()) {
    const at = `fix[${i}]`;
    const check = String(f?.check ?? '');
    if (!['rename', 'copies', 'copied'].includes(check)) {
      problems.push({ at, error: `check is rename, copies or copied (${check.slice(0, 30)} given)` });
      continue;
    }
    const where = f.in === undefined || f.in === null || f.in === '' ? null : find(f.in, isFolder, 'folder');
    if (where?.error || (check === 'copies' && !where)) {
      problems.push({ at, error: where?.error ?? 'copies: give the folder to remove them from (`in`)' });
      continue;
    }
    const inside = item => {
      if (!where) return true;
      for (let p = item.parent; p; p = items.get(p)?.parent) if (p === where.item.id) return true;
      return false;
    };
    const before = ops.length;
    if (check === 'rename') {
      for (const item of all) {
        if (!listedFile(item) || !PHOTO.test(item.name) || TO_DELETE.test(item.name) || !inside(item) || touched.has(item.id)) continue;
        const right = rightName(item.name, info);
        if (right && right !== item.name && rename(item, right)) for (const raw of rawsOf(item)) rename(raw, `${stemOf(right)}${extOf(raw.name)}`, { raw: true });
      }
    } else if (check === 'copies') {
      // The same file twice: the same name (as the sheets read it) and the same size.
      const key = item => `${lower(rightName(item.name, info) ?? item.name)}|${item.size ?? ''}`;
      const elsewhere = new Set();
      for (const item of all) if (listedFile(item) && !TO_DELETE.test(item.name) && !inside(item)) elsewhere.add(key(item));
      for (const item of all)
        if (listedFile(item) && !TO_DELETE.test(item.name) && inside(item) && item.size && elsewhere.has(key(item)) && !touched.has(item.id)) remove(item, { copy: true });
    } else {
      // Left by «Copy files»: the original, renamed …_todelete, beside its copy (its name, its size).
      for (const item of all) {
        if (isFolder(item) || !TO_DELETE.test(item.name) || !inside(item) || touched.has(item.id)) continue;
        const copy = inFolder(item.parent).get(lower(item.name.replace(TO_DELETE, '')));
        if (copy && copy.id !== item.id && String(copy.size ?? '') === String(item.size ?? '')) add({ action: 'remove', copied: true }, item);
      }
    }
    if (ops.length === before)
      problems.push({
        at,
        error:
          check === 'rename'
            ? 'no photo there needs another name'
            : check === 'copies'
              ? 'no file there is also in another listed folder'
              : 'no …_todelete file there has its copy beside it',
      });
  }
  if (ops.length >= MAX_OPERATIONS) problems.push({ error: `more than ${MAX_OPERATIONS} changes: the first ${MAX_OPERATIONS} are planned; ask again for the rest` });
  return { ops, problems };
}

/**
 * What the account that changes files may do: each `remove` is a file binned when it is the
 * account's own (`how: 'bin'`), renamed …_todelete otherwise (`how: 'mark'`); a …_todelete file that
 * is someone else's, and a `to` that is no folder, are left out (added to `problems`).
 */
export async function settleChanges(writer, { ops, problems }, { parallel = 8 } = {}) {
  const left = new Set();
  const asked = new Map();
  const ask = id => asked.get(id) ?? asked.set(id, writer.get(id).catch(() => null)).get(id);
  const queue = ops.filter(op => op.action === 'remove' || op.outside);
  let notOwn = 0;
  await Promise.all(
    Array.from({ length: parallel }, async () => {
      while (queue.length) {
        const op = queue.shift();
        if (op.outside) {
          const folder = await ask(op.toId);
          if (folder?.mimeType !== FOLDER) {
            left.add(op);
            problems.push({ file: `${op.folder}/${op.name}`, error: `${op.to} is not a folder the account can open` });
          } else op.to = folder.name;
          continue;
        }
        const item = await ask(op.id);
        if (item?.ownedByMe) op.how = 'bin';
        else if (op.copied) {
          left.add(op);
          notOwn++;
        } else {
          op.how = 'mark';
          op.to = `${op.name}_todelete`;
        }
      }
    }),
  );
  if (notOwn) problems.push({ error: `${notOwn} …_todelete files belong to another account: their owner deletes them («Delete copied files» in the sheet «Transfer Ownership & Free Up Space»)` });
  // Left out: the ops after them keep pointing at the folders this plan makes.
  const kept = ops.filter(op => !left.has(op));
  const at = new Map(kept.map((op, i) => [op, i]));
  for (const op of kept) if (op.toNew !== undefined) op.toNew = at.get(ops[op.toNew]);
  return { ops: kept, problems };
}

/**
 * Makes the changes, the folders first: each op comes back with `done: true`, or `error`.
 * `onStep(done, failed)` is called as they go; `parallel` files at a time.
 */
export async function runFileChanges(writer, ops, { onStep = null, parallel = 4 } = {}) {
  let done = 0;
  let failed = 0;
  const step = (op, error) => {
    if (error) {
      op.error = String(error.message || error).slice(0, 300);
      failed++;
    } else {
      op.done = true;
      done++;
    }
    onStep?.(done, failed);
  };
  for (const op of ops.filter(o => o.action === 'folder')) {
    try {
      op.id = (await writer.createFolder(op.name, op.toId)).id;
      step(op);
    } catch (e) {
      step(op, e);
    }
  }
  const queue = ops.filter(o => o.action !== 'folder');
  await Promise.all(
    Array.from({ length: parallel }, async () => {
      while (queue.length) {
        const op = queue.shift();
        try {
          if (op.toNew !== undefined) {
            if (!ops[op.toNew]?.done) throw new Error('its folder could not be made');
            op.toId = ops[op.toNew].id;
          }
          if (op.action === 'rename') await writer.rename(op.id, op.to);
          else if (op.action === 'move') await writer.move(op.id, op.from, op.toId);
          else if (op.action === 'shortcut') op.made = (await writer.createShortcut(op.id, op.name, op.toId)).id;
          else if (op.how === 'bin') await writer.trash(op.id);
          else await writer.rename(op.id, op.to);
          step(op);
        } catch (e) {
          step(op, e);
        }
      }
    }),
  );
  return ops;
}

/**
 * Takes back the changes made (`done`), the last first: names and folders as they were, files out
 * of the bin, shortcuts binned. Folders made stay. Answers { undone, failed }.
 */
export async function undoFileChanges(writer, ops, { onStep = null } = {}) {
  let undone = 0;
  let failed = 0;
  for (const op of [...ops].reverse()) {
    if (!op.done || op.action === 'folder') continue;
    try {
      if (op.action === 'move') await writer.move(op.id, op.toId, op.from);
      else if (op.action === 'shortcut') await writer.trash(op.made);
      else if (op.action === 'remove' && op.how === 'bin') await writer.untrash(op.id);
      else await writer.rename(op.id, op.name);
      undone++;
    } catch {
      failed++;
    }
    onStep?.(undone, failed);
  }
  return { undone, failed };
}

/** What an op is, for counts and the card: rename, move, bin, mark (renamed …_todelete), folder, shortcut. */
export const kindOf = op => (op.action === 'remove' ? (op.how === 'bin' ? 'bin' : 'mark') : op.action);

/** A plan as the model is told it: the counts, a few of each, what was left out. */
export function describeChanges(ops, problems) {
  const groups = {};
  for (const op of ops) (groups[kindOf(op)] ??= []).push(op);
  const show = op =>
    op.action === 'folder'
      ? `${op.folder}/${op.name}`
      : op.action === 'remove'
        ? `${op.folder}/${op.name}`
        : `${op.folder}/${op.name} → ${op.to}`;
  const names = { rename: 'rename', move: 'move', bin: 'binned', mark: 'renamedToDelete', folder: 'newFolders', shortcut: 'shortcuts' };
  const out = {};
  for (const [kind, list] of Object.entries(groups))
    out[names[kind]] = {
      count: list.length,
      ...(list.some(o => o.raw) ? { rawFilesWithTheirPhoto: list.filter(o => o.raw).length } : {}),
      examples: list.filter(o => !o.raw).slice(0, EXAMPLES).map(show),
    };
  if (problems.length) out.leftOut = { count: problems.length, examples: problems.slice(0, 10) };
  return out;
}
