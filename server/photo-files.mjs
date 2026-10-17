// The photo folders on Google Drive put in order: files renamed, moved to another folder or removed,
// photos turned (their orientation tag changed, the same file and link: server/orientation.mjs),
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
//
// What only another account can do is left ready for it in that sheet (server/transfer-sheet.mjs):
// a `transfer` step writes the folder and the options in its cells, and the person signed in to
// that account opens it and presses the button. A plan that leaves …_todelete files to other
// accounts gets that step by itself.

import { readFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { randomUUID } from 'node:crypto';
import { FOLDER } from './drive.mjs';
import { readOrientation, turned, writeOrientation } from './orientation.mjs';
import { driveIdOf, rightName } from './photo-links.mjs';
import { TRANSFER_BUTTONS, TRANSFER_SHEET_NAME } from './transfer-sheet.mjs';

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
 *      transfer { step, in, subfolders, permanent }  the sheet «Transfer Ownership & Free Up Space» left ready
 *                                  for the folder `in`: step 'copy' (its «Copy files») or 'delete' (its «Delete
 *                                  copied files»; `permanent`: deleted for good, not binned). One a plan.
 *    `file` and the folders: a Drive link or ID, a path ("PERU_2024/JPG/CAM077740d.JPG") or a name no
 *    other has. A folder made by this plan can be named by its path in later operations; `to` may be
 *    the link of a folder outside the photo folders.
 *  - fix: [{ check, in }] the changes a check of the photos calls for, in the folder `in` and inside it:
 *      rename  every photo whose name the sheets do not find takes the name they look up
 *      copies  every file of `in` that is also, the same file, in another listed folder is removed
 *              (`keptIn`: only those whose other file is in that folder)
 *      copied  every …_todelete file whose copy is beside it is binned (the account's own files);
 *              `permanent`: deleted for good instead, which frees its space at once and cannot be taken back
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
  /** The names the plan frees (a file renamed, moved or removed), by folder and name: the file that frees it. */
  const freed = new Map();
  const free = item => {
    if (inFolder(item.parent).get(lower(item.name)) !== item) return;
    inFolder(item.parent).delete(lower(item.name));
    freed.set(`${item.parent}|${lower(item.name)}`, item.id);
  };
  /** Renames into a name a file not changed yet holds: settled once the whole plan is known (that file may give it up). */
  const waiting = [];
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
    const holder = inFolder(item.parent).get(lower(to));
    if (holder && holder.id !== item.id) {
      // Its file may give the name up further on (renamed, moved, removed: a swap of two names too).
      if (touched.has(holder.id) && !waiting.some(w => w.item === holder)) return problems.push({ file: wholePath(item), error: taken2(to) }) && false;
      if (touched.has(item.id)) return problems.push({ file: wholePath(item), error: `two changes of it (${touched.get(item.id)} and rename)` }) && false;
      touched.set(item.id, 'rename');
      waiting.push({ item, to, extra });
      return true;
    }
    // A name given up by another change of the plan: made after it.
    const after = freed.get(`${item.parent}|${lower(to)}`);
    if (!add({ action: 'rename', to, ...extra, ...(after ? { after } : {}) }, item)) return false;
    free(item);
    inFolder(item.parent).set(lower(to), item);
    return true;
  };
  const remove = (item, extra = {}) => {
    if (TO_DELETE.test(item.name) || !add({ action: 'remove', ...extra }, item)) return false;
    free(item);
    return true;
  };
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
    if (action === 'transfer') {
      const step = lower(o.step);
      const where = ['copy', 'delete'].includes(step) ? findFolder(o.in) : { error: 'transfer: step is copy or delete' };
      if (where.error || where.fresh !== undefined || ops.some(x => x.action === 'transfer')) {
        problems.push({ at, error: where.error ?? (where.fresh !== undefined ? 'transfer: the folder is not made yet' : 'the sheet takes one folder at a time: one transfer a plan') });
        continue;
      }
      const item = where.item;
      ops.push({
        action: 'transfer',
        step,
        id: item?.id ?? where.outside,
        name: item?.name ?? String(o.in).slice(0, 200),
        folder: item ? pathOfFolder(item.parent) : '',
        subfolders: o.subfolders !== false,
        permanent: o.permanent === true,
        ...(item?.webViewLink ? { url: item.webViewLink } : {}),
        ...(item ? {} : { outside: true }),
      });
      continue;
    }
    if (!['rename', 'move', 'remove', 'rotate'].includes(action)) {
      problems.push({ at, error: `action is rename, move, remove, rotate, folder, shortcut or transfer (${String(o?.action).slice(0, 30)} given)` });
      continue;
    }
    const found = find(o.file, action === 'move' ? () => true : f => !isFolder(f), 'file');
    if (found.error) {
      problems.push({ at, error: found.error });
      continue;
    }
    const item = found.item;
    if (action === 'rotate') {
      // A quarter, a half or three quarters clockwise; its RAW file the same. Beside a rename of the same file.
      const asked = Number(o.degrees);
      const degrees = Number.isInteger(asked / 90) ? ((asked % 360) + 360) % 360 : 0;
      if (!degrees || !PHOTO.test(item.name)) {
        problems.push({ at, error: degrees ? `${item.name} is not a photo` : 'rotate: give degrees, 90, 180 or 270 clockwise' });
        continue;
      }
      if (add({ action: 'rotate', degrees }, item, `${item.id}|rotate`))
        for (const raw of rawsOf(item)) add({ action: 'rotate', degrees, raw: true }, raw, `${raw.id}|rotate`);
      continue;
    }
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
      if (moved) free(item);
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
    const kept = check === 'copies' && f.keptIn ? find(f.keptIn, isFolder, 'folder') : null;
    if (where?.error || kept?.error || (check === 'copies' && !where)) {
      problems.push({ at, error: where?.error ?? kept?.error ?? 'copies: give the folder to remove them from (`in`)' });
      continue;
    }
    const under = (item, folderId) => {
      for (let p = item.parent; p; p = items.get(p)?.parent) if (p === folderId) return true;
      return false;
    };
    const inside = item => !where || under(item, where.item.id);
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
      const elsewhere = new Map();
      for (const item of all)
        if (listedFile(item) && !TO_DELETE.test(item.name) && !inside(item) && (!kept || under(item, kept.item.id)) && !elsewhere.has(key(item))) elsewhere.set(key(item), item);
      for (const item of all)
        if (listedFile(item) && !TO_DELETE.test(item.name) && inside(item) && item.size && elsewhere.has(key(item)) && !touched.has(item.id))
          remove(item, { copy: true, keptIn: pathOfFolder(elsewhere.get(key(item)).parent) });
    } else {
      // Left by «Copy files»: the original, renamed …_todelete, beside its copy (its name, its size).
      for (const item of all) {
        if (isFolder(item) || !TO_DELETE.test(item.name) || !inside(item) || touched.has(item.id)) continue;
        const copy = inFolder(item.parent).get(lower(item.name.replace(TO_DELETE, '')));
        if (copy && copy.id !== item.id && String(copy.size ?? '') === String(item.size ?? ''))
          add({ action: 'remove', copied: true, copyId: copy.id, size: Number(item.size) || 0, ...(f.permanent === true ? { permanent: true } : {}) }, item);
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
  settleWaiting();
  if (ops.length >= MAX_OPERATIONS) problems.push({ error: `more than ${MAX_OPERATIONS} changes: the first ${MAX_OPERATIONS} are planned; ask again for the rest` });
  return { ops, problems };

  /**
   * The renames waiting for a name go when, the plan done, no other file has it: its file renamed away
   * in the plan too (a chain, or a swap of names). Each runs after the change that frees its name; in
   * a swap, the first file takes a name of its own on the way (`via`), so a failed step leaves no two
   * files with one name.
   */
  function settleWaiting() {
    let open = waiting;
    for (let dropped = true; dropped && open.length; ) {
      dropped = false;
      const leaving = new Set(open.map(w => w.item.id));
      const aimed = new Map();
      for (const w of open) aimed.set(`${w.item.parent}|${lower(w.to)}`, (aimed.get(`${w.item.parent}|${lower(w.to)}`) ?? 0) + 1);
      open = open.filter(w => {
        const holder = inFolder(w.item.parent).get(lower(w.to));
        const twice = aimed.get(`${w.item.parent}|${lower(w.to)}`) > 1;
        if ((!holder || holder.id === w.item.id || leaving.has(holder.id)) && !twice) return true;
        problems.push({ file: wholePath(w.item), error: twice ? `two files would be named ${w.to}` : taken2(w.to) });
        touched.delete(w.item.id);
        dropped = true;
        return false;
      });
    }
    const holderOf = new Map(open.map(w => [w.item.id, inFolder(w.item.parent).get(lower(w.to))?.id ?? null]));
    for (const w of open) free(w.item);
    for (const w of open) inFolder(w.item.parent).set(lower(w.to), w.item);
    // A file whose name another waiting rename takes goes first; a cycle (a swap) starts by its first file's step aside.
    const byItem = new Map(open.map(w => [w.item.id, w]));
    const placed = new Set();
    for (const start of open) {
      if (placed.has(start.item.id)) continue;
      const chain = [];
      let w = start;
      while (w && !placed.has(w.item.id) && !chain.includes(w)) {
        chain.push(w);
        w = byItem.get(holderOf.get(w.item.id));
      }
      const cycle = w && chain.includes(w) ? chain.slice(chain.indexOf(w)) : null;
      // From the end of the chain (the file whose name is free) back to the start.
      for (const x of chain.reverse()) {
        placed.add(x.item.id);
        const after = holderOf.get(x.item.id) ?? freed.get(`${x.item.parent}|${lower(x.to)}`) ?? null;
        const via = cycle && x === cycle[0] ? viaName(x.item) : null;
        if (ops.length >= MAX_OPERATIONS) break;
        ops.push({
          action: 'rename',
          to: x.to,
          ...x.extra,
          ...(after ? { after } : {}),
          ...(via ? { via } : {}),
          id: x.item.id,
          name: x.item.name,
          folder: pathOfFolder(x.item.parent),
          ...(x.item.webViewLink ? { url: x.item.webViewLink } : {}),
        });
      }
    }
  }
  /** A name of its own for a file stepping aside in a swap: its name, marked. */
  function viaName(item) {
    let n = 1;
    const name = k => `${stemOf(item.name)}__swap${k > 1 ? k : ''}${extOf(item.name)}`;
    while (inFolder(item.parent).has(lower(name(n)))) n++;
    return name(n);
  }
  function taken2(to) {
    return `another file of its folder keeps the name ${to} (the sheets take one file of a name): rename, move or remove that one in the same plan`;
  }
}

/** The folder holding all of these files, within the folders read: { id, name, path }, or null. */
function folderHolding(index, fileIds) {
  const { items, home, folderPath } = index;
  let shared = null;
  for (const id of fileIds) {
    const chain = [];
    for (let p = items.get(id)?.parent; p; p = p === home ? null : items.get(p)?.parent) chain.push(p);
    shared = shared ? shared.filter(p => chain.includes(p)) : chain;
  }
  const id = shared?.[0];
  return id && items.has(id) ? { id, name: items.get(id).name, path: folderPath(id) ?? items.get(id).name, url: items.get(id).webViewLink } : null;
}

/** The files of a folder (and of the folders inside it) counted by the account that owns them, the most first. */
function owners(index, folderId, subfolders, want) {
  const { items } = index;
  const counts = new Map();
  for (const item of items.values()) {
    if (isFolder(item) || !want(item)) continue;
    let inside = item.parent === folderId;
    if (!inside && subfolders) for (let p = items.get(item.parent)?.parent; p && !inside; p = items.get(p)?.parent) inside = p === folderId;
    if (!inside) continue;
    const account = item.owners?.[0]?.emailAddress ?? '';
    counts.set(account, (counts.get(account) ?? 0) + 1);
  }
  return [...counts].map(([account, files]) => ({ account, files })).sort((a, b) => b.files - a.files);
}

/**
 * What the account that changes files may do: each `remove` is a file binned when it is the
 * account's own (`how: 'bin'`), renamed …_todelete otherwise (`how: 'mark'`); a …_todelete file that
 * is someone else's, and a `to` that is no folder, are left out (added to `problems`).
 * With the sheet «Transfer Ownership & Free Up Space» (`sheet`, and the `index` the plan was made
 * from): the …_todelete files left to other accounts get a `transfer` step for the folder holding
 * them, and a `transfer` step says which accounts own the files concerned (`by`).
 */
export async function settleChanges(writer, { ops, problems }, { parallel = 8, index = null, sheet = null } = {}) {
  const left = new Set();
  const asked = new Map();
  const ask = id => asked.get(id) ?? asked.set(id, writer.get(id).catch(() => null)).get(id);
  const queue = ops.filter(op => op.action === 'remove' || op.outside);
  const others = [];
  let notOwn = 0;
  await Promise.all(
    Array.from({ length: parallel }, async () => {
      while (queue.length) {
        const op = queue.shift();
        if (op.outside) {
          const folder = await ask(op.action === 'transfer' ? op.id : op.toId);
          if (folder?.mimeType !== FOLDER) {
            left.add(op);
            problems.push({ file: [op.folder, op.name].filter(Boolean).join('/'), error: `${op.action === 'transfer' ? op.name : op.to} is not a folder the account can open` });
          } else if (op.action === 'transfer') Object.assign(op, { name: folder.name, ...(folder.webViewLink ? { url: folder.webViewLink } : {}) });
          else op.to = folder.name;
          continue;
        }
        const item = await ask(op.id);
        if (item?.ownedByMe) op.how = op.permanent ? 'delete' : 'bin';
        else if (op.copied) {
          left.add(op);
          others.push(op.id);
          notOwn++;
        } else {
          op.how = 'mark';
          op.to = `${op.name}_todelete`;
          others.push(op.id);
        }
      }
    }),
  );
  if (notOwn) problems.push({ error: `${notOwn} …_todelete files belong to another account: their owner deletes them («${TRANSFER_BUTTONS.delete}» in the sheet «${TRANSFER_SHEET_NAME}»)` });

  // What is left to other accounts: the sheet ready for the folder holding it.
  let transfer = ops.find(op => op.action === 'transfer' && !left.has(op));
  if (!transfer && others.length && sheet && index && !ops.some(op => op.action === 'transfer')) {
    const holder = folderHolding(index, others);
    if (holder) {
      const { items } = index;
      transfer = { action: 'transfer', step: 'delete', id: holder.id, name: holder.name, folder: index.folderPath(items.get(holder.id)?.parent) ?? '', subfolders: true, permanent: false, auto: true, ...(holder.url ? { url: holder.url } : {}) };
      ops.push(transfer);
    }
  }
  if (transfer) {
    const now = sheet ? await sheet.read().catch(e => ({ error: e?.message || String(e) })) : { error: `this app has no sheet «${TRANSFER_SHEET_NAME}» set up` };
    const going = now.busy ? `the sheet «${TRANSFER_SHEET_NAME}» is in use (${now.last.action} of ${now.last.folder}: ${now.last.status}, ${now.last.hoursAgo} h ago): when that run ends, ask for this step again` : now.error;
    if (going) {
      left.add(transfer);
      problems.push({ file: [transfer.folder, transfer.name].filter(Boolean).join('/'), error: going });
    } else {
      if (sheet.url) transfer.sheetUrl = sheet.url;
      if (now.last) transfer.lastRun = `${now.last.action} of ${now.last.folder}: ${now.last.files} files, ${now.last.status}${now.last.runtime ? `, ${now.last.runtime}` : ''}`;
      if (index && !transfer.outside) {
        // «Delete copied files»: the accounts whose …_todelete files are there, with those this plan marks.
        // «Copy files»: the accounts whose files are there now (the one that presses it copies the others').
        const marked = new Set(ops.filter(op => op.how === 'mark' && !left.has(op)).map(op => op.id));
        const binned = new Set(ops.filter(op => op.how === 'bin' || op.how === 'delete').map(op => op.id));
        transfer.by = owners(
          index,
          transfer.id,
          transfer.subfolders,
          transfer.step === 'delete' ? f => marked.has(f.id) || (TO_DELETE.test(f.name) && !binned.has(f.id)) : f => !TO_DELETE.test(f.name),
        ).slice(0, 12);
      }
    }
  }
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
export async function runFileChanges(writer, ops, { onStep = null, parallel = 4, sheet = null } = {}) {
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
  // A change into a name another change frees waits for it (`after`); the first file of a swap steps aside
  // to a name of its own first (`via`) and takes its new name last. A change only waits for one taken from
  // the queue before it, so the workers never all wait.
  const changes = ops.filter(o => o.action !== 'folder' && o.action !== 'transfer');
  const freeing = new Map();
  for (const op of changes)
    if (op.action === 'rename' || op.action === 'move' || op.action === 'remove') {
      let settle;
      const promise = new Promise(resolve => (settle = resolve));
      freeing.set(op.id, { promise, settle });
    }
  const gaveUpName = (op, ok) => freeing.get(op.id)?.settle(ok);
  const nameFreed = async op => {
    const by = op.after && freeing.get(op.after);
    if (by && !(await by.promise)) throw new Error('the file that had its name was not changed: left as it is');
  };
  const queue = [
    ...changes.filter(o => o.via).map(op => ({ op, step: 'via' })),
    ...changes.filter(o => !o.via).map(op => ({ op })),
    ...changes.filter(o => o.via).map(op => ({ op, step: 'last' })),
  ];
  await Promise.all(
    Array.from({ length: parallel }, async () => {
      while (queue.length) {
        const { op, step: part } = queue.shift();
        if (part === 'via') {
          try {
            await writer.rename(op.id, op.via);
            gaveUpName(op, true);
          } catch (e) {
            step(op, e);
            gaveUpName(op, false);
          }
          continue;
        }
        if (part === 'last') {
          if (op.error) continue;
          try {
            await nameFreed(op);
            await writer.rename(op.id, op.to);
            step(op);
          } catch (e) {
            // Its own name taken meanwhile: it keeps the one it stepped aside to (undo gives it its name back).
            op.leftAs = op.via;
            step(op, new Error(`${e.message || e} (left named ${op.via})`));
          }
          continue;
        }
        try {
          await nameFreed(op);
          if (op.toNew !== undefined) {
            if (!ops[op.toNew]?.done) throw new Error('its folder could not be made');
            op.toId = ops[op.toNew].id;
          }
          if (op.action === 'rename') await writer.rename(op.id, op.to);
          else if (op.action === 'rotate') op.tagBefore = await rotateFile(writer, op.id, tag => turned(tag, op.degrees), op.name);
          else if (op.action === 'move') await writer.move(op.id, op.from, op.toId);
          else if (op.action === 'shortcut') op.made = (await writer.createShortcut(op.id, op.name, op.toId)).id;
          else if (op.how === 'bin' || op.how === 'delete') {
            // An original is removed only while its copy is still there, the size it had.
            if (op.copyId) {
              const copy = await writer.get(op.copyId);
              if (!copy || copy.gone || (op.size && Number(copy.size) !== op.size)) throw new Error('its copy is no longer beside it');
            }
            if (op.how === 'delete') await writer.delete(op.id);
            else await writer.trash(op.id);
          } else await writer.rename(op.id, op.to);
          step(op);
          gaveUpName(op, true);
        } catch (e) {
          step(op, e);
          gaveUpName(op, false);
        }
      }
    }),
  );
  // Last, the sheet left ready for the account that does the rest.
  for (const op of ops.filter(o => o.action === 'transfer')) {
    try {
      if (!sheet) throw new Error(`this app has no sheet «${TRANSFER_SHEET_NAME}» set up`);
      const now = await sheet.read();
      if (now.busy) throw new Error(`the sheet is in use (${now.last.action} of ${now.last.folder}: ${now.last.status})`);
      await sheet.write({ folder: op.id, subfolders: op.subfolders, ...(op.step === 'delete' ? { permanent: op.permanent } : {}) });
      step(op);
    } catch (e) {
      step(op, e);
    }
  }
  return ops;
}

/**
 * Takes back the changes made (`done`), the last first: names and folders as they were, files out
 * of the bin, shortcuts binned. Folders made stay, and what was written in the sheet «Transfer
 * Ownership & Free Up Space». Answers { undone, failed }.
 */
export async function undoFileChanges(writer, ops, { onStep = null } = {}) {
  let undone = 0;
  let failed = 0;
  let gone = 0;
  for (const op of [...ops].reverse()) {
    if ((!op.done && !op.leftAs) || op.action === 'folder' || op.action === 'transfer') continue;
    // Deleted for good: nothing to bring back.
    if (op.how === 'delete') {
      gone++;
      continue;
    }
    try {
      if (op.action === 'move') await writer.move(op.id, op.toId, op.from);
      else if (op.action === 'rotate') await rotateFile(writer, op.id, () => op.tagBefore, op.name);
      else if (op.action === 'shortcut') await writer.trash(op.made);
      else if (op.action === 'remove' && op.how === 'bin') await writer.untrash(op.id);
      else await writer.rename(op.id, op.name);
      undone++;
    } catch {
      failed++;
    }
    onStep?.(undone, failed);
  }
  return { undone, failed, ...(gone ? { deletedForGood: gone } : {}) };
}

/**
 * A file's orientation tag set to `to(tag it has)`: its bytes read, the tag changed, the same file given
 * them back (its ID and link kept). Answers the tag it had; throws when it has none to change.
 */
async function rotateFile(writer, id, to, name = '') {
  const path = join(tmpdir(), `ithomiini-rotate-${randomUUID()}`);
  try {
    await writer.download(id, path);
    const bytes = new Uint8Array(readFileSync(path));
    const tag = readOrientation(bytes);
    if (!tag) throw new Error('it has no orientation tag to change');
    const value = to(tag.value);
    if (!value) throw new Error(`its orientation tag (${tag.value}) is mirrored: turn it by hand`);
    if (value !== tag.value) await writer.replaceContent(id, writeOrientation(bytes, value), /\.jpe?g$/i.test(name) ? 'image/jpeg' : 'application/octet-stream');
    return tag.value;
  } finally {
    rmSync(path, { force: true });
  }
}

/** What an op is, for counts and the card: rename, move, bin, delete (for good), mark (renamed …_todelete), folder, shortcut, rotate, sheet (a `transfer` step). */
export const kindOf = op => (op.action === 'remove' ? (op.how === 'bin' ? 'bin' : op.how === 'delete' ? 'delete' : 'mark') : op.action === 'transfer' ? 'sheet' : op.action);

/** A `transfer` step as the card and the model are told it: what the sheet is left ready for, and who presses what. */
export const transferOf = op =>
  op && {
    step: op.step,
    button: TRANSFER_BUTTONS[op.step],
    folder: [op.folder, op.name].filter(Boolean).join('/'),
    ...(op.url ? { folderUrl: op.url } : {}),
    sheet: TRANSFER_SHEET_NAME,
    ...(op.sheetUrl ? { sheetUrl: op.sheetUrl } : {}),
    subfolders: op.subfolders,
    ...(op.step === 'delete' ? { permanent: op.permanent } : {}),
    ...(op.by?.length ? { by: op.by } : {}),
    ...(op.lastRun ? { lastRun: op.lastRun } : {}),
    ...(op.auto ? { auto: true } : {}),
  };

/** How many of each, the most first: { name: count }. */
const tally = names => {
  const counts = new Map();
  for (const name of names) counts.set(name, (counts.get(name) ?? 0) + 1);
  return Object.fromEntries([...counts].sort((a, b) => b[1] - a[1]).slice(0, 10));
};

/** A plan as the model is told it: the counts, a few of each, what was left out. */
export function describeChanges(ops, problems) {
  const groups = {};
  for (const op of ops) (groups[kindOf(op)] ??= []).push(op);
  const show = op =>
    op.action === 'folder'
      ? `${op.folder}/${op.name}`
      : op.action === 'remove'
        ? `${op.folder}/${op.name}`
        : op.action === 'rotate'
          ? `${op.folder}/${op.name} ↻ ${op.degrees}°`
          : `${op.folder}/${op.name} → ${op.to}`;
  const names = { rename: 'rename', move: 'move', rotate: 'turned', bin: 'binned', delete: 'deletedForGood', mark: 'renamedToDelete', folder: 'newFolders', shortcut: 'shortcuts' };
  const out = {};
  for (const [kind, list] of Object.entries(groups)) {
    if (kind === 'sheet') {
      const t = transferOf(list[0]);
      out.otherAccountStep = {
        ...t,
        then:
          t.step === 'delete'
            ? `Once applied, each account of \`by\` opens the sheet signed in to that account and presses «${t.button}»: it ${t.permanent ? 'deletes for good' : 'bins'} the …_todelete files that account owns there.`
            : `Once applied, the account that takes the photos over opens the sheet signed in to that account and presses «${t.button}»: it copies the files the other accounts own there (\`by\`) and renames the originals …_todelete. Then sync_photo_links moves the rows to the copies.`,
      };
      continue;
    }
    out[names[kind]] = {
      count: list.length,
      ...(list.some(o => o.raw) ? { rawFilesWithTheirPhoto: list.filter(o => o.raw).length } : {}),
      ...(list.some(o => o.keptIn) ? { sameFileKeptIn: tally(list.filter(o => o.keptIn).map(o => o.keptIn)) } : {}),
      ...(kind === 'delete' || kind === 'bin' ? { gb: Math.round((list.reduce((sum, o) => sum + (o.size || 0), 0) / 2 ** 30) * 100) / 100 || undefined } : {}),
      examples: list.filter(o => !o.raw).slice(0, EXAMPLES).map(show),
    };
  }
  if (problems.length) out.leftOut = { count: problems.length, examples: problems.slice(0, 10) };
  return out;
}
