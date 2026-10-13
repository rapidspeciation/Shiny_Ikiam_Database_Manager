// Photo_links kept up to date from Google Drive: the rows the sheet's script «📷 Photo Database
// Tools» writes (list_google_drive_files.gs, in the Wings Gallery's repository), worked out here
// from one reading of the photo folders (server/drive.mjs), so the app can write them.
//
// The sheet lists every folder and file under its listed folders: the Folder rows whose Full Path
// is a single name (the folders the script was run on), with everything below them except folders
// named …_temp. One row per file name: Photo_dorsal and Photo_ventral look a photo up by its name
// (<CAM_ID>d.JPG, <CAM_ID>v.JPG), so a second file of the same name would never be reached.
//
// A row is its Drive file (the ID in its URL), so a file renamed or moved in Drive keeps its row:
//  - renamed, moved, another owner or size → the row's cells follow;
//  - deleted, with a file of the same name now in the listed folders (photos uploaded again,
//    copies made by another account) → the row takes the new file's link;
//  - deleted otherwise → the row is emptied;
//  - new files and folders → rows at the end, a folder's row before its files.
// Files named …_todelete are left out, as are the folders named …_temp.

import { FOLDER, driveTree } from './drive.mjs';

export const SHEET = 'Photo_links';
const FIELDS = ['Full Path', 'Name', 'Type', 'Capture Date', 'URL', 'Date', 'Description', 'Size KB', 'Owner Email'];
/** What the sheet's script leaves in the last row while and after it runs. */
const STATUS_ROW = /^\s*(?:⏳|🛑|✓)/u;
const SET_ASIDE = /_temp$/i;
const TO_DELETE = /_todelete$/i;
const PHOTO = /\.(jpe?g|png|heic|heif|jfif)$/i;
const RAW = /\.(cr2|cr3|orf|nef|arw|dng|raf|rw2|pef|tiff?)$/i;
/** Rows whose file the listing lacks, asked one by one whether they still exist (the rest is left as it is). */
const LOOKUPS = 400;
const EXAMPLES = 6;

const blank = v => v === null || v === undefined || String(v).trim() === '';
const lower = v => String(v ?? '').toLowerCase();

/** The Drive ID in a Photo_links URL (a file's …/file/d/<id>/view, a folder's …/folders/<id>). */
export function driveIdOf(url) {
  const m = /\/d\/([\w-]{20,})|\/folders\/([\w-]{20,})|[?&]id=([\w-]{20,})/.exec(String(url ?? ''));
  return m ? m[1] || m[2] || m[3] : null;
}

const SHEETS_EPOCH = Date.UTC(1899, 11, 30);
const formatters = new Map();
/** A moment as the sheet's date-time number, in the workbook's time zone (as Apps Script writes a Date). */
export function sheetSerial(ms, timeZone) {
  let format = formatters.get(timeZone);
  if (!format)
    formatters.set(
      timeZone,
      (format = new Intl.DateTimeFormat('en-US', {
        timeZone,
        hourCycle: 'h23',
        year: 'numeric',
        month: 'numeric',
        day: 'numeric',
        hour: 'numeric',
        minute: 'numeric',
        second: 'numeric',
      })),
    );
  const p = Object.fromEntries(format.formatToParts(new Date(ms)).map(x => [x.type, Number(x.value)]));
  const local = Date.UTC(p.year, p.month - 1, p.day, p.hour, p.minute, p.second) + (ms % 1000);
  return (local - SHEETS_EPOCH) / 86_400_000;
}
/**
 * The camera's capture time ("2026:05:06 20:15:28") as the script writes it: read as if it were
 * UTC, then shown in the workbook's zone. Kept the same, so old and new rows sort together.
 */
export function captureSerial(time, timeZone) {
  const m = /(\d{4}):(\d{2}):(\d{2}) (\d{2}):(\d{2}):(\d{2})/.exec(String(time ?? ''));
  return m ? sheetSerial(Date.UTC(+m[1], m[2] - 1, +m[3], +m[4], +m[5], +m[6]), timeZone) : null;
}

const isFolder = item => item.mimeType === FOLDER;
const sizeKb = item => (isFolder(item) ? 0 : (Number(item.size) || 0) / 1024);
const owner = item => item.owners?.[0]?.emailAddress ?? null;
const sameSize = (a, b) => Math.abs((Number(a) || 0) - (Number(b) || 0)) < 0.01;

/** A file name as a specimen photo's: { id, view, extra, ext } ("CAM077884_D 2.jpg" → CAM077884, d, 2), or null. */
export function specimenName(name) {
  const dot = String(name ?? '').lastIndexOf('.');
  if (dot <= 0) return null;
  const m = /^(.*?\d)[\s_-]*([dv])[\s_-]*(\d*)$/i.exec(name.slice(0, dot).trim());
  return m ? { id: m[1], view: m[2].toLowerCase(), extra: m[3], ext: name.slice(dot + 1) } : null;
}
/** The database's ID a name's ID stands for: itself, or without separators, or with the letter O of "CAMO…" as a zero. */
function knownId(id, ids) {
  const tries = [id, id.replace(/[\s_-]+/g, ''), id.replace(/[\s_-]+/g, '').replace(/^CAM[O0-9]+$/i, s => s.replace(/O/gi, '0'))];
  return tries.map(t => t.toUpperCase()).find(t => ids.has(t)) ?? null;
}

/**
 * What Photo_links needs to match Drive.
 *  - rows: the sheet's rows [{ id, row, values }], in row order.
 *  - items: the Drive items (driveTree: Map id → item with `parent`).
 *  - home: the folder holding the listed folders (its other folders are offered as not listed yet).
 *  - add: Drive IDs of folders to start listing.
 *  - lookups: Map Drive ID → drive.get's answer, for rows whose file `items` lacks (`unknown` in the answer).
 *  - ids: the specimen IDs of the database, in capitals (the new photos' names are checked against them).
 * Answers { edits, creates, unknown, summary }: `edits` [{ id, values, expected }] and `creates`
 * [{ module, values }] as server/batch.mjs takes them.
 */
export function planPhotoLinks({ rows, items, home = null, add = [], lookups = new Map(), ids = new Set(), timeZone = 'Europe/London' }) {
  const status = [];
  const listed = new Map();
  let repeated = 0;
  let unread = 0;
  for (const r of rows) {
    const id = driveIdOf(r.values.URL);
    if (id) {
      if (listed.has(id)) repeated++;
      else listed.set(id, r);
    } else if (STATUS_ROW.test(r.values['Full Path'] ?? '') && blank(r.values.Name)) status.push(r);
    else if (FIELDS.some(f => !blank(r.values[f]))) unread++;
  }

  const roots = new Set(add.filter(id => items.has(id) && isFolder(items.get(id))));
  for (const [id, r] of listed)
    if (r.values.Type === 'Folder' && !String(r.values['Full Path'] ?? '').includes('/') && items.has(id) && isFolder(items.get(id)))
      roots.add(id);

  // Where a folder is listed: under its nearest listed folder, by the path from it.
  const places = new Map();
  const placeOf = folderId => {
    if (places.has(folderId)) return places.get(folderId);
    const folder = items.get(folderId);
    let out = null;
    if (folder && roots.has(folderId)) out = { root: folderId, path: folder.name };
    else if (folder && folder.parent && !SET_ASIDE.test(folder.name)) {
      places.set(folderId, null);
      const up = placeOf(folder.parent);
      out = up && { root: up.root, path: `${up.path}/${folder.name}` };
    }
    places.set(folderId, out);
    return out;
  };
  const pathOf = item => {
    if (isFolder(item)) return placeOf(item.id)?.path ?? null;
    const place = item.parent ? placeOf(item.parent) : null;
    return place ? `${place.path}/${item.name}` : null;
  };
  const wholePath = item => {
    const parts = [];
    for (let f = item; f; f = f.parent ? items.get(f.parent) : null) parts.unshift(f.name);
    return parts.join('/');
  };
  const cells = (item, path) => ({
    'Full Path': path,
    Name: item.name,
    Type: isFolder(item) ? 'Folder' : 'File',
    'Capture Date': captureSerial(item.imageMediaMetadata?.time, timeZone),
    URL: item.webViewLink,
    Date: item.createdTime ? sheetSerial(Date.parse(item.createdTime), timeZone) : null,
    Description: item.description || null,
    'Size KB': sizeKb(item),
    'Owner Email': owner(item),
  });

  const edits = [];
  const edit = (r, values, kind) => {
    const changed = Object.fromEntries(Object.entries(values).filter(([f, v]) => !same(f, r.values[f], v)));
    if (!Object.keys(changed).length) return false;
    edits.push({ id: r.id, row: r.row, kind, values: changed, expected: Object.fromEntries(Object.keys(changed).map(f => [f, r.values[f] ?? null])) });
    return true;
  };
  const summary = {
    renamed: [],
    moved: 0,
    details: 0,
    relinked: [],
    removed: [],
    elsewhere: 0,
    unchecked: 0,
    outside: 0,
    repeated,
    unread,
    statusRows: status.length,
  };
  for (const r of status) edit(r, { 'Full Path': null }, 'status');

  // Rows whose file Drive still has.
  const missing = [];
  const emptied = new Set();
  for (const [id, r] of listed) {
    const item = items.get(id);
    if (!item) {
      missing.push([id, r]);
      continue;
    }
    const path = pathOf(item);
    if (!path) {
      summary.outside++;
      continue;
    }
    const renamed = r.values.Name !== item.name;
    // A path written from a folder further down (the script run on a subfolder) is still right.
    const whole = `/${wholePath(item)}`;
    const moved = !renamed && !whole.endsWith(`/${r.values['Full Path']}`);
    const detail = lower(r.values['Owner Email']) !== lower(owner(item)) || (!isFolder(item) && !sameSize(r.values['Size KB'], sizeKb(item)));
    if (!renamed && !moved && !detail) continue;
    edit(
      r,
      { ...(renamed || moved ? { 'Full Path': path, Name: item.name } : {}), 'Size KB': sizeKb(item), 'Owner Email': owner(item) },
      renamed ? 'rename' : moved ? 'move' : 'details',
    );
    if (renamed) summary.renamed.push({ row: r.row, from: r.values.Name, to: item.name });
    else if (moved) summary.moved++;
    else summary.details++;
  }

  // Files of the listed folders without a row, by name.
  const taken = new Set();
  const relinkedTo = new Map();
  const free = new Map();
  for (const item of items.values()) {
    if (listed.has(item.id) || isFolder(item) || TO_DELETE.test(item.name) || !pathOf(item)) continue;
    const key = lower(item.name);
    if (!free.has(key)) free.set(key, []);
    free.get(key).push(item);
  }
  // Rows whose file the listing lacks.
  const unknown = [];
  for (const [id, r] of missing) {
    const again = r.values.Type === 'File' ? free.get(lower(r.values.Name))?.find(f => !taken.has(f.id)) : null;
    if (again) {
      taken.add(again.id);
      relinkedTo.set(r.id, again.id);
      const { Name: _name, Type: _type, ...link } = cells(again, pathOf(again));
      edit(r, link, 'relink');
      summary.relinked.push({ row: r.row, name: r.values.Name });
      continue;
    }
    const found = lookups.get(id);
    if (found === undefined) {
      unknown.push(id);
      summary.unchecked++;
    } else if (found === null) summary.unchecked++;
    else if (found.gone) {
      edit(r, Object.fromEntries(FIELDS.map(f => [f, null])), 'remove');
      emptied.add(r.id);
      summary.removed.push({ row: r.row, path: r.values['Full Path'] });
    } else summary.elsewhere++;
  }

  // Names the sheet holds once the edits are in.
  const names = new Map();
  const renamedTo = new Map(edits.filter(e => e.values.Name).map(e => [e.id, e.values.Name]));
  for (const r of rows) {
    if (emptied.has(r.id) || r.values.Type !== 'File') continue;
    const name = renamedTo.get(r.id) ?? r.values.Name;
    if (!blank(name) && !names.has(lower(name))) names.set(lower(name), relinkedTo.get(r.id) ?? driveIdOf(r.values.URL));
  }

  // New rows: each listed folder in the order Drive was read, its own row first, then its files by name.
  const creates = [];
  const byFolder = [];
  const newFolders = [];
  const sameName = { copies: 0, different: 0, examples: [] };
  let leftOut = 0;
  const added = [];
  const children = new Map();
  for (const item of items.values()) {
    if (!item.parent) continue;
    if (!children.has(item.parent)) children.set(item.parent, []);
    children.get(item.parent).push(item);
  }
  const byName = (a, b) => a.name.localeCompare(b.name, 'en', { numeric: true });
  for (const folder of items.values()) {
    if (!isFolder(folder)) continue;
    const place = placeOf(folder.id);
    if (!place) continue;
    let count = 0;
    if (!listed.has(folder.id)) {
      creates.push({ module: SHEET, values: cells(folder, place.path) });
      newFolders.push(place.path);
    }
    const files = (children.get(folder.id) || []).filter(f => !isFolder(f));
    for (const file of files.sort(byName)) {
      if (listed.has(file.id) || taken.has(file.id)) continue;
      if (TO_DELETE.test(file.name)) {
        leftOut++;
        continue;
      }
      const key = lower(file.name);
      if (names.has(key)) {
        const first = items.get(names.get(key));
        const copy = first && sameSize(first.size, file.size) && (first.imageMediaMetadata?.time ?? '') === (file.imageMediaMetadata?.time ?? '');
        if (copy) sameName.copies++;
        else if (++sameName.different <= EXAMPLES) sameName.examples.push({ name: file.name, in: place.path, listed: first ? pathOf(first) : null });
        continue;
      }
      names.set(key, file.id);
      creates.push({ module: SHEET, values: cells(file, `${place.path}/${file.name}`) });
      added.push(file);
      count++;
    }
    if (count) byFolder.push({ folder: place.path, new: count, files: files.length });
  }

  // Folders not listed: those beside the listed ones (offered), and the …_temp ones inside them (set aside).
  const groups = new Map();
  const groupOf = folderId => {
    if (groups.has(folderId)) return groups.get(folderId);
    const folder = items.get(folderId);
    let out = null;
    if (folder && !placeOf(folderId)) {
      groups.set(folderId, null);
      if (SET_ASIDE.test(folder.name) && folder.parent && placeOf(folder.parent)) out = { kind: 'aside', id: folderId };
      else if (home && folder.parent === home) out = { kind: 'beside', id: folderId };
      else if (folder.parent) out = groupOf(folder.parent);
    }
    groups.set(folderId, out);
    return out;
  };
  const tallies = new Map();
  for (const item of items.values()) {
    if (isFolder(item) || !item.parent) continue;
    const group = groupOf(item.parent);
    if (!group) continue;
    const t = tallies.get(group.id) ?? tallies.set(group.id, { kind: group.kind, files: 0, photos: 0, named: 0 }).get(group.id);
    t.files++;
    if (!PHOTO.test(item.name)) continue;
    t.photos++;
    const parsed = specimenName(item.name);
    if (parsed && knownId(parsed.id, ids)) t.named++;
  }
  const folderPath = id => {
    const parts = [];
    for (let f = items.get(id); f && f.id !== home; f = f.parent ? items.get(f.parent) : null) parts.unshift(f.name);
    return parts.join('/');
  };
  const notListed = [];
  const setAside = [];
  for (const [id, t] of tallies) {
    if (t.kind === 'aside') setAside.push({ folder: pathOf(items.get(items.get(id).parent)) + '/' + items.get(id).name, files: t.files });
    else if (t.named) notListed.push({ folder: folderPath(id), id, files: t.files, photos: t.photos, namedAsSpecimens: t.named });
  }
  notListed.sort((a, b) => b.namedAsSpecimens - a.namedAsSpecimens);

  return {
    edits,
    creates,
    unknown: unknown.slice(0, LOOKUPS),
    summary: {
      ...summary,
      newRows: creates.length,
      newFiles: added.length,
      newFolders,
      byFolder,
      sameName,
      leftOut,
      names: checkNames(added, ids, item => pathOf(items.get(item.parent))),
      notListed,
      setAside,
    },
  };
}

/** Cells compared as the sheet keeps them: numbers within a rounding, texts as they are, empty as empty. */
function same(field, before, after) {
  if (blank(before) && blank(after)) return true;
  if (typeof after === 'number' && typeof before === 'number')
    return field === 'Size KB' ? sameSize(before, after) : Math.abs(before - after) < 1e-6;
  return String(before ?? '') === String(after ?? '');
}

/**
 * The new photos' names against what the sheets look up: Photo_dorsal and Photo_ventral find
 * <ID>d.JPG and <ID>v.JPG (capitals aside) of an ID in the database (`linked`); the CRISPR sheet
 * also takes .HEIC and a first extra view (d1, v1), counted with the other extra views and formats
 * (`otherViews`). RAW files and other files are not looked up.
 */
function checkNames(files, ids, folderOf) {
  const out = { photos: 0, linked: 0, butterflies: 0, otherViews: 0, rename: [], unknownId: [], unnamed: [] };
  if (!ids.size) return null;
  const linked = new Set();
  const counts = { rename: 0, unknownId: 0, unnamed: 0 };
  const note = (kind, entry) => {
    counts[kind]++;
    if (out[kind].length < EXAMPLES) out[kind].push(entry);
  };
  for (const file of files) {
    if (!PHOTO.test(file.name) || RAW.test(file.name)) continue;
    out.photos++;
    const parsed = specimenName(file.name);
    if (!parsed) {
      note('unnamed', { name: file.name, in: folderOf(file) });
      continue;
    }
    const id = knownId(parsed.id, ids);
    if (!id) {
      note('unknownId', { name: file.name, in: folderOf(file) });
      continue;
    }
    // A .jpeg takes the .JPG the lookups expect; another format keeps its own.
    const jpeg = /^jpe?g$/i.test(parsed.ext);
    const right = `${id}${parsed.view}${parsed.extra}.${jpeg ? 'JPG' : parsed.ext}`;
    if (file.name.toUpperCase() !== right.toUpperCase()) {
      note('rename', { name: file.name, to: right, in: folderOf(file) });
      continue;
    }
    if (parsed.extra || !jpeg) out.otherViews++;
    else {
      out.linked++;
      linked.add(id);
    }
  }
  out.butterflies = linked.size;
  for (const kind of ['rename', 'unknownId', 'unnamed']) out[kind] = { count: counts[kind], examples: out[kind] };
  return out;
}

/** The specimen IDs photos are named after: the CAM_ID columns of the sheets that show photos. */
export function specimenIds(store) {
  const ids = new Set();
  const read = (sheet, field) => {
    for (const r of store.db
      .prepare(`SELECT DISTINCT json_extract(values_json, '$."${field}"') v FROM records WHERE sheet=? AND missing=0 AND observed=1`)
      .all(sheet)) {
      const v = String(r.v ?? '').trim();
      if (v && !/^(NA|N\/A|not given|to be assigned)$/i.test(v)) ids.add(v.toUpperCase());
    }
  };
  read('Collection_data', 'CAM_ID');
  read('Collection_data', 'CAM_ID_insectary');
  read('Insectary_data', 'CAM_ID');
  read('CRISPR', 'CAM_ID');
  return ids;
}

/** The sheet's rows as planPhotoLinks takes them. */
export function photoLinkRows(store) {
  return store.db
    .prepare('SELECT id, row_num, values_json FROM records WHERE sheet=? AND missing=0 AND observed=1 AND row_num>1 ORDER BY row_num')
    .all(SHEET)
    .map(r => ({ id: r.id, row: r.row_num, values: JSON.parse(r.values_json) }));
}

/** A folder named by its Drive ID or link. */
const folderIdIn = text => driveIdOf(text) ?? (/^[\w-]{20,}$/.test(String(text).trim()) ? String(text).trim() : null);

/**
 * Reads the photo folders and compares them with the sheet: planPhotoLinks' answer plus
 * `read` { folders, files, requests, seconds }. `add`: folders to start listing, each a name or
 * path among those the last reading offered (`notListed`), or a Drive link or ID.
 */
export async function scanPhotoLinks({ store, drive, add = [], timeZone = 'Europe/London' }) {
  const started = Date.now();
  const rows = photoLinkRows(store);
  const rootIds = rows
    .filter(r => r.values.Type === 'Folder' && !String(r.values['Full Path'] ?? '').includes('/'))
    .map(r => driveIdOf(r.values.URL))
    .filter(Boolean);
  // The folder that holds the listed folders: the parent most of them share.
  const parents = new Map();
  for (const meta of await Promise.all(rootIds.map(id => drive.get(id)))) {
    const parent = meta && !meta.gone ? meta.parents?.[0] : null;
    if (parent && !rootIds.includes(parent)) parents.set(parent, (parents.get(parent) || 0) + 1);
  }
  const home = [...parents].sort((a, b) => b[1] - a[1])[0]?.[0] ?? null;
  const byLink = add.map(folderIdIn);
  const items = await driveTree(drive, [...(home ? [home] : []), ...rootIds, ...byLink.filter(Boolean)]);

  // Folders named by name or path, among those under the folder that holds the listed ones.
  const addIds = [];
  const notFound = [];
  const pathFromHome = item => {
    const parts = [];
    for (let f = item; f && f.id !== home; f = f.parent ? items.get(f.parent) : null) parts.unshift(f.name);
    return parts.join('/');
  };
  for (const [i, text] of add.entries()) {
    if (byLink[i]) {
      if (items.get(byLink[i])?.mimeType === FOLDER) addIds.push(byLink[i]);
      else notFound.push(text);
      continue;
    }
    const wanted = lower(String(text).trim().replace(/^\/+|\/+$/g, ''));
    const folders = [...items.values()].filter(f => f.mimeType === FOLDER && f.id !== home);
    const match = folders.filter(f => lower(pathFromHome(f)) === wanted);
    const named = match.length ? match : folders.filter(f => lower(f.name) === wanted);
    if (named.length === 1) addIds.push(named[0].id);
    else notFound.push(text);
  }

  const input = { rows, items, home, add: addIds, ids: specimenIds(store), timeZone };
  let plan = planPhotoLinks(input);
  if (plan.unknown.length) {
    const lookups = new Map();
    const queue = [...plan.unknown];
    await Promise.all(
      Array.from({ length: 8 }, async () => {
        while (queue.length) {
          const id = queue.shift();
          lookups.set(id, await drive.get(id).catch(() => null));
        }
      }),
    );
    plan = planPhotoLinks({ ...input, lookups });
  }
  const all = [...items.values()];
  return {
    ...plan,
    notFound,
    read: {
      folders: all.filter(isFolder).length,
      files: all.length - all.filter(isFolder).length,
      requests: drive.requests ?? null,
      seconds: Math.round((Date.now() - started) / 100) / 10,
      rows: rows.length,
    },
  };
}
