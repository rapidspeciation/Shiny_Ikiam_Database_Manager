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
 *  - scope: when only some folders' files were read, those folders' IDs: the rest of the sheet is left as it is.
 *  - lookups: Map Drive ID → drive.get's answer, for rows whose file `items` lacks (`unknown` in the answer).
 *  - ids: the specimens of the database, Map ID (in capitals) → { date, country } (specimens(); a Set of IDs
 *    will do): the new photos are checked against them (checkPhotos).
 * Answers { edits, creates, display, unknown, summary }: `edits` [{ id, values, expected }] and `creates`
 * [{ module, values, folder }] as server/batch.mjs takes them; `display`, one line per new row (as `creates`)
 * and then per edit, as the panel lists them.
 */
export function planPhotoLinks({
  rows,
  items,
  home = null,
  add = [],
  scope = null,
  lookups = new Map(),
  ids = new Set(),
  timeZone = 'Europe/London',
}) {
  const info = ids instanceof Map ? ids : new Map([...ids].map(id => [id, {}]));
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
  const inScope = item => !scope || scope.has(isFolder(item) ? item.id : item.parent);
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
  const edited = [];
  const edit = (r, values, kind, shown = {}) => {
    const changed = Object.fromEntries(Object.entries(values).filter(([f, v]) => !same(f, r.values[f], v)));
    if (!Object.keys(changed).length) return false;
    edits.push({ id: r.id, row: r.row, kind, values: changed, expected: Object.fromEntries(Object.keys(changed).map(f => [f, r.values[f] ?? null])) });
    if (kind !== 'status')
      edited.push({ change: kind, row: r.row, folder: dirOf(values['Full Path'] ?? r.values['Full Path']), name: values.Name ?? r.values.Name ?? '', ...shown });
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
  // With only some folders read, a row's file can be missing only when the row was listed in one of them.
  const scopePaths = scope ? new Set([...scope].map(id => placeOf(id)?.path).filter(Boolean)) : null;
  for (const [id, r] of listed) {
    const item = items.get(id);
    if (!item) {
      if (!scopePaths || scopePaths.has(dirOf(r.values['Full Path']))) missing.push([id, r]);
      continue;
    }
    if (!inScope(item)) continue;
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
      renamed ? { from: r.values.Name } : {},
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
  // Rows whose file the listing lacks: linked to a file of the same name now in the listed folders, or
  // emptied once Drive says the file is gone. With only some folders read, the file may be in another
  // one: Drive is asked first.
  const unknown = [];
  for (const [id, r] of missing) {
    const found = lookups.get(id);
    const again = r.values.Type === 'File' ? free.get(lower(r.values.Name))?.find(f => !taken.has(f.id)) : null;
    if (again && (!scope || found?.gone)) {
      taken.add(again.id);
      relinkedTo.set(r.id, again.id);
      const { Name: _name, Type: _type, ...link } = cells(again, pathOf(again));
      edit(r, link, 'relink');
      summary.relinked.push({ row: r.row, name: r.values.Name });
      continue;
    }
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

  // Names the sheet holds once the edits are in, and the butterflies' views they stand for.
  const names = new Map();
  const views = new Set();
  const renamedTo = new Map(edits.filter(e => e.values.Name).map(e => [e.id, e.values.Name]));
  const seeView = name => {
    const parsed = PHOTO.test(name) ? specimenName(name) : null;
    const id = parsed && !parsed.extra ? knownId(parsed.id, info) : null;
    if (id) views.add(`${id}|${parsed.view}`);
  };
  for (const r of rows) {
    if (emptied.has(r.id) || r.values.Type !== 'File') continue;
    const name = renamedTo.get(r.id) ?? r.values.Name;
    if (blank(name)) continue;
    if (!names.has(lower(name))) names.set(lower(name), relinkedTo.get(r.id) ?? driveIdOf(r.values.URL));
    seeView(String(name));
  }

  // New rows: each listed folder in the order Drive was read, its own row first, then its files by name.
  const creates = [];
  const display = [];
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
    if (!isFolder(folder) || !inScope(folder)) continue;
    const place = placeOf(folder.id);
    if (!place) continue;
    let count = 0;
    if (!listed.has(folder.id)) {
      creates.push({ module: SHEET, values: cells(folder, place.path), folder: place.path });
      display.push({ change: 'folder', folder: place.path, name: folder.name });
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
      seeView(file.name);
      creates.push({ module: SHEET, values: cells(file, `${place.path}/${file.name}`), folder: place.path });
      display.push({ change: 'new', folder: place.path, name: file.name, taken: takenText(file), url: file.webViewLink, file: file.id });
      added.push(file);
      count++;
    }
    if (count) byFolder.push({ folder: place.path, new: count, files: files.length });
  }

  // Folders not listed: those beside the listed ones (offered), and the …_temp ones inside them (set aside).
  const notListed = [];
  const setAside = [];
  if (!scope) {
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
      if (parsed && knownId(parsed.id, info)) t.named++;
    }
    const folderPath = id => {
      const parts = [];
      for (let f = items.get(id); f && f.id !== home; f = f.parent ? items.get(f.parent) : null) parts.unshift(f.name);
      return parts.join('/');
    };
    for (const [id, t] of tallies) {
      if (t.kind === 'aside') setAside.push({ folder: pathOf(items.get(items.get(id).parent)) + '/' + items.get(id).name, files: t.files });
      else if (t.named) notListed.push({ folder: folderPath(id), id, files: t.files, photos: t.photos, namedAsSpecimens: t.named });
    }
    notListed.sort((a, b) => b.namedAsSpecimens - a.namedAsSpecimens);
  }

  const checked = checkPhotos({
    added,
    info,
    views,
    photosIn: folderId => (children.get(folderId) || []).filter(f => !isFolder(f) && PHOTO.test(f.name) && !TO_DELETE.test(f.name)),
    folderOf: file => placeOf(file.parent)?.path ?? '',
  });
  for (const line of display) {
    const flags = line.file ? checked.flags.get(line.file) : null;
    if (flags?.length) line.checks = flags;
    delete line.file;
  }

  return {
    edits,
    creates,
    display: [...display, ...edited],
    unknown: unknown.slice(0, LOOKUPS),
    summary: {
      ...summary,
      newRows: creates.length,
      newFiles: added.length,
      newFolders,
      byFolder,
      sameName,
      leftOut,
      names: checked.summary,
      notListed,
      setAside,
    },
  };
}

const dirOf = path => String(path ?? '').split('/').slice(0, -1).join('/');
/** The camera's capture time as text ("2026-07-20 10:15"), or ''. */
const takenText = file => {
  const m = /(\d{4}):(\d{2}):(\d{2}) (\d{2}):(\d{2})/.exec(String(file.imageMediaMetadata?.time ?? ''));
  return m ? `${m[1]}-${m[2]}-${m[3]} ${m[4]}:${m[5]}` : '';
};
/** The camera's capture time as milliseconds (its own clock, read as UTC), or null. */
const takenMs = file => {
  const m = /(\d{4}):(\d{2}):(\d{2}) (\d{2}):(\d{2}):(\d{2})/.exec(String(file.imageMediaMetadata?.time ?? ''));
  return m ? Date.UTC(+m[1], m[2] - 1, +m[3], +m[4], +m[5], +m[6]) : null;
};

/** Cells compared as the sheet keeps them: numbers within a rounding, texts as they are, empty as empty. */
function same(field, before, after) {
  if (blank(before) && blank(after)) return true;
  if (typeof after === 'number' && typeof before === 'number')
    return field === 'Size KB' ? sameSize(before, after) : Math.abs(before - after) < 1e-6;
  return String(before ?? '') === String(after ?? '');
}

/** A session covers nearby IDs: one further than this from every photo around it is out of place (as the Rename Photos app). */
const SEQUENCE_GAP = 15;
/** Photos looked at before and after one, in the order they were taken. */
const AROUND = 3;
/** A folder is one country's when this share of its photos (and at least FOLDER_PHOTOS of them) are of butterflies from it. */
const FOLDER_SHARE = 0.7;
const FOLDER_PHOTOS = 20;

/**
 * The new photos against the workbook and the photos around them. `flags`: Map file ID → what to look at,
 * each { kind, … }; `summary`: the counts, with examples (each with the photo's `url` on Drive).
 *  - rename: Photo_dorsal and Photo_ventral look up <ID>d.JPG and <ID>v.JPG (capitals aside); this name
 *    is another way of writing it (`to`: the name they find).
 *  - unknownId: the ID is not in the workbook (`maybe`: when it is out of order too, workbook IDs one character
 *    away that fit the photos around it and have no such photo yet).
 *  - unnamed: not a specimen's name (the camera's).
 *  - outOfOrder: its ID is far from those of the photos taken just before and after it in its folder
 *    (`around`), which is how a misread ID looks.
 *  - unpaired: the butterfly's other view is in no listed folder (`missing`).
 *  - beforeCollection: taken before the butterfly was collected or preserved (`days` before `date`), so the
 *    ID is another butterfly's.
 *  - otherCountry: the butterfly is from another country (`country`) than nearly all in its folder (`folder`).
 * `linked` counts the photos the sheets find as they are; `otherViews` the extra views and other formats
 * (.HEIC), which only the CRISPR sheet looks up. RAW files and other files are not checked.
 */
function checkPhotos({ added, info, views, photosIn, folderOf }) {
  const flags = new Map();
  const kinds = ['rename', 'unknownId', 'unnamed', 'outOfOrder', 'unpaired', 'beforeCollection', 'otherCountry'];
  const found = Object.fromEntries(kinds.map(k => [k, { count: 0, examples: [] }]));
  const out = { photos: 0, linked: 0, butterflies: 0, otherViews: 0 };
  if (!info.size) return { flags, summary: null };
  const flag = (file, kind, detail = {}) => {
    if (!flags.has(file.id)) flags.set(file.id, []);
    flags.get(file.id).push({ kind, ...detail });
    found[kind].count++;
    if (found[kind].examples.length < EXAMPLES) found[kind].examples.push({ name: file.name, in: folderOf(file), url: file.webViewLink, ...detail });
  };
  const read = file => {
    const parsed = specimenName(file.name);
    // The ID as written, without separators ("CAMO77986" with its letter O as a zero), and its number in its series.
    const written = parsed
      ? parsed.id
          .replace(/[\s_-]+/g, '')
          .toUpperCase()
          .replace(/^CAM[O\d]+$/, t => t.replace(/O/g, '0'))
      : null;
    const number = written ? /^(\D*)(\d+)$/.exec(written) : null;
    return {
      file,
      parsed,
      id: parsed ? knownId(parsed.id, info) : null,
      written,
      series: number ? number[1] : null,
      n: number ? Number(number[2]) : null,
      t: takenMs(file),
    };
  };
  // A folder's photos in the order they were taken, and the country nearly all its butterflies are from.
  const folders = new Map();
  const folderInfo = folderId => {
    if (folders.has(folderId)) return folders.get(folderId);
    const photos = photosIn(folderId).map(read);
    const timed = photos.filter(x => x.t !== null && x.n !== null).sort((a, b) => a.t - b.t || a.file.name.localeCompare(b.file.name));
    const countries = new Map();
    let known = 0;
    for (const x of photos) {
      const country = x.id ? info.get(x.id)?.country : null;
      if (!country) continue;
      known++;
      countries.set(country, (countries.get(country) || 0) + 1);
    }
    const [top, n] = [...countries].sort((a, b) => b[1] - a[1])[0] ?? [null, 0];
    const about = { timed, at: new Map(timed.map((x, i) => [x.file.id, i])), country: known >= FOLDER_PHOTOS && n / known >= FOLDER_SHARE ? top : null };
    folders.set(folderId, about);
    return about;
  };
  // Workbook IDs without a photo of a view, by length (an unknown ID is compared with those one character away).
  let lacking = null;
  const oneAway = (written, view, fits) => {
    lacking ??= [...info.keys()];
    const near = [];
    for (const id of lacking) {
      if (id.length !== written.length || views.has(`${id}|${view}`)) continue;
      let differ = 0;
      for (let i = 0; i < id.length && differ < 2; i++) if (id[i] !== written[i]) differ++;
      if (differ === 1 && fits(Number(/(\d+)$/.exec(id)?.[1]))) near.push(id);
      if (near.length === 3) break;
    }
    return near;
  };

  const linked = new Set();
  for (const file of added) {
    if (!PHOTO.test(file.name) || RAW.test(file.name)) continue;
    out.photos++;
    const x = read(file);
    if (!x.parsed) {
      flag(file, 'unnamed');
      continue;
    }
    const about = folderInfo(file.parent);
    const at = about.at.get(file.id);
    // The photos of other butterflies of its series taken just before and after it.
    const near =
      at === undefined
        ? []
        : [...about.timed.slice(Math.max(0, at - AROUND), at), ...about.timed.slice(at + 1, at + 1 + AROUND)].filter(
            y => y.written !== x.written && y.series === x.series,
          );
    const fits = n => near.some(y => Math.abs(y.n - n) <= SEQUENCE_GAP);
    const outOfOrder = near.length > 0 && !fits(x.n);
    if (outOfOrder) {
      const before = about.timed.slice(0, at).findLast(y => y.written !== x.written);
      const after = about.timed.slice(at + 1).find(y => y.written !== x.written);
      flag(file, 'outOfOrder', { around: [before?.written, after?.written].filter(Boolean) });
    }
    if (!x.id) {
      // Out of order too: a workbook ID one character away that fits the photos around it is likely the one meant.
      const maybe = outOfOrder ? oneAway(x.written, x.parsed.view, fits) : [];
      flag(file, 'unknownId', maybe.length ? { maybe } : {});
      continue;
    }
    // A .jpeg takes the .JPG the lookups expect; another format keeps its own.
    const jpeg = /^jpe?g$/i.test(x.parsed.ext);
    const right = `${x.id}${x.parsed.view}${x.parsed.extra}.${jpeg ? 'JPG' : x.parsed.ext}`;
    const exact = file.name.toUpperCase() === right.toUpperCase();
    if (!exact) flag(file, 'rename', { to: right });
    else if (x.parsed.extra || !jpeg) out.otherViews++;
    else {
      out.linked++;
      linked.add(x.id);
    }
    if (!x.parsed.extra && !views.has(`${x.id}|${x.parsed.view === 'd' ? 'v' : 'd'}`))
      flag(file, 'unpaired', { missing: x.parsed.view === 'd' ? 'ventral' : 'dorsal' });
    const specimen = info.get(x.id) ?? {};
    if (x.t !== null && typeof specimen.date === 'number') {
      const day = Math.floor((x.t - SHEETS_EPOCH) / 86_400_000);
      if (day < Math.floor(specimen.date)) flag(file, 'beforeCollection', { days: Math.floor(specimen.date) - day, date: isoDay(specimen.date) });
    }
    if (about.country && specimen.country && specimen.country !== about.country)
      flag(file, 'otherCountry', { country: specimen.country, folder: about.country });
  }
  out.butterflies = linked.size;
  return { flags, summary: { ...out, ...found } };
}
const isoDay = serial => new Date(SHEETS_EPOCH + Math.floor(serial) * 86_400_000).toISOString().slice(0, 10);

/**
 * The specimens photos are named after: Map ID (in capitals) → { date, country }, from the CAM_ID
 * columns of the sheets that show photos. `date`: the earliest day the butterfly could be photographed
 * (collected in the field; preserved, dead or brought in at the insectary); `country`: where it was collected.
 */
export function specimens(store) {
  const out = new Map();
  const text = v => String(v ?? '').trim();
  const read = (sheet, field, dates, country = null) => {
    for (const r of store.db.prepare('SELECT values_json FROM records WHERE sheet=? AND missing=0 AND observed=1').all(sheet)) {
      const values = JSON.parse(r.values_json);
      const id = text(values[field]).toUpperCase();
      if (!id || /^(NA|N\/A|NOT GIVEN|TO BE ASSIGNED)$/.test(id)) continue;
      const entry = out.get(id) ?? out.set(id, {}).get(id);
      const date = dates.map(f => values[f]).find(v => typeof v === 'number');
      if (date !== undefined && !(entry.date <= date)) entry.date = date;
      const where = country ? text(values[country]) : '';
      if (where && !/^(NA|NOT_FOUND|NOT_PROVIDED)$/i.test(where)) entry.country ??= where;
    }
  };
  read('Collection_data', 'CAM_ID', ['Collection_date'], 'Country');
  read('Collection_data', 'CAM_ID_insectary', ['Collection_date'], 'Country');
  read('Insectary_data', 'CAM_ID', ['Preservation_date', 'Death_date', 'Intro2Insectary_date']);
  read('CRISPR', 'CAM_ID', []);
  return out;
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

/** The folders named in `texts` (a Drive link or ID, a path from `home`, or a name no other folder has): { ids, notFound }. */
function namedFolders(texts, items, home) {
  const ids = [];
  const notFound = [];
  const folders = [...items.values()].filter(f => f.mimeType === FOLDER && f.id !== home);
  const pathFromHome = item => {
    const parts = [];
    for (let f = item; f && f.id !== home; f = f.parent ? items.get(f.parent) : null) parts.unshift(f.name);
    return parts.join('/');
  };
  for (const text of texts) {
    // A link or an ID (a long name without spaces looks like one: it is then looked for by name).
    const linked = folderIdIn(text);
    if (linked && items.get(linked)?.mimeType === FOLDER) {
      ids.push(linked);
      continue;
    }
    const wanted = lower(String(text).trim().replace(/^\/+|\/+$/g, ''));
    const byPath = folders.filter(f => lower(pathFromHome(f)) === wanted || lower(pathFromHome(f)).endsWith(`/${wanted}`));
    const named = byPath.length ? byPath : folders.filter(f => lower(f.name) === wanted);
    if (named.length === 1) ids.push(named[0].id);
    else notFound.push(text);
  }
  return { ids, notFound };
}

/**
 * Reads the photo folders and compares them with the sheet: planPhotoLinks' answer plus
 * `read` { folders, files, requests, seconds, rows }.
 *  - add: folders to start listing, each a name or path among those the last reading offered
 *    (`notListed`), or a Drive link or ID.
 *  - only: the folders to bring up to date (with what they hold), named the same way. Only their
 *    files are read (the folders' names of the rest, to place them), and the rest of the sheet is left as it is.
 */
export async function scanPhotoLinks({ store, drive, add = [], only = [], timeZone = 'Europe/London' }) {
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
  const tops = [...(home ? [home] : []), ...rootIds, ...[...add, ...only].map(folderIdIn).filter(Boolean)];
  const items = await driveTree(drive, tops, { foldersOnly: only.length > 0 });
  const added = namedFolders(add, items, home);
  const chosen = namedFolders(only, items, home);

  // Only some folders: they and the folders inside them, with their files.
  let scope = null;
  if (only.length) {
    scope = new Set(chosen.ids);
    for (const item of items.values()) if (item.mimeType === FOLDER && item.parent && scope.has(item.parent)) scope.add(item.id);
    const queue = [...scope];
    await Promise.all(
      Array.from({ length: 8 }, async () => {
        while (queue.length) {
          const folderId = queue.shift();
          for (const item of await drive.children(folderId)) if (!items.has(item.id)) items.set(item.id, { ...item, parent: folderId });
        }
      }),
    );
  }

  const input = { rows, items, home, add: added.ids, scope, ids: specimens(store), timeZone };
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
    notFound: [...added.notFound, ...chosen.notFound],
    only: scope ? chosen.ids.map(id => items.get(id).name) : null,
    read: {
      folders: all.filter(isFolder).length,
      files: all.length - all.filter(isFolder).length,
      requests: drive.requests ?? null,
      seconds: Math.round((Date.now() - started) / 100) / 10,
      rows: rows.length,
    },
  };
}
