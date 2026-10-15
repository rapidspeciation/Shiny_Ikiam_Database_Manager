// Google Drive as the app uses it: the folders of specimen photos, listed for Photo_links
// (server/photo-links.mjs) and put in order when someone asks (server/photo-files.mjs).
// - Reading (driveFor): the app's Google login, whose drive.metadata.readonly gives names, links,
//   dates, sizes, owners and the photos' capture time.
// - Changing files (driveWriterFor): the project's own Google account (GOOGLE_DRIVE_CREDENTIALS_FILE),
//   an editor of the photo folders: it renames and moves any file there and makes folders, and
//   bins only the files it owns (Drive lets nobody bin someone else's file).
// - LocalDrive: a recorded listing, changed in memory (LOCAL_MODE: the lab, tests).
// All answer children(folderId) → the items in it (`foldersOnly`: its folders), and get(id) → the
// item, { gone: true } when it was deleted or sits in the bin, or null when it cannot be told;
// rename, move, trash, untrash, createFolder and createShortcut answer the item as it is afterwards;
// delete removes a file for good (the account's own only); about() answers the account and its space:
// { account, limit, used, inBin } in bytes (limit null: no limit); owned() every file the account owns.
// The writer also reads a file's bytes (download(id, path): written to that path; the reader's login
// sees only the files' details) and replaces them (replaceContent(id, bytes, mimeType): the same file,
// its link and ID kept; server/drive-photos.mjs turns photos so).

import { createWriteStream, existsSync, readFileSync, readdirSync, statSync, writeFileSync } from 'node:fs';
import { basename, join } from 'node:path';
import { copyFile } from 'node:fs/promises';
import { Readable, Transform } from 'node:stream';
import { pipeline } from 'node:stream/promises';

export const FOLDER = 'application/vnd.google-apps.folder';
const SHORTCUT = 'application/vnd.google-apps.shortcut';
const FIELDS =
  'id,name,mimeType,webViewLink,createdTime,description,size,trashed,ownedByMe,owners(emailAddress),imageMediaMetadata(time),parents';
const API = 'https://www.googleapis.com/drive/v3/files';
const UPLOAD = 'https://www.googleapis.com/upload/drive/v3/files';
/** A file larger than this is not downloaded (a photo is some MB, a RAW file some tens). */
const MAX_DOWNLOAD = 200 * 2 ** 20;
const ABOUT = 'https://www.googleapis.com/drive/v3/about';
const TIMEOUT_MS = 60_000;

export class GoogleDrive {
  constructor({ token, fetch: fetchFn = fetch }) {
    this.token = token;
    this.fetch = fetchFn;
    this.requests = 0;
  }
  async request(url, { method = 'GET', body = null } = {}) {
    for (let attempt = 0; ; attempt++) {
      this.requests++;
      let response;
      try {
        response = await this.fetch(url, {
          method,
          headers: { authorization: `Bearer ${await this.token()}`, ...(body ? { 'content-type': 'application/json' } : {}) },
          ...(body ? { body: JSON.stringify(body) } : {}),
          signal: AbortSignal.timeout(TIMEOUT_MS),
        });
      } catch (e) {
        // A change that got no answer may have been made: it is not sent twice.
        if (attempt < 3 && method === 'GET') continue;
        throw new Error(`Google Drive did not answer (${e?.name || e})`);
      }
      if (response.ok || response.status === 404) return response;
      if ([429, 500, 502, 503].includes(response.status) && attempt < 4) {
        await new Promise(resolve => setTimeout(resolve, 1000 * 2 ** attempt));
        continue;
      }
      const said = await response.text();
      throw Object.assign(new Error(`Google Drive ${response.status}: ${said.slice(0, 300)}`), { status: response.status });
    }
  }
  async children(folderId, { foldersOnly = false } = {}) {
    const out = [];
    let pageToken = null;
    do {
      const params = new URLSearchParams({
        q: `'${folderId}' in parents and trashed = false${foldersOnly ? ` and mimeType = '${FOLDER}'` : ''}`,
        pageSize: '1000',
        fields: `nextPageToken,files(${FIELDS})`,
        supportsAllDrives: 'true',
        includeItemsFromAllDrives: 'true',
      });
      if (pageToken) params.set('pageToken', pageToken);
      const response = await this.request(`${API}?${params}`);
      if (response.status === 404) throw new Error(`Google Drive folder ${folderId} not found`);
      const body = await response.json();
      out.push(...(body.files || []));
      pageToken = body.nextPageToken || null;
    } while (pageToken);
    return out;
  }
  async get(id) {
    const response = await this.request(`${API}/${encodeURIComponent(id)}?fields=${encodeURIComponent(FIELDS)}&supportsAllDrives=true`);
    if (response.status === 404) return { gone: true };
    const item = await response.json();
    return item.trashed ? { gone: true } : item;
  }
  /** Files by name (`exact`, its extension too) or by the start of their name (`prefix`): at most 20, with their folders. */
  async named(exact, prefix = null) {
    const quote = v => String(v).replace(/\\/g, '\\\\').replace(/'/g, "\\'");
    const q = `${exact ? `name = '${quote(exact)}'` : `name contains '${quote(prefix)}'`} and trashed = false and mimeType != '${FOLDER}'`;
    const params = new URLSearchParams({ q, pageSize: '20', fields: `files(${FIELDS})`, supportsAllDrives: 'true', includeItemsFromAllDrives: 'true', corpora: 'allDrives' });
    const response = await this.request(`${API}?${params}`);
    const files = (await response.json()).files || [];
    // `contains` matches words anywhere: the name has to start with it.
    return prefix ? files.filter(f => f.name.toLowerCase().startsWith(String(prefix).toLowerCase())) : files;
  }
  /** A change of a file's own data (its name, its folders, the bin). */
  async update(id, body, params = {}) {
    const query = new URLSearchParams({ fields: FIELDS, supportsAllDrives: 'true', ...params });
    const response = await this.request(`${API}/${encodeURIComponent(id)}?${query}`, { method: 'PATCH', body });
    if (response.status === 404) throw Object.assign(new Error('The file is no longer on Drive'), { status: 404 });
    return response.json();
  }
  rename(id, name) {
    return this.update(id, { name });
  }
  move(id, from, to) {
    return this.update(id, {}, { addParents: to, removeParents: from });
  }
  trash(id) {
    return this.update(id, { trashed: true });
  }
  untrash(id) {
    return this.update(id, { trashed: false });
  }
  async createFolder(name, parent) {
    const query = new URLSearchParams({ fields: FIELDS, supportsAllDrives: 'true' });
    const response = await this.request(`${API}?${query}`, { method: 'POST', body: { name, mimeType: FOLDER, parents: [parent] } });
    return response.json();
  }
  /** A shortcut to a file, in another folder (the file stays where it is). */
  async createShortcut(target, name, parent) {
    const query = new URLSearchParams({ fields: FIELDS, supportsAllDrives: 'true' });
    const response = await this.request(`${API}?${query}`, {
      method: 'POST',
      body: { name, mimeType: SHORTCUT, shortcutDetails: { targetId: target }, parents: [parent] },
    });
    return response.json();
  }
  /** Deleted for good, skipping the bin: its space is free at once, and it cannot be brought back. */
  async delete(id) {
    await this.request(`${API}/${encodeURIComponent(id)}?supportsAllDrives=true`, { method: 'DELETE' });
  }
  async download(id, path, { maxBytes = MAX_DOWNLOAD } = {}) {
    const response = await this.fetch(`${API}/${encodeURIComponent(id)}?alt=media&supportsAllDrives=true`, {
      headers: { authorization: `Bearer ${await this.token()}` },
      signal: AbortSignal.timeout(10 * 60_000),
    });
    this.requests++;
    if (!response.ok) throw Object.assign(new Error(`Google Drive ${response.status}: the file could not be read`), { status: response.status });
    let bytes = 0;
    const bounded = new Transform({
      transform(chunk, _encoding, done) {
        bytes += chunk.length;
        done(bytes > maxBytes ? new Error('The file is larger than the app downloads') : null, chunk);
      },
    });
    await pipeline(Readable.fromWeb(response.body), bounded, createWriteStream(path, { mode: 0o600 }));
    return bytes;
  }
  async replaceContent(id, bytes, mimeType) {
    const response = await this.fetch(`${UPLOAD}/${encodeURIComponent(id)}?uploadType=media&supportsAllDrives=true&fields=${encodeURIComponent(FIELDS)}`, {
      method: 'PATCH',
      headers: { authorization: `Bearer ${await this.token()}`, 'content-type': mimeType || 'application/octet-stream' },
      body: bytes,
      signal: AbortSignal.timeout(10 * 60_000),
    });
    this.requests++;
    if (!response.ok) throw Object.assign(new Error(`Google Drive ${response.status}: ${(await response.text()).slice(0, 200)}`), { status: response.status });
    return response.json();
  }
  /** Every file the account owns, anywhere in its Drive: [{ name, size, parent }] (what takes its space). */
  async owned() {
    const out = [];
    let pageToken = null;
    do {
      const params = new URLSearchParams({ q: "'me' in owners and trashed = false", pageSize: '1000', fields: 'nextPageToken,files(name,quotaBytesUsed,parents)' });
      if (pageToken) params.set('pageToken', pageToken);
      const body = await (await this.request(`${API}?${params}`)).json();
      for (const f of body.files || []) out.push({ name: f.name, size: Number(f.quotaBytesUsed) || 0, parent: f.parents?.[0] ?? null });
      pageToken = body.nextPageToken || null;
    } while (pageToken && out.length < 200_000);
    return out;
  }
  async about() {
    const response = await this.request(`${ABOUT}?fields=${encodeURIComponent('user(emailAddress),storageQuota(limit,usage,usageInDrive,usageInDriveTrash)')}`);
    const { user, storageQuota: q = {} } = await response.json();
    return {
      account: user?.emailAddress ?? null,
      limit: q.limit ? Number(q.limit) : null,
      used: Number(q.usage ?? q.usageInDrive ?? 0),
      inBin: Number(q.usageInDriveTrash ?? 0),
    };
  }
}

export class LocalDrive {
  /**
   * `files`: the items as the Drive API gives them (each with `parents`), or the path of a JSON file
   * of them. `me`: the address of the account changing files (it owns the files of that owner).
   */
  constructor(files, { me = null, limit = 100 * 2 ** 30 } = {}) {
    this.me = me;
    this.limit = limit;
    this.made = 0;
    const list = typeof files === 'string' ? JSON.parse(readFileSync(files, 'utf8')) : files;
    this.items = new Map((Array.isArray(list) ? list : list.files).map(f => [f.id, f]));
    this.byParent = new Map();
    for (const f of this.items.values())
      for (const parent of f.parents || []) {
        if (!this.byParent.has(parent)) this.byParent.set(parent, []);
        this.byParent.get(parent).push(f);
      }
    this.requests = 0;
  }
  async children(folderId, { foldersOnly = false } = {}) {
    this.requests++;
    return (this.byParent.get(folderId) || []).filter(f => !f.trashed && (!foldersOnly || f.mimeType === FOLDER));
  }
  async get(id) {
    this.requests++;
    const item = this.items.get(id);
    // A folder known only as a parent (the one holding the photo folders).
    if (!item) return this.byParent.has(id) ? { id, name: id, mimeType: FOLDER, parents: [] } : { gone: true };
    return item.trashed ? { gone: true } : this.seen(item);
  }
  async named(exact, prefix = null) {
    this.requests++;
    const match = f => (exact ? f.name.toLowerCase() === exact.toLowerCase() : f.name.toLowerCase().startsWith(String(prefix).toLowerCase()));
    return [...this.items.values()].filter(f => !f.trashed && f.mimeType !== FOLDER && match(f)).slice(0, 20);
  }
  /** An item as the account sees it: its own when it is the owner. */
  seen(item) {
    return this.me ? { ...item, ownedByMe: item.ownedByMe ?? item.owners?.[0]?.emailAddress === this.me } : item;
  }
  existing(id) {
    const item = this.items.get(id);
    if (!item) throw Object.assign(new Error('The file is no longer on Drive'), { status: 404 });
    return item;
  }
  async rename(id, name) {
    this.requests++;
    const item = this.existing(id);
    item.name = name;
    return this.seen(item);
  }
  async move(id, from, to) {
    this.requests++;
    const item = this.existing(id);
    this.byParent.set(from, (this.byParent.get(from) || []).filter(f => f.id !== id));
    if (!this.byParent.has(to)) this.byParent.set(to, []);
    this.byParent.get(to).push(item);
    item.parents = [...(item.parents || []).filter(p => p !== from), to];
    return this.seen(item);
  }
  async trash(id) {
    this.requests++;
    const item = this.existing(id);
    if (!this.seen(item).ownedByMe && this.me)
      throw Object.assign(new Error('Google Drive 403: the account does not own this file'), { status: 403 });
    item.trashed = true;
    return this.seen(item);
  }
  async untrash(id) {
    this.requests++;
    const item = this.existing(id);
    item.trashed = false;
    return this.seen(item);
  }
  make(item) {
    this.requests++;
    const id = `local-made-${String(++this.made).padStart(20, '0')}`;
    const made = { id, createdTime: new Date().toISOString(), owners: this.me ? [{ emailAddress: this.me }] : [], ownedByMe: true, ...item };
    this.items.set(id, made);
    for (const parent of made.parents) {
      if (!this.byParent.has(parent)) this.byParent.set(parent, []);
      this.byParent.get(parent).push(made);
    }
    return made;
  }
  async createFolder(name, parent) {
    const folder = this.make({ name, mimeType: FOLDER, parents: [parent] });
    folder.webViewLink = `https://drive.google.com/drive/folders/${folder.id}`;
    return folder;
  }
  async createShortcut(target, name, parent) {
    return this.make({ name, mimeType: SHORTCUT, shortcutDetails: { targetId: target }, parents: [parent] });
  }
  /** A file's bytes: those written last, else its `localPath` (a lab's photos on disk). */
  async download(id, path) {
    this.requests++;
    const item = this.existing(id);
    const held = this.contents?.get(id);
    if (held) writeFileSync(path, held, { mode: 0o600 });
    else if (item.localPath) await copyFile(item.localPath, path);
    else throw Object.assign(new Error('This copy of Drive has no bytes for that file'), { status: 404 });
    return held?.length ?? Number(item.size) ?? 0;
  }
  async replaceContent(id, bytes) {
    this.requests++;
    const item = this.existing(id);
    (this.contents ??= new Map()).set(id, Buffer.from(bytes));
    item.size = String(bytes.length);
    return this.seen(item);
  }
  async delete(id) {
    this.requests++;
    const item = this.existing(id);
    if (!this.seen(item).ownedByMe && this.me) throw Object.assign(new Error('Google Drive 403: the account does not own this file'), { status: 403 });
    this.items.delete(id);
    for (const parent of item.parents || []) this.byParent.set(parent, (this.byParent.get(parent) || []).filter(f => f.id !== id));
  }
  async owned() {
    this.requests++;
    return [...this.items.values()]
      .filter(f => !f.trashed && f.mimeType !== FOLDER && this.seen(f).ownedByMe)
      .map(f => ({ name: f.name, size: Number(f.size) || 0, parent: f.parents?.[0] ?? null }));
  }
  /** The account's files count against its space, those in the bin too. */
  async about() {
    this.requests++;
    let used = 0;
    let inBin = 0;
    for (const item of this.items.values()) {
      if (!this.seen(item).ownedByMe) continue;
      used += Number(item.size) || 0;
      if (item.trashed) inBin += Number(item.size) || 0;
    }
    return { account: this.me, limit: this.limit, used, inBin };
  }
}

/** An access token from a credentials file (client_id, client_secret, refresh_token), renewed when it runs out. */
function tokenFrom(file) {
  let held = null;
  return async () => {
    if (held && held.expires > Date.now() + 60_000) return held.value;
    const c = JSON.parse(readFileSync(file, 'utf8'));
    const response = await fetch('https://oauth2.googleapis.com/token', {
      method: 'POST',
      signal: AbortSignal.timeout(30_000),
      body: new URLSearchParams({ client_id: c.client_id, client_secret: c.client_secret, refresh_token: c.refresh_token, grant_type: 'refresh_token' }),
    });
    if (!response.ok) throw new Error(`Google token refresh failed (${response.status})`);
    const body = await response.json();
    held = { value: body.access_token, expires: Date.now() + body.expires_in * 1000 };
    return held.value;
  };
}

// A local copy's listing is one object for the reader and the writer: what is changed in it is read back.
const locals = new WeakMap();
function localDrive(store, config) {
  if (!config.driveListingFile || !existsSync(config.driveListingFile)) return null;
  if (!locals.has(store)) {
    const drive = new LocalDrive(config.driveListingFile, { me: config.driveAccount || null });
    if (config.driveLocalPhotos) addLocalPhotos(drive, config.driveLocalPhotos);
    locals.set(store, drive);
  }
  return locals.get(store);
}
/**
 * A folder of photos on this computer shown as a Drive folder of the listing ({ dir, under }: inside the
 * folder of that name), as if someone had uploaded them without renaming: the lab's Drive review.
 */
function addLocalPhotos(drive, { dir, under }) {
  if (!dir || !existsSync(dir)) return;
  const parent = [...drive.items.values()].find(f => f.mimeType === FOLDER && f.name === under);
  if (!parent) return;
  const owner = parent.owners ?? [{ emailAddress: drive.me ?? 'photos@example.org' }];
  const folder = drive.make({ name: `Upload_${basename(dir)}`, mimeType: FOLDER, parents: [parent.id], owners: owner });
  folder.webViewLink = `https://drive.google.com/drive/folders/${folder.id}`;
  for (const name of readdirSync(dir).sort()) {
    const path = join(dir, name);
    const info = statSync(path);
    if (!info.isFile()) continue;
    const file = drive.make({ name, mimeType: /\.jpe?g$/i.test(name) ? 'image/jpeg' : 'application/octet-stream', parents: [folder.id], owners: owner, size: String(info.size) });
    Object.assign(file, { localPath: path, webViewLink: `https://drive.google.com/file/d/${file.id}/view`, createdTime: info.mtime.toISOString() });
  }
}
/** The Drive reader of a store: the app's Google login, or the recorded listing of a local copy (null without one). */
export function driveFor(store, config = {}) {
  if (config.drive) return config.drive;
  if (store.localMode) return localDrive(store, config);
  return new GoogleDrive({ token: () => store.sheets.accessToken() });
}
/** The Drive account that changes files for a store: the project's own (null when none is set up). */
export function driveWriterFor(store, config = {}) {
  if (config.drive) return config.drive;
  if (store.localMode) return localDrive(store, config);
  return config.driveCredentialsFile && existsSync(config.driveCredentialsFile) ? new GoogleDrive({ token: tokenFrom(config.driveCredentialsFile) }) : null;
}

/**
 * Everything under `rootIds` (folders and files, a folder before what it holds), read `parallel`
 * folders at a time: Map id → item with `parent` (the folder holding it). `foldersOnly`: the folders, without their files.
 */
export async function driveTree(drive, rootIds, { parallel = 8, foldersOnly = false } = {}) {
  const items = new Map();
  const queue = [];
  for (const id of rootIds) {
    if (items.has(id)) continue;
    const root = await drive.get(id);
    if (!root || root.gone || root.mimeType !== FOLDER) continue;
    // Its own parent, which may be another of the folders read here.
    items.set(id, { ...root, parent: root.parents?.[0] ?? null });
    queue.push(id);
  }
  await new Promise((resolve, reject) => {
    let active = 0;
    let failure = null;
    const next = () => {
      if (failure) {
        if (!active) reject(failure);
        return;
      }
      while (active < parallel && queue.length) {
        const folderId = queue.shift();
        active++;
        drive
          .children(folderId, { foldersOnly })
          .then(
            list => {
              for (const item of list) {
                if (items.has(item.id)) continue;
                items.set(item.id, { ...item, parent: folderId });
                if (item.mimeType === FOLDER) queue.push(item.id);
              }
            },
            e => {
              failure ??= e;
            },
          )
          .finally(() => {
            active--;
            next();
          });
      }
      if (!active) resolve();
    };
    next();
  });
  return items;
}
