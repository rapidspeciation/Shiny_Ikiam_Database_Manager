// Google Drive as the app reads it (never writes): the folders of specimen photos, listed for
// Photo_links (server/photo-links.mjs). The app's Google login has drive.metadata.readonly, which
// gives names, links, dates, sizes, owners and the photos' capture time.
// - GoogleDrive: the Drive API with the app's token (server/sheets.mjs GoogleSheets.accessToken).
// - LocalDrive: a recorded listing (LOCAL_MODE: the lab, tests).
// Both answer children(folderId) → the items in it (`foldersOnly`: its folders), and get(id) → the item, { gone: true } when
// it was deleted or sits in the bin, or null when it cannot be told.

import { existsSync, readFileSync } from 'node:fs';

export const FOLDER = 'application/vnd.google-apps.folder';
const FIELDS = 'id,name,mimeType,webViewLink,createdTime,description,size,trashed,owners(emailAddress),imageMediaMetadata(time),parents';
const API = 'https://www.googleapis.com/drive/v3/files';
const TIMEOUT_MS = 60_000;

export class GoogleDrive {
  constructor({ token, fetch: fetchFn = fetch }) {
    this.token = token;
    this.fetch = fetchFn;
    this.requests = 0;
  }
  async request(url) {
    for (let attempt = 0; ; attempt++) {
      this.requests++;
      let response;
      try {
        response = await this.fetch(url, {
          headers: { authorization: `Bearer ${await this.token()}` },
          signal: AbortSignal.timeout(TIMEOUT_MS),
        });
      } catch (e) {
        if (attempt < 3) continue;
        throw new Error(`Google Drive did not answer (${e?.name || e})`);
      }
      if (response.ok || response.status === 404) return response;
      if ([429, 500, 502, 503].includes(response.status) && attempt < 4) {
        await new Promise(resolve => setTimeout(resolve, 1000 * 2 ** attempt));
        continue;
      }
      const body = await response.text();
      throw Object.assign(new Error(`Google Drive ${response.status}: ${body.slice(0, 300)}`), { status: response.status });
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
}

export class LocalDrive {
  /** `files`: the items as the Drive API gives them (each with `parents`), or the path of a JSON file of them. */
  constructor(files) {
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
    return item.trashed ? { gone: true } : item;
  }
}

/** The Drive reader of a store: the app's Google login, or the recorded listing of a local copy (null without one). */
export function driveFor(store, config = {}) {
  if (config.drive) return config.drive;
  if (store.localMode) return config.driveListingFile && existsSync(config.driveListingFile) ? new LocalDrive(config.driveListingFile) : null;
  return new GoogleDrive({ token: () => store.sheets.accessToken() });
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
