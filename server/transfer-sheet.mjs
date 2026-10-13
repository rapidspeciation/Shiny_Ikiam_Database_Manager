// The team's sheet «Transfer Ownership & Free Up Space» (Drive › Photos), where an account does to
// the photo files what only it can do: its «Copy files» copies a folder's files owned by others (the
// account that presses it owns the copies) and renames the originals …_todelete; its «Delete copied
// files» deletes the …_todelete files the account that presses it owns. Both read the folder and
// the options from the sheet's cells.
//
// The app writes those cells (the folder, whether its subfolders go too, bin or deleted for good)
// with the project's Google account; the person signed in to the account concerned then opens the
// sheet and presses the button (server/photo-files.mjs, the `transfer` step of a plan).
// read() answers { folder, subfolders, permanent, last, busy }: what the cells hold and the last run
// the sheet logged; write({ folder, subfolders, permanent }) writes the ones given.

import { sheetSerial } from './photo-links.mjs';

export const TRANSFER_SHEET_ID = '1AsRf_wQBDzsAncTFEH-qO3Y-HcPCGdBrFsXtLebXE6A';
export const TRANSFER_SHEET_NAME = 'Transfer Ownership & Free Up Space';
export const TRANSFER_BUTTONS = { copy: 'Copy files', delete: 'Delete copied files' };
const API = 'https://sheets.googleapis.com/v4/spreadsheets';
/** The cells read: the labels in A, their values in B, and the last run under «Last Log». */
const RANGE = 'A1:H14';
const LABELS = { folder: /^source folder id/i, permanent: /^mode\b/i, subfolders: /^process subfolders/i };
/** A run the sheet shows as not ended counts as going on this long (its longest took under three hours). */
const RUNNING_HOURS = 4;
const ENDED = /^(completed|error|reset|stopped|aborted|done)/i;

const text = v => String(v ?? '').trim();

/** What the sheet's cells say: the options, where they are (`cells`), and its last run. */
export function parseTransfer(values, { timeZone = 'Europe/London', now = Date.now() } = {}) {
  const cells = {};
  const out = { folder: '', subfolders: true, permanent: false, last: null, busy: false };
  for (const [i, row] of values.entries()) {
    const label = text(row?.[0]);
    for (const [key, pattern] of Object.entries(LABELS))
      if (pattern.test(label) && !cells[key]) {
        cells[key] = `B${i + 1}`;
        out[key] = key === 'folder' ? text(row[1]) : text(row[1]) === '1';
      }
    // «Last Log», its headers, then the run.
    if (/^last log/i.test(label) && /^timestamp/i.test(text(values[i + 1]?.[0])) && values[i + 2]?.length) {
      const headers = values[i + 1].map(h => text(h).toLowerCase());
      const run = Object.fromEntries(headers.map((h, c) => [h, values[i + 2][c]]));
      const at = Number(run.timestamp);
      const hoursAgo = Number.isFinite(at) ? Math.max(0, Math.round((sheetSerial(now, timeZone) - at) * 240) / 10) : null;
      out.last = {
        action: text(run.action),
        folder: text(run['parent folder name']),
        files: Number(run['processed files']) || 0,
        status: text(run.status).slice(0, 120),
        runtime: text(run.runtime),
        by: text(run['user email']),
        hoursAgo,
      };
      out.busy = !!out.last.status && !ENDED.test(out.last.status) && hoursAgo !== null && hoursAgo < RUNNING_HOURS;
    }
  }
  return { ...out, cells };
}

export class GoogleTransferSheet {
  /** `token`: of an account that edits the sheet (the project's own; its Drive access covers it). */
  constructor({ id = TRANSFER_SHEET_ID, token, fetch: fetchFn = fetch }) {
    this.id = id;
    this.token = token;
    this.fetch = fetchFn;
    this.timeZone = null;
    this.url = `https://docs.google.com/spreadsheets/d/${id}/edit`;
  }
  async request(path, { method = 'GET', body = null } = {}) {
    const response = await this.fetch(`${API}/${this.id}${path}`, {
      method,
      headers: { authorization: `Bearer ${await this.token()}`, ...(body ? { 'content-type': 'application/json' } : {}) },
      ...(body ? { body: JSON.stringify(body) } : {}),
      signal: AbortSignal.timeout(60_000),
    });
    if (!response.ok) throw new Error(`The sheet «${TRANSFER_SHEET_NAME}» could not be ${method === 'GET' ? 'read' : 'written'} (${response.status})`);
    return response.json();
  }
  async read() {
    this.timeZone ??= (await this.request('?fields=properties(timeZone)')).properties?.timeZone || 'Europe/London';
    const { values = [] } = await this.request(`/values/${RANGE}?valueRenderOption=UNFORMATTED_VALUE`);
    return parseTransfer(values, { timeZone: this.timeZone });
  }
  async write(options) {
    const { cells } = await this.read();
    const data = [];
    for (const key of ['folder', 'subfolders', 'permanent']) {
      if (options[key] === undefined) continue;
      if (!cells[key]) throw new Error(`The sheet «${TRANSFER_SHEET_NAME}» no longer has its cell for ${key}`);
      data.push({ range: cells[key], values: [[key === 'folder' ? String(options.folder) : options[key] ? 1 : 0]] });
    }
    if (data.length) await this.request('/values:batchUpdate', { method: 'POST', body: { valueInputOption: 'RAW', data } });
  }
}

/** The sheet of a local copy of the app (the lab, tests): its cells in memory. */
export class LocalTransferSheet {
  constructor(state = {}) {
    this.state = { folder: '', subfolders: true, permanent: true, last: null, busy: false, ...state };
    this.url = null;
    this.written = [];
  }
  async read() {
    return { ...this.state };
  }
  async write(options) {
    const given = Object.fromEntries(Object.entries(options).filter(([, v]) => v !== undefined));
    Object.assign(this.state, given);
    this.written.push(given);
  }
}

const locals = new WeakMap();
/** The sheet for a store, written with the account that changes files (`writer`, server/drive.mjs); null without one. */
export function transferSheetFor(store, config = {}, writer = null) {
  if (config.transferSheet !== undefined) return config.transferSheet;
  if (store.localMode) {
    if (!locals.has(store)) locals.set(store, new LocalTransferSheet());
    return locals.get(store);
  }
  return writer?.token ? new GoogleTransferSheet({ id: config.transferSheetId || TRANSFER_SHEET_ID, token: writer.token }) : null;
}
