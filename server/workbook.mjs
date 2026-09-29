// Which Google Sheets workbook the app reads and writes: the team's working
// workbook, or another one named by WORKBOOK_ID.
//
// The local database remembers which workbook it caches (settings.workbookId).
// Moving it to another workbook is scripts/switch-workbook.mjs, never a plain
// restart: a normal sync would log every difference between the two workbooks
// as an edit made in Google Sheets, and rows would take the ids of other rows.

/** The team's working workbook. */
export const REAL_ID = '1QZj6YgHAJ9NmFXFPCtu-i-1NDuDmAdMF2Wogts7S2_4';
/** The personal test copy the app used until September 2026 (older databases cache it). */
export const SANDBOX_ID = '19FXrunwWKK1pbyHqWNPcytmaDmyBQoK7yabzIdRQQYM';

export const workbookUrl = id => `https://docs.google.com/spreadsheets/d/${id}/edit`;

/** A Google Drive file ID; anything else (a URL, a typo with spaces) is refused. */
export function checkWorkbookId(id) {
  const value = String(id ?? '').trim();
  if (!/^[A-Za-z0-9_-]{25,80}$/.test(value))
    throw Object.assign(new Error('WORKBOOK_ID must be a Google Sheets file ID'), { code: 'INVALID_WORKBOOK_ID' });
  return value;
}

/** WORKBOOK_ID from the environment, the team's workbook by default. */
export function workbookFromEnv(env = process.env) {
  const id = checkWorkbookId(env.WORKBOOK_ID || REAL_ID);
  return { id, url: workbookUrl(id) };
}
