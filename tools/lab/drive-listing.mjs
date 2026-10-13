#!/usr/bin/env node
// Records the photo folders on Google Drive for the lab's sync_photo_links (DRIVE_LISTING_FILE,
// server/drive.mjs LocalDrive). Read-only; run where the app's Google login is:
//   GOOGLE_CREDENTIALS_FILE=~/.config/ithomiini/google.json node tools/lab/drive-listing.mjs <folder ID> > drive-listing.json
// <folder ID>: the Drive folder that holds the photo folders. The listing has people's addresses
// (the files' owners): keep it out of the repository.
import { GoogleSheets } from '../../server/sheets.mjs';
import { GoogleDrive, driveTree } from '../../server/drive.mjs';

const root = process.argv[2];
if (!root) {
  console.error('usage: drive-listing.mjs <folder ID>');
  process.exit(1);
}
const sheets = new GoogleSheets({ googleCredentialsFile: process.env.GOOGLE_CREDENTIALS_FILE, readOnly: true });
const drive = new GoogleDrive({ token: () => sheets.accessToken() });
const items = await driveTree(drive, [root]);
const files = [...items.values()].map(({ parent, ...item }) => ({ ...item, parents: parent ? [parent] : [] }));
process.stdout.write(JSON.stringify(files));
console.error(`${files.length} items, ${drive.requests} requests`);
