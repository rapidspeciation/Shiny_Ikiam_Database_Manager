#!/usr/bin/env node
// Envelope curation and Wings Gallery data → the app's database (Revisión tab).
//
// On the PC that has the curation (images are not needed, only the JSONL/CSV/JSON):
//   node scripts/import-envelope-curation.mjs --bundle envelope-bundle.json.gz <curation-dir> <manifest.csv> [--gallery <dir|url>]
// makes one small file to copy to the server; there:
//   DATABASE_PATH=… node scripts/import-envelope-curation.mjs envelope-bundle.json.gz
// Or straight into a database (local tests):
//   DATABASE_PATH=… node scripts/import-envelope-curation.mjs <curation-dir> <manifest.csv> [--gallery <dir|url>]
// --gallery: a clone's public/data folder or the gallery's URL (default: the published gallery);
// --no-gallery leaves wing boxes and predictions out. Running it again replaces the earlier import.

import { DatabaseSync } from 'node:sqlite';
import { readFileSync, writeFileSync, statSync } from 'node:fs';
import { gunzipSync, gzipSync } from 'node:zlib';
import { GALLERY_URL, importBundle, readCuration, readGallery } from '../server/envelope-import.mjs';

const args = process.argv.slice(2);
const option = name => {
  const i = args.indexOf(name);
  if (i < 0) return undefined;
  const [, value] = args.splice(i, 2);
  return value;
};
const out = option('--bundle');
const gallery = option('--gallery') ?? GALLERY_URL;
const dbPath = option('--db') ?? process.env.DATABASE_PATH;
const noGallery = args.includes('--no-gallery') && args.splice(args.indexOf('--no-gallery'), 1);
const [first, manifest] = args;
if (!first || (!manifest && !/\.json(\.gz)?$/.test(first))) {
  console.error('Usage: import-envelope-curation.mjs [--bundle out.json.gz] <curation-dir> <manifest.csv> [--gallery dir|url] | <bundle.json.gz>');
  process.exit(2);
}

let bundle;
if (manifest) {
  bundle = readCuration(first, manifest);
  if (!noGallery) Object.assign(bundle, await readGallery(gallery));
} else {
  const raw = readFileSync(first);
  bundle = JSON.parse((first.endsWith('.gz') ? gunzipSync(raw) : raw).toString('utf8'));
}

if (out) {
  writeFileSync(out, gzipSync(JSON.stringify(bundle)), { mode: 0o600 });
  console.log(
    `Bundle ${out} (${Math.round(statSync(out).size / 1024)} KB): ${bundle.readings.length} readings, ${bundle.flags.length} flags, ${bundle.wingBoxes?.length ?? 0} wing boxes, ${bundle.predictions?.length ?? 0} predictions`,
  );
} else {
  if (!dbPath) {
    console.error('Set DATABASE_PATH (or --db) to the app database');
    process.exit(2);
  }
  const db = new DatabaseSync(dbPath);
  // The app may be running: wait for its writes instead of failing.
  db.exec('PRAGMA busy_timeout=10000; PRAGMA journal_mode=WAL');
  const counts = importBundle(db, bundle);
  db.close();
  console.log(`Imported into ${dbPath}:`, counts);
}
