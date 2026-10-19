#!/usr/bin/env node
// The identifier's and the sex model's readings of every Photo_links photo (a bundle made on the PC from the
// Sanger run, e.g. photo-predictions.json.gz: {version, source, predictions: [one per CAM], photoPredictions:
// [one per photo]}) → the app's database (photo_predictions, photo_file_predictions). They replace the Wings
// Gallery's predictions (scripts/import-envelope-curation.mjs); importing again replaces the earlier import.
//   DATABASE_PATH=… node scripts/import-photo-predictions.mjs photo-predictions.json.gz

import { DatabaseSync } from 'node:sqlite';
import { readFileSync } from 'node:fs';
import { gunzipSync } from 'node:zlib';
import { importBundle } from '../server/envelope-import.mjs';

const args = process.argv.slice(2);
const i = args.indexOf('--db');
const dbPath = i >= 0 ? args.splice(i, 2)[1] : process.env.DATABASE_PATH;
const [file] = args;
if (!file || !/\.json(\.gz)?$/.test(file)) {
  console.error('Usage: import-photo-predictions.mjs [--db app.sqlite] <bundle.json.gz>');
  process.exit(2);
}
const raw = readFileSync(file);
const bundle = JSON.parse((file.endsWith('.gz') ? gunzipSync(raw) : raw).toString('utf8'));
if (!Array.isArray(bundle.predictions) || !Array.isArray(bundle.photoPredictions)) {
  console.error(`${file} holds no photo predictions`);
  process.exit(2);
}
if (!dbPath) {
  console.error('Set DATABASE_PATH (or --db) to the app database');
  process.exit(2);
}
const db = new DatabaseSync(dbPath);
// The app may be running: wait for its writes instead of failing.
db.exec('PRAGMA busy_timeout=10000; PRAGMA journal_mode=WAL');
const counts = importBundle(db, {
  predictions: bundle.predictions,
  photoPredictions: bundle.photoPredictions,
  gallery: bundle.source ?? null,
  source: bundle.source ?? null,
});
db.close();
console.log(`Imported into ${dbPath}: ${counts.predictions} specimens, ${counts.photoPredictions} photos (${bundle.source ?? 'no source'})`);
