#!/usr/bin/env node
// The wing models' reading of the photos (which side the wings show, whether a photo is turned:
// ithomiini_maps models/dorsal_ventral/eval) → the app's database (photo_facts, Photo_links readings).
//
// On the PC that has the results (only the CSV files are read):
//   node scripts/import-wing-sides.mjs --bundle wing-sides.json.gz --manifest <photo manifest.csv> <eval-dir> [<eval-dir>…]
// e.g. <eval-dir> = models/dorsal_ventral/eval (the archive; its photos get their Drive IDs from the
// manifest's Name and google_id) and models/dorsal_ventral/eval/new_photos_20261015 (carries its own).
// That makes one small file to copy to the server; there:
//   DATABASE_PATH=… node scripts/import-wing-sides.mjs wing-sides.json.gz
// Or straight into a database (local tests): DATABASE_PATH=… node scripts/import-wing-sides.mjs --manifest … <eval-dir>…
// Running it again replaces the earlier import.
// A bundle made elsewhere (the nano wing model's run on every Photo_links photo, 18 Oct 2026:
// {version, source, wings: [...], wingBoxes: [[name, box, conf], ...]}) is imported the same way; its wing
// boxes then replace photo_wing_boxes too, and a row's `outlines` (one per wing of wingList: [kind, half, [x0,y0,…] 0..1000]
// or null, server/wing-outlines.mjs) go to photo_wing_sides.outlines_json.

import { DatabaseSync } from 'node:sqlite';
import { readFileSync, writeFileSync, statSync } from 'node:fs';
import { gunzipSync, gzipSync } from 'node:zlib';
import { importBundle, readWingSides } from '../server/envelope-import.mjs';

const args = process.argv.slice(2);
const option = name => {
  const i = args.indexOf(name);
  if (i < 0) return undefined;
  const [, value] = args.splice(i, 2);
  return value;
};
const out = option('--bundle');
const manifest = option('--manifest');
const dbPath = option('--db') ?? process.env.DATABASE_PATH;
if (!args.length) {
  console.error('Usage: import-wing-sides.mjs [--bundle out.json.gz] --manifest <manifest.csv> <eval-dir>… | <bundle.json.gz>');
  process.exit(2);
}

let bundle;
if (args.length === 1 && /\.json(\.gz)?$/.test(args[0])) {
  const raw = readFileSync(args[0]);
  bundle = JSON.parse((args[0].endsWith('.gz') ? gunzipSync(raw) : raw).toString('utf8'));
  if (!Array.isArray(bundle.wings)) {
    console.error(`${args[0]} holds no wing results`);
    process.exit(2);
  }
} else bundle = { version: 1, createdAt: new Date().toISOString(), ...readWingSides(args, manifest) };

const counted = () => {
  const by = {};
  for (const w of bundle.wings) by[w.source] = (by[w.source] ?? 0) + 1;
  return Object.entries(by).map(([s, n]) => `${n} ${s}`).join(', ');
};
if (out) {
  writeFileSync(out, gzipSync(JSON.stringify(bundle)), { mode: 0o600 });
  console.log(`Bundle ${out} (${Math.round(statSync(out).size / 1024)} KB): ${bundle.wings.length} photos (${counted()})`);
} else {
  if (!dbPath) {
    console.error('Set DATABASE_PATH (or --db) to the app database');
    process.exit(2);
  }
  const db = new DatabaseSync(dbPath);
  // The app may be running: wait for its writes instead of failing.
  db.exec('PRAGMA busy_timeout=10000; PRAGMA journal_mode=WAL');
  // Only the wing results (and their boxes, when the bundle has them): the curation's other tables are left as they are.
  const counts = importBundle(db, { wings: bundle.wings, ...(Array.isArray(bundle.wingBoxes) ? { wingBoxes: bundle.wingBoxes } : {}) });
  db.close();
  console.log(
    `Imported into ${dbPath}: ${counts.wings} photos (${counted()})${counts.wingBoxes ? `, ${counts.wingBoxes} wing boxes` : ''}`,
  );
}
