// The OCR's line boxes that envelope imports before 23 Oct 2026 left out (envelope-import.mjs now keeps
// them): put back into an app database from the archive scan's readings-*.jsonl, so the photo archive
// review outlines the line each CAMID was read from. A reading is only given its boxes when its lines
// are still the ones of that scan (text and confidence); a photo read again since keeps its own.
//   node scripts/restore-ocr-boxes.mjs <app database> <folder of readings-*.jsonl>
import { DatabaseSync } from 'node:sqlite';
import { readdirSync, readFileSync } from 'node:fs';
import { join } from 'node:path';
import { bumpReviewRevision } from '../server/photodata.mjs';

const [database, source] = process.argv.slice(2);
if (!database || !source) throw new Error('Usage: node scripts/restore-ocr-boxes.mjs <app database> <folder of readings-*.jsonl>');
const db = new DatabaseSync(database);
db.exec('PRAGMA busy_timeout = 10000');
const get = db.prepare('SELECT camid_lines_json FROM envelope_readings WHERE name=?');
const set = db.prepare('UPDATE envelope_readings SET camid_lines_json=? WHERE name=?');
let restored = 0;
let newer = 0;
db.exec('BEGIN IMMEDIATE');
try {
  for (const name of readdirSync(source).filter(n => /^readings.*\.jsonl$/.test(n))) {
    for (const line of readFileSync(join(source, name), 'utf8').split('\n').filter(Boolean)) {
      const reading = JSON.parse(line);
      if (!reading.name || !reading.lines?.length) continue;
      const row = get.get(reading.name);
      if (!row) continue;
      const stored = JSON.parse(row.camid_lines_json || '[]');
      if (stored.some(l => Array.isArray(l[2]))) continue;
      const pairs = reading.lines.map(l => [l.t, Math.round(l.c * 1000) / 1000]);
      if (JSON.stringify(stored) !== JSON.stringify(pairs)) { newer++; continue; }
      const geometry = reading.lines.map(l => [l.t, Math.round(l.c * 1000) / 1000, ...(Array.isArray(l.b) ? [l.b, l.turn ?? 0] : [])]);
      if (!geometry.some(l => l[2])) continue;
      set.run(JSON.stringify(geometry), reading.name);
      restored++;
    }
  }
  if (restored) bumpReviewRevision(db);
  db.exec('COMMIT');
} catch (error) {
  db.exec('ROLLBACK');
  throw error;
} finally {
  db.close();
}
console.log(JSON.stringify({ restored, newerReadingsKept: newer }));
