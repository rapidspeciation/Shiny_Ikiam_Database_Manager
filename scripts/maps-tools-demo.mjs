// Runs the maps and gallery tools (server/maps-tools.mjs) on the published sites and a copy of the app's
// database opened read-only, and writes each answer as JSON: the material of docs/assistant-tools-maps-gallery-demo.md.
//   node scripts/maps-tools-demo.mjs <app.sqlite> <out folder>
import { mkdirSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';
import { DatabaseSync } from 'node:sqlite';
import { createMapsTools } from '../server/maps-tools.mjs';

const [dbPath, out] = process.argv.slice(2);
if (!dbPath || !out) throw new Error('usage: node scripts/maps-tools-demo.mjs <app.sqlite> <out folder>');
mkdirSync(out, { recursive: true });
const db = new DatabaseSync(dbPath, { readOnly: true });
const tools = createMapsTools({ store: { db } });

const CALLS = [
  ['q1-localities-oleria-onega', 'maps_localities', { taxa: ['Oleria onega'], country: ['Ecuador'], limit: 8 }],
  ['q2-localities-hypomenitis-1000-2000', 'maps_localities', { taxa: ['Hypomenitis libethris'], elevation_min: 1000, elevation_max: 2000, limit: 6 }],
  ['q3-species-near-ikiam', 'maps_species', { near: { lat: -0.95, lng: -77.86, km: 10 }, limit: 15 }],
  ['q4a-hostplants-mechanitis-polymnia', 'host_plants', { butterfly: 'Mechanitis polymnia' }],
  ['q4b-hostplants-on-cestrum', 'host_plants', { plant: 'Cestrum' }],
  ['q5-gallery-methona-confusa-female', 'gallery_specimens', { species: ['Methona confusa'], sex: 'female', limit: 8, thumbnails: 4 }],
  ['q5b-gallery-melinaea-ikiam', 'gallery_specimens', { genus: ['Melinaea'], locality: 'Ikiam', limit: 5, thumbnails: 2 }],
];
for (const [file, name, args] of CALLS) {
  const started = performance.now();
  const answer = await tools.run(name, args);
  const ms = Math.round(performance.now() - started);
  writeFileSync(join(out, `${file}.json`), JSON.stringify({ tool: name, args, ms, answer }, null, 2));
  console.log(file, `${ms} ms`, answer.error ?? `${JSON.stringify(answer).length} chars`);
}
console.log('heap MB', Math.round(process.memoryUsage().heapUsed / 1e6), 'rss MB', Math.round(process.memoryUsage().rss / 1e6));
