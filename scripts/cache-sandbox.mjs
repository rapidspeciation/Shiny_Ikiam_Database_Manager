#!/usr/bin/env node
// Private startup cache. This only reads the hard-coded personal test workbook.
import { mkdir, writeFile, chmod } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import { GoogleSheets } from '../server/sheets.mjs';
import { modules } from '../server/schema.mjs';

const destination = resolve(process.argv[2] || '.local/runtime/seed.json');
const sheets = new GoogleSheets();
const seed = {};
for (const module of modules) {
  seed[module.id] = await sheets.readSheet(module.id);
  console.log(`${module.id}: ${seed[module.id].length} rows cached`);
  await new Promise(resolve => setTimeout(resolve, 1100));
}
await mkdir(dirname(destination), { recursive: true, mode: 0o700 });
await writeFile(destination, JSON.stringify({ sheets: seed }), { mode: 0o600 });
await chmod(destination, 0o600);
console.log(`Saved ${modules.length} sheets to private startup cache`);
