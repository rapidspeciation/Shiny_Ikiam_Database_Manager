#!/usr/bin/env node
import { DatabaseSync, backup } from 'node:sqlite';
import { mkdir, readdir, unlink, chmod } from 'node:fs/promises';
import { join, dirname, resolve } from 'node:path';

const source = resolve(process.env.DATABASE_PATH || '.local/app.sqlite');
const directory = resolve(process.env.BACKUP_DIR || join(dirname(source), 'backups'));
await mkdir(directory, { recursive: true, mode: 0o700 });
const stamp = new Date().toISOString().replaceAll(':', '-').replaceAll('.', '-');
const destination = join(directory, `ithomiini-${stamp}.sqlite`);
const database = new DatabaseSync(source, { readOnly: true });
try { await backup(database, destination); } finally { database.close(); }
await chmod(destination, 0o600);
const check = new DatabaseSync(destination, { readOnly: true });
try {
  const result = check.prepare('PRAGMA integrity_check').get();
  if (Object.values(result)[0] !== 'ok') throw new Error('Backup integrity check failed');
} finally { check.close(); }
const files = (await readdir(directory)).filter(name => /^ithomiini-\d{4}-.*\.sqlite$/.test(name)).sort().reverse();
for (const name of files.slice(14)) await unlink(join(directory, name));
console.log(`Verified backup created: ${destination}`);
