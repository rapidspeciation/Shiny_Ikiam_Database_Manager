#!/usr/bin/env node
import { DatabaseSync, backup } from 'node:sqlite';
import { mkdir, readdir, unlink, chmod, rm } from 'node:fs/promises';
import { createReadStream, createWriteStream } from 'node:fs';
import { pipeline } from 'node:stream/promises';
import { createGzip } from 'node:zlib';
import { join, dirname, resolve } from 'node:path';

const source = resolve(process.env.DATABASE_PATH || '.local/app.sqlite');
const directory = resolve(process.env.BACKUP_DIR || join(dirname(source), 'backups'));
await mkdir(directory, { recursive: true, mode: 0o700 });
const stamp = new Date().toISOString().replaceAll(':', '-').replaceAll('.', '-');
const destination = join(directory, `ithomiini-${stamp}.sqlite`);
const database = new DatabaseSync(source, { readOnly: true });
try {
  await backup(database, destination);
} finally {
  database.close();
}
await chmod(destination, 0o600);
const check = new DatabaseSync(destination, { readOnly: true });
try {
  const result = check.prepare('PRAGMA integrity_check').get();
  if (Object.values(result)[0] !== 'ok') throw new Error('Backup integrity check failed');
} finally {
  check.close();
}
// Checked, then kept compressed (a database compresses to about a fifth); restore with
// `gunzip -k ithomiini-….sqlite.gz`. Opening the copy read-only can leave -shm/-wal files: gone too.
await pipeline(createReadStream(destination), createGzip({ level: 6 }), createWriteStream(`${destination}.gz`, { mode: 0o600 }));
for (const suffix of ['', '-shm', '-wal']) await rm(destination + suffix, { force: true });
// The newest 14 daily backups stay (older uncompressed ones and their side files go too).
const files = (await readdir(directory))
  .filter(name => /^ithomiini-\d{4}-.*\.sqlite(\.gz)?$/.test(name))
  .sort()
  .reverse();
for (const name of files.slice(14)) await unlink(join(directory, name));
const kept = new Set(files.slice(0, 14).map(name => name.replace(/\.gz$/, '')));
for (const name of await readdir(directory)) {
  const base = name.replace(/-(shm|wal)$/, '');
  if (/^ithomiini-\d{4}-.*\.sqlite-(shm|wal)$/.test(name) && !kept.has(base)) await unlink(join(directory, name));
}
console.log(`Verified backup created: ${destination}.gz`);
