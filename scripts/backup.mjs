#!/usr/bin/env node
import { DatabaseSync, backup } from 'node:sqlite';
import { mkdir, readdir, unlink, chmod, rm, copyFile, stat } from 'node:fs/promises';
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
// The clutches' photos (server/clutch-photos.mjs: files, the database holds only their list) are
// copied beside the backups once each: a photo never changes under its name, and one taken away
// in the app stays here.
const photos = resolve(process.env.CLUTCH_PHOTO_DIR || join(dirname(source), 'clutch-photos'));
const photoCopy = join(directory, 'clutch-photos');
let copied = 0;
async function copyPhotos(from, to) {
  let entries;
  try {
    entries = await readdir(from, { withFileTypes: true });
  } catch {
    return;
  }
  for (const entry of entries) {
    if (entry.name === 'uploads') continue;
    const src = join(from, entry.name);
    const dst = join(to, entry.name);
    if (entry.isDirectory()) {
      await mkdir(dst, { recursive: true, mode: 0o700 });
      await copyPhotos(src, dst);
    } else if (entry.isFile() && entry.name.endsWith('.jpg')) {
      try {
        await stat(dst);
      } catch {
        await copyFile(src, dst);
        await chmod(dst, 0o600);
        copied++;
      }
    }
  }
}
await copyPhotos(photos, photoCopy);
if (copied) console.log(`Clutch photos copied: ${copied} new file(s) in ${photoCopy}`);
// The photos sent from Identificar to train the AI (server/contributions.mjs) are copied the same way, except that
// one deleted in the app is deleted here too (its tombstone in the database): the person was told it leaves the server.
// The daily database backups keep its row until they rotate out (14 days).
const contributions = resolve(process.env.CONTRIB_DIR || join(dirname(source), 'contributions'));
const contributionCopy = join(directory, 'contributions');
copied = 0;
await copyPhotos(contributions, contributionCopy);
if (copied) console.log(`Contributed photos copied: ${copied} new file(s) in ${contributionCopy}`);
let tombstones = [];
const tombstoneDb = new DatabaseSync(source, { readOnly: true });
try {
  if (tombstoneDb.prepare("SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = 'contribution_tombstones'").get())
    tombstones = tombstoneDb.prepare('SELECT id FROM contribution_tombstones').all().map(r => r.id);
} finally {
  tombstoneDb.close();
}
let dropped = 0;
async function dropDeleted(dir, ids) {
  let entries;
  try {
    entries = await readdir(dir, { withFileTypes: true });
  } catch {
    return;
  }
  for (const entry of entries) {
    const path = join(dir, entry.name);
    if (entry.isDirectory()) await dropDeleted(path, ids);
    else if (entry.isFile() && ids.has(entry.name.replace(/(\.thumb)?\.jpg$/, ''))) {
      await unlink(path);
      dropped++;
    }
  }
}
if (tombstones.length) await dropDeleted(contributionCopy, new Set(tombstones));
if (dropped) console.log(`Deleted contributed photos removed from the copy: ${dropped} file(s)`);
