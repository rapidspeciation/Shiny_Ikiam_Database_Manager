// The notebook photos a proposal was read from (match_notebook's `photo` and
// `rotate`; show_rows's for a table of rows), shown beside its table: a small
// upright copy for the page's header and a larger one to open in a new tab,
// each with the assistant's few words on why it is there. The photos are T3
// Code chat attachments (<T3 home>/userdata/attachments/<threadId>-<uuid>.jpg); only a
// file of that folder whose name starts with the proposal's chat is served.
//
// WhatsApp photos carry no EXIF orientation (a page taken sideways stays
// sideways), so the turn comes from the reader (clockwise, as crops.py's
// --rotate). The copies are made by Pillow (python3, as the reader's crops.py;
// the server has no image library of its own) and kept as files next to the
// specimen photos' cache; without Pillow the photo is served as it is.

import { execFile } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdirSync, readFileSync, realpathSync, rmSync, statSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { basename, join, sep } from 'node:path';

/** Longest side of each copy, in pixels. */
export const PHOTO_SIZES = { thumb: 240, view: 1600 };
const NAME = /^[\w-][\w.-]{0,199}\.(?:jpe?g|png|webp)$/i;
const MIME = { jpg: 'image/jpeg', jpeg: 'image/jpeg', png: 'image/png', webp: 'image/webp' };

export const attachmentsDir = home => join(home, 'userdata', 'attachments');

/** A turn as the reader gives it: 0, 90, 180 or 270 (clockwise). */
export const rightAngle = value => {
  const n = Math.round(Number(value) / 90);
  return Number.isFinite(n) ? (((n * 90) % 360) + 360) % 360 : 0;
};

/**
 * The attachment a photo argument names (its file name, or the path the chat
 * gives: only the name counts), or null: it must be a file directly in the T3
 * attachments folder (links followed). Any chat's: a new chat may gather the
 * pages left pending in older chats into its own proposals.
 */
export function attachmentFile(home, value) {
  if (!home || typeof value !== 'string' || !value.trim()) return null;
  const name = basename(value.trim().replaceAll('\\', '/'));
  if (!NAME.test(name)) return null;
  try {
    const dir = realpathSync(attachmentsDir(home));
    const path = realpathSync(join(dir, name));
    if (!path.startsWith(dir + sep) || path.slice(dir.length + 1).includes(sep) || !statSync(path).isFile()) return null;
    return { name, path };
  } catch {
    return null;
  }
}

/** Longest note on a photo (why it is there), in characters. */
export const PHOTO_NOTE_LENGTH = 80;
const noteOf = value => (typeof value === 'string' ? value.replace(/\s+/g, ' ').trim().slice(0, PHOTO_NOTE_LENGTH) : null);

/**
 * The photos of a match_notebook call (update_proposal's and show_rows's too):
 * `photo` a name or a list, each a file name or { name, note } (a few words on
 * why the photo is there: "old IDs (21 Sep page)"); `rotate` a turn or a list
 * (one per photo; one turn for all). `old`: the photos kept so far; one given
 * again by name alone keeps its note. Returns { photos, refused }.
 */
export function photosOf(home, args, old = []) {
  const given = Array.isArray(args.photo) ? args.photo : args.photo ? [args.photo] : [];
  const names = given.slice(0, 12);
  const turns = Array.isArray(args.rotate) ? args.rotate : names.map(() => args.rotate);
  const notes = new Map((Array.isArray(old) ? old : []).filter(p => p?.note).map(p => [p.file, p.note]));
  const photos = [];
  const refused = [];
  names.forEach((value, i) => {
    const named = value && typeof value === 'object' ? value : null;
    const file = attachmentFile(home, named ? (named.name ?? named.file) : value);
    if (!file) return refused.push(String((named ? (named.name ?? named.file) : value) ?? '').slice(0, 200));
    const note = named && 'note' in named ? noteOf(named.note) : (notes.get(file.name) ?? null);
    photos.push({ file: file.name, rotate: rightAngle(turns[i] ?? 0), ...(note ? { note } : {}) });
  });
  return { photos, refused };
}

/**
 * What the page sends of its photos: how many, a short tag of which (the note
 * included, so a new caption shows at once) and their notes in the same order
 * (only when one has a note).
 */
export function photoPage(photos) {
  if (!Array.isArray(photos) || !photos.length) return { photos: 0, photoKey: null };
  const photoKey = createHash('sha1').update(JSON.stringify(photos)).digest('base64url').slice(0, 8);
  const notes = photos.map(p => p.note ?? '');
  return { photos: photos.length, photoKey, ...(notes.some(Boolean) ? { photoNotes: notes } : {}) };
}

const SCRIPT = `
import sys
from PIL import Image, ImageOps
src, out, turn, size = sys.argv[1], sys.argv[2], int(sys.argv[3]), int(sys.argv[4])
im = ImageOps.exif_transpose(Image.open(src))
if turn:
    im = im.rotate(-turn, expand=True)
im.thumbnail((size, size))
im.convert('RGB').save(out, 'JPEG', quality=82, optimize=True)
`;

/**
 * Upright copies of attachments. dir: where they are kept (null: in memory,
 * the last few). python: the interpreter with Pillow.
 */
export function createPhotoCopies({ dir = null, python = 'python3', timeoutMs = 30000 } = {}) {
  if (dir) mkdirSync(dir, { recursive: true, mode: 0o700 });
  const memory = new Map();
  const inFlight = new Map();
  const run = (src, out, rotate, size) =>
    new Promise(resolve =>
      execFile(python, ['-c', SCRIPT, src, out, String(rotate), String(size)], { timeout: timeoutMs }, error => resolve(!error)),
    );
  /** { data, mime, etag, upright }: the copy, or the photo itself when no copy can be made. */
  async function get(path, rotate, size) {
    const stat = statSync(path);
    const key = createHash('sha256').update(`${path}\u0000${stat.size}\u0000${stat.mtimeMs}\u0000${rotate}\u0000${size}`).digest('hex').slice(0, 32);
    const etag = `"${key}"`;
    const file = dir ? join(dir, `${key}.jpg`) : null;
    try {
      if (file) return { data: readFileSync(file), mime: 'image/jpeg', etag, upright: true };
    } catch {
      /* not made yet */
    }
    if (memory.has(key)) return memory.get(key);
    if (!inFlight.has(key))
      inFlight.set(
        key,
        (async () => {
          const out = file ?? join(tmpdir(), `ithomiini-photo-${key}.jpg`);
          const made = await run(path, `${out}.part`, rotate, size);
          let copy = null;
          if (made)
            try {
              const data = readFileSync(`${out}.part`);
              if (file) writeFileSync(file, data, { mode: 0o600 });
              copy = { data, mime: 'image/jpeg', etag, upright: true };
            } catch {
              copy = null;
            }
          rmSync(`${out}.part`, { force: true });
          // Without Pillow: the photo as it is (sideways if it was taken so).
          copy ??= { data: readFileSync(path), mime: MIME[path.split('.').pop().toLowerCase()] ?? 'image/jpeg', etag: `"${key}-0"`, upright: false };
          if (!file || !copy.upright) {
            memory.set(key, copy);
            while (memory.size > 40) memory.delete(memory.keys().next().value);
          }
          return copy;
        })().finally(() => inFlight.delete(key)),
      );
    return inFlight.get(key);
  }
  return { get };
}
