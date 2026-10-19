// Views of a specimen photo for the assistant (photo_files_look `view`, reference_photos): the wings
// only, the envelope only, or both side by side in one small picture, cut from Drive's 1600 px copy
// with the boxes read from the photo archive (server/photodata.mjs: photo_wing_boxes, fractions of the
// photo, else the box of each wing in photo_wing_sides; envelope_readings' bbox in the pixels of the
// photo it was read on). A view smaller than the
// whole photo costs the model fewer tokens. The pictures are made by a child process
// (server/photo-view-worker.mjs) with the photo reader's sharp.

import { spawn } from 'node:child_process';
import { existsSync } from 'node:fs';
import { join } from 'node:path';
import { createInterface } from 'node:readline';
import { photoIndex, reviewData } from './photodata.mjs';

export const VIEWS = ['full', 'wings', 'envelope', 'both'];
/** Each view's picture: its long side (one part) or its height (parts side by side). */
export const VIEW_SIZES = { wings: 640, envelope: 560, both: 360 };
/** A wing box with a little room around it, and an envelope's. */
const WING_MARGIN = 0.02;
const ENVELOPE_MARGIN = 0.01;

const WORKER = new URL('./photo-view-worker.mjs', import.meta.url).pathname;
const round = n => Math.round(Math.min(Math.max(n, 0), 1) * 10000) / 10000;
const valid = b => Array.isArray(b) && b.length === 4 && b.every(Number.isFinite) && b[0] < b[2] && b[1] < b[3];
const grown = ([x0, y0, x1, y1], m) => [round(x0 - m), round(y0 - m), round(x1 + m), round(y1 + m)];
const stemOf = name => String(name ?? '').replace(/\.[^.]+$/, '').trim();
const perWing = text => {
  try {
    const list = text ? JSON.parse(text) : [];
    return Array.isArray(list) ? list : [];
  } catch {
    return [];
  }
};
const byFile = new WeakMap();
/** The envelope readings by Drive ID, kept with the imported data they come from. */
const readingsByFile = data => {
  if (!byFile.has(data)) byFile.set(data, new Map([...data.readings.values()].filter(r => r.fileId).map(r => [r.fileId, r])));
  return byFile.get(data);
};

/**
 * The wings' box of a photo from photo_wing_boxes: one box [x0, y0, x1, y1], or a box per wing
 * ([[…], …] or [{ box }, …]) taken together.
 */
export function wingBoxOf(value) {
  if (valid(value)) return value;
  const list = (Array.isArray(value) ? value : Array.isArray(value?.wings) ? value.wings : []).map(b => (valid(b) ? b : b?.box)).filter(valid);
  if (!list.length) return null;
  return [Math.min(...list.map(b => b[0])), Math.min(...list.map(b => b[1])), Math.max(...list.map(b => b[2])), Math.max(...list.map(b => b[3]))];
}

/**
 * What is known of where things are in a photo (by its Drive ID or name): { name, wings, envelope:
 * { box, turned }, wide } (fractions of the photo as shown; `wide`: the shape the envelope was read on).
 */
export function photoBoxes(store, { id = null, name = null } = {}) {
  const data = reviewData(store);
  const index = photoIndex(store);
  const stem = stemOf(name ?? (id && index.files.get(id)?.name) ?? '');
  const key = stem.toUpperCase();
  const reading = (key && data.readings.get(key)) || (id ? readingsByFile(data).get(id) : null) || null;
  const named = key || reading?.name?.toUpperCase() || '';
  // The photo's wing box; else the boxes of its wings, from the wing models' reading (photo readings).
  const sides = (id && data.wings.byFile.get(id)) || data.wings.byName.get(named)?.[0];
  const wings = wingBoxOf(data.boxes.get(named)) ?? wingBoxOf(perWing(sides?.wingsJson).filter(w => w?.kind !== 'extra'));
  let envelope = null;
  let wide = null;
  if (reading && valid(reading.bbox) && Array.isArray(reading.size) && reading.size[0] > 0 && reading.size[1] > 0) {
    const [w, h] = reading.size;
    const [x0, y0, x1, y1] = reading.bbox;
    envelope = { box: [round(x0 / w), round(y0 / h), round(x1 / w), round(y1 / h)], turned: reading.turned === 180 ? 180 : 0 };
    if (w !== h) wide = w > h;
  }
  return { name: stem || reading?.name || null, wings, envelope, wide };
}

/**
 * The picture a view of a photo is made of: { parts: [{ box, turn }], side | height, wide } or, when
 * a box it needs is not known, { missing: 'why' } (the whole photo is shown instead).
 */
export function viewPlan(view, boxes) {
  const wings = boxes?.wings ? grown(boxes.wings, WING_MARGIN) : null;
  const envelope = boxes?.envelope ? { box: grown(boxes.envelope.box, ENVELOPE_MARGIN), turn: boxes.envelope.turned } : null;
  const wide = typeof boxes?.wide === 'boolean' ? boxes.wide : undefined;
  if (view === 'wings') return wings ? { parts: [{ box: wings, turn: 0 }], side: VIEW_SIZES.wings, wide } : { missing: 'no wing box for this photo' };
  if (view === 'envelope') return envelope ? { parts: [envelope], side: VIEW_SIZES.envelope, wide } : { missing: 'no envelope box for this photo (its envelope was not read)' };
  if (view === 'both') {
    if (!wings || !envelope) return { missing: !wings && !envelope ? 'no wing or envelope box for this photo' : !wings ? 'no wing box for this photo' : 'no envelope box for this photo (its envelope was not read)' };
    return { parts: [{ box: wings, turn: 0 }, envelope], height: VIEW_SIZES.both, wide };
  }
  return { missing: null };
}

/**
 * The child process that makes the pictures, started on the first and ended by close(): { make(job)
 * → { out, width, height } or { error } , close() }. `readerDir`: where the photo reader (and its
 * sharp) is.
 */
export function startViewer(readerDir) {
  if (!readerDir || !existsSync(join(readerDir, 'node_modules', 'sharp')))
    throw new Error('This server has no photo reader installed (photo-reader/: scripts/build-rename-app.sh), which makes the views.');
  const child = spawn(process.execPath, [WORKER, '--sharp', readerDir], { stdio: ['pipe', 'pipe', 'pipe'] });
  const waiting = new Map();
  let seq = 0;
  let ready;
  const isReady = new Promise((resolve, reject) => {
    ready = resolve;
    child.once('error', reject);
  });
  child.stderr.on('data', d => console.error('Photo views:', String(d).slice(0, 300)));
  child.on('exit', code => {
    for (const [, w] of waiting) w.resolve({ error: `the views' process stopped (${code})` });
    waiting.clear();
  });
  child.stdin.on('error', () => {});
  createInterface({ input: child.stdout }).on('line', line => {
    let out;
    try {
      out = JSON.parse(line);
    } catch {
      return;
    }
    if (out.ready) return ready();
    waiting.get(out.id)?.resolve(out);
    waiting.delete(out.id);
  });
  return {
    async make(job) {
      await isReady;
      const id = String(++seq);
      return new Promise(resolve => {
        waiting.set(id, { resolve });
        child.stdin.write(`${JSON.stringify({ ...job, id })}\n`);
      });
    },
    close: () => child.stdin.end(),
  };
}
