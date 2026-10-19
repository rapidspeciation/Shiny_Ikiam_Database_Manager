// Pictures made from photos for the assistant (server/photo-views.mjs): parts of photos cut, turned and
// put side by side in one JPEG. A child process of the app, as the photo reader is, with the reader's
// own sharp (photo-reader/node_modules: this platform's build), so the app's thread loads no image library.
//
//   node server/photo-view-worker.mjs --sharp <photo-reader dir>
//
// stdin, one JSON line per picture: { id, out, parts: [{ src, box, turn, wide }], side, height, gap, wide }.
// `box` [x0, y0, x1, y1]: fractions of the photo as shown (its orientation tag applied); `turn`: 0 or
// 180, clockwise. One part: its long side at most `side`. Several: each `height` px high, `gap` px
// apart on white. `wide` (true/false): the photo's shape its boxes were read on; a photo of the other
// shape (turned since) is answered { id, error: 'shape' }. stdout, one line each, in order:
// { id, out, width, height } or { id, error }. A line {"ready": true} comes first.

import { createRequire } from 'node:module';
import { join } from 'node:path';
import { createInterface } from 'node:readline';

const arg = name => {
  const i = process.argv.indexOf(`--${name}`);
  return i >= 0 ? process.argv[i + 1] : undefined;
};
const sharp = createRequire(join(arg('sharp') ?? '.', 'photo-reader.mjs'))('sharp');
sharp.concurrency(1);
sharp.cache(false);

const clamp = v => Math.min(1, Math.max(0, Number(v) || 0));

/** A part of a photo as shown, turned: a JPEG buffer and its size. */
async function part({ src, box = [0, 0, 1, 1], turn = 0, wide }) {
  const meta = await sharp(src).metadata();
  const sideways = (meta.orientation ?? 1) >= 5;
  const width = (sideways ? meta.height : meta.width) ?? 0;
  const height = (sideways ? meta.width : meta.height) ?? 0;
  if (typeof wide === 'boolean' && width !== height && width > height !== wide) throw Object.assign(new Error('shape'), { shape: true });
  const [x0, y0, x1, y1] = box.map(clamp);
  const left = Math.min(width - 1, Math.round(Math.min(x0, x1) * width));
  const top = Math.min(height - 1, Math.round(Math.min(y0, y1) * height));
  const w = Math.max(1, Math.min(width - left, Math.round(Math.abs(x1 - x0) * width)));
  const h = Math.max(1, Math.min(height - top, Math.round(Math.abs(y1 - y0) * height)));
  const cut = await sharp(src).rotate().extract({ left, top, width: w, height: h }).toBuffer();
  return turn === 180 ? sharp(cut).rotate(180).toBuffer() : cut;
}

async function make(job) {
  const parts = await Promise.all((job.parts ?? []).map(p => part({ wide: job.wide, ...p })));
  if (!parts.length) return { id: job.id, error: 'no parts' };
  const quality = 80;
  if (parts.length === 1) {
    const side = Math.max(64, Math.min(2400, Number(job.side) || 800));
    const info = await sharp(parts[0]).resize(side, side, { fit: 'inside', withoutEnlargement: true }).jpeg({ quality }).toFile(job.out);
    return { id: job.id, out: job.out, width: info.width, height: info.height };
  }
  const height = Math.max(64, Math.min(1600, Number(job.height) || 400));
  const gap = Math.max(0, Number(job.gap ?? 8));
  const sized = await Promise.all(parts.map(p => sharp(p).resize({ height }).toBuffer({ resolveWithObject: true })));
  const width = sized.reduce((n, s) => n + s.info.width, 0) + gap * (sized.length - 1);
  let left = 0;
  const layers = sized.map(s => {
    const layer = { input: s.data, left, top: 0 };
    left += s.info.width + gap;
    return layer;
  });
  const info = await sharp({ create: { width, height, channels: 3, background: '#ffffff' } }).composite(layers).jpeg({ quality }).toFile(job.out);
  return { id: job.id, out: job.out, width: info.width, height: info.height };
}

process.stdout.write(`${JSON.stringify({ ready: true })}\n`);
for await (const line of createInterface({ input: process.stdin })) {
  if (!line.trim()) continue;
  let job;
  try {
    job = JSON.parse(line);
  } catch {
    continue;
  }
  const answer = await make(job).catch(e => ({ id: job.id, error: e.shape ? 'shape' : String(e.message || e).slice(0, 200) }));
  process.stdout.write(`${JSON.stringify(answer)}\n`);
}
