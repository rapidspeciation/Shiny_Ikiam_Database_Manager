// Contact sheets: candidate photos on a numbered grid (3 across, up to 3 down, 512 px tiles) for the chat to look at
// and pick from before photos are sent (host_plants and gallery_specimens with `photo_sheet`, then
// send_sheet_photos). The thumbnails are fetched together and kept; Pillow (python3, as server/map-snapshot.mjs)
// makes the grid; a sheet and what each tile stands for are kept on disk, so the pick, which may reach another tools
// worker, finds them. The same tiles give the same sheet again without fetching anything.

import { execFile } from 'node:child_process';
import { createHash } from 'node:crypto';
import { readFileSync, readdirSync, statSync, unlinkSync } from 'node:fs';
import { join } from 'node:path';
import { writeCacheFile } from './cache-write.mjs';

export const TILE = 512;
export const PER_SHEET = 9;
const THUMB_BYTES = 4 * 1024 * 1024;
// Sheets and thumbnails older than this are deleted (checked at most once an hour).
const KEEP_MS = 7 * 86400_000;

const SCRIPT = String.raw`
import json, sys
from PIL import Image, ImageDraw, ImageFont
job = json.load(sys.stdin)
T, BAND = job['tile'], 64
cols = min(3, len(job['tiles']))
rows = (len(job['tiles']) + cols - 1) // cols
img = Image.new('RGB', (cols * T, rows * (T + BAND)), 'white')
def font(size, bold=False):
    names = ['DejaVuSans-Bold.ttf', 'LiberationSans-Bold.ttf'] if bold else ['DejaVuSans.ttf', 'LiberationSans-Regular.ttf']
    for d in ['/usr/share/fonts/truetype/dejavu/', '/usr/share/fonts/TTF/', '/usr/share/fonts/truetype/liberation/', '/usr/share/fonts/liberation/', '']:
        for n in names:
            try:
                return ImageFont.truetype(d + n, size)
            except Exception:
                pass
    return ImageFont.load_default()
big, small = font(30, True), font(19)
draw = ImageDraw.Draw(img)
for i, t in enumerate(job['tiles']):
    x, y = (i % cols) * T, (i // cols) * (T + BAND)
    try:
        ph = Image.open(t['path']).convert('RGB')
        ph.thumbnail((T, T))
        img.paste(ph, (x + (T - ph.width) // 2, y + BAND + (T - ph.height) // 2))
    except Exception:
        draw.text((x + 20, y + BAND + T // 2), 'no photo', fill='grey', font=small)
    draw.rectangle([x, y, x + T - 1, y + BAND - 1], fill=(30, 30, 30))
    draw.text((x + 8, y + 14), str(t['n']), fill=(255, 210, 0), font=big)
    draw.text((x + 62, y + 6), t['line1'][:44], fill='white', font=small)
    draw.text((x + 62, y + 34), t['line2'][:48], fill=(200, 200, 200), font=small)
    draw.rectangle([x, y, x + T - 1, y + T + BAND - 1], outline='white', width=2)
img.save(job['out'], 'JPEG', quality=82)
`;

/** `dir`: where sheets, their tiles and the thumbnails go. `fetchImpl`, `python`: for tests. */
export function createPhotoSheets({ dir, fetchImpl = fetch, python = 'python3', timeoutMs = 20_000 } = {}) {
  const thumbs = new Map();
  const kept = new Map();

  function thumb(url) {
    if (thumbs.has(url)) return thumbs.get(url);
    const path = join(dir, 'thumbs', `${createHash('sha1').update(url).digest('hex').slice(0, 20)}.jpg`);
    const loading = (async () => {
      try {
        if (statSync(path).size > 0) return path;
      } catch {
        /* not kept yet */
      }
      const r = await fetchImpl(url, { headers: { 'user-agent': 'ithomiini-database' }, signal: AbortSignal.timeout(15_000) });
      if (!r.ok) throw new Error(`thumbnail ${r.status}`);
      if (Number(r.headers.get('content-length')) > THUMB_BYTES) throw new Error('thumbnail too large');
      const parts = [];
      let size = 0;
      for await (const part of r.body ?? []) {
        size += part.length;
        if (size > THUMB_BYTES) throw new Error('thumbnail too large');
        parts.push(Buffer.from(part));
      }
      const buf = Buffer.concat(parts);
      if (!writeCacheFile(path, buf)) throw new Error('thumbnail not kept');
      return path;
    })();
    thumbs.set(url, loading);
    loading.catch(() => thumbs.delete(url));
    return loading;
  }

  /**
   * One sheet of up to PER_SHEET tiles [{ thumb (URL), line1, line2, ...what the pick needs }], numbered from `first`.
   * -> { id, path, tiles: [{ n, line1, line2 }] }
   */
  async function make(tiles, { first = 1 } = {}) {
    if (!dir) throw new Error('Contact sheets are off on this server (no folder for them).');
    const numbered = tiles.slice(0, PER_SHEET).map((t, i) => ({ ...t, n: first + i }));
    // Everything a tile stands for is in the id: the same photos asked in another way (another area, reach or
    // language) make another sheet, so a pick never gets an earlier question's captions.
    const id = createHash('sha1').update(JSON.stringify(numbered)).digest('hex').slice(0, 16);
    prune();
    const out = join(dir, `sheet-${id}.jpg`);
    const record = join(dir, `sheet-${id}.json`);
    let exists = false;
    try {
      exists = statSync(out).size > 0 && !JSON.parse(readFileSync(record, 'utf8')).partial;
    } catch {
      /* not made yet */
    }
    if (!exists) {
      const paths = await Promise.all(numbered.map(t => thumb(t.thumb).catch(() => null)));
      // A missing thumbnail is tried again next time: the sheet is kept only when all came.
      const whole = paths.every(Boolean);
      await new Promise((resolve, reject) => {
        const child = execFile(python, ['-c', SCRIPT], { timeout: timeoutMs, maxBuffer: 1 << 20 }, (error, stdout, stderr) =>
          error ? reject(new Error(`The contact sheet failed: ${String(stderr || error.message).trim().split('\n').at(-1)}`)) : resolve(stdout),
        );
        child.stdin.end(JSON.stringify({ tile: TILE, out, tiles: numbered.map((t, i) => ({ n: t.n, line1: t.line1, line2: t.line2, path: paths[i] })) }));
      });
      writeCacheFile(record, JSON.stringify({ id, made: new Date().toISOString(), ...(whole ? {} : { partial: true }), tiles: numbered }));
    }
    kept.set(id, numbered);
    return { id, path: out, tiles: numbered.map(t => ({ n: t.n, line1: t.line1, line2: t.line2 })) };
  }

  /** The tiles of a sheet made here or by another worker: [{ n, ... }], or null. */
  function tilesOf(id) {
    if (!/^[0-9a-f]{16}$/.test(String(id ?? ''))) return null;
    if (kept.has(id)) return kept.get(id);
    try {
      const tiles = JSON.parse(readFileSync(join(dir, `sheet-${id}.json`), 'utf8')).tiles;
      kept.set(id, tiles);
      return tiles;
    } catch {
      return null;
    }
  }

  let pruned = 0;
  function prune() {
    if (Date.now() - pruned < 3600_000) return;
    pruned = Date.now();
    for (const sub of ['', 'thumbs']) {
      try {
        for (const name of readdirSync(join(dir, sub))) {
          const file = join(dir, sub, name);
          try {
            if (Date.now() - statSync(file).mtimeMs > KEEP_MS && statSync(file).isFile()) unlinkSync(file);
          } catch {
            /* gone or a folder */
          }
        }
      } catch {
        /* no folder yet */
      }
    }
  }

  return { make, tilesOf };
}
