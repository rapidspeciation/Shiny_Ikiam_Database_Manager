// A map picture without a browser (maps_snapshot, server/maps-tools.mjs): the basemap's raster tiles (Esri's
// World Topo Map: towns, roads, rivers and the relief; CARTO's raster tiles now need a key) laid side by side, the records on top as dots coloured by species, the
// recommended sites numbered, a legend and the attribution, made into a JPEG small enough for WhatsApp by Pillow
// (python3, as server/proposal-photos.mjs; claudeclaw has it, with DejaVu fonts). Tiles are kept on disk; the
// pictures for a week.

import { execFile } from 'node:child_process';
import { createHash } from 'node:crypto';
import { existsSync, mkdirSync, readdirSync, statSync, unlinkSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';
import { writeCacheFile } from './cache-write.mjs';

export const TILE_URL = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}';
export const ATTRIBUTION = 'Tiles © Esri — Esri, HERE, Garmin, FAO, NOAA, USGS, © OpenStreetMap contributors';
const TILE = 256;
const MAX_ZOOM = 13;
const MIN_ZOOM = 3;
const KEEP_MS = 7 * 86400_000;
const TILE_TRIES = 2;

/** Species colours: distinct on the light basemap, and for the colour-blind as far as ten colours go. */
export const PALETTE = ['#d62728', '#1f77b4', '#ff7f0e', '#2ca02c', '#9467bd', '#8c564b', '#e377c2', '#17becf', '#bcbd22', '#7f7f7f'];

const lngX = (lng, z) => ((lng + 180) / 360) * TILE * 2 ** z;
const latY = (lat, z) => {
  const s = Math.sin((Math.max(-85, Math.min(85, lat)) * Math.PI) / 180);
  return (0.5 - Math.log((1 + s) / (1 - s)) / (4 * Math.PI)) * TILE * 2 ** z;
};
const xLng = (x, z) => (x / (TILE * 2 ** z)) * 360 - 180;
const yLat = (y, z) => (Math.atan(Math.sinh(Math.PI * (1 - (2 * y) / (TILE * 2 ** z)))) * 180) / Math.PI;

/**
 * The view that holds `bbox` [west, south, east, north] in a `width`×`height` map (map pixels, before SCALE),
 * leaving `pad` pixels free on each side: the largest whole zoom, its centre and the bbox it shows.
 */
export function fitView(bbox, width, height, pad = 40) {
  const [w, s, e, n] = bbox;
  let zoom = MIN_ZOOM;
  for (let z = MAX_ZOOM; z >= MIN_ZOOM; z--) {
    if (lngX(e, z) - lngX(w, z) <= width - 2 * pad && latY(s, z) - latY(n, z) <= height - 2 * pad) {
      zoom = z;
      break;
    }
  }
  const cx = (lngX(w, zoom) + lngX(e, zoom)) / 2;
  const cy = (latY(s, zoom) + latY(n, zoom)) / 2;
  const shown = [xLng(cx - width / 2, zoom), yLat(cy + height / 2, zoom), xLng(cx + width / 2, zoom), yLat(cy - height / 2, zoom)];
  return { zoom, lng: xLng(cx, zoom), lat: yLat(cy, zoom), left: cx - width / 2, top: cy - height / 2, bbox: shown };
}

// Pillow: the tiles pasted, then the dots (white rim), the numbered sites, the legend band and the attribution.
const SCRIPT = String.raw`
import json, sys
from PIL import Image, ImageDraw, ImageFont
job = json.load(sys.stdin)
W, H = job['width'], job['height']
img = Image.new('RGB', (W, H), (232, 232, 228))
for t in job['tiles']:
    try:
        tile = Image.open(t['path']).convert('RGB')
        img.paste(tile, (t['x'], t['y']))
    except Exception:
        pass
def font(size, bold=False):
    names = ['DejaVuSans-Bold.ttf', 'LiberationSans-Bold.ttf'] if bold else ['DejaVuSans.ttf', 'LiberationSans-Regular.ttf']
    dirs = ['/usr/share/fonts/truetype/dejavu/', '/usr/share/fonts/TTF/', '/usr/share/fonts/truetype/liberation/', '/usr/share/fonts/liberation/', '']
    for d in dirs:
        for n in names:
            try:
                return ImageFont.truetype(d + n, size)
            except Exception:
                pass
    try:
        return ImageFont.load_default(size)
    except Exception:
        return ImageFont.load_default()
overlay = Image.new('RGBA', (W, H), (0, 0, 0, 0))
d = ImageDraw.Draw(overlay)
r = job['dot']
colors = [tuple(c) for c in job['colors']]
for x, y, c in job['points']:
    d.ellipse((x - r - 2, y - r - 2, x + r + 2, y + r + 2), fill=(255, 255, 255, 230))
    d.ellipse((x - r, y - r, x + r, y + r), fill=colors[c] + (235,))
img = Image.alpha_composite(img.convert('RGBA'), overlay)
d = ImageDraw.Draw(img)
f_site = font(22, True)
R = 17
for s in job['sites']:
    if (s['x'], s['y']) != (s['ax'], s['ay']):
        d.line((s['ax'], s['ay'], s['x'], s['y']), fill=(255, 255, 255), width=5)
        d.line((s['ax'], s['ay'], s['x'], s['y']), fill=(20, 20, 20), width=2)
        d.ellipse((s['ax'] - 4, s['ay'] - 4, s['ax'] + 4, s['ay'] + 4), fill=(20, 20, 20), outline=(255, 255, 255))
# The best site last, on top.
for s in reversed(job['sites']):
    x, y, label = s['x'], s['y'], str(s['rank'])
    d.ellipse((x - R - 3, y - R - 3, x + R + 3, y + R + 3), fill=(255, 255, 255))
    d.ellipse((x - R, y - R, x + R, y + R), fill=(20, 20, 20))
    d.text((x, y), label, font=f_site, fill=(255, 255, 255), anchor='mm')
f_title = font(26, True)
f_text = font(22)
f_small = font(18)
lines = job['legend']
band = 22 + 34 + 30 * len(lines) + 10
box = Image.new('RGBA', (W, H), (0, 0, 0, 0))
bd = ImageDraw.Draw(box)
width = max([d.textlength(job['title'], font=f_title)] + [d.textlength(l['text'], font=f_text) + 34 for l in lines]) + 36
bd.rounded_rectangle((14, 14, 14 + width, 14 + band), radius=10, fill=(255, 255, 255, 225))
img = Image.alpha_composite(img, box)
d = ImageDraw.Draw(img)
d.text((32, 26), job['title'], font=f_title, fill=(20, 20, 20))
y = 26 + 40
for l in lines:
    d.ellipse((34, y + 4, 52, y + 22), fill=colors[l['color']] if l['color'] is not None else (20, 20, 20), outline=(255, 255, 255))
    d.text((62, y), l['text'], font=f_text, fill=(30, 30, 30))
    y += 30
note = job['attribution']
tw = d.textlength(note, font=f_small)
d.rectangle((W - tw - 20, H - 30, W, H), fill=(255, 255, 255))
d.text((W - tw - 10, H - 27), note, font=f_small, fill=(70, 70, 70))
img.convert('RGB').save(job['out'], 'JPEG', quality=job['quality'], optimize=True)
print(json.dumps({'ok': True}))
`;

const MARK = 40; // two site markers closer than this (pixels) would overlap

/**
 * The numbered markers, best first: one that would cover a better one moves aside (right, down, left, up, then
 * further), with a line to its place. → [{ rank, x, y, ax, ay }] (x, y the marker; ax, ay the site).
 */
export function placeSites(sites) {
  const placed = [];
  const steps = [[1, 0], [0, 1], [-1, 0], [0, -1], [1, 1], [-1, 1], [1, -1], [-1, -1]];
  for (const { rank, xy } of sites) {
    let at = xy;
    for (let ring = 1, i = 0; placed.some(p => Math.hypot(p.x - at[0], p.y - at[1]) < MARK) && ring < 6; i++) {
      if (i === steps.length) (i = 0), ring++;
      at = [xy[0] + steps[i][0] * MARK * ring, xy[1] + steps[i][1] * MARK * ring];
    }
    placed.push({ rank, x: at[0], y: at[1], ax: xy[0], ay: xy[1] });
  }
  return placed;
}

const hex = c => [1, 3, 5].map(i => parseInt(c.slice(i, i + 2), 16));

/**
 * The renderer. `dir`: where the pictures and the tiles go; `fetchImpl`, `python`, `tileUrl`, `now`: for tests and
 * other hosts. render(...) → { path, bytes, width, height, view: { lng, lat, zoom (the maps app's, 512-pixel
 * tiles) }, bbox, tiles: { asked, fetched, failed }, ms }.
 */
export function createSnapshots({ dir, fetchImpl = fetch, python = 'python3', tileUrl = TILE_URL, scale = 1, now = Date.now, timeoutMs = 20_000 } = {}) {
  // `scale`: picture pixels to a map pixel (2 for tiles of 512 pixels, the "@2x" ones).
  const SCALE = scale;
  if (!dir) return null;
  const tileDir = join(dir, 'tiles');
  const loading = new Map();
  let cleaned = 0;

  async function tile(z, x, y) {
    const path = join(tileDir, String(z), String(x), `${y}.tile`);
    if (existsSync(path)) return { path, fetched: false };
    if (loading.has(path)) return loading.get(path);
    const job = (async () => {
      const url = tileUrl.replace('{z}', z).replace('{x}', x).replace('{y}', y);
      for (let i = 0; i < TILE_TRIES; i++) {
        try {
          const response = await fetchImpl(url, { headers: { 'user-agent': 'ithomiini-database (map snapshots)' }, signal: AbortSignal.timeout(8_000) });
          if (!response.ok) continue;
          const data = Buffer.from(await response.arrayBuffer());
          writeCacheFile(path, data);
          return { path, fetched: true };
        } catch {
          // tried again once, then the tile is left blank
        }
      }
      return { path: null, fetched: false, failed: true };
    })();
    loading.set(path, job);
    try {
      return await job;
    } finally {
      loading.delete(path);
    }
  }

  /** The pictures older than a week, deleted (at most once an hour). */
  function clean() {
    if (now() - cleaned < 3600_000) return;
    cleaned = now();
    for (const name of readdirSync(dir)) {
      if (!/^snap-[\w-]+\.jpg$/.test(name)) continue;
      const file = join(dir, name);
      try {
        if (now() - statSync(file).mtimeMs > KEEP_MS) unlinkSync(file);
      } catch {
        // gone already
      }
    }
  }

  /**
   * `bbox` [west, south, east, north]; `points` [{ lat, lng, color (PALETTE index) }]; `sites` [{ lat, lng, rank }];
   * `legend` [{ text, color }]; `title`. `width`/`height`: the picture's pixels.
   */
  async function render({ bbox, points = [], sites = [], legend = [], title = '', width = 1280, height = 960, quality = 80 }) {
    const started = now();
    mkdirSync(dir, { recursive: true });
    clean();
    const mapW = width / SCALE;
    const mapH = height / SCALE;
    const view = fitView(bbox, mapW, mapH, 36);
    const { zoom: z, left, top } = view;
    const n = 2 ** z;
    const jobs = [];
    for (let tx = Math.floor(left / TILE); tx * TILE < left + mapW; tx++)
      for (let ty = Math.max(0, Math.floor(top / TILE)); ty * TILE < top + mapH && ty < n; ty++)
        jobs.push(tile(z, ((tx % n) + n) % n, ty).then(t => ({ ...t, x: Math.round((tx * TILE - left) * SCALE), y: Math.round((ty * TILE - top) * SCALE) })));
    const tiles = await Promise.all(jobs);
    const tilesMs = now() - started;
    const px = p => [Math.round((lngX(p.lng, z) - left) * SCALE), Math.round((latY(p.lat, z) - top) * SCALE)];
    const inside = ([x, y]) => x >= -10 && y >= -10 && x <= width + 10 && y <= height + 10;
    const drawn = [];
    for (const p of points) {
      const xy = px(p);
      if (inside(xy)) drawn.push([...xy, p.color]);
    }
    const job = {
      width,
      height,
      quality,
      dot: z >= 11 ? 7 : z >= 8 ? 6 : 5,
      colors: PALETTE.map(hex),
      tiles: tiles.filter(t => t.path).map(t => ({ path: t.path, x: t.x, y: t.y })),
      points: drawn,
      sites: placeSites(sites.map(s => ({ rank: s.rank, xy: px(s) })).filter(s => inside(s.xy))),
      legend,
      title,
      attribution: ATTRIBUTION,
      out: join(dir, `snap-${new Date(now()).toISOString().replace(/[:.]/g, '-')}-${createHash('sha1').update(JSON.stringify([bbox, points.length, title, sites])).digest('hex').slice(0, 8)}.jpg`),
    };
    await new Promise((resolve, reject) => {
      const child = execFile(python, ['-c', SCRIPT], { timeout: timeoutMs, maxBuffer: 1 << 20 }, (error, stdout, stderr) =>
        error ? reject(new Error(`The map picture failed: ${String(stderr || error.message).trim().split('\n').at(-1)}`)) : resolve(stdout),
      );
      child.stdin.end(JSON.stringify(job));
    });
    return {
      path: job.out,
      bytes: statSync(job.out).size,
      width,
      height,
      // The maps app counts zoom on 512-pixel tiles: one less than these 256-pixel ones.
      view: { lng: view.lng, lat: view.lat, zoom: z - 1 },
      bbox: view.bbox.map(v => Number(v.toFixed(4))),
      points_drawn: drawn.length,
      tiles: { asked: tiles.length, fetched: tiles.filter(t => t.fetched).length, failed: tiles.filter(t => t.failed).length },
      ms: { tiles: tilesMs, total: now() - started },
    };
  }

  return { render };
}
