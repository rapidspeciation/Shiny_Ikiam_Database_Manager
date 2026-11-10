// maps_snapshot's picture from the Ithomiini Maps app itself (DESIGN: reports/assistant_maps_photos_20261102/map-pilot/DESIGN.md).
// The app's supported export interface (`?export=1` and window.ithomiiniMapsExport(), contract in
// tests/fixtures/maps-export-contract.json) is driven by a thin browser worker (server/maps-render-worker.mjs) in its own
// process group; what it returns is checked against the tool's own records before it is sent. Anything else, or any
// failure, is the Pillow picture (server/map-snapshot.mjs) with `degraded.reason`.
//
// Processes: the tools run in a few worker threads of the server process (server/assistant-host.mjs, POOL_SIZE up to 3)
// plus the app's thread; each builds its own maps tools, so each has its own queue here. A thread counts the render
// workers that are children of the server process (/proc) before it starts one and again just after, the older one
// keeping its turn, so at most one browser runs per server process. Other server processes on the host are covered only
// by the MemAvailable gate.
// Cleanup (processTree): the run's processes are known by ancestry and start time, never by command line, and killed
// by whichever side is still there: the thread (deadline, memory cap, end of run) or the worker itself (its stdin, the
// lease, closed by a terminated thread or a dead server; its own deadline). See runWorker and the worker's header.
// No new dependency: PLAYWRIGHT_MODULE (scripts/browser-smoke.mjs's convention) and MAPS_RENDER_CHROMIUM (the browser
// binary; on the server the headless shell, docs/operations.md), checked once per thread on first use;
// MAPS_RENDER=pillow turns the app picture off.

import { spawn } from 'node:child_process';
import { createHash } from 'node:crypto';
import { accessSync, constants as fsConstants, existsSync, mkdtempSync, readFileSync, readdirSync, rmSync, statSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { StringDecoder } from 'node:string_decoder';
import { fileURLToPath } from 'node:url';
import { inflateSync } from 'node:zlib';

/** The only page the worker opens. */
export const OFFICIAL_MAPS_URL = 'https://rapidspeciation.github.io/ithomiini_maps/';
/** Fixed by the app's export mode: no size, DPI or format knobs. `padding`/`max_zoom`: the app's fitBounds of `bbox`. */
export const EXPORT = Object.freeze({ version: 1, png: { width: 1600, height: 1200 }, frame: { width: 760, height: 570 }, viewport: { width: 1280, height: 800 }, padding: 32, maxZoom: 12, tileSize: 512 });
/** Basemaps (the app's MAP_STYLES keys) and colourings asked of the app; the Stadia ones are left out. */
export const STYLES = ['light', 'terrain', 'streets', 'satellite', 'dark'];
export const COLOR_BY = ['subspecies', 'species', 'genus'];
/** The URL keys the render URL may carry (closed set; values checked in renderUrlProblem). */
export const URL_KEYS = new Set(['genus', 'sp', 'ssp', 'source', 'country', 'sex', 'sites', 'site_labels', 'site_min', 'style', 'color_by', 'bbox', 'export']);
/** Asked of the tool but not drawn on a Pillow picture. */
export const PILLOW_NOT_KEPT = ['basemap', 'color_by', 'site_labels', 'legend_style'];

const DEADLINE_MS = 50_000;
const QUEUE_MS = 10_000;
const MIN_AVAILABLE = 3 * 1024 ** 3;
const RSS_CAP = 2 * 1024 ** 3;
const POLL_MS = 500;
const STDOUT_MAX = 24 * 1024 * 1024;
const CHECK_MS = 15_000;
/** After the run is killed, how long to wait for the worker's pipes to close before going on without them. */
const CLOSE_MS = 5000;
const KEEP_MS = 7 * 86400_000;
const TOL_DEG = 1e-5;
const TOL_ZOOM = 0.02;
// The app's accent colours (src/index.css, each theme): the export frame's dashed border is drawn in one.
const ACCENTS = [
  [0x12, 0x86, 0x3d],
  [0x4a, 0xde, 0x80],
  [0x07, 0x7f, 0x9c],
  [0x00, 0xd4, 0xff],
];
const WORKER = fileURLToPath(new URL('./maps-render-worker.mjs', import.meta.url));

/** Why this request cannot be drawn by the app exactly (the app has no such filter), or null. */
export function unsupportedFilter(args = {}) {
  if (args.area && (typeof args.area === 'object' || String(args.area).trim())) return 'area';
  if (Number.isFinite(args.elevation_min) || Number.isFinite(args.elevation_max)) return 'elevation';
  if (args.years && (Number.isFinite(args.years.from) || Number.isFinite(args.years.to))) return 'years';
  if (args.near) return 'near';
  if (args.site_km !== undefined && args.site_km !== null && Number(args.site_km) !== 3) return 'site_km';
  return null;
}

// Web Mercator on the app's 512-pixel tiles, as MapLibre's fitBounds/cameraForBounds.
const mx = lng => (lng + 180) / 360;
const my = lat => {
  const s = Math.sin((Math.max(-85.051129, Math.min(85.051129, lat)) * Math.PI) / 180);
  return 0.5 - Math.log((1 + s) / (1 - s)) / (4 * Math.PI);
};
const xLng = x => x * 360 - 180;
const yLat = y => (Math.atan(Math.sinh(Math.PI * (1 - 2 * y))) * 180) / Math.PI;

/** The bounds [w, s, e, n] a `frame` {width, height} (CSS px) shows at `center` [lng, lat] and `zoom`. */
export function visibleBounds(center, zoom, frame, tileSize = EXPORT.tileSize) {
  const world = tileSize * 2 ** zoom;
  const [cx, cy] = [mx(center[0]), my(center[1])];
  const dx = frame.width / 2 / world;
  const dy = frame.height / 2 / world;
  return [xLng(cx - dx), yLat(cy + dy), xLng(cx + dx), yLat(cy - dy)];
}

/** MapLibre's fitBounds(bbox, {padding, maxZoom}) in a frame: { center, zoom, capped, bbox (what it shows) }. */
export function fitBoundsView(bbox, frame, { padding = EXPORT.padding, maxZoom = EXPORT.maxZoom, tileSize = EXPORT.tileSize } = {}) {
  const [w, s, e, n] = bbox;
  const spanX = Math.max(1e-12, mx(e) - mx(w));
  const spanY = Math.max(1e-12, my(s) - my(n));
  const free = { width: Math.max(1, frame.width - 2 * padding), height: Math.max(1, frame.height - 2 * padding) };
  const fit = Math.log2(Math.min(free.width / (spanX * tileSize), free.height / (spanY * tileSize)));
  const zoom = Math.min(maxZoom, fit);
  const center = [xLng((mx(w) + mx(e)) / 2), yLat((my(n) + my(s)) / 2)];
  return { center, zoom, capped: fit > maxZoom, bbox: visibleBounds(center, zoom, frame, tileSize) };
}

/** A point [lng, lat] in CSS px of the map canvas for a `view` { center, zoom, frame } (bearing 0), as map.project. */
export function projectPoint([lng, lat], { center, zoom, frame }, tileSize = EXPORT.tileSize) {
  const world = tileSize * 2 ** zoom;
  return { x: (mx(lng) - mx(center[0])) * world + frame.width / 2, y: (my(lat) - my(center[1])) * world + frame.height / 2 };
}

// Picture geometry (the app's view.overlays, records[].x/y/in_picture, sites[].x/y/radius/label_boxes/occluded).
const OVERLAY_KINDS = new Set(['title', 'legend', 'scale', 'attribution']);
const TOL_PX = 1;
const finite = (...v) => v.every(Number.isFinite);
const isBox = b => !!b && finite(b.x, b.y, b.width, b.height) && b.width > 0 && b.height > 0;
const boxesOverlap = (a, b) => a.x < b.x + b.width && b.x < a.x + a.width && a.y < b.y + b.height && b.y < a.y + a.height;
const discTouches = ({ x, y, radius }, r) => {
  const dx = x - Math.max(r.x, Math.min(x, r.x + r.width));
  const dy = y - Math.max(r.y, Math.min(y, r.y + r.height));
  return dx * dx + dy * dy <= radius * radius;
};
const pointIn = (p, r) => p.x >= r.x && p.x <= r.x + r.width && p.y >= r.y && p.y <= r.y + r.height;
const boxInFrame = (b, f) => b.x >= 0 && b.y >= 0 && b.x + b.width <= f.width && b.y + b.height <= f.height;

/**
 * The app's picture geometry checked against this side's own projection of the view, so a malformed or optimistic
 * answer is not trusted: overlays well formed (a title strip when there is a title, the legend when it has rows);
 * every record's x/y is its lng/lat projected (1 px) and in_picture is exactly "in the frame, outside every overlay,
 * in a drawn marker that is not occluded"; every marker's x/y is its lng/lat projected, `occluded` is exactly "its disc
 * or a label box touches an overlay", and its labels are exactly its boxes inside the frame and clear of overlays.
 * → null, or { reason, detail }.
 */
function geometryProblem(result, view, records, markers) {
  const overlays = view.overlays;
  if (!Array.isArray(overlays)) return { reason: 'geometry_missing', detail: { field: 'view.overlays' } };
  const bad = overlays.find(o => !OVERLAY_KINDS.has(o?.kind) || !isBox(o));
  if (bad) return { reason: 'geometry_invalid', detail: { overlay: bad ?? null } };
  const kinds = new Set(overlays.map(o => o.kind));
  if (typeof result.title === 'string' && result.title.trim() && !kinds.has('title')) return { reason: 'geometry_invalid', detail: { missing_overlay: 'title' } };
  if (result.legend.rows.length && !kinds.has('legend')) return { reason: 'geometry_invalid', detail: { missing_overlay: 'legend' } };
  const frame = view.frame;
  const site = new Map();
  for (const m of markers) {
    if (!finite(m?.x, m?.y, m?.radius, m?.lng, m?.lat) || !(m.radius > 0) || typeof m.occluded !== 'boolean' || !Array.isArray(m.label_boxes) || !Array.isArray(m.labels))
      return { reason: 'geometry_missing', detail: { field: 'sites[].x/y/radius/occluded/label_boxes', site: m?.label ?? null } };
    if (!m.label_boxes.every(isBox)) return { reason: 'geometry_invalid', detail: { site: m.label ?? null, label_boxes: m.label_boxes.slice(0, 3) } };
    const at = projectPoint([m.lng, m.lat], view);
    if (Math.abs(at.x - m.x) > TOL_PX || Math.abs(at.y - m.y) > TOL_PX) return { reason: 'geometry_inconsistent', detail: { site: m.label ?? null, expected: [at.x, at.y], got: [m.x, m.y] } };
    const occluded = overlays.some(o => discTouches(m, o)) || m.label_boxes.some(b => overlays.some(o => boxesOverlap(b, o)));
    const clear = m.label_boxes.filter(b => boxInFrame(b, frame) && !overlays.some(o => boxesOverlap(b, o))).length;
    if (occluded !== m.occluded || clear !== m.labels.length || (m.label != null && !clear))
      return { reason: 'geometry_inconsistent', detail: { site: m.label ?? null, occluded: m.occluded, expected_occluded: occluded, labels: m.labels.length, clear_label_boxes: clear } };
    for (const mem of m.members ?? []) for (const id of mem.record_ids ?? []) site.set(String(id), m);
  }
  for (const r of records) {
    if (typeof r.in_picture !== 'boolean') return { reason: 'geometry_missing', detail: { field: 'records[].in_picture', record: keyOf(r) } };
    const lng = Number(r.lng);
    const lat = Number(r.lat);
    if (!finite(lng, lat)) {
      if (r.in_picture) return { reason: 'geometry_inconsistent', detail: { record: keyOf(r), in_picture: true, coordinates: null } };
      continue;
    }
    if (!finite(r.x, r.y)) return { reason: 'geometry_missing', detail: { field: 'records[].x/y', record: keyOf(r) } };
    const at = projectPoint([lng, lat], view);
    if (Math.abs(at.x - r.x) > TOL_PX || Math.abs(at.y - r.y) > TOL_PX) return { reason: 'geometry_inconsistent', detail: { record: keyOf(r), expected: [at.x, at.y], got: [r.x, r.y] } };
    const m = site.get(keyOf(r));
    const visible = at.x >= 0 && at.x <= frame.width && at.y >= 0 && at.y <= frame.height && !overlays.some(o => pointIn(at, o)) && !!m && !m.occluded;
    if (visible !== r.in_picture) return { reason: 'geometry_inconsistent', detail: { record: keyOf(r), in_picture: r.in_picture, expected: visible } };
  }
  return null;
}

/** How far each side of a frame may grow when moving sites out from under the overlays (shares of its span). */
const GROW_STEPS = [0, 0.08, 0.16, 0.25, 0.35, 0.5, 0.7, 1, 1.4];
const CLEAR_PX = 4;

/**
 * A wider frame that keeps the given sites clear of the overlays, or null. `bbox` the frame asked; `frame` the app's
 * frame (CSS px); `overlays` the app's boxes; `footprints` [{ lng, lat, boxes: [{ dx, dy, width, height }] }] (each
 * marker's disc and label boxes, relative to its centre: their size in px does not change with the zoom).
 * The legend is counted in both bottom corners: the app keeps it bottom-left (its default) unless that covers a marker,
 * then tries bottom-right, and its size depends on the filtered set, not the view. Each side grows by GROW_STEPS
 * independently; of the frames that clear every footprint (CLEAR_PX apart) with its centre in the picture, the one
 * with the highest zoom, then the least growth. The bbox asked stays inside, so no record of it leaves the picture.
 */
export function clearingFrame({ bbox, frame, overlays, footprints }) {
  if (!footprints.length) return null;
  const pad = b => ({ x: b.x - CLEAR_PX, y: b.y - CLEAR_PX, width: b.width + 2 * CLEAR_PX, height: b.height + 2 * CLEAR_PX });
  const blocks = [];
  for (const o of overlays) {
    blocks.push(pad(o));
    if (o.kind === 'legend') blocks.push(pad({ ...o, x: frame.width - o.x - o.width }));
  }
  const [w, s, e, n] = bbox;
  const [sx, sy] = [e - w, n - s];
  let best = null;
  for (const gw of GROW_STEPS)
    for (const gs of GROW_STEPS)
      for (const ge of GROW_STEPS)
        for (const gn of GROW_STEPS) {
          const grown = gw + gs + ge + gn;
          const b = [Math.max(-180, w - gw * sx), Math.max(-85, s - gs * sy), Math.min(180, e + ge * sx), Math.min(85, n + gn * sy)];
          const v = fitBoundsView(b, frame);
          if (best && (v.zoom < best.zoom - 1e-9 || (Math.abs(v.zoom - best.zoom) <= 1e-9 && grown >= best.grown))) continue;
          const view = { center: v.center, zoom: v.zoom, frame };
          const clear = footprints.every(f => {
            const at = projectPoint([f.lng, f.lat], view);
            if (at.x < 0 || at.y < 0 || at.x > frame.width || at.y > frame.height) return false;
            return f.boxes.every(box => {
              const placed = { x: at.x + box.dx, y: at.y + box.dy, width: box.width, height: box.height };
              return !blocks.some(o => boxesOverlap(placed, o));
            });
          });
          if (clear) best = { bbox: b.map(x => Math.round(x * 1e5) / 1e5), zoom: v.zoom, grown };
        }
  return best && best.grown > 0 ? best.bbox : null;
}

/** A marker's disc and label boxes relative to its centre (clearingFrame's footprint). */
const footprintOf = m => ({
  lng: m.lng,
  lat: m.lat,
  boxes: [{ dx: -m.radius, dy: -m.radius, width: 2 * m.radius, height: 2 * m.radius }, ...m.label_boxes.map(b => ({ dx: b.x - m.x, dy: b.y - m.y, width: b.width, height: b.height }))],
});

const inBox = (b, lng, lat) => lng >= b[0] && lng <= b[2] && lat >= b[1] && lat <= b[3];
const r5 = v => Math.round(Number(v) * 1e5) / 1e5;
const normSex = v => {
  const s = String(v ?? '').trim().toLowerCase();
  if (/^(m|male|males|macho|♂)$/.test(s)) return 'male';
  if (/^(f|female|females|hembra|♀)$/.test(s)) return 'female';
  return null;
};
const normYear = v => (v === null || v === undefined || v === '' || !Number.isFinite(Number(v)) ? null : Number(v));
const keyOf = r => `${String(r.source ?? '').trim()}|${String(r.id ?? '').trim()}`;

/** Without a size expected, the largest picture decodePng reads (four export pictures' worth of pixels). */
const PNG_MAX_PIXELS = 4 * 1600 * 1200;

/**
 * A PNG's size and pixels (8-bit RGB or RGBA, not interlaced: what canvas.toDataURL writes), or throws. `expect`
 * { width, height }: any other size in IHDR throws (code PNG_SIZE) before anything is inflated or allocated; the data
 * is inflated to at most its rows' length and must be exactly that.
 */
export function decodePng(buf, expect = null) {
  if (!Buffer.isBuffer(buf) || buf.length < 33 || buf.readUInt32BE(0) !== 0x89504e47 || buf.readUInt32BE(4) !== 0x0d0a1a0a) throw new Error('not a PNG');
  let at = 8;
  let ihdr = null;
  const idat = [];
  while (at + 8 <= buf.length) {
    const len = buf.readUInt32BE(at);
    const type = buf.toString('latin1', at + 4, at + 8);
    if (len > 0x7fff_ffff || at + 12 + len > buf.length) throw new Error(`PNG chunk past the end (${type})`);
    const body = buf.subarray(at + 8, at + 8 + len);
    if (!ihdr) {
      if (type !== 'IHDR' || len !== 13) throw new Error('PNG IHDR missing, not first or not 13 bytes');
      ihdr = { width: body.readUInt32BE(0), height: body.readUInt32BE(4), depth: body[8], color: body[9], interlace: body[12] };
      const { width, height } = ihdr;
      if (expect ? width !== expect.width || height !== expect.height : !width || !height || width * height > PNG_MAX_PIXELS)
        throw Object.assign(new Error(`PNG size ${width}x${height}${expect ? `, not ${expect.width}x${expect.height}` : ''}`), { code: 'PNG_SIZE', width, height });
      if (body[10] !== 0 || body[11] !== 0) throw new Error('PNG IHDR methods unknown');
    } else if (type === 'IDAT') idat.push(body);
    else if (type === 'IEND') break;
    at += 12 + len;
  }
  if (!ihdr || !idat.length) throw new Error('PNG without IHDR or IDAT');
  const channels = { 2: 3, 6: 4 }[ihdr.color];
  if (ihdr.depth !== 8 || !channels || ihdr.interlace) throw new Error(`PNG kind not read (depth ${ihdr.depth}, colour ${ihdr.color})`);
  const { width, height } = ihdr;
  const stride = width * channels;
  const rows = (stride + 1) * height;
  let raw;
  try {
    raw = inflateSync(Buffer.concat(idat), { maxOutputLength: rows });
  } catch (e) {
    if (e?.code === 'ERR_BUFFER_TOO_LARGE') throw new Error('PNG data longer than its rows');
    throw e;
  }
  if (raw.length !== rows) throw new Error(raw.length < rows ? 'PNG data short' : 'PNG data longer than its rows');
  const data = Buffer.alloc(stride * height);
  for (let y = 0; y < height; y++) {
    const filter = raw[y * (stride + 1)];
    const line = raw.subarray(y * (stride + 1) + 1, (y + 1) * (stride + 1));
    const out = y * stride;
    for (let i = 0; i < stride; i++) {
      const a = i >= channels ? data[out + i - channels] : 0;
      const b = y ? data[out - stride + i] : 0;
      const c = y && i >= channels ? data[out - stride + i - channels] : 0;
      let v = line[i];
      if (filter === 1) v += a;
      else if (filter === 2) v += b;
      else if (filter === 3) v += (a + b) >> 1;
      else if (filter === 4) {
        const p = a + b - c;
        const [pa, pb, pc] = [Math.abs(p - a), Math.abs(p - b), Math.abs(p - c)];
        v += pa <= pb && pa <= pc ? a : pb <= pc ? b : c;
      } else if (filter !== 0) throw new Error(`PNG filter ${filter}`);
      data[out + i] = v & 255;
    }
  }
  return { width, height, channels, data };
}

const isAccent = (r, g, b) => ACCENTS.some(([ar, ag, ab]) => Math.abs(r - ar) <= 40 && Math.abs(g - ag) <= 40 && Math.abs(b - ab) <= 40);

/**
 * Blank and border checks of a decoded picture: { sd (luminance), stripe: null | { edge, index, share } }. A stripe
 * is an outer row or column (the outer 4 px) with more than 25 % accent pixels; isolated accent pixels (a green dot
 * of the map near the edge) are allowed.
 */
export function pixelChecks(img, band = 4, share = 0.25) {
  const { width, height, channels, data } = img;
  const px = (x, y) => {
    const i = (y * width + x) * channels;
    return [data[i], data[i + 1], data[i + 2]];
  };
  let sum = 0;
  let sq = 0;
  let n = 0;
  for (let y = 0; y < height; y += 4)
    for (let x = 0; x < width; x += 4) {
      const [r, g, b] = px(x, y);
      const l = 0.2126 * r + 0.7152 * g + 0.0722 * b;
      sum += l;
      sq += l * l;
      n++;
    }
  const sd = Math.sqrt(Math.max(0, sq / n - (sum / n) ** 2));
  let stripe = null;
  const lines = [];
  for (let k = 0; k < band; k++) lines.push(['row', k], ['row', height - 1 - k], ['col', k], ['col', width - 1 - k]);
  for (const [edge, index] of lines) {
    const len = edge === 'row' ? width : height;
    let accent = 0;
    for (let t = 0; t < len; t++) if (isAccent(...(edge === 'row' ? px(t, index) : px(index, t)))) accent++;
    if (accent / len > share && (!stripe || accent / len > stripe.share)) stripe = { edge, index, share: Number((accent / len).toFixed(3)) };
  }
  return { sd: Number(sd.toFixed(2)), stripe };
}

/** Why the render URL is not one the worker may open (origin, keys, values), or null. */
export function renderUrlProblem(url) {
  let u;
  try {
    u = new URL(url);
  } catch {
    return 'not a URL';
  }
  if (`${u.origin}${u.pathname}` !== OFFICIAL_MAPS_URL || u.hash || u.username || u.password) return 'not the official maps app';
  for (const [k, v] of u.searchParams) {
    if (!URL_KEYS.has(k)) return `parameter ${k}`;
    if (v.length > 4000 || /[<>`]/.test(v)) return `value of ${k}`;
  }
  const p = u.searchParams;
  if (p.get('export') !== '1') return 'export=1 missing';
  if (p.has('style') && !STYLES.includes(p.get('style'))) return 'style';
  if (p.has('color_by') && !COLOR_BY.includes(p.get('color_by'))) return 'color_by';
  if (p.has('sex') && !['male', 'female'].includes(p.get('sex'))) return 'sex';
  if (p.has('bbox')) {
    const b = p.get('bbox').split(',').map(Number);
    if (b.length !== 4 || !b.every(Number.isFinite) || b[0] >= b[2] || b[1] >= b[3] || b[1] < -85 || b[3] > 85 || b[0] < -180 || b[2] > 180) return 'bbox';
  }
  return null;
}

/**
 * What the app returned, checked against what the tool drew (DESIGN 3.4). `expect`: { records (the tool's set, app
 * fields: source, id, sex, country, year, lng, lat), bbox (the frame asked, sent as bbox=), explicit (the user's bbox:
 * the tool's set is the app's records inside it), style, colorBy, sites [{ rank, site, lat, lng, ids }], required (ranks
 * the answer names; default all) }.
 * Visible means in the picture: inside the frame and not under the title strip, legend, scale or credits (the app's
 * picture geometry, checked by geometryProblem). A required site that is hidden fails (`ranked_site_hidden`, with
 * `reframe`: a wider frame that clears the required sites, from clearingFrame, or null); another hidden site is left
 * out of picture_labels and listed in hidden_sites.
 * → { ok: true, png, view, visible: { records, asked_records, sites }, picture_labels, hidden_sites, legend_unknown,
 *   app_records, title, title_truncated } or { ok: false, reason, detail }.
 */
export function checkExport(result, expect) {
  const fail = (reason, detail = null) => ({ ok: false, reason, detail });
  if (!result || result.version !== EXPORT.version) return fail('contract_version', { version: result?.version ?? null });
  let png;
  let img;
  try {
    png = Buffer.from(String(result.png_base64 ?? ''), 'base64');
    img = decodePng(png, EXPORT.png);
  } catch (e) {
    return e?.code === 'PNG_SIZE' ? fail('png_size', { width: e.width, height: e.height }) : fail('png_invalid', { message: e.message });
  }
  if (result.width !== img.width || result.height !== img.height) return fail('png_size', { width: img.width, height: img.height });
  const ignored = result.params?.ignored;
  if (!Array.isArray(ignored) || ignored.length) return fail('params_ignored', { ignored: Array.isArray(ignored) ? ignored.slice(0, 10) : null });
  if (result.basemap !== expect.style || result.color_by !== expect.colorBy) return fail('settings_differ', { basemap: result.basemap ?? null, color_by: result.color_by ?? null });

  // The view: fitBounds of the bbox asked in the frame measured, with the fixed padding and zoom cap.
  const v = result.view ?? {};
  const frame = v.frame;
  if (!frame || Math.abs(frame.width - EXPORT.frame.width) > 8 || Math.abs(frame.height - EXPORT.frame.height) > 8) return fail('frame_size', { frame: frame ?? null });
  if (v.fit?.padding !== EXPORT.padding || v.fit?.max_zoom !== EXPORT.maxZoom) return fail('view_fit_options', { fit: v.fit ?? null });
  if (!Array.isArray(v.center) || !Array.isArray(v.bbox) || v.bbox.length !== 4 || ![...v.center, v.zoom, ...v.bbox].every(Number.isFinite)) return fail('view_missing');
  const want = fitBoundsView(expect.bbox, frame);
  const off = { lng: Math.abs(v.center[0] - want.center[0]), lat: Math.abs(v.center[1] - want.center[1]), zoom: Math.abs(v.zoom - want.zoom) };
  if (off.lng > TOL_DEG || off.lat > TOL_DEG || off.zoom > TOL_ZOOM)
    return fail('view_mismatch', { expected: { center: want.center, zoom: Number(want.zoom.toFixed(4)) }, got: { center: v.center, zoom: v.zoom } });
  // The bounds returned are the ones that view shows, and hold the bbox asked.
  const shows = visibleBounds(v.center, v.zoom, frame);
  if (shows.some((x, i) => Math.abs(x - v.bbox[i]) > 1e-4)) return fail('view_bbox_inconsistent', { bbox: v.bbox, expected: shows });
  const [w, s, e, n] = expect.bbox;
  if (!(v.bbox[0] <= w + TOL_DEG && v.bbox[1] <= s + TOL_DEG && v.bbox[2] >= e - TOL_DEG && v.bbox[3] >= n - TOL_DEG)) return fail('view_not_containing', { bbox: v.bbox });

  // Parity: identities and map-native fields, the tool's set against the app's (inside the bbox asked, when one was).
  const appAll = Array.isArray(result.records) ? result.records : [];
  const appCompared = expect.explicit ? appAll.filter(r => inBox(expect.bbox, Number(r.lng), Number(r.lat))) : appAll;
  const app = new Map(appCompared.map(r => [keyOf(r), r]));
  const tool = new Map(expect.records.map(r => [keyOf(r), r]));
  const onlyApp = [...app.keys()].filter(k => !tool.has(k));
  const onlyTool = [...tool.keys()].filter(k => !app.has(k));
  const fields = { sex: 0, country: 0, year: 0, coords: 0 };
  const examples = [];
  for (const [k, t] of tool) {
    const a = app.get(k);
    if (!a) continue;
    const bad = [];
    if (normSex(a.sex) !== normSex(t.sex)) bad.push('sex');
    if (String(a.country ?? '').trim() !== String(t.country ?? '').trim()) bad.push('country');
    if (normYear(a.year) !== normYear(t.year)) bad.push('year');
    if (r5(a.lng) !== r5(t.lng) || r5(a.lat) !== r5(t.lat)) bad.push('coords');
    for (const f of bad) fields[f]++;
    if (bad.length && examples.length < 3) examples.push({ record: k, fields: bad });
  }
  if (onlyApp.length || onlyTool.length || examples.length || app.size !== appCompared.length)
    return fail('parity_mismatch', { only_app: onlyApp.length, only_tool: onlyTool.length, fields, duplicates: appCompared.length - app.size, examples: [...onlyApp.slice(0, 3), ...onlyTool.slice(0, 3)].map(record => ({ record })).concat(examples) });

  // Legend: rows (with the unknown subspecies) add up to the app's whole filtered set, counted in records.
  const rows = result.legend?.rows;
  if (result.legend?.unit !== 'records' || !Array.isArray(rows)) return fail('legend_unit');
  const legendSum = rows.reduce((t, r) => t + (Number(r.records) || 0), 0);
  if (legendSum !== appAll.length) return fail('legend_sum', { legend: legendSum, records: appAll.length });

  // What the picture shows: the returned view, minus what the overlays cover (checked against our own projection).
  const markers = Array.isArray(result.sites) ? result.sites : [];
  const geometry = geometryProblem(result, v, appAll, markers);
  if (geometry) return fail(geometry.reason, geometry.detail);
  const visibleRecords = appAll.filter(r => r.in_picture).length;
  const askedVisible = appCompared.filter(r => r.in_picture).length;
  const memberOf = new Map();
  for (const m of markers) for (const mem of m.members ?? []) for (const id of mem.record_ids ?? []) memberOf.set(String(id), m);
  const coords = new Map(appAll.map(r => [keyOf(r), r]));
  const sitesIn = expect.sites.filter(s => inBox(v.bbox, s.lng, s.lat));
  const required = new Set(expect.required ?? expect.sites.map(s => s.rank));
  const labels = [];
  const hidden = [];
  const missing = [];
  const requiredMarkers = new Set();
  for (const s of sitesIn) {
    const ids = s.ids.filter(id => {
      const r = coords.get(id);
      return r && inBox(v.bbox, Number(r.lng), Number(r.lat));
    });
    const hits = new Map();
    for (const id of ids) {
      const m = memberOf.get(id);
      if (m) hits.set(m, (hits.get(m) ?? 0) + 1);
    }
    const covered = [...hits.values()].reduce((t, x) => t + x, 0);
    if (!ids.length || covered !== ids.length) {
      missing.push(s.rank);
      continue;
    }
    if (required.has(s.rank)) for (const m of hits.keys()) requiredMarkers.add(m);
    // Any of its markers under an overlay: not shown as a visible site.
    const under = [...hits.keys()].filter(m => m.occluded);
    if (under.length) {
      hidden.push({ rank: s.rank, site: s.site, occluded_by: [...new Set(under.flatMap(m => (Array.isArray(m.occluded_by) ? m.occluded_by : [])))] });
      continue;
    }
    const best = [...hits].sort((a, b) => b[1] - a[1])[0][0];
    labels.push({ rank: s.rank, site: s.site, label_on_picture: String(best.label ?? ''), merged_sites: Number(best.merged_sites) || 0 });
  }
  if (missing.length) return fail('sites_not_drawn', { ranks: missing.slice(0, 10) });
  const hiddenRequired = hidden.filter(h => required.has(h.rank));
  if (hiddenRequired.length) {
    // The frame to ask next (the tool's own frames only): the named sites, visible ones too, clear of the overlays.
    const reframe = clearingFrame({ bbox: expect.bbox, frame, overlays: v.overlays, footprints: [...requiredMarkers].map(footprintOf) });
    return fail('ranked_site_hidden', { sites: hiddenRequired.slice(0, 10), legend_placement: v.legend_placement ?? null, reframe });
  }

  const { sd, stripe } = pixelChecks(img);
  if (sd <= 8) return fail('blank', { luminance_sd: sd });
  if (stripe) return fail('border_stripe', stripe);
  return {
    ok: true,
    png,
    view: { center: v.center, zoom: v.zoom, bbox: v.bbox },
    visible: { records: visibleRecords, asked_records: askedVisible, sites: labels.length },
    picture_labels: labels,
    hidden_sites: hidden,
    legend_unknown: rows.filter(r => r.unknown).reduce((t, r) => t + (Number(r.records) || 0), 0),
    app_records: appAll.length,
    title: typeof result.title === 'string' ? result.title.slice(0, 200) : null,
    title_truncated: result.title_truncated === true,
  };
}

/**
 * The process table from /proc: Map pid → { ppid, rss (bytes), start (clock ticks), cmd }. `cmd: false`: without the
 * command lines (cmd ''), for the render's memory watch, which needs only ppid and rss. With them, this read took
 * 420–570 ms of each half-second watch on a loaded PC (the watch's other, stat-only read: 90–310 ms); the command
 * lines are about 40 % of it (21–36 ms with them, 13–21 ms without, at a lighter moment).
 */
export function procTable(root = '/proc', { cmd: withCmd = true } = {}) {
  const out = new Map();
  const page = 4096;
  for (const name of readdirSync(root)) {
    if (!/^\d+$/.test(name)) continue;
    try {
      const stat = readFileSync(join(root, name, 'stat'), 'utf8');
      const rest = stat.slice(stat.lastIndexOf(')') + 2).split(' ');
      // After the name: state(0) ppid(1) … starttime(19) vsize(20) rss(21, pages).
      let cmd = '';
      try {
        if (withCmd) cmd = readFileSync(join(root, name, 'cmdline'), 'utf8').replace(/\0/g, ' ');
      } catch {
        // gone
      }
      out.set(Number(name), { ppid: Number(rest[1]), start: Number(rest[19]), rss: Number(rest[21]) * page, cmd });
    } catch {
      // gone between readdir and read
    }
  }
  return out;
}

/** The RSS of `pid` and all its descendants (bytes) in `table`. */
export function treeRss(table, pid) {
  const kids = new Map();
  for (const [p, e] of table) (kids.get(e.ppid) ?? kids.set(e.ppid, []).get(e.ppid)).push(p);
  let sum = 0;
  const stack = [pid];
  const seen = new Set();
  while (stack.length) {
    const p = stack.pop();
    if (seen.has(p)) continue;
    seen.add(p);
    sum += table.get(p)?.rss ?? 0;
    stack.push(...(kids.get(p) ?? []));
  }
  return sum;
}

/** /proc/<pid>/stat → { state, ppid, pgrp, start (clock ticks after boot) }, or null when the process is gone. */
export function procStat(pid, proc = '/proc') {
  try {
    const stat = readFileSync(join(proc, String(pid), 'stat'), 'utf8');
    const f = stat.slice(stat.lastIndexOf(')') + 2).split(' ');
    return { state: f[0], ppid: Number(f[1]), pgrp: Number(f[2]), start: Number(f[19]) };
  } catch {
    return null;
  }
}
const running = s => Boolean(s) && s.state !== 'Z' && s.state !== 'X';

/**
 * The processes started under one render's `root` ({ pid, alive() → is that pid still the root (the parent: Node has
 * not reaped the child; the worker: itself), group → the root leads its own process group }).
 *
 * A process is the render's only when it was seen as a child of the root or of one already known (ancestry read from
 * /proc's ppid), and it stays known by pid and start time, so it is still known after its parent died (Chromium's
 * helpers once the browser is killed, the browser once the worker exited). Nothing is chosen by name or command line.
 * Each signal is sent only after the pid's start time is read again and matches; a process group is killed only when
 * its leader is such a known process (or the root, while alive() holds), never by a bare pid that may have been reused.
 *
 * observe() → how many were added; reap({ withRoot }) → stops all of them (SIGSTOP: root first, then its tree, again
 * until a round finds nothing new, so none forks or exits meanwhile), kills them, waits up to `waitMs` until none runs:
 * { killed, left: [pids still running] }.
 */
export function processTree(root, { proc = '/proc', sig = (pid, signal) => process.kill(pid, signal) } = {}) {
  const known = new Map(); // pid → start
  const send = (pid, signal) => {
    try {
      sig(pid, signal);
    } catch {
      // gone, or not ours to signal
    }
  };
  const still = pid => {
    const s = procStat(pid, proc);
    return running(s) && s.start === known.get(pid) ? s : null;
  };

  function observe(onNew = null) {
    const table = new Map();
    try {
      for (const name of readdirSync(proc)) {
        if (!/^\d+$/.test(name)) continue;
        const s = procStat(name, proc);
        if (running(s)) table.set(Number(name), s);
      }
    } catch {
      return 0; // no /proc: the root's group is all there is
    }
    // Gone, or the pid now someone else's (another start time): forgotten.
    for (const [pid, start] of known) if (table.get(pid)?.start !== start) known.delete(pid);
    const kids = new Map();
    for (const [pid, s] of table) (kids.get(s.ppid) ?? kids.set(s.ppid, []).get(s.ppid)).push(pid);
    const stack = [...known.keys()];
    if (root.alive()) stack.push(root.pid);
    let added = 0;
    while (stack.length) {
      for (const c of kids.get(stack.pop()) ?? []) {
        if (c === root.pid || c === process.pid || known.has(c)) continue;
        known.set(c, table.get(c).start);
        added++;
        onNew?.(c);
        stack.push(c);
      }
    }
    return added;
  }

  async function reap({ withRoot = false, rounds = 8, waitMs = 3000 } = {}) {
    const rootToo = () => withRoot && root.alive();
    if (rootToo()) send(root.pid, 'SIGSTOP');
    observe();
    for (const pid of known.keys()) if (still(pid)) send(pid, 'SIGSTOP');
    for (let i = 0; i < rounds && observe(pid => {
      if (still(pid)) send(pid, 'SIGSTOP');
    }); i++);
    for (const pid of known.keys()) if (still(pid)?.pgrp === pid) send(-pid, 'SIGKILL');
    for (const pid of known.keys()) if (still(pid)) send(pid, 'SIGKILL');
    if (rootToo()) {
      if (root.group) send(-root.pid, 'SIGKILL');
      send(root.pid, 'SIGKILL');
    }
    // Until none of them runs (a zombie waiting for its parent counts as gone), the kill sent again to any still there.
    const until = Date.now() + waitMs;
    for (;;) {
      const left = [...known.keys()].filter(still);
      if (!left.length || Date.now() >= until) return { killed: known.size, left };
      await sleep(25);
      for (const pid of left) if (still(pid)) send(pid, 'SIGKILL');
    }
  }

  return { observe, reap, known: () => new Map(known) };
}

/** Why `file` cannot be the browser to start (missing, not a file, not executable), or null. */
export function executableProblem(file) {
  try {
    if (!statSync(file).isFile()) return 'not a file';
    accessSync(file, fsConstants.X_OK);
    return null;
  } catch (e) {
    return e?.code === 'ENOENT' ? 'missing' : 'not executable';
  }
}

const memAvailable = () => {
  const m = /MemAvailable:\s+(\d+)\s*kB/.exec(readFileSync('/proc/meminfo', 'utf8'));
  return m ? Number(m[1]) * 1024 : null;
};

const sleep = ms => new Promise(r => setTimeout(r, ms));

/**
 * The renderer of one maps tools (one per thread). Options for tests: env, workerPath, workerArgs, memAvailable,
 * procTable (called with { cmd: false } by the memory watch, with nothing when the command lines are needed),
 * deadlineMs, queueMs, minAvailable, rssCap, pollMs, stdoutMax (bytes), dir (where PNGs go), log.
 * status() → { enabled, reason }; render(url) → { ok, result, timing } | { ok: false, reason, detail, timing }.
 */
export function createMapRenderer({
  env = process.env,
  dir = null,
  workerPath = WORKER,
  workerArgs = [],
  memAvailable: memOf = memAvailable,
  procTable: procOf = options => procTable('/proc', options),
  deadlineMs = DEADLINE_MS,
  queueMs = QUEUE_MS,
  minAvailable = MIN_AVAILABLE,
  rssCap = RSS_CAP,
  pollMs = POLL_MS,
  stdoutMax = STDOUT_MAX,
  log = m => console.warn(`maps-render: ${m}`),
  tmpRoot = tmpdir,
  now = () => performance.now(),
} = {}) {
  const config = { module: String(env.PLAYWRIGHT_MODULE ?? '').trim(), chromium: String(env.MAPS_RENDER_CHROMIUM ?? '').trim(), mode: String(env.MAPS_RENDER ?? '').trim().toLowerCase() };
  let checked = null; // Promise<{ enabled, reason, detail }>
  let logged = false;
  let busy = false;
  const waiting = [];
  let cleaned = 0;

  /** Is the browser module there? Checked once (a short `--check` run of the worker: the server never imports it). */
  function availability() {
    if (checked) return checked;
    checked = (async () => {
      if (config.mode === 'pillow') return { enabled: false, reason: 'renderer_off', detail: 'MAPS_RENDER=pillow' };
      if (!config.module) return { enabled: false, reason: 'renderer_unavailable', detail: 'PLAYWRIGHT_MODULE is not set' };
      if (!existsSync(config.module)) return { enabled: false, reason: 'renderer_unavailable', detail: 'PLAYWRIGHT_MODULE file missing' };
      const browserProblem = config.chromium ? executableProblem(config.chromium) : null;
      if (browserProblem) return { enabled: false, reason: 'renderer_unavailable', detail: `MAPS_RENDER_CHROMIUM ${browserProblem}` };
      const run = await runWorker({ check: true }, CHECK_MS).catch(e => ({ ok: false, reason: 'renderer_unavailable', detail: { message: e.message } }));
      return run.ok ? { enabled: true, reason: null, detail: null } : { enabled: false, reason: 'renderer_unavailable', detail: run.detail ?? run.reason };
    })().then(s => {
      if (!s.enabled && !logged) log(`app pictures off for this process (${s.reason}: ${typeof s.detail === 'string' ? s.detail : JSON.stringify(s.detail)}); maps_snapshot draws with Pillow`);
      logged = true;
      return s;
    });
    return checked;
  }

  /** Render workers that are children of this server process (any thread's), but not `except`. */
  function otherRenders(except = null) {
    try {
      const table = procOf();
      const mine = table.get(except);
      return [...table].filter(([pid, e]) => pid !== except && e.ppid === process.pid && e.cmd.includes('maps-render-worker') && !e.cmd.includes('--check')).map(([pid, e]) => ({ pid, start: e.start, earlier: !mine || e.start < mine.start || (e.start === mine.start && pid < except) }));
    } catch {
      return [];
    }
  }

  /**
   * One worker run: spawn in its own process group with a fresh TMPDIR, watch RSS and the deadline, parse its one
   * JSON line. Always, before returning: everything started under the worker killed (processTree), the temp dir removed.
   *
   * The worker's stdin is its lease: the job is one line, and stdin then stays open until this run ends. It closes
   * when this function's `finally` runs, but also when the thread is terminated (Node closes the thread's handles) or
   * the server process dies (the kernel closes its pipes): then the worker kills what it started, removes its TMPDIR
   * and exits (server/maps-render-worker.mjs), with no one here to do it. The worker also stops itself at its own
   * deadline, so a run whose parent froze still ends.
   */
  async function runWorker(job, budgetMs, started = now(), onSpawn = null) {
    // Always under the system's temp folder: a short path (Chromium's singleton socket goes in TMPDIR and a Unix socket
    // path has at most 107 bytes; under the picture folder a run failed to launch).
    const tmp = mkdtempSync(join(tmpRoot(), 'maps-render-'));
    let child = null;
    let tree = null;
    let timer = null;
    let poll = null;
    let grace = null;
    let peak = 0;
    let killedFor = null;
    let reaping = Promise.resolve();
    let reaped = null; // what the last kill left: { killed, left }
    const reap = () => (reaping = reaping.then(() => tree?.reap({ withRoot: true })).then(r => (reaped = r ?? null)).catch(() => {}));
    let giveUp = null;
    /** Kill the run (once); its 'close' then answers, or, if its pipes stay open, CLOSE_MS later the run gives up. */
    const stop = reason => {
      if (killedFor) return;
      killedFor = reason;
      reap();
      grace = setTimeout(() => giveUp?.(), CLOSE_MS);
    };
    try {
      const childEnv = {
        PATH: env.PATH ?? '/usr/bin:/bin',
        HOME: env.HOME ?? tmp,
        TMPDIR: tmp,
        XDG_CONFIG_HOME: join(tmp, 'config'),
        XDG_CACHE_HOME: join(tmp, 'cache'),
        PLAYWRIGHT_MODULE: config.module,
        ...(config.chromium ? { MAPS_RENDER_CHROMIUM: config.chromium } : {}),
        // Where Playwright finds its browsers, fixed here: XDG_CACHE_HOME above would move it into the temp folder.
        PLAYWRIGHT_BROWSERS_PATH: env.PLAYWRIGHT_BROWSERS_PATH || join(env.XDG_CACHE_HOME || join(env.HOME ?? tmp, '.cache'), 'ms-playwright'),
      };
      const out = await new Promise(resolve => {
        // Decoded as UTF-8 across chunks (a character may be cut between two); the cap counts bytes.
        const stdout = new StringDecoder('utf8');
        const stderr = new StringDecoder('utf8');
        let text = '';
        let errText = '';
        let size = 0;
        let settled = false;
        const done = v => {
          if (settled) return;
          settled = true;
          resolve(v);
        };
        giveUp = () => done({ ok: false, reason: killedFor, detail: { peak_rss_mb: Math.round(peak / 2 ** 20), closed: false } });
        try {
          child = spawn(process.execPath, [workerPath, ...workerArgs, ...(job.check ? ['--check'] : [])], { detached: true, stdio: ['pipe', 'pipe', 'pipe'], env: childEnv, cwd: tmp });
        } catch (e) {
          return done({ ok: false, reason: 'worker_spawn', detail: { message: e.message } });
        }
        child.on('error', e => done({ ok: false, reason: 'worker_spawn', detail: { message: e.message } }));
        if (!child.pid) return;
        // Until Node reaps it, the worker's pid (and its group's) cannot belong to anyone else.
        const proc = child;
        tree = processTree({ pid: proc.pid, alive: () => proc.exitCode === null && proc.signalCode === null, group: true });
        child.stdout.on('data', c => {
          size += c.length;
          if (size > stdoutMax) stop('output_too_large');
          else if (!killedFor) text += stdout.write(c);
        });
        child.stderr.on('data', c => {
          if (errText.length < 4000) errText += stderr.write(c);
        });
        child.on('close', code => {
          if (killedFor) return done({ ok: false, reason: killedFor, detail: { peak_rss_mb: Math.round(peak / 2 ** 20) } });
          text += stdout.end();
          errText += stderr.end();
          let parsed = null;
          try {
            parsed = JSON.parse(text.trim().split('\n').at(-1) || 'null');
          } catch {
            // not JSON
          }
          if (parsed?.ok) return done({ ok: true, result: parsed.result ?? null });
          done({ ok: false, reason: parsed?.reason ?? 'worker_failed', detail: { code, stage: parsed?.stage ?? null, message: String(parsed?.message ?? errText.trim().split('\n').at(-1) ?? '').slice(0, 300) } });
        });
        child.stdin.on('error', () => {});
        child.stdin.write(`${JSON.stringify({ url: job.url ?? null, viewport: EXPORT.viewport, deadline_ms: Math.max(1000, budgetMs - 1000) })}\n`);
        onSpawn?.(child, stop);
        const left = budgetMs - (now() - started);
        timer = setTimeout(() => stop('deadline'), Math.max(0, left));
        poll = setInterval(() => {
          tree.observe();
          try {
            const rss = treeRss(procOf({ cmd: false }), child.pid);
            peak = Math.max(peak, rss);
            if (rss > rssCap) stop('memory_cap');
          } catch {
            // no /proc: the deadline still holds
          }
        }, pollMs);
      });
      return { ...out, peak_rss_mb: Math.round(peak / 2 ** 20) };
    } finally {
      clearTimeout(timer);
      clearInterval(poll);
      clearTimeout(grace);
      // Whatever is left: the worker if it still runs, and anything seen under it that is still the same process. Not
      // again when a kill (stop) left none of them running and the worker has exited: nothing of the tree is left that
      // could have started another, so a new look (two full reads of /proc, 240–530 ms on a loaded machine) would
      // find nothing.
      await reaping;
      const gone = reaped?.left.length === 0 && child && (child.exitCode !== null || child.signalCode !== null);
      if (!gone) await reap();
      child?.stdin?.destroy();
      rmSync(tmp, { recursive: true, force: true });
    }
  }

  async function slot(started) {
    while (busy) {
      const left = queueMs - (now() - started);
      if (left <= 0) return false;
      let wake;
      let timer;
      const ok = await new Promise(r => {
        wake = () => (clearTimeout(timer), r(true));
        waiting.push(wake);
        timer = setTimeout(() => r(false), left);
      });
      if (!ok) {
        // Timed out: off the list, so a later release wakes someone still waiting.
        const i = waiting.indexOf(wake);
        if (i >= 0) waiting.splice(i, 1);
        return false;
      }
    }
    busy = true;
    return true;
  }
  const release = () => {
    busy = false;
    waiting.shift()?.();
  };

  /**
   * Old app pictures (app-*.png) older than a week removed, and temp folders of renders older than 10 minutes (left by
   * a tools thread terminated mid-render, whose `finally` never ran), at most once an hour.
   */
  function clean() {
    if (Date.now() - cleaned < 3600_000) return;
    cleaned = Date.now();
    const sweep = (folder, test) => {
      try {
        for (const name of readdirSync(folder)) {
          const file = join(folder, name);
          try {
            if (test(name, Date.now() - statSync(file).mtimeMs)) rmSync(file, { recursive: true, force: true });
          } catch {
            // gone
          }
        }
      } catch {
        // no folder yet
      }
    };
    if (dir) sweep(dir, (name, age) => /^app-[\w-]+\.png$/.test(name) && age > KEEP_MS);
    sweep(tmpRoot(), (name, age) => /^maps-render-\w{6}$/.test(name) && age > 600_000);
  }

  async function render(url) {
    const t0 = now();
    const timing = () => ({ total: Math.round(now() - t0) });
    const state = await availability();
    if (!state.enabled) return { ok: false, reason: state.reason, detail: state.detail, timing: timing() };
    const problem = renderUrlProblem(url);
    if (problem) return { ok: false, reason: 'url_rejected', detail: problem, timing: timing() };
    if (!(await slot(t0))) return { ok: false, reason: 'queue_wait', detail: 'this thread was rendering', timing: timing() };
    try {
      clean();
      // The server process's other threads: one browser at a time for the process, waiting within the queue time.
      for (;;) {
        while (otherRenders().length) {
          if (now() - t0 >= queueMs) return { ok: false, reason: 'queue_wait', detail: 'another tools thread was rendering', timing: timing() };
          await sleep(Math.min(250, pollMs));
        }
        const waited = Math.round(now() - t0);
        let mem = null;
        try {
          mem = memOf();
        } catch {
          // unknown
        }
        if (mem === null || mem < minAvailable) return { ok: false, reason: 'low_memory', detail: { mem_available_mb: mem === null ? null : Math.round(mem / 2 ** 20) }, timing: timing() };
        const started = now();
        let lost = false;
        const run = await runWorker({ url }, deadlineMs, started, (child, stop) => {
          // Started at the same moment as another thread's: the older one keeps its turn.
          if (otherRenders(child.pid).some(o => o.earlier)) {
            lost = true;
            stop('lost_turn');
          }
        });
        if (lost) {
          if (now() - t0 >= queueMs) return { ok: false, reason: 'queue_wait', detail: 'another tools thread was rendering', timing: timing() };
          continue;
        }
        return { ...run, timing: { queue: waited, render: Math.round(now() - started), total: Math.round(now() - t0), peak_rss_mb: run.peak_rss_mb } };
      }
    } finally {
      release();
    }
  }

  return { render, availability, config: () => ({ ...config }) };
}

/** A file name for an app picture. */
export const appPictureName = (url, at = Date.now()) => `app-${new Date(at).toISOString().replace(/[:.]/g, '-')}-${createHash('sha1').update(url).digest('hex').slice(0, 8)}.png`;
