// Reference photos for the Identificar tab (frontend/identificar/, src/views/IdentifyView.vue). The tab is cross-origin
// isolated (COEP require-corp, server/offline-id.mjs), so iNaturalist's photos cannot be shown from their own servers
// (they send no Cross-Origin-Resource-Policy): they come through here, same origin, and the phone keeps them.
//
// - GET /api/identify/ref-photo?u=<url>: one iNaturalist photo, only of iNaturalist's photo hosts and sizes (the URLs
//   /api/species-photos gives), streamed with a size limit. Signed in only, like the rest of the team's API.
// - GET /api/identify/field-photos: one photo per Ithomiini species of the identifier's head (the genera the workbook
//   files under Tribe Ithomiini, server/frozen-stock.mjs) for «Fotos para el campo»: those iNaturalist has at species
//   level (a genus fallback would show another species), with their credit; `pending` the names still being looked up.

import { randomUUID } from 'node:crypto';
import { mkdirSync, rmSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';

const HOSTS = /^https:\/\/(inaturalist-open-data\.s3\.amazonaws\.com|static\.inaturalist\.org)\/photos\/\d+\/(square|small|medium)\.(jpe?g|png|gif|webp)$/i;
const MAX_BYTES = 3 * 1024 * 1024;

/** The iNaturalist photo URL, or null when it is not one this proxy sends. */
export function refPhotoUrl(u) {
  const value = String(u ?? '').trim();
  return HOSTS.test(value) ? value : null;
}

/** Streams the photo at `u` (checked by refPhotoUrl) to `res`; 404 when not allowed, 502 when iNaturalist fails. */
export async function sendRefPhoto(req, res, u, { fetchImpl = fetch } = {}) {
  const url = refPhotoUrl(u);
  const fail = (status, message) => {
    res.writeHead(status, { 'content-type': 'application/json', 'cache-control': 'no-store' });
    res.end(JSON.stringify({ error: { code: 'REF_PHOTO', message } }));
  };
  if (!url) return fail(404, 'Not an iNaturalist photo');
  let r;
  try {
    r = await fetchImpl(url, { signal: AbortSignal.timeout(15_000), redirect: 'error' });
  } catch {
    return fail(502, 'iNaturalist did not answer');
  }
  const type = r.headers.get('content-type') || '';
  const length = Number(r.headers.get('content-length') || 0);
  if (!r.ok || !type.startsWith('image/') || length > MAX_BYTES) {
    r.body?.cancel().catch(() => {});
    return fail(502, `iNaturalist: ${r.status}`);
  }
  const chunks = [];
  let got = 0;
  for await (const chunk of r.body) {
    got += chunk.length;
    if (got > MAX_BYTES) return fail(502, 'Photo too large');
    chunks.push(chunk);
  }
  const body = Buffer.concat(chunks);
  res.writeHead(200, {
    'content-type': type,
    'content-length': body.length,
    // A photo of a URL never changes; the phone keeps it (the tab's Cache Storage) for use without signal.
    'cache-control': 'private, max-age=2592000, immutable',
    'cross-origin-resource-policy': 'same-origin',
    'x-content-type-options': 'nosniff',
  });
  res.end(req.method === 'HEAD' ? undefined : body);
}

/** The head's species of the Ithomiini genera, sorted. */
export function ithomiiniSpecies(headSpecies, genera) {
  const set = new Set(genera);
  return headSpecies.filter(s => set.has(s.split(' ')[0])).sort();
}

/**
 * { species: [{ name, thumb, attribution, license, taxonUrl }], pending, total }: one photo per species that
 * iNaturalist has at species level. `photosOf` is server/species-photos.mjs's (at most `batch` names per call).
 */
export function fieldPhotos(photosOf, names, { batch = 300 } = {}) {
  const species = [];
  let pending = 0;
  for (let i = 0; i < names.length; i += batch) {
    const found = photosOf(names.slice(i, i + batch));
    for (const name of names.slice(i, i + batch)) {
      const entry = found[name];
      if (entry?.pending) pending++;
      const inat = entry?.inat;
      const photo = inat && inat.level !== 'genus' ? inat.photos?.find(p => refPhotoUrl(p.thumb)) : null;
      if (photo) species.push({ name, thumb: photo.thumb, attribution: photo.attribution, license: photo.license, taxonUrl: inat.taxon?.url ?? null });
    }
  }
  return { species, pending, total: names.length };
}

/**
 * POST /api/ai/identify-photo (server/assistant.mjs), Identificar's «En línea (servidor)», signed in only: a photo
 * the page made smaller (JPEG as base64, at most MAX_UPLOAD bytes) read by the team's AI Identifier
 * (server/identifier.mjs, the same process as identify_species), with the side of the Andes. The photo is written to
 * a file of its own for the identifier and removed after, whatever the answer: nothing is kept.
 * Returns { status, body }: 200 { species, subspecies, genus, crop, wingBox, prior, ms } (the worker's lists),
 * 400 a bad photo, 503 no identifier here or it failed.
 */
export const MAX_UPLOAD = 6 * 1024 * 1024;
export async function identifyUpload({ identifier, body, scratch }) {
  const bad = (status, code, message) => ({ status, body: { error: { code, message } } });
  if (!identifier) return bad(503, 'unavailable', 'The identifier is not installed on this server.');
  const data = typeof body?.photo === 'string' ? body.photo : '';
  if (!data || data.length > Math.ceil(MAX_UPLOAD / 3) * 4 || !/^[A-Za-z0-9+/=]+$/.test(data)) return bad(400, 'invalid_photo', 'Send the photo as base64 JPEG.');
  const bytes = Buffer.from(data, 'base64');
  if (bytes.length < 4 || bytes[0] !== 0xff || bytes[1] !== 0xd8) return bad(400, 'invalid_photo', 'The photo must be a JPEG.');
  const side = body?.side === 'East' || body?.side === 'West' ? body.side : null;
  mkdirSync(scratch, { recursive: true, mode: 0o700 });
  const path = join(scratch, `${randomUUID()}.jpg`);
  try {
    writeFileSync(path, bytes, { mode: 0o600 });
    const out = await identifier.identify({ path, side });
    if (!out || out.error) return bad(503, 'identifier_failed', String(out?.error ?? 'The identifier did not answer.'));
    const pairs = list => (Array.isArray(list) ? list.filter(x => Array.isArray(x) && typeof x[0] === 'string').map(([n, p]) => [n, Number(p) || 0]) : []);
    return {
      status: 200,
      body: {
        species: pairs(out.species),
        subspecies: pairs(out.subspecies),
        genus: pairs(out.genus),
        crop: typeof out.crop === 'string' ? out.crop : null,
        wingBox: Array.isArray(out.wingBox) && out.wingBox.length === 4 ? out.wingBox.map(Number) : null,
        prior: out.prior ?? null,
        ms: Number(out.ms) || null,
      },
    };
  } finally {
    rmSync(path, { force: true });
  }
}
