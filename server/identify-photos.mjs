// Reference photos for the Identificar tab (frontend/identificar/, src/views/IdentifyView.vue). The tab is cross-origin
// isolated (COEP require-corp, server/offline-id.mjs), so iNaturalist's photos cannot be shown from their own servers
// (they send no Cross-Origin-Resource-Policy): they come through here, same origin, and the phone keeps them.
//
// - GET /api/identify/ref-photo?u=<url>: one iNaturalist photo, only of iNaturalist's photo hosts and sizes (the URLs
//   /api/species-photos gives), streamed with a size limit. Signed in only, like the rest of the team's API.
// - GET /api/identify/field-photos: one photo per Ithomiini species of the identifier's head (the genera the workbook
//   files under Tribe Ithomiini, server/frozen-stock.mjs) for «Fotos para el campo»: those iNaturalist has at species
//   level (a genus fallback would show another species), with their credit; `pending` the names still being looked up.

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
