// Host-plant photos ready to send (host_plants' `send_photos`, server/maps-tools.mjs): each photo of the maps' plant
// gallery downloaded at about HD size (MAX_PX on its long side, a JPEG: not the original, not a thumbnail), with its credit read from its GBIF record (photographer, licence,
// place, year, link). Photos and credits are kept on disk (`dir`) and in memory, so a photo asked again is ready
// at once; all of one answer's are fetched together.

import { createHash } from 'node:crypto';
import { mkdirSync, readFileSync, statSync } from 'node:fs';
import { join } from 'node:path';
import { writeCacheFile } from './cache-write.mjs';

export const MAX_PX = 2048;
const GBIF_API = 'https://api.gbif.org/v1/occurrence/';
const TIMEOUT_MS = 20_000;
// A photo larger than this is not read further (a 2048 px JPEG is 0.2-3 MB).
export const MAX_BYTES = 12 * 1024 * 1024;

/** The image proxy's JPEG of a photo, at most MAX_PX on its long side. */
export const proxyUrl = img => `https://wsrv.nl/?url=${encodeURIComponent(img)}&w=${MAX_PX}&h=${MAX_PX}&fit=inside&we&output=jpg&q=82`;

/**
 * The URL of a photo at about MAX_PX on its long side: iNaturalist's own `original` size (at most 2048 px there; its
 * `large` is 1024), else the image proxy's JPEG.
 */
export function sizedUrl(img) {
  const inat = /^(https:\/\/(?:inaturalist-open-data\.s3\.amazonaws\.com|static\.inaturalist\.org)\/photos\/\d+\/)(?:original|large|medium)\.(jpe?g)$/i.exec(img);
  if (inat) return `${inat[1]}original.${inat[2]}`;
  return proxyUrl(img);
}

/** "CC BY-NC 4.0" from a Creative Commons URL ("http://creativecommons.org/licenses/by-nc/4.0/"), else the text. */
export function licenceName(value) {
  const s = String(value ?? '').trim();
  if (!s) return null;
  if (/publicdomain\/zero|^cc0/i.test(s)) return 'CC0';
  if (/publicdomain\/mark/i.test(s)) return 'Public domain';
  const cc = /creativecommons\.org\/licenses\/([a-z-]+)\/(\d\.\d)/i.exec(s);
  if (cc) return `CC ${cc[1].toUpperCase()} ${cc[2]}`;
  return s.replace(/_/g, ' ');
}

/** The width and height of a JPEG from its frame header, or null. */
export function jpegSize(buf) {
  if (buf.length < 4 || buf[0] !== 0xff || buf[1] !== 0xd8) return null;
  let i = 2;
  while (i + 9 < buf.length) {
    if (buf[i] !== 0xff) return null;
    const marker = buf[i + 1];
    const len = buf.readUInt16BE(i + 2);
    if (marker >= 0xc0 && marker <= 0xcf && ![0xc4, 0xc8, 0xcc].includes(marker)) return { height: buf.readUInt16BE(i + 5), width: buf.readUInt16BE(i + 7) };
    i += 2 + len;
  }
  return null;
}

/** An iNaturalist photo's number (its sizes share it), else the URL itself. */
const photoKey = url => /inaturalist[^/]*\/photos\/(\d+)\//.exec(String(url ?? ''))?.[1] ?? String(url ?? '');

/**
 * The credit of one photo from its GBIF record, compact. Only the record's `media` entry of this photo speaks for it
 * (a record's photos, and its licence, can differ per photo): without one, `matched` is false and the photographer
 * and licence are null, never borrowed from another photo or the record.
 */
export function creditOf(record, img) {
  const found = (record?.media ?? []).find(m => m.identifier && photoKey(m.identifier) === photoKey(img));
  const media = found ?? {};
  // iNaturalist's verbatim places end in the country code (", EC"): dropped, the country follows.
  const parts = [record?.locality || record?.verbatimLocality || record?.municipality, record?.stateProvince, record?.country]
    .flatMap(v => String(v ?? '').split(/,\s*/))
    .map(v => v.trim())
    .filter(v => v && !/^[A-Z]{2}$/.test(v));
  // "Tena, Napo Prov., Ecuador": a later part already inside an earlier one is dropped.
  const place = parts.filter((p, i) => !parts.slice(0, i).some(q => q.toLowerCase().includes(p.toLowerCase().replace(/ prov\.?$/, '')))).join(', ');
  return {
    matched: !!found,
    photographer: String(media.creator || media.rightsHolder || '').trim() || null,
    licence: licenceName(media.license),
    licence_url: media.license || null,
    rights_holder: String(media.rightsHolder || '').trim() || null,
    place: place.length > 120 ? `${place.slice(0, 117)}…` : place || null,
    year: Number.isFinite(record?.year) ? record.year : null,
    as_recorded: record?.species || record?.scientificName || null,
    link: /^https?:\/\//.test(record?.references ?? '') ? record.references : record?.key ? `https://www.gbif.org/occurrence/${record.key}` : null,
    gbif: record?.key ? `https://www.gbif.org/occurrence/${record.key}` : null,
    institution: record?.institutionCode ?? null,
  };
}

const KEEP = ['key', 'species', 'scientificName', 'locality', 'verbatimLocality', 'municipality', 'stateProvince', 'country', 'year', 'recordedBy', 'rightsHolder', 'license', 'references', 'institutionCode', 'media'];

/**
 * `dir`: where photos and credits are kept (none: memory only). `fetchImpl`: for tests.
 * -> { credit(gbifId, img), photo(img), stats() }
 */
export function createPlantPhotos({ dir = null, fetchImpl = fetch } = {}) {
  const credits = new Map();
  const photos = new Map();
  let made = false;
  const ensure = () => {
    if (!dir || made) return;
    mkdirSync(join(dir, 'photos'), { recursive: true });
    mkdirSync(join(dir, 'gbif'), { recursive: true });
    made = true;
  };
  const counts = { credit_hits: 0, credit_fetched: 0, photo_hits: 0, photo_fetched: 0 };

  /** The GBIF record's fields a credit needs, from memory, the disk, or the API. */
  function record(id) {
    const key = String(id ?? '').replace(/\D/g, '');
    if (!key) return Promise.resolve(null);
    if (credits.has(key)) {
      counts.credit_hits++;
      return credits.get(key);
    }
    const loading = (async () => {
      ensure();
      const file = dir ? join(dir, 'gbif', `${key}.json`) : null;
      if (file) {
        try {
          const kept = JSON.parse(readFileSync(file, 'utf8'));
          counts.credit_hits++;
          return kept;
        } catch {
          /* not kept yet */
        }
      }
      const r = await fetchImpl(`${GBIF_API}${key}`, { headers: { 'user-agent': 'ithomiini-database' }, signal: AbortSignal.timeout(TIMEOUT_MS) });
      if (!r.ok) throw new Error(`GBIF answered ${r.status} for ${key}`);
      const full = await r.json();
      const kept = Object.fromEntries(KEEP.filter(k => full[k] !== undefined).map(k => [k, full[k]]));
      counts.credit_fetched++;
      if (file) writeCacheFile(file, JSON.stringify(kept));
      return kept;
    })();
    credits.set(key, loading);
    loading.catch(() => credits.delete(key));
    return loading;
  }

  /** A photo of at most MAX_PX on disk: { path, bytes, width, height, url, cached }. */
  function photo(img) {
    if (photos.has(img)) return photos.get(img);
    const loading = (async () => {
      const url = sizedUrl(img);
      ensure();
      const name = `${createHash('sha1').update(url).digest('hex').slice(0, 20)}.jpg`;
      const path = dir ? join(dir, 'photos', name) : null;
      if (path) {
        try {
          const buf = readFileSync(path);
          const size = jpegSize(buf);
          if (size) {
            counts.photo_hits++;
            return { path, bytes: statSync(path).size, ...size, url, cached: true };
          }
        } catch {
          /* not kept yet */
        }
      }
      const get = async u => {
        const r = await fetchImpl(u, { headers: { 'user-agent': 'ithomiini-database' }, signal: AbortSignal.timeout(TIMEOUT_MS) });
        if (!r.ok) throw new Error(`The photo's site answered ${r.status}`);
        const buf = await capped(r);
        return { buf, size: jpegSize(buf) };
      };
      let got = await get(url);
      let used = url;
      // An iNaturalist original larger than MAX_PX (or not a JPEG): the proxy's copy instead.
      if (url !== proxyUrl(img) && (!got.size || Math.max(got.size.width, got.size.height) > MAX_PX)) {
        used = proxyUrl(img);
        got = await get(used);
      }
      if (!got.size) throw new Error('Not a JPEG');
      counts.photo_fetched++;
      if (path) writeCacheFile(path, got.buf);
      return { path, bytes: got.buf.length, ...got.size, url: used, cached: false };
    })();
    photos.set(img, loading);
    loading.catch(() => photos.delete(img));
    return loading;
  }

  return {
    credit: async (id, img) => creditOf(await record(id), img),
    photo,
    stats: () => ({ ...counts }),
  };
}

/** A response's bytes, given up past MAX_BYTES (the download is cancelled). */
async function capped(response) {
  if (Number(response.headers.get('content-length')) > MAX_BYTES) {
    await response.body?.cancel();
    throw new Error('Photo too large');
  }
  if (!response.body) return Buffer.alloc(0);
  const parts = [];
  let size = 0;
  for await (const part of response.body) {
    size += part.length;
    if (size > MAX_BYTES) throw new Error('Photo too large');
    parts.push(part);
  }
  return Buffer.concat(parts.map(p => Buffer.from(p)));
}
