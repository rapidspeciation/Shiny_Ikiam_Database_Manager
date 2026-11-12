/**
 * A photo's EXIF, read on the phone: the day it was taken (kept with a photo sent to train the AI) and its location
 * (only for lib/photoCountry.ts, which orders the reference photos by country on this phone; never sent).
 * stripJpegMeta takes a JPEG's metadata segments out (EXIF with its GPS, XMP, comments) before a photo leaves the
 * phone; the server takes them out again on receipt (server/contributions.mjs).
 */

export interface PhotoMeta {
  gps: { lat: number; lon: number } | null
  /** YYYY-MM-DD (DateTimeOriginal, else DateTime), or null. */
  takenAt: string | null
}

const NONE: PhotoMeta = { gps: null, takenAt: null }

/** The EXIF of a JPEG (its APP1 «Exif» segment) as GPS and the day taken; nothing found gives nulls. */
export function photoMeta(buf: ArrayBuffer): PhotoMeta {
  try {
    const v = new DataView(buf)
    if (v.byteLength < 4 || v.getUint16(0) !== 0xffd8) return NONE
    let i = 2
    while (i + 4 <= v.byteLength) {
      if (v.getUint8(i) !== 0xff) return NONE
      const m = v.getUint8(i + 1)
      if (m === 0xda || m === 0xd9) return NONE
      const len = v.getUint16(i + 2)
      if (m === 0xe1 && len >= 8 && v.getUint32(i + 4) === 0x45786966 /* Exif */) return readTiff(new DataView(buf, i + 10, len - 8))
      i += 2 + len
    }
  } catch {
    /* a cut or odd file: nothing read */
  }
  return NONE
}

function readTiff(t: DataView): PhotoMeta {
  const little = t.getUint16(0) === 0x4949
  const u16 = (o: number) => t.getUint16(o, little)
  const u32 = (o: number) => t.getUint32(o, little)
  /** An IFD's entries: tag -> { type, count, value offset (or the 4 bytes in place) }. */
  const ifd = (at: number) => {
    const out = new Map<number, { type: number; count: number; at: number }>()
    const n = u16(at)
    for (let k = 0; k < n; k++) {
      const e = at + 2 + k * 12
      const type = u16(e + 2), count = u32(e + 4)
      const size = ({ 1: 1, 2: 1, 3: 2, 4: 4, 5: 8 } as Record<number, number>)[type] ?? 1
      out.set(u16(e), { type, count, at: size * count > 4 ? u32(e + 8) : e + 8 })
    }
    return out
  }
  const ascii = (e?: { count: number; at: number }) =>
    e ? String.fromCharCode(...Array.from({ length: Math.min(e.count, 40) }, (_, k) => t.getUint8(e.at + k))).replace(/\0.*$/, '') : ''
  const rational = (at: number) => {
    const d = u32(at + 4)
    return d ? u32(at) / d : NaN
  }
  const root = ifd(u32(4))
  let taken = ''
  const sub = root.get(0x8769)
  if (sub) taken = ascii(ifd(u32(sub.at)).get(0x9003))
  if (!taken) taken = ascii(root.get(0x0132))
  const day = /^(\d{4}):(\d{2}):(\d{2})/.exec(taken)
  let gps: PhotoMeta['gps'] = null
  const gp = root.get(0x8825)
  if (gp) {
    const g = ifd(u32(gp.at))
    const deg = (tag: number) => {
      const e = g.get(tag)
      if (!e || e.type !== 5 || e.count < 3) return NaN
      return rational(e.at) + rational(e.at + 8) / 60 + rational(e.at + 16) / 3600
    }
    const ref = (tag: number) => String.fromCharCode(t.getUint8(g.get(tag)?.at ?? 0))
    let lat = deg(2), lon = deg(4)
    if (g.get(1) && ref(1) === 'S') lat = -lat
    if (g.get(3) && ref(3) === 'W') lon = -lon
    // 0,0 is what some phones write when there was no fix.
    if (Number.isFinite(lat) && Number.isFinite(lon) && Math.abs(lat) <= 90 && Math.abs(lon) <= 180 && (lat !== 0 || lon !== 0)) gps = { lat, lon }
  }
  return { gps, takenAt: day && day[1] !== '0000' ? `${day[1]}-${day[2]}-${day[3]}` : null }
}

/**
 * The JPEG without its APP1-APP15 and comment segments (EXIF and its GPS, XMP, ICC, maker notes), the image data
 * untouched; the same bytes when there are none. Not a JPEG, or a cut one: null (nothing is sent rather than a photo
 * whose metadata could not be checked).
 */
export function stripJpegMeta(bytes: Uint8Array): Uint8Array | null {
  if (bytes.length < 4 || bytes[0] !== 0xff || bytes[1] !== 0xd8) return null
  const keep: Uint8Array[] = [bytes.subarray(0, 2)]
  let i = 2
  let dropped = false
  while (i < bytes.length) {
    if (bytes[i] !== 0xff) return null
    let m = bytes[i + 1]
    // Fill bytes before a marker.
    while (m === 0xff && i + 2 < bytes.length) m = bytes[++i + 1]
    if (m === 0xd9) {
      keep.push(bytes.subarray(i, i + 2))
      break
    }
    if (m === 0x01 || (m >= 0xd0 && m <= 0xd7)) {
      keep.push(bytes.subarray(i, i + 2))
      i += 2
      continue
    }
    if (i + 4 > bytes.length) return null
    const len = (bytes[i + 2] << 8) | bytes[i + 3]
    if (len < 2 || i + 2 + len > bytes.length) return null
    if (m === 0xda) {
      // The image data runs to the end.
      keep.push(bytes.subarray(i))
      break
    }
    if ((m >= 0xe1 && m <= 0xef) || m === 0xfe) dropped = true
    else keep.push(bytes.subarray(i, i + 2 + len))
    i += 2 + len
  }
  if (!dropped) return bytes
  const out = new Uint8Array(keep.reduce((n, k) => n + k.length, 0))
  let at = 0
  for (const k of keep) {
    out.set(k, at)
    at += k.length
  }
  return out
}
