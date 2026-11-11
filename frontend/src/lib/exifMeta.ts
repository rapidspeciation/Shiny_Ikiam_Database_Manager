/**
 * The location and the day a photo was taken, read from the camera's own bytes (its EXIF) before the canvas makes
 * the copy that is sent (the canvas writes no EXIF). Only these two are read; nothing else of the EXIF leaves the
 * phone, and the location only if the person ticks «Incluir la ubicación de la foto».
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

/** The location rounded to `decimals` (0.01°: about 1 km). */
export const roundGps = (g: { lat: number; lon: number }, decimals: number) => {
  const k = 10 ** decimals
  return { lat: Math.round(g.lat * k) / k, lon: Math.round(g.lon * k) / k }
}
