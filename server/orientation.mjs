// A photo turned without touching its pixels: its EXIF orientation tag (0x0112) changed in place, in a
// JPEG (in its Exif block) or a TIFF-based RAW file (Canon .CR2, .NEF, .ARW, .DNG: in the first
// directory), the file the same size. Viewers and Drive's previews show the photo turned by the tag.
// readOrientation(bytes) → { value, at } (null without a tag); writeOrientation(bytes, value) → a copy
// with the tag set, or null when the file has no tag to change. turned(value, degrees) → the tag that
// shows the photo `degrees` further clockwise.

/** Where a TIFF header starts in the file: 0 for a TIFF-based RAW, inside the Exif block of a JPEG. */
function tiffStart(bytes) {
  if (bytes[0] === 0xff && bytes[1] === 0xd8) {
    for (let at = 2; at + 4 < bytes.length; ) {
      if (bytes[at] !== 0xff) return null;
      const marker = bytes[at + 1];
      // The image data starts: no Exif block before it.
      if (marker === 0xda || marker === 0xd9) return null;
      const length = (bytes[at + 2] << 8) | bytes[at + 3];
      if (marker === 0xe1 && String.fromCharCode(...bytes.subarray(at + 4, at + 10)) === 'Exif\0\0') return at + 10;
      at += 2 + length;
    }
    return null;
  }
  const order = String.fromCharCode(bytes[0], bytes[1]);
  return order === 'II' || order === 'MM' ? 0 : null;
}

export function readOrientation(bytes) {
  const start = tiffStart(bytes);
  if (start === null) return null;
  const little = bytes[start] === 0x49;
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  const u16 = at => view.getUint16(at, little);
  if (u16(start + 2) !== 42) return null;
  const ifd = start + view.getUint32(start + 4, little);
  if (ifd + 2 > bytes.length) return null;
  const entries = u16(ifd);
  for (let i = 0; i < entries; i++) {
    const entry = ifd + 2 + i * 12;
    if (entry + 12 > bytes.length) return null;
    // A SHORT, one of it: the value sits in the entry itself.
    if (u16(entry) === 0x0112 && u16(entry + 2) === 3) return { value: u16(entry + 8), at: entry + 8, little };
  }
  return null;
}

export function writeOrientation(bytes, value) {
  const tag = readOrientation(bytes);
  if (!tag) return null;
  const out = new Uint8Array(bytes);
  new DataView(out.buffer, out.byteOffset, out.byteLength).setUint16(tag.at, value, tag.little);
  return out;
}

// The tags without a mirror, by the clockwise turn they show: 1 upright, 6 a quarter, 3 a half, 8 three quarters.
const BY_TURN = [1, 6, 3, 8];
/** The tag that shows a photo of tag `value` turned `degrees` further clockwise (null for a mirrored tag). */
export function turned(value, degrees) {
  const now = BY_TURN.indexOf(value || 1);
  if (now < 0) return null;
  return BY_TURN[(now + Math.round(degrees / 90) + 4 * 10) % 4];
}
