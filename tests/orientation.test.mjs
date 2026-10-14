import test from 'node:test';
import assert from 'node:assert/strict';
import { readOrientation, turned, writeOrientation } from '../server/orientation.mjs';

// A photo turned by its EXIF orientation tag, its pixels untouched (server/orientation.mjs).

/** A TIFF header and one directory holding the orientation tag (and another before it). */
function tiff(value, little = true) {
  const out = new Uint8Array(8 + 2 + 2 * 12 + 4);
  const view = new DataView(out.buffer);
  out.set(little ? [0x49, 0x49] : [0x4d, 0x4d]);
  view.setUint16(2, 42, little);
  view.setUint32(4, 8, little);
  view.setUint16(8, 2, little);
  // ImageWidth (a LONG), then Orientation (a SHORT).
  view.setUint16(10, 0x0100, little);
  view.setUint16(12, 4, little);
  view.setUint32(14, 1, little);
  view.setUint32(18, 6000, little);
  view.setUint16(22, 0x0112, little);
  view.setUint16(24, 3, little);
  view.setUint32(26, 1, little);
  view.setUint16(30, value, little);
  return out;
}
/** A JPEG: its start, an APP0 block, the Exif block, then the image data. */
function jpeg(value) {
  const exif = new Uint8Array([...Buffer.from('Exif\0\0'), ...tiff(value, false)]);
  const app1 = [0xff, 0xe1, (exif.length + 2) >> 8, (exif.length + 2) & 0xff, ...exif];
  return new Uint8Array([0xff, 0xd8, 0xff, 0xe0, 0x00, 0x04, 0x4a, 0x46, ...app1, 0xff, 0xda, 0x00, 0x02, 1, 2, 3, 0xff, 0xd9]);
}

test('the tag is read and changed in a JPEG and in a TIFF-based RAW, nothing else moving', () => {
  for (const [kind, bytes] of [
    ['jpeg', jpeg(1)],
    ['raw, little-endian', tiff(1)],
    ['raw, big-endian', tiff(1, false)],
  ]) {
    assert.equal(readOrientation(bytes).value, 1, kind);
    const out = writeOrientation(bytes, 6);
    assert.equal(readOrientation(out).value, 6, kind);
    assert.equal(out.length, bytes.length);
    const differ = [...out].filter((b, i) => b !== bytes[i]).length;
    assert.equal(differ, 1, `${kind}: one byte changed`);
    assert.equal(readOrientation(bytes).value, 1, `${kind}: the original is left as it was`);
  }
});

test('a file without the tag is not changed', () => {
  const plain = new Uint8Array([0xff, 0xd8, 0xff, 0xda, 0x00, 0x02, 1, 0xff, 0xd9]);
  assert.equal(readOrientation(plain), null);
  assert.equal(writeOrientation(plain, 6), null);
  assert.equal(readOrientation(new Uint8Array([1, 2, 3, 4])), null);
});

test('turning: the tag that shows the photo further clockwise', () => {
  assert.equal(turned(1, 90), 6);
  assert.equal(turned(6, 90), 3);
  assert.equal(turned(8, 90), 1);
  assert.equal(turned(1, 270), 8);
  assert.equal(turned(3, 180), 1);
  assert.equal(turned(undefined, 180), 3);
  assert.equal(turned(6, 0), 6);
  // Mirrored tags are left to a person.
  assert.equal(turned(2, 90), null);
});
