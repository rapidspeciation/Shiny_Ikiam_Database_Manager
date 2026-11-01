import { describe, expect, it } from 'vitest'
import { chooseModel, encoderInput, modelReady, resizeBilinear, roleOf, storedName, studentInput, type ListedFile, type StoredFile } from '../offlineId'

// Reference values from torch 2 F.interpolate(mode='bilinear', align_corners=False), antialias off (the default).
describe('the small model of the offline page', () => {
  it('resizes like torch bilinear with half-pixel centres and no antialias', () => {
    const x = Float32Array.from({ length: 2 * 7 * 5 }, (_, i) => i * i * 0.01)
    const y = resizeBilinear(x, 2, 7, 5, 4, 3)
    const want = [
      0.109583, 0.20875, 0.367917, 1.230417, 1.62125, 2.072083, 3.91375, 4.59625, 5.33875, 8.159583, 9.13375, 10.167916, 13.905416, 15.171249, 16.497084,
      21.151251, 22.70875, 24.32625, 29.959583, 31.808748, 33.717915, 40.330414, 42.471249, 44.672081,
    ]
    expect(y.length).toBe(want.length)
    want.forEach((v, i) => expect(y[i]).toBeCloseTo(v, 3))
  })

  it('224 -> 192 as torch gives it, edges included', () => {
    const x = new Float32Array(3 * 224 * 224)
    for (let c = 0; c < 3; c++) for (let r = 0; r < 224; r++) for (let q = 0; q < 224; q++) x[c * 224 * 224 + r * 224 + q] = ((c * 31 + r * 7 + q * 3) % 17) / 17
    const y = resizeBilinear(x, 3, 224, 224, 192, 192)
    const at = (c: number, r: number, q: number) => y[c * 192 * 192 + r * 192 + q]
    const want: [number, number, number, number][] = [[0, 0, 0, 0.04902], [0, 0, 1, 0.254902], [1, 5, 7, 0.715686], [2, 100, 57, 0.470584], [2, 191, 191, 0.774504], [1, 191, 0, 0.627447]]
    for (const [c, r, q, v] of want) expect(at(c, r, q)).toBeCloseTo(v, 4) // float32 rounding: within 1e-5
    expect(y.reduce((a, b) => a + b, 0)).toBeCloseTo(52042.211, 0)
  })

  it('normalises with the CLIP mean and std, then resizes (the same as the other order: both are linear)', () => {
    const rgb = new Uint8Array(224 * 224 * 3)
    for (let i = 0; i < rgb.length; i++) rgb[i] = (i * 37) % 256
    const x = studentInput(rgb)
    expect(x.length).toBe(3 * 192 * 192)
    // resize the 0..1 image first, normalise after
    const plain = new Float32Array(3 * 224 * 224)
    for (let i = 0; i < 224 * 224; i++) for (let c = 0; c < 3; c++) plain[c * 224 * 224 + i] = rgb[i * 3 + c] / 255
    const r = resizeBilinear(plain, 3, 224, 224, 192, 192)
    const mean = [0.48145466, 0.4578275, 0.40821073], std = [0.26862954, 0.26130258, 0.27577711]
    for (const i of [0, 1, 500, 192 * 192 - 1, 192 * 192 + 77, 2 * 192 * 192 + 1234]) {
      const c = Math.floor(i / (192 * 192))
      expect(x[i]).toBeCloseTo((r[i] - mean[c]) / std[c], 4)
    }
    // a flat white crop stays the normalised white everywhere
    const white = studentInput(new Uint8Array(224 * 224 * 3).fill(255))
    expect(white[192 * 192 * 2 + 5]).toBeCloseTo(encoderInput(new Uint8Array(3).fill(255), 1)[2], 5)
  })

  const listed: ListedFile[] = [
    { name: 'wing_seg_v5n.onnx', etag: '"d"', bytes: 1, role: 'detector' },
    { name: 'bioclip25h_visual_int8.onnx', etag: '"e"', bytes: 1, role: 'encoder' },
    { name: 'head_b8127dbf.json', etag: '"m"', bytes: 1, role: 'head-meta' },
    { name: 'head_b8127dbf_fp16.bin', etag: '"h"', bytes: 1, role: 'head' },
    { name: 'student_L1_int8.onnx', etag: '"s"', bytes: 1, role: 'student' },
  ]
  const store = (names: string[]): Record<string, StoredFile> => Object.fromEntries(listed.filter(f => names.includes(f.name)).map(f => [f.name, { etag: f.etag, bytes: f.bytes, role: f.role }]))

  it('a model is ready with its own encoder and the shared detector and head, in the versions listed', () => {
    const small = store(['wing_seg_v5n.onnx', 'head_b8127dbf.json', 'head_b8127dbf_fp16.bin', 'student_L1_int8.onnx'])
    expect(modelReady('small', small, listed)).toBe(true)
    expect(modelReady('full', small, listed)).toBe(false)
    expect(modelReady('small', small, null)).toBe(true) // offline: what is stored
    // a newer small model on the server (another name): not ready until downloaded again
    const swapped = listed.map(f => (f.role === 'student' ? { ...f, name: 'student_L2_int8.onnx', etag: '"s2"' } : f))
    expect(modelReady('small', small, swapped)).toBe(false)
    // the server has no small model: never ready online
    expect(modelReady('small', small, listed.filter(f => f.role !== 'student'))).toBe(false)
    expect(modelReady('full', null, listed)).toBe(false)
  })

  it('reads stored.json written before the small model (no roles) by name', () => {
    expect(roleOf('bioclip25h_visual_int8.onnx')).toBe('encoder')
    expect(roleOf('student_L1_int8.onnx')).toBe('student')
    expect(roleOf('wing_seg_v5n.onnx')).toBe('detector')
    expect(roleOf('head_b8127dbf.json')).toBe('head-meta')
    expect(roleOf('head_b8127dbf_fp16.bin')).toBe('head')
    const old = { 'wing_seg_v5n.onnx': { etag: '"d"', bytes: 1 }, 'bioclip25h_visual_int8.onnx': { etag: '"e"', bytes: 1 }, 'head_b8127dbf.json': { etag: '"m"', bytes: 1 }, 'head_b8127dbf_fp16.bin': { etag: '"h"', bytes: 1 } }
    expect(modelReady('full', old, listed)).toBe(true)
    expect(storedName(old, 'encoder')).toBe('bioclip25h_visual_int8.onnx')
    expect(storedName(old, 'student')).toBe(null)
  })

  it('identifies with the model chosen, or the only one downloaded', () => {
    expect(chooseModel('small', { full: true, small: true })).toBe('small')
    expect(chooseModel('full', { full: false, small: true })).toBe('small')
    expect(chooseModel(null, { full: true, small: false })).toBe('full')
    expect(chooseModel(null, { full: true, small: true })).toBe('full')
    expect(chooseModel('small', { full: false, small: false })).toBe(null)
  })
})
