import { createHash, randomBytes } from 'node:crypto'
import { describe, expect, it } from 'vitest'
import { Sha256 } from '../sha256'

// The SHA-256 a downloaded model is checked with, fed part by part (lib/sha256.ts), against Node's.

const node = (b: Uint8Array) => createHash('sha256').update(b).digest('hex')

describe('Sha256', () => {
  it('matches the standard vectors', () => {
    expect(new Sha256().hex()).toBe('e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855')
    expect(new Sha256().update(new TextEncoder().encode('abc')).hex()).toBe('ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad')
  })

  it('gives Node’s hash for any length, fed in pieces of any size', () => {
    for (const n of [1, 55, 56, 63, 64, 65, 119, 120, 1000, 70000]) {
      const b = new Uint8Array(randomBytes(n))
      expect(new Sha256().update(b).hex(), `${n}`).toBe(node(b))
      const h = new Sha256()
      for (let i = 0, step = 1; i < n; i += step, step = (step * 7 + 3) % 97 || 1) h.update(b.subarray(i, i + step))
      expect(h.hex(), `${n} in pieces`).toBe(node(b))
    }
  })
})
