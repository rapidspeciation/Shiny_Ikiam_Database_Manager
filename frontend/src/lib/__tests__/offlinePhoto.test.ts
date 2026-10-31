import { describe, expect, it } from 'vitest'
import { errorText, takePhoto, type Photo } from '../offlinePhoto'

/** A File stand-in that logs when it is read, and fails once `revoked` (Android's lost read grant). */
function androidFile(log: string[], state: { revoked: boolean }) {
  const bytes = new Uint8Array([0xff, 0xd8, 1, 2, 3]).buffer
  return {
    name: 'IMG_1.jpg',
    type: 'image/jpeg',
    size: 5,
    arrayBuffer: async () => {
      log.push('read')
      if (state.revoked) throw new DOMException('The requested file could not be read', 'NotReadableError')
      return bytes
    },
  } as unknown as File
}

function fileInput(file: File | null, log: string[], state: { revoked: boolean }) {
  let value = 'C:\\fakepath\\IMG_1.jpg'
  return {
    files: file ? [file] : [],
    get value() { return value },
    set value(v: string) {
      log.push('clear')
      value = v
      state.revoked = true
    },
  }
}

describe('takePhoto', () => {
  it('reads the bytes before clearing the input and before identify loads any model', async () => {
    const log: string[] = []
    const state = { revoked: false }
    let got: Photo | null = null
    const identify = async (photo: Photo) => {
      log.push('load models')
      await new Promise(r => setTimeout(r, 5))
      got = photo
      log.push('identify')
    }
    await takePhoto(fileInput(androidFile(log, state), log, state), identify, () => log.push('unreadable'))
    expect(log).toEqual(['read', 'clear', 'load models', 'identify'])
    expect(got!.bytes.byteLength).toBe(5)
    expect(got!.size).toBe(5)
    expect(got!.blob.type).toBe('image/jpeg')
    expect(new Uint8Array(await got!.blob.arrayBuffer())).toEqual(new Uint8Array([0xff, 0xd8, 1, 2, 3]))
    expect(got!.name).toBe('IMG_1.jpg')
  })

  it('reports an unreadable photo and identifies nothing', async () => {
    const log: string[] = []
    const state = { revoked: true }
    let error: unknown = null
    await takePhoto(fileInput(androidFile(log, state), log, state), () => log.push('identify'), err => (error = err))
    expect(log).toEqual(['read', 'clear'])
    expect(errorText(error)).toBe('NotReadableError: The requested file could not be read')
  })

  it('does nothing without a file', async () => {
    const log: string[] = []
    const state = { revoked: false }
    await takePhoto(fileInput(null, log, state), () => log.push('identify'), () => log.push('unreadable'))
    expect(log).toEqual(['clear'])
  })
})

describe('errorText', () => {
  it('keeps a plain message and names other errors', () => {
    expect(errorText(new Error('boom'))).toBe('boom')
    expect(errorText(new RangeError('Array buffer allocation failed'))).toBe('RangeError: Array buffer allocation failed')
    expect(errorText('text')).toBe('text')
  })
})
