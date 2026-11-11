import { describe, expect, it } from 'vitest'
import { isCurrent, MODEL_ROLES, modelReady, sameFile, studentSizeOf, type ListedFile, type StoredFile } from '../offlineId'
import { PUBLIC_FILES } from '../publicModels'
import { ITHOMIINI_GENERA } from '../ithomiiniGenera'
import { PART, publicListing } from '../offlineEngine'

describe('the public (Hugging Face) models', () => {
  it('cover both models, pinned to a commit, one hash per 32 MB part', () => {
    for (const roles of Object.values(MODEL_ROLES)) for (const r of roles) expect(PUBLIC_FILES.some(f => f.role === r)).toBe(true)
    for (const f of PUBLIC_FILES) {
      expect(f.url).toMatch(/^https:\/\/huggingface\.co\/fr4nzzch\/[\w.-]+\/resolve\/[0-9a-f]{40}\//)
      expect(f.parts.length).toBe(Math.ceil(f.bytes / PART))
      for (const h of [f.sha256, ...f.parts]) expect(h).toMatch(/^[0-9a-f]{64}$/)
      // A file of one part: its part hash is the file's.
      if (f.parts.length === 1) expect(f.parts[0]).toBe(f.sha256)
    }
    expect(ITHOMIINI_GENERA).toContain('Mechanitis')
  })

  it('a model kept from the server counts as the public copy (same part hashes), and the other way round', () => {
    const pub = publicListing()
    const fromServer: Record<string, StoredFile> = Object.fromEntries(pub.map(f => [f.name, { etag: `"server-${f.name}"`, bytes: f.bytes, role: f.role, parts: f.parts }]))
    expect(modelReady('small', fromServer, pub as ListedFile[])).toBe(true)
    expect(modelReady('full', fromServer, pub as ListedFile[])).toBe(true)
    const server: ListedFile[] = pub.map(f => ({ name: f.name, etag: `"other-${f.name}"`, bytes: f.bytes, role: f.role, parts: f.parts }))
    const fromHf: Record<string, StoredFile> = Object.fromEntries(pub.map(f => [f.name, { etag: f.etag, bytes: f.bytes, role: f.role, parts: f.parts }]))
    expect(modelReady('small', fromHf, server)).toBe(true)
  })

  it('different bytes, or no hashes to compare, are not the same file', () => {
    const f = publicListing()[0] as ListedFile
    expect(sameFile({ etag: '"x"', bytes: f.bytes }, f)).toBe(false)
    expect(sameFile({ etag: '"x"', bytes: f.bytes, parts: ['0'.repeat(64)] }, f)).toBe(false)
    expect(sameFile({ etag: '"x"', bytes: f.bytes + 1, parts: f.parts }, f)).toBe(false)
    expect(sameFile({ etag: f.etag, bytes: f.bytes }, f)).toBe(true)
    expect(sameFile(undefined, f)).toBe(false)
  })
})

describe('the public small model is the server\'s current one (server/offline-id.mjs STUDENT: S_B, or L1 after a rollback)', () => {
  it('the same file, version, input size and note; a phone holding L1 from Hugging Face is offered S_B', async () => {
    const url = new URL('../../../../server/offline-id.mjs', import.meta.url).href
    const { STUDENT } = await import(/* @vite-ignore */ url)
    const small = publicListing().find(f => f.role === 'student')!
    expect({ name: small.name, sha256: small.sha256, version: small.version, input: small.input, notes: small.notes }).toEqual({
      name: STUDENT.name,
      sha256: STUDENT.sha256,
      version: STUDENT.version,
      input: STUDENT.input,
      notes: STUDENT.notes,
    })
    expect(small.url).toContain(`/fr4nzzch/butterfly-id-small-onnx/resolve/`)
    if (small.name === 'student_SB_int8.onnx') {
      expect(small.url).toContain('/resolve/fb2068d595546d1a42e6c0f780afb82500b238c7/')
      expect(small.notes?.es).toBe('Mejor en fotos nuevas de iNaturalist: acierta la especie en el 81,8 % frente al 79,4 % de la versión anterior.')
      // L1 as a signed-out phone kept it (its Hugging Face ETag and part hash, no version): not current, so an update.
      const l1: StoredFile = { etag: '"sha256-d6bdc3e5f8721b6ff707b4f46625cc8d8c9af86c0df757f9b2619aed1cc2f5fb"', bytes: 23565423, role: 'student', parts: ['d6bdc3e5f8721b6ff707b4f46625cc8d8c9af86c0df757f9b2619aed1cc2f5fb'] }
      expect(isCurrent(l1, small as ListedFile)).toBe(false)
      expect(studentSizeOf(l1, 'student_L1_int8.onnx')).toBe(192)
    }
  })
})

describe('reference photo credits', () => {
  it('link each licence to its Creative Commons deed and each photo to its iNaturalist page', async () => {
    const { licenseUrl, inatPhotoPage } = await import('../identifyRefs')
    expect(licenseUrl('CC-BY-NC-ND')).toBe('https://creativecommons.org/licenses/by-nc-nd/4.0/')
    expect(licenseUrl('CC-BY')).toBe('https://creativecommons.org/licenses/by/4.0/')
    expect(licenseUrl('CC0')).toBe('https://creativecommons.org/publicdomain/zero/1.0/')
    expect(licenseUrl('')).toBeNull()
    expect(inatPhotoPage('https://inaturalist-open-data.s3.amazonaws.com/photos/27132503/small.jpg')).toBe('https://www.inaturalist.org/photos/27132503')
  })
})
