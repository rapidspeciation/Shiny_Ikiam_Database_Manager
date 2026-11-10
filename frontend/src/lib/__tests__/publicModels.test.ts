import { describe, expect, it } from 'vitest'
import { MODEL_ROLES, modelReady, sameFile, type ListedFile, type StoredFile } from '../offlineId'
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
