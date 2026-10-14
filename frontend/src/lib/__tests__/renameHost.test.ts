import { describe, expect, it } from 'vitest'
import { batchCounts, batchLine, blobBase64, type RenameState } from '../renameHost'

const state = (extra: Partial<RenameState> = {}): RenameState => ({
  folder: 'batch july 20',
  canWrite: true,
  files: 4,
  reading: { on: false, percent: 100, message: '' },
  rows: [
    { file: 'IMG_1.JPG', id: 'CAM077001', review: [] },
    { file: 'IMG_2.JPG', id: 'CAM077001', review: [], byHost: ['ID CAM077008 → CAM077001'] },
    { file: 'IMG_3.JPG', id: '', review: ['no-reading'] },
    { file: 'IMG_4.JPG', id: 'CAM077002', review: ['out-of-sequence'], skip: true },
  ],
  ...extra,
})

describe('the batch of the Renombrar fotos tab', () => {
  it('counts the photos waiting for a person, without the skipped ones', () => {
    expect(batchCounts(state())).toEqual({ photos: 4, waiting: 1, withoutId: 1, byAssistant: 1, renamed: 0 })
    expect(batchCounts(null)).toEqual({ photos: 0, waiting: 0, withoutId: 0, byAssistant: 0, renamed: 0 })
  })

  it('says the batch in a line', () => {
    expect(batchLine(null)).toBe('')
    expect(batchLine(state())).toBe('batch july 20 · 4 fotos · 1 por revisar · 1 cambiada por el asistente')
    expect(batchLine(state({ reading: { on: true, percent: 42, message: '' } }))).toBe('batch july 20 · leyendo 42%')
    expect(batchLine(state({ rows: [] }))).toBe('batch july 20 · 4 fotos sin leer')
    expect(batchLine(state({ rows: [{ file: 'a.JPG', id: 'CAM077001', review: [] }] }))).toBe('batch july 20 · 1 foto · ninguna por revisar')
  })

  it('turns a picture into text for the server', async () => {
    expect(await blobBase64(new Blob([new Uint8Array([255, 216, 255, 0, 1, 2])]))).toBe('/9j/AAEC')
  })
})
