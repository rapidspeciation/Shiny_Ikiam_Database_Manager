import { describe, expect, it } from 'vitest'
import { batchCounts, batchLine, blobBase64, folderName, spaceLine, stepOf, type RenameState } from '../renameHost'

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

  it("says the space left in the project's Drive, and whether the folder fits", () => {
    const drive = { usedGb: 69.7, limitGb: 100, freeGb: 30.3, percent: 70 }
    expect(spaceLine(null, null)).toBeNull()
    expect(spaceLine(drive, null)).toMatchObject({ level: 'ok', folderGb: null })
    expect(spaceLine(drive, 4 * 2 ** 30)?.text).toMatch(/^Drive del proyecto: 30[.,]3 GB libres de 100$/)
    expect(spaceLine({ ...drive, freeGb: 9, nearlyFull: true }, 4 * 2 ** 30)).toMatchObject({ level: 'low', folderGb: 4 })
    expect(spaceLine({ ...drive, freeGb: 9, nearlyFull: true }, 14.2 * 2 ** 30)).toMatchObject({ level: 'full', folderGb: 14.2 })
    // An account without a limit says nothing.
    expect(spaceLine({ usedGb: 5 }, null)).toBeNull()
  })

  it('knows the step a batch is at, from the folder to the sheets', () => {
    const read = { on: false, percent: 100, message: '' }
    expect(stepOf(null)).toBe('open')
    expect(stepOf(state({ files: 0, rows: [] }))).toBe('open')
    expect(stepOf(state({ rows: [] }))).toBe('read')
    expect(stepOf(state({ reading: { ...read, on: true } }))).toBe('read')
    // A photo waiting for a person, or without an ID.
    expect(stepOf(state())).toBe('review')
    const confirmed = [
      { file: 'a.JPG', id: 'CAM077001', review: [] },
      { file: 'b.JPG', id: 'CAM077001', review: [] },
      { file: 'c.JPG', id: '', review: ['no-reading'], skip: true },
    ]
    expect(stepOf(state({ rows: confirmed }))).toBe('rename')
    const renamed = confirmed.map(r => ({ ...r, status: 'Renamed' }))
    expect(stepOf(state({ rows: [renamed[0], confirmed[1]] }))).toBe('rename')
    expect(stepOf(state({ rows: renamed }))).toBe('upload')
    expect(stepOf(state({ rows: renamed }), true)).toBe('update')
    expect(folderName('Photos wings of collected butterflies/PERU_2024/JPG')).toBe('JPG')
  })

  it('turns a picture into text for the server', async () => {
    expect(await blobBase64(new Blob([new Uint8Array([255, 216, 255, 0, 1, 2])]))).toBe('/9j/AAEC')
  })
})
