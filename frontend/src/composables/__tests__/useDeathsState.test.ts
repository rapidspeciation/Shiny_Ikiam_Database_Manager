import { beforeEach, describe, expect, it } from 'vitest'
import { nextTick } from 'vue'
import { createDeathsState, migrateDeathKeys, migrateGroupKeys, useDeathsState } from '../useDeathsState'

const put = (storage: Storage, key: string, value: unknown) => storage.setItem(`ithomiini:${key}`, JSON.stringify(value))
const got = (storage: Storage, key: string) => {
  const v = storage.getItem(`ithomiini:${key}`)
  return v === null ? undefined : JSON.parse(v)
}

beforeEach(() => {
  sessionStorage.clear()
  localStorage.clear()
})

describe('the IDs and choices Muertes keeps', () => {
  it('joins the table\'s and the cards\' old lists into one, without repeats, and drops the old keys', () => {
    put(sessionStorage, 'deaths:picked', ['A1D', 'A2D'])
    put(sessionStorage, 'deaths:phone-picked', ['a2d', 'B5E'])
    put(sessionStorage, 'deaths:phone-cause', 'Eaten')
    put(sessionStorage, 'deaths:not-preserved', true)
    put(sessionStorage, 'deaths:phone-preserved', true)
    put(localStorage, 'deaths:phone-medium', 'Ethanol')
    migrateDeathKeys()
    expect(got(sessionStorage, 'deaths:ids')).toEqual(['A1D', 'A2D', 'B5E'])
    expect(got(sessionStorage, 'deaths:cause')).toBe('Eaten')
    expect(got(sessionStorage, 'deaths:preserved')).toBe(true)
    expect(got(localStorage, 'deaths:medium')).toBe('Ethanol')
    for (const key of ['deaths:picked', 'deaths:phone-picked', 'deaths:phone-cause', 'deaths:not-preserved', 'deaths:phone-preserved'])
      expect(got(sessionStorage, key), key).toBeUndefined()
    expect(got(localStorage, 'deaths:phone-medium')).toBeUndefined()
    // Running again changes nothing.
    migrateDeathKeys()
    expect(got(sessionStorage, 'deaths:ids')).toEqual(['A1D', 'A2D', 'B5E'])
  })
  it('keeps a cause already chosen, takes the older "loaded" list, and reads "not preserved" reversed', () => {
    put(sessionStorage, 'deaths:cause', 'Spider')
    put(sessionStorage, 'deaths:phone-cause', 'Eaten')
    put(sessionStorage, 'deaths:loaded', ['C1D'])
    put(sessionStorage, 'deaths:not-preserved', false)
    migrateDeathKeys()
    expect(got(sessionStorage, 'deaths:cause')).toBe('Spider')
    expect(got(sessionStorage, 'deaths:ids')).toEqual(['C1D'])
    expect(got(sessionStorage, 'deaths:preserved')).toBe(true)
  })
  it('adds the old lists to the shared one when both exist', () => {
    put(sessionStorage, 'deaths:ids', ['D1D'])
    put(sessionStorage, 'deaths:phone-picked', ['D2D', 'D1D'])
    migrateDeathKeys()
    expect(got(sessionStorage, 'deaths:ids')).toEqual(['D1D', 'D2D'])
  })
  it('turns the group and the unfinished ones of 1–6 Oct into cards, each with its own values', () => {
    put(sessionStorage, 'deaths:ids', ['A1D', 'A2D'])
    put(sessionStorage, 'deaths:unfinished', ['B5E'])
    put(sessionStorage, 'deaths:cause', 'Eaten')
    put(sessionStorage, 'deaths:preserved', false)
    put(sessionStorage, 'deaths:own', { A2D: { cause: 'Spider' }, B5E: { preserved: true, note: 'Head eaten' } })
    migrateGroupKeys()
    const cards = got(sessionStorage, 'deaths:cards') as { id: string; choice: { cause: string; preserved: boolean; note: string } }[]
    expect(cards.map(c => [c.id, c.choice.cause, c.choice.preserved, c.choice.note])).toEqual([
      ['A1D', 'Eaten', false, ''],
      ['A2D', 'Spider', false, ''],
      ['B5E', 'Eaten', true, 'Head eaten'],
    ])
    expect(got(sessionStorage, 'deaths:defaults')).toEqual({ cause: 'Eaten', preserved: false, note: '' })
    for (const key of ['deaths:ids', 'deaths:unfinished', 'deaths:own', 'deaths:cause', 'deaths:preserved'])
      expect(got(sessionStorage, key), key).toBeUndefined()
  })
  it('starts empty, today, not preserved, Flash frozen', () => {
    const s = createDeathsState()
    expect(s.cards.value).toEqual([])
    expect(s.focus.value).toBeNull()
    expect(s.defaults.value.date).toMatch(/^\d{4}-\d{2}-\d{2}$/)
    expect(s.defaults.value.cause).toBe('')
    expect(s.defaults.value.preserved).toBe(false)
    expect(s.medium.value).toBe('Flash frozen')
  })
  it('is one state for both modes: the table\'s IDs and values are the cards\', and they survive a reload', async () => {
    const cards = useDeathsState()
    const table = useDeathsState()
    expect(table).toBe(cards)
    cards.cause.value = 'Unknown'
    cards.preserved.value = true
    cards.date.value = '2026-09-30'
    table.picked.value = ['E1D', 'E2D']
    cards.samples.E1D = { cam: 'CAM078001', tube: 'FS1' }
    expect(cards.cards.value.map(c => [c.id, c.choice.cause, c.choice.date])).toEqual([
      ['E1D', 'Unknown', '2026-09-30'],
      ['E2D', 'Unknown', '2026-09-30'],
    ])
    expect(table.defaults.value).toEqual({ date: '2026-09-30', cause: 'Unknown', preserved: true, note: '' })
    expect(table.samples.E1D.tube).toBe('FS1')
    await nextTick()
    const reloaded = createDeathsState()
    expect(reloaded.picked.value).toEqual(['E1D', 'E2D'])
    expect(reloaded.cause.value).toBe('Unknown')
    expect(reloaded.preserved.value).toBe(true)
    // The date starts at today again.
    expect(reloaded.date.value).not.toBe('2026-09-30')
  })
})
