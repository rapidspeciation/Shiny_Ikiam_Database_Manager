import { beforeEach, describe, expect, it } from 'vitest'
import { nextTick } from 'vue'
import { DEFAULT_MODE, choiceFor, modeFor, useEntryMode } from '../useEntryMode'

beforeEach(() => localStorage.clear())

describe('cards or table', () => {
  it('cards by default on every device, a choice stored before is kept', () => {
    expect(DEFAULT_MODE).toBe('cards')
    expect(modeFor(null)).toBe('cards')
    expect(modeFor(undefined)).toBe('cards')
    expect(modeFor('something old')).toBe('cards')
    expect(modeFor('table')).toBe('table')
    expect(modeFor('cards')).toBe('cards')
    expect(choiceFor('cards')).toBeNull()
    expect(choiceFor('table')).toBe('table')
  })
  it('reads and remembers the choice per tab in this browser', async () => {
    expect(useEntryMode('deaths').mode.value).toBe('cards')
    localStorage.setItem('ithomiini:entry-mode:clutches', JSON.stringify('table'))
    const clutches = useEntryMode('clutches')
    expect(clutches.mode.value).toBe('table')
    clutches.mode.value = 'cards'
    await nextTick()
    expect(localStorage.getItem('ithomiini:entry-mode:clutches')).toBe('null')
    const deaths = useEntryMode('deaths')
    deaths.mode.value = 'table'
    await nextTick()
    expect(localStorage.getItem('ithomiini:entry-mode:deaths')).toBe('"table"')
    expect(useEntryMode('deaths').mode.value).toBe('table')
  })
})
