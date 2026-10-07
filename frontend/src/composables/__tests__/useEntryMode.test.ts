import { beforeEach, describe, expect, it } from 'vitest'
import { nextTick } from 'vue'
import { useEntryMode } from '../useEntryMode'

beforeEach(() => localStorage.clear())

describe('cards or table', () => {
  it('cards by default; the choice per tab is kept in this browser, an unknown old one read as the default', async () => {
    expect(useEntryMode('deaths').mode.value).toBe('cards')
    localStorage.setItem('ithomiini:entry-mode:tubes', JSON.stringify('something old'))
    expect(useEntryMode('tubes').mode.value).toBe('cards')
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
