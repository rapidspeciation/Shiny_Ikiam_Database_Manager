import { afterEach, describe, expect, it } from 'vitest'
import { isStaged } from '../../stores/pending'
import { setStagedSaving } from '../stagedSwitch'

describe('the staged-saving switch', () => {
  afterEach(() => setStagedSaving(true))
  it('on: Emergidos and Clutches changes are kept in the app; off: they go straight to the sheet', () => {
    expect(isStaged('emergidos')).toBe(true)
    expect(isStaged('clutches')).toBe(true)
    expect(isStaged('muertes')).toBe(false)
    setStagedSaving(false)
    expect(isStaged('emergidos')).toBe(false)
    expect(isStaged('clutches')).toBe(false)
    // A row entered in the app before the switch was turned off still goes through the app.
    expect(isStaged('emergidos', 'staged:abc')).toBe(true)
  })
})
