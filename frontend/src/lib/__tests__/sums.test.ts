import { describe, expect, it } from 'vitest'
import { normalizeInput } from '../cells'
import { simpleSum } from '../sums'

describe('counts typed as sums', () => {
  it('keeps the terms of a sum as a formula', () => {
    expect(simpleSum('12+15')).toBe('=12+15')
    expect(simpleSum('= 41 + 36 + 2')).toBe('=41+36+2')
    expect(simpleSum('27')).toBeNull()
    expect(simpleSum('=A2+1')).toBeNull()
    expect(simpleSum('27-5')).toBe('=27-5')
  })
  it('only in the count columns of Insectary_stocks', () => {
    const eggs = { key: 'NUMBER OF EGGS', type: 'number' as const }
    expect(normalizeInput('12+15', eggs, 'Insectary_stocks')).toEqual({ ok: true, value: '=12+15' })
    expect(normalizeInput('=12+15', { key: 'Notes', type: 'text' as const }, 'Insectary_stocks').ok).toBe(false)
    const dissections = { key: 'NUMBER OF PUPAE/LARVAE FOR DISECTIONS', type: 'number' as const }
    expect(normalizeInput('2+6', dissections, 'Insectary_stocks')).toEqual({ ok: true, value: '=2+6' })
  })
})
