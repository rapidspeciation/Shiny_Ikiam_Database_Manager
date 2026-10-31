import { describe, expect, it } from 'vitest'
import { genusStock, LOW_STOCK, speciesStock, stockDate, type FrozenStock } from '../frozenStock'

const stock: FrozenStock = {
  version: 'v',
  generatedAt: '2026-10-31T02:15:00Z',
  species: {
    'Mechanitis polymnia': { M: 12, F: 3, unknown: 1, total: 16 },
    'Oleria onega': { M: 24, F: 41, unknown: 0, total: 65 },
  },
  genus: { Mechanitis: { M: 900, F: 1000, unknown: 5, total: 1905 }, Greta: { M: 1, F: 0, unknown: 0, total: 1 } },
}
const line = (parts: { text: string }[]) => parts.map(p => p.text).join('')

describe('frozen stock lines', () => {
  it('reads as «Congelados: 12 ♂ · 3 ♀ (1 sin sexo)» with the low count marked', () => {
    const parts = speciesStock(stock, 'Mechanitis polymnia')
    expect(line(parts)).toBe('Congelados: 12 ♂ · 3 ♀ (1 sin sexo)')
    expect(parts.filter(p => p.low).map(p => p.text)).toEqual(['3 ♀'])
    expect(LOW_STOCK).toBe(5)
  })
  it('leaves out «sin sexo» when there is none and marks nothing above the threshold', () => {
    const parts = speciesStock(stock, 'Oleria onega')
    expect(line(parts)).toBe('Congelados: 24 ♂ · 41 ♀')
    expect(parts.some(p => p.low)).toBe(false)
  })
  it('says «ninguno» for a species without stock', () => {
    expect(line(speciesStock(stock, 'Greta andromica'))).toBe('Congelados: ninguno')
    expect(speciesStock(stock, 'Greta andromica').some(p => p.low)).toBe(true)
  })
  it('gives the genus total when there is one', () => {
    expect(genusStock(stock, 'Mechanitis lysimnia')).toBe('Género Mechanitis: 1905 congelados')
    expect(genusStock(stock, 'Greta andromica')).toBe('Género Greta: 1 congelado')
    expect(genusStock(stock, 'Oleria onega')).toBeNull()
  })
  it('dates the inventory', () => {
    expect(stockDate('2026-10-31T02:15:00Z')).toMatch(/^Inventario congelado al .*2026/)
    expect(stockDate('nope')).toBe('Inventario congelado: fecha desconocida')
  })
})
