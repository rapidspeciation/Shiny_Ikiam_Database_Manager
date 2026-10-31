/**
 * Flash-frozen stock beside the offline identifier's top 5 (prueba-offline): the snapshot the server makes
 * (server/frozen-stock.mjs, GET /api/frozen-stock) and how a species' line reads.
 */

export interface StockCount {
  M: number
  F: number
  unknown: number
  total: number
}

export interface FrozenStock {
  version: string
  generatedAt: string
  species: Record<string, StockCount>
  genus: Record<string, StockCount>
}

/** Fewer males or females than this is marked (a subtle colour): worth keeping one more. */
export const LOW_STOCK = 5

export interface StockPart {
  text: string
  low?: boolean
}

/** «Congelados: 12 ♂ · 3 ♀ (1 sin sexo)» or «Congelados: ninguno», as parts (the low counts marked). */
export function speciesStock(stock: FrozenStock, name: string): StockPart[] {
  const c = stock.species[name]
  if (!c || !c.total) return [{ text: 'Congelados: ' }, { text: 'ninguno', low: true }]
  const out: StockPart[] = [{ text: 'Congelados: ' }, { text: `${c.M} ♂`, low: c.M < LOW_STOCK }, { text: ' · ' }, { text: `${c.F} ♀`, low: c.F < LOW_STOCK }]
  if (c.unknown) out.push({ text: ` (${c.unknown} sin sexo)` })
  return out
}

/** «Género Mechanitis: 1905 congelados», or null when the genus has none. */
export function genusStock(stock: FrozenStock, name: string): string | null {
  const genus = name.split(' ')[0]
  const c = stock.genus[genus]
  return c?.total ? `Género ${genus}: ${c.total} congelado${c.total === 1 ? '' : 's'}` : null
}

/** «Inventario congelado al 31 oct 2026, 02:15» (the last time the server confirmed it). */
export function stockDate(iso: string, locale = 'es-EC'): string {
  const d = new Date(iso)
  if (Number.isNaN(d.getTime())) return 'Inventario congelado: fecha desconocida'
  return `Inventario congelado al ${d.toLocaleString(locale, { day: 'numeric', month: 'short', year: 'numeric', hour: '2-digit', minute: '2-digit' })}`
}
