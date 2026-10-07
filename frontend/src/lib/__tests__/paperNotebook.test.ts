import { beforeEach, describe, expect, it } from 'vitest'
import { locale } from '../i18n'
import {
  copyLine,
  filterPaper,
  highlightsCopy,
  highlightsSince,
  lineText,
  momentLabel,
  notebookCopy,
  shortDay,
  slashDay,
  sortPaper,
  type EnteredDeath,
  type NotebookLine,
} from '../paperNotebook'

const DAY = 46297 // 2 Oct 2026
const line = (id: string, row: number | null, entered: number | null, extra: Partial<NotebookLine> = {}): NotebookLine => ({
  recordId: `r-${id}`,
  id,
  row,
  species: 'Mechanitis polymnia',
  sex: 'female',
  entered,
  wild: false,
  census: null,
  death: null,
  todo: null,
  ...extra,
})
const census = (status: 'seen' | 'disappeared' | 'excluded', note = '') => ({ status, censusId: 'c', note, waiting: false })
const lines = [
  line('P6D', 12, 46280, { death: { date: 46294, cause: 'Unknown', staged: false }, todo: 'highlight' }),
  line('P4D', 10, 46282, {
    census: census('disappeared'),
    death: { date: DAY, cause: 'Disappearance', staged: true },
    todo: 'write',
  }),
  line('P5D', 11, 46282, { census: census('seen') }),
  line('Q1E', null, 46290, { census: census('seen'), staged: true }),
  line('P7D', 13, null, { census: census('excluded', 'Other cage') }),
  line('P8D', 14, 46270),
]

beforeEach(() => {
  locale.value = 'es'
})

describe('«Actualizar el cuaderno»', () => {
  it('days as the list and the notebook write them', () => {
    expect(shortDay(DAY)).toBe('2-Oct')
    expect(slashDay(46294)).toBe('29/9')
  })
  it('sorts in the notebook order (rows) or by emergence, ↑/↓; one with no pre-made row last', () => {
    expect(sortPaper(lines, { by: 'row', desc: false }).map(l => l.id)).toEqual(['P4D', 'P5D', 'P6D', 'P7D', 'P8D', 'Q1E'])
    expect(sortPaper(lines, { by: 'row', desc: true }).map(l => l.id)).toEqual(['Q1E', 'P8D', 'P7D', 'P6D', 'P5D', 'P4D'])
    // Oldest first; the same day by row; no emergence date last.
    expect(sortPaper(lines, { by: 'emergence', desc: false }).map(l => l.id)).toEqual(['P8D', 'P6D', 'P4D', 'P5D', 'Q1E', 'P7D'])
    expect(sortPaper(lines, { by: 'emergence', desc: true }).map(l => l.id)).toEqual(['Q1E', 'P5D', 'P4D', 'P6D', 'P8D', 'P7D'])
  })
  it('says what to do on paper and filters what has to be marked', () => {
    expect(lines.map(l => [l.id, lineText(l, DAY).mark, lineText(l, DAY).text])).toEqual([
      ['P6D', '▬', 'Ya muerta en la base: 29-Sep, Unknown'],
      ['P4D', '✗', 'Desapareció 2-Oct'],
      ['P5D', '☺', 'vista'],
      ['Q1E', '☺', 'vista'],
      ['P7D', '·', 'no contada: Other cage'],
      ['P8D', '·', 'viva'],
    ])
    // Seen, then dead: highlight, and the smiley is said.
    const later = line('P9D', 15, 46280, {
      census: census('seen'),
      death: { date: 'NA', cause: 'Natural', staged: false },
      todo: 'highlight',
    })
    expect(lineText(later, DAY).text).toBe('☺ vista · Ya muerta en la base: NA, Natural')
    expect(lineText(line('P0E', 16, 46280, { census: census('disappeared'), undone: true }), DAY).text).toBe(
      'viva (desaparición deshecha)',
    )
    expect(filterPaper(lines, true).map(l => l.id)).toEqual(['P6D', 'P4D'])
    expect(filterPaper(lines, false)).toHaveLength(6)
  })
  it('copies one line per butterfly in the order shown, in the notebook words', () => {
    const shown = sortPaper(filterPaper(lines, false), { by: 'row', desc: false })
    expect(notebookCopy(shown, DAY, 'Mechanitis polymnia · 2/10/26')).toBe(
      [
        'Mechanitis polymnia · 2/10/26',
        'P4D ✗ desapareció 2/10',
        'P5D ☺ vista',
        'P6D ▬ muerta 29/9 Unknown — resaltar',
        'P7D no contada: Other cage',
        'P8D viva',
        'Q1E ☺ vista',
      ].join('\n'),
    )
    expect(copyLine(line('X1A', 1, 1, { death: { date: null, cause: 'Eaten', staged: false }, todo: 'highlight' }), DAY)).toBe(
      'X1A ▬ muerta Eaten — resaltar',
    )
    locale.value = 'en'
    expect(copyLine(lines[1], DAY)).toBe('P4D ✗ disappeared 2/10')
    expect(copyLine(lines[0], DAY)).toBe('P6D ▬ dead 29/9 Unknown — highlight')
  })
})

describe('«Filas para resaltar»', () => {
  it('starts at the last time this person opened it, a day chosen, or the last 7 days', () => {
    expect(highlightsSince({ kind: 'last' }, '2026-10-05T19:12:00.000Z', '2026-10-06')).toBe('2026-10-05T19:12:00.000Z')
    expect(highlightsSince({ kind: 'last' }, null, '2026-10-06')).toBe('2026-09-30T05:00:00.000Z')
    expect(highlightsSince({ kind: 'week' }, '2026-10-05T19:12:00.000Z', '2026-10-06')).toBe('2026-09-30T05:00:00.000Z')
    // A day: from its start in Ecuador.
    expect(highlightsSince({ kind: 'day', day: '2026-10-01' }, null, '2026-10-06')).toBe('2026-10-01T05:00:00.000Z')
    // Shown in Ecuador's time, day first.
    expect(momentLabel('2026-10-06T03:30:00.000Z')).toBe('05/10/2026 22:30')
  })
  it('copies one line per butterfly', () => {
    const item = (id: string, date: number, cause: string): EnteredDeath => ({
      recordId: id,
      id,
      row: 1,
      species: '',
      sex: '',
      entered: null,
      death: { date, cause, staged: false },
      enteredAt: '2026-10-05T15:00:00.000Z',
      source: 'app',
      by: [],
    })
    expect(highlightsCopy([item('P6D', 46294, 'Unknown'), item('P7D', DAY, 'Disappearance')], 'Muertes')).toBe(
      'Muertes\nP6D ▬ muerta 29/9 Unknown — resaltar\nP7D ▬ muerta 2/10 Disappearance — resaltar',
    )
  })
})
