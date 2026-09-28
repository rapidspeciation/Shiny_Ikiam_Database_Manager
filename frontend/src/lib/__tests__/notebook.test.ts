import { describe, expect, it } from 'vitest'
import {
  bands,
  cellClass,
  cellTitle,
  choicesFor,
  fitSize,
  jobState,
  kindForSheet,
  lineState,
  mergeEdits,
  nextJob,
  warningText,
  type Job,
  type ReviewCell,
  type ReviewLine,
} from '../notebook'

const cell = (over: Partial<ReviewCell> = {}): ReviewCell => ({
  value: 'female',
  before: null,
  status: 'fill',
  confidence: 1,
  doubt: false,
  alternatives: [],
  edited: false,
  include: true,
  formula: false,
  message: null,
  ...over,
})
const job = (over: Partial<Job>): Job => ({
  id: 'a',
  status: 'ready',
  requestedKind: 'auto',
  kind: 'stocks',
  label: 'Posturas',
  sheet: 'Insectary_stocks',
  attachmentId: 'x',
  name: 'p.jpg',
  owner: 'Franz',
  error: null,
  createdAt: '2026-09-28T10:00:00Z',
  updatedAt: '2026-09-28T10:00:00Z',
  durationMs: 40000,
  model: 'sonnet',
  threadId: null,
  proposalId: null,
  proposalStatus: null,
  lines: 3,
  keys: [],
  appliedLines: [],
  warnings: [],
  ...over,
})

describe('photos', () => {
  it('shrinks the long side to 2400 px and never enlarges', () => {
    expect(fitSize(4000, 3000)).toEqual({ width: 2400, height: 1800 })
    expect(fitSize(3000, 4000)).toEqual({ width: 1800, height: 2400 })
    expect(fitSize(1280, 960)).toEqual({ width: 1280, height: 960 })
  })
})

describe('bands on the photo', () => {
  it('reach half-way to the neighbouring lines', () => {
    const [a, b, c] = bands([
      { n: 1, y: 0.2 },
      { n: 2, y: 0.26 },
      { n: 3, y: 0.32 },
    ])
    expect(a.bottom).toBeCloseTo(0.23)
    expect(b.top).toBeCloseTo(0.23)
    expect(b.bottom).toBeCloseTo(0.29)
    expect(c.bottom).toBeCloseTo(0.35)
  })
  it('place a line without a position between its neighbours, and are capped in height', () => {
    const out = bands([
      { n: 1, y: 0.2 },
      { n: 2, y: null },
      { n: 3, y: 0.3 },
      { n: 4, y: null },
    ])
    expect((out[1].top + out[1].bottom) / 2).toBeCloseTo(0.25)
    expect((out[3].top + out[3].bottom) / 2).toBeCloseTo(0.35)
    expect(bands([{ n: 1, y: null }])).toEqual([])
    const lone = bands([{ n: 1, y: 0.5 }])[0]
    expect(lone.bottom - lone.top).toBeLessThanOrEqual(0.08)
  })
})

describe('cells', () => {
  it('are coloured by what the page would do to the sheet', () => {
    expect(cellClass(cell())).toBe('nb-fill')
    expect(cellClass(cell({ status: 'conflict', before: 'male' }))).toBe('nb-conflict')
    expect(cellClass(cell({ status: 'conflict', doubt: true }))).toBe('nb-doubt')
    expect(cellClass(cell({ status: 'error' }))).toBe('nb-error')
    expect(cellClass(cell({ status: 'same' }))).toBe('')
    expect(cellClass(cell({ status: 'formula' }))).toBe('nb-formula')
  })
  it('say both values of a difference, with the sheet first', () => {
    const c = cell({ status: 'conflict', before: 45873, value: 45875, doubt: true, confidence: 0.5, alternatives: [45876] })
    const title = cellTitle('Death_date', c, 'date')
    expect(title).toContain('hoja: 4-Aug-25 → cuaderno: 6-Aug-25')
    expect(title).toContain('Lectura dudosa (50 %)')
    expect(title).toContain('Otras lecturas: 7-Aug-25')
  })
  it('offer the reading, the other readings, the sheet value and then the list', () => {
    const c = cell({ value: 'female', alternatives: ['male'], before: 'NA', status: 'conflict' })
    expect(choicesFor(c, 'text', ['female', 'male', 'NA', 'NOT_COLLECTED'])).toEqual(['female', 'male', 'NA', 'NOT_COLLECTED'])
    expect(choicesFor(cell({ value: 45873 }), 'date')).toEqual(['4-Aug-25'])
  })
})

describe('lines and pages', () => {
  const line = (over: Partial<ReviewLine>): ReviewLine => ({
    n: 1,
    y: 0.2,
    raw: '',
    crossed: false,
    status: 'match',
    message: '',
    recordId: 'r',
    row: 10932,
    label: '5VB',
    cells: {},
    changes: 2,
    picked: true,
    applied: false,
    ...over,
  })
  it('state where each line goes in the sheet', () => {
    expect(lineState(line({}))).toEqual({ text: 'fila 10932', tone: 'ok' })
    expect(lineState(line({ status: 'new', row: null }))).toEqual({ text: 'nueva', tone: 'new' })
    expect(lineState(line({ status: 'missing' })).tone).toBe('bad')
    expect(lineState(line({ applied: true, changes: 0 })).text).toBe('aplicada')
  })
  it('state what a page is doing', () => {
    const now = Date.parse('2026-09-28T10:00:30Z')
    expect(jobState(job({ status: 'reading' }), now).text).toBe('Leyendo… 30 s')
    expect(jobState(job({ counts: { lines: 3, rows: 1, fills: 1, conflicts: 0, doubts: 0, errors: 0, created: 0, same: 4 } })).text).toBe(
      'Lista · 1 fila con cambios',
    )
    expect(jobState(job({ status: 'done', appliedLines: [1, 2] })).text).toBe('Aplicada (2 filas)')
  })
  it('go to the next open page, round to the first', () => {
    const jobs = [job({ id: 'a' }), job({ id: 'b', status: 'done' }), job({ id: 'c', status: 'reading' })]
    expect(nextJob(jobs, 'a')).toBe('c')
    expect(nextJob(jobs, 'c')).toBe('a')
    expect(nextJob([job({ id: 'a' })], 'a')).toBeNull()
  })
  it('warn about a repeated photo or clutches already digitized', () => {
    expect(warningText({ kind: 'photo', jobId: 'x', at: '2026-09-27T15:00:00Z', by: 'Franz' })).toMatch(/^Esta foto ya se subió el .* por Franz\.$/)
    expect(warningText({ kind: 'keys', jobId: 'x', at: '2026-09-27T15:00:00Z', keys: ['994(1)', '994(2)'], count: 5 })).toMatch(
      /^5 líneas ya estaban en otra página .*: 994\(1\), 994\(2\)…$/,
    )
  })
  it('send a paste or a fill as one set of corrections', () => {
    const edits = {}
    mergeEdits(edits, 3, 'Sex', 'male')
    mergeEdits(edits, 3, 'Death_date', '7/8')
    mergeEdits(edits, 4, 'Sex', null)
    expect(edits).toEqual({ 3: { Sex: 'male', Death_date: '7/8' }, 4: { Sex: null } })
  })
  it('open the notebook of the sheet shown in Tablas', () => {
    expect(kindForSheet('Insectary_stocks')).toBe('stocks')
    expect(kindForSheet('Collection_data')).toBe('auto')
  })
})
