import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, h, nextTick } from 'vue'
import { t, tn } from '../../lib/i18n'
import type { Proposal } from '../../lib/proposals'
import { cardChoices, curationChips, curationChange, type CurationEntry } from '../../lib/photoCuration'

// The photo archive review card: cases always open with their evidence, the sure ones ticked, one «Aplicar».

const posts: { path: string; body: unknown }[] = []
vi.mock('../../lib/api', () => ({
  api: async (path: string, options: { body?: unknown } = {}) => {
    posts.push({ path, body: options.body })
    return { status: 'partial', written: 1, kept: 1, planned: 1, entries: undefined }
  },
}))
vi.mock('../assistant/PhotoViewer.vue', async () => {
  const { defineComponent } = await import('vue')
  return {
    default: defineComponent({
      props: { count: Number, marks: Array, photo: Number },
      setup: p => () => h('div', { 'data-viewer': '', 'data-mark': JSON.stringify(p.marks?.[p.photo ?? 0] ?? null) }),
    }),
  }
})

const turn: CurationEntry = { id: 'photo_turn:CAM1D', kind: 'turn', cam: 'CAM1', tier: 'clear', checked: true, conf: 0.97, evidence: { photo: 'CAM1d', turn: 'upside down' }, fix: { drive: [{ action: 'rotate', degrees: 180 }] }, photos: { files: [{ id: 'f1', name: 'CAM1d' }] } }
const camid: CurationEntry = {
  id: 'photo_camid:CAM2', kind: 'camid', cam: 'CAM2', tier: 'clear', checked: true, why: ['idEverywhere', 'speciesFitsTarget'],
  evidence: { file: 'CAM2', envelope: ['CAM9', 0.99, 2, 2], target: 'Heliconius sara', model: ['Heliconius sara', 0.94] },
  fix: { to: 'CAM9', drive: [{ action: 'rename', name: 'CAM9d', from: 'CAM2d' }] },
  photos: { files: [{ id: 'f2', name: 'CAM2d' }] }, idLines: [{ fileId: 'f2', name: 'CAM2d', read: 'CAM9', box: [0.1, 0.2, 0.4, 0.3], aspect: 1.5 }],
}
const sex: CurationEntry = { id: 'ai_sex:CAM3', kind: 'aisex', cam: 'CAM3', tier: 'look', evidence: { database: 'female', model: ['male', 0.95] }, fix: { field: 'Sex', from: 'female', to: 'male' } }

let unmount = () => {}
afterEach(() => {
  unmount()
  posts.length = 0
  cardChoices.clear()
  document.body.innerHTML = ''
})

async function mount(entries: CurationEntry[]) {
  const { default: PhotoCurationCard } = await import('../assistant/PhotoCurationCard.vue')
  const host = document.createElement('div')
  document.body.append(host)
  const proposal = { id: 'card-1', kind: 'photo_curation', reason: '', fields: [], types: {}, status: 'pending', changes: [], applied: null, sync: { type: 'curation', title: '', entries, totalInSection: 90, offset: 0, filters: { kind: 'camid' } } } as unknown as Proposal
  const app = createApp({ render: () => h(PhotoCurationCard, { proposal }) })
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  app.mount(host)
  unmount = () => app.unmount()
  await nextTick()
  return { host }
}
const checkbox = (host: HTMLElement, id: string) => host.querySelector<HTMLInputElement>(`[data-entry="${id}"] input[type=checkbox]`)!

describe('PhotoCurationCard', () => {
  it('groups the cases, ticks the sure ones and opens the first one in the viewer, its ID line outlined', async () => {
    const { host } = await mount([turn, camid, sex])
    expect([...host.querySelectorAll('[data-group]')].map(g => g.getAttribute('data-group'))).toEqual(['clear', 'look'])
    expect(checkbox(host, turn.id).checked).toBe(true)
    expect(checkbox(host, sex.id).checked).toBe(false)
    expect(host.textContent).toContain('1–3 de 90')
    // The camid case: its ID line crop, and its photo opened on the line read.
    host.querySelector<HTMLElement>(`[data-entry="${camid.id}"]`)!.click()
    await nextTick()
    expect(JSON.parse(host.querySelector('[data-viewer]')!.getAttribute('data-mark')!)).toEqual([0.1, 0.2, 0.4, 0.3])
    expect(host.querySelector(`[data-entry="${camid.id}"] img[alt^="Línea del ID"]`)).not.toBeNull()
  })

  it('keys move, tick and mark a wrong reading; one «Aplicar» sends what was chosen', async () => {
    const { host } = await mount([turn, camid, sex])
    const list = host.querySelector<HTMLElement>('[data-rows]')!
    const key = (k: string) => list.dispatchEvent(new KeyboardEvent('keydown', { key: k, bubbles: true }))
    key('ArrowDown') // camid
    key('ArrowDown') // sex
    key(' ') // ticked
    key('ArrowUp')
    key('x') // camid: a wrong reading, no longer ticked
    await nextTick()
    expect(checkbox(host, camid.id).checked).toBe(false)
    expect(host.querySelector('[data-apply-curation]')!.textContent).toContain('Aplicar 3')
    host.querySelector<HTMLButtonElement>('[data-apply-curation]')!.click()
    await new Promise(resolve => setTimeout(resolve, 0))
    expect(posts[0]).toMatchObject({ path: 'chat/proposals/card-1/curation/apply', body: { accept: [turn.id, sex.id], keep: [camid.id] } })
  })

  it('what was ticked stays when the card is drawn again', async () => {
    const first = await mount([turn, camid, sex])
    checkbox(first.host, turn.id).click()
    await nextTick()
    unmount()
    const again = await mount([turn, camid, sex])
    expect(checkbox(again.host, turn.id).checked).toBe(false)
    expect(checkbox(again.host, camid.id).checked).toBe(true)
  })

  it('the group buttons tick and untick all of a group', async () => {
    const { host } = await mount([turn, camid, sex])
    host.querySelector<HTMLButtonElement>('[data-group="clear"] button:last-child')!.click()
    await nextTick()
    expect(checkbox(host, turn.id).checked).toBe(false)
    expect(checkbox(host, camid.id).checked).toBe(false)
    expect(host.querySelector<HTMLButtonElement>('[data-apply-curation]')!.disabled).toBe(true)
  })
})

describe('a case in words', () => {
  it('the change and the evidence, green where a source supports it, amber where it argues against it', () => {
    expect(curationChange(camid)).toMatchObject({ from: 'CAM2', to: 'CAM9' })
    expect(curationChips(camid).map(c => c.tone)).toEqual(['ok', 'ok', 'info'])
    expect(curationChips(camid)[0].text).toBe('Sobre: CAM9 · 99 % · 2 de 2 fotos')
    expect(curationChips({ ...camid, why: ['ownIdAlsoRead', 'targetHasPhotos'] }).filter(c => c.tone === 'against').map(c => c.text)).toEqual([
      'El sobre también lee CAM2',
      'CAM9 ya tiene fotos',
    ])
    // The model sure of another species than the CAM read argues against it; unsure, it only informs.
    const other = (p: number) => curationChips({ ...camid, evidence: { ...camid.evidence, model: ['Oleria gunilla', p] } })[1].tone
    expect([other(0.9), other(0.3)]).toEqual(['against', 'info'])
    // The sex model beside the database sex of the CAM read and of the file's: green when it fits the one read only,
    // amber when it fits the file's only, informing when both have its sex or one is unknown.
    const sexChip = (sex: unknown[]) => curationChips({ ...camid, evidence: { ...camid.evidence, sex } }).find(c => c.text.startsWith('Modelo de sexo'))!
    expect(sexChip(['female', 0.97, 'female', 'male'])).toEqual({ text: 'Modelo de sexo: hembra · 97 % · = sexo de CAM9 · ≠ macho de CAM2', tone: 'ok' })
    expect(sexChip(['male', 0.95, 'female', 'male']).tone).toBe('against')
    expect([sexChip(['male', 0.95, 'male', 'male']).tone, sexChip(['male', 0.95, null, 'female']).tone]).toEqual(['info', 'info'])
    expect(sexChip(['female', 0.95, 'female', null])).toEqual({ text: 'Modelo de sexo: hembra · 95 % · = sexo de CAM9', tone: 'info' })
    expect(curationChips(camid).some(c => c.text.startsWith('Modelo de sexo'))).toBe(false)
    expect(curationChange(sex)).toMatchObject({ from: 'hembra', to: 'macho' })
    const side: CurationEntry = { id: 's', kind: 'side', cam: 'CAM4', tier: 'clear', evidence: { photos: [['CAM4d', 'ventral', 0.01], ['CAM4v', 'dorsal', 0.98]] }, fix: { drive: [{ action: 'rename' }] } }
    expect(curationChips(side).map(c => c.text)).toEqual(['d: alas ventral · 99 %', 'v: alas dorsal · 98 %'])
  })
})

describe('the envelope beside the database', () => {
  it('one line per field, amber where both have a value and they disagree; a date matched to its kind', async () => {
    const { recordLines, notesOnCase } = await import('../../lib/photoCuration')
    const r = {
      cam: 'CAM5',
      envelope: { camid: ['CAM5', 0.99] as [string, number], species: ['Mechanitis lysimnia', 0.9] as [string, number], sex: ['female', 0.8] as [string, number], dates: [['date', '2022-11-21', 0.95], ['preserved', '2022-11-29', 0.9]] as [string, string, number][] },
      database: [{ sheet: 'Collection_data', row: 9, species: 'Mechanitis polymnia', sex: 'female', locality: 'Ikiam', dates: { collected: '2022-11-21', preserved: '2022-11-30' }, notes: [{ field: 'species' as const, from: 'Mechanitis lysimnia', to: 'Mechanitis polymnia', text: 'changed' }] }],
    }
    expect(recordLines(r).map(l => [l.label, l.envelope, l.database, l.differ])).toEqual([
      ['CAM', 'CAM5 · 99 %', 'CAM5', false],
      ['Especie', 'Mechanitis lysimnia · 90 %', 'Mechanitis polymnia', true],
      ['Sexo', 'hembra · 80 %', 'hembra', false],
      ['Localidad', '', 'Ikiam', false],
      ['Fecha', '21/11/2022 · 95 %', '21/11/2022', false],
      ['Preservación', '29/11/2022 · 90 %', '30/11/2022', true],
    ])
    // A short place name the database writes longer is the same place; another place is not.
    const placed = (envelope: string, database: string) =>
      recordLines({ cam: 'CAM5', envelope: { locality: [envelope, 1] }, database: [{ ...r.database[0], locality: database }] }).find(l => l.label === 'Localidad')!.differ
    expect([placed('Arajuno', 'San Pedro de Arajuno 2'), placed('Tamiayura', 'Tamia Yura'), placed('San Isidro, Cosanga', 'San Isidro, Macas')]).toEqual([false, false, true])
    // A short name that fits several places (kept as written, the places listed): any of them agrees, even misread.
    const options = (envelope: string, database: string) =>
      recordLines({ cam: 'CAM5', envelope: { locality: [envelope, 0.92], localityOptions: ['San Isidro, Cosanga', 'San Isidro, Macas'] }, database: [{ ...r.database[0], locality: database }] }).find(l => l.label === 'Localidad')!
    expect(options('San Isidro', 'San Isidro, Macas')).toMatchObject({ envelope: 'San Isidro · 92 % · una de: San Isidro, Cosanga / San Isidro, Macas', differ: false })
    expect([options('San IKidro', 'San Isidro, Macas').differ, options('San IKidro', 'Ikiam').differ]).toEqual([false, true])
    // The note changed the species from what the envelope reads: the envelope is out of date.
    const species = { id: 'x', kind: 'species' as const, cam: 'CAM5', tier: 'look' as const, evidence: {}, records: [r] }
    expect(notesOnCase(species).map(n => n.verdict)).toEqual(['outdated'])
    expect(notesOnCase({ ...species, kind: 'sex' })).toEqual([])
    const read = { ...r, envelope: { ...r.envelope, species: ['Mechanitis polymnia', 0.9] as [string, number] } }
    expect(notesOnCase({ ...species, records: [read] }).map(n => n.verdict)).toEqual(['supports'])
    // A reared butterfly's envelope: its plain date is the day it died, a wing clip the day it came in.
    const reared = {
      cam: 'CAM6',
      envelope: { code: ['F77', 1] as [string, number], dates: [['date', '2024-03-02', 0.9], ['wingClip', '2024-01-10', 0.9]] as [string, string, number][] },
      database: [{ sheet: 'Insectary_data', row: 3, species: null, sex: null, locality: null, dates: { intoInsectary: '2024-01-10', death: '2024-03-02', preserved: '2024-03-02' } }],
    }
    expect(recordLines(reared).filter(l => l.envelope).map(l => [l.label, l.database, l.differ])).toEqual([
      ['Muerte', '02/03/2024', false],
      ['Corte de ala', '10/01/2024', false],
    ])
  })
})
