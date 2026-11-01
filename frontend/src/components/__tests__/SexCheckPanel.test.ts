import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, h, nextTick } from 'vue'
import { t, tn } from '../../lib/i18n'
import type { RenameState } from '../../lib/renameHost'

// The «Fotos» tab's sex check: each CAM's d and v photos from the frame, to the server, and the flags shown.

const calls: { path: string; body?: Record<string, unknown> }[] = []
const answer = (cam: string, flag: string, more: Record<string, unknown> = {}) => ({
  cam,
  flag,
  reasons: [],
  model: { sex: 'male', p: 0.99, supported: true, status: 'species_supported' },
  envelope: null,
  database: { sex: 'male', rows: [{ sheet: 'Collection_data', row: 2, value: 'male' }] },
  species: 'Oleria baizana',
  ms: 500,
  ...more,
})
vi.mock('../../lib/api', () => ({
  api: async (path: string, options: { body?: Record<string, unknown> } = {}) => {
    calls.push({ path, body: options.body })
    const cam = options.body?.cam
    if (cam === 'CAM077001')
      return answer(cam, 'disagree', {
        reasons: ['model_vs_database', 'envelope_agrees_with_model'],
        envelope: { sex: 'male', conf: 0.97, sure: true, line: 'Oleria ♂' },
        database: { sex: 'female', rows: [{ sheet: 'Collection_data', row: 2, value: 'female' }] },
        suggest: 'male',
      })
    if (cam === 'CAM077002') return answer(cam, 'agree')
    return answer(String(cam), 'uncertain', { reasons: ['model_not_supported'], model: { sex: 'female', p: 0.78, supported: false, status: 'below_confidence' } })
  },
}))

const state = (): RenameState => ({
  folder: 'batch',
  canWrite: true,
  files: 7,
  reading: { on: false, percent: 100, message: '' },
  rows: [
    { file: 'IMG_1.JPG', id: 'CAM077001', view: 'd', review: [] },
    { file: 'IMG_2.JPG', id: 'CAM077001', view: 'v', review: [] },
    { file: 'IMG_3.JPG', id: 'CAM077002', view: 'd', review: [] },
    { file: 'IMG_4.JPG', id: 'CAM077002', view: 'v', review: [] },
    { file: 'IMG_5.JPG', id: 'CAM077003', view: 'v', review: [] },
    { file: 'IMG_6.JPG', id: 'CAM077003', view: 'd2', review: [] },
    { file: 'IMG_7.JPG', id: 'CAM077004', view: 'd', review: [], skip: true },
  ],
})

let unmount = () => {}
afterEach(() => {
  unmount()
  calls.length = 0
  document.body.innerHTML = ''
})

describe('the sex check of the Fotos tab', () => {
  it('pairs the first d and v photo of each CAM, without skipped photos or photos without a view', async () => {
    const { sexPairs } = await import('../../lib/sexCheck')
    const s = state()
    s.rows.push({ file: 'IMG_8.JPG', id: 'CAM077004', view: 'v', review: [] }, { file: 'IMG_9.JPG', id: 'cam077005', view: 'd', review: [] }, { file: 'IMG_10.JPG', id: 'CAM077005', view: 'v', review: [] })
    expect(sexPairs(s)).toEqual([
      { cam: 'CAM077001', d: 'IMG_1.JPG', v: 'IMG_2.JPG' },
      { cam: 'CAM077002', d: 'IMG_3.JPG', v: 'IMG_4.JPG' },
      { cam: 'CAM077005', d: 'IMG_9.JPG', v: 'IMG_10.JPG' },
    ])
    expect(sexPairs(null)).toEqual([])
  })

  it('sends each pair as the frame gives it, at 1600 px, and lists the disagreement first', async () => {
    const { renameFrame, renameHost } = await import('../../lib/renameHost')
    const asked: unknown[] = []
    renameFrame.ask = async (name, args) => {
      asked.push({ name, args })
      return { ok: true, data: { images: [{ files: [], blob: new Blob([new Uint8Array(200).fill(65)], { type: 'image/jpeg' }) }] } }
    }
    renameHost.state = state()
    renameHost.ready = true
    const { default: SexCheckPanel } = await import('../SexCheckPanel.vue')
    const host = document.createElement('div')
    document.body.append(host)
    const app = createApp({ render: () => h(SexCheckPanel) })
    app.config.globalProperties.$t = t
    app.config.globalProperties.$tn = tn
    app.mount(host)
    unmount = () => app.unmount()
    await nextTick()
    const run = host.querySelector<HTMLButtonElement>('[data-sex-run]')!
    expect(run.textContent).toContain(t('Comprobar {n} mariposas', { n: 2 }))
    run.click()
    for (let i = 0; i < 20 && !host.querySelector('[data-sex-list]'); i++) await new Promise(resolve => setTimeout(resolve, 5))
    await new Promise(resolve => setTimeout(resolve, 20))
    expect(asked).toEqual([
      { name: 'view', args: { files: ['IMG_1.JPG'], part: 'photo', side: 1600 } },
      { name: 'view', args: { files: ['IMG_2.JPG'], part: 'photo', side: 1600 } },
      { name: 'view', args: { files: ['IMG_3.JPG'], part: 'photo', side: 1600 } },
      { name: 'view', args: { files: ['IMG_4.JPG'], part: 'photo', side: 1600 } },
    ])
    expect(calls.map(c => [c.path, c.body?.cam, typeof c.body?.d, typeof c.body?.v])).toEqual([
      ['sex-check', 'CAM077001', 'string', 'string'],
      ['sex-check', 'CAM077002', 'string', 'string'],
    ])
    // Only the one to look at is listed; the agreement is counted.
    const rows = [...host.querySelectorAll('[data-sex-flag]')]
    expect(rows.map(r => r.getAttribute('data-cam'))).toEqual(['CAM077001'])
    expect(rows[0].textContent).toContain(t('El modelo (Supported) y el sobre dicen {m}, la base dice {db}: probablemente la base está mal.', { m: '♂', db: '♀' }))
    expect(host.querySelector('[data-sex-count="disagree"]')?.textContent).toBe(t('{n} no coinciden', { n: 1 }))
    expect(host.textContent).toContain(t('{n} coinciden', { n: 1 }))
    expect(host.querySelector('[data-sex-run]')?.textContent).toContain(t('Comprobar de nuevo'))
  })

  it('says a call that is not Supported as a leaning, not an error', async () => {
    const { flagText, modelText } = await import('../../lib/sexCheck')
    const a = answer('CAM077009', 'uncertain', { reasons: ['model_not_supported'], model: { sex: 'female', p: 0.78, supported: false, status: 'below_confidence' } }) as never
    expect(modelText(a)).toBe(t('{sign} {p} % · inseguro', { sign: '♀', p: 78 }))
    expect(flagText(a)).toBe(t('El modelo no está seguro (no es una llamada Supported): no se marca como error.'))
  })

  it('says a species the model is not trusted on as such', async () => {
    const { flagText, modelText } = await import('../../lib/sexCheck')
    const a = answer('CAM077010', 'uncertain', {
      species: 'Methona confusa',
      reasons: ['model_unreliable_species'],
      model: { sex: 'male', p: 0.97, supported: false, status: 'below_confidence', unreliable: 'Training labels were all male' },
    }) as never
    expect(modelText(a)).toBe(t('{sign} {p} % · no fiable en esta especie', { sign: '♂', p: 97 }))
    expect(flagText(a)).toBe(t('El modelo no es fiable para esta especie: no se marca como error.'))
  })
})
