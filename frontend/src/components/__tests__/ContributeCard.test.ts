import { afterEach, beforeEach, describe, expect, it } from 'vitest'
import { createApp, h, nextTick } from 'vue'
import { t, tn } from '../../lib/i18n'
import ContributeCard, { type Labels, type Outcome } from '../identify/ContributeCard.vue'
import { listSource } from '../../lib/taxonPick'
import { CONSENT_VERSION } from '../../lib/contribConfig'

// «Ayude a entrenar la IA»: the labels it sends, the consent the first time, «Deshacer».

let host: HTMLElement | null = null
const sent: Labels[] = []
let undone = 0
function show(props: Partial<{ shown: { name: string; rank: 'genus' | 'species' }; pick: string | null; hasGps: boolean; outcome: Outcome }> = {}) {
  host = document.createElement('div')
  document.body.append(host)
  const app = createApp({
    render: () =>
      h(ContributeCard, {
        shown: props.shown ?? { name: 'Mechanitis polymnia', rank: 'species' },
        pick: props.pick ?? null,
        source: listSource(['Mechanitis polymnia', 'Mechanitis lysimnia', 'Oleria onega']),
        hasGps: props.hasGps ?? false,
        send: async (l: Labels) => (sent.push(l), props.outcome ?? 'sent'),
        undo: async () => void undone++,
      }),
  })
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  app.mount(host)
  return host
}
const $ = (sel: string) => host!.querySelector<HTMLElement>(sel)
const click = async (sel: string) => {
  $(sel)!.click()
  await nextTick()
}
beforeEach(() => {
  sent.length = 0
  undone = 0
  localStorage.setItem('identify:contrib-consent', CONSENT_VERSION)
})
afterEach(() => {
  host?.remove()
  host = null
  localStorage.clear()
})

describe('ContributeCard', () => {
  it('«Sí, es esta» + Hembra + en la mano: the confirmed species and the sex go', async () => {
    show()
    expect(($('[data-testid=contrib-send]') as HTMLButtonElement).disabled).toBe(true)
    await click('[data-testid=species-yes]')
    await click('[data-testid=sex-female]')
    // A sex label needs the kind of photo.
    expect(($('[data-testid=contrib-send]') as HTMLButtonElement).disabled).toBe(true)
    await click('[data-testid=kind-hand]')
    await click('[data-testid=contrib-send]')
    await nextTick()
    expect(sent).toEqual([{ species: { name: 'Mechanitis polymnia', rank: 'species', confirmed: true, freeText: false }, sex: 'female', photoKind: 'hand', includeGps: false }])
    expect($('[data-testid=contrib-done]')!.textContent).toMatch(/Sent|Enviada/)
  })

  it('«Es otra especie» opens on the person\'s pick; a name no list knows is free text; «No sé» sends no sex', async () => {
    show({ pick: 'Oleria onega' })
    expect(($('[data-testid=taxon-input]') as HTMLInputElement).value).toBe('Oleria onega')
    await click('[data-testid=sex-unknown]')
    await click('[data-testid=contrib-send]')
    expect(sent[0]).toMatchObject({ species: { name: 'Oleria onega', rank: 'species', confirmed: false, freeText: false }, sex: null })
    host!.remove()
    show()
    await click('[data-testid=species-other]')
    const input = $('[data-testid=taxon-input]') as HTMLInputElement
    input.value = 'Napeogenes sp. 2'
    input.dispatchEvent(new Event('input'))
    await nextTick()
    expect($('[data-testid=taxon-unknown]')).not.toBeNull()
    await click('[data-testid=contrib-send]')
    expect(sent[1].species).toEqual({ name: 'Napeogenes sp. 2', rank: 'subspecies', confirmed: false, freeText: true })
  })

  it('a genus-only result asks for the genus', async () => {
    show({ shown: { name: 'Mechanitis', rank: 'genus' } })
    expect(host!.textContent).toMatch(/genus|género/)
    await click('[data-testid=species-yes]')
    await click('[data-testid=contrib-send]')
    expect(sent[0].species).toMatchObject({ name: 'Mechanitis', rank: 'genus', confirmed: true })
  })

  it('the first time, the consent sheet (marked as a draft) before anything is sent', async () => {
    localStorage.clear()
    show()
    await click('[data-testid=species-yes]')
    await click('[data-testid=contrib-send]')
    expect(sent).toHaveLength(0)
    expect($('[data-testid=consent-draft]')).not.toBeNull()
    await click('[data-testid=consent-accept]')
    await nextTick()
    expect(sent).toHaveLength(1)
    expect(localStorage.getItem('identify:contrib-consent')).toBe(CONSENT_VERSION)
  })

  it('without signal it says it waits on the phone; «Deshacer» takes it back', async () => {
    show({ outcome: 'queued', hasGps: true })
    await click('[data-testid=species-yes]')
    ;($('[data-testid=contrib-gps]') as HTMLInputElement).click()
    await nextTick()
    await click('[data-testid=contrib-send]')
    await nextTick()
    expect(sent[0].includeGps).toBe(true)
    expect($('[data-testid=contrib-done]')!.textContent).toMatch(/signal|señal/)
    await click('[data-testid=contrib-undo]')
    await nextTick()
    expect(undone).toBe(1)
    expect($('[data-testid=contrib-send]')).not.toBeNull()
  })
})
