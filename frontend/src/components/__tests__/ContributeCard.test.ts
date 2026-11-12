import { afterEach, beforeEach, describe, expect, it } from 'vitest'
import { createApp, h, nextTick, ref } from 'vue'
import { t, tn } from '../../lib/i18n'
import ContributeCard, { type Labels, type Outcome } from '../identify/ContributeCard.vue'
import { listSource } from '../../lib/taxonPick'
import { CONSENT_VERSION } from '../../lib/contribConfig'

// «Ayude a entrenar la IA»: the labels it sends, the consent the first time, «Deshacer».

let host: HTMLElement | null = null
const sent: Labels[] = []
let undone = 0
let consentedCalls = 0
const pickNow = ref<string | null>(null)
function show(props: Partial<{ shown: { name: string; rank: 'genus' | 'species' }; pick: string | null; contact: string | null; outcome: Outcome }> = {}) {
  host = document.createElement('div')
  document.body.append(host)
  const app = createApp({
    render: () =>
      h(ContributeCard, {
        shown: props.shown ?? { name: 'Mechanitis polymnia', rank: 'species' },
        pick: props.pick !== undefined ? props.pick : pickNow.value,
        source: listSource(['Mechanitis polymnia', 'Mechanitis lysimnia', 'Oleria onega']),
        contact: props.contact ?? null,
        onConsented: () => void consentedCalls++,
        send: async (l: Labels) => (sent.push(l), props.outcome ?? 'sent'),
        refusal: 'labels.species.name is not a taxon name',
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
  pickNow.value = null
  sent.length = 0
  undone = 0
  consentedCalls = 0
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
    expect(sent).toEqual([{ species: { name: 'Mechanitis polymnia', rank: 'species', confirmed: true, freeText: false }, sex: 'female', photoKind: 'hand' }])
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

  it('the first time, the consent sheet v2 (no draft mark, no location, Franz Chandi with the server\'s address) before anything is sent', async () => {
    localStorage.clear()
    show({ contact: 'person@example.org' })
    await click('[data-testid=species-yes]')
    await click('[data-testid=contrib-send]')
    expect(sent).toHaveLength(0)
    const sheet = $('[data-testid=consent-sheet]')!.textContent!
    expect(sheet).not.toMatch(/BORRADOR|DRAFT|IKIAM|experto|expert/i)
    expect(sheet).toMatch(/GPS/)
    expect(sheet).not.toMatch(/Incluir la ubicación|Include the photo's location|1 km/)
    expect($('[data-testid=consent-contact]')!.textContent).toMatch(/Franz Chandi.*person@example\.org/)
    await click('[data-testid=consent-accept]')
    await nextTick()
    expect(sent).toHaveLength(1)
    expect(consentedCalls).toBe(1)
    expect(localStorage.getItem('identify:contrib-consent')).toBe(CONSENT_VERSION)
  })

  it('someone who accepted v1 sees v2 once and must accept it again; «Ahora no» sends nothing', async () => {
    expect(CONSENT_VERSION).toBe('v2')
    localStorage.setItem('identify:contrib-consent', 'v1-draft')
    show()
    await click('[data-testid=species-yes]')
    await click('[data-testid=contrib-send]')
    expect($('[data-testid=consent-sheet]')).not.toBeNull()
    await click('[data-testid=consent-later]')
    expect(sent).toHaveLength(0)
    expect(localStorage.getItem('identify:contrib-consent')).toBe('v1-draft')
    await click('[data-testid=contrib-send]')
    await click('[data-testid=consent-accept]')
    await nextTick()
    expect(sent).toHaveLength(1)
    host!.remove()
    // Once accepted, not again.
    show()
    await click('[data-testid=species-yes]')
    await click('[data-testid=contrib-send]')
    await nextTick()
    expect($('[data-testid=consent-sheet]')).toBeNull()
    expect(sent).toHaveLength(2)
  })

  it('without the server\'s address, the contact line says to ask the team', async () => {
    localStorage.clear()
    show()
    await click('[data-testid=species-yes]')
    await click('[data-testid=contrib-send]')
    expect($('[data-testid=consent-contact]')!.textContent).toMatch(/Franz Chandi/)
    expect($('[data-testid=consent-contact]')!.textContent).not.toMatch(/@/)
  })

  it('no location option on the card, and none in what it sends', async () => {
    show()
    expect($('[data-testid=contrib-gps]')).toBeNull()
    expect(host!.textContent).not.toMatch(/ubicaci|location/i)
    await click('[data-testid=species-yes]')
    await click('[data-testid=contrib-send]')
    expect(Object.keys(sent[0]).sort()).toEqual(['photoKind', 'sex', 'species'])
  })

  it('without signal it says it waits on the phone; «Deshacer» takes it back', async () => {
    show({ outcome: 'queued' })
    await click('[data-testid=species-yes]')
    await click('[data-testid=contrib-send]')
    await nextTick()
    expect($('[data-testid=contrib-done]')!.textContent).toMatch(/signal|señal/)
    await click('[data-testid=contrib-undo]')
    await nextTick()
    expect(undone).toBe(1)
    expect($('[data-testid=contrib-send]')).not.toBeNull()
  })

  it('«Se parece a esta» chosen after the card opened fills «Es otra especie», unless the person answered already', async () => {
    show()
    pickNow.value = 'Oleria onega'
    await nextTick()
    expect(($('[data-testid=taxon-input]') as HTMLInputElement).value).toBe('Oleria onega')
    pickNow.value = null
    await nextTick()
    expect($('[data-testid=taxon-input]')).toBeNull()
    await click('[data-testid=species-yes]')
    pickNow.value = 'Oleria onega'
    await nextTick()
    expect($('[data-testid=taxon-input]')).toBeNull()
  })

  it('a refusal by the server is said on the card, with its reason, and the card stays to fix it', async () => {
    show({ outcome: 'refused' })
    await click('[data-testid=species-yes]')
    await click('[data-testid=contrib-send]')
    await nextTick()
    expect($('[data-testid=contrib-refused]')!.textContent).toMatch(/not a taxon name/)
    expect($('[data-testid=contrib-send]')).not.toBeNull()
  })
})
