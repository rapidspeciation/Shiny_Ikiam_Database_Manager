import { afterEach, beforeEach, describe, expect, it } from 'vitest'
import { createApp, h, nextTick } from 'vue'
import { t, tn } from '../../lib/i18n'
import SexPanel, { type Labels, type Outcome } from '../identify/SexPanel.vue'
import type { SexState } from '../identify/SexEstimate.vue'
import { CONSENT_VERSION } from '../../lib/contribConfig'

// Identificar's sex panel: the AI's line in one row (details on a tap), «Sexo real» ♀ / ♂ and «Enviar» through the
// contributions flow (consent v2 the first time, «Deshacer»), no kind of photo asked.

let host: HTMLElement | null = null
const sent: Labels[] = []
let undone = 0
let consentedCalls = 0
const KEY = 'identify:contrib-consent:u-ana'
type Props = { state: SexState | null; canSend: boolean; species: Labels['species']; contact: string | null; outcome: Outcome; userId: string }
function show(props: Partial<Props> = {}) {
  host = document.createElement('div')
  document.body.append(host)
  const app = createApp({
    render: () =>
      h(SexPanel, {
        state: props.state === undefined ? { kind: 'ready', z: -2 } : props.state,
        field: null,
        canSend: props.canSend ?? true,
        species: props.species ?? null,
        contact: props.contact ?? null,
        userId: props.userId ?? 'u-ana',
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
  sent.length = 0
  undone = 0
  consentedCalls = 0
  localStorage.setItem(KEY, CONSENT_VERSION)
})
afterEach(() => {
  host?.remove()
  host = null
  localStorage.clear()
})

describe('SexPanel', () => {
  it('one line with the call; the scale and notes behind «Detalles»; no kind of photo asked', () => {
    show()
    expect($('[data-testid=sex-line]')!.textContent).toMatch(/♂/)
    // the in-hand line's details stay in the page (closed), with its scale
    expect($('details [data-testid=sex-estimate]')).not.toBeNull()
    expect($('details')!.hasAttribute('open')).toBe(false)
    expect(host!.textContent).not.toMatch(/Tipo de foto|Kind of photo|En la mano|In the hand|Colección/)
    expect($('[data-testid^=kind-]')).toBeNull()
  })

  it('«Enviar» is off until a sex (or a species above) is chosen; the real sex goes without a kind of photo', async () => {
    show()
    expect(($('[data-testid=contrib-send]') as HTMLButtonElement).disabled).toBe(true)
    await click('[data-testid=sex-female]')
    expect(($('[data-testid=contrib-send]') as HTMLButtonElement).disabled).toBe(false)
    await click('[data-testid=contrib-send]')
    await nextTick()
    expect(sent).toEqual([{ species: null, sex: 'female', photoKind: null }])
    expect($('[data-testid=contrib-done]')!.textContent).toMatch(/Enviada|Sent/)
  })

  it('the species the person chose above goes with the sex, and the panel names it', async () => {
    const species = { name: 'Mechanitis polymnia', rank: 'species' as const, confirmed: true, freeText: false }
    show({ species })
    expect($('[data-testid=send-species]')!.textContent).toContain('Mechanitis polymnia')
    await click('[data-testid=sex-male]')
    await click('[data-testid=contrib-send]')
    await nextTick()
    expect(sent).toEqual([{ species, sex: 'male', photoKind: null }])
  })

  it('a species alone can be sent (no sex chosen)', async () => {
    show({ species: { name: 'Oleria onega', rank: 'species', confirmed: false, freeText: false } })
    await click('[data-testid=contrib-send]')
    await nextTick()
    expect(sent[0].sex).toBeNull()
    expect(sent[0].species!.name).toBe('Oleria onega')
  })

  it('the first time, the consent sheet v2 with the server\'s address before anything is sent', async () => {
    localStorage.clear()
    show({ contact: 'person@example.org' })
    await click('[data-testid=sex-female]')
    await click('[data-testid=contrib-send]')
    expect(sent).toHaveLength(0)
    const sheet = $('[data-testid=consent-sheet]')!.textContent!
    expect(sheet).toMatch(/GPS/)
    expect($('[data-testid=consent-contact]')!.textContent).toMatch(/Franz Chandi.*person@example\.org/)
    await click('[data-testid=consent-accept]')
    await nextTick()
    expect(sent).toHaveLength(1)
    expect(consentedCalls).toBe(1)
    expect(localStorage.getItem(KEY)).toBe(CONSENT_VERSION)
  })

  it('the consent is each person\'s; «Ahora no» sends nothing', async () => {
    show({ userId: 'u-bob' })
    await click('[data-testid=sex-male]')
    await click('[data-testid=contrib-send]')
    expect($('[data-testid=consent-sheet]')).not.toBeNull()
    await click('[data-testid=consent-later]')
    expect(sent).toHaveLength(0)
    expect($('[data-testid=contrib-send]')).not.toBeNull()
  })

  it('without signal it says it waits on the phone; «Deshacer» takes it back', async () => {
    show({ outcome: 'queued' })
    await click('[data-testid=sex-female]')
    await click('[data-testid=contrib-send]')
    await nextTick()
    expect($('[data-testid=contrib-done]')!.textContent).toMatch(/signal|señal/)
    await click('[data-testid=contrib-undo]')
    await nextTick()
    expect(undone).toBe(1)
    expect($('[data-testid=contrib-send]')).not.toBeNull()
  })

  it('a refusal by the server is said, with its reason, and the row stays to fix it', async () => {
    show({ outcome: 'refused' })
    await click('[data-testid=sex-female]')
    await click('[data-testid=contrib-send]')
    await nextTick()
    expect($('[data-testid=contrib-refused]')!.textContent).toMatch(/not a taxon name/)
    expect($('[data-testid=contrib-send]')).not.toBeNull()
  })

  it('no location option, and none in what it sends', async () => {
    show()
    expect(host!.textContent).not.toMatch(/ubicaci|location/i)
    await click('[data-testid=sex-female]')
    await click('[data-testid=contrib-send]')
    expect(Object.keys(sent[0]).sort()).toEqual(['photoKind', 'sex', 'species'])
  })

  it('signed out or no photo to send: the AI line only; no estimate and nothing to send: nothing', () => {
    show({ canSend: false })
    expect($('[data-testid=sex-send]')).toBeNull()
    expect($('[data-testid=sex-line]')).not.toBeNull()
    host!.remove()
    show({ canSend: false, state: null })
    expect($('[data-testid=sex-panel]')).toBeNull()
  })

  it('no sex model on the phone: one line, the download behind «Detalles»; «Enviar» neutral until a sex is chosen', async () => {
    show({ state: { kind: 'download', mb: 54 } })
    expect($('[data-testid=sex-no-model]')).not.toBeNull()
    expect($('details')!.hasAttribute('open')).toBe(false)
    expect($('details [data-testid=sex-download]')).not.toBeNull()
    const send = $('[data-testid=contrib-send]') as HTMLButtonElement
    expect(send.className).not.toMatch(/btn-primary/)
    expect(send.getAttribute('aria-label')).toMatch(/entrenar|training/)
    await click('[data-testid=sex-female]')
    expect(($('[data-testid=contrib-send]') as HTMLButtonElement).className).toMatch(/btn-primary/)
  })
})
