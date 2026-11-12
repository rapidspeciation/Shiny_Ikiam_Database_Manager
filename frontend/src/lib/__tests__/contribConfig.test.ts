import { describe, expect, it, vi } from 'vitest'
import { acceptConsent, CONSENT, CONSENT_VERSION, consentAccepted, contribFor, CONTROLLER } from '../contribConfig'

// Who may send photos to train the AI (Franz, 11 Nov 2026): every signed-in team member, never a signed-out visitor.
describe('contribFor', () => {
  it('shows the card to every signed-in team member when the build has it', () => {
    for (const role of ['observer', 'editor', 'reviewer', 'admin']) expect(contribFor({ role }, true)).toBe(true)
  })
  it('hides it when signed out', () => {
    expect(contribFor(null, true)).toBe(false)
    expect(contribFor(undefined, true)).toBe(false)
  })
  it('hides it from everyone in a build without VITE_CONTRIB=1', () => {
    expect(contribFor({ role: 'admin' }, false)).toBe(false)
    expect(contribFor(null, false)).toBe(false)
  })
})

// The consent, v2 (Franz, 12 Nov 2026): no draft, no location, Franz Chandi responsible, the photos on the server only.
describe('consent v2', () => {
  it('is v2; a v1 acceptance does not count', () => {
    const kept = new Map<string, string>()
    vi.stubGlobal('localStorage', { getItem: (k: string) => kept.get(k) ?? null, setItem: (k: string, v: string) => void kept.set(k, v) })
    localStorage.setItem('identify:contrib-consent', 'v1-draft')
    localStorage.setItem('identify:contrib-consent:u-ana', 'v1-draft')
    expect(CONSENT_VERSION).toBe('v2')
    expect(consentAccepted('u-ana')).toBe(false)
    acceptConsent('u-ana')
    expect(consentAccepted('u-ana')).toBe(true)
    // Each person's own; nobody signed in never counts as accepted.
    expect(consentAccepted('u-bob')).toBe(false)
    expect(consentAccepted(null)).toBe(false)
    vi.unstubAllGlobals()
  })
  it('says what Franz decided, and holds no email address', () => {
    const all = [CONSENT.title, ...CONSENT.body, CONSENT.contact, CONSENT.noContact].join(' ')
    expect(CONTROLLER).toBe('Franz Chandi')
    expect(all).not.toMatch(/IKIAM|borrador|experto|1 km|Incluir la ubicación/i)
    expect(all).toMatch(/No se envía la ubicación de la foto/)
    expect(all).toMatch(/registro mínimo/)
    expect(all).toMatch(/copias de seguridad del servidor pueden conservarlas hasta 14 días/)
    expect(all).toMatch(/solo en ese servidor/)
    expect(all).toMatch(/CC BY 4\.0/)
    expect(all).not.toMatch(/@/)
    expect(CONSENT.contact).toMatch(/\{email\}/)
  })
})
