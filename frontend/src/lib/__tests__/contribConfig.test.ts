import { describe, expect, it } from 'vitest'
import { CONSENT_DRAFT, contribFor } from '../contribConfig'

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
  it('keeps the consent text marked as a draft', () => {
    expect(CONSENT_DRAFT).toBe(true)
  })
})
