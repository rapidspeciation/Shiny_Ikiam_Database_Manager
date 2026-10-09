import { describe, expect, it } from 'vitest'
import { activationError, activationStep, shortDay, usernameFromEmail, type InvitationLookup } from '../invitations'

const invitation = (status: InvitationLookup['status'], codeSent = false): InvitationLookup => ({
  email: 'p•••a@example.org',
  displayName: 'Paula',
  role: 'editor',
  status,
  codeSent,
})

describe('the activation page', () => {
  it('the form in the first 7 days; after them a code first, then the form', () => {
    expect(activationStep(invitation('pending'))).toBe('form')
    expect(activationStep(invitation('expired'))).toBe('ask-code')
    expect(activationStep(invitation('expired'), { codeAsked: true })).toBe('enter-code')
    // A code sent earlier and still good: straight to typing it (the page was reloaded).
    expect(activationStep(invitation('expired', true))).toBe('enter-code')
    expect(activationStep(invitation('expired', true), { codeOk: true })).toBe('form')
  })
  it('used, revoked and broken links stop, whatever was typed', () => {
    expect(activationStep(invitation('used'), { codeOk: true })).toBe('used')
    expect(activationStep(invitation('revoked'), { codeOk: true })).toBe('revoked')
    expect(activationStep(null, { invalid: true })).toBe('invalid')
    expect(activationStep(null)).toBeNull()
  })
  it('its own words for the code errors, the usual text for the rest', () => {
    for (const code of ['CODE_WRONG', 'CODE_EXPIRED', 'CODE_LIMIT', 'CODE_NOT_SENT', 'CODE_NEEDED'])
      expect(activationError(code)).toBeTruthy()
    expect(activationError('USERNAME_TAKEN')).toBeNull()
    expect(activationError(undefined)).toBeNull()
  })
  it('a username from the address, and dates day first', () => {
    expect(usernameFromEmail('Paula.Pérez+x@example.org')).toBe('paula.prezx')
    expect(shortDay('2026-10-09T15:00:00.000Z')).toBe('9/10/26')
    expect(shortDay(null)).toBe('')
  })
})
