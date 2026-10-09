/**
 * Invitations (server/invitations.mjs): the link never dies. In its first 7
 * days it opens the account form straight away; after that the page emails a
 * 6-digit code to the invitation's address first. Used and revoked ones stop.
 */
export type InvitationStatus = 'pending' | 'expired' | 'used' | 'revoked'

export interface InvitationLookup {
  email: string
  displayName: string
  role: string
  status: InvitationStatus
  /** A code sent and still good (expired invitations). */
  codeSent?: boolean
}

/** What the activation page shows. */
export type ActivationStep = 'form' | 'ask-code' | 'enter-code' | 'used' | 'revoked' | 'invalid'

export function activationStep(
  invitation: InvitationLookup | null,
  state: { invalid?: boolean; codeAsked?: boolean; codeOk?: boolean } = {},
): ActivationStep | null {
  if (state.invalid) return 'invalid'
  if (!invitation) return null
  if (invitation.status === 'used') return 'used'
  if (invitation.status === 'revoked') return 'revoked'
  if (invitation.status === 'pending' || state.codeOk) return 'form'
  return state.codeAsked || invitation.codeSent ? 'enter-code' : 'ask-code'
}

/** The page's own answer to a server error code (Spanish key, translated by the page), or null for the usual text. */
export function activationError(code: string | undefined): string | null {
  const known: Record<string, string> = {
    CODE_WRONG: 'Ese código no es correcto. Revísalo e inténtalo de nuevo.',
    CODE_EXPIRED: 'Ese código ya no vale (vence a los 15 minutos o tras varios intentos). Pide uno nuevo.',
    CODE_NEEDED: 'Primero pide un código.',
    CODE_LIMIT: 'Ya pediste varios códigos. Espera una hora e inténtalo de nuevo.',
    CODE_NOT_SENT: 'No se pudo enviar el código. Inténtalo en unos minutos.',
  }
  return known[code ?? ''] ?? null
}

/** A username to start from: the address before the @, as usernames may be written. */
export const usernameFromEmail = (email: string) =>
  email
    .split('@')[0]
    .toLowerCase()
    .replace(/[^a-z0-9._-]/g, '')
    .slice(0, 30)

/** 9/10/26: day first, as the team writes dates. */
export function shortDay(iso: string | null | undefined): string {
  if (!iso) return ''
  const d = new Date(iso)
  if (Number.isNaN(d.getTime())) return ''
  return `${d.getDate()}/${d.getMonth() + 1}/${String(d.getFullYear()).slice(2)}`
}
