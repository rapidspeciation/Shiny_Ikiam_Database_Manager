/**
 * Photos sent from Identificar to train the AI (WingsClassificator reports/contributions_20261111/DESIGN.md): the
 * phone's side of Franz's answers (12 Nov 2026, DESIGN §8), in one place. The server's side is POLICY in
 * server/contributions.mjs (who may send, who reviews, the day kept, no location, sex labels for any species).
 *
 * - CONTRIB_ENABLED: the card and «Mis envíos» show only in a build made with VITE_CONTRIB=1 (off by default), and only
 *   to signed-in people (Q1: the team only in v1).
 * - CONSENT_VERSION: sent with each item; the server takes only this version. Someone who accepted an older one sees
 *   this one once, on their next Enviar (or from «Mis envíos» when items wait for it), and must accept it again.
 * - CONSENT: the text of the sheet. It must match what is kept (the day taken; no location: the phone and the server
 *   both take the photo's EXIF out), where (only the project's server) and the licence the server records with each
 *   item (server/contributions.mjs LICENCE: the grant plus CC BY 4.0). Its contact address comes from the server at
 *   runtime (GET /api/identify/contributions/consent, its config), never from this repo.
 */
export const CONTRIB_ENABLED = import.meta.env.VITE_CONTRIB === '1'
/**
 * Who sees the card and «Mis envíos»: every signed-in team member (Franz, 11 Nov 2026: not admins only), never a
 * signed-out visitor, and nobody in a build without VITE_CONTRIB=1. The server checks the same (POLICY.roles).
 */
export function contribFor(user: { role?: string } | null | undefined, enabled = CONTRIB_ENABLED): boolean {
  return enabled && !!user
}
export const CONSENT_VERSION = 'v2'
/** Who the consent names as responsible for the data (the same as the server's CONTROLLER). */
export const CONTROLLER = 'Franz Chandi'
/** Items waiting for signal on one phone at most (about 25 MB). Sent ones keep only a 72 px thumbnail. */
export const MAX_PENDING = 50
/** How long «Deshacer» stays after Enviar. */
export const UNDO_MS = 10_000

/**
 * Each person's acceptance, on this phone: another account signed in on the same phone is asked for its own. (v1 kept
 * one for the whole phone, `identify:contrib-consent`: it is not read, so everyone sees v2.)
 */
const consentKey = (userId: string) => `identify:contrib-consent:${userId}`
/** This person accepted the current version on this phone (an older one, v1-draft, does not count). */
export function consentAccepted(userId: string | null | undefined): boolean {
  if (!userId) return false
  try {
    return localStorage.getItem(consentKey(userId)) === CONSENT_VERSION
  } catch {
    return false
  }
}
export function acceptConsent(userId: string | null | undefined): void {
  if (!userId) return
  try {
    localStorage.setItem(consentKey(userId), CONSENT_VERSION)
  } catch {
    /* private mode: asked again next time */
  }
}

/** The consent sheet, in Spanish (its English is in locales/en/identify.ts). `contact`: the line with the address, else `noContact`. */
export const CONSENT = {
  title: 'Enviar fotos para entrenar la IA',
  body: [
    'Si acepta, esta foto y lo que usted indique (especie, sexo, tipo de foto) se guardan en el servidor del proyecto Ithomiini, junto con lo que respondió la IA, el lado de los Andes que usted eligió (si eligió uno), la fecha y hora del envío y su nombre de usuario. Las fotos se guardan solo en ese servidor: no se copian a Google Drive ni a la hoja de cálculo del proyecto.',
    'Antes de usar una foto para entrenar o evaluar los modelos de identificación y de sexo, la revisa una persona del equipo con rol de revisor o administrador.',
    'No se envía la ubicación de la foto. El teléfono quita de la foto los datos de la cámara, incluida la ubicación (GPS), antes de enviarla, y el servidor los quita otra vez al recibirla. De esos datos solo se guarda el día en que se tomó la foto.',
    'Usted conserva los derechos de su foto. Al enviarla, concede al proyecto una licencia no exclusiva, gratuita y sin plazo para usarla en el entrenamiento y la evaluación de modelos, y para publicar esos modelos. La foto solo se publicaría como parte de un conjunto de datos bajo licencia CC BY 4.0, con crédito a su nombre de usuario.',
    'Puede borrar cualquier envío desde «Mis envíos». Se borran del servidor la foto y sus etiquetas, y no se usa en los próximos entrenamientos. Queda solo un registro mínimo (el identificador del envío, su usuario, la huella digital de la foto y la fecha) para que el mismo envío no vuelva a subirse. Los modelos ya publicados no se pueden «desentrenar», y las copias de un conjunto de datos ya publicado no se pueden retirar.',
  ],
  contact: 'Responsable de los datos: {name}. Para borrar un envío o hacer preguntas, escriba a {email}.',
  noContact: 'Responsable de los datos: {name}. Para borrar un envío o hacer preguntas, hable con un administrador del proyecto.',
} as const
