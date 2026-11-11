/**
 * Photos sent from Identificar to train the AI (WingsClassificator reports/contributions_20261111/DESIGN.md): the
 * phone's side of the choices Franz has not made yet (DESIGN §8), in one place. The server's side is POLICY in
 * server/contributions.mjs (who may send, the location's rounding, the day kept, sex labels for any species).
 *
 * - CONTRIB_ENABLED: the card and «Mis envíos» show only in a build made with VITE_CONTRIB=1 (off by default), and only
 *   to signed-in people (Q1: the team only in v1).
 * - CONSENT_VERSION: sent with each item; the server takes only the versions in its CONSENTS.
 * - CONSENT: the text of the sheet shown on the first Enviar. A DRAFT (DESIGN §6), not checked by IKIAM: the sheet
 *   says so on screen (CONSENT_DRAFT). It must match what is kept (the day taken, the opt-in location) and the
 *   licence the server records with each item (server/contributions.mjs LICENCE: the grant plus CC BY 4.0).
 * - GPS_DECIMALS: the location is rounded on the phone before it is kept or sent (the server rounds it again).
 */
export const CONTRIB_ENABLED = import.meta.env.VITE_CONTRIB === '1'
/**
 * Who sees the card and «Mis envíos»: every signed-in team member (Franz, 11 Nov 2026: not admins only), never a
 * signed-out visitor, and nobody in a build without VITE_CONTRIB=1. The server checks the same (POLICY.roles).
 */
export function contribFor(user: { role?: string } | null | undefined, enabled = CONTRIB_ENABLED): boolean {
  return enabled && !!user
}
export const CONSENT_VERSION = 'v1-draft'
export const CONSENT_DRAFT = true
export const GPS_DECIMALS = 2
/** Items waiting for signal on one phone at most (about 25 MB). Sent ones keep only a 72 px thumbnail. */
export const MAX_PENDING = 50
/** How long «Deshacer» stays after Enviar. */
export const UNDO_MS = 10_000

/** The consent sheet, in Spanish (its English is in locales/en/identify.ts). */
export const CONSENT = {
  title: 'Enviar fotos para entrenar la IA',
  body: [
    'Si acepta, esta foto y lo que usted indique (especie, sexo, tipo de foto) se guardan en el servidor del proyecto Ithomiini (IKIAM) junto con lo que respondió la IA y su nombre de usuario.',
    'El equipo revisa cada foto antes de usarla para entrenar o evaluar los modelos de identificación y de sexo.',
    'De la información de la foto solo se guarda el día en que se tomó. La ubicación solo se envía si marca «Incluir la ubicación de la foto», redondeada a unos 1 km, y nunca se publica.',
    'Usted conserva los derechos de su foto. Al enviarla, concede al proyecto una licencia no exclusiva, gratuita y sin plazo para usarla en el entrenamiento y la evaluación de modelos, y para publicar esos modelos. La foto solo se publicaría como parte de un conjunto de datos bajo licencia CC BY 4.0, con crédito a su nombre de usuario.',
    'Puede borrar cualquier envío desde «Mis envíos». Se borra del servidor y de los próximos entrenamientos; los modelos ya publicados no se pueden «desentrenar».',
  ],
} as const
