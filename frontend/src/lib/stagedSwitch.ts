/**
 * Whether Emergidos and Clutches keep their changes in the app until «Guardar en
 * Google Sheets» (server/staged.mjs): the server's setting (STAGED_SAVING), set
 * from the session; false: those tabs save straight to the sheet and Censo is hidden.
 */
let on = true
export const stagedSaving = () => on
export function setStagedSaving(value: boolean) {
  on = value
}
