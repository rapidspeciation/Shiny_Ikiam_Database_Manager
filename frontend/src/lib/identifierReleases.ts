/**
 * The identifier's head files in the order they were released (oldest first), with the day each went out. The head
 * is named by a hash (head_<8 hex>.json), which says nothing about which came first, so «Mis identificaciones» uses
 * this list to tell that an item was read by an OLDER identifier than the one in use now («Reprocesar»,
 * lib/identifyHistory.ts olderThan). A head missing from the list (unknown, or newer than this build: a server ahead
 * of the app) is never called older or newer, so no button.
 *
 * NEXT MODEL RELEASE: add the new head file at the END of this list, in the same change that updates
 * lib/publicModels.ts (a test checks that the last entry is the head publicModels.ts lists).
 *
 * Built from the git history of lib/publicModels.ts and server/offline-id.mjs (the file names the phone and the
 * server's model list have used for role 'head-meta'):
 * - head_b8127dbf.json: the first offline identifier page (commit 9909c335, 29 Oct 2026) and Identificar for everyone
 *   (92317f9b, 3 Nov 2026).
 * - head_6fa6ab78.json: head_hier 6fa6ab78, alias_w075 on V5N crops with label fixes (3fc6082c, 4 Nov 2026).
 * The server's earlier head_hier 749fbffd (19 Oct 2026) was never listed as a file, so no item can name it.
 */
export const IDENTIFIER_HEADS: readonly { file: string; released: string }[] = [
  { file: 'head_b8127dbf.json', released: '2026-10-29' },
  { file: 'head_6fa6ab78.json', released: '2026-11-04' },
]

/** A head file's place in the release order (0 = oldest), or null when this build does not know it. */
export function headRelease(file: string | null | undefined): number | null {
  if (!file) return null
  const i = IDENTIFIER_HEADS.findIndex(h => h.file === file)
  return i < 0 ? null : i
}
