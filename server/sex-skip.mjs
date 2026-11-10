// The collection sex model's locked test portion (COLLECTION_SEX_SKIP_FILE, one CAM per line or space): its CAMs'
// sex is never scored, stored, shown or published. Every place that scores or shows the collection sex asks here:
// Dudas (server/monitoring.mjs), the Wings Gallery's files (server/gallery-public.mjs), the photo readings
// (server/readings-worker.mjs, server/readings.mjs and their store step in server/envelope-import.mjs), the photo
// review's data (server/photodata.mjs) and the assistant's specimen answers (server/maps-tools.mjs).
//
// Fails closed: without a readable, non-empty list of CAM IDs only (a CSV, commas or quotes would match no CAM and
// open it) no collection sex is scored or shown at all. Read once per file name; one log line when it fails.

import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';

const CAM = /^CAM\d{6}$/i;
let kept = { file: undefined, set: null };

/** The locked CAMs (upper case), or null when the list is missing, empty or not CAM IDs only. */
export function sexSkipList() {
  const file = process.env.COLLECTION_SEX_SKIP_FILE;
  if (kept.file === file) return kept.set;
  let set = null;
  try {
    const list = file ? readFileSync(file, 'utf8').split(/\s+/).filter(Boolean) : [];
    set = list.length && list.every(c => CAM.test(c)) ? new Set(list.map(c => c.toUpperCase())) : null;
  } catch {}
  if (!set) console.error('COLLECTION_SEX_SKIP_FILE is not set, not readable or not only CAM IDs: no collection sex is scored, shown or published.');
  kept = { file, set };
  return set;
}

/** Whether a CAM's collection sex may be scored or shown: there is a valid list and the CAM is not on it. */
export const sexOpen = cam => {
  const set = sexSkipList();
  return !!set && !set.has(String(cam ?? '').trim().toUpperCase());
};

/** Why a CAM's sex is not scored, as the readings' sex_status says it (null: it may be scored). */
export const sexClosedStatus = cam => (!sexSkipList() ? 'not_scored:no_valid_skip_list' : sexOpen(cam) ? null : 'not_scored:locked_test_split');

/**
 * Which list the published files were made with (the Wings Gallery's, server/readings.mjs): 'none' without a valid
 * list, else a short hash of its CAMs, so the files are made again when the app starts with another list (or its
 * first): files made before this check, or with the locked CAMs' sex, are replaced at the next start.
 */
export const sexSkipStamp = () => {
  const set = sexSkipList();
  return set ? createHash('sha256').update([...set].sort().join('\n')).digest('hex').slice(0, 16) : 'none';
};
