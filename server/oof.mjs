// Which species predictions are out-of-fold. The identifier's head (head_hier 749fbffd, tools/identifier) was trained on
// the collection photos of 3,022 butterflies (its pub_sanger rows, tools/identifier/head_749fbffd_training_cams.json,
// from the Sanger plan of tools/oof-identifier): its own predictions for them are in-sample and never used. Theirs come
// from heads trained without them (5 folds by CAM, gallery_readings_20261019) or from the gallery's out-of-fold
// predictions.json; every other butterfly was never in its training, so the release head's answer is out-of-fold.

import { readFileSync } from 'node:fs';

const LIST = new URL('../tools/identifier/head_749fbffd_training_cams.json', import.meta.url);
let training = null;
/** The CAMs of the head's training (upper case). */
export function trainingCams() {
  training ??= new Set(JSON.parse(readFileSync(LIST, 'utf8')).map(c => String(c).toUpperCase()));
  return training;
}

/** Sources whose rows are out-of-fold whatever the CAM (photo_predictions.source). */
export const HELD_OUT_SOURCES = {
  gallery_predictions_oof: 'gallery_predictions_oof',
  gallery_readings_20261019_oof: 'held_out_fold',
};

/**
 * How a CAM's species prediction is out-of-fold: 'gallery_predictions_oof' (the gallery's predictions.json),
 * 'held_out_fold' (a head trained without that CAM), 'never_in_training' (the release head, the CAM not in its
 * training), or null (the release head on a CAM it was trained on: in-sample).
 */
export function oofSource({ cam, source }) {
  if (HELD_OUT_SOURCES[source]) return HELD_OUT_SOURCES[source];
  return trainingCams().has(String(cam).toUpperCase()) ? null : 'never_in_training';
}
