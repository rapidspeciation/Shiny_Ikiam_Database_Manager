#!/usr/bin/env node
// The app's photo-predictions bundle of 19 Oct 2026 (scripts/import-photo-predictions.mjs): out-of-fold species only
// and the sex of the 19 Oct 2026 CNN for every butterfly (server/oof.mjs).
//   Species, per CAM: the Wings Gallery's out-of-fold predictions (its predictions.json rows with oof = 1: candidate D
//   and the nextgen out-of-fold rows; they agree with the workbook more than the heads below), else the readings of
//   gallery_readings_20261019 (photo-predictions-oof.json.gz): head 749fbffd's recipe with that CAM held out (5 folds
//   by CAM) for the CAMs it was trained on, the release head for the others (never in its training). photo_predictions
//   .source says which: gallery_predictions_oof, gallery_readings_20261019_oof, gallery_readings_20261019.
//   Each photo alone (photo_file_predictions): the readings' rows as they are (held out for training CAMs).
//   Sex: sex_cnn.json, the CNN computed for every CAM as the readings worker does (Sanger, gallery_readings_20261019/sex_cnn).
//
//   node tools/oof-identifier/build-app-bundle.mjs --gallery <predictions.json> --readings <photo-predictions-oof.json.gz>
//        --sex <sex_cnn.json> --out <photo-predictions-oof-sexcnn.json.gz>

import { readFileSync, writeFileSync } from 'node:fs';
import { gunzipSync, gzipSync } from 'node:zlib';
import { trainingCams } from '../../server/oof.mjs';
import { sexModelName } from '../../server/readings.mjs';

const args = Object.fromEntries(process.argv.slice(2).reduce((a, v, i, all) => (v.startsWith('--') ? [...a, [v.slice(2), all[i + 1]]] : a), []));
for (const k of ['gallery', 'readings', 'sex', 'out']) if (!args[k]) throw new Error(`--${k} is required`);
const load = f => {
  const raw = readFileSync(f);
  return JSON.parse((f.endsWith('.gz') ? gunzipSync(raw) : raw).toString('utf8'));
};
const r4 = v => Math.round(Number(v) * 10000) / 10000;
const top = (list, n) => (Array.isArray(list) ? list.slice(0, n).map(r => [String(r[0]), r4(r[1])]) : []);

const gallery = Object.fromEntries(Object.entries(load(args.gallery)).filter(([, v]) => v.oof === 1 || v.oof === true).map(([k, v]) => [k.toUpperCase(), v]));
const readings = load(args.readings);
const sex = load(args.sex);
const training = trainingCams();
const counts = { gallery_predictions_oof: 0, gallery_readings_20261019_oof: 0, gallery_readings_20261019: 0, sexScored: 0 };

const predictions = readings.predictions.map(r => {
  const cam = r.cam.toUpperCase();
  const heldOut = /out-of-fold/.test(r.model);
  if (training.has(cam) && !heldOut) throw new Error(`${cam}: a training CAM without a held-out prediction`);
  const g = gallery[cam];
  const out = g
    ? {
        cam: r.cam,
        source: 'gallery_predictions_oof',
        species: top(g.species, 5),
        genus: top(g.genus, 3),
        subspecies: top(g.subspecies, 5),
        andes: /^(East|West)$/.test(g.side ?? '') ? g.side : null,
        model: `Wings Gallery predictions.json ${g.model_meta?.build_identity ?? g.source} (out-of-fold, ${g.source}; ${g.n_views ?? '?'} photos)`,
        photos: r.photos,
      }
    : {
        cam: r.cam,
        source: heldOut ? 'gallery_readings_20261019_oof' : 'gallery_readings_20261019',
        species: r.species,
        genus: r.genus,
        subspecies: r.subspecies,
        andes: r.andes,
        model: r.model,
        photos: r.photos,
      };
  counts[out.source]++;
  const s = sex[cam];
  if (!s) throw new Error(`${cam}: no sex row`);
  const name = sexModelName(s.sexStatus?.startsWith('not_scored') ? null : { model: 'release_20261019 cnn_strips4', crops: s.crops });
  if (Number.isFinite(s.sexPMale)) {
    counts.sexScored++;
    Object.assign(out, { sex: s.sex, sexConf: s.sexConf, sexSupported: !!s.sexSupported, sexSpecies: s.sexSpecies ?? null, sexStatus: s.sexStatus, sexPMale: s.sexPMale, sexModel: name });
  } else Object.assign(out, { sexStatus: s.sexStatus, sexModel: name });
  return out;
});
const photoPredictions = readings.photoPredictions.map(p => ({ ...p, source: /out-of-fold/.test(p.model) ? 'gallery_readings_20261019_oof' : 'gallery_readings_20261019' }));
const bundle = {
  version: 1,
  source: 'gallery_readings_20261019',
  model: 'out-of-fold species per CAM (photo_predictions.source): Wings Gallery predictions.json OOF > head 749fbffd recipe with the CAM held out > release head (CAM never in its training)',
  sexModel: sexModelName(null),
  predictions,
  photoPredictions,
};
writeFileSync(args.out, gzipSync(JSON.stringify(bundle)));
console.log(JSON.stringify({ out: args.out, cams: predictions.length, photos: photoPredictions.length, ...counts }));
