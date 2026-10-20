#!/usr/bin/env python3
"""Gallery files (spec "## Decisions", GALLERY formats) and the app-import bundle for species + sex.

- predictions_app.json      species per CAM from final.json (eval_oof.py: the chosen OOF method)
- sex_predictions_app.json  sex per CAM from the 18 Oct run (the collection sex model's own out-of-fold folds)
- photo-predictions-oof.json.gz  the app bundle (same shape as /tmp/bundles/photo-predictions.json.gz) with the
                                  OOF species per CAM and per photo
"""
import gzip
import json
from pathlib import Path

H = Path(__file__).resolve().parent
OUT = Path('/tmp/bundles/gallery-20261019')
OUT.mkdir(parents=True, exist_ok=True)
SOURCE = 'gallery_readings_20261019'
fin = json.load(open(H / 'final.json'))
winner = fin['winner']
old = json.load(gzip.open('/tmp/bundles/photo-predictions.json.gz', 'rt'))
targets = {t['file_id']: t for t in json.load(open('/tmp/pr/targets.json'))}
r4 = lambda x: round(float(x), 4)
BASE = 'bioclip-2.5-h + head_hier 749fbffd recipe (mf_topup60, seed 1701) + wing_seg_v5n d74f9c13'
if winner == 'a':
    M_OOF = f'{BASE}, out-of-fold head (5 folds by CAM, this CAM held out), feature mean of the photos'
else:
    M_OOF = f'{BASE}, out-of-fold paired-view head (5 folds by CAM, this CAM held out), [dorsal, ventral]'
M_REL = f'{BASE.replace(" recipe (mf_topup60, seed 1701)", "")} (release head; this CAM was never in its training), feature mean of the photos'


def model_of(c):
    return M_OOF if c['how'].startswith('oof') else M_REL


def tops(x, n):
    return [[s, r4(p)] for s, p in x[:n]]


pred_app = {}
for cam, c in sorted(fin['cams'].items()):
    pred_app[cam] = {
        'species': tops(c['species'], 5), 'subspecies': tops(c['subspecies'], 5), 'genus': tops(c['genus'], 3),
        'side': c['andes'] or '', 'n_views': len(c['photos']), 'mode': 'app_readings',
        'model_meta': {'source': SOURCE, 'model': model_of(c), 'oof': c['how'].startswith('oof'),
                       'photos': [{'name': targets[f]['stem'], 'file': f} for f in c['photos']]}}
(OUT / 'predictions_app.json').write_text(json.dumps(pred_app, separators=(',', ':')))

sex_app = {}
for p in old['predictions']:
    if 'sexPMale' not in p:
        continue
    sex_app[p['cam']] = {'sex': p['sex'], 'confidence': p['sexConf'], 'male_probability': p['sexPMale'],
                         'supported': bool(p['sexSupported']), 'species': p.get('sexSpecies'),
                         'screen_status': p.get('sexStatus'), 'model_arm': p.get('sexModel'), 'source': 'photo_readings_20261018'}
(OUT / 'sex_predictions_app.json').write_text(json.dumps(sex_app, separators=(',', ':')))

# app bundle: yesterday's bundle with the species replaced
preds = []
for p in old['predictions']:
    c = fin['cams'].get(p['cam'])
    q = dict(p)
    if c:
        q.update(species=tops(c['species'], 5), genus=tops(c['genus'], 3), subspecies=tops(c['subspecies'], 5), andes=c['andes'],
                 model=f"{model_of(c)} ({len(c['photos'])} photo{'s' if len(c['photos']) != 1 else ''})", source=SOURCE)
    preds.append(q)
photo_preds = []
for p in old['photoPredictions']:
    f = fin['photos'].get(p['fileId'])
    q = dict(p)
    if f:
        oof = f['how'].startswith('oof')
        q.update(species=tops(f['species'], 5), genus=tops(f['genus'], 3), subspecies=tops(f['subspecies'], 5), andes=f['andes'],
                 model=(f'{BASE}, out-of-fold head (its CAM held out), single photo' if oof else
                        f'{BASE.replace(" recipe (mf_topup60, seed 1701)", "")} (release head), single photo'), source=SOURCE)
    photo_preds.append(q)
bundle = {**old, 'source': SOURCE, 'model': M_OOF, 'predictions': preds, 'photoPredictions': photo_preds}
with gzip.open(OUT / 'photo-predictions-oof.json.gz', 'wt') as fo:
    json.dump(bundle, fo)
print(json.dumps({'predictions_app': len(pred_app), 'oof_cams': sum(v['model_meta']['oof'] for v in pred_app.values()),
                  'sex_app': len(sex_app), 'bundle_cams': len(preds), 'bundle_photos': len(photo_preds), 'winner': winner}))
