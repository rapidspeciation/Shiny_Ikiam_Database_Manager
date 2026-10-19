#!/usr/bin/env python3
"""Evaluate the OOF heads (RULE.md, written before the results) and pick the combine method.

Inputs: out/<mode>_f<k>/preds.jsonl.gz (downloaded), the 18 Oct release outputs (/tmp/pr/run/out), the workbook
species (/tmp/pr/stage/meta.json), training CAMs (train_cams.json), head vocabulary (vocab.json.gz), the gallery's
OOF predictions (predictions.json oof == 1). Writes results.json and final.json (per CAM / per photo tops of the
chosen method, with provenance).
"""
import collections
import gzip
import json
import re
import sys
from pathlib import Path

H = Path(__file__).resolve().parent
import os
OUT = Path(os.environ.get('OOF_OUT', H / 'out'))
GAL = Path('/home/franz/Documents/CodeProjs/wings-gallery-masks-20261019/public/data/predictions.json')


def jl(p):
    with gzip.open(p, 'rt') as f:
        return [json.loads(l) for l in f if l.strip()]


meta = json.load(open('/tmp/pr/stage/meta.json'))
cams_meta = meta['cams']
train = json.load(open(H / 'train_cams.json'))
vocab = set(json.load(gzip.open(H / 'vocab.json.gz'))['species'])
release_c = {r['cam']: r for r in jl('/tmp/pr/run/out/ident_cams.jsonl.gz')}
release_p = {r['file_id']: r for r in jl('/tmp/pr/run/out/ident_photos.jsonl.gz')}

pred = {'single': {}, 'paired': {}}
photo = {}
folds = {}
for d in sorted(OUT.glob('*_f*')):
    for r in jl(d / 'preds.jsonl.gz'):
        if r['kind'] == 'cam':
            pred[r['mode']][r['cam']] = r
            folds[r['cam']] = r['fold']
        else:
            photo[r['file_id']] = r
for m in pred:
    print(m, len(pred[m]), file=sys.stderr)


def comparable(s):
    return bool(s) and not re.search(r'\b(sp|spp|cf|aff)\.?$|\?|hybrid| x ', s, re.I) and len(s.split()) == 2


wb = {c: m.get('species') for c, m in cams_meta.items()}
trusted = {}
for c in release_c:
    s = wb.get(c)
    if not comparable(s) or s not in vocab:
        continue
    if c in train and any(' '.join(t.split()[:2]) != s for t in train[c]):
        continue
    trusted[c] = s


def top1(r):
    return r['species'][0][0], r['species'][0][1]


def ece(rows, bins=15):
    tot = len(rows)
    if not tot:
        return None
    b = collections.defaultdict(list)
    for conf, ok in rows:
        b[min(bins - 1, int(conf * bins))].append((conf, ok))
    return sum(len(v) / tot * abs(sum(x for x, _ in v) / len(v) - sum(o for _, o in v) / len(v)) for v in b.values())


def score(src, cams):
    rows = [(top1(src[c])[1], top1(src[c])[0] == trusted[c]) for c in cams if c in src]
    n = len(rows); right = sum(o for _, o in rows)
    return {'n': n, 'right': right, 'top1': round(100 * right / n, 2) if n else None,
            'ece': round(100 * ece(rows), 2) if n else None,
            'mean_conf': round(100 * sum(c for c, _ in rows) / n, 2) if n else None}


ev = sorted(c for c in trusted if c in pred['single'] and c in pred['paired'])
tr_ev = [c for c in ev if c in train]
nt_ev = [c for c in ev if c not in train]
res = {'trusted_cams': len(trusted), 'evaluated': len(ev), 'training_cams_in_eval': len(tr_ev),
       'a_single_oof': score(pred['single'], ev), 'b_paired_oof': score(pred['paired'], ev),
       'by_training': {'training_cams': {'a': score(pred['single'], tr_ev), 'b': score(pred['paired'], tr_ev),
                                         'release_in_sample': score(release_c, tr_ev)},
                       'never_in_training': {'a': score(pred['single'], nt_ev), 'b': score(pred['paired'], nt_ev),
                                             'release': score(release_c, nt_ev)}}}
A, B = res['a_single_oof'], res['b_paired_oof']
d = B['top1'] - A['top1']; n = B['right'] - A['right']
if abs(d) < 0.2 or abs(n) < 10:
    winner, why = 'a', f'tie (d = {d:+.2f} pp, n = {n:+d})'
else:
    hi, lo = ('b', 'a') if d > 0 else ('a', 'b')
    e_hi, e_lo = (B, A) if hi == 'b' else (A, B)
    if e_hi['ece'] - e_lo['ece'] > 2.0:
        winner, why = 'a', f'{hi} higher Top-1 ({d:+.2f} pp, {n:+d}) but ECE worse by {e_hi["ece"] - e_lo["ece"]:.2f} pp'
    else:
        winner, why = hi, f'{hi} higher Top-1 ({d:+.2f} pp, {n:+d}), ECE {e_hi["ece"]} vs {e_lo["ece"]}'
res['decision'] = {'winner': winner, 'why': why, 'rule_sha256': '97b1529631f35076bfc345b1215a5f3cdd706ea657f2310faf53dd06eb97c982'}

# final per-CAM predictions: (a) fold head for training CAMs, release head for the others; (b) fold head for all
final = {}
missing = [c for c in release_c if c not in pred['single'] or c not in pred['paired']]
res['missing_oof_cams'] = len(missing)
for c in release_c:
    if c in missing:
        continue
    if winner == 'a':
        if c in train:
            r = pred['single'][c]; how = f'oof fold {r["fold"]} (feature mean)'
        else:
            r = release_c[c]; how = 'release head (never in training)'
    else:
        r = pred['paired'][c]; how = f'oof fold {r["fold"]} (paired view)'
    final[c] = {'species': r['species'], 'genus': r['genus'], 'subspecies': r['subspecies'], 'andes': r.get('andes'),
                'how': how, 'training': c in train, 'photos': release_c[c]['photos']}
# per photo: the fold head for training CAMs' photos (single photo), release for the rest
final_photo = {}
for fid, r in release_p.items():
    if 'error' in r:
        continue
    if r['cam'] in missing:
        continue
    if r['cam'] in train:
        p = photo[fid]; how = f'oof fold {p["fold"]} (photo alone)'
    else:
        p = r; how = 'release head (never in training)'
    final_photo[fid] = {'cam': r['cam'], 'species': p['species'], 'genus': p['genus'], 'subspecies': p['subspecies'],
                        'andes': p.get('andes'), 'how': how, 'crop_status': r.get('crop_status'), 'wing_box': r.get('wing_box')}

# disagreements with the workbook (any comparable binomial, as the 18 Oct checks), and the gallery's OOF predictions
gal = json.load(open(GAL))
gal_oof = {c: v for c, v in gal.items() if v.get('oof') == 1 and v.get('species')}
cmp = [c for c in final if comparable(wb.get(c))]
dis = [c for c in cmp if final[c]['species'][0][0] != wb[c]]
dis6 = [c for c in dis if final[c]['species'][0][1] >= 0.6]
old_dis = [c for c in cmp if release_c[c]['species'][0][0] != wb[c]]
old_dis6 = [c for c in old_dis if release_c[c]['species'][0][1] >= 0.6]
ov = [c for c in cmp if c in gal_oof]
g_dis = [c for c in ov if gal_oof[c]['species'][0][0] != wb[c]]
g_dis6 = [c for c in g_dis if gal_oof[c]['species'][0][1] >= 0.6]
n_dis_ov = [c for c in ov if final[c]['species'][0][0] != wb[c]]
n_dis6_ov = [c for c in n_dis_ov if final[c]['species'][0][1] >= 0.6]
res['vs_workbook'] = {
    'cams_with_binomial': len(cmp),
    'new_oof_disagree': len(dis), 'new_oof_disagree_ge_0_6': len(dis6),
    'new_oof_disagree_training_cams': sum(c in train for c in dis),
    'release_18oct_disagree': len(old_dis), 'release_18oct_disagree_ge_0_6': len(old_dis6),
    'on_gallery_oof_overlap': {'cams': len(ov), 'gallery_oof_disagree': len(g_dis), 'gallery_oof_disagree_ge_0_6': len(g_dis6),
                               'new_disagree': len(n_dis_ov), 'new_disagree_ge_0_6': len(n_dis6_ov),
                               'both_disagree': len(set(g_dis) & set(n_dis_ov)),
                               'both_disagree_same_species': sum(1 for c in set(g_dis) & set(n_dis_ov)
                                                                 if gal_oof[c]['species'][0][0] == final[c]['species'][0][0]),
                               'top1_new_vs_gallery_same': sum(1 for c in ov if gal_oof[c]['species'][0][0] == final[c]['species'][0][0])},
    'sample_ge_0_6': [[c, wb[c], final[c]['species'][0][0], final[c]['species'][0][1], c in train] for c in sorted(dis6)[:40]],
}
# sanity: OOF single vs release on never-in-training CAMs (same features; should mostly agree)
nt = [c for c in release_c if c not in train and c in pred['single']]
res['sanity_single_vs_release_never_in_training'] = [sum(top1(pred['single'][c])[0] == top1(release_c[c])[0] for c in nt), len(nt)]
tr = [c for c in release_c if c in train and c in pred['single']]
res['training_cams_top1_changed_vs_release'] = [sum(top1(pred['single'][c])[0] != top1(release_c[c])[0] for c in tr), len(tr)]
(H / 'results.json').write_text(json.dumps(res, indent=1))
(H / 'final.json').write_text(json.dumps({'winner': winner, 'cams': final, 'photos': final_photo}))
print(json.dumps({k: v for k, v in res.items() if k != 'vs_workbook'}, indent=1))
print(json.dumps({k: v for k, v in res['vs_workbook'].items() if k != 'sample_ge_0_6'}, indent=1))
