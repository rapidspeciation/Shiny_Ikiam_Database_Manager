#!/usr/bin/env python3
"""Out-of-fold identifier heads for the collection's specimens (gallery_readings_20261019, SANGER).

One fold, one mode per run:
  single  the 14 Oct head recipe (mf_topup60, release setting, seed 1701; train_head_429b7afcf652.py) trained on every
          harness row except the pub_sanger rows of this fold's CAMs; predicts each CAM of the fold from the mean of its
          photos' features (the 18 Oct per-CAM photo set) and each of its photos alone.
  paired  the same recipe on 2048-d inputs [dorsal, ventral] / sqrt 2: a training CAM (outside the fold) with a d and a
          v row gives one paired row (first d with first v), every other row is duplicated [x, x] / sqrt 2 (also the
          top-up rows); predicts each CAM of the fold from [mean d, mean v] / sqrt 2 (a missing view duplicates the other).
Prediction features: the 18 Oct V5N-crop BioCLIP 2.5-H features (ident_features.npy, row = photo of meta.json).
Outputs (OUT): preds.jsonl.gz (tops after the Space's geographic prior, as readings.py), probs.npz (leaf
probabilities before the prior, fp16), head.pt (harness predictor state), receipt.json.
"""
from __future__ import annotations

import argparse
import gzip
import hashlib
import json
import math
import sys
import time
import types
from pathlib import Path

import numpy as np
import torch
import torch.nn.functional as F

ap = argparse.ArgumentParser()
ap.add_argument('--data', type=Path, required=True)          # node-local data_v1
ap.add_argument('--hx-code', type=Path, required=True)       # harness code dir (hx_data.py)
ap.add_argument('--train-head', type=Path, required=True)    # train_head_429b7afcf652.py
ap.add_argument('--space-code', type=Path, required=True)    # geo_prior.py
ap.add_argument('--checklist', type=Path, required=True)     # region_checklist.json 527ca3ea
ap.add_argument('--head-hier', type=Path, required=True)     # head_hier 749fbffd (leaf / rank metadata)
ap.add_argument('--plan', type=Path, required=True)
ap.add_argument('--feats', type=Path, required=True)
ap.add_argument('--fold', type=int, required=True)
ap.add_argument('--mode', choices=['single', 'paired'], required=True)
ap.add_argument('--epochs', type=int, default=0)             # smoke only
ap.add_argument('--out', type=Path, required=True)
a = ap.parse_args()
a.out.mkdir(parents=True, exist_ok=True)
sys.path.insert(0, str(a.hx_code)); sys.path.insert(0, str(a.space_code))
import hx_data  # noqa: E402
from geo_prior import GeoPrior  # noqa: E402

CONFIG = {'marg_weight': 2.0, 'kind_weights': {'gbif_inat': 2.0, 'moth_inat': 2.0, 'pub_inat': 2.0},
          'topup': {'name': 'topup', 'cap': 60}, 'seed': 1701}
SQ2 = math.sqrt(2.0)
t0 = time.time()


def status(**kw):
    p = a.out / 'status.json'
    p.with_suffix('.partial').write_text(json.dumps({**kw, 'fold': a.fold, 'mode': a.mode, 's': round(time.time() - t0)}))
    p.with_suffix('.partial').replace(p)


src = a.train_head.read_text()
sha = hashlib.sha256(src.encode()).hexdigest()
assert sha.startswith('429b7afcf652'), sha
mod = types.ModuleType('train_head')
exec(compile(src, a.train_head.name, 'exec'), mod.__dict__)

dev = torch.device('cuda')
assert torch.cuda.is_available()
status(phase='load')
data = hx_data.ExperimentData.load(a.data, dev)
td = data.train_data('release')
plan = json.loads(a.plan.read_text())
P, srows = plan['plan'], plan['sanger_rows']
fold_cams = sorted(c for c, v in P.items() if v['fold'] == a.fold)
fold_set = set(fold_cams)

# the pub_sanger rows: row index -> CAM; sanity: every listed row is pub_sanger
kinds = td.kinds
kid = kinds.index('pub_sanger')
rows_idx = torch.as_tensor([r for r, _, _ in srows], device=dev)
assert bool((td.kind[rows_idx] == kid).all()) and int((td.kind == kid).sum()) == len(srows)
drop = [r for r, c, _ in srows if c in fold_set]
pos = td.train_positions
keep = torch.ones(len(td.y), dtype=torch.bool, device=dev)
keep[torch.as_tensor(drop, device=dev, dtype=torch.long)] = False

paired_rows = 0
if a.mode == 'paired':
    N = data.X.shape[0]
    partner = torch.arange(N, device=dev)
    by = {}
    for r, c, suf in srows:
        if c in fold_set:
            continue
        by.setdefault(c, {}).setdefault(suf[:1], []).append((suf, r))
    for c, g in by.items():
        if g.get('d') and g.get('v'):
            d = sorted(g['d'])[0][1]; v = sorted(g['v'])[0][1]
            assert int(td.y[d]) == int(td.y[v]) and int(td.m[d]) == int(td.m[v]), c
            partner[d] = v; keep[v] = False; paired_rows += 1
    X2 = torch.empty((N, 2048), dtype=torch.float32, device=dev)
    for s in range(0, N, 65536):
        i = torch.arange(s, min(N, s + 65536), device=dev)
        X2[i] = torch.cat([F.normalize(data.X[i], dim=1), F.normalize(data.X[partner[i]], dim=1)], 1) / SQ2
    data.X = None; td.X = None; torch.cuda.empty_cache()
    ex = data.extra['topup']
    ex['X'] = torch.cat([F.normalize(ex['X'], dim=1)] * 2, 1) / SQ2
    td.X = X2
td.train_positions = pos[keep[pos]]
if a.epochs:
    td.epochs = a.epochs
status(phase='fit', dropped_rows=len(drop), paired_rows=paired_rows, n_positions=int(len(td.train_positions)))
print(json.dumps({'fold': a.fold, 'mode': a.mode, 'fold_cams': len(fold_cams), 'dropped_rows': len(drop),
                  'paired_rows': paired_rows, 'positions': int(len(td.train_positions)), 'gpu': torch.cuda.get_device_name(0)}), flush=True)
last = [0]


def progress(d):
    if d['epoch'] % 5 == 0 or d['epoch'] == 1:
        print(json.dumps(d), flush=True)
        status(phase='fit', **d)


pred = mod.fit(td, dict(CONFIG), progress)
fit_info = {k: v for k, v in (pred.fit_info or {}).items() if k != 'recipe'}
torch.save({'leaves': td.leaves, 'state': pred.state(), 's': pred.s, 'fit_info': fit_info, 'mode': a.mode, 'fold': a.fold,
            'config': CONFIG, 'train_head_sha': sha, 'dropped_cams': fold_cams}, a.out / 'head.pt')
status(phase='predict', **fit_info)

# ---- prediction
ck = torch.load(a.head_hier, map_location='cpu', weights_only=False)
leaves = list(ck['leaves'])
assert leaves == list(td.leaves), 'leaf order differs from head_hier'
sp_vocab, ge_vocab = ck['rank_vocab']['species'], ck['rank_vocab']['genus']
leaf2sp, leaf2ge = np.array(ck['leaf2rank']['species']), np.array(ck['leaf2rank']['genus'])
is_ssp = np.array([lf.count(' ') >= 2 for lf in leaves])
geo = GeoPrior(a.checklist, leaves, sp_vocab, ge_vocab)
wcache = {}


def r4(x):
    return round(float(x), 4)


def tops(probs, side, ecuador):
    k = (side, ecuador)
    if k not in wcache:
        wcache[k] = geo.leaf_weights(side or 'unknown', ecuador, 0.02)
    pr = probs * wcache[k]; s = pr.sum(); pr = pr / s if s > 0 else probs

    def marg(l2r, vocab, n):
        agg = np.zeros(len(vocab)); np.add.at(agg, l2r, pr)
        return [[vocab[j], r4(agg[j])] for j in np.argsort(-agg, kind='stable')[:n]]
    subs = []
    for li in np.argsort(-pr, kind='stable'):
        if is_ssp[li]:
            subs.append([leaves[li], r4(pr[li])])
            if len(subs) >= 5:
                break
    vis = np.zeros(len(sp_vocab)); np.add.at(vis, leaf2sp, probs)
    j = int(np.argmax(vis))
    return {'species': marg(leaf2sp, sp_vocab, 5), 'genus': marg(leaf2ge, ge_vocab, 3), 'subspecies': subs,
            'visual_top1': [sp_vocab[j], r4(vis[j])]}


feats = torch.from_numpy(np.load(a.feats).astype(np.float32)).to(dev)
nf = lambda t: F.normalize(t, dim=-1)
cam_x, photo_x, photo_of = [], [], []
for c in fold_cams:
    v = P[c]
    if a.mode == 'single':
        cam_x.append(nf(feats[v['use']].mean(0)))
        for i in v['photos']:
            photo_x.append(feats[i]); photo_of.append((c, i))
    else:
        d = nf(feats[v['d']].mean(0)) if v['d'] else None
        w = nf(feats[v['v']].mean(0)) if v['v'] else None
        if d is None and w is None:
            d = w = nf(feats[v['use']].mean(0))
        d = w if d is None else d; w = d if w is None else w
        cam_x.append(torch.cat([d, w]) / SQ2)


@torch.inference_mode()
def probs_of(xs):
    out = []
    X = torch.stack(xs)
    for s in range(0, len(X), 1024):
        out.append(pred.leaf_probs(X[s:s + 1024]).double().cpu().numpy())
    return np.concatenate(out) if out else np.zeros((0, len(leaves)))


Pc = probs_of(cam_x)
Pp = probs_of(photo_x) if photo_x else np.zeros((0, len(leaves)))
fids = plan['file_ids']
with gzip.open(a.out / 'preds.jsonl.gz', 'wt') as fo:
    for c, pr in zip(fold_cams, Pc):
        fo.write(json.dumps({'cam': c, 'kind': 'cam', 'mode': a.mode, 'fold': a.fold, 'andes': P[c]['side'],
                             **tops(pr, P[c]['side'], P[c]['ecuador'])}) + '\n')
    for (c, i), pr in zip(photo_of, Pp):
        fo.write(json.dumps({'cam': c, 'kind': 'photo', 'file_id': fids[i], 'mode': a.mode, 'fold': a.fold,
                             'andes': P[c]['side'], **tops(pr, P[c]['side'], P[c]['ecuador'])}) + '\n')
np.savez_compressed(a.out / 'probs.npz', cams=np.array(fold_cams), cam_probs=Pc.astype(np.float16),
                    photo_ids=np.array([fids[i] for _, i in photo_of]), photo_probs=Pp.astype(np.float16))
rec = {'fold': a.fold, 'mode': a.mode, 'fold_cams': len(fold_cams), 'photos': len(photo_of), 'dropped_rows': len(drop),
       'paired_rows': paired_rows, 'train_head_sha': sha, 'config': CONFIG, 'epochs': td.epochs, **fit_info,
       'gpu': torch.cuda.get_device_name(0), 'torch': torch.__version__, 'seconds': round(time.time() - t0)}
(a.out / 'receipt.json').write_text(json.dumps(rec, indent=1))
status(phase='done', **rec)
print(json.dumps(rec), flush=True)
