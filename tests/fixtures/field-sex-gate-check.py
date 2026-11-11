"""tests/field-sex.test.mjs: the worker's field sex gate ignores the geographic prior (tools/identifier/worker.py).

worker.py's own Identifier.tops, species_noprior and field_sex_block run on a made-up head (3 leaves, numpy
stand-ins for torch) with a geographic prior that removes the top species on the East side. The field sex model is a
stand-in that records what it was given. No model files, no AI. Writes one JSON object to OUT (the worker sends its own
stdout to stderr).

usage: python -I field-sex-gate-check.py TOOLS_DIR OUT_JSON
"""
import contextlib
import json
import sys

import numpy as np

tools, out_path = sys.argv[1], sys.argv[2]
sys.path.insert(0, tools)
sys.argv = ["worker.py", "--dir", "/nonexistent", "--no-pin"]
import worker  # noqa: E402


class Probs:
    def __init__(self, p):
        self.p = p

    def __getitem__(self, i):
        return self

    def numpy(self):
        return self.p


class Logits:
    def __init__(self, z):
        self.z = z

    def softmax(self, dim):
        e = np.exp(self.z - self.z.max())
        return Probs((e / e.sum())[0])


class FakeTorch:
    no_grad = staticmethod(contextlib.nullcontext)

    @staticmethod
    def from_numpy(a):
        return a


class Recorder:
    def __init__(self):
        self.seen = []

    def model_name(self, name):
        return name if name in ("Aa bb", "Cc dd") else None

    def score(self, feat, name, p, crop):
        self.seen.append([name, round(p, 4), crop])
        return {"species": name, "status": "call"}


ident = worker.Identifier.__new__(worker.Identifier)
ident.np, ident.torch = np, FakeTorch()
ident.leaves = ["Aa bb", "Cc dd", "Ee ff"]
ident.sp_vocab = ["Aa bb", "Cc dd", "Ee ff"]
ident.sp_index = {n: i for i, n in enumerate(ident.sp_vocab)}
ident.ge_vocab = ["Aa", "Cc", "Ee"]
ident.leaf2sp = np.array([0, 1, 2]); ident.leaf2ge = np.array([0, 1, 2]); ident.is_ssp = np.array([False, False, False])
probs = np.array([0.85, 0.10, 0.05])
ident.head = lambda x: Logits(np.log(probs)[None])
ident.geo = {"known": np.array([True] * 3), "inec": np.array([True] * 3), "East": np.array([False, True, True]), "West": np.array([True] * 3)}
ident.field_sex = Recorder()
feat = np.ones((1, 1024), np.float32)
east = ident.tops(feat, "East", None)
none = ident.tops(feat, None, None)
ident.field_sex = None
res = {"east_top": east["species"][0], "east_prior": east["prior"], "none_top": none["species"][0],
       "noprior": [round(worker.Identifier.species_noprior(ident, feat, n), 4) for n in ("Aa bb", "Cc dd", "Zz zz")],
       "no_model_block": ident.field_sex_block(feat, "success", "Aa bb")}
rec = Recorder(); ident.field_sex = rec
# as identify() does: the block is for the species the answer names (after the prior), gated on its no-prior score
ident.field_sex_block(feat, "success", east["species"][0][0])
ident.field_sex_block(feat, "centre_crop_fallback", none["species"][0][0])
res["not_six"] = ident.field_sex_block(feat, "success", "Ee ff")
res["seen"] = rec.seen
json.dump(res, open(out_path, "w"))
