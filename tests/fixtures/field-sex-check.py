"""tests/field-sex.test.mjs: the general sex line's server piece (tools/identifier/field_sex.py) on made-up folders.

No model files, no AI: a synthetic field_sex folder (species "Aa bb", "Cc dd", "Ee ff"; an alias "Xx bb" -> "Aa bb")
whose ADAPT head returns the species prior and whose kNN banks are all one sex, plus a folder with the real display
table (tests/fixtures/field-sex-display.json) and the six species. Prints one JSON line of results.

usage: python -I field-sex-check.py TOOLS_DIR DISPLAY_JSON
"""
import hashlib
import json
import math
import os
import shutil
import sys
import tempfile

import numpy as np

sys.path.insert(0, sys.argv[1])
from field_sex import FieldSex  # noqa: E402

DISPLAY = json.load(open(sys.argv[2]))
rng = np.random.default_rng(7)
D = 1024


def unit(n):
    x = rng.standard_normal((n, D)).astype(np.float32)
    return x / np.linalg.norm(x, axis=1, keepdims=True)


def make(folder, species, display, adapt=None, aliases=None):
    os.makedirs(os.path.join(folder, "bank"))
    meta = {"schema": "field-sex/1", "rule": {"t": 0.85, "a": 0.80, "gate": 0.80, "knn": {"k": 15, "temp": 0.05, "pseudo": 2.0}},
            "species": {}, "aliases": aliases or {}, "display": "display.json"}
    for i, (name, (prior, X, y)) in enumerate(species.items()):
        slug = name.replace(" ", "_")
        np.save(os.path.join(folder, "bank", slug + ".X.npy"), X.astype(np.float16))
        np.save(os.path.join(folder, "bank", slug + ".y.npy"), np.asarray(y, np.int8))
        meta["species"][name] = {"index": i, "bank": "bank/" + slug, "adapt_prior": prior, "knn_prior": prior}
    n = len(species)
    a = adapt or {"lin_w": np.zeros(D, np.float32), "lin_b": np.zeros(1, np.float32), "c": np.float32(1.0),
                  "P": np.zeros((32, D), np.float32), "Eg": np.zeros((n, 32), np.float32), "Es": np.zeros((n, 32), np.float32)}
    np.savez(os.path.join(folder, "adapt.npz"), **a)
    json.dump(meta, open(os.path.join(folder, "model.json"), "w"))
    json.dump(display, open(os.path.join(folder, "display.json"), "w"))
    with open(os.path.join(folder, "MANIFEST.sha256"), "w") as f:
        for root, _, files in os.walk(folder):
            for fn in sorted(files):
                if fn != "MANIFEST.sha256":
                    p = os.path.join(root, fn)
                    f.write(hashlib.sha256(open(p, "rb").read()).hexdigest() + "  " + os.path.relpath(p, folder) + "\n")


out = {}
tmp = tempfile.mkdtemp(prefix="field-sex-check-")
try:
    simple_maps = {sx: {"points": [[0.85, 0.93], [0.95, 0.995], [1.0, 1.0]]} for sx in ("female", "male")}
    table = [
        {"species": "Aa bb", "called": "female", "shown": True, "cap": 0.96},
        {"species": "Aa bb", "called": "male", "shown": True, "cap": 0.90},
        {"species": "Cc dd", "called": "female", "shown": True, "cap": 0.99},
        {"species": "Cc dd", "called": "male", "shown": False, "cap": None},  # suppressed: under 90 % on the fresh test
        {"species": "Ee ff", "called": "female", "shown": True, "cap": 0.99},
        {"species": "Ee ff", "called": "male", "shown": True, "cap": 0.99},
    ]
    disp = {"version": "field-sex-display/test", "cap_max": 0.99, "maps": simple_maps, "table": table}
    A = os.path.join(tmp, "a")
    make(A, {"Aa bb": (0.9, unit(20), [1] * 20), "Cc dd": (0.1, unit(20), [0] * 20), "Ee ff": (0.5, unit(20), [1, 0] * 10)},
         disp, aliases={"Xx bb": "Aa bb"})
    fs = FieldSex(A)
    feat = unit(1)
    out["outside"] = fs.score(feat, "Mechanitis messenoides", 0.99, "success")
    out["alias"] = fs.score(feat, "Xx bb", 0.9, "success")
    out["model_name_alias"] = fs.model_name("Xx bb")
    out["model_name_plain"] = fs.model_name("Aa bb")
    out["gate_0799"] = fs.score(feat, "Aa bb", 0.7999, "success")
    out["gate_08"] = fs.score(feat, "Aa bb", 0.8, "success")
    out["no_wings"] = fs.score(feat, "Aa bb", 0.95, "centre_crop_fallback")
    out["suppressed"] = fs.score(feat, "Cc dd", 0.95, "success")
    out["not_supported"] = fs.score(feat, "Ee ff", 0.95, "success")
    # the % never exceeds the row's cap (nor 99), and a suppressed species x sex never gives one
    worst = {}
    for t in table:
        for c in np.linspace(0.85, 1.0, 301):
            v = fs.shown_pct(t["species"], t["called"], float(c))
            key = t["species"] + "|" + t["called"]
            if not t["shown"]:
                worst[key] = worst.get(key, None) if v is None else "shown"
            else:
                worst[key] = max(worst.get(key, 0), v)
    out["max_pct_per_row"] = worst
    out["pct_at_cap_099"] = fs.shown_pct("Cc dd", "female", 1.0)  # 0.99 is 99, not 98

    # a map value under the 90 % floor shows no call (a shown row whose map starts low)
    L = os.path.join(tmp, "low")
    low = {"version": "field-sex-display/test", "cap_max": 0.99, "floor": 0.90, "table": [{"species": "Aa bb", "called": "female", "shown": True, "cap": 0.99},
           {"species": "Aa bb", "called": "male", "shown": True, "cap": 0.99}],
           "maps": {sx: {"points": [[0.85, 0.80], [0.95, 0.90], [1.0, 0.99]]} for sx in ("female", "male")}}
    make(L, {"Aa bb": (0.9, unit(20), [1] * 20)}, low)
    fl = FieldSex(L)
    out["low_map"] = {str(c): fl.shown_pct("Aa bb", "female", c) for c in (0.85, 0.9, 0.94, 0.95, 1.0)}

    # a changed file is refused
    B = os.path.join(tmp, "b")
    shutil.copytree(A, B)
    with open(os.path.join(B, "bank", "Aa_bb.y.npy"), "r+b") as f:
        f.seek(-1, 2); f.write(b"\x00")
    try:
        FieldSex(B)
        out["tamper"] = "loaded"
    except ValueError as e:
        out["tamper"] = str(e)

    # ADAPT + kNN numbers against the training code's formulas (train_eval.py Head ADAPT, knn_within_species), on a
    # bank larger than one chunk, mixed sexes, random head
    C = os.path.join(tmp, "c")
    X = unit(9000); y = (rng.random(9000) < 0.4).astype(int)
    ad = {"lin_w": rng.standard_normal(D).astype(np.float32) * 0.05, "lin_b": np.array([0.1], np.float32), "c": np.float32(0.8),
          "P": rng.standard_normal((32, D)).astype(np.float32) * 0.05, "Eg": rng.standard_normal((1, 32)).astype(np.float32),
          "Es": rng.standard_normal((1, 32)).astype(np.float32)}
    make(C, {"Gg hh": (0.37, X, y)}, disp)
    np.savez(os.path.join(C, "adapt.npz"), **ad)
    fc = FieldSex(C, check=False)
    diffs = []
    for q in unit(5):
        x = q.astype(np.float64); pp = 0.37
        z = x @ ad["lin_w"] + ad["lin_b"][0] + ad["c"] * math.log(pp / (1 - pp)) + ((ad["Eg"][0] + ad["Es"][0]) * (ad["P"].astype(np.float64) @ x)).sum()
        pa = 1 / (1 + math.exp(-z))
        P = X.astype(np.float16).astype(np.float32); sim = P @ q
        idx = np.argpartition(-sim, 14)[:15]; ss = sim[idx]
        w = np.exp((ss - ss.max()) / 0.05); w = w / w.sum() * 15
        pk = ((w * y[idx]).sum() + 2 * pp) / 17
        diffs.append(abs(fc.ens(q, "Gg hh") - (pa + pk) / 2))
    out["ens_max_diff"] = float(max(diffs))

    # the real display table, with the six species
    R = os.path.join(tmp, "r")
    six = sorted({t["species"] for t in DISPLAY["table"]})
    make(R, {s: (0.4, unit(20), [1] * 20) for s in six}, DISPLAY)
    fr = FieldSex(R)
    real = {}
    for t in DISPLAY["table"]:
        vals = [fr.shown_pct(t["species"], t["called"], float(c)) for c in np.linspace(0.85, 1.0, 1501)]
        real[t["species"] + "|" + t["called"]] = {"shown": t["shown"], "cap": t["cap"], "max_pct": max((v for v in vals if v is not None), default=None),
                                                  "any_none": any(v is None for v in vals)}
    out["real"] = real
    out["real_six"] = six
finally:
    shutil.rmtree(tmp)
print(json.dumps(out))
