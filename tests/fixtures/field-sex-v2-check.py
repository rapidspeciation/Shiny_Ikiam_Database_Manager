"""tests/field-sex.test.mjs: the all-species field sex folder (schema field-sex/2, tools/identifier/field_sex.py).

No model files, no AI: made-up field-sex/2 folders with one bank (rows sorted by genus then species), one verified
species "Aa bb" and estimated species with level-table rows, compared against a field-sex/1 folder of the same data
for the verified species; plus every row of the real level table (tests/fixtures/field-sex-calibration.json, a copy of
WingsClassificator reports/general_sex_20261111/allspecies/calibration.json). Prints one JSON line of results.

usage: python -I field-sex-v2-check.py TOOLS_DIR DISPLAY_JSON CALIBRATION_JSON
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
from field_sex import ESTIMATE_CAP_MAX, FieldSex  # noqa: E402

DISPLAY = json.load(open(sys.argv[2]))
REAL_CAL = json.load(open(sys.argv[3]))
rng = np.random.default_rng(9)
D = 1024
RULE = {"t": 0.85, "a": 0.80, "gate": 0.80, "knn": {"k": 15, "temp": 0.05, "pseudo": 2.0}}


def unit(n):
    x = rng.standard_normal((n, D)).astype(np.float32)
    return x / np.linalg.norm(x, axis=1, keepdims=True)


def manifest(folder):
    with open(os.path.join(folder, "MANIFEST.sha256"), "w") as f:
        for root, _, files in os.walk(folder):
            for fn in sorted(files):
                if fn != "MANIFEST.sha256":
                    p = os.path.join(root, fn)
                    f.write(hashlib.sha256(open(p, "rb").read()).hexdigest() + "  " + os.path.relpath(p, folder) + "\n")


disp = {"version": "field-sex-display/test", "cap_max": 0.99, "maps": {sx: {"points": [[0.85, 0.93], [0.95, 0.995], [1.0, 1.0]]} for sx in ("female", "male")},
        "table": [{"species": "Aa bb", "called": "female", "shown": True, "cap": 0.96}, {"species": "Aa bb", "called": "male", "shown": True, "cap": 0.90}]}
# bank rows, sorted by genus then species: Aa bb (20), Gg hh (30), Gg rare (3, under 5: the genus pool Gg = 33 rows)
XA, yA = unit(20), np.array([1] * 14 + [0] * 6)
XG, yG = unit(30), np.array([1, 0] * 15)
XR, yR = unit(3), np.array([1, 1, 1])
cal_rows = {
    "Gg hh": {"a": 0.8, "b": 0.1, "cap_female": 0.90, "cap_male": 0.80, "level": "species", "rows": 30, "bin": "B3"},
    "Gg rare": {"a": 0.3, "b": -0.2, "cap_female": 0.84, "cap_male": 0.84, "level": "family", "rows": 3, "bin": "B1"},
    "Kk solo": {"a": 1.0, "b": 0.0, "cap_female": 0.99, "cap_male": 0.99, "level": "global", "rows": 0, "bin": "B1"},  # cap over 0.95
}
ad = {"lin_w": rng.standard_normal(D).astype(np.float32) * 0.05, "lin_b": np.array([0.1], np.float32), "c": np.float32(0.8),
      "P": rng.standard_normal((32, D)).astype(np.float32) * 0.05, "Eg": rng.standard_normal((4, 32)).astype(np.float32),
      "Es": rng.standard_normal((5, 32)).astype(np.float32)}


def make_v2(folder, cal=None, drop_cal=None):
    os.makedirs(os.path.join(folder, "bank"))
    X = np.concatenate([XA, XG, XR]).astype(np.float16); y = np.concatenate([yA, yG, yR]).astype(np.int8)
    np.save(os.path.join(folder, "bank", "all.X.npy"), X); np.save(os.path.join(folder, "bank", "all.y.npy"), y)
    species = {
        "Aa bb": {"pool": [0, 20], "pool_kind": "species", "rows": 20, "adapt_prior": 0.7, "knn_prior": 0.7, "eg": 1, "es": 1},
        "Gg hh": {"pool": [20, 50], "pool_kind": "species", "rows": 30, "adapt_prior": 0.5, "knn_prior": 0.5, "eg": 2, "es": 2},
        "Gg rare": {"pool": [20, 53], "pool_kind": "genus", "rows": 3, "adapt_prior": 0.6, "knn_prior": 0.6, "eg": 2, "es": 3},
        "Kk solo": {"pool": [0, 0], "pool_kind": "genus", "rows": 0, "adapt_prior": 0.3, "knn_prior": 0.3, "eg": 0, "es": 0},
    }
    rows = dict(cal or cal_rows)
    if drop_cal:
        rows.pop(drop_cal)
    meta = {"schema": "field-sex/2", "rule": RULE, "verified": ["Aa bb"], "species": species, "aliases": {"Xx hh": "Gg hh"},
            "display": "display.json", "calibration": "calibration.json"}
    json.dump(meta, open(os.path.join(folder, "model.json"), "w"))
    json.dump(disp, open(os.path.join(folder, "display.json"), "w"))
    json.dump({"schema": "field-sex-calibration/1", "date": "test", "rule": {"band": 0.85}, "species": rows}, open(os.path.join(folder, "calibration.json"), "w"))
    np.savez(os.path.join(folder, "adapt.npz"), **ad)
    manifest(folder)


def make_v1(folder):
    """The verified species alone in a field-sex/1 folder (adapter row = Eg[1] + Es[1])."""
    os.makedirs(os.path.join(folder, "bank"))
    np.save(os.path.join(folder, "bank", "Aa_bb.X.npy"), XA.astype(np.float16)); np.save(os.path.join(folder, "bank", "Aa_bb.y.npy"), yA.astype(np.int8))
    meta = {"schema": "field-sex/1", "rule": RULE, "aliases": {}, "display": "display.json",
            "species": {"Aa bb": {"index": 0, "bank": "bank/Aa_bb", "adapt_prior": 0.7, "knn_prior": 0.7}}}
    json.dump(meta, open(os.path.join(folder, "model.json"), "w"))
    json.dump(disp, open(os.path.join(folder, "display.json"), "w"))
    np.savez(os.path.join(folder, "adapt.npz"), **{**ad, "Eg": ad["Eg"][1:2], "Es": ad["Es"][1:2]})
    manifest(folder)


def knn_ref(X, y, q, prior):
    sim = X.astype(np.float16).astype(np.float32) @ q
    kk = min(15, len(sim)); idx = np.argpartition(-sim, kk - 1)[:kk]; ss = sim[idx]
    w = np.exp((ss - ss.max()) / 0.05); w = w / w.sum() * kk
    return ((w * y[idx]).sum() + 2 * prior) / (kk + 2)


def calibrated_ref(row, p):
    q = min(max(p, 1e-4), 1 - 1e-4)
    pc = 1 / (1 + math.exp(-(row["a"] * math.log(q / (1 - q)) + row["b"])))
    female = pc >= 0.5
    conf = min(max(pc, 1 - pc), min(0.95, row["cap_female"] if female else row["cap_male"]))
    return (conf if female else 1 - conf), female, conf


out = {}
tmp = tempfile.mkdtemp(prefix="field-sex-v2-check-")
try:
    A = os.path.join(tmp, "v2"); make_v2(A); fs = FieldSex(A)
    B = os.path.join(tmp, "v1"); make_v1(B); f1 = FieldSex(B)
    feats = unit(40)
    # the verified species: the same block as the field-sex/1 folder, apart from the version and the verified flag
    same = 0
    for q in feats:
        for gate, crop in ((0.95, "success"), (0.5, "success"), (0.95, "centre_crop_fallback")):
            a = fs.score(q, "Aa bb", gate, crop); b = f1.score(q, "Aa bb", gate, crop)
            a2 = {k: v for k, v in a.items() if k not in ("version", "verified")}; b2 = {k: v for k, v in b.items() if k not in ("version", "verified")}
            same += a2 == b2
    out["verified_same_as_v1"] = [same, 3 * len(feats)]
    out["verified_flag"] = fs.score(feats[0], "Aa bb", 0.95, "success")["verified"]
    # estimated species: kNN pools (own rows, the genus' rows, none) against the formulas
    q = feats[1]
    out["knn_species_diff"] = abs(fs.knn(q, "Gg hh") - knn_ref(XG, yG, q, 0.5))
    out["knn_genus_diff"] = abs(fs.knn(q, "Gg rare") - knn_ref(np.concatenate([XG, XR]), np.concatenate([yG, yR]), q, 0.6))
    out["knn_empty"] = fs.knn(q, "Kk solo")
    # the estimated block: level, training photos, the capped marker, the % (inside the band too)
    blocks = {}
    worst = {"over_cap": 0, "over_95": 0, "pct_mismatch": 0, "marker_mismatch": 0, "sure_mismatch": 0, "estimates": 0, "inside_band_with_pct": 0}
    for sp, name in (("Gg hh", "Gg hh"), ("Gg rare", "Gg rare"), ("Kk solo", "Kk solo"), ("Gg hh", "Xx hh")):
        for qq in feats:
            blk = fs.score(qq, name, 0.95, "success")
            p = fs.ens(qq, sp)
            marker, female, conf = calibrated_ref(cal_rows[sp], p)
            cap = min(0.95, cal_rows[sp]["cap_female"] if female else cal_rows[sp]["cap_male"])
            worst["estimates"] += 1
            worst["over_cap"] += blk["pct"] > math.floor(100 * cap + 1e-9)
            worst["over_95"] += blk["pct"] > 95
            worst["pct_mismatch"] += blk["pct"] != int(math.floor(100 * conf + 1e-9))
            worst["marker_mismatch"] += abs(blk["p"] - round(marker, 3)) > 1e-9
            worst["sure_mismatch"] += blk["sure"] != (conf >= 0.85)
            worst["inside_band_with_pct"] += (not blk["sure"]) and blk.get("pct") is not None
        blocks[name] = blk
    out["estimate_checks"] = worst
    out["estimate_block"] = blocks["Gg hh"]
    out["alias_block_species"] = blocks["Xx hh"]["species"]
    out["rare_block"] = blocks["Gg rare"]
    out["solo_block"] = blocks["Kk solo"]
    out["estimate_unsure"] = fs.score(feats[2], "Gg hh", 0.7999, "success")
    out["estimate_no_wings"] = fs.score(feats[2], "Gg hh", 0.95, "centre_crop_fallback")
    out["outside"] = fs.score(feats[2], "Mechanitis messenoides", 0.99, "success")
    out["cap_max"] = ESTIMATE_CAP_MAX
    # a strong ENS on a species whose cap is 0.99 still shows at most 95
    strong = [fs.calibrated("Kk solo", p) for p in (0.999, 0.0001)]
    out["strong_capped"] = [[round(m, 4), s, round(c, 4)] for m, s, c in strong]
    # an estimated species without a level-table row: the folder is refused
    C = os.path.join(tmp, "nocal"); make_v2(C, drop_cal="Gg rare")
    try:
        FieldSex(C); out["missing_row"] = "loaded"
    except ValueError as e:
        out["missing_row"] = str(e)
    # the real level table: every species row, a grid of ENS values
    R = REAL_CAL["species"]
    levels = {}; bad = []; b1_sure = 0; b1_max = 0
    grid = np.concatenate([np.linspace(0.0001, 0.9999, 199), [1e-6, 1 - 1e-6]])
    for sp, row in R.items():
        levels[row["level"]] = levels.get(row["level"], 0) + 1
        if row["level"] not in ("species", "genus", "family", "global"):
            bad.append(sp)
        for p in grid:
            m, female, conf = calibrated_ref(row, float(p))
            pct = int(math.floor(100 * conf + 1e-9))
            if not (50 <= pct <= 95):
                bad.append((sp, float(p), pct))
            if row["bin"] == "B1":
                b1_max = max(b1_max, pct); b1_sure += conf >= 0.85
    out["real"] = {"species": len(R), "levels": levels, "bad": bad[:5], "n_bad": len(bad), "b1_sure": b1_sure, "b1_max_pct": b1_max}
finally:
    shutil.rmtree(tmp)
print(json.dumps(out))
