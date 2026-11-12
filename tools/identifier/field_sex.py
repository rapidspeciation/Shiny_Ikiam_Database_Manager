"""The general (field) sex line of Identificar «En línea»: experimental, tested on live photos.

The model is WingsClassificator's September 2026 single-photo sex model (reports/field_sex_20260928), scored once on
fresh iNaturalist photos (reports/general_sex_20261111: PASS_RULE.md and RESULTS.md, 11 Nov 2026). It reads the same
1,024-D BioCLIP feature the identifier already makes for the photo, so it adds no image work:
  ENS = mean of the ADAPT head's P(female) and a within-species kNN P(female) over the training photos of that
  species (the genus when the species has fewer than 5; k 15, temperature 0.05, 2 pseudo-counts toward the species'
  female rate), exactly as the fresh test's scorer (score_fresh.py, which calls train_eval.py knn_within_species and
  predict).

Two kinds of species (BUILD.md «All species», 11 Nov 2026):
- **Verified** (the six that passed the fresh test): unchanged since Franz's phone test. A call is Supported when the
  identifier's species (head 6fa6ab78's species marginal WITHOUT the geographic prior) is >= 0.8 and the ENS
  confidence max(p, 1 - p) is >= 0.85. The % shown: per called sex, an isotonic map of the confidence (September
  cross-fitted rows), capped by the fresh precision of that species and called sex, at most 99 %, rounded down; a
  species and called sex under 90 % on the fresh test shows no call. Those caps were set on the fresh test itself.
- **Estimated** (every other species of the model, folder schema field-sex/2 only): the ENS is mapped to a calibrated
  P(female) by the level table calibration.json (one Platt map per species, fitted on the September OOF rows at the
  deepest level with enough photos: species, genus, family or all species, by training-photo count, shrunk toward
  its parent), and capped per level and called sex (at most 0.95). The bar sits at the capped value and the % is
  shown inside the band as well, once the species gate (0.8) passes and the wings were found. Not verified on fresh
  photos for that species: the page says so.

Files (--dir/field_sex, from WingsClassificator reports/general_sex_20261111/scripts/export_field_sex*.py; team-only,
never public: the bank holds features of iNaturalist photos):
  model.json        species, their priors, adapter indices and kNN pools; verified species; aliases; rule
  display.json      the verified six's display rule: per-called-sex maps and the species x called sex table (cap)
  calibration.json  (field-sex/2) the level table for the estimated species: a, b, caps, level per species
  adapt.npz         lin_w, lin_b, c, P (32 x 1024), Eg and Es rows
  bank/             field-sex/1: <Genus_species>.X.npy (fp16) and .y.npy per species;
                    field-sex/2: all.X.npy (fp16, one row per training photo, sorted by genus then species, opened
                    memory-mapped) and all.y.npy (1 = female); a species' pool is a row range of it
  MANIFEST.sha256   every file's SHA-256, checked when the folder loads
Pure numpy: no torch, no image code, so it is tested on its own (tests/field-sex.test.mjs).
"""

import hashlib
import json
import math
import os

import numpy as np

SCHEMAS = ("field-sex/1", "field-sex/2")
CHUNK = 8192  # bank rows per matrix product (a Monarch has 30,260: the float32 copy stays small)
ESTIMATE_CAP_MAX = 0.95  # an estimated (unverified) species never shows more, whatever the table says


def _floor_pct(v):
    """Whole percent, rounded down; 0.99 is 99, not 98 (float noise)."""
    return int(math.floor(100 * v + 1e-9))


class FieldSex:
    def __init__(self, folder, check=True):
        self.folder = folder
        meta = json.load(open(os.path.join(folder, "model.json")))
        if meta.get("schema") not in SCHEMAS:
            raise ValueError(f"{folder}: not a field sex folder ({', '.join(SCHEMAS)})")
        if check:
            self.verify()
        self.meta = meta
        self.schema = meta["schema"]
        rule = meta["rule"]
        self.t, self.gate = float(rule["t"]), float(rule["gate"])
        knn = rule["knn"]
        self.k, self.temp, self.pseudo = int(knn["k"]), float(knn["temp"]), float(knn["pseudo"])
        self.species = meta["species"]
        self.aliases = dict(meta.get("aliases") or {})
        # field-sex/1: every species in the folder is verified; field-sex/2 lists them
        self.verified = set(meta["verified"]) if self.schema == "field-sex/2" else set(self.species)
        z = np.load(os.path.join(folder, "adapt.npz"))
        self.lin_w = z["lin_w"].astype(np.float64)
        self.lin_b = float(z["lin_b"].reshape(-1)[0])
        self.c = float(z["c"])
        self.P = z["P"].astype(np.float64)
        self.Eg = z["Eg"].astype(np.float64)
        self.Es = z["Es"].astype(np.float64)
        d = json.load(open(os.path.join(folder, meta.get("display") or "display.json")))
        self.maps = {sx: (np.array([p[0] for p in m["points"]], np.float64), np.array([p[1] for p in m["points"]], np.float64)) for sx, m in d["maps"].items()}
        self.cap_max = float(d["cap_max"])
        self.floor = max(0.90, float(d.get("floor", 0.90)))  # never below 90 %, whatever the table says
        self.table = {(r["species"], r["called"]): r for r in d["table"]}
        self.display_version = d["version"]
        self.calib = None
        if self.schema == "field-sex/2":
            cal = json.load(open(os.path.join(folder, meta.get("calibration") or "calibration.json")))
            if cal.get("schema") != "field-sex-calibration/1":
                raise ValueError(f"{folder}: calibration.json is not field-sex-calibration/1")
            self.calib = cal["species"]
            self.calib_version = cal.get("date")
            self.band = float(cal["rule"]["band"])
            missing = [s for s in self.species if s not in self.verified and s not in self.calib]
            if missing:
                raise ValueError(f"field_sex: {len(missing)} estimated species have no calibration row ({missing[0]}, ...)")
            # one bank for all species, opened memory-mapped: only the rows of the photo's pool are read
            self.X = np.load(os.path.join(folder, "bank", "all.X.npy"), mmap_mode="r")
            self.y = np.load(os.path.join(folder, "bank", "all.y.npy"), mmap_mode="r")

    def verify(self):
        """Every file in MANIFEST.sha256 has its SHA-256, and nothing listed is missing."""
        listed = 0
        for line in open(os.path.join(self.folder, "MANIFEST.sha256")):
            if not line.strip():
                continue
            want, name = line.split(None, 1)
            name = name.strip()
            h = hashlib.sha256()
            with open(os.path.join(self.folder, name), "rb") as f:
                for block in iter(lambda: f.read(1 << 20), b""):
                    h.update(block)
            if h.hexdigest() != want:
                raise ValueError(f"field_sex/{name}: SHA-256 differs from MANIFEST.sha256")
            listed += 1
        if listed == 0:
            raise ValueError("field_sex: empty MANIFEST.sha256")

    def model_name(self, app_name):
        """The sex model's name for the identifier's species, or None when the model has no line for it."""
        name = self.aliases.get(app_name, app_name)
        return name if name in self.species else None

    def adapter(self, e):
        if "eg" in e:  # field-sex/2: rows of the full genus and species tables (0 = none)
            return self.Eg[int(e["eg"])] + self.Es[int(e["es"])]
        return self.Eg[e["index"]] + self.Es[e["index"]]  # field-sex/1: one row per species, in folder order

    def adapt(self, x, sp):
        e = self.species[sp]
        pp = min(max(float(e["adapt_prior"]), 1e-4), 1 - 1e-4)
        z = float(x @ self.lin_w) + self.lin_b + self.c * math.log(pp / (1 - pp))
        z += float(self.adapter(e) @ (self.P @ x))
        return 1 / (1 + math.exp(-z))

    def pool(self, sp):
        """(X, y) of the kNN pool: the species' training rows, or its genus' when it has fewer than 5; None if empty."""
        e = self.species[sp]
        if "pool" in e:
            lo, hi = e["pool"]
            if hi <= lo:
                return None
            return self.X[lo:hi], np.asarray(self.y[lo:hi])
        base = os.path.join(self.folder, e["bank"])
        return np.load(base + ".X.npy", mmap_mode="r"), np.load(base + ".y.npy")

    def knn(self, x32, sp):
        e = self.species[sp]
        p0 = float(e["knn_prior"])
        pool = self.pool(sp)
        if pool is None:  # no training photo of the species nor its genus: the prior (train_eval.py does the same)
            return p0
        X, y = pool
        sims = np.concatenate([np.asarray(X[i:i + CHUNK], dtype=np.float32) @ x32 for i in range(0, X.shape[0], CHUNK)])
        kk = min(self.k, len(sims))
        idx = np.argpartition(-sims, kk - 1)[:kk]
        ss = sims[idx]
        w = np.exp((ss - ss.max()) / self.temp)
        w = w / w.sum() * kk
        return float(((w * y[idx]).sum() + self.pseudo * p0) / (kk + self.pseudo))

    def ens(self, feat, sp):
        """P(female) of the ENS for a feature (1 x 1024 or 1024, L2-normalised) of species `sp` (sex-model name)."""
        x32 = np.asarray(feat, np.float32).reshape(-1)
        return (self.adapt(x32.astype(np.float64), sp) + self.knn(x32, sp)) / 2

    def shown_pct(self, sp, called, conf):
        """The whole % shown for a Supported call of a verified species, or None when this species and called sex never
        shows a call, or when the value would be under the table's floor (90 %: no call is shown under it)."""
        row = self.table.get((sp, called))
        if not row or not row.get("shown"):
            return None
        xs, ys = self.maps[called]
        v = min(float(np.interp(conf, xs, ys)), self.cap_max, float(row["cap"]))
        pct = _floor_pct(v)
        return pct if pct >= _floor_pct(self.floor) else None

    def calibrated(self, sp, p):
        """An estimated species' bar: (marker P(female), the side it leans to, shown confidence) from the ENS p, by its
        row of the level table: Platt map, then the leaning sex's cap (at most 0.95). The marker is the capped value."""
        r = self.calib[sp]
        q = min(max(p, 1e-4), 1 - 1e-4)
        v = float(r["a"]) * math.log(q / (1 - q)) + float(r["b"])
        pc = 1 / (1 + math.exp(-max(-50.0, min(50.0, v))))
        female = pc >= 0.5
        cap = min(ESTIMATE_CAP_MAX, float(r["cap_female"] if female else r["cap_male"]))
        conf = min(max(pc, 1 - pc), cap)
        return (conf if female else 1 - conf), ("female" if female else "male"), conf

    def score(self, feat, name, gate_p, crop):
        """The fieldSex block for one photo, or None when `name` (the species the page names) has no line.

        Verified six (verified: true), unchanged: p (P(female), 3 decimals), sex (the side it leans to, female when
        p >= 0.5) and sure (confidence max(p, 1 - p) >= t, outside the «no seguro» band, P 0.15 to 0.85). Then one
        status, first that applies:
          species_unsure  gate_p (head 6fa6ab78's no-prior score for `name`) under 0.8: the species is not sure
          no_wings        the wings were not found (the crop fell back)
          band            confidence under t (0.85): «no seguro», leaning to `sex`
          cell_under_90   a Supported answer whose species x sex cell (or its %) is under 90 % on the fresh test
          call            a Supported answer: sex and pct (90 to 99), the frozen rule's served condition
        Estimated species (verified: false): p is the calibrated, capped P(female), sure is the same band on it, and
        level / trainPhotos say where the calibration comes from. Status species_unsure or no_wings as above, with no %
        and sure false (the page then names no sex, only the side it leans to: the table is for the named species and
        a crop without wings), else estimate with pct (50 to 95), inside the band too."""
        sp = self.model_name(name)
        if sp is None:
            return None
        if sp not in self.verified:
            return self.estimate(feat, sp, name, gate_p, crop)
        p = self.ens(feat, sp)
        conf = max(p, 1 - p)
        called = "female" if p >= 0.5 else "male"
        sure = conf >= self.t
        out = {"species": name, "gateP": round(float(gate_p), 4), "version": self.schema, "display": self.display_version,
               "verified": True, "p": round(p, 3), "pFemale": round(p, 6), "sex": called, "sure": bool(sure),
               "supported": bool(gate_p >= self.gate and sure)}  # all verified species are in the evidence table
        if gate_p < self.gate:
            out["status"] = "species_unsure"
        elif crop != "success":
            out["status"] = "no_wings"
        elif not sure:
            out["status"] = "band"
        else:
            pct = self.shown_pct(sp, called, conf)
            if pct is None:
                out["status"] = "cell_under_90"
            else:
                out.update(status="call", pct=pct)
        return out

    def estimate(self, feat, sp, name, gate_p, crop):
        p = self.ens(feat, sp)
        marker, sex, conf = self.calibrated(sp, p)
        r = self.calib[sp]
        out = {"species": name, "gateP": round(float(gate_p), 4), "version": self.schema, "calibration": self.calib_version,
               "verified": False, "level": r["level"], "trainPhotos": int(r["rows"]),
               "p": round(marker, 3), "pFemale": round(p, 6), "sex": sex, "sure": bool(conf >= self.band)}
        if gate_p < self.gate:
            out.update(status="species_unsure", sure=False)  # the table is for the named species: no clear sex
        elif crop != "success":
            out.update(status="no_wings", sure=False)
        else:
            out.update(status="estimate", pct=_floor_pct(conf))
        return out
