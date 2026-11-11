"""The general (field) sex line of Identificar «En línea»: experimental, six species, live photos only.

The model is WingsClassificator's September 2026 single-photo sex model (reports/field_sex_20260928), scored once on
fresh iNaturalist photos (reports/general_sex_20261111: PASS_RULE.md and RESULTS.md, 11 Nov 2026). It reads the same
1,024-D BioCLIP feature the identifier already makes for the photo, so it adds no image work:
  ENS = mean of the ADAPT head's P(female) and a within-species kNN P(female) over the training photos of that
  species (k 15, temperature 0.05, 2 pseudo-counts toward the species' female rate), exactly as the fresh test's
  scorer (score_fresh.py, which calls train_eval.py knn_within_species and predict).
A call is Supported when the identifier's species (head 6fa6ab78's species marginal WITHOUT the geographic prior) is
one of the six species that passed the fresh test, at >= 0.8, and the ENS confidence max(p, 1 - p) is >= 0.85. The
fresh test licenses only that: live photos, that gate, these six species. The page asks whether the photo is of a live
butterfly before it shows anything.

The % shown (Franz, 11 Nov 2026): per called sex, an isotonic map of the confidence to how often that call was right
(September cross-fitted rows), capped by the fresh precision of that species and called sex (or of the six pooled
when a species has fewer than 20 such calls), at most 99 %, rounded down. A species and called sex under 90 % on the
fresh test shows no call at all. Those caps were set on the fresh test itself: unvalidated until the next fresh read.

Files (--dir/field_sex, from WingsClassificator reports/general_sex_20261111/scripts/export_field_sex.py; team-only,
never public: the bank holds features of iNaturalist photos):
  model.json        species (six), their priors, adapter indices and evidence rows; aliases; rule
  display.json      the display rule: per-called-sex maps and the species x called sex table (shown, cap)
  adapt.npz         lin_w, lin_b, c, P (32 x 1024), Eg and Es rows of the six species
  bank/<Genus_species>.X.npy (fp16, one row per training photo) and .y.npy (1 = female)
  MANIFEST.sha256   every file's SHA-256, checked when the folder loads
Pure numpy: no torch, no image code, so it is tested on its own (tests/field-sex.test.mjs).
"""

import hashlib
import json
import math
import os

import numpy as np

SCHEMA = "field-sex/1"
CHUNK = 8192  # bank rows per matrix product (a Monarch has 30,260: the float32 copy stays small)


def _floor_pct(v):
    """Whole percent, rounded down; 0.99 is 99, not 98 (float noise)."""
    return int(math.floor(100 * v + 1e-9))


class FieldSex:
    def __init__(self, folder, check=True):
        self.folder = folder
        meta = json.load(open(os.path.join(folder, "model.json")))
        if meta.get("schema") != SCHEMA:
            raise ValueError(f"{folder}: not a {SCHEMA} folder")
        if check:
            self.verify()
        self.meta = meta
        rule = meta["rule"]
        self.t, self.gate = float(rule["t"]), float(rule["gate"])
        knn = rule["knn"]
        self.k, self.temp, self.pseudo = int(knn["k"]), float(knn["temp"]), float(knn["pseudo"])
        self.species = meta["species"]
        self.aliases = dict(meta.get("aliases") or {})
        z = np.load(os.path.join(folder, "adapt.npz"))
        self.lin_w = z["lin_w"].astype(np.float64)
        self.lin_b = float(z["lin_b"].reshape(-1)[0])
        self.c = float(z["c"])
        self.P = z["P"].astype(np.float64)
        self.adapter = (z["Eg"] + z["Es"]).astype(np.float64)  # one row per species index
        d = json.load(open(os.path.join(folder, meta.get("display") or "display.json")))
        self.maps = {sx: (np.array([p[0] for p in m["points"]], np.float64), np.array([p[1] for p in m["points"]], np.float64)) for sx, m in d["maps"].items()}
        self.cap_max = float(d["cap_max"])
        self.table = {(r["species"], r["called"]): r for r in d["table"]}
        self.display_version = d["version"]

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
        """The sex model's name for the identifier's species, or None when it is not one of the passing species."""
        name = self.aliases.get(app_name, app_name)
        return name if name in self.species else None

    def adapt(self, x, sp):
        e = self.species[sp]
        pp = min(max(float(e["adapt_prior"]), 1e-4), 1 - 1e-4)
        z = float(x @ self.lin_w) + self.lin_b + self.c * math.log(pp / (1 - pp))
        z += float(self.adapter[e["index"]] @ (self.P @ x))
        return 1 / (1 + math.exp(-z))

    def knn(self, x32, sp):
        e = self.species[sp]
        base = os.path.join(self.folder, e["bank"])
        X = np.load(base + ".X.npy", mmap_mode="r")
        y = np.load(base + ".y.npy")
        sims = np.concatenate([np.asarray(X[i:i + CHUNK], dtype=np.float32) @ x32 for i in range(0, X.shape[0], CHUNK)])
        kk = min(self.k, len(sims))
        idx = np.argpartition(-sims, kk - 1)[:kk]
        ss = sims[idx]
        w = np.exp((ss - ss.max()) / self.temp)
        w = w / w.sum() * kk
        p0 = float(e["knn_prior"])
        return float(((w * y[idx]).sum() + self.pseudo * p0) / (kk + self.pseudo))

    def ens(self, feat, sp):
        """P(female) of the ENS for a feature (1 x 1024 or 1024, L2-normalised) of species `sp` (sex-model name)."""
        x32 = np.asarray(feat, np.float32).reshape(-1)
        return (self.adapt(x32.astype(np.float64), sp) + self.knn(x32, sp)) / 2

    def shown_pct(self, sp, called, conf):
        """The whole % shown for a Supported call, or None when this species and called sex never shows a call."""
        row = self.table.get((sp, called))
        if not row or not row.get("shown"):
            return None
        xs, ys = self.maps[called]
        v = min(float(np.interp(conf, xs, ys)), self.cap_max, float(row["cap"]))
        return _floor_pct(v)

    def score(self, feat, top_name, top_p, crop):
        """The fieldSex block for one photo: None when the identifier's species (no prior) is not a passing species;
        else {species, gateP, status, ...} with status 'call' (sex, pct), 'below_gate', 'no_wings', 'not_supported'
        or 'suppressed'. top_name/top_p: head 6fa6ab78's top species marginal WITHOUT the geographic prior."""
        sp = self.model_name(top_name)
        if sp is None:
            return None
        out = {"species": top_name, "gateP": round(float(top_p), 4), "version": SCHEMA, "display": self.display_version}
        p = self.ens(feat, sp)
        conf = max(p, 1 - p)
        called = "female" if p >= 0.5 else "male"
        out.update(pFemale=round(p, 6), conf=round(conf, 6))
        supported = top_p >= self.gate and conf >= self.t  # all passing species are in the evidence table
        out["supported"] = bool(supported)
        if top_p < self.gate:
            out["status"] = "below_gate"
        elif not supported:
            out["status"] = "not_supported"
        elif crop != "success":
            out["status"] = "no_wings"
        else:
            pct = self.shown_pct(sp, called, conf)
            if pct is None:
                out["status"] = "suppressed"
            else:
                out.update(status="call", sex=called, pct=pct)
        return out
