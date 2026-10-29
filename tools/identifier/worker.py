"""The team's AI Identifier on the app server: which species a butterfly photo shows.

The same pipeline as the identifier's Space (WingsClassificator/hf-space, crop of 18 Oct 2026):
the photo upright by its EXIF tag, the nano V5N wing detector at 640 px (as ONNX with its one-to-one
head, the head the Space's PyTorch model runs), the wings cropped (the central 90 % when nothing is
found) and padded to a 224 px square, BioCLIP 2.5-H's image encoder (ONNX, dynamic int8), then the
trained hierarchical head (15,347 names). Optionally the Space's geographic prior: names not recorded
on that side of the Andes in Ecuador weigh 2 %.

Started by the app (server/identifier.mjs), one per app. JSON lines on stdin, one answer per
line on stdout, one photo at a time:
  {"id": "1", "path": "/abs/photo.jpg", "side": "East"|"West"|null, "box": [x0,y0,x1,y1]|null}
  -> {"id": "1", "species": [[name, p], ...], "subspecies": [...], "genus": [...], "crop": ..., "ms": ...}
     or {"id": "1", "error": "..."}
The models load on the first photo (about 4 s, 2.3 GB) and the process ends after --idle seconds
without photos (the memory goes back to the server), or when stdin closes (the app stopped).

The background photo readings (server/readings-worker.mjs) start their own one (--nice 19, 2 threads) and ask
for a butterfly at a time:
  {"id": "1", "op": "specimen", "photos": [{"key": fileId, "path": ..., "dup": "d|812"}], "side": "East"|null,
   "ecuador": true|false, "sex": {"cam": "CAM036744", "d": key, "v": key, "species": "Genus species"|null,
   "wings": {"d": reading, "v": reading}}|null}
  -> {"id": "1", "photos": {key: {species, subspecies, genus, crop, wingBox, ms} | {error}},
      "combined": {species, subspecies, genus, used: [keys]} (the mean of the photos' features, identical copies
      once, as the 18 Oct 2026 readings), "sex": {pMale, status, fold, ...} | {status: "not_scored:<why>"} | null}
Sex: the collection sex model of 19 Oct 2026 (release_20261019_sexcnn cnn_strips4; --dir/sex: model.json,
cand_cv/seed*_fold*.onnx and sex_predictions.json, from setup.sh): a small network (MobileNetV3-Large) on the 4
androconial strips of the d and v photos (each dorsal hindwing's costal half, each ventral forewing's inner-margin half,
one 112 x 336 crop each), the wing boxes from the nano wing chain the readings already ran on those photos (the job's
sex.wings: {"d": reading, "v": reading}); no detector, no image encoder. It reads whatever strips a butterfly has (crop
dropout in training): one wing found is enough. The cand_cv networks of the specimen's fold, mean of 3 seeds, then the
supported screen (t 0.85, a 0.80, 3 of each sex) against that model's out-of-fold evidence (its sex_predictions.json).

The «Fotos» tab's sex check (server/sex-check.mjs) asks for the sex model alone (no image encoder, no torch):
  {"id": "1", "op": "sex", "photos": [{"key": "d", "path": ...}, {"key": "v", "path": ...}],
   "sex": {"cam", "d": "d", "v": "v", "species", "wings": {"d": reading, "v": reading}}} -> {"id": "1", "sex": {...}}

Files under --dir (shared/identifier on the server, tools/identifier/setup.sh):
  app/v6_feature.py, app/scripts/wings_v6/...    the Space's crop code (18 Oct 2026: knows V5N)
  app/assets/wing_seg_v5n.pt, wing_seg_v5n.onnx, field_wingtip_override_v1.json, head_hier_b8127dbf.pt
  app/assets/region_checklist.json               (optional: the geographic prior)
  onnx/bioclip25h_visual_int8.onnx
"""

import argparse
import gc
import hashlib
import json
import math
import os
import select
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))  # -I leaves the worker's own folder out
import sex_crops  # noqa: E402

HEAD_SHA256 = "b8127dbf0591630c99a9e4af1c00eb8d50f217acfb795fe9e23093702e816d42"
# Its own file name: head_hier.pt (749fbffd) is hard-linked with ~/bioclip-bench and stays for the previous release.
HEAD_FILE = "head_hier_b8127dbf.pt"
DETECTOR = "wing_seg_v5n"
MEAN = (0.48145466, 0.4578275, 0.40821073)
STD = (0.26862954, 0.26130258, 0.27577711)
TOP = 5


def parse_args():
    p = argparse.ArgumentParser()
    p.add_argument("--dir", required=True)
    p.add_argument("--encoder", default="onnx/bioclip25h_visual_int8.onnx")
    p.add_argument("--threads", type=int, default=3)
    p.add_argument("--idle", type=float, default=600)
    p.add_argument("--no-pin", action="store_true", help="no affinity or lower priority (measuring)")
    p.add_argument("--nice", type=int, default=5, help="how much lower the priority (the background readings: 19)")
    return p.parse_args()


ARGS = parse_args()
# The app keeps a core: the worker's threads on the last cores, at a lower priority than the app.
try:
    if ARGS.no_pin:
        raise OSError
    cpus = sorted(os.sched_getaffinity(0))
    if len(cpus) > ARGS.threads:
        os.sched_setaffinity(0, cpus[-ARGS.threads:])
    os.nice(ARGS.nice)
except (AttributeError, OSError):
    pass
for var in ("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS"):
    os.environ[var] = str(ARGS.threads)


def rss_mb():
    try:
        for line in open("/proc/self/status"):
            if line.startswith("VmRSS:"):
                return int(line.split()[1]) // 1024
    except OSError:
        return None


# The answers have stdout to themselves: what the libraries print goes to stderr.
OUT = os.fdopen(os.dup(1), "w")
os.dup2(2, 1)
sys.stdout = sys.stderr


def send(obj):
    OUT.write(json.dumps(obj, separators=(",", ":")) + "\n")
    OUT.flush()


class Identifier:
    def __init__(self, root, encoder, threads):
        t0 = time.perf_counter()
        sys.path.insert(0, os.path.join(root, "app"))
        import numpy as np
        import onnxruntime as ort
        import torch
        from torch import nn
        import torch.nn.functional as F

        torch.set_num_threads(threads)

        # ultralytics opens the detector's ONNX session without options: it would use every core.
        base = ort.InferenceSession

        class Limited(base):
            def __init__(self, path, sess_options=None, *a, **k):
                if sess_options is None:
                    sess_options = ort.SessionOptions()
                    sess_options.intra_op_num_threads = threads
                    sess_options.inter_op_num_threads = 1
                super().__init__(path, sess_options, *a, **k)

        ort.InferenceSession = Limited

        so = ort.SessionOptions()
        so.intra_op_num_threads = threads
        so.inter_op_num_threads = 1
        so.graph_optimization_level = ort.GraphOptimizationLevel.ORT_ENABLE_ALL
        self.encoder = base(os.path.join(root, encoder), so, providers=["CPUExecutionProvider"])

        from v6_feature import V6FeatureExtractor
        from ultralytics import YOLO

        assets = os.path.join(root, "app", "assets")
        # The .pt names the detector and its input size (checked by SHA-256); the ONNX copy runs it.
        self.v6 = V6FeatureExtractor(os.path.join(assets, DETECTOR + ".pt"), os.path.join(assets, "field_wingtip_override_v1.json"))
        self.v6.detector = YOLO(os.path.join(assets, DETECTOR + ".onnx"), task="segment")
        gc.collect()

        head_path = os.path.join(assets, HEAD_FILE)
        with open(head_path, "rb") as f:
            if hashlib.sha256(f.read()).hexdigest() != HEAD_SHA256:
                raise ValueError(HEAD_FILE + " is not the release's head (SHA-256 differs)")
        ck = torch.load(head_path, map_location="cpu", weights_only=False)

        class CosineHead(nn.Module):
            def __init__(self, d, n, s=30.0, K=1):
                super().__init__()
                self.s, self.K, self.n = s, K, n
                self.layer_norm = nn.LayerNorm(d)
                self.weight = nn.Parameter(torch.empty(n * K, d))

            def forward(self, x):
                x = self.layer_norm(x)
                cos = F.linear(F.normalize(x, dim=1), F.normalize(self.weight, dim=1)).clamp(-1 + 1e-6, 1 - 1e-6)
                if self.K > 1:
                    cos = cos.view(cos.size(0), self.n, self.K).max(dim=2).values
                return self.s * cos

        self.leaves = list(ck["leaves"])
        self.head = CosineHead(ck["feat_dim"], len(self.leaves), K=ck.get("K", 1))
        self.head.load_state_dict(ck["head_state_dict"])
        self.head.eval()
        self.sp_vocab = list(ck["rank_vocab"]["species"])
        self.ge_vocab = list(ck["rank_vocab"]["genus"])
        self.leaf2sp = np.array(ck["leaf2rank"]["species"])
        self.leaf2ge = np.array(ck["leaf2rank"]["genus"])
        self.is_ssp = np.array([lf.count(" ") >= 2 for lf in self.leaves])
        del ck

        # The Space's geographic prior (hf-space/geo_prior.py), when its checklist is there.
        self.geo = None
        checklist = os.path.join(assets, "region_checklist.json")
        if os.path.exists(checklist):
            chk = json.load(open(checklist))
            n = len(self.leaves)
            known, inec, east, west = (np.zeros(n, bool) for _ in range(4))
            for i, lf in enumerate(self.leaves):
                e = chk.get(lf)
                if e is None:
                    p = lf.split()
                    e = chk.get(p[0] + " " + p[1]) if len(p) >= 2 else None
                if e is None:
                    continue
                known[i] = True
                inec[i] = e.get("ec", 0) == 1
                east[i] = e.get("East", 0) > 0
                west[i] = e.get("West", 0) > 0
            self.geo = {"known": known, "inec": inec, "East": east, "West": west}
        self.np, self.torch = np, torch
        self.mean = np.array(MEAN, np.float32)[:, None, None]
        self.std = np.array(STD, np.float32)[:, None, None]
        gc.collect()
        self.load_ms = round((time.perf_counter() - t0) * 1000)

    def weights(self, side, ecuador=None):
        """hf-space geo_prior.py leaf_weights, eps 0.02: off that side of the Andes, and (in Ecuador) not recorded
        there. `ecuador` None: as the side says (a side given means a place in Ecuador)."""
        np = self.np
        w = np.ones(len(self.leaves), np.float32)
        if ecuador is None:
            ecuador = side in ("East", "West")
        if self.geo is None or (side not in ("East", "West") and not ecuador):
            return w
        off = ~self.geo[side] if side in ("East", "West") else np.zeros(len(self.leaves), bool)
        if ecuador:
            off = off | ~self.geo["inec"]
        w[self.geo["known"] & off] = 0.02
        return w

    def feature(self, raw, box=None):
        """The photo's BioCLIP feature (normalised) and its crop's provenance."""
        np = self.np
        square, prov = self.v6.crop(raw, manual_box=box) if box else self.v6.crop(raw)
        x = ((np.asarray(square.convert("RGB"), np.float32).transpose(2, 0, 1) / 255.0 - self.mean) / self.std)[None]
        feat = self.encoder.run(None, {"pixel_values": x})[0]
        return feat / np.linalg.norm(feat, axis=1, keepdims=True), prov

    def tops(self, feat, side=None, ecuador=None):
        np, torch = self.np, self.torch
        with torch.no_grad():
            probs = self.head(torch.from_numpy(feat.astype(np.float32))).softmax(1)[0].numpy().astype(np.float64)
        w = self.weights(side, ecuador)
        prior = bool((w < 1).any())
        if prior:
            pr = probs * w
            probs = pr / pr.sum() if pr.sum() > 0 else probs

        def marg(leaf2r, vocab, k):
            agg = np.zeros(len(vocab))
            np.add.at(agg, leaf2r, probs)
            return [[vocab[j], round(float(agg[j]), 4)] for j in np.argsort(-agg, kind="stable")[:k]]

        order = np.argsort(-probs, kind="stable")
        subs = []
        for i in order:
            if self.is_ssp[i]:
                subs.append([self.leaves[i], round(float(probs[i]), 4)])
                if len(subs) >= TOP:
                    break
        return {
            "species": marg(self.leaf2sp, self.sp_vocab, TOP),
            "subspecies": subs,
            "genus": marg(self.leaf2ge, self.ge_vocab, 3),
            "top": [self.leaves[order[0]], round(float(probs[order[0]]), 4)],
            "prior": side if prior and side in ("East", "West") else ("Ecuador" if prior else None),
        }

    def identify(self, path, side=None, box=None, ecuador=None):
        t0 = time.perf_counter()
        with open(path, "rb") as f:
            raw = f.read()
        feat, prov = self.feature(raw, box)
        geometry = prov.get("geometry") or {}
        out = self.tops(feat, side, ecuador)
        if out["prior"] == "Ecuador":
            out["prior"] = None
        return {
            **out,
            "crop": geometry.get("status"),
            "wingBox": prov.get("wing_box"),
            "ms": round((time.perf_counter() - t0) * 1000),
            "rssMb": rss_mb(),
        }

    def specimen(self, job, sex_model):
        """A butterfly's photos one by one, their features' mean, and its sex (see the module's notes)."""
        np = self.np
        t0 = time.perf_counter()
        side, ecuador = job.get("side"), job.get("ecuador")
        photos, feats, used, seen = {}, {}, [], set()
        for p in job.get("photos", []):
            key = p["key"]
            try:
                t1 = time.perf_counter()
                with open(p["path"], "rb") as f:
                    raw = f.read()
                feat, prov = self.feature(raw)
                geometry = prov.get("geometry") or {}
                out = self.tops(feat, side, ecuador)
                out.pop("prior", None)
                photos[key] = {**out, "crop": geometry.get("status"), "wingBox": prov.get("wing_box"), "ms": round((time.perf_counter() - t1) * 1000)}
                feats[key] = feat
                # identical copies (same view and size, or the same bytes) count once in the mean
                k1, k2 = p.get("dup") or key, hashlib.sha1(raw).hexdigest()
                if k1 not in seen and k2 not in seen:
                    seen.update([k1, k2])
                    used.append(key)
            except Exception as e:  # one photo's problem is that photo's answer
                photos[key] = {"error": f"{type(e).__name__}: {e}"[:300]}
        combined = None
        if used:
            f = np.concatenate([feats[k] for k in used]).mean(0, keepdims=True)
            f = f / np.linalg.norm(f)
            combined = {**self.tops(f, side, ecuador), "used": used}
            combined.pop("prior", None)
        sex = None
        if job.get("sex"):
            paths = {p["key"]: p["path"] for p in job.get("photos", [])}
            sex = sex_model.predict(job["sex"], paths, np)
        return {"photos": photos, "combined": combined, "sex": sex, "ms": round((time.perf_counter() - t0) * 1000), "rssMb": rss_mb()}


class SexModel:
    """The collection sex model (CNN on the androconial strips) and its supported screen (readings.py's screen)."""

    THRESHOLD, A, MIN_EACH = 0.85, 0.80, 3

    def __init__(self, root, threads):
        from PIL import Image, ImageOps

        try:  # only to resize the strips (a model whose size is not the strips' own); the Fotos tab's check has none
            import torch
        except ImportError:
            torch = None

        here = os.path.join(root, "sex")
        self.torch, self.Image, self.ImageOps = torch, Image, ImageOps
        self.threads, self.here = threads, here
        meta = os.path.join(here, "model.json")
        if not os.path.exists(meta):
            raise ValueError(f"{here}: no model.json (not the 19 Oct 2026 CNN sex model; tools/identifier/setup.sh with SEX_SRC)")
        self.load_cnn(json.load(open(meta)))
        self.load_evidence(here)

    def load_cnn(self, meta):
        """One ONNX network per fold and seed (cand_cv/seed<seed>_fold<fold>.onnx) on the strips of the nano chain's
        wing boxes."""
        import glob

        if meta.get("schema") != "collection-sex-cnn-onnx/1":
            raise ValueError("model.json: not a collection sex CNN")
        self.meta, self.arm = meta, meta.get("arm")
        self.crops = list(meta["roles"])
        for c in self.crops:
            if c not in sex_crops.KNOWN:
                raise ValueError(f"unknown sex crop {c}")
        self.sessions, self.files = {}, {}
        for path in sorted(glob.glob(os.path.join(self.here, "cand_cv", "seed*_fold*.onnx"))):
            name = os.path.basename(path)
            seed, fold = name[4:-5].split("_fold")
            self.files.setdefault(int(fold), []).append((path, seed))
        if sorted(self.files) != [0, 1, 2, 3, 4] or any(len(v) != 3 for v in self.files.values()):
            raise ValueError("the sex model needs 3 cand_cv networks for each of folds 0-4")

    def session(self, fold):
        """The fold's 3 networks, opened the first time a butterfly of that fold comes."""
        if fold not in self.sessions:
            import onnxruntime as ort

            so = ort.SessionOptions()
            so.intra_op_num_threads = self.threads
            so.inter_op_num_threads = 1
            self.sessions[fold] = [(ort.InferenceSession(p, so, providers=["CPUExecutionProvider"]), seed) for p, seed in self.files[fold]]
        return self.sessions[fold]

    def load_evidence(self, here):
        pub = json.load(open(os.path.join(here, "sex_predictions.json")))
        self.folds, self.evidence = {}, {}
        for cam, e in pub.items():
            prov = e.get("evaluation_provenance") or {}
            fold = prov.get("outer_fold")
            if fold is None and prov.get("seeds"):
                fold = prov["seeds"][0].get("fold")
            if fold is not None:
                self.folds[cam.upper()] = int(fold)
            if e.get("oof") and e.get("recorded_sex") in ("male", "female"):
                pm = e["views"]["fused"]["male_probability"]
                if max(pm, 1 - pm) >= self.THRESHOLD:
                    self.evidence.setdefault(e.get("species"), []).append((cam.upper(), int(e["recorded_sex"] == "male"), pm))

    def fold_of(self, cam):
        cam = cam.upper()
        if cam in self.folds:
            return self.folds[cam], "gallery"
        group = "identity:" + hashlib.sha256(repr(("cam", cam)).encode()).hexdigest()[:24]
        return int(hashlib.sha256(group.encode()).hexdigest(), 16) % 5, "sha256(identity_group) mod 5"

    def load(self, path):
        with self.Image.open(path) as im:
            if im.getexif().get(274, 1) != 1:
                raise ValueError("unexpected_exif_orientation")
            return self.ImageOps.exif_transpose(im).convert("RGB")

    def screen(self, cam, species, pm):
        """analyze.py new_rule: (supported, status)."""
        conf = max(pm, 1 - pm)
        if conf < self.THRESHOLD:
            return False, "below_confidence"
        ev = [x for x in self.evidence.get(species, []) if x[0] != cam]
        nf = sum(1 for x in ev if x[1] == 0)
        nm = len(ev) - nf
        if nf < self.MIN_EACH or nm < self.MIN_EACH:
            return False, "species_evidence_lacks_both_sexes"
        k = sum(1 for x in ev if (x[2] >= 0.5) == bool(x[1]))
        n = len(ev)
        p, z = k / n, 1.96
        d = 1 + z * z / n
        lb = max(0.0, (p + z * z / (2 * n)) / d - z * math.sqrt(p * (1 - p) / n + z * z / (4 * n * n)) / d)
        return (lb >= self.A), ("species_supported" if lb >= self.A else "species_accuracy_bound_below_threshold")

    def predict(self, job, paths, np):
        """The nano chain's wings of the d and v photos (job["wings"]), the strips of the wings that are there (a mask
        says which; model.json use_available), the fold's 3 networks (mean of their P(male)); the preparation is the
        training's (train_sex.cnn_batch without changes)."""
        torch = self.torch
        cam = job["cam"].upper()
        t0 = time.perf_counter()
        partial = bool(self.meta.get("use_available"))
        try:
            boxes = sex_crops.nano_boxes(job.get("wings"), partial=partial)
            views = sorted({"v" if w.startswith("vf") else "d" for w in boxes})
            images = {v: self.load(paths[job[v]]) for v in views}
            cut = dict(sex_crops.cut_boxes(self.crops, images, boxes))
            stored = self.meta.get("stored") or [112, 336]
            blank = np.zeros((*stored, 3), np.uint8)
            crops = [sex_crops.cnn_input(cut[r], stored) if r in cut else blank for r in self.crops]
        except (ValueError, OSError, KeyError) as e:
            return {"status": f"not_scored:{e}"[:200]}
        present = [r in cut for r in self.crops]
        size = self.meta["size"]
        hw = tuple(size) if isinstance(size, list) else (int(size), int(size))
        x = np.stack(crops).transpose(0, 3, 1, 2).astype(np.float32) / 255
        if tuple(x.shape[-2:]) != hw:
            if torch is None:
                raise ValueError("the sex model's size needs torch to resize its strips")
            x = torch.nn.functional.interpolate(torch.from_numpy(x), size=hw, mode="bilinear", antialias=True, align_corners=False).numpy()
        mean = np.asarray(self.meta["mean"], np.float32).reshape(1, 3, 1, 1)
        std = np.asarray(self.meta["std"], np.float32).reshape(1, 3, 1, 1)
        x = ((x - mean) / std)[None].astype(np.float32)
        fold, how = self.fold_of(cam)
        seeds = {}
        feed = {"crops": x}
        if partial:
            feed["mask"] = np.asarray([present], np.float32)
        for s, seed in self.session(fold):
            lg = s.run(None, feed)[0][0].astype(np.float64)
            seeds[str(seed)] = float(1 / (1 + math.exp(lg[0] - lg[1])))
        pm = float(np.mean(list(seeds.values())))
        supported, status = self.screen(cam, job.get("species"), pm)
        return {"pMale": round(pm, 4), "status": status, "supported": bool(supported and max(pm, 1 - pm) >= 0.8), "fold": fold, "foldHow": how,
                "seeds": {k: round(v, 4) for k, v in seeds.items()}, "model": self.arm, "kind": "cnn", "crops": sum(present), "boxes": "nano",
                "ms": round((time.perf_counter() - t0) * 1000)}


def main():
    model = None
    sex_model = None
    buf = b""
    stdin = sys.stdin.buffer.raw if hasattr(sys.stdin.buffer, "raw") else sys.stdin.buffer
    fd = stdin.fileno()
    send({"ready": True, "pid": os.getpid()})
    while True:
        while b"\n" not in buf:
            readable, _, _ = select.select([fd], [], [], ARGS.idle)
            if not readable:
                send({"idle": True})
                return 0
            chunk = os.read(fd, 1 << 16)
            if not chunk:
                return 0
            buf += chunk
        line, buf = buf.split(b"\n", 1)
        if not line.strip():
            continue
        try:
            job = json.loads(line)
        except ValueError:
            continue
        jid = job.get("id")
        try:
            if job.get("op") == "ping":
                send({"id": jid, "loaded": model is not None})
                continue
            if job.get("op") == "sex":
                # the sex model alone (the «Fotos» tab's sex check, server/sex-check.mjs): no image encoder
                if not sex_model:
                    sex_model = SexModel(ARGS.dir, ARGS.threads)
                import numpy as np

                paths = {p["key"]: p["path"] for p in job.get("photos", [])}
                send({"id": jid, "sex": sex_model.predict(job["sex"], paths, np)})
                continue
            first = model is None
            if first:
                model = Identifier(ARGS.dir, ARGS.encoder, ARGS.threads)
            if job.get("op") == "specimen":
                if job.get("sex") and sex_model is None:
                    try:
                        sex_model = SexModel(ARGS.dir, ARGS.threads)
                    except Exception as e:  # without the sex model's files the species are still read
                        sex_model = False
                        print(f"Sex model not loaded: {type(e).__name__}: {e}"[:300], file=sys.stderr)
                out = model.specimen(job if sex_model else {**job, "sex": None}, sex_model)
                if job.get("sex") and not sex_model:
                    out["sex"] = {"status": "not_scored:sex_model_not_installed"}
            else:
                out = model.identify(job["path"], side=job.get("side"), box=job.get("box"), ecuador=job.get("ecuador"))
            if first:
                out["loadMs"] = model.load_ms
            send({"id": jid, **out})
        except Exception as e:  # one photo's problem is that photo's answer
            send({"id": jid, "error": f"{type(e).__name__}: {e}"[:300]})


if __name__ == "__main__":
    sys.exit(main())
