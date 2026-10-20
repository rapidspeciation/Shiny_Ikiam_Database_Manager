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
   "ecuador": true|false, "sex": {"cam": "CAM036744", "d": key, "v": key, "species": "Genus species"|null}|null}
  -> {"id": "1", "photos": {key: {species, subspecies, genus, crop, wingBox, ms} | {error}},
      "combined": {species, subspecies, genus, used: [keys]} (the mean of the photos' features, identical copies
      once, as the 18 Oct 2026 readings), "sex": {pMale, status, fold, ...} | {status: "not_scored:<why>"} | null}
Sex: the collection sex model of 14 Oct 2026 (VF_DH_PARTS + ANDRO_TILES, role heads; the cand_cv models of the
specimen's fold, mean of 3 seeds) on 12 crops of the d and v photos (4 wing crops + 8 androconial tiles, boxes from
the 4-wing detector yolo_wings_best.pt), with the same image encoder as the species, and its supported screen
(t 0.85, a 0.80, 3 of each sex) against the gallery's out-of-fold evidence. Files under --dir/sex (setup.sh):
  yolo_wings_best.pt, cand_cv/seed*_fold*.pt, train_sex.py (its Net), sex_predictions.json (the gallery's).

Files under --dir (shared/identifier on the server, tools/identifier/setup.sh):
  app/v6_feature.py, app/scripts/wings_v6/...    the Space's crop code (18 Oct 2026: knows V5N)
  app/assets/wing_seg_v5n.pt, wing_seg_v5n.onnx, field_wingtip_override_v1.json, head_hier.pt
  app/assets/region_checklist.json               (optional: the geographic prior)
  onnx/bioclip25h_visual_int8.onnx
"""

import argparse
import gc
import hashlib
import io
import json
import math
import os
import select
import sys
import time

HEAD_SHA256 = "749fbffd5f73f12c0f8598b5b2e3f575c0510b93a61c3f3c0123ab2ee8990830"
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
    p.add_argument("--sex-encoder", default=None, help="another image encoder for the sex model's crops (a path under --dir; default: --encoder)")
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

        head_path = os.path.join(assets, "head_hier.pt")
        with open(head_path, "rb") as f:
            if hashlib.sha256(f.read()).hexdigest() != HEAD_SHA256:
                raise ValueError("head_hier.pt is not the release's head (SHA-256 differs)")
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
            sex = sex_model.predict(job["sex"], paths, self)
        return {"photos": photos, "combined": combined, "sex": sex, "ms": round((time.perf_counter() - t0) * 1000), "rssMb": rss_mb()}


class SexModel:
    """The collection sex model of 14 Oct 2026 with its crops and screen (photo_readings_20261018 readings.py)."""

    CANVAS, SIZE, GREY, PAD = 512, 224, (128, 128, 128), 0.15
    THRESHOLD, A, MIN_EACH = 0.85, 0.80, 3

    def __init__(self, root, threads, encoder=None):
        import torch
        from PIL import Image, ImageOps
        from ultralytics import YOLO

        here = os.path.join(root, "sex")
        sys.path.insert(0, here)
        import train_sex as T  # noqa: E402  (release_20261014/collection/scripts/train_sex.py 8624e713)

        self.T, self.torch, self.Image, self.ImageOps = T, torch, Image, ImageOps
        self.encoder = None
        if encoder:
            import onnxruntime as ort

            so = ort.SessionOptions()
            so.intra_op_num_threads = threads
            so.inter_op_num_threads = 1
            self.encoder = ort.InferenceSession(os.path.join(root, encoder), so, providers=["CPUExecutionProvider"])
        self.yolo = YOLO(os.path.join(here, "yolo_wings_best.pt"))
        self.heads = {}
        import glob

        for path in sorted(glob.glob(os.path.join(here, "cand_cv", "seed*_fold*.pt"))):
            ck = torch.load(path, map_location="cpu", weights_only=False)
            if ck.get("schema") != "collection-sex-head/2":
                raise ValueError(f"{os.path.basename(path)}: not a collection sex head")
            net = T.Net(1024, 0, len(ck["role_types"]), ck["config"])
            net.load_state_dict({k: v.float() for k, v in ck["head"].items()}, strict=True)
            self.heads.setdefault(int(ck["fold"]), []).append((net.eval(), ck["role_types"], ck["seed"]))
        if sorted(self.heads) != [0, 1, 2, 3, 4]:
            raise ValueError("the sex model needs cand_cv heads for folds 0-4")
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

    # --- the crops, as readings.py copied them from the rendering modules
    def bounds(self, box, size):
        w, h = size
        x1, y1, x2, y2 = box
        dx, dy = (x2 - x1) * self.PAD, (y2 - y1) * self.PAD
        return (max(0, math.floor((x1 - dx) * w)), max(0, math.floor((y1 - dy) * h)), min(w, math.ceil((x2 + dx) * w)), min(h, math.ceil((y2 + dy) * h)))

    def load(self, path):
        with self.Image.open(path) as im:
            if im.getexif().get(274, 1) != 1:
                raise ValueError("unexpected_exif_orientation")
            return self.ImageOps.exif_transpose(im).convert("RGB")

    def model_input(self, crop):
        Image = self.Image
        if crop.width < 1 or crop.height < 1:
            raise ValueError("empty_crop")
        scale = min(self.CANVAS / crop.width, self.CANVAS / crop.height)
        width, height = max(1, round(crop.width * scale)), max(1, round(crop.height * scale))
        canvas = Image.new("RGB", (self.CANVAS, self.CANVAS), self.GREY)
        canvas.paste(crop.resize((width, height), Image.Resampling.BICUBIC), ((self.CANVAS - width) // 2, (self.CANVAS - height) // 2))
        image = canvas.resize((self.SIZE, self.SIZE), Image.Resampling.BICUBIC)
        b = io.BytesIO()  # the training crops went through PNG files
        image.save(b, format="PNG", optimize=False, compress_level=6)
        return Image.open(io.BytesIO(b.getvalue())).convert("RGB")

    def tiles(self, image, box, role):
        x1, y1, x2, y2 = self.bounds(box, image.size)
        mid = (y1 + y2) // 2
        top, bottom = (y1, mid) if role.startswith("dh") else (mid, y2)
        xm = (x1 + x2) // 2
        return [(f"{role}_t0", image.crop((x1, top, xm, bottom))), (f"{role}_t1", image.crop((xm, top, x2, bottom)))]

    def boxes(self, path):
        d = self.yolo.predict([path], conf=0.25, verbose=False)[0]
        if d is None or d.boxes is None or len(d.boxes) == 0:
            return []
        boxes, confs = d.boxes.xyxyn.cpu().numpy(), d.boxes.conf.cpu().numpy()
        idx = [i for i in range(len(boxes)) if confs[i] >= 0.5] or list(range(len(boxes)))
        if len(idx) > 4:
            area = [(boxes[i][2] - boxes[i][0]) * (boxes[i][3] - boxes[i][1]) for i in idx]
            idx = [i for _, i in sorted(zip(area, idx), reverse=True)[:4]]
        keep = set(idx)
        return [{"box": [round(float(x), 4) for x in boxes[i]], "conf": round(float(confs[i]), 2)} for i in range(len(boxes)) if i in keep]

    @staticmethod
    def rows(boxes):
        if len(boxes) != 4:
            raise ValueError(f"requires_exactly_four_boxes:{len(boxes)}")
        for box in boxes:
            x1, y1, x2, y2 = box["box"]
            if not (0 <= x1 < x2 <= 1 and 0 <= y1 < y2 <= 1):
                raise ValueError("invalid_normalized_box")
        rows = sorted(boxes, key=lambda b: (b["box"][1] + b["box"][3]) / 2)
        if (rows[2]["box"][1] + rows[2]["box"][3]) / 2 - (rows[1]["box"][1] + rows[1]["box"][3]) / 2 < 0.02:
            raise ValueError("ambiguous_vertical_rows")
        top = sorted(rows[:2], key=lambda b: (b["box"][0] + b["box"][2]) / 2)
        bottom = sorted(rows[2:], key=lambda b: (b["box"][0] + b["box"][2]) / 2)
        for pair in (top, bottom):
            if (pair[1]["box"][0] + pair[1]["box"][2]) / 2 - (pair[0]["box"][0] + pair[0]["box"][2]) / 2 < 0.03:
                raise ValueError("ambiguous_horizontal_separation")
        return top, bottom

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

    def predict(self, job, paths, ident):
        np, torch = ident.np, self.torch
        cam = job["cam"].upper()
        try:
            selected, conf = {}, []
            for v in ("d", "v"):
                boxes = self.boxes(paths[job[v]])
                try:
                    selected[v] = self.rows(boxes)
                except ValueError as e:
                    raise ValueError(f"{v}:{e}") from e
                conf += [b["conf"] for row in selected[v] for b in row]
            images = {v: self.load(paths[job[v]]) for v in ("d", "v")}
            if images["d"].size != images["v"].size:
                raise ValueError("unequal_paired_source_dimensions")
            d_top, d_bottom = selected["d"]
            v_top, _ = selected["v"]
            chosen = [("v", "vf_l", v_top[0]), ("v", "vf_r", v_top[1]), ("d", "dh_l", d_bottom[0]), ("d", "dh_r", d_bottom[1])]
            crops = [(part, self.model_input(images[v].crop(self.bounds(box["box"], images[v].size)))) for v, part, box in chosen]
            for v, part, box in chosen:
                crops += [(role, self.model_input(c)) for role, c in self.tiles(images[v], box["box"], part)]
        except (ValueError, OSError, KeyError) as e:
            return {"status": f"not_scored:{e}"[:200]}
        x = np.stack([(np.asarray(im, np.float32).transpose(2, 0, 1) / 255.0 - ident.mean) / ident.std for _, im in crops]).astype(np.float32)
        encoder = self.encoder or ident.encoder
        try:
            f = encoder.run(None, {"pixel_values": x})[0]
        except Exception:  # an encoder exported for one picture at a time
            f = np.concatenate([encoder.run(None, {"pixel_values": x[i:i + 1]})[0] for i in range(len(x))])
        f = f / np.linalg.norm(f, axis=1, keepdims=True)
        fold, how = self.fold_of(cam)
        roles = [r for r, _ in crops]
        seeds = {}
        with torch.no_grad():
            for net, types, seed in self.heads[fold]:
                rt = torch.tensor([[types.index(self.T.role_type(r)) for r in roles]])
                mask = torch.ones(1, len(roles), dtype=torch.bool)
                seeds[str(seed)] = float(net(torch.from_numpy(f.astype(np.float32)).unsqueeze(0), mask, rt).softmax(1)[0, 1])
        pm = float(np.mean(list(seeds.values())))
        supported, status = self.screen(cam, job.get("species"), pm)
        return {"pMale": round(pm, 4), "status": status, "supported": bool(supported and max(pm, 1 - pm) >= 0.8), "fold": fold, "foldHow": how,
                "seeds": {k: round(v, 4) for k, v in seeds.items()}, "minBoxConf": min(conf) if conf else None}


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
            first = model is None
            if first:
                model = Identifier(ARGS.dir, ARGS.encoder, ARGS.threads)
            if job.get("op") == "specimen":
                if job.get("sex") and sex_model is None:
                    try:
                        sex_model = SexModel(ARGS.dir, ARGS.threads, ARGS.sex_encoder)
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
