"""The team's AI Identifier on the app server: which species a butterfly photo shows.

The same pipeline as the identifier's Space (WingsClassificator/hf-space, crop of 18 Oct 2026):
the photo upright by its EXIF tag, the nano wing detector (V5N retrained, wing_seg_s2, since 5 Nov 2026) at 640 px (as ONNX with its one-to-one
head, the head the Space's PyTorch model runs), the wings cropped (the central 90 % when nothing is
found) and padded to a 224 px square, BioCLIP 2.5-H's image encoder (ONNX, dynamic int8), then either
the server-only Slot 1 3-backbone fusion (BioCLIP 2.5-H + DINOv2-S w1c + Meta DINOv3-S w1d with
head_slot1_3bb_dist_rel_7d54b54f.pt when all three fusion assets are present under --dir) or the legacy
hierarchical head (head_hier_6fa6ab78.pt, automatic fallback when any fusion asset is absent; 15,347 names).
Optionally the Space's geographic prior: names not recorded on that side of the Andes in Ecuador weigh 2 %.

Started by the app (server/identifier.mjs), one per app. JSON lines on stdin, one answer per
line on stdout, one photo at a time:
  {"id": "1", "path": "/abs/photo.jpg", "side": "East"|"West"|null, "box": [x0,y0,x1,y1]|null}
  -> {"id": "1", "species": [[name, p], ...], "subspecies": [...], "genus": [...], "tribe": [...], "subfamily": [...],
      "head": ..., "crop": ..., "ms": ...}   (tribe, subfamily: with taxonomy_6fa6ab78.json beside this file, placeholders left out)
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
  app/assets/wing_seg_s2.pt, wing_seg_s2.onnx, field_wingtip_override_v1.json, head_hier_6fa6ab78.pt
  app/assets/region_checklist.json               (optional: the geographic prior)
  onnx/bioclip25h_visual_int8.onnx
  field_sex/                                     (optional: the general sex line of «En línea», field_sex.py)

The general sex line (11 Nov 2026, experimental, field_sex.py): an online photo whose answer names a species of the
sex model gets a "fieldSex" block (None otherwise): the bar position, the side it leans to, a status and, when shown,
the %. Six species passed WingsClassificator's fresh test (verified); with a field-sex/2 folder every other species
of the model (except the 13 Ithomiini genera of the in-hand line) gets a calibrated estimate, marked not verified.
It reads the feature the photo already has. Without the folder, answers carry no fieldSex and nothing else changes.
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

HEAD_SHA256 = "6fa6ab78c6c3b76566a15d322061d2a5bec95c7dd705797c75c95799e1603552"
# Its own file name (5 Nov 2026: alias recipe retrained on V5N serving crops with label fixes, WingsClassificator
# reports/wingseg_next_20261103); head_hier_b8127dbf.pt and head_hier.pt (749fbffd, hard-linked) stay for earlier releases.
HEAD_FILE = "head_hier_6fa6ab78.pt"
# Slot 1 3-backbone server-only fusion (26 Nov 2026: BioCLIP 2.5-H int8 + Apache-2.0 DINOv2-S w1c + Meta DINOv3-S w1d,
# WingsClassificator reports/bioclip_dino_fusion_step_b1_20261126). Used automatically when all three files are present
# under --dir; otherwise falls back cleanly to HEAD_FILE (head_hier_6fa6ab78.pt).
FUSION_HEAD_FILE = "head_slot1_3bb_dist_rel_7d54b54f.pt"
FUSION_HEAD_SHA256 = "7d54b54f9853c9b6bd41df66138f3695190c3f47fe52880c03a031801712b6d1"
FUSION_SCHEMA = "slot1_3bb_decoupled_release_bundle/1"
W1C_ONNX = os.path.join("onnx", "w1c_pre384_fp32.onnx")
W1C_SHA256 = "baddedde2f528b39f6f2a643c2630cb7f325101e9e12b4b2df66104cb1ce1366"
W1D_ONNX = os.path.join("onnx", "w1d_pre384_fp32.onnx")
W1D_SHA256 = "6e5174daddfca36843fcbf9db52ce593469454ffc24ae946c38c12d41358aa27"
DETECTOR = "wing_seg_s2"  # 5 Nov 2026: V5N retrained on mined photos (WingsClassificator wingseg_next_20261103 step 2/2c); v6_feature.py knows its SHA
MEAN = (0.48145466, 0.4578275, 0.40821073)
STD = (0.26862954, 0.26130258, 0.27577711)
IMN_MEAN = (0.485, 0.456, 0.406)
IMN_STD = (0.229, 0.224, 0.225)
TOP = 5
# The head's tribe and subfamily per species, placeholders left out (export_taxonomy.py): the worker adds each
# tribe's and subfamily's total to a photo's answer (Identificar's tribe / subfamily answer).
TAXONOMY = os.path.join(os.path.dirname(os.path.abspath(__file__)), "taxonomy_6fa6ab78.json")
# Option 1A display-time 48-pair leaf merges (labels_q/leaf_map.tsv): probabilities of merged leaves sum into their
# corrected leaf before geographic prior and species/genus/tribe/subfamily roll-up.
LEAF_MERGES = os.path.join(os.path.dirname(os.path.abspath(__file__)), "leaf_merges_6fa6ab78.json")


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


def load_field_sex(root):
    """The general sex line (optional folder <root>/field_sex): its model, or None. Imported here, after the thread and
    priority limits are set, so a missing or broken field_sex.py or folder leaves the species answers as they are."""
    fs_dir = os.path.join(root, "field_sex")
    if not os.path.isdir(fs_dir):
        return None
    try:
        from field_sex import FieldSex

        return FieldSex(fs_dir)
    except Exception as e:
        print(f"Field sex model not loaded: {type(e).__name__}: {e}"[:300], file=sys.stderr)
        return None


def _sha256_file(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(8 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def _fusion_assets(root):
    """Paths to the three server-only Slot 1 fusion artifacts and which ones are present/missing."""
    assets = os.path.join(root, "app", "assets")
    items = (
        ("head", os.path.join(assets, FUSION_HEAD_FILE), FUSION_HEAD_SHA256),
        ("w1c", os.path.join(root, W1C_ONNX), W1C_SHA256),
        ("w1d", os.path.join(root, W1D_ONNX), W1D_SHA256),
    )
    present = [name for name, path, _ in items if os.path.exists(path)]
    missing = [name for name, path, _ in items if not os.path.exists(path)]
    return items, present, missing


def _unpack_adapter_branch(b_obj, F):
    st0 = b_obj["state"]
    return {
        "lnw": st0["layer_norm.weight"].float(),
        "lnb": st0["layer_norm.bias"].float(),
        "dw": st0["down.weight"].float(),
        "db": st0["down.bias"].float(),
        "uw": st0["up.weight"].float(),
        "ub": st0["up.bias"].float(),
        "rs": float(st0.get("residual_scale", 0.2)),
        "W": F.normalize(st0["weight_normalized"].float(), dim=1),
        "s": float(b_obj["s"]),
    }


def _fwd_adapter(hp, arr, torch, F, np):
    xb = torch.from_numpy(np.asarray(arr, dtype=np.float32))
    h = xb + hp["rs"] * F.linear(F.gelu(F.linear(xb, hp["dw"], hp["db"])), hp["uw"], hp["ub"])
    z = F.normalize(F.layer_norm(h, (h.shape[1],), hp["lnw"], hp["lnb"]), dim=1)
    return hp["s"] * F.linear(z, hp["W"]).clamp(-1.0 + 1e-6, 1.0 - 1e-6)


def _slot1_blend_probs(zb, zc, zd, blend_weights, t_disp, torch, np):
    wb, wc, wd = blend_weights
    p_blend = wb * zb.double().softmax(1) + wc * zc.double().softmax(1) + wd * zd.double().softmax(1)
    z_eff = torch.log(p_blend.clamp_min(1e-12)) / float(t_disp)
    return z_eff.softmax(1)[0].numpy().astype(np.float64)


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

        self.fusion = False
        self.head = None
        self.hp_bio = None
        self.hp_w1c = None
        self.hp_w1d = None
        self.w1c_sess = None
        self.w1d_sess = None
        self.blend_weights = (0.50, 0.25, 0.25)
        self.t_disp = 0.97

        fusion_items, fusion_present, fusion_missing = _fusion_assets(root)
        if not fusion_missing:
            fusion_paths = {}
            for name, path, want_sha in fusion_items:
                if _sha256_file(path) != want_sha:
                    raise ValueError(f"{os.path.basename(path)} is not the release's fusion asset (SHA-256 differs)")
                fusion_paths[name] = path
            ck = torch.load(fusion_paths["head"], map_location="cpu", weights_only=False)
            if ck.get("schema") != FUSION_SCHEMA:
                raise ValueError(f"{FUSION_HEAD_FILE}: unexpected schema {ck.get('schema')!r}")
            spec = ck.get("serving_spec") or {}
            if spec.get("blend_mode") != "prob":
                raise ValueError(f"{FUSION_HEAD_FILE}: unsupported blend_mode {spec.get('blend_mode')!r}")
            weights_cfg = spec["weights"]
            self.blend_weights = (
                float(weights_cfg["bio"]),
                float(weights_cfg["w1c"]),
                float(weights_cfg["w1d"]),
            )
            self.t_disp = float(spec["T_disp"])
            if not (self.t_disp > 0):
                raise ValueError(f"{FUSION_HEAD_FILE}: invalid T_disp {self.t_disp!r}")
            raw_leaves = list(ck["leaves"])
            self.hp_bio = _unpack_adapter_branch(ck["branches"]["bio"], F)
            self.hp_w1c = _unpack_adapter_branch(ck["branches"]["w1c"], F)
            self.hp_w1d = _unpack_adapter_branch(ck["branches"]["w1d"], F)
            self.w1c_sess = base(fusion_paths["w1c"], so, providers=["CPUExecutionProvider"])
            self.w1d_sess = base(fusion_paths["w1d"], so, providers=["CPUExecutionProvider"])
            self.fusion = True
            self.head_id = FUSION_HEAD_FILE[:-3]
        else:
            if fusion_present:
                print(
                    f"Slot 1 fusion assets incomplete (missing {', '.join(fusion_missing)}); falling back to {HEAD_FILE}",
                    file=sys.stderr,
                )
            head_path = os.path.join(assets, HEAD_FILE)
            if _sha256_file(head_path) != HEAD_SHA256:
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

            raw_leaves = list(ck["leaves"])
            self.head = CosineHead(ck["feat_dim"], len(raw_leaves), K=ck.get("K", 1))
            self.head.load_state_dict(ck["head_state_dict"])
            self.head.eval()
            self.head_id = HEAD_FILE[:-3]

        import importlib.util

        lm_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "leaf_merges.py")
        lm_spec = importlib.util.spec_from_file_location("_worker_leaf_merges", lm_path)
        leaf_merges = importlib.util.module_from_spec(lm_spec)
        lm_spec.loader.exec_module(leaf_merges)

        merges = leaf_merges.load_leaf_merges(LEAF_MERGES, HEAD_SHA256, required=True)
        space = leaf_merges.build_display_space(
            raw_leaves,
            ck["rank_vocab"]["species"],
            ck["rank_vocab"]["genus"],
            ck["leaf2rank"]["species"],
            ck["leaf2rank"]["genus"],
            merges,
            np,
            strict=True,
        )
        self._merge_leaf_probs = leaf_merges.merge_leaf_probs
        self.old_to_new = space["old_to_new"]
        self.leaves = space["leaves"]
        self.sp_vocab = space["sp_vocab"]
        self.sp_index = {n: i for i, n in enumerate(self.sp_vocab)}
        self.ge_vocab = space["ge_vocab"]
        self.leaf2sp = space["leaf2sp"]
        self.leaf2ge = space["leaf2ge"]
        self.is_ssp = space["is_ssp"]
        del ck
        self.higher = load_taxonomy(TAXONOMY, self.sp_vocab, np)

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
        self.np, self.torch, self.F = np, torch, F
        self.mean = np.array(MEAN, np.float32)[:, None, None]
        self.std = np.array(STD, np.float32)[:, None, None]
        self.imn_mean = np.array(IMN_MEAN, np.float32)[:, None, None]
        self.imn_std = np.array(IMN_STD, np.float32)[:, None, None]
        self.field_sex = load_field_sex(root)
        gc.collect()
        self.load_ms = round((time.perf_counter() - t0) * 1000)
        missing_note = f", fusion_missing={','.join(fusion_missing)}" if (not self.fusion and fusion_present) else ""
        print(
            f"Identifier loaded: {self.head_id} (mode={'fusion' if self.fusion else 'legacy'}{missing_note}, loadMs={self.load_ms}, rssMb={rss_mb()})",
            file=sys.stderr,
        )

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

    def _encode_dino(self, rgb_u8):
        np = self.np
        x01 = rgb_u8.astype(np.float32).transpose(2, 0, 1) / 255.0
        x224 = ((x01 - self.imn_mean) / self.imn_std)[None]
        f_w1c = self.w1c_sess.run(None, {"pixel_values": x224})[0]
        xt = self.torch.from_numpy(x01)[None]
        x208_01 = self.F.interpolate(xt, size=(208, 208), mode="bilinear", align_corners=False, antialias=False).numpy()[0]
        x208 = ((x208_01 - self.imn_mean) / self.imn_std)[None]
        f_w1d = self.w1d_sess.run(None, {"pixel_values": x208})[0]
        return f_w1c, f_w1d

    def feature(self, raw, box=None):
        """The photo's normalised BioCLIP feature (or Slot 1 {bio, w1c, w1d} dict) and its crop's provenance."""
        np = self.np
        square, prov = self.v6.crop(raw, manual_box=box) if box else self.v6.crop(raw)
        rgb_u8 = np.asarray(square.convert("RGB"), dtype=np.uint8)
        x = ((rgb_u8.astype(np.float32).transpose(2, 0, 1) / 255.0 - self.mean) / self.std)[None]
        feat = self.encoder.run(None, {"pixel_values": x})[0]
        feat_bio = feat / np.linalg.norm(feat, axis=1, keepdims=True)
        if getattr(self, "fusion", False):
            f_w1c, f_w1d = self._encode_dino(rgb_u8)
            return {"bio": feat_bio, "w1c": f_w1c, "w1d": f_w1d}, prov
        return feat_bio, prov

    def _combine_features(self, feat_list):
        """Average features across deduplicated specimen photos (unit-sphere mean for BioCLIP; native-scale mean for DINO)."""
        np = self.np
        if isinstance(feat_list[0], dict):
            fb = np.concatenate([f["bio"] for f in feat_list]).mean(0, keepdims=True)
            fb = fb / np.linalg.norm(fb)
            fc = np.concatenate([f["w1c"] for f in feat_list]).mean(0, keepdims=True)
            fd = np.concatenate([f["w1d"] for f in feat_list]).mean(0, keepdims=True)
            return {"bio": fb, "w1c": fc, "w1d": fd}
        f = np.concatenate(feat_list).mean(0, keepdims=True)
        return f / np.linalg.norm(f)

    def _merged_probs(self, feat):
        np, torch = self.np, self.torch
        if getattr(self, "fusion", False) and isinstance(feat, dict):
            with torch.inference_mode():
                zb = _fwd_adapter(self.hp_bio, feat["bio"], torch, self.F, np)
                zc = _fwd_adapter(self.hp_w1c, feat["w1c"], torch, self.F, np)
                zd = _fwd_adapter(self.hp_w1d, feat["w1d"], torch, self.F, np)
                raw = _slot1_blend_probs(zb, zc, zd, self.blend_weights, self.t_disp, torch, np)
        else:
            bio_feat = feat["bio"] if isinstance(feat, dict) else feat
            with torch.no_grad():
                logits = self.head(torch.from_numpy(bio_feat.astype(np.float32)))
                # field-sex-gate-check.py stubs Logits without .double() and constructs Identifier via __new__.
                if hasattr(logits, "double"):
                    logits = logits.double()
                raw = logits.softmax(1)[0].numpy().astype(np.float64)
        old_to_new = getattr(self, "old_to_new", None)
        if old_to_new is None:
            return raw
        return self._merge_leaf_probs(raw, old_to_new, len(self.leaves), np)

    def _score_distributions(self, feat, side=None, ecuador=None, merged_probs=None):
        np = self.np
        probs = self._merged_probs(feat) if merged_probs is None else merged_probs
        w = self.weights(side, ecuador)
        prior = bool((w < 1).any())
        if prior:
            pr = probs * w
            probs = pr / pr.sum() if pr.sum() > 0 else probs
        sp = np.zeros(len(self.sp_vocab), dtype=np.float64)
        np.add.at(sp, self.leaf2sp, probs)
        ge = np.zeros(len(self.ge_vocab), dtype=np.float64)
        np.add.at(ge, self.leaf2ge, probs)
        return probs, sp, ge, prior

    def tops(self, feat, side=None, ecuador=None, higher=False, merged_probs=None):
        np = self.np
        probs, sp, ge, prior = self._score_distributions(feat, side, ecuador, merged_probs=merged_probs)

        def top_k(agg, vocab, k):
            return [[vocab[j], round(float(agg[j]), 4)] for j in np.argsort(-agg, kind="stable")[:k]]

        order = np.argsort(-probs, kind="stable")
        subs = []
        for i in order:
            if self.is_ssp[i]:
                subs.append([self.leaves[i], round(float(probs[i]), 4)])
                if len(subs) >= TOP:
                    break
        out = {
            "species": top_k(sp, self.sp_vocab, TOP),
            "subspecies": subs,
            "genus": top_k(ge, self.ge_vocab, 3),
            "top": [self.leaves[order[0]], round(float(probs[order[0]]), 4)],
            "prior": side if prior and side in ("East", "West") else ("Ecuador" if prior else None),
        }
        if higher and self.higher:
            out.update(higher_totals(sp, self.higher, np))
        return out

    def species_noprior(self, feat, name, merged_probs=None):
        """The species marginal of `name` WITHOUT the geographic prior (the field sex line's gate, whatever side the
        person chose), unrounded; 0 for a name the head does not have."""
        if name not in self.sp_index:
            return 0.0
        probs = self._merged_probs(feat) if merged_probs is None else merged_probs
        return float(probs[self.leaf2sp == self.sp_index[name]].sum())

    def field_sex_block(self, feat, crop, name, merged_probs=None):
        """The general sex line for the species the page names (the answer's top species, after the prior when a side
        was chosen), gated on that species' no-prior score. Always passes the untouched 1024-D BioCLIP feature."""
        if self.field_sex is None or not name or self.field_sex.model_name(name) is None:
            return None
        bio_feat = feat["bio"] if isinstance(feat, dict) else feat
        try:
            return self.field_sex.score(bio_feat, name, self.species_noprior(feat, name, merged_probs=merged_probs), crop)
        except Exception as e:  # the species answer stands without it
            return {"status": f"error:{type(e).__name__}"}

    def identify(self, path, side=None, box=None, ecuador=None):
        t0 = time.perf_counter()
        with open(path, "rb") as f:
            raw = f.read()
        feat, prov = self.feature(raw, box)
        geometry = prov.get("geometry") or {}
        merged_probs = self._merged_probs(feat)
        out = self.tops(feat, side, ecuador, higher=True, merged_probs=merged_probs)
        if out["prior"] == "Ecuador":
            out["prior"] = None
        extra = {}
        if self.field_sex is not None:
            extra["fieldSex"] = self.field_sex_block(
                feat, geometry.get("status"), (out["species"] or [[None]])[0][0], merged_probs=merged_probs
            )
        return {
            **out,
            **extra,
            "head": getattr(self, "head_id", HEAD_FILE[:-3]),
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
            f = self._combine_features([feats[k] for k in used])
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


def load_taxonomy(path, sp_vocab, np):
    """For each rank (tribe, subfamily): its shown names and each head species' index into them (-1: none or a
    placeholder). None without the file, or when it is another head's."""
    if not os.path.exists(path):
        return None
    doc = json.load(open(path))
    if doc.get("head") != HEAD_SHA256:
        return None
    out = {}
    for rank, names, col in (("tribe", doc["tribes"], 0), ("subfamily", doc["subfamilies"], 1)):
        idx = np.full(len(sp_vocab), -1, np.int64)
        for i, name in enumerate(sp_vocab):
            pair = doc["species"].get(name)
            if pair is not None:
                idx[i] = pair[col]
        out[rank] = (names, idx)
    return out


def higher_distributions(sp_probs, higher, np):
    """Full tribe and subfamily probability totals plus unassigned mass: {rank: (names, agg, unassigned)}."""
    out = {}
    for rank, (names, idx) in higher.items():
        has = idx >= 0
        agg = np.zeros(len(names), dtype=np.float64)
        np.add.at(agg, idx[has], sp_probs[has])
        out[rank] = (names, agg, float(sp_probs[~has].sum()))
    return out


def higher_totals(sp_probs, higher, np, k=3):
    """The `k` most probable tribes and subfamilies: [[name, p], ...], each the sum of its species' scores."""
    out = {}
    for rank, (names, agg, _unassigned) in higher_distributions(sp_probs, higher, np).items():
        out[rank] = [[names[j], round(float(agg[j]), 4)] for j in np.argsort(-agg, kind="stable")[:k] if agg[j] > 0]
    return out


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
