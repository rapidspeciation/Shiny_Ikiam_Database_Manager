"""Unit tests for Slot 1 3-backbone server fusion in tools/identifier/worker.py.

Runs in two modes:
  - Always (numpy only): asset discovery/fallback, SHA-256 verification, _slot1_blend_probs math parity,
    field_sex untouched 1024-D feat_bio + pre-prior gate, Option 1A leaf merges + geographic prior,
    and specimen() multi-photo feature combining (unit-sphere bio, native-scale w1c/w1d).
  - When torch is importable: exact numerical parity of _unpack_adapter_branch, _fwd_adapter,
    _slot1_blend_probs, _encode_dino (224px w1c + 208px bilinear align_corners=False antialias=False w1d),
    and end-to-end Identifier.__init__ fusion load, legacy fallback (absent & partial assets),
    and fail-closed SHA-256 mismatch.

usage: python -I fusion-slot1-check.py TOOLS_DIR OUT_JSON
"""
import contextlib
import hashlib
import io
import json
import math
import os
import sys
import tempfile
import types

import numpy as np

tools_dir, out_path = sys.argv[1], sys.argv[2]
sys.path.insert(0, tools_dir)
sys.argv = ["worker.py", "--dir", "/nonexistent", "--no-pin"]
import leaf_merges  # noqa: E402
import worker  # noqa: E402

try:
    import torch
    import torch.nn.functional as F

    HAS_TORCH = True
except ImportError:
    torch = None
    F = None
    HAS_TORCH = False


def np_softmax(x):
    x = np.asarray(x, dtype=np.float64)
    e = np.exp(x - x.max(axis=-1, keepdims=True))
    return e / e.sum(axis=-1, keepdims=True)


def np_gelu(x):
    # Exact PyTorch default GELU (approximate='none'): 0.5 * x * (1 + erf(x / sqrt(2)))
    v_erf = np.vectorize(math.erf, otypes=[np.float64])
    x64 = np.asarray(x, dtype=np.float64)
    return 0.5 * x64 * (1.0 + v_erf(x64 / math.sqrt(2.0)))


def ref_slot1_blend(zb, zc, zd, weights=(0.50, 0.25, 0.25), t_disp=0.97):
    wb, wc, wd = weights
    p_mix = wb * np_softmax(zb) + wc * np_softmax(zc) + wd * np_softmax(zd)
    z_eff = np.log(np.maximum(p_mix, 1e-12)) / float(t_disp)
    return np_softmax(z_eff)[0]


def ref_fwd_adapter_np(st0, s, arr):
    x = np.asarray(arr, dtype=np.float64)
    h1 = x @ st0["down.weight"].T + st0["down.bias"]
    h2 = np_gelu(h1) @ st0["up.weight"].T + st0["up.bias"]
    h = x + float(st0.get("residual_scale", 0.2)) * h2
    mu = h.mean(axis=1, keepdims=True)
    var = ((h - mu) ** 2).mean(axis=1, keepdims=True)
    ln = (h - mu) / np.sqrt(var + 1e-5) * st0["layer_norm.weight"] + st0["layer_norm.bias"]
    z = ln / np.linalg.norm(ln, axis=1, keepdims=True)
    W = st0["weight_normalized"] / np.linalg.norm(st0["weight_normalized"], axis=1, keepdims=True)
    cos = np.clip(z @ W.T, -1.0 + 1e-6, 1.0 - 1e-6)
    return float(s) * cos


class StubTensor:
    """Minimal numpy-backed stand-in for torch.Tensor when torch is not installed."""

    def __init__(self, arr):
        self.arr = np.asarray(arr, dtype=np.float64)

    def double(self):
        return StubTensor(self.arr.astype(np.float64))

    def softmax(self, dim):
        return StubTensor(np_softmax(self.arr))

    def clamp_min(self, lo):
        return StubTensor(np.maximum(self.arr, lo))

    def __rmul__(self, other):
        return StubTensor(other * self.arr)

    def __add__(self, other):
        return StubTensor(self.arr + other.arr)

    def __truediv__(self, other):
        return StubTensor(self.arr / other)

    def __getitem__(self, idx):
        return StubTensor(self.arr[idx])

    def numpy(self):
        return self.arr


class StubTorch:
    inference_mode = staticmethod(contextlib.nullcontext)
    no_grad = staticmethod(contextlib.nullcontext)

    @staticmethod
    def log(t):
        return StubTensor(np.log(t.arr))


results = {"has_torch": HAS_TORCH}

# 1. Pure-numpy _fusion_assets discovery (absent, partial, complete) & _sha256_file
with tempfile.TemporaryDirectory() as td:
    items, present, missing = worker._fusion_assets(td)
    assert present == [] and set(missing) == {"head", "w1c", "w1d"}
    os.makedirs(os.path.join(td, "app", "assets"), exist_ok=True)
    os.makedirs(os.path.join(td, "onnx"), exist_ok=True)
    p_head = os.path.join(td, "app", "assets", worker.FUSION_HEAD_FILE)
    open(p_head, "wb").write(b"partial-head")
    _, present, missing = worker._fusion_assets(td)
    assert present == ["head"] and set(missing) == {"w1c", "w1d"}
    assert worker._sha256_file(p_head) == hashlib.sha256(b"partial-head").hexdigest()
    open(os.path.join(td, worker.W1C_ONNX), "wb").write(b"w1c")
    open(os.path.join(td, worker.W1D_ONNX), "wb").write(b"w1d")
    _, present, missing = worker._fusion_assets(td)
    assert set(present) == {"head", "w1c", "w1d"} and missing == []
results["asset_discovery"] = "ok"

# 2. _slot1_blend_probs numerical parity (both stub & real torch if present)
rng = np.random.default_rng(20261126)
zb_np = rng.normal(0.0, 2.5, size=(1, 12)).astype(np.float32)
zc_np = rng.normal(0.0, 2.5, size=(1, 12)).astype(np.float32)
zd_np = rng.normal(0.0, 2.5, size=(1, 12)).astype(np.float32)
ref_p = ref_slot1_blend(zb_np, zc_np, zd_np, (0.50, 0.25, 0.25), 0.97)
stub_p = worker._slot1_blend_probs(StubTensor(zb_np), StubTensor(zc_np), StubTensor(zd_np), (0.50, 0.25, 0.25), 0.97, StubTorch, np)
results["blend_stub_max_diff"] = float(np.max(np.abs(stub_p - ref_p)))
assert results["blend_stub_max_diff"] < 1e-14

# 3. Fusion Identifier: Option 1A merges + geo prior + field_sex untouched 1024-D feat_bio + specimen() combining
merges = leaf_merges.load_leaf_merges(worker.LEAF_MERGES, worker.HEAD_SHA256, required=True)
assert len(merges) == 48
# Build a compact leaf space containing real merged leaves + unmerged species
raw_leaves = [
    "Greta diaphanus diaphanus",
    "Greta diaphanus",
    "Greta diaphanus quisqueya",
    "Danaus plexippus",
    "Melinaea marsaeus mothone",
    "Melinaea mothone mothone",
]
raw_sp = ["Greta diaphanus", "Danaus plexippus", "Melinaea marsaeus", "Melinaea mothone"]
raw_ge = ["Greta", "Danaus", "Melinaea"]
raw_l2sp = np.array([0, 0, 0, 1, 2, 3], dtype=np.int64)
raw_l2ge = np.array([0, 0, 0, 1, 2, 2], dtype=np.int64)
sub_merges = {k: v for k, v in merges.items() if k in raw_leaves}
assert len(sub_merges) == 2  # Greta diaphanus diaphanus -> Greta diaphanus, Melinaea marsaeus mothone -> Melinaea mothone mothone
space = leaf_merges.build_display_space(raw_leaves, raw_sp, raw_ge, raw_l2sp, raw_l2ge, sub_merges, np, strict=True)
assert len(space["leaves"]) == 4
assert "Melinaea marsaeus" not in space["sp_vocab"]

ident = worker.Identifier.__new__(worker.Identifier)
ident.np = np
ident.fusion = True
ident.head_id = worker.FUSION_HEAD_FILE[:-3]
ident.blend_weights = (0.50, 0.25, 0.25)
ident.t_disp = 0.97
ident._merge_leaf_probs = leaf_merges.merge_leaf_probs
ident.old_to_new = space["old_to_new"]
ident.leaves = space["leaves"]
ident.sp_vocab = space["sp_vocab"]
ident.sp_index = {n: i for i, n in enumerate(ident.sp_vocab)}
ident.ge_vocab = space["ge_vocab"]
ident.leaf2sp = space["leaf2sp"]
ident.leaf2ge = space["leaf2ge"]
ident.is_ssp = space["is_ssp"]
ident.higher = None
# Geographic prior suppresses Greta diaphanus on East so Danaus plexippus wins with East, while Greta wins no-prior
ident.geo = {
    "known": np.array([True] * len(ident.leaves)),
    "inec": np.array([True] * len(ident.leaves)),
    "East": np.array([False if lf.startswith("Greta") else True for lf in ident.leaves]),
    "West": np.array([True] * len(ident.leaves)),
}

feat_bio = rng.normal(size=(1, 1024)).astype(np.float32)
feat_bio /= np.linalg.norm(feat_bio, axis=1, keepdims=True)
feat_bio_sha = hashlib.sha256(feat_bio.tobytes()).hexdigest()
feat_w1c = (rng.normal(size=(1, 384)) * 12.0).astype(np.float32)  # deliberately non-unit norm (~235)
feat_w1d = (rng.normal(size=(1, 384)) * 8.5).astype(np.float32)

# Verify _combine_features preserves unit norm on bio and native scale on w1c/w1d
feat_bio_2 = rng.normal(size=(1, 1024)).astype(np.float32)
feat_bio_2 /= np.linalg.norm(feat_bio_2, axis=1, keepdims=True)
feat_w1c_2 = (rng.normal(size=(1, 384)) * 12.0).astype(np.float32)
feat_w1d_2 = (rng.normal(size=(1, 384)) * 8.5).astype(np.float32)
f1 = {"bio": feat_bio, "w1c": feat_w1c, "w1d": feat_w1d}
f2 = {"bio": feat_bio_2, "w1c": feat_w1c_2, "w1d": feat_w1d_2}

comb1 = ident._combine_features([f1])
assert float(np.max(np.abs(comb1["bio"] - feat_bio))) < 1e-6
assert np.array_equal(comb1["w1c"], feat_w1c)
assert np.array_equal(comb1["w1d"], feat_w1d)

comb2 = ident._combine_features([f1, f2])
assert abs(float(np.linalg.norm(comb2["bio"])) - 1.0) < 1e-6
exp_w1c_mean = 0.5 * (feat_w1c + feat_w1c_2)
exp_w1d_mean = 0.5 * (feat_w1d + feat_w1d_2)
assert float(np.max(np.abs(comb2["w1c"] - exp_w1c_mean))) < 1e-5
assert float(np.max(np.abs(comb2["w1d"] - exp_w1d_mean))) < 1e-5
# Confirm native scale was NOT shrunk to unit norm
assert float(np.linalg.norm(comb2["w1c"])) > 10.0
assert float(np.linalg.norm(comb2["w1d"])) > 10.0
# Legacy ndarray combine
leg_comb = ident._combine_features([feat_bio, feat_bio_2])
assert abs(float(np.linalg.norm(leg_comb)) - 1.0) < 1e-6
results["combine_features"] = "ok"

# Verify field_sex receives untouched 1024-D feat_bio and pre-prior species marginal
seen_fs = []


class FieldSexRecorder:
    def model_name(self, name):
        return name if name in ("Danaus plexippus", "Greta diaphanus") else None

    def score(self, feat, name, gate_p, crop):
        assert isinstance(feat, np.ndarray) and feat.shape == (1, 1024)
        sha = hashlib.sha256(feat.astype(np.float32).tobytes()).hexdigest()
        seen_fs.append({"name": name, "gate_p": round(float(gate_p), 6), "crop": crop, "sha": sha})
        return {"species": name, "status": "call", "gateP": round(float(gate_p), 4)}


ident.field_sex = FieldSexRecorder()
# Craft raw_p so Greta diaphanus diaphanus (0.45) + Greta diaphanus (0.25) = 0.70, Danaus plexippus = 0.20, Melinaea = 0.10
raw_p_fixed = np.array([0.45, 0.25, 0.00, 0.20, 0.06, 0.04], dtype=np.float64)
merged_fixed = leaf_merges.merge_leaf_probs(raw_p_fixed, ident.old_to_new, len(ident.leaves), np)
# Inject _merged_probs for numpy-only verification of option1A + prior + field_sex
ident._merged_probs = lambda _feat: merged_fixed

top_noprior = ident.tops(f1, side=None, ecuador=None)
top_east = ident.tops(f1, side="East", ecuador=None)
assert top_noprior["species"][0][0] == "Greta diaphanus"
assert abs(top_noprior["species"][0][1] - 0.70) < 1e-4
assert top_east["species"][0][0] == "Danaus plexippus"
assert top_east["prior"] == "East"
# Melinaea marsaeus mothone (0.06) + Melinaea mothone mothone (0.04) = 0.10 merged under Melinaea mothone
sp_map_noprior = dict(top_noprior["species"])
assert abs(sp_map_noprior["Melinaea mothone"] - 0.10) < 1e-4

fs_east = ident.field_sex_block(f1, "success", top_east["species"][0][0], merged_probs=merged_fixed)
assert fs_east is not None
assert seen_fs[0]["sha"] == feat_bio_sha
assert seen_fs[0]["name"] == "Danaus plexippus"
assert abs(seen_fs[0]["gate_p"] - 0.20) < 1e-6  # pre-prior marginal of Danaus plexippus, NOT post-prior!
results["field_sex_bio_bit_identical"] = True
results["option1a_and_geo_prior"] = "ok"

# 4. Real PyTorch parity & full Identifier.__init__ fusion/fallback/tamper tests
if HAS_TORCH:
    def make_branch_dict(d_in, n_cls, seed, s=25.0):
        g = np.random.default_rng(seed)
        st = {
            "layer_norm.weight": torch.from_numpy(g.uniform(0.8, 1.2, size=(d_in,)).astype(np.float32)),
            "layer_norm.bias": torch.from_numpy(g.normal(0.0, 0.1, size=(d_in,)).astype(np.float32)),
            "down.weight": torch.from_numpy(g.normal(0.0, 0.05, size=(64, d_in)).astype(np.float32)),
            "down.bias": torch.from_numpy(g.normal(0.0, 0.05, size=(64,)).astype(np.float32)),
            "up.weight": torch.from_numpy(g.normal(0.0, 0.05, size=(d_in, 64)).astype(np.float32)),
            "up.bias": torch.from_numpy(g.normal(0.0, 0.05, size=(d_in,)).astype(np.float32)),
            "residual_scale": 0.20,
            "weight_normalized": torch.from_numpy(g.normal(0.0, 1.0, size=(n_cls, d_in)).astype(np.float32)),
        }
        return {"state": st, "s": s}

    n_cls = 6
    b_bio = make_branch_dict(1024, n_cls, 101, 25.0)
    b_w1c = make_branch_dict(384, n_cls, 102, 25.0)
    b_w1d = make_branch_dict(384, n_cls, 103, 25.0)
    hp_bio = worker._unpack_adapter_branch(b_bio, F)
    hp_w1c = worker._unpack_adapter_branch(b_w1c, F)
    hp_w1d = worker._unpack_adapter_branch(b_w1d, F)

    with torch.inference_mode():
        zb = worker._fwd_adapter(hp_bio, feat_bio, torch, F, np)
        zc = worker._fwd_adapter(hp_w1c, feat_w1c, torch, F, np)
        zd = worker._fwd_adapter(hp_w1d, feat_w1d, torch, F, np)
        p_worker = worker._slot1_blend_probs(zb, zc, zd, (0.50, 0.25, 0.25), 0.97, torch, np)

    def to_np_st(b):
        return {k: (v.numpy().astype(np.float64) if hasattr(v, "numpy") else v) for k, v in b["state"].items()}

    zb_ref = ref_fwd_adapter_np(to_np_st(b_bio), b_bio["s"], feat_bio)
    zc_ref = ref_fwd_adapter_np(to_np_st(b_w1c), b_w1c["s"], feat_w1c)
    zd_ref = ref_fwd_adapter_np(to_np_st(b_w1d), b_w1d["s"], feat_w1d)
    p_ref = ref_slot1_blend(zb_ref, zc_ref, zd_ref, (0.50, 0.25, 0.25), 0.97)
    max_prob_diff = float(np.max(np.abs(p_worker - p_ref)))
    assert max_prob_diff < 1e-5, f"Slot 1 torch-vs-numpy parity diff too large: {max_prob_diff}"
    results["torch_adapter_blend_max_diff"] = max_prob_diff

    # Test _encode_dino exact preprocessing parity against step_b1_claudeclaw_arm_bench.py
    rgb_u8 = rng.integers(0, 256, size=(224, 224, 3), dtype=np.uint8)
    seen_inputs = {}

    class FakeSess:
        def __init__(self, tag):
            self.tag = tag

        def run(self, _out, feed):
            seen_inputs[self.tag] = feed["pixel_values"].copy()
            return [np.ones((1, 384), dtype=np.float32)]

    ident_enc = worker.Identifier.__new__(worker.Identifier)
    ident_enc.np = np
    ident_enc.torch = torch
    ident_enc.F = F
    ident_enc.imn_mean = np.array(worker.IMN_MEAN, np.float32)[:, None, None]
    ident_enc.imn_std = np.array(worker.IMN_STD, np.float32)[:, None, None]
    ident_enc.w1c_sess = FakeSess("w1c")
    ident_enc.w1d_sess = FakeSess("w1d")
    ident_enc._encode_dino(rgb_u8)

    imn_m = np.array((0.485, 0.456, 0.406), np.float32)[:, None, None]
    imn_s = np.array((0.229, 0.224, 0.225), np.float32)[:, None, None]
    x01 = rgb_u8.astype(np.float32).transpose(2, 0, 1) / 255.0
    exp_x224 = ((x01 - imn_m) / imn_s)[None]
    exp_x208_01 = F.interpolate(torch.from_numpy(x01)[None], size=(208, 208), mode="bilinear", align_corners=False, antialias=False).numpy()[0]
    exp_x208 = ((exp_x208_01 - imn_m) / imn_s)[None]
    assert seen_inputs["w1c"].shape == (1, 3, 224, 224)
    assert seen_inputs["w1d"].shape == (1, 3, 208, 208)
    assert float(np.max(np.abs(seen_inputs["w1c"] - exp_x224))) == 0.0
    assert float(np.max(np.abs(seen_inputs["w1d"] - exp_x208))) == 0.0
    results["encode_dino_exact_zero_diff"] = True

    # Test real Identifier.__init__ with synthetic checkpoints for:
    #   (a) fusion active when all 3 files exist & match SHA-256
    #   (b) graceful fallback when fusion files are absent or partial
    #   (c) fail-closed ValueError when all 3 fusion files exist but SHA-256 differs
    with tempfile.TemporaryDirectory() as td:
        assets = os.path.join(td, "app", "assets")
        onnx_dir = os.path.join(td, "onnx")
        os.makedirs(assets, exist_ok=True)
        os.makedirs(onnx_dir, exist_ok=True)

        # Build a valid leaf space covering all 48 merge keys so strict=True passes!
        full_raw_leaves = sorted(set(merges.keys()) | (set(merges.values()) - {"Melinaea mothone messenina"}) | {"Danaus plexippus"})
        full_sp = sorted({" ".join(lf.split()[:2]) for lf in full_raw_leaves})
        full_ge = sorted({lf.split()[0] for lf in full_raw_leaves})
        sp_map = {s: i for i, s in enumerate(full_sp)}
        ge_map = {g: i for i, g in enumerate(full_ge)}
        full_l2sp = [sp_map[" ".join(lf.split()[:2])] for lf in full_raw_leaves]
        full_l2ge = [ge_map[lf.split()[0]] for lf in full_raw_leaves]
        n_full = len(full_raw_leaves)

        leg_path = os.path.join(assets, worker.HEAD_FILE)
        leg_ck = {
            "feat_dim": 16,
            "leaves": full_raw_leaves,
            "rank_vocab": {"species": full_sp, "genus": full_ge},
            "leaf2rank": {"species": full_l2sp, "genus": full_l2ge},
            "head_state_dict": {
                "layer_norm.weight": torch.ones(16),
                "layer_norm.bias": torch.zeros(16),
                "weight": torch.randn(n_full, 16),
            },
        }
        torch.save(leg_ck, leg_path)
        leg_sha = worker._sha256_file(leg_path)

        fus_path = os.path.join(assets, worker.FUSION_HEAD_FILE)
        fus_ck = {
            "schema": worker.FUSION_SCHEMA,
            "setting": "release",
            "n_train": 666561,
            "leaves": full_raw_leaves,
            "rank_vocab": {"species": full_sp, "genus": full_ge},
            "leaf2rank": {"species": full_l2sp, "genus": full_l2ge},
            "serving_spec": {"blend_mode": "prob", "weights": {"bio": 0.50, "w1c": 0.25, "w1d": 0.25}, "T_disp": 0.97},
            "branches": {
                "bio": make_branch_dict(16, n_full, 201, 25.0),
                "w1c": make_branch_dict(12, n_full, 202, 25.0),
                "w1d": make_branch_dict(12, n_full, 203, 25.0),
            },
        }
        torch.save(fus_ck, fus_path)
        fus_sha = worker._sha256_file(fus_path)

        w1c_path = os.path.join(td, worker.W1C_ONNX)
        w1d_path = os.path.join(td, worker.W1D_ONNX)
        bio_path = os.path.join(td, "onnx", "bioclip25h_visual_int8.onnx")
        open(bio_path, "wb").write(b"fake-bio")

        # Install lightweight mocks for onnxruntime, v6_feature, ultralytics
        fake_ort = types.ModuleType("onnxruntime")

        class MockSO:
            pass

        class MockGraphOpt:
            ORT_ENABLE_ALL = 1

        class MockSession:
            def __init__(self, path, sess_options=None, providers=None):
                self.path = path

            def run(self, _out, feed):
                if "w1c" in self.path or "w1d" in self.path:
                    return [np.ones((1, 12), dtype=np.float32)]
                return [np.ones((1, 16), dtype=np.float32)]

        fake_ort.SessionOptions = MockSO
        fake_ort.GraphOptimizationLevel = MockGraphOpt
        fake_ort.InferenceSession = MockSession

        fake_v6 = types.ModuleType("v6_feature")

        class MockV6:
            def __init__(self, *a, **k):
                pass

            def crop(self, raw, manual_box=None):
                class FakeImg:
                    def convert(self, _m):
                        return np.zeros((224, 224, 3), dtype=np.uint8)

                return FakeImg(), {"geometry": {"status": "success"}, "wing_box": [0.1, 0.1, 0.9, 0.9]}

        fake_v6.V6FeatureExtractor = MockV6

        fake_ultra = types.ModuleType("ultralytics")
        fake_ultra.YOLO = lambda *a, **k: object()

        saved_mods = {k: sys.modules.get(k) for k in ("onnxruntime", "v6_feature", "ultralytics")}
        saved_shas = (worker.HEAD_SHA256, worker.FUSION_HEAD_SHA256, worker.W1C_SHA256, worker.W1D_SHA256, worker.LEAF_MERGES)
        # Write a temp leaf_merges file matching our synthetic leg_sha
        tmp_merges = os.path.join(td, "leaf_merges_tmp.json")
        orig_doc = json.load(open(worker.LEAF_MERGES))
        orig_doc["head"] = leg_sha
        json.dump(orig_doc, open(tmp_merges, "w"))

        try:
            sys.modules["onnxruntime"] = fake_ort
            sys.modules["v6_feature"] = fake_v6
            sys.modules["ultralytics"] = fake_ultra
            worker.HEAD_SHA256 = leg_sha
            worker.FUSION_HEAD_SHA256 = fus_sha
            worker.LEAF_MERGES = tmp_merges

            # (b1) Partial fusion assets (only head present, w1c & w1d absent) -> graceful fallback to legacy
            buf_err = io.StringIO()
            old_err = sys.stderr
            sys.stderr = buf_err
            try:
                m_partial = worker.Identifier(td, "onnx/bioclip25h_visual_int8.onnx", 1)
            finally:
                sys.stderr = old_err
            assert m_partial.fusion is False
            assert m_partial.head_id == worker.HEAD_FILE[:-3]
            err_partial = buf_err.getvalue()
            assert "incomplete" in err_partial
            assert "Identifier loaded: head_hier_6fa6ab78 (mode=legacy, fusion_missing=w1c,w1d," in err_partial, err_partial

            # (b2) All 3 fusion files absent -> clean silent fallback to legacy
            os.unlink(fus_path)
            buf_err2 = io.StringIO()
            sys.stderr = buf_err2
            try:
                m_absent = worker.Identifier(td, "onnx/bioclip25h_visual_int8.onnx", 1)
            finally:
                sys.stderr = old_err
            assert m_absent.fusion is False
            assert m_absent.head_id == worker.HEAD_FILE[:-3]
            assert "incomplete" not in buf_err2.getvalue()

            # (a) All 3 fusion files present & verified -> Slot 1 fusion loads & identifies + specimen() works
            torch.save(fus_ck, fus_path)
            open(w1c_path, "wb").write(b"fake-w1c")
            open(w1d_path, "wb").write(b"fake-w1d")
            worker.W1C_SHA256 = worker._sha256_file(w1c_path)
            worker.W1D_SHA256 = worker._sha256_file(w1d_path)
            m_fus = worker.Identifier(td, "onnx/bioclip25h_visual_int8.onnx", 1)
            assert m_fus.fusion is True
            assert m_fus.head_id == worker.FUSION_HEAD_FILE[:-3]
            photo_a = os.path.join(td, "a.jpg")
            photo_b = os.path.join(td, "b.jpg")
            open(photo_a, "wb").write(b"photo-a")
            open(photo_b, "wb").write(b"photo-b")
            ans = m_fus.identify(photo_a, side="East")
            assert ans["head"] == worker.FUSION_HEAD_FILE[:-3]
            assert len(ans["species"]) == 5
            spec_ans = m_fus.specimen(
                {"photos": [{"key": "d", "path": photo_a}, {"key": "v", "path": photo_b}], "side": "East", "ecuador": True},
                sex_model=None,
            )
            assert spec_ans["combined"]["used"] == ["d", "v"]
            assert len(spec_ans["combined"]["species"]) == 5

            # (c) Tampered SHA-256 when all 3 fusion files exist -> fail closed ValueError
            worker.W1D_SHA256 = "0" * 64
            tamper_err = ""
            try:
                worker.Identifier(td, "onnx/bioclip25h_visual_int8.onnx", 1)
            except ValueError as e:
                tamper_err = str(e)
            assert "SHA-256 differs" in tamper_err, tamper_err
            results["init_fusion_fallback_tamper"] = "ok"
        finally:
            worker.HEAD_SHA256, worker.FUSION_HEAD_SHA256, worker.W1C_SHA256, worker.W1D_SHA256, worker.LEAF_MERGES = saved_shas
            for k, v in saved_mods.items():
                if v is None:
                    sys.modules.pop(k, None)
                else:
                    sys.modules[k] = v

with open(out_path, "w", encoding="utf-8") as f:
    json.dump(results, f)
