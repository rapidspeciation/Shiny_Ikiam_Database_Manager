"""Export the Slot 1 3-backbone fusion head for the offline browser identifier (`ithomiini-offline-fusion-head/1`).

Run on the server with the identifier's venv (inference only, nothing trained):
  shared/identifier/venv/bin/python -I tools/identifier/export_offline_fusion_head.py shared/identifier [out_dir]

Writes under <dir>/offline/ (or out_dir when given):
  head_slot1_<sha8>_fp16.bin  4-byte-aligned little-endian float16 matrices per branch (down_w, up_w, L2-normalised W)
  head_slot1_<sha8>.json      schema ithomiini-offline-fusion-head/1, compat_live_head_sha256 (for Option 1A merges),
                              blend_mode, T_disp, per-branch byte offsets + biases + LayerNorm + input specs,
                              species, leaf2sp, leaves, is_ssp, geographic prior flags, and reviewed full-photo overrides.
"""

import hashlib
import json
import os
import sys

import numpy as np
import torch
import torch.nn.functional as F

FUSION_HEAD_FILE = "head_slot1_3bb_dist_rel_7d54b54f.pt"
FUSION_HEAD_SHA256 = "7d54b54f9853c9b6bd41df66138f3695190c3f47fe52880c03a031801712b6d1"
COMPAT_LIVE_HEAD_SHA256 = "6fa6ab78c6c3b76566a15d322061d2a5bec95c7dd705797c75c95799e1603552"
EXPECTED_BIN_SHA256 = "54fdde1ac6158438d537c1217cde2b781f3d4db01df39706938ceca4a6dd56b9"
EXPECTED_JSON_SHA256 = "345d066647d13e5474d53d51e13b58d55504b5b03ec831cc3779d64bda8cb06d"
SLOT_TAG = "slot1"


def main(root, out=None):
    assets = os.path.join(root, "app", "assets")
    head_path = os.path.join(assets, FUSION_HEAD_FILE)
    raw = open(head_path, "rb").read()
    got_sha = hashlib.sha256(raw).hexdigest()
    if got_sha != FUSION_HEAD_SHA256:
        raise SystemExit(f"{FUSION_HEAD_FILE}: SHA-256 {got_sha} differs from pinned {FUSION_HEAD_SHA256}")

    ck = torch.load(head_path, map_location="cpu", weights_only=False)
    spec = ck["serving_spec"]
    branch_order = list(spec["branch_order"])
    weights_map = spec["weights"]
    leaves = list(ck["leaves"])

    flags = ["0"] * len(leaves)
    checklist = os.path.join(assets, "region_checklist.json")
    has_checklist = os.path.exists(checklist)
    if has_checklist:
        chk = json.load(open(checklist))
        for i, lf in enumerate(leaves):
            e = chk.get(lf)
            if e is None:
                p = lf.split()
                e = chk.get(p[0] + " " + p[1]) if len(p) >= 2 else None
            if e is None:
                continue
            f = 1 | (2 if e.get("ec", 0) == 1 else 0) | (4 if e.get("East", 0) > 0 else 0) | (8 if e.get("West", 0) > 0 else 0)
            flags[i] = "%x" % f
    overrides = json.load(open(os.path.join(assets, "field_wingtip_override_v1.json")))

    buf = bytearray()
    branches_meta = []
    for bname in branch_order:
        bobj = ck["branches"][bname]
        st0 = bobj["state"]
        bb_spec = spec["backbones"][bname]
        dw = st0["down.weight"].float().numpy().astype("<f2")
        uw = st0["up.weight"].float().numpy().astype("<f2")
        W = F.normalize(st0["weight_normalized"].float(), dim=1).numpy().astype("<f2")

        def append_mat(arr):
            while len(buf) % 4 != 0:
                buf.append(0)
            off = len(buf)
            buf.extend(arr.tobytes(order="C"))
            return [off, int(arr.shape[0]), int(arr.shape[1])]

        off_dw = append_mat(dw)
        off_uw = append_mat(uw)
        off_W = append_mat(W)

        branches_meta.append({
            "name": bname,
            "role": "encoder" if bname == "bio" else f"dino_{bname}",
            "backbone_file": bb_spec["file"],
            "backbone_sha256": bb_spec["sha256"],
            "input_size": int(bb_spec["input_size"]),
            "norm": str(bb_spec["norm"]),
            "resize": str(bb_spec.get("resize", "identity_224")),
            "l2_normalize_input": bool(bb_spec.get("l2_normalize_output", bname == "bio")),
            "weight": float(weights_map[bname]),
            "arch": "res_adapter",
            "dim": int(bobj["in_dim"]),
            "bottleneck": int(dw.shape[0]),
            "residual_scale": float(st0.get("residual_scale", 0.2)),
            "s": float(bobj["s"]),
            "ln_eps": 1e-5,
            "dtype": "float16",
            "offsets": {"down_w": off_dw, "up_w": off_uw, "W": off_W},
            "down_b": [float(v) for v in st0["down.bias"].float().numpy()],
            "up_b": [float(v) for v in st0["up.bias"].float().numpy()],
            "ln_w": [float(v) for v in st0["layer_norm.weight"].float().numpy()],
            "ln_b": [float(v) for v in st0["layer_norm.bias"].float().numpy()],
        })

    out = out or os.path.join(root, "offline")
    os.makedirs(out, exist_ok=True)
    sha8 = FUSION_HEAD_SHA256[:8]
    bin_name = f"head_{SLOT_TAG}_{sha8}_fp16.bin"
    json_name = f"head_{SLOT_TAG}_{sha8}.json"
    bin_bytes = bytes(buf)
    bin_sha = hashlib.sha256(bin_bytes).hexdigest()
    with open(os.path.join(out, bin_name), "wb") as f:
        f.write(bin_bytes)

    meta = {
        "schema": "ithomiini-offline-fusion-head/1",
        "slot": SLOT_TAG,
        "head_sha256": FUSION_HEAD_SHA256,
        "compat_live_head_sha256": COMPAT_LIVE_HEAD_SHA256,
        "weights": bin_name,
        "weights_sha256": bin_sha,
        "weights_bytes": len(bin_bytes),
        "rows": len(leaves),
        "K": 1,
        "blend_mode": str(spec["blend_mode"]),
        "T_disp": float(spec["T_disp"]),
        "branches": branches_meta,
        "species": list(ck["rank_vocab"]["species"]),
        "leaf2sp": [int(v) for v in ck["leaf2rank"]["species"]],
        "leaves": leaves,
        "is_ssp": "".join("1" if lf.count(" ") >= 2 else "0" for lf in leaves),
        "geo": "".join(flags) if has_checklist else None,
        "full_photo_overrides": sorted(r["source_sha256"] for r in overrides["overrides"]),
    }
    json_bytes = json.dumps(meta, separators=(",", ":")).encode("utf-8")
    json_sha = hashlib.sha256(json_bytes).hexdigest()
    with open(os.path.join(out, json_name), "wb") as f:
        f.write(json_bytes)
    print(out, len(branches_meta), len(leaves), len(meta["species"]), len(bin_bytes), bin_sha, json_sha)
    if bin_sha != EXPECTED_BIN_SHA256:
        raise SystemExit(f"{bin_name}: SHA-256 {bin_sha} differs from pinned {EXPECTED_BIN_SHA256}")
    if json_sha != EXPECTED_JSON_SHA256:
        raise SystemExit(f"{json_name}: SHA-256 {json_sha} differs from pinned {EXPECTED_JSON_SHA256}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2] if len(sys.argv) > 2 else None)
