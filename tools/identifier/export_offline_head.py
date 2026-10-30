"""The identifier's head for the offline test page (/prueba-offline/, server/offline-id.mjs), in a form a browser reads.

Run on the server with the identifier's venv (inference only, nothing trained):
  shared/identifier/venv/bin/python -I tools/identifier/export_offline_head.py shared/identifier

Writes under <dir>/offline/ (the only new files; the encoder and the wing detector are served from where they are):
  head_<sha8>_fp16.bin   the class weights L2-normalised per row, float16, rows x 1024, little-endian
  head_<sha8>.json       LayerNorm weight and bias, scale, K, the species of each leaf, the geographic prior's flags
                         (worker.py weights: per leaf one hex digit, 1 known, 2 recorded in Ecuador, 4 East, 8 West)
                         and the reviewed full-photo overrides (by the photo's SHA-256)
The 29 Oct 2026 pilot (offline-id-pilot RESULTS.md) found fp16 weights give the same top-5 lists as fp32.
"""

import hashlib
import json
import os
import sys

import numpy as np
import torch

HEAD_FILE = "head_hier_b8127dbf.pt"
HEAD_SHA256 = "b8127dbf0591630c99a9e4af1c00eb8d50f217acfb795fe9e23093702e816d42"


def main(root):
    assets = os.path.join(root, "app", "assets")
    raw = open(os.path.join(assets, HEAD_FILE), "rb").read()
    if hashlib.sha256(raw).hexdigest() != HEAD_SHA256:
        raise SystemExit(HEAD_FILE + ": SHA-256 differs from the release's head")
    ck = torch.load(os.path.join(assets, HEAD_FILE), map_location="cpu", weights_only=False)
    sd = ck["head_state_dict"]
    leaves = list(ck["leaves"])
    W = torch.nn.functional.normalize(sd["weight"].float(), dim=1).numpy()
    K = int(ck.get("K", 1))
    if W.shape != (len(leaves) * K, ck["feat_dim"]):
        raise SystemExit(f"unexpected weight shape {W.shape}")

    flags = ["0"] * len(leaves)
    checklist = os.path.join(assets, "region_checklist.json")
    if os.path.exists(checklist):
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
    out = os.path.join(root, "offline")
    os.makedirs(out, exist_ok=True)
    name = "head_" + HEAD_SHA256[:8]
    W.astype("<f2").tofile(os.path.join(out, name + "_fp16.bin"))
    meta = {
        "schema": "ithomiini-offline-head/1",
        "head_sha256": HEAD_SHA256,
        "weights": name + "_fp16.bin",
        "rows": int(W.shape[0]),
        "dim": int(W.shape[1]),
        "K": K,
        "s": 30.0,
        "ln_eps": 1e-5,
        "ln_w": [float(v) for v in sd["layer_norm.weight"].float().numpy()],
        "ln_b": [float(v) for v in sd["layer_norm.bias"].float().numpy()],
        "species": list(ck["rank_vocab"]["species"]),
        "leaf2sp": [int(v) for v in ck["leaf2rank"]["species"]],
        "geo": "".join(flags) if os.path.exists(checklist) else None,
        "full_photo_overrides": sorted(r["source_sha256"] for r in overrides["overrides"]),
    }
    with open(os.path.join(out, name + ".json"), "w") as f:
        json.dump(meta, f, separators=(",", ":"))
    print(out, W.shape, K, len(meta["species"]))


if __name__ == "__main__":
    main(sys.argv[1])
