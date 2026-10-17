"""The team's AI Identifier on the app server: which species a butterfly photo shows.

The same pipeline as the identifier's Space (WingsClassificator/hf-space, release of 14 Oct 2026):
the photo upright by its EXIF tag, the Wings v6 detector at 1280 px (as ONNX), the wings cropped
(the central 90 % when nothing is found) and padded to a 224 px square, BioCLIP 2.5-H's image
encoder (ONNX, dynamic int8), then the trained hierarchical head (15,347 names). Optionally the
Space's geographic prior: names not recorded on that side of the Andes in Ecuador weigh 2 %.

Started by the app (server/identifier.mjs), one per app. JSON lines on stdin, one answer per
line on stdout, one photo at a time:
  {"id": "1", "path": "/abs/photo.jpg", "side": "East"|"West"|null, "box": [x0,y0,x1,y1]|null}
  -> {"id": "1", "species": [[name, p], ...], "subspecies": [...], "genus": [...], "crop": ..., "ms": ...}
     or {"id": "1", "error": "..."}
The models load on the first photo (about 4 s, 2.3 GB) and the process ends after --idle seconds
without photos (the memory goes back to the server), or when stdin closes (the app stopped).

Files under --dir (shared/identifier on the server, tools/identifier/setup.sh):
  app/v6_feature.py, app/scripts/wings_v6/...    the Space's crop code
  app/assets/wing_seg_v6.pt, wing_seg_v6.onnx, field_wingtip_override_v1.json, head_hier.pt
  app/assets/region_checklist.json               (optional: the geographic prior)
  onnx/bioclip25h_visual_int8.onnx
"""

import argparse
import gc
import hashlib
import json
import os
import select
import sys
import time

HEAD_SHA256 = "749fbffd5f73f12c0f8598b5b2e3f575c0510b93a61c3f3c0123ab2ee8990830"
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
    return p.parse_args()


ARGS = parse_args()
# The app keeps a core: the worker's threads on the last cores, at a lower priority than the app.
try:
    if ARGS.no_pin:
        raise OSError
    cpus = sorted(os.sched_getaffinity(0))
    if len(cpus) > ARGS.threads:
        os.sched_setaffinity(0, cpus[-ARGS.threads:])
    os.nice(5)
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
        self.v6 = V6FeatureExtractor(os.path.join(assets, "wing_seg_v6.pt"), os.path.join(assets, "field_wingtip_override_v1.json"))
        self.v6.detector = YOLO(os.path.join(assets, "wing_seg_v6.onnx"), task="segment")
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

    def weights(self, side):
        np = self.np
        w = np.ones(len(self.leaves), np.float32)
        if self.geo is None or side not in ("East", "West"):
            return w
        # In Ecuador, on that side of the Andes (hf-space geo_prior.py leaf_weights, eps 0.02).
        off = ~self.geo[side] | ~self.geo["inec"]
        w[self.geo["known"] & off] = 0.02
        return w

    def identify(self, path, side=None, box=None):
        np, torch = self.np, self.torch
        t0 = time.perf_counter()
        with open(path, "rb") as f:
            raw = f.read()
        square, prov = self.v6.crop(raw, manual_box=box) if box else self.v6.crop(raw)
        x = ((np.asarray(square.convert("RGB"), np.float32).transpose(2, 0, 1) / 255.0 - self.mean) / self.std)[None]
        feat = self.encoder.run(None, {"pixel_values": x})[0]
        feat = feat / np.linalg.norm(feat, axis=1, keepdims=True)
        with torch.no_grad():
            probs = self.head(torch.from_numpy(feat.astype(np.float32))).softmax(1)[0].numpy().astype(np.float64)
        w = self.weights(side)
        prior = bool((w < 1).any())
        if prior:
            pr = probs * w
            probs = pr / pr.sum() if pr.sum() > 0 else probs

        def marg(leaf2r, vocab, k):
            agg = np.zeros(len(vocab))
            np.add.at(agg, leaf2r, probs)
            return [[vocab[j], round(float(agg[j]), 4)] for j in np.argsort(-agg)[:k]]

        order = np.argsort(-probs)
        subs = []
        for i in order:
            if self.is_ssp[i]:
                subs.append([self.leaves[i], round(float(probs[i]), 4)])
                if len(subs) >= TOP:
                    break
        geometry = prov.get("geometry") or {}
        return {
            "species": marg(self.leaf2sp, self.sp_vocab, TOP),
            "subspecies": subs,
            "genus": marg(self.leaf2ge, self.ge_vocab, 3),
            "top": [self.leaves[order[0]], round(float(probs[order[0]]), 4)],
            "crop": geometry.get("status"),
            "wingBox": prov.get("wing_box"),
            "prior": side if prior else None,
            "ms": round((time.perf_counter() - t0) * 1000),
            "rssMb": rss_mb(),
        }


def main():
    model = None
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
            out = model.identify(job["path"], side=job.get("side"), box=job.get("box"))
            if first:
                out["loadMs"] = model.load_ms
            send({"id": jid, **out})
        except Exception as e:  # one photo's problem is that photo's answer
            send({"id": jid, "error": f"{type(e).__name__}: {e}"[:300]})


if __name__ == "__main__":
    sys.exit(main())
