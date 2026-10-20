"""The collection sex model's inputs (tools/identifier/worker.py, SexModel): the 19 Oct 2026 CNN model
(WingsClassificator reports/release_20261019_sexcnn, autoresearch w07c cnn_strips4) takes its wing boxes from the nano
wing chain the readings already run on every photo (nano_boxes), cuts the 4 androconial strips (cut_boxes, roles
<wing>_s: the upper half of each dorsal hindwing's padded box, the lower half of each ventral forewing's) and feeds a
small network each strip letterboxed into 112 x 336 px (cnn_input), as build_w07_strips.py made them for its training;
whatever strips a butterfly has.
"""

import math

WINGS = ("vf_l", "vf_r", "dh_l", "dh_r")
# each wing's androconial strip: the upper half of a dorsal hindwing's padded box (the costa, male hair-pencils), the
# lower half of a ventral forewing's (the inner margin that rubs over it)
STRIPS = tuple(f"{w}_s" for w in WINGS)
KNOWN = STRIPS
PAD = 0.15


def bounds(box, size, pad=PAD):
    w, h = size
    x1, y1, x2, y2 = box
    dx, dy = (x2 - x1) * pad, (y2 - y1) * pad
    return (max(0, math.floor((x1 - dx) * w)), max(0, math.floor((y1 - dy) * h)), min(w, math.ceil((x2 + dx) * w)), min(h, math.ceil((y2 + dy) * h)))


def nano_boxes(readings, partial=False):
    """{vf_l, vf_r, dh_l, dh_r: [x1, y1, x2, y2] fractions of the photo as shown} from the nano wing chain's readings
    of the v and d photos ({"d"|"v": reading}, a reading as server/readings-worker.mjs wingsOf gives it: wings [{box
    (fractions as shown), kind, half, score}], outlines [[kind, half, flat 0..1000] | null] in the same order, turnK).
    Per role the wing of that kind and image half with the highest score; its box = its outline's bounding box (its
    own box when it has no outline). ValueError("<view>:<why>") when a photo cannot give both: no reading, a photo the
    chain turned (turnK != 0, as in training), or a half without that wing.
    partial (the crop-dropout model): the wings that are there; ValueError("no_wings") only when none is."""
    out, why = {}, []
    for view, kind, prefix in (("v", "forewing", "vf"), ("d", "hindwing", "dh")):
        r = (readings or {}).get(view)
        if not r or not isinstance(r.get("wings"), list) or r.get("turnK"):
            reason = f"{view}:turned_photo" if r and isinstance(r.get("wings"), list) else f"{view}:no_wing_reading"
            if not partial:
                raise ValueError(reason)
            why.append(reason)
            continue
        outlines = r.get("outlines") or [None] * len(r["wings"])
        best = {}
        for w, ol in zip(r["wings"], outlines):
            if w.get("kind") != kind or w.get("half") not in ("left", "right"):
                continue
            h = w["half"][0]
            score = w.get("score") if w.get("score") is not None else -1.0
            if h in best and best[h][0] >= score:
                continue
            if ol and len(ol) == 3 and ol[2]:
                xs, ys = ol[2][0::2], ol[2][1::2]
                box = [min(xs) / 1000, min(ys) / 1000, max(xs) / 1000, max(ys) / 1000]
            else:
                box = list(w["box"])
            best[h] = (score, box)
        if ("l" not in best or "r" not in best) and not partial:
            raise ValueError(f"{view}:{kind}_halves_{''.join(sorted(best))}")
        for h, (_, box) in best.items():
            out[f"{prefix}_{h}"] = box
    for k, b in list(out.items()):
        if not (0 <= b[0] < b[2] <= 1 and 0 <= b[1] < b[3] <= 1):
            if not partial:
                raise ValueError("invalid_normalized_box")
            del out[k]
    if partial and not out:
        raise ValueError("no_wings" + (f" ({', '.join(why)})" if why else ""))
    return out


def cut_boxes(crops, images, boxes):
    """[(role, PIL strip)] in the order of `crops` (<wing>_s roles); boxes: {vf_l, vf_r, dh_l, dh_r: [x1, y1, x2, y2]
    fractions}; the strip of a wing missing from `boxes` is left out."""
    out = []
    for role in crops:
        wing = role[:4]
        if role not in STRIPS:
            raise ValueError(f"unknown sex crop {role}")
        if wing not in boxes:
            continue
        image = images["v" if wing.startswith("vf") else "d"]
        x1, y1, x2, y2 = bounds(boxes[wing], image.size)
        mid = (y1 + y2) // 2
        y_top, y_bottom = (y1, mid) if wing.startswith("dh") else (mid, y2)
        out.append((role, image.crop((x1, y_top, x2, y_bottom))))
    return out


def cnn_input(crop, stored=(112, 336)):
    """A strip as the CNN's training stored it (build_w07_strips.py): letterboxed (grey, bicubic) straight into an
    h x w canvas. uint8 [h, w, 3]."""
    import numpy as np
    from PIL import Image

    if crop.width < 1 or crop.height < 1:
        raise ValueError("empty_crop")
    H, W = stored
    s = min(W / crop.width, H / crop.height)
    w, h = max(1, round(crop.width * s)), max(1, round(crop.height * s))
    c = Image.new("RGB", (W, H), (128, 128, 128))
    c.paste(crop.resize((w, h), Image.Resampling.BICUBIC), ((W - w) // 2, (H - h) // 2))
    return np.asarray(c)
