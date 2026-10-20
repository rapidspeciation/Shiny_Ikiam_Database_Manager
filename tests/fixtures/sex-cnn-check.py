"""Checks the CNN sex model's inputs in tools/identifier/sex_crops.py (nano_boxes, cut_boxes, cnn_input) against the
training's own code (WingsClassificator autoresearch w07 build_w07_crops.py's cut and w07c build_w07_strips.py's
strip and letterbox; copied below) on a made-up pair. Run by tests/sex-crops.test.mjs: python3 -I sex-cnn-check.py <tools/identifier>. Prints one JSON line."""
import json
import math
import sys

sys.path.insert(0, sys.argv[1])
import numpy as np  # noqa: E402
from PIL import Image  # noqa: E402

import sex_crops as S  # noqa: E402

ROLES = ['vf_l', 'vf_r', 'dh_l', 'dh_r', 'vf_l_t0', 'vf_l_t1', 'vf_r_t0', 'vf_r_t1', 'dh_l_t0', 'dh_l_t1', 'dh_r_t0', 'dh_r_t1']


# --- build_w07_crops.py (training)
def t_bounds(box, size, pad=0.15):
    w, h = size
    x1, y1, x2, y2 = box
    dx, dy = (x2 - x1) * pad, (y2 - y1) * pad
    return (max(0, math.floor((x1 - dx) * w)), max(0, math.floor((y1 - dy) * h)), min(w, math.ceil((x2 + dx) * w)), min(h, math.ceil((y2 + dy) * h)))


def t_cut(image, boxes):
    res = []
    for role in ROLES:
        wing = role[:4]
        im = image['v' if wing.startswith('vf') else 'd']
        x1, y1, x2, y2 = t_bounds(boxes[wing], im.size)
        if role == wing:
            res.append(im.crop((x1, y1, x2, y2)))
            continue
        mid, xm = (y1 + y2) // 2, (x1 + x2) // 2
        yt, yb = (y1, mid) if wing.startswith('dh') else (mid, y2)
        xl, xr = (x1, xm) if role.endswith('t0') else (xm, x2)
        res.append(im.crop((xl, yt, xr, yb)))
    return res


def picture(seed, size=(1600, 1067)):
    im = Image.new("RGB", size)
    im.putdata([((x * 7 + y * 3 + seed) % 256, (x * 5 + seed * 11) % 256, (y * 9 + seed) % 256) for y in range(size[1]) for x in range(size[0])])
    return im


def flat(x1, y1, x2, y2):  # a box's outline in 0..1000
    return [x1, y1, x2, y1, x2, y2, x1, y2]


def reading(kind_top, kind_bottom, extra=()):
    wings = [{"box": [0.08, 0.10, 0.45, 0.48], "kind": kind_top, "half": "left", "score": 0.9},
             {"box": [0.55, 0.11, 0.93, 0.47], "kind": kind_top, "half": "right", "score": 0.8},
             {"box": [0.12, 0.52, 0.44, 0.86], "kind": kind_bottom, "half": "left", "score": 0.85},
             {"box": [0.56, 0.53, 0.88, 0.85], "kind": kind_bottom, "half": "right", "score": 0.7}] + list(extra)
    outlines = [["fw", "l", flat(90, 110, 440, 470)], None, ["hw", "l", flat(130, 530, 430, 850)], ["hw", "r", flat(570, 540, 870, 840)]] + [None] * len(extra)
    return {"wings": wings, "outlines": outlines, "turnK": 0, "size": [1600, 1067]}


v = reading("forewing", "hindwing", [{"box": [0.5, 0.1, 0.6, 0.2], "kind": "forewing", "half": "right", "score": 0.95}])  # a better right forewing
d = reading("forewing", "hindwing", [{"box": [0.1, 0.5, 0.2, 0.6], "kind": "hindwing", "half": "left", "score": 0.5}])  # a worse left hindwing
boxes = S.nano_boxes({"d": d, "v": v})
out = {"boxes": {k: [round(x, 4) for x in b] for k, b in boxes.items()}}
errors = []
for r in ({"d": d}, {"d": {**d, "turnK": 2}, "v": v}, {"d": {**d, "wings": d["wings"][:2] + d["wings"][3:4]}, "v": v}, {"d": d, "v": {**v, "turnK": 1}}):
    try:
        S.nano_boxes(r)
    except ValueError as e:
        errors.append(str(e))
out["errors"] = errors
# the crop-dropout model takes whatever wings are there
d_one = {**d, "wings": d["wings"][:2] + d["wings"][3:4], "outlines": [None, None, None]}   # only the right hindwing
out["partial"] = sorted(S.nano_boxes({"d": d_one, "v": v}, partial=True))
out["partial_turned"] = sorted(S.nano_boxes({"d": {**d, "turnK": 2}, "v": v}, partial=True))
try:
    S.nano_boxes({"d": {**d, "turnK": 2}}, partial=True)
except ValueError as e:
    out["partial_none"] = str(e)
images = {"d": picture(1), "v": picture(2)}
out["partial_roles"] = [r for r, _ in S.cut_boxes(list(S.STRIPS), images, S.nano_boxes({"d": d_one, "v": v}, partial=True))]
try:
    S.cut_boxes(["vf_l_t0"], images, boxes)
except ValueError as e:
    out["unknown_role"] = str(e)
# --- build_w07_strips.py (training of the released 4-strip model)
def t_strip(wing, upper):
    mid = wing.height // 2
    return wing.crop((0, 0, wing.width, mid)) if upper else wing.crop((0, mid, wing.width, wing.height))


def t_canvas(crop, H=112, W=336):
    s = min(W / crop.width, H / crop.height)
    w, h = max(1, round(crop.width * s)), max(1, round(crop.height * s))
    c = Image.new('RGB', (W, H), (128, 128, 128))
    c.paste(crop.resize((w, h), Image.Resampling.BICUBIC), ((W - w) // 2, (H - h) // 2))
    return np.asarray(c)


STRIPS = ['vf_l_s', 'vf_r_s', 'dh_l_s', 'dh_r_s']
mine = dict(S.cut_boxes(STRIPS, images, boxes))
theirs = {r: t_strip(images['v' if r.startswith('vf') else 'd'].crop(t_bounds(boxes[r[:4]], images['v' if r.startswith('vf') else 'd'].size)), r.startswith('dh')) for r in STRIPS}
out["same_strips"] = all(mine[r].tobytes() == theirs[r].tobytes() and mine[r].size == theirs[r].size for r in STRIPS)
out["strip_tiles"] = all(mine[r].width == full[0].width + full[1].width for r, full in
                         ((r, [c for k, c in zip(ROLES, theirs12) if k in (r[:4] + '_t0', r[:4] + '_t1')]) for r in STRIPS for theirs12 in [t_cut(images, boxes)]))
a = np.stack([S.cnn_input(mine[r], [112, 336]) for r in STRIPS])
b = np.stack([t_canvas(theirs[r]) for r in STRIPS])
out["same_strip_inputs"] = bool((a == b).all())
out["strip_input"] = [list(a.shape), str(a.dtype)]
print(json.dumps(out))
