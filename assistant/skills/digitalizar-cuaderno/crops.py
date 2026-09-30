#!/usr/bin/env python3
"""Crops of a notebook photo for reading it: an overview with a coordinate ruler, then
strips of ~10 lines of each page, the header row repeated on each strip, straightened
(a tilted or keystoned page becomes a flat rectangle), contrast enhanced, at a readable
zoom. Prints JSON: the files and the lines each strip holds. Needs Pillow (python3-pil).

1. Overview (orientation from EXIF; --rotate 90|180|270 if the page is still sideways):
     python3 crops.py PHOTO --out work/<date>-<topic>
   → overview.jpg with x/y rulers (fractions 0–1 of the photo). Read off it, for each page:
   the page's left and right edge (x), the top of the header row (head), and the top of
   the first written line and the bottom of the last one at the page's left and right
   edges (top, bottom), and count the lines.
2. Strips:
     python3 crops.py PHOTO --out DIR --lines 30 \\
       --left  "x=0.06-0.45 head=0.10 top=0.15,0.14 bottom=0.95,0.93" \\
       --right "x=0.46-0.82 head=0.09 top=0.14,0.13 bottom=0.93,0.92"
   (one page: --page "…"). Options: --per 10 (lines per strip), --enhance strong (faint
   pencil), --rotate as in step 1, --zoom x0,y0,x1,y1 (repeatable: an enlarged detail,
   fractions of the photo, e.g. a crossed-out count).
Every strip also shows ~half a line above and below its range, so a line on a cut is
never lost; the JSON says which lines are the strip's own.
"""
import argparse
import json
import os
import sys

from PIL import Image, ImageDraw, ImageFilter, ImageOps

MAX_W = 1600  # what the model sees at full detail (larger images are shrunk anyway)
MARGIN = 0.7  # lines of context above and below each strip


def load(path, rotate):
    im = ImageOps.exif_transpose(Image.open(path)).convert('RGB')
    if rotate:
        im = im.rotate(-rotate, expand=True)  # clockwise, as a person turns the photo
    return im


def enhance(im, strong):
    if strong:
        im = ImageOps.autocontrast(ImageOps.grayscale(im), cutoff=2)
        im = im.point(lambda v: int(255 * (v / 255) ** 1.6))  # darker faint strokes
        return im.filter(ImageFilter.UnsharpMask(radius=2, percent=140, threshold=2)).convert('RGB')
    try:
        im = ImageOps.autocontrast(im, cutoff=1, preserve_tone=True)
    except TypeError:  # Pillow < 8.2
        im = ImageOps.autocontrast(im, cutoff=1)
    return im.filter(ImageFilter.UnsharpMask(radius=2, percent=70, threshold=3))


def fit(im, width=MAX_W):
    if im.width <= width:
        return im
    return im.resize((width, max(1, round(im.height * width / im.width))), Image.LANCZOS)


def overview(im, out, stem):
    """The photo shrunk, with a ruler of fractions on every side and a faint grid."""
    small = im.copy()
    small.thumbnail((1500, 1500))
    w, h = small.size
    m = 44
    canvas = Image.new('RGB', (w + 2 * m, h + 2 * m), 'white')
    canvas.paste(small, (m, m))
    d = ImageDraw.Draw(canvas)
    for i in range(0, 101):
        f = i / 100
        major = i % 10 == 0
        if i % 5 and not major:
            tick = 4
        else:
            tick = 12 if major else 8
        x, y = m + f * w, m + f * h
        for (a, b) in (((x, m - tick), (x, m)), ((x, m + h), (x, m + h + tick)),
                       ((m - tick, y), (m, y)), ((m + w, y), (m + w + tick, y))):
            d.line([a, b], fill='black', width=1)
        if i % 5 == 0:
            d.line([(x, m), (x, m + h)], fill=(255, 0, 0) if major else (255, 150, 150), width=1)
            d.line([(m, y), (m + w, y)], fill=(0, 0, 255) if major else (150, 150, 255), width=1)
            label = f'{f:.2f}'.lstrip('0') if i not in (0, 100) else str(i // 100)
            d.text((x - 10, 2), label, fill='red')
            d.text((x - 10, m + h + 26), label, fill='red')
            d.text((2, y - 6), label, fill='blue')
            d.text((m + w + 14, y - 6), label, fill='blue')
    path = os.path.join(out, f'{stem}-overview.jpg')
    canvas.save(path, quality=88)
    return path


def parse_page(text, name):
    spec = {}
    for part in text.replace(';', ' ').split():
        key, _, value = part.partition('=')
        spec[key.strip()] = value.strip()
    try:
        x = [float(v) for v in spec['x'].replace('-', ',').split(',')]
        xb = [float(v) for v in spec['xb'].replace('-', ',').split(',')] if 'xb' in spec else x
        top = [float(v) for v in spec['top'].split(',')]
        bottom = [float(v) for v in spec['bottom'].split(',')]
    except (KeyError, ValueError):
        sys.exit(f'--{name}: give x=X0-X1 top=YL[,YR] bottom=YL[,YR] [head=YL[,YR]] [xb=X0-X1] [lines=N]')
    top = top * 2 if len(top) == 1 else top
    bottom = bottom * 2 if len(bottom) == 1 else bottom
    head = [float(v) for v in spec['head'].split(',')] if 'head' in spec else None
    if head and len(head) == 1:
        head = [head[0], head[0] + (top[1] - top[0])]  # the header tilts like the lines
    return {'name': name, 'x': x, 'xb': xb, 'top': top, 'bottom': bottom, 'head': head,
            'lines': int(spec['lines']) if 'lines' in spec else None}


def solve(a, b):
    """Gaussian elimination (8×8): no numpy needed."""
    n = len(b)
    m = [row[:] + [b[i]] for i, row in enumerate(a)]
    for c in range(n):
        p = max(range(c, n), key=lambda r: abs(m[r][c]))
        m[c], m[p] = m[p], m[c]
        for r in range(n):
            if r != c and m[c][c]:
                f = m[r][c] / m[c][c]
                m[r] = [x - f * y for x, y in zip(m[r], m[c])]
    return [m[i][n] / m[i][i] for i in range(n)]


def rectify(im, corners, size):
    """The quadrilateral `corners` (TL, TR, BR, BL in pixels) as a flat w×h image (perspective)."""
    w, h = size
    dst = [(0, 0), (w, 0), (w, h), (0, h)]
    a, b = [], []
    for (x, y), (u, v) in zip(dst, corners):
        a.append([x, y, 1, 0, 0, 0, -u * x, -u * y]); b.append(u)
        a.append([0, 0, 0, x, y, 1, -v * x, -v * y]); b.append(v)
    return im.transform((w, h), Image.PERSPECTIVE, solve(a, b), Image.BICUBIC, fillcolor='white')


def dist(p, q):
    return ((p[0] - q[0]) ** 2 + (p[1] - q[1]) ** 2) ** 0.5


def page_images(im, page):
    """The page's header band and body (first line's top to last line's bottom), straightened."""
    W, H = im.size
    (xl, xr), (xlb, xrb) = page['x'], page['xb']
    (tl, tr), (bl, br) = page['top'], page['bottom']
    # The page's side edges, from the top of the body to its bottom (x may differ at the bottom).
    at = lambda xt, xb, y0, y1, y: (W * (xt + (xb - xt) * (y - y0) / ((y1 - y0) or 1)), H * y)
    TL, TR = at(xl, xlb, tl, bl, tl), at(xr, xrb, tr, br, tr)
    BL, BR = at(xl, xlb, tl, bl, bl), at(xr, xrb, tr, br, br)
    width = round((dist(TL, TR) + dist(BL, BR)) / 2)
    height = round((dist(TL, BL) + dist(TR, BR)) / 2)
    body = rectify(im, [TL, TR, BR, BL], (width, height))
    header = None
    if page['head']:
        hl, hr = page['head']
        HL, HR = at(xl, xlb, tl, bl, hl), at(xr, xrb, tr, br, hr)
        hh = max(8, round((dist(HL, TL) + dist(HR, TR)) / 2))
        header = rectify(im, [HL, HR, TR, TL], (width, hh))
    return header, body


def ruling(gray, x0, x1, n, step):
    """The printed ruled lines in a vertical slice: (period, offset of the first, strength).
    The line tops the person gave are close; the ruling makes them exact."""
    H = gray.height
    col = gray.crop((x0, 0, x1, H)).resize((1, H), Image.BOX).tobytes()
    ink = [255 - v for v in col]
    k = max(2, int(step * 0.6))
    acc = [0]
    for v in ink:
        acc.append(acc[-1] + v)
    prof = [ink[i] - (acc[min(H, i + k)] - acc[max(0, i - k)]) / (min(H, i + k) - max(0, i - k)) for i in range(H)]
    def score(p, ph):
        total = 0
        for j in range(n + 1):
            i = int(round(ph + j * p))
            if 0 <= i < H:
                total += max(prof[max(0, i - 1):i + 2])
        return total
    # The first and the last border may each move up to about half a line.
    best = (float('-inf'), step, 0.0)
    shifts = [step * t / 20 for t in range(-11, 12)]
    for top in shifts:
        for bottom in shifts:
            p = (H + bottom - top) / n
            sc = score(p, top)
            if sc > best[0]:
                best = (sc, p, top)
    sc, p, ph = best
    # Strength: the best fit against the same period half a line off.
    off = score(p, ph + p / 2)
    return p, ph, (sc - off) / (abs(off) + 1e-6 + (n + 1))


def snap(body, n):
    """The body re-straightened so its ruled lines fall exactly on the n+1 line borders
    (fitted on the left and right quarter of the page); unchanged when the ruling is unclear."""
    gray = ImageOps.grayscale(body)
    W, H = gray.size
    step = H / n
    fits = [(xc, ruling(gray, a, b, n, step)) for xc, a, b in ((0.125, 0, W // 4), (0.875, 3 * W // 4, W))]
    good = [(xc, p, ph) for xc, (p, ph, strength) in fits if strength > 0.5]
    if not good:
        return body, None
    if len(good) == 1:
        xc, p, ph = good[0]
        good = [(0.125, p, ph), (0.875, p, ph)]
    (xa, pa, pha), (xb, pb, phb) = good
    at = lambda ya, yb, x: ya + (yb - ya) * (x - xa) / (xb - xa)
    top0, topW = at(pha, phb, 0), at(pha, phb, 1)
    bot0, botW = at(pha + n * pa, phb + n * pb, 0), at(pha + n * pa, phb + n * pb, 1)
    height = round(((bot0 - top0) + (botW - topW)) / 2)
    fixed = rectify(body, [(0, top0), (W, topW), (W, botW), (0, bot0)], (W, height))
    return fixed, round(max(abs(top0), abs(topW), abs(bot0 - H), abs(botW - H)) / step, 2)


def strips(im, page, lines, per, strong, out, stem, key=None):
    """Strips of `per` lines. `key`: the left page's first column (clutch/ID) cut on the same
    lines, put before each right-page strip so every line shows its ID."""
    n = page['lines'] or lines
    if not n:
        sys.exit('Give --lines N (the written lines of the page) or lines=N in the page')
    header, body = page_images(im, page)
    body, moved = snap(body, n)
    step = body.height / n
    result = []
    for first in range(0, n, per):
        last = min(first + per, n)
        # Some context above and below, so a line on the cut is whole in one of the strips.
        a, b = first - MARGIN, last + MARGIN
        band = body.crop((0, max(0, round(a * step)), body.width, min(body.height, round(b * step))))
        head = header
        if key is not None:
            kh, kb, kn = key
            ks = kb.height / kn
            kband = kb.crop((0, max(0, round(a * ks)), kb.width, min(kb.height, round(b * ks)))).resize((kb.width, band.height))
            band = side(kband, band)
            if head is not None:
                head = side(kh.resize((kh.width, head.height)) if kh is not None else Image.new('RGB', (kb.width, head.height), 'white'), head)
        if head is not None:
            joined = Image.new('RGB', (band.width, head.height + 6 + band.height), (200, 0, 0))
            joined.paste(head, (0, 0))
            joined.paste(band, (0, head.height + 6))
            band = joined
        band = fit(enhance(band, strong))
        path = os.path.join(out, f'{stem}-{page["name"][0].upper()}{first + 1:02d}-{last:02d}.jpg')
        band.save(path, quality=90)
        result.append({'page': page['name'], 'lines': [first + 1, last], 'path': path})
    if result:
        # How far (in lines) the ruled lines moved the borders given; null: no clear ruling.
        result[0]['snapped'] = moved
    return result, (header, body, n)


def side(a, b):
    """a | b, with a red bar between (a is the left page's ID column)."""
    joined = Image.new('RGB', (a.width + 6 + b.width, max(a.height, b.height)), (200, 0, 0))
    joined.paste(a, (0, 0))
    joined.paste(b, (a.width + 6, 0))
    return joined


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument('photo')
    p.add_argument('--out', required=True, help='work/<date>-<topic>/ of this chat')
    p.add_argument('--rotate', type=int, default=0, choices=[0, 90, 180, 270])
    p.add_argument('--left')
    p.add_argument('--right')
    p.add_argument('--page')
    p.add_argument('--lines', type=int)
    p.add_argument('--per', type=int, default=10)
    p.add_argument('--key', type=float, default=0.12,
                   help="width of the left page's ID column put before each right-page strip (fraction of the left page; 0: none)")
    p.add_argument('--enhance', choices=['normal', 'strong'], default='normal')
    p.add_argument('--zoom', action='append', default=[], help='x0,y0,x1,y1 (fractions of the photo)')
    args = p.parse_args()
    os.makedirs(args.out, exist_ok=True)
    stem = os.path.splitext(os.path.basename(args.photo))[0]
    im = load(args.photo, args.rotate)
    strong = args.enhance == 'strong'
    out = {'photo': args.photo, 'size': im.size}
    pages = [parse_page(t, n) for n, t in (('left', args.left), ('right', args.right), ('page', args.page)) if t]
    if not pages and not args.zoom:
        out['overview'] = overview(im, args.out, stem)
        out['next'] = 'Read x, head, top and bottom of each page off the rulers, count the lines, then run again with --left/--right (or --page) and --lines.'
    out['strips'] = []
    key = None
    for page in pages:
        made, (header, body, n) = strips(im, page, args.lines, max(3, args.per), strong, args.out, stem,
                                         key if page['name'] == 'right' else None)
        out['strips'] += made
        if page['name'] == 'left' and args.key > 0:
            w = max(1, round(body.width * args.key))
            key = (header.crop((0, 0, w, header.height)) if header is not None else None,
                   body.crop((0, 0, w, body.height)), n)
    zooms = []
    for i, box in enumerate(args.zoom, 1):
        x0, y0, x1, y1 = (float(v) for v in box.split(','))
        W, H = im.size
        crop = im.crop((round(x0 * W), round(y0 * H), round(x1 * W), round(y1 * H)))
        scale = min(3, MAX_W / max(1, crop.width))
        if scale > 1:
            crop = crop.resize((round(crop.width * scale), round(crop.height * scale)), Image.LANCZOS)
        path = os.path.join(args.out, f'{stem}-zoom{i}.jpg')
        fit(enhance(crop, strong)).save(path, quality=92)
        zooms.append({'box': box, 'path': path})
    if zooms:
        out['zooms'] = zooms
    print(json.dumps(out, indent=1))


if __name__ == '__main__':
    main()
