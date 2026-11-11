#!/usr/bin/env python3
"""The Neotropical countries' outlines for Identificar (frontend/src/lib/photoCountry.ts): which country a photo's GPS
position is in, on the phone and without signal, so Compare and the search show that country's photos first.

  python3 -I scripts/country-shapes.py --ne ne_50m_admin_0_countries.geojson [--out frontend/public/identificar/countries.json]

Input: Natural Earth 1:50m Admin 0 Countries (public domain), the GeoJSON from
https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_50m_admin_0_countries.geojson
Kept: the 49 Neotropical countries and territories of the photo packs (the offline-packs research's countries.py), by
ISO_A2_EH. France's and the Netherlands' Caribbean and South American parts are split off by position (French Guiana,
Guadeloupe, Martinique; Bonaire, Sint Eustatius and Saba). Each ring is simplified (Douglas-Peucker, --tolerance
degrees, default 0.01: about 1 km) and written as integers of 1/1000 degree, flat [lon, lat, lon, lat, ...]. A
country is a list of rings (outer and holes alike: the page tests a point with the even-odd rule) and its box.
"""
import argparse
import json
import os

NEOTROPICS = {
    "MX", "GT", "BZ", "SV", "HN", "NI", "CR", "PA",
    "CU", "JM", "HT", "DO", "PR", "BS", "TC", "KY", "VG", "VI", "AI", "MF", "SX", "BL", "KN", "AG", "MS", "GP",
    "DM", "MQ", "LC", "VC", "BB", "GD", "TT", "AW", "CW", "BQ",
    "CO", "VE", "GY", "SR", "GF", "EC", "PE", "BO", "BR", "PY", "UY", "AR", "CL",
}
# Overseas parts inside a European country's outline: (country, ISO2, lon min, lon max, lat min, lat max).
SPLIT = [
    ("FR", "GF", -55.0, -51.0, 1.5, 6.5),
    ("FR", "GP", -62.0, -60.8, 15.7, 16.6),
    ("FR", "MQ", -61.3, -60.7, 14.3, 15.0),
    ("NL", "BQ", -69.0, -62.8, 11.9, 17.7),
]


def simplify(points, tol):
    """Douglas-Peucker on a closed ring (iterative)."""
    if len(points) < 5:
        return points
    keep = [False] * len(points)
    keep[0] = keep[-1] = True
    stack = [(0, len(points) - 1)]
    while stack:
        a, b = stack.pop()
        ax, ay = points[a]
        bx, by = points[b]
        dx, dy = bx - ax, by - ay
        norm = (dx * dx + dy * dy) ** 0.5
        far, at = -1.0, None
        for i in range(a + 1, b):
            px, py = points[i]
            d = abs(dy * (px - ax) - dx * (py - ay)) / norm if norm else ((px - ax) ** 2 + (py - ay) ** 2) ** 0.5
            if d > far:
                far, at = d, i
        if at is not None and far > tol:
            keep[at] = True
            stack += [(a, at), (at, b)]
    out = [p for p, k in zip(points, keep) if k]
    return out if len(out) >= 4 else points


def rings_of(geom):
    polys = geom["coordinates"] if geom["type"] == "MultiPolygon" else [geom["coordinates"]]
    for poly in polys:
        for ring in poly:
            yield ring


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--ne", required=True)
    ap.add_argument("--out", default=os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "frontend", "public", "identificar", "countries.json"))
    ap.add_argument("--tolerance", type=float, default=0.01)
    a = ap.parse_args()
    ne = json.load(open(a.ne, encoding="utf-8"))
    out = {}
    for f in ne["features"]:
        iso = f["properties"].get("ISO_A2_EH") or f["properties"].get("ISO_A2")
        for ring in rings_of(f["geometry"]):
            lon = sum(p[0] for p in ring) / len(ring)
            lat = sum(p[1] for p in ring) / len(ring)
            code = iso
            for country, part, x0, x1, y0, y1 in SPLIT:
                if iso == country and x0 <= lon <= x1 and y0 <= lat <= y1:
                    code = part
            if code not in NEOTROPICS:
                continue
            pts = simplify([(p[0], p[1]) for p in ring], a.tolerance)
            flat = []
            for x, y in pts:
                flat += [round(x * 1000), round(y * 1000)]
            out.setdefault(code, []).append(flat)
    missing = sorted(NEOTROPICS - set(out))
    countries = {}
    for code, rings in sorted(out.items()):
        xs = [v for r in rings for v in r[0::2]]
        ys = [v for r in rings for v in r[1::2]]
        countries[code] = {"box": [min(xs), min(ys), max(xs), max(ys)], "rings": rings}
    doc = {
        "schema": "ithomiini-countries/1",
        "source": "Natural Earth 1:50m Admin 0 Countries (public domain), simplified (scripts/country-shapes.py)",
        "scale": 1000,
        "countries": countries,
    }
    with open(a.out, "w", encoding="utf-8") as fh:
        json.dump(doc, fh, separators=(",", ":"))
    print(f"{a.out}: {len(countries)} countries, {sum(len(c['rings']) for c in countries.values())} rings, {os.path.getsize(a.out) / 1024:.0f} KB; not in the input: {missing}")


if __name__ == "__main__":
    main()
