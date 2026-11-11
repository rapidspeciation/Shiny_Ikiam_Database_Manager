#!/usr/bin/env python3
"""Photo packs (Identificar tab, Descargas): the list identificar/packs/index.json the page chooses packs from
(frontend/src/lib/photoPacks.ts). One entry per taxon: name, rank, species, genus, family, group
('ithomiini' | 'nymphalidae' | 'butterfly' | 'moth'), countries (ISO2), photo {file, attribution, license, taxonUrl,
source} (or null) and bytes (the photo file's size). `version` is a hash of the entries: a phone whose pack has
another version is offered an update.

From the Ithomiini «Fotos para el campo» set (scripts/field-photos.py), its photos left where they are (base ../field/):

  python3 -I scripts/photo-packs.py field --field frontend/public/identificar/field/index.json \
      [--countries base.json] [--out frontend/public/identificar/packs]

  Every entry is group 'ithomiini', family Nymphalidae. Countries: `--countries`, a list of {name, appCountries}
  (the offline-packs research's base.json: the checklist's and the map points' countries), else ['EC'] for an entry
  recorded in Ecuador (`ec`) and none otherwise (only «Neotrópico» takes those).
"""
import argparse
import gzip
import hashlib
import json
import os
import sys
import time

GROUPS = ("ithomiini", "nymphalidae", "butterfly", "moth")


def version_of(taxa):
    body = json.dumps(taxa, ensure_ascii=False, sort_keys=True, separators=(",", ":")).encode()
    return hashlib.sha256(body).hexdigest()[:12]


def check(taxa):
    names = set()
    for t in taxa:
        assert t["name"] not in names, f"twice: {t['name']}"
        names.add(t["name"])
        assert t["rank"] in ("species", "subspecies"), t
        assert t["group"] in GROUPS, t
        assert all(len(c) == 2 and c.isupper() for c in t["countries"]), t
        if t["rank"] == "species":
            assert t["species"] == t["name"], t
        else:
            assert t["name"].startswith(t["species"] + " "), t
        assert t["genus"] == t["name"].split()[0], t
        if t["photo"]:
            for k in ("file", "attribution", "license", "taxonUrl", "source"):
                assert k in t["photo"], (k, t)
            assert t["bytes"] > 0, t


def write(out, taxa, base, source, sample=None):
    check(taxa)
    taxa = sorted(taxa, key=lambda t: t["name"])
    index = {"schema": "ithomiini-photo-packs/1", "version": version_of(taxa), "built": time.strftime("%Y-%m-%d"), "base": base, "source": source}
    if sample:
        index["sample"] = sample
    index["taxa"] = taxa
    os.makedirs(out, exist_ok=True)
    path = os.path.join(out, "index.json")
    with open(path + ".part", "w", encoding="utf-8") as f:
        json.dump(index, f, ensure_ascii=False, separators=(",", ":"))
    os.replace(path + ".part", path)
    # Beside it, compressed (the server sends it to a browser that takes gzip): written after, so never older.
    with open(path, "rb") as f, open(path + ".gz.part", "wb") as g:
        g.write(gzip.compress(f.read(), compresslevel=9, mtime=0))
    os.replace(path + ".gz.part", path + ".gz")
    with_photo = [t for t in taxa if t["photo"]]
    by_group = {g: sum(1 for t in with_photo if t["group"] == g) for g in GROUPS}
    print(f"{path}: {len(taxa)} taxa, {len(with_photo)} with a photo, {sum(t['bytes'] for t in with_photo) / 1048576:.1f} MB; by group {by_group}; version {index['version']}; index {os.path.getsize(path) / 1024:.0f} KB")


def from_field(a):
    field = json.load(open(a.field, encoding="utf-8"))
    countries = {}
    if a.countries:
        for e in json.load(open(a.countries, encoding="utf-8")):
            countries[e["name"]] = sorted(set(e.get("appCountries") or []))
    taxa = []
    for rank, key in (("species", "species"), ("subspecies", "subspecies")):
        for e in field.get(key, []):
            parts = e["name"].split()
            cs = countries.get(e["name"])
            if cs is None:
                cs = ["EC"] if e.get("ec") else []
            elif e.get("ec") and "EC" not in cs:
                cs = sorted(cs + ["EC"])
            taxa.append({
                "name": e["name"], "rank": rank, "species": " ".join(parts[:2]), "genus": parts[0], "family": "Nymphalidae",
                "group": "ithomiini", "countries": cs,
                "photo": {"file": e["file"], "attribution": e["attribution"], "license": e["license"], "taxonUrl": e.get("taxonUrl"), "source": e.get("source")},
                "bytes": e["bytes"],
            })
    rel = os.path.relpath(os.path.dirname(os.path.abspath(a.field)), os.path.abspath(a.out)).replace(os.sep, "/") + "/"
    # Only Ithomiini have photos in this list: the page says so beside the other groups.
    write(a.out, taxa, rel, field.get("source") or "iNaturalist photos under Creative Commons licences", sample="ithomiini-only")


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)
    f = sub.add_parser("field", help="the list from the Ithomiini field set")
    f.add_argument("--field", required=True)
    f.add_argument("--countries")
    f.add_argument("--out", default=os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "frontend", "public", "identificar", "packs"))
    a = ap.parse_args()
    if a.cmd == "field":
        from_field(a)
    else:
        sys.exit(f"unknown command {a.cmd}")


if __name__ == "__main__":
    main()
