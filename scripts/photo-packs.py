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
  recorded in Ecuador (`ec`) and none otherwise (only «Neotrópico» takes those). Those files are named by their
  iNaturalist photo id and never rewritten (scripts/field-photos.py), so a name always means the same bytes.

From the per-taxon list (the offline-packs research's taxa.json: name, rank, species, genus, family, group, countries,
photo {url, inatPhotoId, attribution, license, taxonUrl, source}, bytes), its photos made here as mobile WebP:

  python3 -I scripts/photo-packs.py taxa --taxa taxa.json --out <PACKS_DIR> \
      [--cell EC:all --cell NT:butterflies] [--size 400] [--quality 60] [--workers 3] [--pause 0.4] [--limit N] \
      [--fallback frontend/public/identificar/field]

  --cell REGION:SCOPE chooses the photos made on this server (REGION an ISO2 code, codes joined by +, or NT for any
  Neotropical country; SCOPE ithomiini, nymphalidae, butterflies or all, each taking the groups before it); default
  NT:all. Each photo (taxa.json's iNaturalist «medium» URL, or a local path) is fetched once, resized to --size px on
  its long side (Lanczos) and written as photos/<inatPhotoId>-<sha256 of the WebP, 8 hex>.webp at --quality (method
  6): a changed photo gets a new name, so a phone never keeps old bytes under a name the list gives (the page also
  checks each kept photo's size against `bytes`). Resumable: a photo already in photos/ is not fetched again.
  Photos outside the cells stay in the list with `missing: true` and a size estimated from the made photos of their
  group; the page says how many of a choice are not on the server yet and saves the rest. Files the list no longer
  names are printed, for the caller to remove. Network: the photo host in `url` (--workers at once, each waiting
  --pause s after a download), with a User-Agent that names the tool only.
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
            assert t["bytes"] > 0 or t["photo"].get("missing"), t


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
    with_photo = [t for t in taxa if t["photo"] and not t["photo"].get("missing")]
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


SCOPE_GROUPS = {
    "ithomiini": {"ithomiini"},
    "nymphalidae": {"ithomiini", "nymphalidae"},
    "butterflies": {"ithomiini", "nymphalidae", "butterfly"},
    "all": {"ithomiini", "nymphalidae", "butterfly", "moth"},
}


def taxa_mode(a):
    """The list from taxa.json; the photos of the --cell choices made as WebP in <out>/photos/ (others: `missing`)."""
    import concurrent.futures
    import threading
    import urllib.request
    from io import BytesIO
    from PIL import Image, ImageOps

    src = json.load(open(a.taxa, encoding="utf-8"))
    if isinstance(src, dict):
        src = src.get("taxa", [])
    cells = []
    for c in a.cell or ["NT:all"]:
        region, _, scope = c.partition(":")
        assert scope in SCOPE_GROUPS, f"--cell REGION:SCOPE, scope one of {sorted(SCOPE_GROUPS)}: {c}"
        cells.append((None if region == "NT" else set(region.split("+")), SCOPE_GROUPS[scope]))

    def wanted(e):
        cs = set(e.get("countries") or [])
        return any(e["group"] in groups and cs and (region is None or region & cs) for region, groups in cells)

    photos_dir = os.path.join(a.out, "photos")
    os.makedirs(photos_dir, exist_ok=True)
    # Resumable: a photo already made (photos/<id>-<hash>.webp) is not fetched again.
    have = {}
    for f in os.listdir(photos_dir):
        if f.endswith(".webp"):
            have[f.split("-")[0]] = f
    ua = {"User-Agent": "ithomiini-photo-packs/1.0 (offline field guide; one request per photo)"}
    lock = threading.Lock()
    stats = {"made": 0, "kept": 0, "failed": 0}

    def make(pid, url):
        if pid in have:
            with lock:
                stats["kept"] += 1
            return have[pid]
        if os.path.exists(url):
            body = open(url, "rb").read()
        else:
            body = None
            for attempt in range(4):
                try:
                    with urllib.request.urlopen(urllib.request.Request(url, headers=ua), timeout=60) as r:
                        body = r.read()
                    break
                except Exception:  # noqa: BLE001 - retried, then counted as failed
                    time.sleep(3 * (attempt + 1))
            time.sleep(a.pause)
            if body is None and a.fallback:
                # The field set's own file of the photo (scripts/field-photos.py: <id>.jpg, s<id>.jpg for a subspecies).
                for stem in (pid, f"s{pid}"):
                    for ext in ("jpg", "jpeg", "png"):
                        local = os.path.join(a.fallback, f"{stem}.{ext}")
                        if body is None and os.path.exists(local):
                            body = open(local, "rb").read()
            if body is None:
                with lock:
                    stats["failed"] += 1
                return None
        try:
            im = ImageOps.exif_transpose(Image.open(BytesIO(body))).convert("RGB")
        except Exception:  # noqa: BLE001 - not an image
            with lock:
                stats["failed"] += 1
            return None
        im.thumbnail((a.size, a.size), Image.LANCZOS)
        buf = BytesIO()
        im.save(buf, "WEBP", quality=a.quality, method=6)
        webp = buf.getvalue()
        name = f"{pid}-{hashlib.sha256(webp).hexdigest()[:8]}.webp"
        path = os.path.join(photos_dir, name)
        with open(path + ".part", "wb") as f:
            f.write(webp)
        os.replace(path + ".part", path)
        with lock:
            stats["made"] += 1
            have[pid] = name
            done = stats["made"] + stats["kept"] + stats["failed"]
            if done % 250 == 0:
                print(f"  {done} photos ({stats})", flush=True)
        return name

    todo = {}
    for e in src:
        ph = e.get("photo")
        if ph and ph.get("url") and wanted(e):
            pid = str(ph.get("inatPhotoId") or hashlib.sha256(ph["url"].encode()).hexdigest()[:12])
            todo.setdefault(pid, ph["url"])
    if a.limit is not None:
        todo = dict(list(todo.items())[: a.limit])
    print(f"{len(todo)} photos to have ({len([p for p in todo if p in have])} already made)", flush=True)
    with concurrent.futures.ThreadPoolExecutor(max_workers=a.workers) as pool:
        made = dict(zip(todo, pool.map(lambda kv: make(*kv), todo.items())))

    out, by_group, unavailable = [], {}, []
    for e in src:
        parts = e["name"].split()
        ph = e.get("photo") or None
        entry = {"name": e["name"], "rank": e["rank"], "species": e.get("species") or " ".join(parts[:2]), "genus": e.get("genus") or parts[0],
                 "family": e.get("family") or "", "group": e["group"], "countries": sorted(set(e.get("countries") or [])), "photo": None, "bytes": 0}
        if ph and ph.get("url"):
            pid = str(ph.get("inatPhotoId") or hashlib.sha256(ph["url"].encode()).hexdigest()[:12])
            file = made.get(pid) or have.get(pid)
            if not file and pid in todo:
                # Inside the cells but not fetched (gone from iNaturalist, not an image): listed without a photo.
                out.append(entry)
                unavailable.append(e["name"])
                continue
            entry["photo"] = {"file": file or f"{pid}.webp", "attribution": ph.get("attribution") or "", "license": ph.get("license") or "",
                              "taxonUrl": ph.get("taxonUrl"), "source": ph.get("source")}
            if file:
                entry["bytes"] = os.path.getsize(os.path.join(photos_dir, file))
                by_group.setdefault(e["group"], []).append(entry["bytes"])
            else:
                # Not made on this server (outside the --cell choices, or not fetched): listed, with its size estimated
                # from the photos made of its group; the page says so and does not ask for it.
                entry["photo"]["missing"] = True
        out.append(entry)
    mean = {g: round(sum(v) / len(v)) for g, v in by_group.items() if v}
    fallback = round(sum(sum(v) for v in by_group.values()) / max(1, sum(len(v) for v in by_group.values())))
    for entry in out:
        if entry["photo"] and entry["photo"].get("missing"):
            entry["bytes"] = mean.get(entry["group"], fallback)
    print(f"photos: {stats}; mean bytes by group {mean}; listed without a photo (not fetched): {len(unavailable)} {unavailable[:12]}", flush=True)
    stale = sorted(set(os.listdir(photos_dir)) - {f for f in made.values() if f} - set(have.values()))
    for f in stale[:20]:
        print("  not named by the list:", os.path.join(photos_dir, f))
    write(a.out, out, "photos/", "iNaturalist photos under Creative Commons licences (taxon photos; 400 px WebP)", sample=a.sample)


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)
    f = sub.add_parser("field", help="the list from the Ithomiini field set")
    f.add_argument("--field", required=True)
    f.add_argument("--countries")
    f.add_argument("--out", default=os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "frontend", "public", "identificar", "packs"))
    tx = sub.add_parser("taxa", help="the list and its WebP photos from the per-taxon taxa.json")
    tx.add_argument("--taxa", required=True)
    tx.add_argument("--out", required=True)
    tx.add_argument("--cell", action="append", help="REGION:SCOPE to make photos of, REGION an ISO2 (EC, EC+CO) or NT (any Neotropical country)")
    tx.add_argument("--size", type=int, default=400)
    tx.add_argument("--quality", type=int, default=60)
    tx.add_argument("--workers", type=int, default=3)
    tx.add_argument("--pause", type=float, default=0.4, help="seconds each worker waits after a download")
    tx.add_argument("--fallback", help="a folder of photos by iNaturalist id (the field set) for URLs that no longer answer")
    tx.add_argument("--sample")
    tx.add_argument("--limit", type=int)
    a = ap.parse_args()
    if a.cmd == "taxa":
        taxa_mode(a)
    elif a.cmd == "field":
        from_field(a)
    else:
        sys.exit(f"unknown command {a.cmd}")


if __name__ == "__main__":
    main()
