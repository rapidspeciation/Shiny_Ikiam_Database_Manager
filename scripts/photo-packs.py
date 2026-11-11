#!/usr/bin/env python3
"""Photo packs (Identificar tab, Descargas): the list identificar/packs/index.json the page chooses packs from
(frontend/src/lib/photoPacks.ts). Schema 2: one entry per taxon: name, rank, species, genus, family, group
('ithomiini' | 'nymphalidae' | 'butterfly' | 'moth'), countries (ISO2, where it is recorded) and photos, its best
first, each {file, attribution, license, taxonUrl, source, bytes, country (ISO2 where it was taken, or null), kind
('live' or 'preserved'), specimen (a preserved specimen's ID)}. The page keeps up to 2 photos of a taxon in a pack
(PER_TAXON): the region's first, then one from a neighbouring country or else from elsewhere (lib/photoCountry.ts
choosePhotos); with signal it shows them all.
`version` is a hash of the entries: a phone whose pack has another version is offered an update. Each entry also keeps
schema 1's `photo` and `bytes` (its first photo), so pages still running the first schema's code read the same list
(write(), legacy()).

Country photos (--obs ISO2=<file>): the observations of a country that photo-packs-obs.py saved (research grade,
reusable licence). For each taxon (its iNaturalist id, same rank) the best one (most faves, then the newest) gives a
photo from that country; a taxon photo that is also an observation photo there takes that country.

Preserved specimens (--preserved <manifest.json>): {"schema": "ithomiini-preserved/1", "photos": [{"name", "file"
(an image, relative to the manifest), "specimen" (its ID), "country" (or null), "attribution", "license", "source"
(or null)}]}, or the Sanger wing crops' manifest ({"items": [{"taxon", "specimen_id", "file", "poor", ...}]}: entries
flagged poor are left out; credit "Ikiam / Wellcome Sanger Institute", shown as «Foto: …», no licence line). Each becomes a photo of kind 'preserved' of a taxon
that has no live photo (with --preserved-all, of every taxon named). A WebP no larger than --size is kept byte for
byte (the crops are made at the pack's settings already); anything else is made like the other photos.

Bundles (taxa mode): the photos made are also written as a few files of about --bundle-mb MB each
(bundles/b<NN>-<sha256, 10 hex>.bin, the WebP files back to back, uncompressed: WebP gains nothing from gzip), and
each photo in the list gets "at": [bundle number, byte offset]; the list names the files in "bundles" [{file, bytes}].
A phone then saves a pack with a few tens of range requests instead of one request per photo
(frontend/src/lib/photoPacks.ts). Order inside: the photos an Ecuador pack keeps first, then those a Neotropics pack
keeps, then the rest; inside each by group (Ithomiini, other Nymphalidae, other butterflies, moths), species before
subspecies, by name; so the common choices are a few long runs. A bundle's name changes with its bytes (served as immutable). The single photos stay in photos/ for the
online view of one taxon.

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
        assert isinstance(t["photos"], list), t
        for ph in t["photos"]:
            for k in ("file", "attribution", "license", "taxonUrl", "source", "bytes"):
                assert k in ph, (k, t)
            assert ph["bytes"] > 0 or ph.get("missing"), t
            assert ph.get("country") is None or (len(ph["country"]) == 2 and ph["country"].isupper()), t
            assert ph.get("kind", "live") in ("live", "preserved"), t
            assert ph.get("kind") != "preserved" or ph.get("specimen"), t


LEGACY = ("file", "attribution", "license", "taxonUrl", "source")


def legacy(t):
    """Schema 1's fields beside schema 2's (`photo` and `bytes`: the taxon's first photo), so a page still running the
    first schema's code (release 1, 642d9251, open in a tab or offline on a phone) reads the new list as before: its
    installer takes `photo`, which is in photos/, and never sees an empty list (it would record a pack with no photos
    and delete the photos it had: review r4-sol-F1)."""
    first = t["photos"][0] if t["photos"] else None
    t["photo"] = {**{k: first.get(k) for k in LEGACY}, **({"missing": True} if first.get("missing") else {})} if first else None
    t["bytes"] = first["bytes"] if first else 0
    return t


def write(out, taxa, base, source, sample=None, bundles=None):
    taxa = [legacy(t) for t in taxa]
    check(taxa)
    taxa = sorted(taxa, key=lambda t: t["name"])
    index = {"schema": "ithomiini-photo-packs/2", "version": version_of(taxa), "built": time.strftime("%Y-%m-%d"), "base": base, "source": source}
    if sample:
        index["sample"] = sample
    if bundles:
        index["bundles"] = bundles
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
    made = {ph["file"]: ph for t in taxa for ph in t["photos"] if not ph.get("missing")}
    with_photo = [t for t in taxa if any(not ph.get("missing") for ph in t["photos"])]
    assert all((t["photo"] or {}).get("file") == (t["photos"][0]["file"] if t["photos"] else None) for t in taxa)
    by_group = {g: sum(1 for t in with_photo if t["group"] == g) for g in GROUPS}
    known = sum(1 for ph in made.values() if ph.get("country"))
    print(f"{path}: {len(taxa)} taxa, {len(with_photo)} with a photo, {len(made)} photo files ({known} with their country), {sum(ph['bytes'] for ph in made.values()) / 1048576:.1f} MB; by group {by_group}; version {index['version']}; index {os.path.getsize(path) / 1024:.0f} KB")


LICENCES = {"cc0", "cc-by", "cc-by-nc", "cc-by-sa", "cc-by-nc-sa", "cc-by-nd", "cc-by-nc-nd"}


def read_obs(specs):
    """--obs ISO2=<obs-ISO2.jsonl.gz>: (best[(iNaturalist taxon id, ISO2)] = photo, country of a photo id)."""
    best, country_of = {}, {}
    for spec in specs or []:
        iso, _, path = spec.partition("=")
        assert len(iso) == 2 and iso.isupper() and path, f"--obs ISO2=<file>: {spec}"
        rows = {}
        with gzip.open(path, "rt", encoding="utf-8") as f:
            for line in f:
                try:
                    o = json.loads(line)
                except ValueError:
                    continue  # a line cut short by a stop
                rows[o["id"]] = o  # a page asked twice after a stop: once
        for o in rows.values():
            ph = next((p for p in o.get("photos") or [] if p.get("url") and (p.get("license") or "").lower() in LICENCES), None)
            if not ph:
                continue
            country_of.setdefault(ph["id"], iso)
            key = (o["taxon"]["id"], iso)
            rank = (o.get("faves") or 0, o["id"])
            if key not in best or rank > best[key][0]:
                best[key] = (rank, {
                    "pid": str(ph["id"]),
                    "url": ph["url"].replace("/square.", "/medium."),
                    "attribution": ph.get("attribution") or "",
                    "license": ph["license"].upper(),
                    "source": f"https://www.inaturalist.org/photos/{ph['id']}",
                    "country": iso,
                })
        print(f"{iso}: {len(rows)} observations, {sum(1 for k in best if k[1] == iso)} taxa with a photo", flush=True)
    return {k: v[1] for k, v in best.items()}, country_of


def read_preserved(path):
    """The preserved specimens' manifest: name -> its photos (file made absolute)."""
    if not path:
        return {}
    doc = json.load(open(path, encoding="utf-8"))
    base = os.path.dirname(os.path.abspath(path))
    out = {}
    if doc.get("schema") != "ithomiini-preserved/1" and isinstance(doc.get("items"), list):
        # The Sanger wing crops (offline-packs research, sanger-crops/crops-*/manifest.json).
        doc = {"photos": [{"name": e["taxon"], "file": e["file"], "specimen": e["specimen_id"], "country": e.get("country"),
                           "attribution": "Ikiam / Wellcome Sanger Institute", "license": "", "source": None}
                          for e in doc["items"] if not e.get("poor") and e.get("taxon") and e.get("file") and e.get("specimen_id")]}
    else:
        assert doc.get("schema") == "ithomiini-preserved/1", f"{path}: schema ithomiini-preserved/1 or a crops manifest with items"
    for e in doc.get("photos") or []:
        assert e.get("name") and e.get("file") and e.get("specimen"), e
        file = os.path.join(base, e["file"])
        out.setdefault(e["name"], []).append({
            "pid": "p" + hashlib.sha256(f"{e['specimen']}|{e['file']}".encode()).hexdigest()[:12],
            "url": file,
            "attribution": e.get("attribution") or "",
            "license": e.get("license") or "",
            "source": e.get("source"),
            "country": e.get("country"),
            "kind": "preserved",
            "specimen": str(e["specimen"]),
        })
    return out


def from_field(a):
    field = json.load(open(a.field, encoding="utf-8"))
    countries = {}
    if a.countries:
        for e in json.load(open(a.countries, encoding="utf-8")):
            countries[e["name"]] = sorted(set(e.get("appCountries") or []))
    _, country_of = read_obs(a.obs)
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
                "photos": [{"file": e["file"], "attribution": e["attribution"], "license": e["license"], "taxonUrl": e.get("taxonUrl"), "source": e.get("source"),
                            "bytes": e["bytes"], "country": country_of.get(photo_id(e.get("source")))}],
            })
    rel = os.path.relpath(os.path.dirname(os.path.abspath(a.field)), os.path.abspath(a.out)).replace(os.sep, "/") + "/"
    # Only Ithomiini have photos in this list: the page says so beside the other groups.
    write(a.out, taxa, rel, field.get("source") or "iNaturalist photos under Creative Commons licences", sample="ithomiini-only")


def photo_id(source):
    """The iNaturalist photo id of a photo page URL (…/photos/<id>), or None."""
    tail = (source or "").rstrip("/").rsplit("/", 1)[-1]
    return int(tail) if tail.isdigit() else None


NEAR_EC = {"CO", "PE"}


def chosen_for_ec(photos, k=2):
    """The photos an Ecuador pack keeps of a taxon (frontend/src/lib/photoCountry.ts choosePhotos with region EC)."""
    rank = lambda p: 0 if p.get("country") == "EC" else 1 if p.get("country") in NEAR_EC else 2 if p.get("country") else 3
    tier = lambda p: min(rank(p), 2)
    firsts = [next(p for p in photos if tier(p) == t) for t in (0, 1, 2) if any(tier(p) == t for p in photos)]
    rest = sorted((p for p in photos if p not in firsts), key=rank)
    return (firsts + rest)[:k]


def make_bundles(out_dir, taxa, photos_dir, bundle_mb):
    """The made photos back to back in bundles/ (about bundle_mb MB each); each photo gets "at": [bundle, offset]."""
    order = {}
    for t in taxa:
        made = [p for p in t["photos"] if not p.get("missing")]
        ec = {p["file"] for p in chosen_for_ec(made)} if "EC" in t["countries"] else set()
        # A Neotropics pack keeps the list's first two (the taxon photo, then Ecuador's).
        nt = {p["file"] for p in made[:2]} if t["countries"] else set()
        for i, p in enumerate(made):
            key = (0 if p["file"] in ec else 1, 0 if p["file"] in nt else 1, GROUPS.index(t["group"]), 0 if t["rank"] == "species" else 1, t["name"], i)
            order[p["file"]] = min(order.get(p["file"], key), key)
    files = sorted(order, key=order.get)
    bdir = os.path.join(out_dir, "bundles")
    os.makedirs(bdir, exist_ok=True)
    bundles, at, chunk, offset = [], {}, [], 0

    def flush():
        nonlocal chunk, offset
        if not chunk:
            return
        body = b"".join(chunk)
        name = f"b{len(bundles):03d}-{hashlib.sha256(body).hexdigest()[:10]}.bin"
        path = os.path.join(bdir, name)
        if not os.path.exists(path):
            with open(path + ".part", "wb") as fh:
                fh.write(body)
            os.replace(path + ".part", path)
        bundles.append({"file": f"bundles/{name}", "bytes": len(body)})
        chunk, offset = [], 0

    limit = int(bundle_mb * 1048576)
    for f in files:
        body = open(os.path.join(photos_dir, f), "rb").read()
        if offset and offset + len(body) > limit:
            flush()
        at[f] = [len(bundles), offset]
        chunk.append(body)
        offset += len(body)
    flush()
    for t in taxa:
        for p in t["photos"]:
            if p["file"] in at:
                p["at"] = at[p["file"]]
    names = {b["file"].split("/")[-1] for b in bundles}
    for f in sorted(set(os.listdir(bdir)) - names):
        print("  bundle not named by the list:", os.path.join(bdir, f))
    print(f"bundles: {len(bundles)} files, {sum(b['bytes'] for b in bundles) / 1048576:.1f} MB, {len(files)} photos", flush=True)
    return bundles


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
            src_im = Image.open(BytesIO(body))
            im = ImageOps.exif_transpose(src_im).convert("RGB")
        except Exception:  # noqa: BLE001 - not an image
            with lock:
                stats["failed"] += 1
            return None
        if src_im.format == "WEBP" and max(src_im.size) <= a.size:
            webp = body  # made at these settings already (the Sanger crops): not encoded twice
        else:
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

    best, country_of = read_obs(a.obs)
    preserved = read_preserved(a.preserved)

    def listed(e):
        """The taxon's photos to list, best first: its taxon photo, then one from each --obs country, then preserved."""
        out = []
        ph = e.get("photo") or None
        if ph and ph.get("url"):
            pid = str(ph.get("inatPhotoId") or hashlib.sha256(ph["url"].encode()).hexdigest()[:12])
            out.append({"pid": pid, "url": ph["url"], "attribution": ph.get("attribution") or "", "license": ph.get("license") or "",
                        "source": ph.get("source"), "taxonUrl": ph.get("taxonUrl"), "country": country_of.get(ph.get("inatPhotoId"))})
        tid = e.get("inatId")
        taxon_url = (ph or {}).get("taxonUrl") or (f"https://www.inaturalist.org/taxa/{tid}" if tid else None)
        for spec in a.obs or []:
            iso = spec.partition("=")[0]
            o = best.get((tid, iso)) if tid else None
            if o and all(o["pid"] != x["pid"] for x in out):
                out.append({**o, "taxonUrl": taxon_url})
        if a.preserved_all or not out:
            out += [{**x, "taxonUrl": taxon_url} for x in preserved.get(e["name"], [])]
        return out

    lists = {e["name"]: listed(e) for e in src}
    todo = {}
    for e in src:
        if wanted(e):
            for x in lists[e["name"]]:
                todo.setdefault(x["pid"], x["url"])
    if a.limit is not None:
        todo = dict(list(todo.items())[: a.limit])
    print(f"{len(todo)} photos to have ({len([p for p in todo if p in have])} already made)", flush=True)
    with concurrent.futures.ThreadPoolExecutor(max_workers=a.workers) as pool:
        made = dict(zip(todo, pool.map(lambda kv: make(*kv), todo.items())))

    out, by_group, unavailable = [], {}, []
    for e in src:
        parts = e["name"].split()
        entry = {"name": e["name"], "rank": e["rank"], "species": e.get("species") or " ".join(parts[:2]), "genus": e.get("genus") or parts[0],
                 "family": e.get("family") or "", "group": e["group"], "countries": sorted(set(e.get("countries") or [])), "photos": []}
        for x in lists[e["name"]]:
            file = made.get(x["pid"]) or have.get(x["pid"])
            if not file and x["pid"] in todo:
                # Inside the cells but not fetched (gone from iNaturalist, not an image): not listed.
                unavailable.append(e["name"])
                continue
            photo = {"file": file or f"{x['pid']}.webp", "attribution": x["attribution"], "license": x["license"], "taxonUrl": x.get("taxonUrl"),
                     "source": x.get("source"), "bytes": 0, "country": x.get("country")}
            if x.get("kind") == "preserved":
                photo["kind"] = "preserved"
                photo["specimen"] = x["specimen"]
            if file:
                photo["bytes"] = os.path.getsize(os.path.join(photos_dir, file))
                by_group.setdefault(e["group"], []).append(photo["bytes"])
            else:
                # Not made on this server (outside the --cell choices): listed, with its size estimated from the photos
                # made of its group; the page says so and does not ask for it.
                photo["missing"] = True
            entry["photos"].append(photo)
        out.append(entry)
    mean = {g: round(sum(v) / len(v)) for g, v in by_group.items() if v}
    fallback = round(sum(sum(v) for v in by_group.values()) / max(1, sum(len(v) for v in by_group.values())))
    for entry in out:
        for photo in entry["photos"]:
            if photo.get("missing"):
                photo["bytes"] = mean.get(entry["group"], fallback)
    print(f"photos: {stats}; mean bytes by group {mean}; photos not fetched: {len(unavailable)} {unavailable[:12]}", flush=True)
    stale = sorted(set(os.listdir(photos_dir)) - {f for f in made.values() if f} - set(have.values()))
    for f in stale[:20]:
        print("  not named by the list:", os.path.join(photos_dir, f))
    bundles = make_bundles(a.out, out, photos_dir, a.bundle_mb) if a.bundle_mb > 0 else None
    write(a.out, out, "photos/", "iNaturalist photos under Creative Commons licences (taxon photos and research-grade observations by country; 400 px WebP)", sample=a.sample, bundles=bundles)


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)
    f = sub.add_parser("field", help="the list from the Ithomiini field set")
    f.add_argument("--field", required=True)
    f.add_argument("--countries")
    f.add_argument("--obs", action="append", help="ISO2=<obs file>: the country of field photos that are observation photos there")
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
    tx.add_argument("--obs", action="append", help="ISO2=<obs-ISO2.jsonl.gz> (photo-packs-obs.py): a photo from that country per taxon")
    tx.add_argument("--preserved", help="the preserved specimens' manifest (ithomiini-preserved/1)")
    tx.add_argument("--preserved-all", action="store_true", help="a preserved photo also for taxa with a live photo")
    tx.add_argument("--bundle-mb", type=float, default=8, help="bundle size in MB (0: no bundles)")
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
