#!/usr/bin/env python3
"""«Fotos para el campo» (Identificar tab): the static set beside the page, frontend/public/identificar/field/.

One CC-licensed photo per Ithomiini species of the identifier's head (kept from the 3 Nov 2026 set, built from the
server's iNaturalist taxon photos) and, new on 4 Nov 2026, one per subspecies leaf of the head where iNaturalist has
a CC-licensed photo of that subspecies: the taxon's own photo, else a research-grade (then any) observation of it,
else an iNaturalist photo GBIF files under the subspecies. Museum specimen images (GBIF) are left out: many first
images are labels or drawers and would need someone to look at each one.

Each entry says whether it is recorded in Ecuador (`ec`), for «solo Ecuador»:
  species     the head's geo flags (region_checklist.json: a leaf of the species recorded in Ecuador) or a Dore et al.
              (2022) / Sanger record in Ecuador (ithomiini_maps public/data/map_points_{dore,sanger}.json);
  subspecies  a Dore / Sanger record of that subspecies in Ecuador, or the checklist's own entry for the subspecies
              (not the species' entry the head falls back to, which would mark every subspecies of the species).
`bytes` is each file's size, so the page can say how much each choice takes before saving.

  python3 -I scripts/field-photos.py --head head_b8127dbf.json --checklist region_checklist.json \
      --records map_points_dore.json map_points_sanger.json [--out frontend/public/identificar/field] [--cache DIR]

Network: api.inaturalist.org (about 1 request a second), api.gbif.org, inaturalist-open-data (the photos, 'small',
240 px on the long side like the species set).
"""
import argparse
import json
import os
import re
import time
import urllib.parse
import urllib.request


def ithomiini_genera():
    """The app's one list of Ithomiini genera, read from frontend/src/lib/ithomiiniGenera.ts (not a copy)."""
    path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "frontend", "src", "lib", "ithomiiniGenera.ts")
    with open(path, encoding="utf-8") as f:
        body = re.search(r"ITHOMIINI_GENERA[^=]*=\s*Object\.freeze\(\[([^\]]*)\]", f.read()).group(1)
    names = set(re.findall(r"'([A-Za-z]+)'", body))
    assert len(names) >= 40, f"{path}: only {len(names)} genera read"
    return names


GENERA = ithomiini_genera()
UA = {"User-Agent": "ithomiini-field-photos/1.0 (Ikiam Ithomiini project)"}
CC = "cc0,cc-by,cc-by-nc,cc-by-sa,cc-by-nc-sa,cc-by-nd,cc-by-nc-nd"


def get(url, raw=False, pause=0.0):
    err = None
    for attempt in range(4):
        try:
            with urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=60) as r:
                body = r.read()
            if pause:
                time.sleep(pause)
            return body if raw else json.loads(body)
        except Exception as e:  # noqa: BLE001 - retried, then raised
            err = e
            time.sleep(5 * (attempt + 1))
    raise err


def cached(cache, key, fn):
    path = os.path.join(cache, key + ".json")
    if os.path.exists(path):
        return json.load(open(path))
    value = fn()
    json.dump(value, open(path, "w"))
    return value


def lic(code):
    return code.upper()


def small(url):
    # iNaturalist photo URLs: .../photos/<id>/<size>.<ext>
    head, _, tail = url.rpartition("/")
    return f"{head}/small.{tail.split('.')[-1]}"


def inat_taxa(cache, genera):
    """Every iNaturalist subspecies taxon under the genera: name -> {id, obs, photo}."""
    out = {}
    for g in sorted(genera):
        def fetch(g=g):
            r = get("https://api.inaturalist.org/v1/taxa?" + urllib.parse.urlencode({"q": g, "rank": "genus", "per_page": 10}), pause=1.1)
            gt = [t for t in r["results"] if t["name"] == g and t.get("iconic_taxon_name") == "Insecta"]
            if not gt:
                return {}
            got, page = {}, 1
            while True:
                q = {"taxon_id": gt[0]["id"], "rank": "subspecies", "per_page": 200, "page": page}
                r = get("https://api.inaturalist.org/v1/taxa?" + urllib.parse.urlencode(q), pause=1.1)
                for t in r["results"]:
                    dp = t.get("default_photo") or {}
                    got[t["name"]] = {"id": t["id"], "obs": t.get("observations_count", 0), "photo": dp or None}
                if page * 200 >= r["total_results"]:
                    return got
                page += 1
        out.update(cached(cache, f"inat-genus-{g}", fetch))
    return out


def inat_obs_photo(cache, taxon_id):
    def fetch():
        for grade in ("research", "needs_id"):
            q = {"taxon_id": taxon_id, "photo_license": CC, "quality_grade": grade, "per_page": 10, "order_by": "votes"}
            r = get("https://api.inaturalist.org/v1/observations?" + urllib.parse.urlencode(q), pause=1.1)
            for o in r["results"]:
                for p in o.get("photos", []):
                    if p.get("license_code") and p.get("url"):
                        return {"photo": p, "obs": o["id"], "grade": grade}
        return None
    return cached(cache, f"inat-obs-{taxon_id}", fetch)


def gbif_inat_photo(cache, name):
    """An iNaturalist photo GBIF files under the subspecies (through its own synonyms), or None."""
    def fetch():
        m = get("https://api.gbif.org/v1/species/match?" + urllib.parse.urlencode({"name": name, "rank": "SUBSPECIES", "strict": "true"}))
        # Only a subspecies GBIF accepts: under a synonym's accepted key the photo can be of another subspecies.
        if m.get("rank") != "SUBSPECIES" or m.get("matchType") == "NONE" or m.get("status") != "ACCEPTED":
            return None
        k = m["usageKey"]
        q = {"taxonKey": k, "mediaType": "StillImage", "datasetKey": "50c9509d-22c7-4a22-a47d-8c48425ef4a7", "limit": 20}
        o = get("https://api.gbif.org/v1/occurrence/search?" + urllib.parse.urlencode(q))
        for r in o["results"]:
            for md in r.get("media", []):
                url, license_ = md.get("identifier") or "", md.get("license") or ""
                if "inaturalist" in url and "creativecommons.org" in license_:
                    return {"url": url, "license": license_, "who": md.get("rightsHolder") or md.get("creator"), "occ": r.get("references")}
        return None
    return cached(cache, "gbif-" + name.replace(" ", "_"), fetch)


def gbif_license(url):
    # http://creativecommons.org/licenses/by-nc/4.0/ -> CC-BY-NC; publicdomain/zero -> CC0
    if "publicdomain/zero" in url:
        return "CC0"
    part = url.split("/licenses/")[1].split("/")[0]
    return "CC-" + part.upper()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--head", required=True)
    ap.add_argument("--checklist", required=True)
    ap.add_argument("--records", nargs="+", required=True)
    ap.add_argument("--out", default=os.path.join(os.path.dirname(__file__), "..", "frontend", "public", "identificar", "field"))
    ap.add_argument("--cache", required=True)
    a = ap.parse_args()
    os.makedirs(a.cache, exist_ok=True)
    head = json.load(open(a.head))
    chk = json.load(open(a.checklist))
    index_path = os.path.join(a.out, "index.json")
    index = json.load(open(index_path))

    leaves, geo, is_ssp = head["leaves"], head["geo"], head["is_ssp"]
    ec_species, ec_ssp = set(), set()
    for rec_file in a.records:
        for r in json.load(open(rec_file)):
            g, sp, ss = r.get("genus"), r.get("species"), r.get("subspecies")
            if g in GENERA and sp and r.get("country") == "Ecuador":
                ec_species.add(f"{g} {sp}")
                if ss:
                    ec_ssp.add(f"{g} {sp} {ss}")
    ssp_leaves = []
    for i, leaf in enumerate(leaves):
        if leaf.split()[0] not in GENERA:
            continue
        flags = int(geo[i], 16)
        if flags & 1 and flags & 2:
            ec_species.add(head["species"][head["leaf2sp"][i]])
        if is_ssp[i] == "1":
            ssp_leaves.append(leaf)
            if leaf in chk and chk[leaf].get("ec") == 1:
                ec_ssp.add(leaf)

    def fetch_file(url, stem):
        ext = url.rsplit(".", 1)[-1].lower().replace("jpeg", "jpg")
        name = f"{stem}.{ext}"
        path = os.path.join(a.out, name)
        if not os.path.exists(path):
            body = get(url, raw=True, pause=0.3)
            with open(path + ".part", "wb") as f:
                f.write(body)
            os.replace(path + ".part", path)
        return name

    for e in index["species"]:
        e["ec"] = e["name"] in ec_species
        e["bytes"] = os.path.getsize(os.path.join(a.out, e["file"]))

    taxa = inat_taxa(a.cache, GENERA)
    subspecies, gaps = [], {"no_photo": 0}
    for n in sorted(ssp_leaves):
        t = taxa.get(n)
        photo = attribution = license_ = source = None
        how = None
        if t and t["photo"] and t["photo"].get("license_code"):
            p = t["photo"]
            photo, attribution, license_, how = p.get("medium_url") or p["url"], " ".join(p["attribution"].split()), lic(p["license_code"]), "taxon"
            source = f"https://www.inaturalist.org/photos/{p['id']}"
        elif t and t["obs"] > 0:
            got = inat_obs_photo(a.cache, t["id"])
            if got:
                p = got["photo"]
                photo, attribution, license_, how = p["url"], " ".join(p["attribution"].split()), lic(p["license_code"]), f"observation ({got['grade']})"
                source = f"https://www.inaturalist.org/observations/{got['obs']}"
        if not photo:
            g = gbif_inat_photo(a.cache, n)
            if g:
                photo, license_, how = g["url"], gbif_license(g["license"]), "gbif"
                attribution = f"(c) {g['who']}, some rights reserved ({license_.replace('-', ' ', 1)})" if g.get("who") else license_
                source = g.get("occ")
        if not photo:
            gaps["no_photo"] += 1
            continue
        stem = "s" + photo.split("/photos/")[1].split("/")[0]
        file = fetch_file(small(photo), stem)
        subspecies.append({
            "name": n, "file": file, "attribution": attribution, "license": license_,
            "taxonUrl": f"https://www.inaturalist.org/taxa/{t['id']}" if t else None, "source": source,
            "from": how, "ec": n in ec_ssp, "bytes": os.path.getsize(os.path.join(a.out, file)),
        })
    head_species = {head["species"][head["leaf2sp"][i]] for i, lf in enumerate(leaves) if lf.split()[0] in GENERA}
    # A photo iNaturalist gives two subspecies (4 Nov 2026: Napeogenes inachia pyrois and N. pharo pheranthes) names
    # one of them wrongly: left out for both.
    shared = {f for f in (e["file"] for e in subspecies) if sum(x["file"] == f for x in subspecies) > 1}
    subspecies = [e for e in subspecies if e["file"] not in shared]
    for f in shared:
        os.remove(os.path.join(a.out, f))
    index["built"] = time.strftime("%Y-%m-%d")
    index["source"] = "iNaturalist photos under Creative Commons licences (species: taxon photos; subspecies: taxon or observation photos, some through GBIF)"
    index["counts"] = {
        "species": {"head": len(head_species), "photos": len(index["species"]), "ecuador": len(ec_species & head_species),
                    "ecuador_photos": sum(e["ec"] for e in index["species"])},
        "subspecies": {"head": len(ssp_leaves), "photos": len(subspecies), "ecuador": len(ec_ssp & set(ssp_leaves)),
                       "ecuador_photos": sum(e["ec"] for e in subspecies)},
    }
    index["subspecies"] = subspecies
    with open(index_path + ".part", "w") as f:
        json.dump(index, f, ensure_ascii=False, indent=1)
    os.replace(index_path + ".part", index_path)
    print(json.dumps(index["counts"]), "| subspecies from:", {k: sum(1 for s in subspecies if s["from"].startswith(k)) for k in ("taxon", "observation", "gbif")})


if __name__ == "__main__":
    main()
