#!/usr/bin/env python3
"""For scripts/photo-packs.py --obs: every research-grade Lepidoptera observation of a country on iNaturalist with a
reusable photo licence, paged by id (resumable): one line per observation in cache/obs-<ISO2>.jsonl.gz {id, faves, taxon {id, name, rank, anc}, photos
[{id, url, license, attribution}]}. About one request a second (iNaturalist's guidance), 200 observations a request.

  python3 -I scripts/photo-packs-obs.py --country EC:7512 --country CO:7196 --country PE:7513 --cache cache

ISO2:place_id, the place ids from /v1/places/autocomplete (admin_level 0). No AI calls.
"""
import argparse
import gzip
import json
import os
import time
import urllib.request

API = "https://api.inaturalist.org/v2/observations"
LIC = "cc0,cc-by,cc-by-nc,cc-by-sa,cc-by-nc-sa,cc-by-nd,cc-by-nc-nd"
FIELDS = "(id:!t,faves_count:!t,taxon:(id:!t,name:!t,rank:!t,ancestor_ids:!t),photos:(id:!t,url:!t,license_code:!t,attribution:!t))"
UA = {"User-Agent": "ithomiini-photo-packs/1.0 (offline field guide; paged, one request a second)", "Accept": "application/json"}


def last_id(path):
    top = 0
    if os.path.exists(path):
        with gzip.open(path, "rt", encoding="utf-8") as f:
            for line in f:
                try:
                    top = max(top, json.loads(line)["id"])
                except Exception:  # noqa: BLE001 - a line cut short by a stop: the page is asked again
                    pass
    return top


def get(url):
    for attempt in range(6):
        try:
            with urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=60) as r:
                return json.load(r)
        except Exception as e:  # noqa: BLE001 - retried with a growing wait
            print(f"  retry {attempt + 1}: {e}", flush=True)
            time.sleep(10 * (attempt + 1))
    raise SystemExit(f"gave up on {url}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--country", action="append", required=True, help="ISO2:iNaturalist place id")
    ap.add_argument("--cache", required=True)
    ap.add_argument("--taxon", default="47157", help="Lepidoptera")
    a = ap.parse_args()
    os.makedirs(a.cache, exist_ok=True)
    for c in a.country:
        iso, _, place = c.partition(":")
        path = os.path.join(a.cache, f"obs-{iso}.jsonl.gz")
        above = last_id(path)
        n = 0
        print(f"{iso}: from id {above}", flush=True)
        while True:
            url = f"{API}?place_id={place}&taxon_id={a.taxon}&quality_grade=research&photo_license={LIC}&per_page=200&order_by=id&order=asc&id_above={above}&fields={FIELDS}"
            t0 = time.time()
            res = get(url).get("results") or []
            if not res:
                break
            with gzip.open(path, "at", encoding="utf-8") as f:
                for o in res:
                    tx = o.get("taxon") or {}
                    rec = {"id": o["id"], "faves": o.get("faves_count") or 0,
                           "taxon": {"id": tx.get("id"), "name": tx.get("name"), "rank": tx.get("rank"), "anc": tx.get("ancestor_ids") or []},
                           "photos": [{"id": p.get("id"), "url": p.get("url"), "license": p.get("license_code"), "attribution": p.get("attribution")} for p in o.get("photos") or []]}
                    f.write(json.dumps(rec, separators=(",", ":")) + "\n")
            above = res[-1]["id"]
            n += len(res)
            if n % 4000 < 200:
                print(f"  {iso}: {n} observations, last id {above}", flush=True)
            time.sleep(max(0.0, 1.05 - (time.time() - t0)))
        print(f"{iso}: done, {n} new", flush=True)


if __name__ == "__main__":
    main()
