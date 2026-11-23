"""Build leaf_merges_6fa6ab78.json from the approved 48-pair labels_q/leaf_map.tsv.

Option 1A display-time taxonomic name merge on production head_hier_6fa6ab78.pt:
the probabilities of merged source leaves are summed into their corrected target leaf
before geographic-prior weighting and species/genus/tribe/subfamily roll-up.

Only the approved 48-pair labels_q table is used (Greta diaphanus quisqueya stays its
own leaf, as decided 18 Nov 2026; Mechanitis polymnia proceriformis and eurydice stay
unchanged). Output is deterministic (no timestamp).

Usage:
  python3 -I tools/identifier/build_leaf_merges.py <path/to/labels_q/leaf_map.tsv> [out.json]
"""

import csv
import hashlib
import json
import os
import sys

HEAD_SHA256 = "6fa6ab78c6c3b76566a15d322061d2a5bec95c7dd705797c75c95799e1603552"
EXPECTED_SOURCE_SHA256 = "05e847e69d187106955955cef615efd83aefe07a441014024f59bea3fca43603"
EXPECTED_TOTAL_ROWS = 15347
EXPECTED_MERGE_COUNT = 48
KEPT = [
    "Greta diaphanus quisqueya",
    "Mechanitis polymnia eurydice",
    "Mechanitis polymnia proceriformis",
]


def sha256_file(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def build_doc(leaf_map_path):
    src_sha = sha256_file(leaf_map_path)
    if src_sha != EXPECTED_SOURCE_SHA256:
        raise SystemExit(f"Unexpected leaf_map.tsv SHA-256: {src_sha} (expected {EXPECTED_SOURCE_SHA256})")

    with open(leaf_map_path, newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f, delimiter="\t"))
    if len(rows) != EXPECTED_TOTAL_ROWS:
        raise SystemExit(f"Expected {EXPECTED_TOTAL_ROWS} rows, got {len(rows)}")

    merges = {}
    unchanged = set()
    for r in rows:
        old_leaf = r["old_leaf"].strip()
        new_leaf = r["new_leaf"].strip()
        changed = r["changed"].strip()
        if changed == "yes":
            if not old_leaf or not new_leaf or old_leaf == new_leaf:
                raise SystemExit(f"Invalid changed row: {r}")
            if old_leaf in merges:
                raise SystemExit(f"Duplicate old_leaf: {old_leaf}")
            merges[old_leaf] = new_leaf
        elif changed == "no":
            if old_leaf != new_leaf:
                raise SystemExit(f"Unchanged row has different names: {r}")
            unchanged.add(old_leaf)
        else:
            raise SystemExit(f"Unexpected changed flag: {r}")

    if len(merges) != EXPECTED_MERGE_COUNT:
        raise SystemExit(f"Expected {EXPECTED_MERGE_COUNT} merges, got {len(merges)}")

    for kept_name in KEPT:
        if kept_name in merges or kept_name not in unchanged:
            raise SystemExit(f"Kept leaf {kept_name!r} must stay unmerged")

    for old_leaf, new_leaf in merges.items():
        if new_leaf in merges:
            raise SystemExit(f"Merge chain detected: {old_leaf} -> {new_leaf} -> {merges[new_leaf]}")

    return {
        "schema": "ithomiini-leaf-merges/1",
        "head": HEAD_SHA256,
        "source": "WingsClassificator/reports/head_retrain_names_20261115/labels_q/leaf_map.tsv",
        "source_sha256": src_sha,
        "count": len(merges),
        "kept": list(KEPT),
        "merges": dict(sorted(merges.items())),
    }


def main(argv):
    if len(argv) < 2:
        raise SystemExit(__doc__)
    src = argv[1]
    out = argv[2] if len(argv) > 2 else os.path.join(os.path.dirname(os.path.abspath(__file__)), "leaf_merges_6fa6ab78.json")
    doc = build_doc(src)
    text = json.dumps(doc, indent=1, ensure_ascii=False) + "\n"
    with open(out, "w", encoding="utf-8", newline="\n") as f:
        f.write(text)
    out_sha = hashlib.sha256(text.encode("utf-8")).hexdigest()
    print(f"{out} count={doc['count']} sha256={out_sha}")


if __name__ == "__main__":
    main(sys.argv)
