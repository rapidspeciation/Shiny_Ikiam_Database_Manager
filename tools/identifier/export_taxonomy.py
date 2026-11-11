"""The identifier head's tribe and subfamily per species, for Identificar's tribe / subfamily answer.

Reads the head's own taxonomy (rank_vocab and leaf2rank of head_hier_6fa6ab78.pt, the names the head was trained on)
and writes tools/identifier/taxonomy_6fa6ab78.json, read by the server identifier (worker.py) and by the Identificar
page on the phone (frontend/src/lib/identifyResult.ts higherRanks):
  {"head": sha256, "tribes": [name, ...], "subfamilies": [name, ...], "species": {"Genus species": [tribe, subfamily]}}
with indexes into the two lists, -1 for none. Placeholder names (UNKNOWN_tribe_<genus>, UNKNOWN_subfamily_<genus>, any
UNKNOWN_*) and «incertae sedis» are not names to show: those species count as having no tribe (or subfamily), so a
placeholder group can never be the answer. Species with neither are left out.

The head was trained on a genus table that splits Ithomiini into 8 old tribes for some genera, files others under
"Ithomiini" with subfamily "Ithomiinae", puts the moth Macrosoma in Ithomiini and lacks Athyrtis, Epityches and
Leucothyris (WingsClassificator reports/model_autoresearch_20261109/taxonomic/TRIBE_FIX.md). genus_fix_ithomiini_20261110.json
overrides those genera with the lab taxonomy's values (the Ithomiini-only table Franz chose on 10 Nov): every Ithomiini
genus is tribe Ithomiini, subfamily Danainae; Macrosoma is Hedylinae with no tribe.
genus_fix_other_20261111.json leaves out tribe names that are wrong outside Ithomiini (Dismorphia «Pierini» and others)
and gives Libytheana Libytheinae.

  <identifier venv>/bin/python tools/identifier/export_taxonomy.py <dir>/app/assets/head_hier_6fa6ab78.pt [out.json]
  python3 tools/identifier/export_taxonomy.py --refix [taxonomy.json]   (applies the genus fix to an exported file)
"""

import hashlib
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
HIDDEN = ("incertae sedis",)
GENUS_FIX = [os.path.join(HERE, n) for n in ("genus_fix_ithomiini_20261110.json", "genus_fix_other_20261111.json")]


def shown(name):
    return bool(name) and not name.startswith("UNKNOWN") and name not in HIDDEN


def fixed(per_species, fix=None):
    """per_species {species: (tribe, subfamily)} with the genus fix applied; an empty value means none."""
    if fix is None:
        fix = {}
        for path in GENUS_FIX:
            with open(path) as f:
                fix.update(json.load(f)["genera"])
    out = {}
    for sp, pair in per_species.items():
        g = fix.get(sp.split(" ")[0])
        out[sp] = (g["tribe"], g["subfamily"]) if g else pair
    return out


def write(per_species, sha, out):
    """The file the server and the page read, from {species: (tribe, subfamily)}."""
    tribes = sorted({t for t, _ in per_species.values() if shown(t)})
    subfamilies = sorted({s for _, s in per_species.values() if shown(s)})
    ti, si = {n: k for k, n in enumerate(tribes)}, {n: k for k, n in enumerate(subfamilies)}
    species = {}
    for sp in sorted(per_species):
        t, s = per_species[sp]
        pair = [ti.get(t, -1), si.get(s, -1)]
        if pair != [-1, -1]:
            species[sp] = pair
    doc = {"head": sha, "tribes": tribes, "subfamilies": subfamilies, "species": species}
    with open(out, "w") as f:
        json.dump(doc, f, separators=(",", ":"), ensure_ascii=False)
        f.write("\n")
    return doc


def refix(path):
    """Applies the genus fix to an exported file (its names, read back), keeping its head."""
    with open(path) as f:
        doc = json.load(f)
    name = lambda names, k: names[k] if k >= 0 else ""
    per_species = {sp: (name(doc["tribes"], t), name(doc["subfamilies"], s)) for sp, (t, s) in doc["species"].items()}
    new = write(fixed(per_species), doc["head"], path)
    print(json.dumps({"species": len(new["species"]), "tribes": len(new["tribes"]), "subfamilies": len(new["subfamilies"]), "out": path}))


def main():
    if sys.argv[1] == "--refix":
        return refix(sys.argv[2] if len(sys.argv) > 2 else os.path.join(HERE, "taxonomy_6fa6ab78.json"))
    import torch

    src = sys.argv[1]
    out = sys.argv[2] if len(sys.argv) > 2 else os.path.join(HERE, "taxonomy_6fa6ab78.json")
    sha = hashlib.sha256(open(src, "rb").read()).hexdigest()
    ck = torch.load(src, map_location="cpu", weights_only=False)
    voc, l2r = ck["rank_vocab"], ck["leaf2rank"]
    raw = {}
    for i in range(len(ck["leaves"])):
        sp = voc["species"][l2r["species"][i]]
        t, s = voc["tribe"][l2r["tribe"][i]], voc["subfamily"][l2r["subfamily"][i]]
        prev = raw.setdefault(sp, (t, s))
        if prev != (t, s):
            raise SystemExit(f"{sp}: leaves disagree on tribe / subfamily ({prev} vs {(t, s)})")
    per_species = fixed(raw)
    write(per_species, sha, out)
    species = json.load(open(out))["species"]
    tribes, subfamilies = sorted({t for t, _ in per_species.values() if shown(t)}), sorted({s for _, s in per_species.values() if shown(s)})
    hidden_t = sum(1 for t, _ in per_species.values() if not shown(t))
    hidden_s = sum(1 for _, s in per_species.values() if not shown(s))
    print(json.dumps({
        "leaves": len(ck["leaves"]), "species": len(per_species),
        "species_with_tribe": sum(1 for p in species.values() if p[0] >= 0),
        "species_with_subfamily": sum(1 for p in species.values() if p[1] >= 0),
        "leaves_with_tribe": sum(1 for i in range(len(ck["leaves"])) if shown(voc["tribe"][l2r["tribe"][i]])),
        "leaves_with_subfamily": sum(1 for i in range(len(ck["leaves"])) if shown(voc["subfamily"][l2r["subfamily"][i]])),
        "tribes": len(tribes), "subfamilies": len(subfamilies),
        "species_tribe_hidden": hidden_t, "species_subfamily_hidden": hidden_s,
        "placeholder_tribe_names": sum(1 for n in voc["tribe"] if not shown(n)),
        "placeholder_subfamily_names": sum(1 for n in voc["subfamily"] if not shown(n)),
        "bytes": os.path.getsize(out), "out": out,
    }))


if __name__ == "__main__":
    main()
