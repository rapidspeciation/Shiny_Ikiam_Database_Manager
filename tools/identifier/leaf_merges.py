"""Display-time leaf probability merging (Option 1A) for hierarchical CosineHead outputs.

Maps raw head leaves (e.g. 15,347 leaves of head_hier_6fa6ab78.pt) onto display leaves
(15,300 after the 48 approved labels_q merges) by summing raw softmax probabilities of
merged leaves before geographic-prior weighting and species/genus/tribe/subfamily roll-up.

Pure numpy + stdlib helper so worker.py and unit/parity tests share identical semantics.
"""

import json
import os

SCHEMA = "ithomiini-leaf-merges/1"


def load_leaf_merges(path, head_sha=None, required=False):
    """Load and validate an ithomiini-leaf-merges/1 file.

    When `required=True`, raises FileNotFoundError if absent and ValueError on head mismatch.
    """
    if not path or not os.path.exists(path):
        if required:
            raise FileNotFoundError(f"Required leaf merges file not found: {path}")
        return {}
    with open(path, encoding="utf-8") as f:
        doc = json.load(f)
    if doc.get("schema") != SCHEMA:
        raise ValueError(f"{path}: unexpected schema {doc.get('schema')!r}")
    if head_sha is not None and doc.get("head") != head_sha:
        if required:
            raise ValueError(f"{path}: head mismatch {doc.get('head')!r} != {head_sha!r}")
        return {}
    merges = doc.get("merges")
    if not isinstance(merges, dict):
        raise ValueError(f"{path}: merges must be an object")
    clean = {}
    for old_leaf, new_leaf in merges.items():
        if not isinstance(old_leaf, str) or not isinstance(new_leaf, str) or not old_leaf or not new_leaf or old_leaf == new_leaf:
            raise ValueError(f"{path}: invalid merge entry {old_leaf!r} -> {new_leaf!r}")
        clean[old_leaf] = new_leaf
    for old_leaf, new_leaf in clean.items():
        if new_leaf in clean:
            raise ValueError(f"{path}: merge chain {old_leaf!r} -> {new_leaf!r} -> {clean[new_leaf]!r}")
    expected = doc.get("count")
    if expected is not None and len(clean) != int(expected):
        raise ValueError(f"{path}: expected {expected} merges, found {len(clean)}")
    return clean


def _species_of(leaf):
    parts = leaf.split()
    return " ".join(parts[:2]) if len(parts) >= 2 else leaf


def _genus_of(leaf):
    return leaf.split()[0]


def build_display_space(raw_leaves, raw_sp_vocab, raw_ge_vocab, raw_leaf2sp, raw_leaf2ge, merges, np, strict=False):
    """Build the merged display leaf/species/genus tables and old_to_new index map.

    If `merges` is empty (or no raw leaf matches when `strict=False`), returns an identity projection
    over the raw vocab. When `strict=True`, every merge source key must exist in `raw_leaves`.
    When merges apply, emptied species/genera are dropped from the display vocabularies so old
    synonym/misspelled species names never appear in rankings.
    """
    raw_leaves = list(raw_leaves)
    raw_sp_vocab = list(raw_sp_vocab)
    raw_ge_vocab = list(raw_ge_vocab)
    n_raw = len(raw_leaves)

    if not merges:
        return {
            "old_to_new": None,
            "leaves": raw_leaves,
            "sp_vocab": raw_sp_vocab,
            "ge_vocab": raw_ge_vocab,
            "leaf2sp": np.asarray(raw_leaf2sp, dtype=np.int64),
            "leaf2ge": np.asarray(raw_leaf2ge, dtype=np.int64),
            "is_ssp": np.array([lf.count(" ") >= 2 for lf in raw_leaves], dtype=bool),
        }

    raw_index = {lf: i for i, lf in enumerate(raw_leaves)}
    if strict:
        missing = [k for k in merges if k not in raw_index]
        if missing:
            raise ValueError(f"Merged source leaves missing from head vocabulary ({len(missing)}): {missing[:5]!r}")
    elif not any(lf in merges for lf in raw_leaves):
        return {
            "old_to_new": None,
            "leaves": raw_leaves,
            "sp_vocab": raw_sp_vocab,
            "ge_vocab": raw_ge_vocab,
            "leaf2sp": np.asarray(raw_leaf2sp, dtype=np.int64),
            "leaf2ge": np.asarray(raw_leaf2ge, dtype=np.int64),
            "is_ssp": np.array([lf.count(" ") >= 2 for lf in raw_leaves], dtype=bool),
        }

    raw_sp_set = set(raw_sp_vocab)
    raw_ge_set = set(raw_ge_vocab)

    old_to_new = np.empty(n_raw, dtype=np.int64)
    display_leaves = []
    display_index = {}
    display_sp_names = []
    display_ge_names = []

    for i, lf in enumerate(raw_leaves):
        target = merges.get(lf, lf)
        j = display_index.get(target)
        if j is None:
            j = len(display_leaves)
            display_index[target] = j
            display_leaves.append(target)
            orig = raw_index.get(target)
            if orig is not None:
                sp = raw_sp_vocab[int(raw_leaf2sp[orig])]
                ge = raw_ge_vocab[int(raw_leaf2ge[orig])]
            else:
                sp = _species_of(target)
                ge = _genus_of(target)
                if sp not in raw_sp_set:
                    raise ValueError(f"Merged target {target!r}: species {sp!r} not in head species vocabulary")
                if ge not in raw_ge_set:
                    raise ValueError(f"Merged target {target!r}: genus {ge!r} not in head genus vocabulary")
            display_sp_names.append(sp)
            display_ge_names.append(ge)
        old_to_new[i] = j

    used_sp = set(display_sp_names)
    used_ge = set(display_ge_names)
    sp_vocab = [s for s in raw_sp_vocab if s in used_sp]
    ge_vocab = [g for g in raw_ge_vocab if g in used_ge]
    sp_idx = {s: i for i, s in enumerate(sp_vocab)}
    ge_idx = {g: i for i, g in enumerate(ge_vocab)}

    leaf2sp = np.array([sp_idx[s] for s in display_sp_names], dtype=np.int64)
    leaf2ge = np.array([ge_idx[g] for g in display_ge_names], dtype=np.int64)
    is_ssp = np.array([lf.count(" ") >= 2 for lf in display_leaves], dtype=bool)

    return {
        "old_to_new": old_to_new,
        "leaves": display_leaves,
        "sp_vocab": sp_vocab,
        "ge_vocab": ge_vocab,
        "leaf2sp": leaf2sp,
        "leaf2ge": leaf2ge,
        "is_ssp": is_ssp,
    }


def merge_leaf_probs(raw_probs, old_to_new, n_display, np):
    """Sum raw leaf probabilities into display leaves (conserves total mass within float64 rounding, < 1e-12)."""
    probs = np.asarray(raw_probs, dtype=np.float64)
    if old_to_new is None:
        return probs
    out = np.zeros(n_display, dtype=np.float64)
    np.add.at(out, old_to_new, probs)
    return out
