#!/usr/bin/env python3
"""Build the unified Confirmar visual field-ID card bundle for /identificar/.

Ingests verified Wave 2 Regional, Wave 3A (D+V), Wave 3B (Non-Ithomiini),
Wave 4 (D+V), Wave 5 (Non-Ithomiini), and Wave 6 (D+V) regional routing tables
+ ES/EN card PNGs (plus optional future waves via --wave/--extra-wave), verifies
every wave's AUDIT_RECEIPT.json fail-closed (status=PASS, explicit zero
locked/blind overlap fields required, exact source PNG SHA-256 match), handles
cross-wave superseding cleanly (active supersedes prior null hold; null+null
deduplicates to newer verified hold; active+active conflicts fail closed),
optimizes crisp full-resolution WebP copies for phone zoom legibility, and writes:
  - frontend/public/identificar/confirmar/confirmar_visual_manifest.json
  - frontend/public/identificar/confirmar/cards/{es,en}/<card_id>.webp

Subspecies aliases are written clean (no rank marker, no trailing "?", no database placeholder) and, when the
head's leaf list is on this host, every active subspecies-routed side must be reachable by at least one subspecies
the head names, or the build (and --check) fails.

Deterministic output (no wall-clock timestamp) so clean re-runs produce zero diff.
"""
from __future__ import annotations

import argparse
import csv
import hashlib
import io
import json
import re
import sys
import tempfile
from pathlib import Path
from typing import Any

from PIL import Image

sys.dont_write_bytecode = True

REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_OUT_DIR = REPO_ROOT / "frontend" / "public" / "identificar" / "confirmar"
WINGS_REPORTS = Path("/home/franz/Documents/CodeProjs/WingsClassificator/reports")

DEFAULT_WAVES = [
    (
        "wave2_regional",
        WINGS_REPORTS / "confirmar_wave2_regional_20261125",
        "REGIONAL_MAP.json",
    ),
    (
        "wave3a_dv",
        WINGS_REPORTS / "confirmar_wave3a_dv_20261125",
        "REGIONAL_MAP_W3A.json",
    ),
    (
        "wave3b_non_ithomiini",
        WINGS_REPORTS / "confirmar_wave3b_non_ithomiini_20261125",
        "REGIONAL_MAP_W3B.json",
    ),
    (
        "wave4_dv",
        WINGS_REPORTS / "confirmar_wave4_dv_20261125",
        "REGIONAL_MAP_W4.json",
    ),
    (
        "wave5_non_ithomiini",
        WINGS_REPORTS / "confirmar_wave5_non_ithomiini_20261125",
        "REGIONAL_MAP_W5.json",
    ),
    (
        "wave6_dv",
        WINGS_REPORTS / "confirmar_wave6_dv_20261125",
        "REGIONAL_MAP_W6.json",
    ),
]

LOCKED_AUDIT_SCRIPT_DIR = WINGS_REPORTS / "confirmar_visual_wave1_20261123" / "scripts"

# The identifier head's leaf names (one row per leaf; a leaf of three words is a subspecies the head can name).
HEAD_LEAF_MAP = WINGS_REPORTS / "head_retrain_names_20261115" / "labels_q" / "leaf_map.tsv"
HEAD_LEAF_COLUMNS = ("old_leaf", "new_leaf")

# Rank markers the collection writes before an epithet. The head never sends them, so they are no part of a name.
RANK_MARKERS = {"f.", "ssp.", "subsp."}

# Aliases the wave maps copied from the collection's free text that are no name at all. Keyed by the normalized alias.
ALIAS_REPAIRS: dict[str, list[str]] = {
    "deceptus- phasianita": ["deceptus", "phasianita"],  # two subspecies of Mechanitis messenoides run together
    "ssp-f": [],  # the collection's mark for an unnamed form of Hypothyris lycaste
    "ssp f 1": [],  # the same, of Hypothyris semifulva
    "nov.": [],  # "ssp. nov.": an undescribed subspecies of Heliconius timareta (w6_02)
}

# Subspecies the wave maps left out, keyed by (pair_id, card_id, side). The maps list Heliconius cydno by the colour
# forms the collection records (f. white, f. yellow, f. intermedia). Those are the forms of H. cydno alithea, the
# subspecies of West Ecuador, and the head names the subspecies, never the form: without this line no head output
# reaches these sides.
ALIAS_ADDITIONS: dict[tuple[str, str, str], list[str]] = {
    ("p14", "p14", "a"): ["alithea", "Heliconius cydno alithea"],
    ("w3a_15", "w3a_15", "a"): ["alithea"],
    ("w4_11", "w4_11", "a"): ["alithea"],
    # The map writes the third form of Eresia pelonia «calonia»; the head's leaf is «callonia».
    ("w6_05", "w6_05", "b"): ["callonia"],
    # Napo's rayed Heliconius melpomene malleti (the co-mimic of H. erato lativitta) is read by the head as the rayed
    # Amazonian H. m. schunkei (a real phone photo of 26 Nov 2026: schunkei 77 %). Without this line that photo reaches
    # no East guide of this pair; schunkei lives in the same rayed ring, east of the Andes only.
    ("p01", "p01_east_napo", "b"): ["schunkei", "Heliconius melpomene schunkei"],
}

# What may never be left in an alias of a subspecies-routed pair: a question mark, a rank marker with no epithet
# ("ssp-f", "ssp f 1", "f.", "nov."), or a hyphen beside a space ("deceptus- phasianita").
BROKEN_ALIAS_RE = re.compile(r"\?|(?:^|\s)(?:ssp|subsp|sp|f|nov)(?:[\s.\-]|$)|-\s|\s-")

ALLOWED_NON_ACTIVE_STATUSES = {
    "no_unlocked_museum_pair",
    "inseparable_in_photos",
    "not_reliably_separable_in_photos",
    "held_sex_confounded_small_n",
    "held_small_n_co_mimics",
    "held_suspect_reference_labels",
    "inseparable_or_unverified_refs",
}

REGION_BUCKET_LABELS = {
    "oriente": ("Oriente", "East Ecuador"),
    "occidente": ("Occidente", "West Ecuador"),
    "both": ("Oriente y Occidente", "East & West Ecuador"),
}


def sha256_file(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(65536), b""):
            h.update(chunk)
    return h.hexdigest()


def norm_species_pair(a: str, b: str) -> tuple[str, str]:
    na = " ".join(a.strip().lower().split())
    nb = " ".join(b.strip().lower().split())
    return (na, nb) if na <= nb else (nb, na)


def norm_subspecies(value: str) -> str:
    """A subspecies name as the router compares it (normSubspecies in frontend/src/lib/confirmarVisualRouter.ts):
    lower case, single spaces, no rank marker (f., ssp., subsp.), no trailing question mark."""
    words = [w for w in value.lower().split() if w not in RANK_MARKERS]
    return re.sub(r"(\s*\?)+$", "", " ".join(words))


def clean_aliases(raw_aliases: list[Any], extra: list[str] | None = None) -> list[str]:
    """The aliases the manifest carries: each as written but without rank marker or trailing "?", the broken ones
    repaired (ALIAS_REPAIRS), then `extra`; one of each, compared as the router compares them."""
    out: list[str] = []
    seen: set[str] = set()
    for raw in [*raw_aliases, *(extra or [])]:
        words = [w for w in str(raw).split() if w.lower() not in RANK_MARKERS]
        alias = re.sub(r"(\s*\?)+$", "", " ".join(words))
        for fixed in ALIAS_REPAIRS.get(norm_subspecies(alias), [alias]):
            key = norm_subspecies(fixed)
            if key and key not in seen:
                seen.add(key)
                out.append(fixed)
    return out


def load_head_subspecies(path: Path) -> list[dict[str, set[str]]] | None:
    """The subspecies the head can name, per species (lower case): one map per name column of the leaf list.
    None when the list is not on this host."""
    if not path.is_file():
        return None
    with path.open(encoding="utf-8", newline="") as f:
        rows = list(csv.DictReader(f, delimiter="\t"))
    columns = [c for c in HEAD_LEAF_COLUMNS if rows and c in rows[0]]
    if not columns:
        raise RuntimeError(f"{path} has none of the columns {HEAD_LEAF_COLUMNS}")
    maps: list[dict[str, set[str]]] = []
    for column in columns:
        by_species: dict[str, set[str]] = {}
        for row in rows:
            words = str(row[column]).lower().split()
            if len(words) >= 3:
                by_species.setdefault(" ".join(words[:2]), set()).add(" ".join(words[2:]))
        maps.append(by_species)
    return maps


def alias_problems(
    pairs: dict[str, Any], head: list[dict[str, set[str]]] | None
) -> tuple[list[str], dict[str, int]]:
    """What is wrong with the aliases of the subspecies-routed pairs, and how many active sides were checked.

    Always: no alias may be left unclean or broken. With the head's leaf list: every active side whose species the
    head splits into subspecies must list at least one of them (under each name column), or no head output reaches it.
    """
    problems: list[str] = []
    counts = {"reachable": 0, "no_head_subspecies": 0}
    for pair_id, pair in pairs.items():
        if pair.get("routing_mode") != "subspecies_and_side":
            continue
        for route in pair.get("routes", []):
            where = f"{pair_id}:{route.get('card_id') or route.get('status')}"
            for side in ("a", "b"):
                aliases = list(route.get(f"subspecies_{side}_aliases", []))
                for alias in aliases:
                    if clean_aliases([alias]) != [alias] or BROKEN_ALIAS_RE.search(norm_subspecies(alias)):
                        problems.append(f"{where} side {side}: broken alias {alias!r}")
                if head is None or route.get("status") != "active":
                    continue
                species = " ".join(str(pair[f"species_{side}"]).lower().split()[:2])
                epithets = {
                    n[len(species) + 1 :] if n.startswith(species + " ") else n
                    for n in (norm_subspecies(a) for a in aliases)
                }
                named = [leaves[species] for leaves in head if leaves.get(species)]
                if not named:
                    counts["no_head_subspecies"] += 1  # the head sends no subspecies of it: the side routes by region
                elif all(epithets & leaves for leaves in named):
                    counts["reachable"] += 1
                else:
                    problems.append(
                        f"{where} side {side} ({species}): no alias {sorted(epithets)} is a subspecies the head "
                        f"names ({sorted(set().union(*named))})"
                    )
    return problems, counts


def load_optional_locked_cohorts() -> tuple[set[str], set[str], set[str]] | None:
    """Load canonical locked CAM + iNat cohorts from WingsClassificator when present on this host."""
    if not (LOCKED_AUDIT_SCRIPT_DIR / "run_full_audit.py").is_file():
        return None
    sys.path.insert(0, str(LOCKED_AUDIT_SCRIPT_DIR))
    try:
        import run_full_audit as rfa  # type: ignore

        locked_cams, s1_obs, fs_obs, fs_photos = rfa.load_locked_sets()
        cams = {str(x).strip().upper() for x in locked_cams if str(x).strip()}
        obs = {str(x).strip() for x in (set(s1_obs) | set(fs_obs)) if str(x).strip()}
        photos = {str(x).strip() for x in fs_photos if str(x).strip()}
        if len(cams) < 1583:
            raise RuntimeError(f"Canonical locked CAM cohort unexpectedly small: {len(cams)}")
        if len(obs) < 32776:
            raise RuntimeError(f"Canonical locked iNat obs cohort unexpectedly small: {len(obs)}")
        if len(photos) < 1368:
            raise RuntimeError(f"Canonical locked iNat photo cohort unexpectedly small: {len(photos)}")
        return cams, obs, photos
    finally:
        if sys.path and sys.path[0] == str(LOCKED_AUDIT_SCRIPT_DIR):
            sys.path.pop(0)


def _require_zero_int(mapping: dict[str, Any], key: str, label: str) -> int:
    if key not in mapping:
        raise RuntimeError(f"{label} missing required zero-overlap field {key!r}")
    val = mapping[key]
    if isinstance(val, bool) or not isinstance(val, int) or val != 0:
        raise RuntimeError(f"{label} field {key!r} must be integer 0, got {val!r}")
    return val


KNOWN_WAVE_REQUIRED_OVERLAP_KEY = {
    "wave2_regional": "locked_or_blind_or_curation_overlap",
    "wave3a_dv": "leakage_or_integrity_errors",
    "wave4_dv": "leakage_hits_count",
    "wave6_dv": "leakage_hits_count",
}

KNOWN_INAT_COHORT_WAVES = {
    "wave3b_non_ithomiini",
    "wave5_non_ithomiini",
}


def verify_receipt_dict(
    wave_id: str,
    receipt: dict[str, Any],
    locked_cohorts: tuple[set[str], set[str], set[str]] | None,
) -> tuple[dict[str, Any], int]:
    """Fail-closed audit receipt validation; returns (receipt, measured_overlap_count)."""
    if receipt.get("status") != "PASS":
        raise RuntimeError(f"[{wave_id}] AUDIT_RECEIPT status is {receipt.get('status')!r}, expected 'PASS'")

    measured_overlap = 0

    # Unconditionally reject any contradictory error or leakage field if present in any schema (Sol R2-F1)
    if "leakage_hits" in receipt:
        if not isinstance(receipt["leakage_hits"], list) or len(receipt["leakage_hits"]) != 0:
            raise RuntimeError(f"[{wave_id}] Contradictory non-empty 'leakage_hits': {receipt['leakage_hits']!r}")
    if "errors" in receipt:
        if not isinstance(receipt["errors"], list) or len(receipt["errors"]) != 0:
            raise RuntimeError(f"[{wave_id}] Contradictory non-empty 'errors': {receipt['errors']!r}")
    for cross_key in ("locked_or_blind_or_curation_overlap", "leakage_or_integrity_errors", "leakage_hits_count"):
        if cross_key in receipt:
            measured_overlap += _require_zero_int(receipt, cross_key, f"[{wave_id}] AUDIT_RECEIPT")

    receipt_locked = receipt.get("locked_cohorts")

    if isinstance(receipt_locked, dict):
        if wave_id in KNOWN_WAVE_REQUIRED_OVERLAP_KEY:
            raise RuntimeError(f"[{wave_id}] Unexpected locked_cohorts schema for museum wave")
        # Wave 3B / Wave 5 structured schema: every overlap counter + errors list is mandatory
        required_cohort_keys = [
            "locked_cam_overlap",
            "locked_inat_obs_overlap",
            "locked_inat_photo_overlap",
            "ref_blind_sha256_overlap",
        ]
        if wave_id == "wave5_non_ithomiini":
            required_cohort_keys.extend(
                ["fresh_s3_overlap", "w3b_blind_overlap", "ref_blind_pixel_sha256_overlap"]
            )
        for key in required_cohort_keys:
            measured_overlap += _require_zero_int(receipt_locked, key, f"[{wave_id}] locked_cohorts")
        for extra_zero_key in (
            "fresh_s3_overlap",
            "w3b_blind_overlap",
            "ref_blind_pixel_sha256_overlap",
            "cam_ids_found_in_inputs",
        ):
            if extra_zero_key in receipt_locked and extra_zero_key not in required_cohort_keys:
                measured_overlap += _require_zero_int(receipt_locked, extra_zero_key, f"[{wave_id}] locked_cohorts")
        if "errors" not in receipt:
            raise RuntimeError(f"[{wave_id}] AUDIT_RECEIPT must have empty list 'errors'")
        locked_cams_n = receipt_locked.get("locked_cams")
        if isinstance(locked_cams_n, bool) or not isinstance(locked_cams_n, int) or locked_cams_n < 1583:
            raise RuntimeError(f"[{wave_id}] locked_cohorts.locked_cams must be >= 1583, got {locked_cams_n!r}")
    else:
        if wave_id in KNOWN_INAT_COHORT_WAVES:
            raise RuntimeError(f"[{wave_id}] Missing required 'locked_cohorts' object")
        # Wave 2 / 3A / 4 / 6 museum schema: explicit leakage_hits + exact overlap/integrity counter mandatory
        if "leakage_hits" not in receipt:
            raise RuntimeError(f"[{wave_id}] AUDIT_RECEIPT must have empty list 'leakage_hits'")
        required_key = KNOWN_WAVE_REQUIRED_OVERLAP_KEY.get(wave_id)
        if required_key:
            _require_zero_int(receipt, required_key, f"[{wave_id}] AUDIT_RECEIPT")
        else:
            overlap_keys = [
                k
                for k in ("locked_or_blind_or_curation_overlap", "leakage_or_integrity_errors", "leakage_hits_count")
                if k in receipt
            ]
            if not overlap_keys:
                raise RuntimeError(
                    f"[{wave_id}] AUDIT_RECEIPT missing required zero-overlap field "
                    "('locked_or_blind_or_curation_overlap', 'leakage_or_integrity_errors', or 'leakage_hits_count')"
                )
        locked_cams_n = receipt.get("locked_cams_checked")
        if isinstance(locked_cams_n, bool) or not isinstance(locked_cams_n, int) or locked_cams_n < 1583:
            raise RuntimeError(f"[{wave_id}] locked_cams_checked must be >= 1583, got {locked_cams_n!r}")
        if not isinstance(receipt.get("cams_used"), list) or not receipt["cams_used"]:
            raise RuntimeError(f"[{wave_id}] AUDIT_RECEIPT missing non-empty 'cams_used' list")

    if locked_cohorts is not None:
        locked_cams, locked_obs, locked_photos = locked_cohorts
        if isinstance(receipt.get("cams_used"), list):
            used = {str(c).strip().upper() for c in receipt["cams_used"]}
            overlap = sorted(used & locked_cams)
            measured_overlap += len(overlap)
            if overlap:
                raise RuntimeError(f"[{wave_id}] Locked CAM overlap detected: {overlap}")
        if isinstance(receipt.get("card_used_photo_provenance"), dict):
            for slot_key, prov in receipt["card_used_photo_provenance"].items():
                obs_id = str(prov.get("obs_id") or "").strip()
                photo_id = str(prov.get("photo_id") or "").strip()
                if obs_id and obs_id in locked_obs:
                    measured_overlap += 1
                    raise RuntimeError(f"[{wave_id}] Locked iNat obs overlap on {slot_key}: {obs_id}")
                if photo_id and photo_id in locked_photos:
                    measured_overlap += 1
                    raise RuntimeError(f"[{wave_id}] Locked iNat photo overlap on {slot_key}: {photo_id}")

    output_png_sha256 = receipt.get("output_png_sha256")
    if not isinstance(output_png_sha256, dict) or not output_png_sha256:
        raise RuntimeError(f"[{wave_id}] Missing output_png_sha256 map in AUDIT_RECEIPT.json")

    return receipt, measured_overlap


def verify_wave_receipt(
    wave_id: str,
    wave_dir: Path,
    locked_cohorts: tuple[set[str], set[str], set[str]] | None,
) -> tuple[dict[str, Any], int]:
    receipt_path = wave_dir / "AUDIT_RECEIPT.json"
    if not receipt_path.is_file():
        raise RuntimeError(f"[{wave_id}] Missing AUDIT_RECEIPT.json at {receipt_path}")
    receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
    return verify_receipt_dict(wave_id, receipt, locked_cohorts)


def load_rendered_text(wave_dir: Path, lang: str) -> dict[str, list[dict[str, Any]]]:
    p = wave_dir / "cards" / f"rendered_text_{lang}.json"
    if not p.is_file():
        return {}
    data = json.loads(p.read_text(encoding="utf-8"))
    cards = data.get("cards", data)
    return cards if isinstance(cards, dict) else {}


def derive_view_badge(
    route: dict[str, Any],
    card_id: str,
    rendered_es: dict[str, list[dict[str, Any]]],
    rendered_en: dict[str, list[dict[str, Any]]],
) -> tuple[str | None, str | None]:
    if route.get("view_pill_es") and route.get("view_pill_en"):
        return str(route["view_pill_es"]).strip(), str(route["view_pill_en"]).strip()

    items_en = rendered_en.get(f"{card_id}_en") or []
    items_es = rendered_es.get(f"{card_id}_es") or []

    pill_es = next((x["text"].strip() for x in items_es if x.get("role") == "view_pill" and x.get("text")), None)
    pill_en = next((x["text"].strip() for x in items_en if x.get("role") == "view_pill" and x.get("text")), None)
    if pill_es and pill_en:
        return pill_es, pill_en

    tags_en = " ".join(
        str(x.get("text", "")).lower() for x in items_en if x.get("role") == "view_tag"
    )
    has_dorsal = "dorsal" in tags_en or "upperside" in tags_en
    has_ventral = "ventral" in tags_en or "underside" in tags_en
    if has_dorsal and has_ventral:
        return "Anverso y reverso", "Upperside & underside"
    if has_ventral:
        return "Vista ventral", "Underside view"
    if has_dorsal:
        return "Vista dorsal", "Upperside view"
    return None, None


def detect_wave_routing_mode(reg_map: dict[str, Any], wave_id: str = "") -> str:
    """Detect whether a wave routes by side-only (Wave 3B/5) or subspecies+side (Wave 2/3A/4/6)."""
    if wave_id in KNOWN_INAT_COHORT_WAVES:
        return "side_only"
    lookup_list = reg_map.get("routing_contract", {}).get("lookup_order") or []
    purpose = str(reg_map.get("purpose", "")).lower()
    if lookup_list:
        lookup = " ".join(str(x).lower() for x in lookup_list)
        if "subspecies" in lookup or "subspecies" in purpose:
            return "subspecies_and_side"
        return "side_only"
    if "subspecies" in purpose:
        return "subspecies_and_side"
    if wave_id in KNOWN_WAVE_REQUIRED_OVERLAP_KEY or "_dv" in str(reg_map.get("wave", "")).lower():
        return "subspecies_and_side"
    return "side_only"


def resolve_region_labels(route: dict[str, Any], wave_id: str, pair_id: str) -> tuple[str, str]:
    region_es = str(route.get("region_es") or "").strip()
    region_en = str(route.get("region_en") or "").strip()
    if region_es and region_en:
        return region_es, region_en
    bucket = str(route.get("region_bucket") or "").strip().lower()
    if bucket not in REGION_BUCKET_LABELS:
        raise RuntimeError(
            f"[{wave_id}:{pair_id}] Missing region_es/region_en and unknown region_bucket {bucket!r}"
        )
    fallback_es, fallback_en = REGION_BUCKET_LABELS[bucket]
    return region_es or fallback_es, region_en or fallback_en


def resolve_taxon_display(route: dict[str, Any], pair: dict[str, Any], side_key: str, species: str) -> str:
    raw_taxon = str(route.get(f"taxon_{side_key}") or "").strip()
    display_name = str(pair.get(f"display_name_{side_key}") or "").strip()
    if display_name and (not raw_taxon or raw_taxon.lower() == species.lower()):
        return display_name
    return raw_taxon or species


def resolve_species_pair_collision(
    sp_key: tuple[str, str],
    prev_pair_id: str,
    prev_pair: dict[str, Any],
    new_pair_id: str,
    new_routes: list[dict[str, Any]],
) -> str:
    """Validate cross-wave collision for the same species pair.

    Returns 'supersede_prev' when the incoming pair cleanly supersedes an earlier
    non-active hold (active rescues null hold, or newer verified null hold replaces
    earlier null hold on the same region/sides). Raises RuntimeError fail-closed on
    any active-vs-active conflict, active-vs-null regression, or disjoint-region hold.
    """
    prev_routes = prev_pair.get("routes", [])
    prev_has_active = any(str(r.get("status", "")).strip() == "active" for r in prev_routes)
    new_has_active = any(str(r.get("status", "")).strip() == "active" for r in new_routes)

    if prev_has_active and new_has_active:
        raise RuntimeError(
            f"Duplicate active species pair {sp_key!r}: {new_pair_id!r} conflicts with active {prev_pair_id!r}"
        )
    if prev_has_active and not new_has_active:
        raise RuntimeError(
            f"Non-active species pair {sp_key!r} ({new_pair_id!r}) cannot overwrite active {prev_pair_id!r}"
        )

    prev_sides = {str(s).strip() for r in prev_routes for s in (r.get("side_andes") or []) if str(s).strip()}
    if new_has_active:
        new_active_sides = {
            str(s).strip()
            for r in new_routes
            if str(r.get("status", "")).strip() == "active"
            for s in (r.get("side_andes") or [])
            if str(s).strip()
        }
        if prev_sides and not prev_sides.issubset(new_active_sides):
            raise RuntimeError(
                f"Active pair {new_pair_id!r} (sides={sorted(new_active_sides)}) does not cover "
                f"prior held pair {prev_pair_id!r} (sides={sorted(prev_sides)}) for {sp_key!r}"
            )
        return "supersede_prev"

    # Both non-active (null holds): deduplicate to the newer verified hold when sides match/subsume
    new_sides = {str(s).strip() for r in new_routes for s in (r.get("side_andes") or []) if str(s).strip()}
    if prev_sides and not prev_sides.issubset(new_sides):
        raise RuntimeError(
            f"Held pair {new_pair_id!r} (sides={sorted(new_sides)}) does not cover "
            f"prior held pair {prev_pair_id!r} (sides={sorted(prev_sides)}) for {sp_key!r}"
        )
    return "supersede_prev"


def optimize_png_to_webp(
    src_png: Path, dst_webp: Path, quality: int, dry_run: bool
) -> tuple[int, int, int, str]:
    with Image.open(src_png) as im:
        if im.mode not in ("RGB", "RGBA"):
            im = im.convert("RGB")
        width, height = im.size
        if dry_run:
            buf = io.BytesIO()
            im.save(buf, format="WEBP", quality=quality, method=6)
            data = buf.getvalue()
            return width, height, len(data), hashlib.sha256(data).hexdigest()

        dst_webp.parent.mkdir(parents=True, exist_ok=True)
        tmp = dst_webp.with_suffix(".webp.tmp")
        im.save(tmp, format="WEBP", quality=quality, method=6)
        tmp.replace(dst_webp)
        return width, height, dst_webp.stat().st_size, sha256_file(dst_webp)


def parse_wave_arg(spec: str) -> tuple[str, Path, str]:
    parts = spec.split(":", 2)
    if len(parts) != 3 or not all(parts):
        raise argparse.ArgumentTypeError(
            f"Expected wave_id:dir_path:regional_map_filename, got {spec!r}"
        )
    return parts[0], Path(parts[1]), parts[2]


def build_bundle(
    waves: list[tuple[str, Path, str]],
    out_dir: Path,
    quality: int = 92,
    check_only: bool = False,
    leaf_map: Path = HEAD_LEAF_MAP,
) -> dict[str, Any]:
    locked_cohorts = load_optional_locked_cohorts()
    head_subspecies = load_head_subspecies(leaf_map)
    # A check that cannot check does not pass; nor does a list asked for by name. Said before any card is written.
    # A plain build on a host without the list goes on, and says so in the manifest (`head_subspecies_checked`).
    if head_subspecies is None and (check_only or leaf_map != HEAD_LEAF_MAP):
        raise RuntimeError(f"The head's leaf list is not at {leaf_map}: the aliases cannot be checked against it")
    additions_used: set[tuple[str, str, str]] = set()

    unified_pairs: dict[str, Any] = {}
    seen_card_ids: set[str] = set()
    seen_species_pairs: dict[tuple[str, str], str] = {}
    superseded_pairs: dict[str, str] = {}
    expected_webp_relpaths: set[Path] = set()
    wave_summaries: list[dict[str, Any]] = []

    total_active_cards = 0
    total_active_pairs = 0
    total_non_active_pairs = 0
    total_deferred_routes = 0
    total_webp_bytes = 0
    total_src_png_bytes = 0
    total_measured_overlap = 0

    for wave_id, wave_dir, map_name in waves:
        wave_dir = wave_dir.resolve()
        map_path = wave_dir / map_name
        if not map_path.is_file():
            raise RuntimeError(f"[{wave_id}] Missing regional map at {map_path}")

        receipt, wave_overlap = verify_wave_receipt(wave_id, wave_dir, locked_cohorts)
        total_measured_overlap += wave_overlap
        receipt_png_sha = receipt["output_png_sha256"]
        reg_map = json.loads(map_path.read_text(encoding="utf-8"))
        routing_mode = detect_wave_routing_mode(reg_map, wave_id=wave_id)
        rendered_es = load_rendered_text(wave_dir, "es")
        rendered_en = load_rendered_text(wave_dir, "en")

        pairs_obj = reg_map.get("pairs")
        if not isinstance(pairs_obj, dict) or not pairs_obj:
            raise RuntimeError(f"[{wave_id}] Empty or invalid pairs in {map_path}")

        wave_active_cards = 0
        wave_non_active_pairs = 0

        for pair_id, pair in pairs_obj.items():
            if pair_id in unified_pairs or pair_id in superseded_pairs:
                raise RuntimeError(f"Duplicate pair_id {pair_id!r} across waves")

            species_a = str(pair["species_a"]).strip()
            species_b = str(pair["species_b"]).strip()
            sp_key = norm_species_pair(species_a, species_b)
            supersedes_list: list[str] = []
            if sp_key in seen_species_pairs:
                prev_pair_id = seen_species_pairs[sp_key]
                prev_pair = unified_pairs[prev_pair_id]
                resolve_species_pair_collision(
                    sp_key=sp_key,
                    prev_pair_id=prev_pair_id,
                    prev_pair=prev_pair,
                    new_pair_id=pair_id,
                    new_routes=list(pair.get("routes", [])),
                )
                # Remove superseded non-active hold so it can never shadow the newer entry
                prior_supersedes = list(prev_pair.get("supersedes", []))
                for old_id in prior_supersedes:
                    superseded_pairs[old_id] = pair_id
                superseded_pairs[prev_pair_id] = pair_id
                supersedes_list = [*prior_supersedes, prev_pair_id]
                del unified_pairs[prev_pair_id]
                total_non_active_pairs -= 1
            seen_species_pairs[sp_key] = pair_id

            routes_out: list[dict[str, Any]] = []
            pair_has_active = False

            for route in pair.get("routes", []):
                status = str(route["status"]).strip()
                card_id = route.get("card_id")
                region_es, region_en = resolve_region_labels(route, wave_id, pair_id)

                route_entry: dict[str, Any] = {
                    "card_id": card_id,
                    "status": status,
                    "side_andes": list(route.get("side_andes", [])),
                    "side_default_for": list(route.get("side_default_for", [])),
                    "region_bucket": str(route.get("region_bucket", "both")),
                    "region_es": region_es,
                    "region_en": region_en,
                    "taxon_a": resolve_taxon_display(route, pair, "a", species_a),
                    "taxon_b": resolve_taxon_display(route, pair, "b", species_b),
                }
                for side_key in ("a", "b"):
                    add_key = (pair_id, str(card_id), side_key)
                    if add_key in ALIAS_ADDITIONS:
                        additions_used.add(add_key)
                    raw_aliases = route.get(f"subspecies_{side_key}_aliases", [])
                    # The side-only waves carry notes in these fields, which the router never reads: kept as written.
                    route_entry[f"subspecies_{side_key}_aliases"] = (
                        clean_aliases(raw_aliases, ALIAS_ADDITIONS.get(add_key))
                        if routing_mode == "subspecies_and_side"
                        else [str(x).strip() for x in raw_aliases if str(x).strip()]
                    )
                if route.get("region_detail_es"):
                    route_entry["region_detail_es"] = str(route["region_detail_es"]).strip()
                if route.get("region_detail_en"):
                    route_entry["region_detail_en"] = str(route["region_detail_en"]).strip()

                if status == "active":
                    if not isinstance(card_id, str) or not card_id.strip():
                        raise RuntimeError(f"[{wave_id}:{pair_id}] Active route missing card_id")
                    if card_id in seen_card_ids:
                        raise RuntimeError(f"Duplicate active card_id {card_id!r}")
                    seen_card_ids.add(card_id)
                    pair_has_active = True
                    wave_active_cards += 1
                    total_active_cards += 1

                    vb_es, vb_en = derive_view_badge(route, card_id, rendered_es, rendered_en)
                    if vb_es:
                        route_entry["view_badge_es"] = vb_es
                    if vb_en:
                        route_entry["view_badge_en"] = vb_en

                    assets: dict[str, Any] = {}
                    for lang in ("es", "en"):
                        src_png = wave_dir / "cards" / lang / f"card_{card_id}_{lang}.png"
                        if not src_png.is_file():
                            raise RuntimeError(f"[{wave_id}:{card_id}] Missing source PNG: {src_png}")
                        actual_src_sha = sha256_file(src_png)
                        expected_src_sha = receipt_png_sha.get(f"{card_id}_{lang}")
                        if not expected_src_sha:
                            raise RuntimeError(
                                f"[{wave_id}:{card_id}] Missing {card_id}_{lang} in AUDIT_RECEIPT.json output_png_sha256"
                            )
                        if actual_src_sha != expected_src_sha:
                            raise RuntimeError(
                                f"[{wave_id}:{card_id}_{lang}] SHA-256 mismatch vs AUDIT_RECEIPT.json: "
                                f"actual={actual_src_sha} expected={expected_src_sha}"
                            )
                        total_src_png_bytes += src_png.stat().st_size

                        rel_webp = Path("cards") / lang / f"{card_id}.webp"
                        expected_webp_relpaths.add(rel_webp)
                        dst_webp = out_dir / rel_webp
                        width, height, webp_bytes, webp_sha = optimize_png_to_webp(
                            src_png, dst_webp, quality=quality, dry_run=check_only
                        )
                        total_webp_bytes += webp_bytes
                        assets[lang] = {
                            "path": f"identificar/confirmar/{rel_webp.as_posix()}",
                            "width": width,
                            "height": height,
                            "bytes": webp_bytes,
                            "sha256": webp_sha,
                            "source_png_sha256": actual_src_sha,
                        }
                    route_entry["assets"] = assets
                else:
                    if status not in ALLOWED_NON_ACTIVE_STATUSES:
                        raise RuntimeError(f"[{wave_id}:{pair_id}] Unknown non-active status {status!r}")
                    if card_id is not None:
                        raise RuntimeError(f"[{wave_id}:{pair_id}] Non-active route must have card_id=null")
                    if status == "no_unlocked_museum_pair":
                        total_deferred_routes += 1

                routes_out.append(route_entry)

            if pair_has_active:
                total_active_pairs += 1
            else:
                total_non_active_pairs += 1
                wave_non_active_pairs += 1

            pair_record: dict[str, Any] = {
                "pair_id": pair_id,
                "wave": wave_id,
                "routing_mode": routing_mode,
                "species_a": species_a,
                "species_b": species_b,
                "classification": str(pair.get("classification", "single_region")),
                "default_card_id": pair.get("default_card_id"),
                "routes": routes_out,
            }
            if supersedes_list:
                pair_record["supersedes"] = supersedes_list
            unified_pairs[pair_id] = pair_record

        wave_summaries.append(
            {
                "wave": wave_id,
                "routing_mode": routing_mode,
                "map_version": reg_map.get("version") or reg_map.get("schema_version"),
                "pairs": len(pairs_obj),
                "active_cards": wave_active_cards,
                "non_active_pairs": wave_non_active_pairs,
                "audit_status": receipt.get("status"),
            }
        )

    # An addition whose pair is in this build but whose card is not: the map changed under it.
    stale_additions = sorted(k for k in ALIAS_ADDITIONS if k not in additions_used and k[0] in unified_pairs)
    if stale_additions:
        raise RuntimeError(f"ALIAS_ADDITIONS names a card or side that is not in the maps: {stale_additions}")
    problems, alias_counts = alias_problems(unified_pairs, head_subspecies)
    if problems:
        raise RuntimeError("Subspecies aliases:\n  " + "\n  ".join(problems))
    if head_subspecies is None:
        print(f"NOTE: {leaf_map} is not on this host: the aliases were not checked against the head's subspecies")

    # Prune stale .webp files in cards/{es,en} before writing manifest
    removed_stale = 0
    if not check_only:
        for lang in ("es", "en"):
            lang_dir = out_dir / "cards" / lang
            if not lang_dir.is_dir():
                continue
            for existing in lang_dir.iterdir():
                if existing.is_file() and existing.suffix == ".webp":
                    rel = Path("cards") / lang / existing.name
                    if rel not in expected_webp_relpaths:
                        existing.unlink()
                        removed_stale += 1

    verification: dict[str, Any] = {
        "all_wave_receipts_passed": all(w["audit_status"] == "PASS" for w in wave_summaries),
        "locked_or_blind_overlap": total_measured_overlap,
        "canonical_locked_cams_cross_checked": len(locked_cohorts[0]) if locked_cohorts is not None else 0,
        "canonical_locked_inat_obs_cross_checked": len(locked_cohorts[1]) if locked_cohorts is not None else 0,
        "canonical_locked_inat_photos_cross_checked": len(locked_cohorts[2]) if locked_cohorts is not None else 0,
        "webp_quality": quality,
        "head_subspecies_checked": head_subspecies is not None,
        "subspecies_sides_reachable": alias_counts["reachable"],
        "subspecies_sides_without_head_subspecies": alias_counts["no_head_subspecies"],
    }
    if removed_stale:
        print(f"Pruned {removed_stale} stale WebP asset(s) under {out_dir / 'cards'}")

    manifest: dict[str, Any] = {
        "schema": "confirmar-visual-cards/1",
        "waves": wave_summaries,
        "verification": verification,
        "counts": {
            "total_pairs": len(unified_pairs),
            "active_pairs": total_active_pairs,
            "active_cards": total_active_cards,
            "non_active_pairs": total_non_active_pairs,
            "superseded_pairs": len(superseded_pairs),
            "deferred_routes": total_deferred_routes,
            "assets_total": total_active_cards * 2,
            "source_png_bytes": total_src_png_bytes,
            "webp_bytes": total_webp_bytes,
        },
        "superseded_pairs": superseded_pairs,
        "pairs": unified_pairs,
    }

    if not check_only:
        out_dir.mkdir(parents=True, exist_ok=True)
        manifest_path = out_dir / "confirmar_visual_manifest.json"
        tmp = manifest_path.with_suffix(".json.tmp")
        tmp.write_text(json.dumps(manifest, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
        tmp.replace(manifest_path)

    return manifest


def run_self_tests() -> None:
    """Negative self-tests ensuring fail-closed receipt verification."""
    # 1. Missing overlap fields must fail (Sol R1-F1 / Fable R1-F9)
    bad_w2_missing_overlap = {
        "status": "PASS",
        "locked_cams_checked": 1583,
        "cams_used": ["CAM000001"],
        "output_png_sha256": {"p01_es": "0" * 64},
    }
    for label, bad_receipt in [
        ("missing_overlap_and_leakage_hits", bad_w2_missing_overlap),
        ("missing_overlap_only", {**bad_w2_missing_overlap, "leakage_hits": []}),
        ("missing_leakage_hits", {**bad_w2_missing_overlap, "locked_or_blind_or_curation_overlap": 0}),
        ("nonzero_overlap", {**bad_w2_missing_overlap, "leakage_hits": [], "locked_or_blind_or_curation_overlap": 1}),
        (
            "w3b_missing_field",
            {
                "status": "PASS",
                "locked_cohorts": {"locked_cams": 1583, "locked_cam_overlap": 0},
                "errors": [],
                "output_png_sha256": {"w3b_01_es": "0" * 64},
            },
        ),
        (
            "w3b_missing_errors_list",
            {
                "status": "PASS",
                "locked_cohorts": {
                    "locked_cams": 1583,
                    "locked_cam_overlap": 0,
                    "locked_inat_obs_overlap": 0,
                    "locked_inat_photo_overlap": 0,
                    "ref_blind_sha256_overlap": 0,
                },
                "output_png_sha256": {"w3b_01_es": "0" * 64},
            },
        ),
        (
            "wave3b_non_ithomiini",
            {
                "status": "PASS",
                "locked_cohorts": {
                    "locked_cams": 1583,
                    "locked_cam_overlap": 0,
                    "locked_inat_obs_overlap": 0,
                    "locked_inat_photo_overlap": 0,
                    "ref_blind_sha256_overlap": 0,
                },
                "errors": [],
                "leakage_hits": ["contradictory audit hit"],
                "output_png_sha256": {"w3b_01_es": "0" * 64},
            },
        ),
        (
            "wave2_regional",
            {
                **bad_w2_missing_overlap,
                "leakage_hits": [],
                "leakage_or_integrity_errors": 0,
            },
        ),
    ]:
        try:
            verify_receipt_dict(label, bad_receipt, None)
        except RuntimeError:
            pass
        else:
            raise AssertionError(f"Self-test failed: {label} was unexpectedly accepted")

    # 2. Valid Wave 2 & Wave 3B receipts must pass with overlap=0
    _, ov2 = verify_receipt_dict(
        "wave2_regional",
        {
            "status": "PASS",
            "locked_cams_checked": 1583,
            "cams_used": ["CAM000001"],
            "leakage_hits": [],
            "locked_or_blind_or_curation_overlap": 0,
            "output_png_sha256": {"p01_es": "0" * 64},
        },
        ({"CAM999999"}, set(), set()),
    )
    assert ov2 == 0
    _, ov3b = verify_receipt_dict(
        "wave3b_non_ithomiini",
        {
            "status": "PASS",
            "locked_cohorts": {
                "locked_cams": 1583,
                "locked_cam_overlap": 0,
                "locked_inat_obs_overlap": 0,
                "locked_inat_photo_overlap": 0,
                "ref_blind_sha256_overlap": 0,
            },
            "errors": [],
            "output_png_sha256": {"w3b_01_es": "0" * 64},
        },
        ({"CAM999999"}, {"111"}, {"222"}),
    )
    assert ov3b == 0

    # 3. Wave 4 / Wave 6 museum receipts & Wave 5 iNat cohort receipts (negative + positive)
    for bad_w_label, bad_w_id, bad_rec in [
        (
            "w4_nonzero_leakage_hits_count",
            "wave4_dv",
            {
                **bad_w2_missing_overlap,
                "leakage_hits": [],
                "leakage_hits_count": 1,
            },
        ),
        (
            "w6_missing_leakage_hits_count",
            "wave6_dv",
            {
                **bad_w2_missing_overlap,
                "leakage_hits": [],
                "locked_or_blind_or_curation_overlap": 0,
            },
        ),
        (
            "w5_nonzero_fresh_s3_overlap",
            "wave5_non_ithomiini",
            {
                "status": "PASS",
                "locked_cohorts": {
                    "locked_cams": 1583,
                    "locked_cam_overlap": 0,
                    "locked_inat_obs_overlap": 0,
                    "locked_inat_photo_overlap": 0,
                    "fresh_s3_overlap": 1,
                    "w3b_blind_overlap": 0,
                    "ref_blind_sha256_overlap": 0,
                    "ref_blind_pixel_sha256_overlap": 0,
                },
                "errors": [],
                "output_png_sha256": {"w5_02_es": "0" * 64},
            },
        ),
    ]:
        try:
            verify_receipt_dict(bad_w_id, bad_rec, None)
        except RuntimeError:
            pass
        else:
            raise AssertionError(f"Self-test failed: {bad_w_label} was unexpectedly accepted")

    _, ov4 = verify_receipt_dict(
        "wave4_dv",
        {
            "status": "PASS",
            "locked_cams_checked": 1583,
            "cams_used": ["CAM000001"],
            "leakage_hits": [],
            "leakage_hits_count": 0,
            "output_png_sha256": {"w4_01_es": "0" * 64},
        },
        ({"CAM999999"}, set(), set()),
    )
    assert ov4 == 0

    _, ov5 = verify_receipt_dict(
        "wave5_non_ithomiini",
        {
            "status": "PASS",
            "locked_cohorts": {
                "locked_cams": 1583,
                "locked_cam_overlap": 0,
                "locked_inat_obs_overlap": 0,
                "locked_inat_photo_overlap": 0,
                "fresh_s3_overlap": 0,
                "w3b_blind_overlap": 0,
                "ref_blind_sha256_overlap": 0,
                "ref_blind_pixel_sha256_overlap": 0,
            },
            "errors": [],
            "output_png_sha256": {"w5_02_es": "0" * 64},
        },
        ({"CAM999999"}, {"111"}, {"222"}),
    )
    assert ov5 == 0

    # 4. Cross-wave species-pair superseding & fail-closed conflict checks
    held_east = {"routes": [{"status": "held_sex_confounded_small_n", "card_id": None, "side_andes": ["East"]}]}
    active_east = {"routes": [{"status": "active", "card_id": "w5_02", "side_andes": ["East"]}]}
    cryptic_east = [{"status": "not_reliably_separable_in_photos", "card_id": None, "side_andes": ["East"]}]

    assert (
        resolve_species_pair_collision(
            ("nessaea hewitsonii", "nessaea obrinus"), "w3a_03", held_east, "w5_02", active_east["routes"]
        )
        == "supersede_prev"
    )
    assert (
        resolve_species_pair_collision(
            ("morpho achilles", "morpho helenor"), "w3a_01", held_east, "w5_01", cryptic_east
        )
        == "supersede_prev"
    )
    for conflict_label, prev_p, new_r in [
        ("active_vs_active", active_east, [{"status": "active", "card_id": "w6_99", "side_andes": ["East"]}]),
        ("active_vs_null", active_east, cryptic_east),
        ("disjoint_region_hold", held_east, [{"status": "active", "card_id": "w5_99", "side_andes": ["West"]}]),
    ]:
        try:
            resolve_species_pair_collision(("a", "b"), "prev_id", prev_p, "new_id", new_r)
        except RuntimeError:
            pass
        else:
            raise AssertionError(f"Self-test failed: {conflict_label} was unexpectedly accepted")

    # 5. Subspecies aliases: the same normal form as the router, broken ones repaired, unreachable sides refused
    for raw, want in [
        ("gudruna?", "gudruna"),
        ("f. ithomiola", "ithomiola"),
        ("  Heliconius  cydno\u00a0F. White ", "heliconius cydno white"),
        ("ssp. cyrbia", "cyrbia"),
        ("Heliconius erato subsp. cyrbia ?", "heliconius erato cyrbia"),
        ("Hypoleria alema forma", "hypoleria alema forma"),
    ]:
        assert norm_subspecies(raw) == want, (raw, norm_subspecies(raw))
    assert clean_aliases(["gudruna?"]) == ["gudruna"]
    assert clean_aliases(["deceptus", "deceptus- phasianita", "f. intermedia", "intermedia", "phasianita"]) == [
        "deceptus",
        "phasianita",
        "intermedia",
    ]
    assert clean_aliases(["antonia", "ssp-f"]) == ["antonia"]
    assert clean_aliases(["semifulva", "ssp. nov."]) == ["semifulva"]
    assert clean_aliases(["semifulva", "ssp f 1"], ["satura", "Semifulva"]) == ["semifulva", "satura"]

    def one_pair(aliases_a: list[str], aliases_b: list[str], status: str = "active") -> dict[str, Any]:
        return {
            "x1": {
                "routing_mode": "subspecies_and_side",
                "species_a": "Eresia eunice",
                "species_b": "Eresia pelonia",
                "routes": [
                    {"card_id": "x1", "status": status, "subspecies_a_aliases": aliases_a, "subspecies_b_aliases": aliases_b}
                ],
            }
        }

    head = [{"eresia eunice": {"gudruna", "eunice"}, "eresia pelonia": {"ithomiola", "pelonia"}}]
    ok, counted = alias_problems(one_pair(["gudruna"], ["Eresia pelonia ithomiola", "calonia"]), head)
    assert ok == [] and counted == {"reachable": 2, "no_head_subspecies": 0}, (ok, counted)
    # A species the head does not split is routed by region: nothing to reach.
    ok, counted = alias_problems(one_pair(["gudruna"], []), [{"eresia eunice": {"gudruna"}}])
    assert ok == [] and counted == {"reachable": 1, "no_head_subspecies": 1}, (ok, counted)
    # No leaf list on this host: the broken strings are still refused, the sides are not counted.
    ok, counted = alias_problems(one_pair(["nothing the head names"], ["pelonia"]), None)
    assert ok == [] and counted == {"reachable": 0, "no_head_subspecies": 0}, (ok, counted)
    for label, bad_pairs, bad_head in [
        ("question_mark", one_pair(["gudruna?"], ["pelonia"]), None),
        ("rank_marker_left", one_pair(["gudruna"], ["f. ithomiola"]), None),
        ("unnamed_form", one_pair(["gudruna", "ssp-f"], ["pelonia"]), None),
        ("unnamed_form_spaced", one_pair(["gudruna", "ssp f 1"], ["pelonia"]), None),
        ("undescribed", one_pair(["gudruna", "nov."], ["pelonia"]), None),
        ("two_names_run_together", one_pair(["gudruna"], ["pelonia- ithomiola"]), None),
        ("broken_on_a_held_route", one_pair(["gudruna?"], ["pelonia"], "inseparable_in_photos"), None),
        ("colour_forms_only", one_pair(["white", "yellow"], ["pelonia"]), head),
        ("no_alias_at_all", one_pair([], ["pelonia"]), head),
        ("reachable_under_one_name_column_only", one_pair(["gudruna"], ["pelonia"]), [*head, {"eresia eunice": {"eunice"}}]),
    ]:
        found, _ = alias_problems(bad_pairs, bad_head)
        if not found:
            raise AssertionError(f"Self-test failed: {label} was unexpectedly accepted")
    for add_key in ALIAS_ADDITIONS:
        assert len(add_key) == 3 and add_key[2] in ("a", "b"), add_key

    print("SELF-TEST PASS (20/20 receipt & cross-wave superseding checks, 25/25 subspecies alias checks)")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=DEFAULT_OUT_DIR,
        help=f"Destination public bundle folder (default: {DEFAULT_OUT_DIR})",
    )
    parser.add_argument(
        "--wave",
        action="append",
        type=parse_wave_arg,
        help="Override wave list (repeatable): wave_id:dir_path:regional_map_filename",
    )
    parser.add_argument(
        "--extra-wave",
        action="append",
        type=parse_wave_arg,
        default=[],
        help="Append an extra wave (e.g. Wave 4): wave_id:dir_path:regional_map_filename",
    )
    parser.add_argument("--quality", type=int, default=92, help="WebP quality (default: 92)")
    parser.add_argument(
        "--check",
        action="store_true",
        help="Verify inputs, SHA-256 and the subspecies aliases without writing files; fails without the head's leaf list",
    )
    parser.add_argument(
        "--leaf-map",
        type=Path,
        default=HEAD_LEAF_MAP,
        help=f"The head's leaf list, for the reachability check (default: {HEAD_LEAF_MAP}). A build skips the "
        "check when the default is absent; --check, or a list named here that is absent, fails",
    )
    parser.add_argument("--self-test", action="store_true", help="Run fail-closed verifier self-tests and exit")
    args = parser.parse_args()

    if args.self_test:
        run_self_tests()
        return 0

    waves = list(args.wave) if args.wave else list(DEFAULT_WAVES)
    waves.extend(args.extra_wave)

    manifest = build_bundle(
        waves=waves, out_dir=args.out_dir, quality=args.quality, check_only=args.check, leaf_map=args.leaf_map
    )
    c = manifest["counts"]
    v = manifest["verification"]
    print(
        f"OK confirmar visual bundle ({'check-only' if args.check else args.out_dir}): "
        f"{c['total_pairs']} pairs, {c['active_cards']} active cards ({c['assets_total']} WebP assets, "
        f"{c['webp_bytes'] / 1e6:.2f} MB vs {c['source_png_bytes'] / 1e6:.2f} MB PNG), "
        f"{c['non_active_pairs']} non-active pairs, {c['deferred_routes']} deferred routes, "
        f"locked_overlap={v['locked_or_blind_overlap']}"
    )
    if v["head_subspecies_checked"]:
        print(
            f"OK subspecies aliases: {v['subspecies_sides_reachable']} active sides reachable by a subspecies the head "
            f"names, {v['subspecies_sides_without_head_subspecies']} of species the head does not split; "
            "no question mark, rank marker or unnamed form left"
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
