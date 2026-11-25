#!/usr/bin/env python3
"""Build the unified Confirmar visual field-ID card bundle for /identificar/.

Ingests verified Wave 2 Regional, Wave 3A (D+V), and Wave 3B (Non-Ithomiini)
regional routing tables + ES/EN card PNGs (plus optional future waves via --wave),
verifies every wave's AUDIT_RECEIPT.json fail-closed (status=PASS, explicit zero
locked/blind overlap fields required, exact source PNG SHA-256 match), optimizes
crisp full-resolution WebP copies for phone zoom legibility, and writes:
  - frontend/public/identificar/confirmar/confirmar_visual_manifest.json
  - frontend/public/identificar/confirmar/cards/{es,en}/<card_id>.webp

Re-runnable cleanly whenever Wave 3A/3B sol-loop updates a PNG or Wave 4 adds cards.
Deterministic output (no wall-clock timestamp) so clean re-runs produce zero diff.
"""
from __future__ import annotations

import argparse
import hashlib
import io
import json
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
]

LOCKED_AUDIT_SCRIPT_DIR = WINGS_REPORTS / "confirmar_visual_wave1_20261123" / "scripts"

ALLOWED_NON_ACTIVE_STATUSES = {
    "no_unlocked_museum_pair",
    "inseparable_in_photos",
    "not_reliably_separable_in_photos",
    "held_sex_confounded_small_n",
    "held_suspect_reference_labels",
    "inseparable_or_unverified_refs",
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
    for cross_key in ("locked_or_blind_or_curation_overlap", "leakage_or_integrity_errors"):
        if cross_key in receipt:
            measured_overlap += _require_zero_int(receipt, cross_key, f"[{wave_id}] AUDIT_RECEIPT")

    receipt_locked = receipt.get("locked_cohorts")

    if isinstance(receipt_locked, dict):
        if wave_id in KNOWN_WAVE_REQUIRED_OVERLAP_KEY:
            raise RuntimeError(f"[{wave_id}] Unexpected locked_cohorts schema for museum wave")
        # Wave 3B structured schema: every overlap counter + errors list is mandatory
        for key in (
            "locked_cam_overlap",
            "locked_inat_obs_overlap",
            "locked_inat_photo_overlap",
            "ref_blind_sha256_overlap",
        ):
            measured_overlap += _require_zero_int(receipt_locked, key, f"[{wave_id}] locked_cohorts")
        if "errors" not in receipt:
            raise RuntimeError(f"[{wave_id}] AUDIT_RECEIPT must have empty list 'errors'")
        locked_cams_n = receipt_locked.get("locked_cams")
        if isinstance(locked_cams_n, bool) or not isinstance(locked_cams_n, int) or locked_cams_n < 1583:
            raise RuntimeError(f"[{wave_id}] locked_cohorts.locked_cams must be >= 1583, got {locked_cams_n!r}")
    else:
        if wave_id == "wave3b_non_ithomiini":
            raise RuntimeError(f"[{wave_id}] Missing required 'locked_cohorts' object")
        # Wave 2 / Wave 3A museum schema: explicit leakage_hits + exact overlap/integrity counter mandatory
        if "leakage_hits" not in receipt:
            raise RuntimeError(f"[{wave_id}] AUDIT_RECEIPT must have empty list 'leakage_hits'")
        required_key = KNOWN_WAVE_REQUIRED_OVERLAP_KEY.get(wave_id)
        if required_key:
            _require_zero_int(receipt, required_key, f"[{wave_id}] AUDIT_RECEIPT")
        else:
            overlap_keys = [
                k for k in ("locked_or_blind_or_curation_overlap", "leakage_or_integrity_errors") if k in receipt
            ]
            if not overlap_keys:
                raise RuntimeError(
                    f"[{wave_id}] AUDIT_RECEIPT missing required zero-overlap field "
                    "('locked_or_blind_or_curation_overlap' or 'leakage_or_integrity_errors')"
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


def detect_wave_routing_mode(reg_map: dict[str, Any]) -> str:
    """Detect whether a wave's regional map routes by side-only (Wave 3B) or subspecies+side (Wave 2/3A)."""
    purpose = str(reg_map.get("purpose", "")).lower()
    lookup = " ".join(str(x).lower() for x in (reg_map.get("routing_contract", {}).get("lookup_order") or []))
    if "subspecies" in lookup or "subspecies" in purpose:
        return "subspecies_and_side"
    return "side_only"


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
) -> dict[str, Any]:
    locked_cohorts = load_optional_locked_cohorts()

    unified_pairs: dict[str, Any] = {}
    seen_card_ids: set[str] = set()
    seen_species_pairs: dict[tuple[str, str], str] = {}
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
        routing_mode = detect_wave_routing_mode(reg_map)
        rendered_es = load_rendered_text(wave_dir, "es")
        rendered_en = load_rendered_text(wave_dir, "en")

        pairs_obj = reg_map.get("pairs")
        if not isinstance(pairs_obj, dict) or not pairs_obj:
            raise RuntimeError(f"[{wave_id}] Empty or invalid pairs in {map_path}")

        wave_active_cards = 0
        wave_non_active_pairs = 0

        for pair_id, pair in pairs_obj.items():
            if pair_id in unified_pairs:
                raise RuntimeError(f"Duplicate pair_id {pair_id!r} across waves")

            species_a = str(pair["species_a"]).strip()
            species_b = str(pair["species_b"]).strip()
            sp_key = norm_species_pair(species_a, species_b)
            if sp_key in seen_species_pairs:
                raise RuntimeError(
                    f"Duplicate species pair {sp_key!r}: {pair_id!r} conflicts with {seen_species_pairs[sp_key]!r}"
                )
            seen_species_pairs[sp_key] = pair_id

            routes_out: list[dict[str, Any]] = []
            pair_has_active = False

            for route in pair.get("routes", []):
                status = str(route["status"]).strip()
                card_id = route.get("card_id")

                route_entry: dict[str, Any] = {
                    "card_id": card_id,
                    "status": status,
                    "side_andes": list(route.get("side_andes", [])),
                    "side_default_for": list(route.get("side_default_for", [])),
                    "region_bucket": str(route.get("region_bucket", "both")),
                    "region_es": str(route["region_es"]).strip(),
                    "region_en": str(route["region_en"]).strip(),
                    "taxon_a": str(route.get("taxon_a") or species_a).strip(),
                    "taxon_b": str(route.get("taxon_b") or species_b).strip(),
                    "subspecies_a_aliases": [str(x).strip() for x in route.get("subspecies_a_aliases", []) if str(x).strip()],
                    "subspecies_b_aliases": [str(x).strip() for x in route.get("subspecies_b_aliases", []) if str(x).strip()],
                }
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

            unified_pairs[pair_id] = {
                "pair_id": pair_id,
                "wave": wave_id,
                "routing_mode": routing_mode,
                "species_a": species_a,
                "species_b": species_b,
                "classification": str(pair.get("classification", "single_region")),
                "default_card_id": pair.get("default_card_id"),
                "routes": routes_out,
            }

        wave_summaries.append(
            {
                "wave": wave_id,
                "routing_mode": routing_mode,
                "map_version": reg_map.get("version"),
                "pairs": len(pairs_obj),
                "active_cards": wave_active_cards,
                "non_active_pairs": wave_non_active_pairs,
                "audit_status": receipt.get("status"),
            }
        )

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
            "deferred_routes": total_deferred_routes,
            "assets_total": total_active_cards * 2,
            "source_png_bytes": total_src_png_bytes,
            "webp_bytes": total_webp_bytes,
        },
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
    print("SELF-TEST PASS (10/10 negative & positive receipt checks)")


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
    parser.add_argument("--check", action="store_true", help="Verify inputs & SHA-256 without writing files")
    parser.add_argument("--self-test", action="store_true", help="Run fail-closed verifier self-tests and exit")
    args = parser.parse_args()

    if args.self_test:
        run_self_tests()
        return 0

    waves = list(args.wave) if args.wave else list(DEFAULT_WAVES)
    waves.extend(args.extra_wave)

    manifest = build_bundle(waves=waves, out_dir=args.out_dir, quality=args.quality, check_only=args.check)
    c = manifest["counts"]
    print(
        f"OK confirmar visual bundle ({'check-only' if args.check else args.out_dir}): "
        f"{c['total_pairs']} pairs, {c['active_cards']} active cards ({c['assets_total']} WebP assets, "
        f"{c['webp_bytes'] / 1e6:.2f} MB vs {c['source_png_bytes'] / 1e6:.2f} MB PNG), "
        f"{c['non_active_pairs']} non-active pairs, {c['deferred_routes']} deferred routes, "
        f"locked_overlap={manifest['verification']['locked_or_blind_overlap']}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
