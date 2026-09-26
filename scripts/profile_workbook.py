#!/usr/bin/env python3
"""Aggregate an XLSX workbook without exporting private specimen or contact rows.

Uses only the standard library. Outputs JSON and Markdown beside the supplied
workbook by default. Cached formula values are inspected, never recalculated.
"""

from __future__ import annotations

import argparse
from collections import Counter, defaultdict
from datetime import datetime, timedelta
import json
from pathlib import Path
import re
from xml.etree import ElementTree as ET
from zipfile import ZipFile

N = "{http://schemas.openxmlformats.org/spreadsheetml/2006/main}"
R = "{http://schemas.openxmlformats.org/officeDocument/2006/relationships}"
P = "{http://schemas.openxmlformats.org/package/2006/relationships}"
MISSING = {"", "NA", "N/A", "NONE", "NULL", "BLANK", "NOT_COLLECTED", "#N/A"}
ID_RE = re.compile(r"^[0-9][A-ZÑ]{2}$")
NEW_ID_RE = re.compile(r"^[A-ZÑ][0-9][A-ZÑ]$")
CAM_RE = re.compile(r"^CAM\d+$", re.I)
LETTERS = "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ"
ORDER = {letter: i for i, letter in enumerate(LETTERS)}
HEADER_ROWS = {"Crosses_Lys_x_Pol": 6}
RECORD_FIELDS = {
    "Pheromones_data": ("CAM_ID",),
    "Melinaea_crosses": ("Female", "Male"),
    "Melinaea_eggs": ("Mother ID", "Father ID", "Tube_ID"),
    "Crosses_Lys_x_Pol": ("female Id", "male Id"),
    "Stocks_Matings": ("male_ID", "female_ID"),
    "Insectary_stocks": ("CLUTCH NUMBER", "SPECIES"),
    "Location_data": ("Collection_location",),
}


def colnum(ref: str) -> int:
    n = 0
    for ch in re.match(r"[A-Z]+", ref).group():
        n = 26 * n + ord(ch) - 64
    return n


def read_shared(z: ZipFile) -> list[str]:
    if "xl/sharedStrings.xml" not in z.namelist():
        return []
    strings = []
    with z.open("xl/sharedStrings.xml") as stream:
        for _, elem in ET.iterparse(stream, events=("end",)):
            if elem.tag == N + "si":
                strings.append("".join(t.text or "" for t in elem.iter(N + "t")))
                elem.clear()
    return strings


def sheet_files(z: ZipFile) -> list[tuple[str, str, str]]:
    root = ET.fromstring(z.read("xl/workbook.xml"))
    rels = ET.fromstring(z.read("xl/_rels/workbook.xml.rels"))
    targets = {r.get("Id"): r.get("Target") for r in rels.findall(P + "Relationship")}
    result = []
    for item in root.findall(N + "sheets/" + N + "sheet"):
        target = targets[item.get(R + "id")]
        path = target.lstrip("/") if target.startswith("/") else "xl/" + target
        result.append((item.get("name"), item.get("state", "visible"), path))
    return result


def rows(z: ZipFile, path: str, strings: list[str]):
    with z.open(path) as stream:
        for _, elem in ET.iterparse(stream, events=("end",)):
            if elem.tag != N + "row":
                continue
            data = {}
            formulas = set()
            for cell in elem.findall(N + "c"):
                idx = colnum(cell.get("r"))
                f = cell.find(N + "f")
                if f is not None:
                    formulas.add(idx)
                v = cell.find(N + "v")
                inline = cell.find(N + "is")
                if v is not None and v.text is not None:
                    value = strings[int(v.text)] if cell.get("t") == "s" else v.text
                elif inline is not None:
                    value = "".join(t.text or "" for t in inline.iter(N + "t"))
                else:
                    value = ""
                if value != "" or f is not None:
                    data[idx] = value.strip() if isinstance(value, str) else value
            yield int(elem.get("r")), data, formulas
            elem.clear()


def present(value: str | None) -> bool:
    return value is not None and str(value).strip().upper() not in MISSING and not str(value).startswith("#")


def normalize_id(value: str | None) -> str:
    return (value or "").strip().upper()


def date_string(value: str | None) -> str | None:
    if not present(value):
        return None
    try:
        number = float(value)
        if 20000 < number < 80000:
            return (datetime(1899, 12, 30) + timedelta(days=number)).date().isoformat()
    except (TypeError, ValueError, OverflowError):
        pass
    try:
        return datetime.fromisoformat(value).date().isoformat()
    except (TypeError, ValueError):
        return None


def summarize(path: Path) -> dict:
    result = {"source": str(path.resolve()), "sheets": [], "relationships": {}}
    keysets = defaultdict(set)
    id_occurrences = defaultdict(Counter)
    refs = defaultdict(Counter)
    with ZipFile(path) as z:
        strings = read_shared(z)
        for name, state, xmlpath in sheet_files(z):
            header = {}
            header_row = HEADER_ROWS.get(name, 1)
            counts = Counter()
            formcounts = Counter()
            value_counts = defaultdict(Counter)
            dates = defaultdict(list)
            last_real = 0
            first_real = None
            physical = 0
            actual = 0
            only_id = 0
            id_column = "Insectary_ID" if name == "Insectary_data" else "CAM_ID" if name == "Collection_data" else None
            id_format = Counter()
            status_death = Counter()
            status_preservation = Counter()
            for rownum, cells, formulas in rows(z, xmlpath, strings):
                physical += 1
                if rownum == header_row:
                    header = {k: v for k, v in cells.items() if v}
                    continue
                if rownum < header_row:
                    continue
                vals = {str(header[k]).strip(): v for k, v in cells.items() if k in header and str(header[k]).strip()}
                for k, v in cells.items():
                    if present(v):
                        counts[header.get(k, f"column_{k}")] += 1
                for k in formulas:
                    formcounts[header.get(k, f"column_{k}")] += 1
                if name == "Collection_data":
                    # CAM_ID is preallocated. A row needs independent observational content.
                    evidence = ("SPECIES", "Collection_date", "Collection_location", "Sex", "Collector", "Death_date", "Insectary_ID", "FieldMark_ID")
                elif name == "Insectary_data":
                    evidence = ("SPECIES", "Intro2Insectary_date", "Collection_location", "Sex", "Death_date", "Stock_of_origin", "CLUTCH NUMBER")
                else:
                    evidence = RECORD_FIELDS.get(name, ())
                literal = {str(header[k]).strip(): v for k, v in cells.items() if k in header and k not in formulas}
                real = any(present(literal.get(k)) for k in evidence) if evidence else any(present(v) for v in vals.values())
                if real:
                    actual += 1
                    first_real = first_real or rownum
                    last_real = rownum
                elif id_column and present(vals.get(id_column)):
                    only_id += 1
                if id_column:
                    value = normalize_id(vals.get(id_column))
                    if present(value) and real:
                        id_occurrences[name][value] += 1
                        id_format["digit_letter_letter" if ID_RE.fullmatch(value) else "letter_digit_letter" if NEW_ID_RE.fullmatch(value) else "cam" if CAM_RE.fullmatch(value) else "other"] += 1
                        keysets[name].add(value)
                    other_id = "Insectary_ID" if name == "Collection_data" else "CAM_ID_CollData"
                    v = normalize_id(vals.get(other_id))
                    if present(v) and real:
                        refs[(name, other_id)][v] += 1
                    if name == "Collection_data":
                        v = normalize_id(vals.get("CAM_ID_insectary"))
                        if present(v) and real: keysets["Collection_data.CAM_ID_insectary"].add(v)
                    else:
                        v = normalize_id(vals.get("CAM_ID"))
                        if present(v) and real: keysets["Insectary_data.CAM_ID"].add(v)
                if name == "Pheromones_data":
                    v = normalize_id(vals.get("CAM_ID"))
                    if present(v): refs[(name, "CAM_ID")][v] += 1
                for refname in ("Female", "Male", "Mother ID", "Father ID", "male_ID", "female_ID", "Mate_with", "female Id", "male Id"):
                    v = normalize_id(vals.get(refname))
                    if present(v): refs[(name, refname)][v] += 1
                if name in {"Collection_data", "Insectary_data"} and real:
                    for field in ("SPECIES", "Sex", "Release_Collect", "Wild_Reared", "Death_cause", "LIFESTAGE", "Preserved_Dead_Alive", "Preserved_dead_alive"):
                        v = vals.get(field)
                        if present(v): value_counts[field][str(v).strip()] += 1
                    for field in ("Collection_date", "Intro2Insectary_date", "Death_date", "Preservation_date"):
                        d = date_string(vals.get(field))
                        if d: dates[field].append(d)
                    status = vals.get("Preserved_dead_alive" if name == "Collection_data" else "Preserved_Dead_Alive")
                    if present(status):
                        if date_string(vals.get("Death_date")): status_death[status] += 1
                        if date_string(vals.get("Preservation_date")): status_preservation[status] += 1
            sheet = {
                "name": name, "state": state, "physical_rows_including_header": physical, "header_row": header_row,
                "profile_status": "headerless_or_empty" if not header or not counts else "profiled",
                "active_rows": actual, "first_active_row": first_real, "last_active_row": last_real,
                "preallocated_id_only_rows": only_id,
                "headers": [{"column": k, "name": v} for k, v in sorted(header.items())],
                "populated_cells_by_field": dict(counts), "formula_cells_by_field": dict(formcounts),
            }
            if id_column:
                duplicates = {k: v for k, v in id_occurrences[name].items() if v > 1}
                valid_id = ID_RE if name == "Insectary_data" else CAM_RE
                valid = {k: v for k, v in id_occurrences[name].items() if valid_id.fullmatch(k)}
                valid_dupes = {k: v for k, v in valid.items() if v > 1}
                new_ids = {k: v for k, v in id_occurrences[name].items() if NEW_ID_RE.fullmatch(k)} if name == "Insectary_data" else {}
                top = {field: counter.most_common(15) for field, counter in value_counts.items()}
                sheet.update({"id_column": id_column, "id_formats": dict(id_format),
                              "unique_active_ids": len(id_occurrences[name]),
                              "duplicate_active_id_count": len(duplicates),
                              "duplicate_active_id_occurrences": sum(v - 1 for v in duplicates.values()),
                              "valid_format_unique_ids": len(valid),
                              "valid_format_duplicate_values": len(valid_dupes),
                              "valid_format_rows": sum(valid.values()),
                              "nonstandard_active_id_rows": sum(id_occurrences[name].values()) - sum(valid.values()),
                              "new_letter_digit_letter_unique_ids": len(new_ids),
                              "new_letter_digit_letter_max": max(new_ids, key=lambda x: (ORDER[x[2]], ORDER[x[0]], int(x[1])), default=None),
                              "active_rows_without_id": actual - sum(id_occurrences[name].values()),
                              "three_char_max_spanish_order": max((k for k in id_occurrences[name] if ID_RE.fullmatch(k)), key=lambda x: (ORDER[x[1]], ORDER[x[2]], int(x[0])), default=None),
                              "top_values": top, "preservation_status_with_death_date": dict(status_death),
                              "preservation_status_with_preservation_date": dict(status_preservation),
                              "dates": {field: {"min": min(ds), "max": max(ds), "count": len(ds)} for field, ds in dates.items() if ds}})
            result["sheets"].append(sheet)
    for (name, field), counter in refs.items():
        if name == "Insectary_data" and field == "CAM_ID_CollData":
            target = "Collection_data.CAM_ID_insectary"
        elif name == "Pheromones_data" and field == "CAM_ID":
            target = "Collection_data + Insectary_data.CAM_ID"
            keysets[target] = keysets["Collection_data"] | keysets["Insectary_data.CAM_ID"]
        elif name == "Collection_data" and field == "Insectary_ID":
            target = "Insectary_data"
        else:
            target = "Insectary_data"
        result["relationships"][f"{name}.{field} -> {target}"] = {
            "nonempty_references": sum(counter.values()),
            "unique_references": len(counter),
            "unique_matching_primary_ids": len(set(counter) & keysets[target]),
            "unique_unmatched": len(set(counter) - keysets[target]),
        }
    result["sheet_count"] = len(result["sheets"])
    return result


def markdown(report: dict) -> str:
    lines = ["# Workbook profile", "", "Aggregates only. Formula caches are not recalculated. Main-sheet active rows require a non-empty, non-formula species, sex, location, event date, stock/clutch or related field; IDs, computed fields and photos alone do not qualify. These are evidence-bearing rows, not confirmed distinct biological specimens. Experiment sheets use literal participant or record keys. Headerless/empty sheets are marked unprofiled; other sheets may include formula-only rows.", "", "| Sheet | State | Profile | Physical rows | Active rows | ID-only rows | Formulas |", "|---|---|---|---:|---:|---:|---:|"]
    for s in report["sheets"]:
        lines.append(f'| {s["name"]} | {s["state"]} | {s["profile_status"]} | {s["physical_rows_including_header"]} | {s["active_rows"] if s["profile_status"] == "profiled" else "n/a"} | {s["preallocated_id_only_rows"]} | {sum(s["formula_cells_by_field"].values())} |')
    for s in report["sheets"]:
        if s["name"] not in {"Collection_data", "Insectary_data", "Insectary_stocks", "Pheromones_data", "Melinaea_crosses", "Melinaea_eggs", "Crosses_Lys_x_Pol", "Hybrid_Attempts", "Stocks_Matings", "Location_data", "Lists"}:
            continue
        lines += ["", f'## {s["name"]}', "", "Columns: " + ", ".join(f'{h["column"]} {h["name"]}' for h in s["headers"]), ""]
        if "id_column" in s:
            lines.append(f'Active IDs: {s["unique_active_ids"]} unique; {s["duplicate_active_id_count"]} duplicate values; {s["active_rows_without_id"]} active rows without ID; max digit-letter-letter ID by Spanish Ñ order: {s["three_char_max_spanish_order"]}; later letter-digit-letter IDs: {s["new_letter_digit_letter_unique_ids"]} unique, max {s["new_letter_digit_letter_max"]}.')
            lines.append("Date ranges: " + json.dumps(s["dates"], ensure_ascii=False))
            lines.append("Top controlled values: " + json.dumps(s["top_values"], ensure_ascii=False))
            lines.append("Preservation status with recorded death date: " + json.dumps(s["preservation_status_with_death_date"], ensure_ascii=False) + ". This status is not a current alive census.")
        lines.append("Formula columns: " + json.dumps(s["formula_cells_by_field"], ensure_ascii=False))
    lines += ["", "## Reference coverage", ""]
    for key, data in report["relationships"].items(): lines.append(f'- {key}: {data}')
    return "\n".join(lines) + "\n"


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("workbook", type=Path)
    parser.add_argument("--outdir", type=Path)
    args = parser.parse_args()
    outdir = args.outdir or args.workbook.parent
    outdir.mkdir(parents=True, exist_ok=True)
    report = summarize(args.workbook)
    (outdir / "workbook-profile.json").write_text(json.dumps(report, ensure_ascii=False, indent=2) + "\n")
    (outdir / "workbook-profile.md").write_text(markdown(report))
    print(f'Profiled {report["sheet_count"]} sheets; reports in {outdir}')


if __name__ == "__main__":
    main()
