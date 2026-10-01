---
name: monitoring
description: Butterfly monitoring on the Ikiam transects T1–T4 — a Wikiloc walk turned into Collection_data rows (queue_wikiloc, get_walk), a monitoring capture recorded by hand, field marks (M/A/B series) and recaptures, the 30-preserved rule, weather codes (paper and Wikiloc shorthand → list values), SamplingDay_data. Use it when the person sends a Wikiloc link, asks about a walk, a mark or a recapture, whether a species should be preserved or marked, or how monitoring data are recorded.
---

# Monitoring (Ikiam transects)

The Ikiam trail (1.11 km; section T1 by the campus to T4) is walked 4 days a
month, one walker per day, who records each butterfly as a Wikiloc waypoint
with a photo and a note (species, sex, time, height, weather, mark). The
project documentation's `monitoring.md` has the app's full rules.

## A Wikiloc walk

1. `queue_wikiloc` → `get_walk` → one `propose_changes` with its `newRows` →
   the person confirms → `apply_proposal`.
2. Check that SamplingDay_data has the walker's row for that day (below).

The person can also import the walk in Monitoreo → Importar, which hands out
the next CAM and tube for preserved points; a GPX file uploaded there also
keeps the GPS times.

## A capture by hand (no Wikiloc)

Check a mark first (`find_records` on FieldMark_ID). Then one new
Collection_data row: the template of its fate (below) plus Collection_date,
Collection_time, Transect_section, Collector and Identifier (the walker, as
the Lists value), SPECIES / Subspecies_Form / Sex, Rainfall, Cloud_cover,
Flight_height, and the mark (marked) or CAM and tube (preserved, only as
given).

## What goes where

- **Collection_data**, one row per capture: Purpose `Monitoring` (an
  opportunistic catch at Ikiam has Purpose `NA`), Collection_location `Ikiam`,
  Identifier = Collector = the walker, Collection_time, Flight_height (m),
  Cloud_cover per row, Rainfall per day, Transect_section 1–4.
- **Transect_section**: the walkers have left it blank since Apr 2026;
  `get_walk` and the import take it from the point's GPS position, and
  `list_suggested_edits` (source `wikiloc-transects`) suggests the missing
  ones.
- **Which butterflies**: all Ithomiini **and** their tiger-pattern mimics
  (Heliconius numata and others); the 30 rule applies to Ithomiini only.
- **SamplingDay_data**, one row per walker and day, also on days without
  captures: Date · Location `Ikiam` · Purpose `Monitoring` · Start_time ·
  End_time (from a GPX; `NA` if unknown) · Collectors_initials (several
  joined by `|`: `AA|FCH`) · Notes (rain, fallen trees, "No butterflies
  collected").

## The fates (templates)

| Fate | Release_Collect | Values |
|---|---|---|
| Preserved | `Collected_Preserved` | the field-preserved template (skill `data-rules`, field collections) with FieldMark_ID `NA` and Death_date = Preservation_date = the walk day; the weight is measured later in the lab (leave it) |
| Marked and released | `Mark_Released` | FieldMark_ID = the mark · CAM_ID, CAM_ID_insectary, Insectary_ID, Tube_1–4, Butterfly_weight, Death_date, Preservation_date, Location_Head…_wings `NA` · Preserved_dead_alive `NOT_PRESERVED` · Splitted_body `No` |
| Released unmarked | `Released_Unmarked` | as marked, with FieldMark_ID `NA`. Rare (last used Sep 2025). Not settled when it applies — ask AA |

Marked and released, tube tissues and Preservation_medium: they have switched
back and forth between `NA`/`NOT_PRESERVED` and `NOT_COLLECTED` (by the same
people); the app's import writes `NOT_COLLECTED`, like the Sep 2026 rows. Not
settled — ask AA; until then follow the import and never mass-correct old
rows.

## Marks

- Written on the ventral right wing. Series: `M0`–`M99` (to Aug 2025), then
  `A1`–`A99`, then `B1`… (B73 on 26 Sep 2026). Next mark = the highest of the
  current series + 1; after 99, the next letter.
- In Aug 2026 the numbering restarted at B40 by mistake, so B40–B59 each
  belong to two butterflies: a mark alone is not unique; use mark + species +
  sex.
- A mark recorded on another species is a conflict (a mistyped mark or
  species), not a recapture: ask the walker.
- In a waypoint note (`M1 Hyposcada illinissa ida hembra 9:20 0.5m NO id:
  B69`) the leading `M1` is the capture's order in the walk, **not** a mark;
  the mark comes after `id`. No mark → preserved. Some walkers write only the
  mark and leave the species for the photo.
- Before 2023 and on expeditions FieldMark_ID held collectors' field numbers
  (CR12, PAS16, TP5_3, LTS000009): not marks.

## Recaptures

- A recapture is a **new `Mark_Released` row** with the same FieldMark_ID,
  species and sex on a later date (a "Recapture" note is optional). The sheet
  has such rows since Aug 2025, and every recapture since Jul 2026 is one.
- Before that, some recaptures were written only in the marking row's note
  (2024 to May 2026: "recatch&realease transect=4…", "Recapture …"). They stay
  notes (decided Sep 2026); the app shows them in Monitoreo → Recapturas.
- A mark above the last one handed out is a new butterfly, even if the number
  was used long ago.

## The 30-preserved rule

- In force: once an Ithomiini species has 30 preserved individuals from
  Ikiam, Casa de Lin and Mariposario Ikiam together (every
  `Collected_Preserved` row from those places, whatever its Purpose), further
  captures there are marked and released.
- `get_alerts` gives the counts, the day each species reached 30 and those
  close to it.
- Several species went past 30 unnoticed: when you propose a preserved
  capture of a species already at 30, or are asked what to do with one, say
  it should be marked and released.

## Weather codes

List values: Rainfall `DY_(dry)`, `DZ_(drizzle)`, `WR_(weak_rain)`,
`SR_(strong_rain)`, `NA`; Cloud_cover `S_(cloudless_sunny)`,
`S&C_(sun_&_cloud_patches)`, `CL_(cloudy_light)`, `CD_(cloudy_dark)`, `NA`.

| Written on paper / in Wikiloc | Column | Value |
|---|---|---|
| `sol`, `DS` / "Despejado" | Cloud_cover | `S_(cloudless_sunny)` |
| `parches`, `SyN` (sol y nubes) | Cloud_cover | `S&C_(sun_&_cloud_patches)` |
| `NC`, `N-C`, `N.C`, `CN` (nublado claro) | Cloud_cover | `CL_(cloudy_light)` |
| `NO`, `N-O` (nublado oscuro) | Cloud_cover | `CD_(cloudy_dark)` |
| nothing about rain, `S` (seco), `NO` in a rain column | Rainfall | `DY_(dry)` |
| `llovizna`, `LV` | Rainfall | `DZ_(drizzle)` |
| `LD` (lluvia débil), "lluvia" (inferred: say so) | Rainfall | `WR_(weak_rain)` |
| `LF` (lluvia fuerte; deduced: say so) | Rainfall | `SR_(strong_rain)` |
| `ND`, "no data" | either | `NA` |

- `NO` is overcast in a cloud column but "no rain" in a rain column.
- The 2022 paper codes were rain `LF/LD/LV/S` and cloud `NO/NC/SyN/DS`, the
  list's four values in the same order (hence `LF` = strong rain).
- A bare `C` has no known mapping: ask the walker.
- Trap envelopes use `Sol / Parches / NC` the same way.
