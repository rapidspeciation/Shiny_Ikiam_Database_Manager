# Monitoring, mark–release, recaptures, weather

The Ikiam trail (1.11 km, sections T1 by the campus to T4) is walked 4 days a
month, mid-month, every other day, 09:00–11:00, one walker per day; the
walkers alternate (in 2026 one person took two of the days, another the other
two). Walks are imported
with `queue_wikiloc` / `get_walk` (the tool parses the notes, weather, marks,
sections and the checks below). Full detail: `docs/monitoring.md`.

## What goes where

- Every capture → one Collection_data row: Purpose `Monitoring`,
  Collection_location `Ikiam`, Identifier = the walker (= Collector),
  Collection_time, Flight_height (m, at first sight), Cloud_cover per row,
  Rainfall per day, Transect_section 1–4 (not typed since Apr 2026; it is
  filled from the Wikiloc track: the app derives it from the GPS point).
  Coordinates of a capture have no column yet: **Ask** PAS.
- All Ithomiini **and** their tiger-pattern mimics (Heliconius numata and
  others) are recorded; the 30 rule below applies to Ithomiini only.
- Each walker and day → one SamplingDay_data row: Date · Location `Ikiam` ·
  Purpose `Monitoring` · Start_time · End_time (the GPX's first and last
  times; standard 09:00–11:00; `NA` if unknown) · Collectors_initials
  (initials only, several joined by `|`: `AA|FCH`) · Notes (`d/m/yy INI:` rain,
  fallen trees, "No butterflies collected"). Days without captures still get
  their row. After a monitoring import, check the row exists for every
  walker and day.

## The two fates

**Preserved** (`Collected_Preserved`): the template of a field-preserved
butterfly ([field-collections.md](field-collections.md)) with FieldMark_ID
`NA`, Death_date = Preservation_date = the walk day, weight measured later in
the lab (g, 3 decimals: leave it for the lab).

**Marked and released** (`Mark_Released`), the rows of Oct 2025–Aug 2026:
FieldMark_ID = the mark · CAM_ID, CAM_ID_insectary, Insectary_ID, Tube_1–4
`NA` · Tube_1–3 tissues `NA` · Butterfly_weight, Death_date, Preservation_date
`NA` · Preservation_medium and Preserved_dead_alive `NOT_PRESERVED` ·
Location_Head…_wings `NA`, Location_WholeBody blank · Splitted_body `No`.
The rows drifted (tissues `NOT_COLLECTED` Oct 2025–Jan 2026 and again in Sep
2026; medium `NOT_COLLECTED` in some months and in Sep 2026; Death_date = walk
day in May 2026; Splitted_body `NA` Nov 2025–Apr 2026): propose this template,
mention a deviation, never mass-correct old rows.

**Released unmarked** (`Released_Unmarked`): as marked, FieldMark_ID `NA`,
Splitted_body `NA`. Rare (last Sep 2025): **Ask** AA when it is used.

## Marks

- Written on the ventral right wing with a permanent marker. Series: `M0`–`M99`
  (to Aug 2025), then `A1`–`A99`, then `B1`… (B73 on 26 Sep 2026). Next mark =
  highest of the current series + 1; after 99 the next letter.
- In Aug 2026 the numbering restarted at B40 by mistake, so B40–B59 each belong
  to two butterflies: a mark alone is not unique; use mark + species + sex.
- A mark recorded on another species is a conflict (mistyped mark or species),
  not a recapture: ask the walker.
- In a Wikiloc waypoint note (`M1 Hyposcada illinissa ida hembra 9:20 0.5m NO id: B69`)
  the leading `M1` is the capture's order in the walk, **not** a mark; the mark
  is after `id`. No mark → preserved. Some walkers write only the mark and leave
  the species for the photo.
- Before 2023 and on expeditions FieldMark_ID held collectors' field numbers
  (CR12, PAS16, TP5_3, LTS000009): not marks. Old "K12"-style codes (place
  letter + number) are superseded.

## Recaptures

- A recapture is a **new Mark_Released row** with the same FieldMark_ID,
  species and sex on a later date (note "Recapture" optional). A mark above the
  last one handed out is a new butterfly, even if the number was used long ago.
- Recaptures written only in notes (mostly 2024: "recatch&realease transect=4…")
  stay as notes: the team decided not to turn them into rows.

## The 30-preserved rule

**In force.** Once an Ithomiini species has 30 preserved individuals from
Ikiam, Casa de Lin (Lin's house) and Mariposario Ikiam together, the team
stops preserving it there: further captures are marked and released (meeting
of Nov 2023; the 2023 method says at most 30 per species). Count every
`Collected_Preserved` row from those three places, whatever its Purpose
(`count_records`). The team has not been checking the counts (H. euclea,
H. anchiala, P. florula, O. tigilla went past 30): when you propose a
preserved capture of a species already at 30, or when asked what to do with
one, say it should be marked and released.

## Weather codes

List values (Collection_data): Rainfall `DY_(dry)`, `DZ_(drizzle)`,
`WR_(weak_rain)`, `SR_(strong_rain)`, `NA`; Cloud_cover `S_(cloudless_sunny)`,
`S&C_(sun_&_cloud_patches)`, `CL_(cloudy_light)`, `CD_(cloudy_dark)`, `NA`
(check with `describe_sheet`).

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

`NO` means overcast in a cloud column but "no rain" in a rain column: read it
by column. The 2022 paper codes were rain `LF/LD/LV/S` and cloud
`NO/NC/SyN/DS`, the list's four values each in the same order, hence `LF` =
strong rain. `CN` was typed `CL_(cloudy_light)` (the wild lines "11:23 Naty
CN" and "10:30 FCH NC" of 7 Nov 2023 are both CL). A bare `C` has no known
mapping: ask the walker. Trap envelopes use `Sol / Parches / NC` the same way.
