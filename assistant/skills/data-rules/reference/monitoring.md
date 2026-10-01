# Monitoring, mark–release, recaptures, weather

The Ikiam trail (1.11 km, sections T1 by the campus to T4) is walked monthly,
about 4 days mid-month, one walker per day, 09:00–11:00. Walks are imported
with `queue_wikiloc` / `get_walk` (the tool parses the notes, weather, marks,
sections and the checks below). Full detail: `docs/monitoring.md`.

## What goes where

- Every capture → one Collection_data row: Purpose `Monitoring`,
  Collection_location `Ikiam`, Identifier = the walker (= Collector),
  Collection_time, Flight_height (m, at first sight), Cloud_cover per row,
  Rainfall per day, Transect_section 1–4 (not typed since Apr 2026; the app
  derives it from the GPS point).
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

**Marked and released** (`Mark_Released`): FieldMark_ID = the mark · CAM_ID,
CAM_ID_insectary, Insectary_ID, Tube_1–4 `NA` · tissues `NOT_COLLECTED` ·
Butterfly_weight, Death_date, Preservation_date `NA` · Preservation_medium and
Preserved_dead_alive `NOT_PRESERVED` · Location_* `NA` · Splitted_body `No`.
The team's rows drift month to month (tissue `NA` vs `NOT_COLLECTED`, medium
`NOT_COLLECTED` vs `NOT_PRESERVED`, Death_date = walk day in May 2026): propose
this template and point out a deviation; **Ask** PAS which is canonical before
mass-correcting old rows.

**Released unmarked** (`Released_Unmarked`): as marked, FieldMark_ID `NA`,
Splitted_body `NA`. Rare; ask why when it appears.

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

Once an Ithomiini species has 30 preserved individuals it is marked and
released instead (meeting of Nov 2023; the 2023 method says at most 30 per
species). Count every `Collected_Preserved` row from Ikiam, Casa de Lin and
Mariposario Ikiam, whatever its Purpose, per species (`get_alerts` gives the
counts, the day each species reached 30 and those close to it). The import warns; when you propose a
preserved capture of a species already at 30, say so and ask whether it
should have been marked. It is applied loosely (Hypothyris euclea, Hyposcada
anchiala, Pseudoscada florula kept being preserved past 30): **Ask** whether it
still holds for them and whether it counts per species or per subspecies.

## Weather codes

List values (Collection_data): Rainfall `DY_(dry)`, `DZ_(drizzle)`,
`WR_(weak_rain)`, `NA`; Cloud_cover `S_(cloudless_sunny)`,
`S&C_(sun_&_cloud_patches)`, `CL_(cloudy_light)`, `CD_(cloudy_dark)`, `NA`
(check with `describe_sheet`).

| Written on paper / in Wikiloc | Column | Value |
|---|---|---|
| `sol`, `DS` / "Despejado" | Cloud_cover | `S_(cloudless_sunny)` |
| `parches`, `SyN` (sol y nubes) | Cloud_cover | `S&C_(sun_&_cloud_patches)` |
| `NC`, `N-C`, `N.C` (nublado claro) | Cloud_cover | `CL_(cloudy_light)` |
| `NO`, `N-O` (nublado oscuro) | Cloud_cover | `CD_(cloudy_dark)` |
| nothing about rain, `S` (seco), `NO` in a rain column | Rainfall | `DY_(dry)` |
| `llovizna`, `LV` | Rainfall | `DZ_(drizzle)` |
| `LD` (lluvia débil), "lluvia" (inferred: say so) | Rainfall | `WR_(weak_rain)` |
| `ND`, "no data" | either | `NA` |

`NO` means overcast in a cloud column but "no rain" in a rain column: read it
by column. `LF` (heavy rain), `CN` and a bare `C` have no agreed mapping: ask
the walker. Trap envelopes use `Sol / Parches / NC` the same way.
