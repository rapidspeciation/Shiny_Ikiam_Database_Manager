# Monitoring at Ikiam

The team walks the Ikiam trail once a month (about four days between the 10th and 15th). The 1.11 km trail is split into four sections, T1 (by the campus) to T4 (the far end), stored in `Transect_section`. Captures go into Collection_data with `Purpose` = Monitoring and `Collection_location` = Ikiam; these rows are the only ones the Monitoreo tab counts.

## Field notes in Wikiloc

Each capture is a Wikiloc waypoint whose name holds the data, in any order:

`M1 Hyposcada illinissa ida hembra 9:20 0.5m NO id: B69`

| Part | Meaning | Collection_data |
| --- | --- | --- |
| `M1` | butterfly 1 of the walk | (not stored) |
| species, subspecies | typos of one or two letters are corrected against names already in the sheet; `H. illinissa` also works | SPECIES, Subspecies_Form |
| `hembra`/`female`, `macho`/`male` | sex | Sex |
| `9:20` | time | Collection_time |
| `0.5m`, `50cm` | flight height at first sight | Flight_height |
| `NO`, `NC`, `parches`, `sol` | nublado oscuro, nublado claro, sun and cloud patches, sunny | Cloud_cover (CD, CL, S&C, S) |
| `llovizna` | drizzle (otherwise dry) | Rainfall |
| `id B69` | field mark: the butterfly was marked and released | FieldMark_ID, Release_Collect = Mark_Released |
| `recaptura` | optional: a mark seen before is a recapture anyway | Notes_Collection_data ("Recapture") |

Without a mark the capture is `Collected_Preserved`; CAM and tube IDs are added later (Colecta or Tubos). Words the app does not understand are kept in Notes_Collection_data.

Export from the Wikiloc app with *Send to your GPS → Send trail as file* (GPX), or from the website with *Download → GPX* and *waypoints* ticked. Wikiloc has no API and forbids automated downloads, so the file is uploaded by hand.

## Rules the app applies

- **Transect**: the nearest of the four sections to the waypoint (none if more than 40 m from the trail). The sections were reconstructed on 26 Sep 2026 by fitting the coloured transects of the QGIS map in the monitoring reports onto that day's GPS track (median error about 1 m); see `frontend/src/lib/transects.ts`. The last ~80 m of T1 towards the campus come from the QGIS map only.
- **Recapture**: a Mark_Released row whose FieldMark_ID already appears on an earlier date. Older recaptures written only in notes (2024) are not counted.
- **30-preserved rule** (meeting 38, Nov 2023): once an Ithomiini species has 30 preserved individuals in the Ikiam monitoring, it is marked and released instead. Resumen shows each species' progress; the import warns when a preserved capture belongs to a species that already reached 30.
- **Next mark**: the highest number of the series in use plus one (B68 → B69; after 99 the next letter).
- **SamplingDay_data**: the walk's first and last GPS times fill Start_time and End_time of the day's row for that collector, or add the row.

Tracks and capture coordinates are stored in the app database (`monitoring_tracks`), not in the workbook, until the team decides on GPS columns for Collection_data.
