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

Two ways to bring a walk in:

- **GPX file** (new walks): in the Wikiloc app, *Send to your GPS → Send trail as file*, then *Elegir GPX de Wikiloc*. The GPX has the GPS times, so the day's start and end go into SamplingDay_data.
- **Wikiloc link** (old walks, and photos): on a computer with this repository, run

  ```
  npm --prefix tools/wikiloc install    # once
  npm run wikiloc -- https://es.wikiloc.com/rutas-senderismo/...-213523060 [more links]
  ```

  The helper opens each public trail page in a headless browser (Wikiloc has no API, and its Cloudflare check blocks the server but lets a home connection through). It reads each waypoint's note, position and photos and the trail line, without any Wikiloc login, and sends them to the app. The app server downloads the photos (from wklcdn.com). The walk then appears under *Desde Wikiloc, por revisar* in Importar recorrido. The public page has no GPS times, so SamplingDay_data is not filled from it. If the GPX of the same day was already imported, *Solo añadir las fotos al GPX ya subido* attaches the photos to it by matching the waypoint notes. The app login is asked once and kept in `~/.config/ithomiini-wikiloc/session.json` (mode 600). The pages are opened a few seconds apart; use it only for the team's own trails.

## Rules the app applies

- **Transect**: the nearest of the four sections to the waypoint (none if more than 40 m from the trail). The sections were reconstructed on 26 Sep 2026 by fitting the coloured transects of the QGIS map in the monitoring reports onto that day's GPS track (median error about 1 m); see `frontend/src/lib/transects.ts`. The last ~80 m of T1 towards the campus come from the QGIS map only.
- **Recapture**: a row whose FieldMark_ID was already recorded on an earlier date **for the same species**. A mark recorded on another species is a conflict (an ID given twice or a wrong species), listed under *Revisión de datos* in Resumen and not counted as a recapture. In August–September 2026 the numbering restarted at B40 although B40–B61 had been used in May–July, so B40–B59 each belong to two butterflies.
- **Recaptures written only in notes** (mostly 2024, e.g. "7/7/24 AA: recatch&realease transect=4, time=9:59"): Resumen lists them and *Crear filas de recaptura* proposes one Mark_Released row each, copying the marked individual and reading date, time, height, weather, collector and transect from the note. Each recapture is its own row from now on.
- **30-preserved rule** (meeting 38, Nov 2023): once an Ithomiini species has 30 preserved individuals, it is marked and released instead. The count includes every Collected_Preserved row from Ikiam and Casa de Lin, whatever its Purpose: with that count every marked species had reached 30 when its marking began (Oleria gunilla 31 in July 2024; with monitoring rows alone it had 26). Resumen shows each species' progress; the import warns when a preserved capture belongs to a species that had already reached 30 on that date. Non-Ithomiini such as Heliconius numata are part of the monitoring (every M-numbered capture is) but the rule does not apply to them; *Solo Ithomiini* reproduces the report tables.
- **Next mark**: the highest number of the series in use plus one (B68 → B69; after 99 the next letter).
- **SamplingDay_data**: the walk's first and last GPS times fill Start_time and End_time of the day's row for that collector, or add the row.

Tracks, capture coordinates and photos are stored in the app database (`monitoring_tracks`, `wikiloc_walks`, `monitoring_photos`), not in the workbook, until the team decides on GPS columns for Collection_data.
