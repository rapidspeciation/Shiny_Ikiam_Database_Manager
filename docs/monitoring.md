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

Without a mark the capture is `Collected_Preserved`; CAM and tube IDs are added later (Colecta or Tubos). Words the app does not understand are kept in Notes_Collection_data. A mark may also be written on its own ("B51 9:51 female sol 1,5m …"). A point noted without a species (identified later from its photo) is matched to its sheet row by day, minute and sex.

**Pasar al mapa … ya registrados en la hoja** stores, in one go, every waiting Wikiloc walk with captures in the sheet. Points are paired with the collector's rows of that day by mark, species and minute (or minute and sex); short old notes ("Mariposa 1 y 2", "Marip 3") are paired in order when the number of butterflies and rows agree; a title one day off is corrected when the marks match the next or previous day. On the map each capture shows the row's curated species, sex, mark and section. Points without a row are left out (field notes never entered, usually Wikiloc mistakes). Walks none of whose points are in the sheet stay for review.

Ways to bring a walk in (all in Importar recorrido):

- **Share from the phone** (Android): install the app (Chrome menu → *Instalar app* / *Añadir a pantalla de inicio*). In Wikiloc, *Enviar a tu GPS → Enviar ruta como archivo* (the GPX) or *Compartir* (the link), and pick *Ikiam DB*. A GPX opens for review at once, with the GPS times that fill SamplingDay_data. A link is queued like a pasted one. iPhones do not offer web apps in the share menu; there, save the GPX and use *Elegir GPX de Wikiloc*, or paste the link.
- **GPX file**: *Elegir GPX de Wikiloc*.
- **Paste a Wikiloc link**, or **Buscar nuevos en Wikiloc** for the followed profiles (Franz Chandi, Alex Arias and María José Sánchez, each with their collector), which brings every trail whose title contains "monitor" and that is not in the app yet. The walk's date is the day written in the title within the month and year Wikiloc recorded (titles have typos such as "27/4/2024" for April 2025, or no year). Wikiloc blocks the app server, so these are done by a processor on a computer at home (tools/wikiloc, see its README). The walk, with its photos, appears under *Desde Wikiloc, por revisar* a minute or two later. The bar shows whether that computer is online; while it is off, links wait in the queue. The public page has no GPS times, so SamplingDay_data is not filled from it; if the GPX of the same day was already imported, *Solo añadir las fotos al GPX ya subido* attaches the photos to it.

## Reporte (live report)

Monitoreo → Reporte replaces the monthly slides. One row of filters (period, collector, transect, species, only Ithomiini, by subspecies) scopes everything below and is kept in the page link, so a filtered view can be shared (e.g. `#/monitoreo?vista=resumen&desde=2025-01&hasta=2025-12&rec=AA`). Charts use Apache ECharts (SVG), loaded only when the report opens; hover details sit beside the pointer, never over the mark; every chart has a *Tabla* view.

- **Headline numbers:** individuals, monitoring days (one per collector and date, from SamplingDay_data plus days with captures), individuals per day, species, preserved and marked, recaptures (and the share of marked individuals recaptured), next mark.
- **Abundance and effort:** individuals per month by fate (zoomable), individuals per monitoring day, monitoring days per collector, comparison between years.
- **Species:** most abundant species by fate, species accumulation curve (with singletons and doubletons), seasonality (individuals per monitoring day by calendar month), composition by transect, sex ratio.
- **Behaviour and weather:** hour of capture, flight height, cloud cover.
- **Marking and recapture:** days between captures of the same individual, and distance moved when both captures have GPS (walks on the map).
- **Tables:** species with the 30-preserved rule (top 10, expandable), individuals per transect and month, recapture histories.
- **Data review** (at the bottom, as notes): marks recorded on two species, recaptures written only in notes, and monitoring rows whose Purpose is empty or "NA" (counted as monitoring when the collector recorded that day in SamplingDay_data).

## Mapa

Monitoreo → Mapa shows the captures of the walks on the map over a satellite image, with the transect sections T1–T4. The filters follow the atlas (rapidspeciation.github.io/ithomiini_maps): nothing chosen shows everything, and each option shows how many captures it would show given the other filters. The filters and layers are kept in the page link (*Compartir* copies it), e.g. `#/monitoreo?vista=mapa&fechas=2026-09-21&capa=grupos`.

- **Filters:** monitoring dates (a searchable list grouped by year; choosing a date shows every walk of that day), species (searchable, with the legend colour), and chips for year, collector, transect, sex and type (preserved, marked, recapture).
- **Mostrar como:** *Puntos*; *Grupos*, clusters whose ring shows the colours of the captures inside; *Calor*, a heatmap scaled to the busiest spot at each zoom.
- **Colours:** by species (the 10 most common over all walks keep their colour; the rest are grey "Otras"), sex, type or walk. Clicking a legend entry filters by it.
- **Layers:** the transects are on; the walks' Wikiloc GPS lines are off by default and, when on, show only for the walks shown (broken where the signal jumped more than 60 m). *Unir recapturas* joins the captures of the same mark and species.
- **Walks of the chosen days** can be opened in Wikiloc or removed from the map.

## Recapturas

Monitoreo → Recapturas lists every marked butterfly caught more than once (same mark and same species), with the photos of the marking and of each recapture side by side, the days and metres between captures, collector, transect, time and sheet row. A photo opens large (arrows move through that butterfly's photos). *En el mapa* shows only that individual on the map; a capture's popup on the map links back to its photos.

## Rules the app applies

- **Transect**: the nearest of the four sections to the waypoint (none if more than 40 m from the trail). The sections were reconstructed on 26 Sep 2026 by fitting the coloured transects of the QGIS map in the monitoring reports onto that day's GPS track (median error about 1 m); see `frontend/src/lib/transects.ts`. The last ~80 m of T1 towards the campus come from the QGIS map only.
- **Recapture**: a row whose FieldMark_ID was already recorded on an earlier date **for the same species**. A mark recorded on another species is a conflict (an ID given twice or a wrong species), listed under *Revisión de datos* in Resumen and not counted as a recapture. In August–September 2026 the numbering restarted at B40 although B40–B61 had been used in May–July, so B40–B59 each belong to two butterflies.
- **Recaptures written only in notes** (mostly 2024, e.g. "7/7/24 AA: recatch&realease transect=4, time=9:59"): Resumen lists them and *Crear filas de recaptura* proposes one Mark_Released row each, copying the marked individual and reading date, time, height, weather, collector and transect from the note. Each recapture is its own row from now on.
- **30-preserved rule** (meeting 38, Nov 2023): once an Ithomiini species has 30 preserved individuals, it is marked and released instead. The count includes every Collected_Preserved row from Ikiam and Casa de Lin, whatever its Purpose: with that count every marked species had reached 30 when its marking began (Oleria gunilla 31 in July 2024; with monitoring rows alone it had 26). Resumen shows each species' progress; the import warns when a preserved capture belongs to a species that had already reached 30 on that date. Non-Ithomiini such as Heliconius numata are part of the monitoring (every M-numbered capture is) but the rule does not apply to them; *Solo Ithomiini* reproduces the report tables.
- **Next mark**: the highest number of the series in use plus one (B68 → B69; after 99 the next letter).
- **SamplingDay_data**: the walk's first and last GPS times fill Start_time and End_time of the day's row for that collector, or add the row.

Tracks, capture coordinates and photos are stored in the app database (`monitoring_tracks`, `wikiloc_walks`, `monitoring_photos`), not in the workbook, until the team decides on GPS columns for Collection_data.
