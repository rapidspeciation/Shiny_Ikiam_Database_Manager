# Assistant tools: maps, host plants, Wings Gallery

Six read-only tools that let the assistant (Asistente tab) answer questions such as "where are the best places to find Oleria onega?", "what does Mechanitis feed on?", "show me photos of Tithorea harmonia's host plants found in Ecuador" or "show me female Melinaea from Tena", with links that open the public sites already filtered. Code: `server/maps-tools.mjs`. Tests: `tests/maps-tools.test.mjs`.

None of them writes anything: they read the public sites' data files over HTTPS and the app's own database with `SELECT` only. What they keep is in memory, in the tools' worker (`server/assistant-host.mjs`: they are not in `MAIN_TOOLS`, so a 20 MB file read never holds up the app's thread).

## Data sources and how fresh they are

| Data | Where the tool reads it | How fresh |
|---|---|---|
| Occurrence records of the maps (`rapidspeciation.github.io/ithomiini_maps`) | `data/data_manifest.json` and the five `data/map_points_*.json` it lists: Sanger Institute (the project's own collection, 7,850 records), GBIF (UNAM) 21,562, GBIF (other institutions) 26,956, Dore et al. (2022) 28,927, iNaturalist 20,424 | As published by the maps' «Update Database» run (`update_websites`). The project's records follow the workbook when that run is started; the GBIF and iNaturalist downloads only when it runs with `gbif: true` (last on 26 Sep 2026, `generated_at` of the manifest). The tool re-reads a file at most every 6 hours, and only if its ETag changed. Each answer says the manifest's date and each file's Last-Modified. |
| Host plants | `data/host_plants/host_plant_associations.json` of the same site (863 butterfly-plant associations, 16 Jun 2026) | Changes only when the maps' host plant review is rebuilt. Same 6-hour check. |
| Where host plants grow, and their photos (only when an area is asked) | `data/host_plants/host_plant_layers_manifest.json`; per plant its `taxon_occurrence_refs/<slug>.json` (which records of the 24 `occurrence_chunks/chunk_NNN.json` are its own: 465,757 GBIF plant records with coordinates, year and basis of record, no locality or country) and its photo index `gallery_taxa/<slug>/index.json` with pages of 100 photos | The maps' GBIF plant download (generated 13 Jun 2026). Only the chunks the asked plants' refs name are read (Tithorea harmonia's hosts: 6 of 24), each kept as typed arrays (about 0.4 MB); at most 4 photo pages per plant. Same 6-hour check. |
| Countries of plant records | `data/country_boundaries.json` of the maps site (Natural Earth 1:50m outlines of the Americas, made by the maps' `scripts/build_country_boundaries.mjs`) | Fixed. The maps app's country filter places plant records with the same file and rule (`server/country-outlines.mjs` here, `src/utils/countryLookup.js` there), so the counts match what the map shows. |
| Elevation of a record | The workbook's `ELEVATION` for the project's records (by CAM ID, from this app's database); for other records, the Copernicus 90 m DEM through Open-Meteo's elevation API at the record's coordinates | Workbook: live. DEM: looked up only when a call filters or sorts by elevation, at most 1,000 distinct points a call (10 requests of 100), kept in memory. |
| Specimens and photos (Wings Gallery, `rapidspeciation.github.io/Shiny_Ikiam_Wings_Gallery`) | This app's own copy of the workbook: `Collection_data` rows and the `Photo_links` sheet (Drive file IDs) | Live (the workbook sync, every few minutes). The gallery site itself is rebuilt by its «Update Database» run, so a specimen added today may be in the tool's answer before the gallery shows it: the answer says so. |
| What the models read from the photos | This app's photo tables (`server/photodata.mjs`): `photo_predictions` (the Wings Gallery's species and subspecies, the sex model's call), `envelope_readings` (the envelope's box and text lines, the photo's size), `photo_wing_boxes` (the wings' box) | As last imported (`scripts/import-envelope-curation.mjs` and the predictions import). A specimen photographed since has none yet: the answer says "none stored". |

## Tools

### `maps_localities`
Places with the most records of one or more taxa, best first.

- `taxa` (required, up to 20): species ("Oleria onega"), subspecies ("Oleria onega janarilla") or genus ("Oleria"). Synonyms in the maps' `taxonomy_synonyms.json` are resolved to their accepted names.
- Region, any of: `country` (list), `bbox` `[west, south, east, north]`, `near` `{lat, lng, km}`.
- `elevation_min`, `elevation_max` (metres).
- `sources`: subset of `sanger`, `gbif_unam`, `gbif_other`, `dore`, `inaturalist` (default all).
- `years` `{from, to}`: the record's year, when it has a date.
- `site_km` (default 3): records closer than this fall in one site (a grid of that size).
- `limit` (default 15, at most 50).

Answer: per site its name (the commonest locality name of its records), country, centre, records, records by source, years seen (first, last), elevation (median, with its source), and a map URL zoomed on it; a map URL with all of them; the totals; and the freshness note. Ranking is by number of records, then distinct years: many records over many years is the best sign a species is reliably there.

Every region filter also takes `sex` (`male`, `female`), from the maps' `sex` field; a project record without one in the maps takes the workbook's `Sex` by CAM ID. With it, a site is ranked by its records of that sex (sites without any are left out, and so are Dore et al.'s cells, which have no sex), and each site gives `by_sex` and `own_by_sex` (the project's records). Records with a sex in the maps' data (26 Sep 2026): Sanger Institute 7,293 of 7,850 (93 %), GBIF (UNAM) 6,186 of 21,562 (29 %), GBIF (other institutions) 11,325 of 26,956 (42 %), iNaturalist 267 of 20,424 (1 %), Dore et al. 0 of 28,927.

### `maps_snapshot`
A picture of the maps with some taxa (one sex too), framed on the best sites, for answers sent where a link is not enough (WhatsApp). Code: `server/maps-render.mjs` and `server/maps-render-worker.mjs` (the maps app's own export), `server/map-snapshot.mjs` (Pillow). Design: `reports/assistant_maps_photos_20261102/map-pilot/DESIGN.md`.

- `species` (as `taxa`), `sex`, `country`, `sources`, `years`, elevations, `site_km`; `bbox` or, left out, the best site and the next best within 150 km of it (up to `sites`, default 8), or `frame: "all"` (every record); `style` (light, terrain, streets, satellite, dark) and `color_by` (subspecies, species, genus) for the app picture; `lang` `en`/`es`.
- Answer: `image`, `image_type` (png or jpeg), `renderer` (`app` or `pillow`), `degraded` (null, or {reason, detail, not_kept}), `map_url` (the maps app on the same filters, sites layer, basemap, colouring and frame), `picture_labels` [{rank, site, label_on_picture, merged_sites}], `caption` (taxon · sex · area; N records at M sites, K on the map; with a `bbox`: N records at M sites in the area asked, K visible in the picture, and the records the frame shows just outside the area apart; without a subspecies; years), `records.on_picture` (visible in the picture: in the frame, not under its title, legend, scale or credits; with a `bbox` also `records.asked_on_picture`, those of the area), `hidden_sites` (app: sites in the frame that an overlay covers, left out of `picture_labels`), `legend_unknown` (app), the sites as `maps_localities` gives them, `elsewhere`, `best_by_taxon`, `timing_ms`.

#### The app picture (`renderer: "app"`)
- When: the maps app can show exactly these records: taxa, `sources`, `country`, `sex` only. `area`, `elevation_min/max`, `years`, `near` and `site_km` other than 3 give `degraded.reason: "unsupported_filter:<name>"`. With `sex`, a project record whose sex the workbook fills in or changes gives `sex_from_workbook` (`detail.records_affected`); without `sex` the workbook is not consulted.
- How: `node server/maps-render-worker.mjs` in its own process group opens only `https://rapidspeciation.github.io/ithomiini_maps/` with the tool's filters as URL parameters (closed set: `sp, ssp, genus, source, country, sex, sites=1, site_labels=records, site_min=1, style, color_by, bbox, export=1`), waits for `window.ithomiiniMapsExport` and calls it; nothing else is evaluated in the page. The app's answer (contract: `tests/fixtures/maps-export-contract.json`) is checked before it is sent: version 1, a 1600×1200 PNG; `params.ignored` empty, `basemap`/`color_by` as asked; the view equal to MapLibre's fitBounds of the `bbox` sent in the frame the app measured (760×570 CSS px ±8, padding 32, max zoom 12; centre within 1e-5°, zoom within 0.02); the same `source|id` set as the tool (with a `bbox` asked: the app's records inside it), with the same map sex, country, year and coordinates rounded to 1e-5°; legend rows (with the unknown-subspecies row) adding up to the app's records, in records; each ranked site inside the view a member of a drawn marker; the picture geometry (`view.overlays`: title strip, legend, scale, credits as boxes in CSS px of the canvas; records' `x/y/in_picture`; markers' `x/y/radius/label_boxes/occluded`) present, well formed and consistent with this side's own projection of the view within 1 px (`geometry_missing`, `geometry_invalid`, `geometry_inconsistent`); no site the answer names (the best sites) in a marker an overlay touches (`ranked_site_hidden`; the app first moves its legend to a bottom corner that covers no marker when it can, but the title strip stays); not blank (luminance SD > 8); no outer row or column (outer 4 px) more than 25 % accent colour (the export frame's dashed border). What is on the picture (`records.on_picture`, sites, labels) is what the app reports visible (`in_picture`, not `occluded`, labels with clear boxes), after those checks. A title the strip cut short is not quoted (`picture_title_truncated`). Any failure: the Pillow picture with that reason (`parity_mismatch`, `view_mismatch`, `border_stripe`, …).
- Limits: queue at most 10 s per tools thread (`queue_wait`), at most one browser per server process across threads (render workers counted in /proc), MemAvailable at least 3 GB before launch (`low_memory`), 50 s from spawn to answer (`deadline`), RSS of the worker and its descendants at most 2 GB, sampled every 500 ms (`memory_cap`). After every run, failed launches included, the worker's process group is killed, then every process whose command line names the run's temp folder (Playwright starts Chromium in a group of its own) with its descendants, and the temp folder (`$TMPDIR/maps-render-XXXXXX`, the run's TMPDIR: profile, crash reports, downloads; kept under the system temp folder for Chromium's socket path limit) is removed; folders left by a tools thread terminated mid-render are swept after 10 minutes. The page waits at most 25 s for `window.ithomiiniMapsExport` (`no_export_interface`). Pictures `app-*.png` kept a week in `map-snapshots`. Worst case about 80 s with the Pillow fallback (its own 20 s limit), under the tools' 180 s call limit.
- Off: no `PLAYWRIGHT_MODULE` (or a module without `chromium.launch`, or no Chromium) gives `renderer_unavailable`, logged once per thread; `MAPS_RENDER=pillow` gives `renderer_off`. See [operations](operations.md).

#### The Pillow picture (`renderer: "pillow"`)
- The same records, frame (best sites, `all` or `bbox`) and ranking as the app picture would have; `degraded.not_kept` is always `basemap, color_by, site_labels, legend_style`; the sites are numbered (`on_map`, also `picture_labels[].label_on_picture`). A 1280×960 JPEG of about 400–500 KB with the OpenTopoMap relief, in `<photo cache>/map-snapshots`, kept a week.
- How it is drawn: no browser. The basemap is OpenTopoMap's raster tiles (towns, roads, rivers, contours, relief; CC-BY-SA, credited on every picture as «© OpenStreetMap contributors, SRTM | © OpenTopoMap (CC-BY-SA)»). Since 1 Nov 2026; before, Esri's World Topo Map, whose terms need an ArcGIS subscription and forbid keeping tiles; CARTO's raster tiles answer "API key required". OpenTopoMap asks for light use: one request at a time per tools worker, a User-Agent naming the app and its repository, tiles kept a week in `map-snapshots/topo-tiles` (about 200 MB at most, the oldest downloads dropped first by the hourly clean); the records are dots coloured by taxon, the sites black numbered circles (moved aside with a line when two would overlap), a legend and the attribution; Pillow (python3, already used for the proposal photos; claudeclaw has it with the DejaVu fonts) makes the JPEG.
- Speed (30 Oct 2026, with Esri's tiles fetched all at once; OpenTopoMap's, one at a time, take about 20 s for 30 new tiles on this PC (1 Nov), once per area and week; the three-species question with `sex: male`): this PC, first call 2.5–3.8 s (the five data files and 30 tiles fetched), again 0.3–0.6 s; claudeclaw (4-core ARM), 3.7 s and 0.5 s.

### `maps_species`
The taxa recorded in a region, most records first ("what can we find around Mindo above 1,500 m?"). Same region, elevation, sources and years parameters; `genus` narrows it; `limit` default 30, at most 100. Each taxon with its records, sites, and last year seen, and a map URL.

### `host_plants`
Host plant associations from the maps' review, with sources.

- `butterfly`: species, subspecies or genus: its host plants.
- `plant`: plant family, genus or species: the Ithomiini recorded on it.
- `min_confidence`: `low`, `medium` (default), `high`; `needs_check` rows only with `include_unchecked: true`.

Answer: each association with the butterfly taxon, the plant (family, genus, species, as written in the source), host level (species, genus, family), evidence level (direct, literature), confidence, the citation, its DOI or URL, and the primary references (cut to 400 characters). Grouped and counted by plant genus (or butterfly genus). A note when only family-level or catalogue rows exist: the review says not to treat a catalogue row as observed local use.

With an area (`country` list, `bbox` [west, south, east, north] or `near` {lat, lng, km}), or `records: true` for the whole range, the same call also answers where those host plants grow:

- `plants`: each host plant that has a records layer (species and genus level; at most 12), with its GBIF name when it differs, the butterflies it is a host of, the evidence, its records in the area and in all, years, records by basis (preserved specimen, human observation), and its top 3 sites. A plant with no records there is listed with 0. `plants_without_records`: hosts with no layer (family level, or not downloaded).
- `sites`: the best sites of the species-level hosts together (a grid of `site_km`, default 5 km), busiest first (`limit`, default 8): centre, records (a record of two synonymous plants once), records per plant, years, the nearest named collecting place of the maps' butterfly records within 10 km (Sanger, GBIF and iNaturalist localities: plant records have no locality name), and a map link zoomed on it. Genus-level hosts are counted in `plants` but are not in `sites`, photos or the map, unless they are the only hosts or that genus was asked: their records are of any species of the genus.
- `markdown`: per plant, `photos` thumbnails (default 3, at most 6; 0 for none) from the maps' plant gallery: those taken in the area first, field photos before herbarium sheets, one per GBIF record before a second; if the area has too few, photos from elsewhere with their country in the caption. Each is a 240x180 wsrv.nl thumbnail linked to the full photo, then a line with the GBIF record of each photo and the datasets they come from. The maps' gallery data keep no licence or photographer, so the line says they are on each record's page.
- `map_url`: the Wings Atlas with the butterfly's records, the host-plant layer with those plants (`hp_bf`, `hp_taxa`, `hp_level`, `hp_evidence`), the country, and a view of the sites.
- `records_note`: the download's date; countries placed by coordinates with 1:50m outlines; plant records show where the plant grows, not that the butterfly uses it there.
- Size: the answer keeps itself under 36,000 characters. `markdown` at most 12,000 (plants past that in `photos_left_out`), map links on the first 3 sites only (`map_url` has them all), `limit` at most 15. When still too long it drops, in this order, the associations' references, the associations of plants not listed with records, the last associations, the plants' top sites, the last photo blocks and the last sites, and says so in `cut`. A photo page that does not answer leaves that plant without photos (`photos_note`), not the answer without its records.
- Cache: the plant files are kept per version of the layers (the manifest's `generated_at`); when the maps republish them, all are read again, so a refs file never points into an older chunk.

Tithorea harmonia in Ecuador (low confidence and unchecked rows too): one call, about 25,000 characters; 0.8 s the first time from local files, 20 ms after.

### `host_plant_sites`
Where to go first to collect a butterfly's host plants (or a plant) near a point: "near Ikiam, which site first for Tithorea harmonia's host plants?".

- `butterfly` or `plant`, `min_confidence`, `include_unchecked` as in `host_plants`; `near` {lat, lng, km} (required; `km` default 25, at most 100).
- `rank`: `localities` (default): the project's collection localities, from its Sanger records in the maps (grouped by locality name, split where its records are over about 5 km apart; the median of their coordinates). `grid`: cells of `cell_km` (default 1.5 km, 0.5 to 5) holding plant records, scored at their centre and named after the nearest named place of the maps' records.
- `limit` (default 8, at most 20), `photos` (default 2, at most 4) for the first 3 sites.

Score: the plant records of the species-level hosts (as in `host_plants`' sites; a GBIF record of two synonymous plants once) within 10 km, each weighing (1 - (d/10)²)² (quartic kernel: 1 at the site, 0.92 at 2 km, 0.56 at 5 km, 0 at 10 km). Sites with none are counted, not listed. Plain JavaScript in the tools worker (haversine distances): no GIS library.

Answer, per site: locality (or the nearest named place for a cell), coordinates, km from the point, score, records within 2, 5 and 10 km, records per plant within 10 km, the nearest record (plant, km, year), the oldest and newest years and the undated records, the elevation (the workbook's median for a locality; the 90 m DEM for a cell), the project's records there and those of the butterfly asked, and a map link on it. `map_url`: the first site's, with the host-plant layer on. `markdown`: for the first 3 sites, the plant photos nearest each (from the first 10 gallery pages of each plant with records in reach), with plant, year and distance, each linked to its full photo, and their GBIF records and datasets. `method` and `caveats`: the kernel; records follow roads, stations and observers (a high score can be a well-sampled place, not more plants); a plant record is not evidence that the butterfly uses the plant there.

Tithorea harmonia near Ikiam (30 km, low confidence and unchecked rows too): 7 plant records in reach, so the scores rest on few, old records (1968 to 1995); about 11,000 characters, 0.8 s from local files.

### `area`: provinces and cantons of Ecuador
Every tool with a region (`maps_localities`, `maps_species`, `maps_snapshot`, `host_plants`) takes `area`: {province: "Napo"}, {canton: "Tena"} (with `province` when two cantons share the name, e.g. Bolívar), or a name ("Napo", "provincia de Napo", "cantón Tena"; a province wins over a canton of the same name unless a canton is asked, and the answer says the canton exists). The records are kept when they lie inside the outline. Code: `server/admin-areas.mjs`.

- Outlines: geoBoundaries gbOpen ECU (release 9469f09) simplified files: ADM2, INEC's 2019 cantons (via OCHA ROLAC, CC BY 3.0 IGO), and ADM1 provinces (CC0), about 3.5 MB. Downloaded on the first question that names an area, made compact (each canton with its province, read from ADM1 at points inside it) and kept as `<photo cache>/maps-cache/admin/ecuador-admin-v1.json`; read from there after a restart, and kept in memory. A province is the union of its cantons, so provinces and cantons share edges. A point in no canton (slivers of the simplification, or just past the border) counts in the nearest canton within 0.0055° (about 600 m).
- Where INEC's 2019 limits differ from older maps (the Napo–Sucumbíos limit near El Chaco and Gonzalo Pizarro), INEC's are used; a photo outside the area says the canton its outline puts it in.
- `distanceKm`: 0 inside, else the km to the area's edge (the edges of its cantons, on a plane scaled at the point's latitude); points whose distance to the area's box is past 300 km are not measured.

`host_plants` with `area`, `bbox` or `near` (not a country) adds, per plant:

- `status`: `in_area`, `nearby` (no record inside, some within `nearby_km`, default 50, at most 200; 0 for none) or `far`; `nearby` {within_km, records, years, by_region (province in Ecuador, else country), nearest {km, year, region} or "none within 300 km"}. `in_area`, `nearby_only` and `not_near` list the species-level hosts.
- `same_records_as`: two plants whose GBIF records are the same (a synonym kept apart by the review, e.g. Prestonia acutifolia and P. quinquangularis): one entry in the lists, one photo.
- `photo_sheet: true` (or `send_photos`, which gives the same): 1–2 contact sheets (3×3 numbered 512 px tiles, server/photo-sheets.mjs) of each plant's candidate photos: living plants first, then by distance to the area, one photo per GBIF record before a second, the nearest leaves-or-plant photo and a flower kept among them when the photo tags (tools/plant-photo-tags) know one. The chat looks and picks; `send_sheet_photos` {sheets, tiles} downloads the chosen (iNaturalist's `original`, at most 2048 px; else wsrv.nl at 2048) and credits them from GBIF. `auto_pick: true` (tests, scripts) picks without looking. `tags_fallback: true` with `send_photos` (default 3): the same pick for a chat that cannot use a sheet (`photo_sheet_error` points to it), given as `unchecked_images` and `unchecked_images_block` with `looked_at: false`, never as `images_block`, so nothing is sent unless the chat adds it after telling the person; `photo_tags_missing` when the tags file is not in the cache folder (then by living plant and place only; the server logs it once).

`images`, the shape the WhatsApp bridge reads:

```
images: [ { path,        // a JPEG on this server, about 2048 px on its long side (null only without a cache folder: tests)
            caption,     // ready to send: _Plant_ — recorded in Napo (n records) | CANDIDATE: no record in Napo, n within 50 km
                         //   / place, year (photo outside Napo: canton, province) / Photo: author (licence) / link
            plant, status, photo_in_area, field_photo, width, height, bytes,
            url,         // the source the file was made from
            source_url,  // the gallery's original
            credit: { photographer, licence, licence_url, place, year, link, as_recorded? } } ]
images_note  // when a photo could not be fetched and the next was used
map_url      // Wings Atlas: the butterfly, the host-plant layer, Ecuador, the area's view
timing_ms { records, photos, area_total }, plant_data.records_from (memory, disk, download)
```

- Cache (`<photo cache>/maps-cache`): `admin/`, `plant-records/<layers version>/<plant>.json` (each plant's coordinates, years, basis and record keys: a new worker or a restart reads these instead of the 7.5 MB chunks; `host_plant_sites` uses them too), `plant-photos/photos/` and `plant-photos/gbif/`. All in memory as well; the maps' files as before (6 hours, ETag).

### `taxon_photos`
Photos of a taxon at a life stage ("fotos de huevos y larvas de Tithorea harmonia"), to look at and pick from. `taxon` (genus or species), `life_stage` (egg, larva, pupa, adult; one or a list), optional `area` or `near` (photos there first; without one, nearest Ikiam first), `sex` (male, female: adults recorded so only), `lang`.

- Sources, together: the project's clutch photos (Clutches tab, `clutch_photos` of Insectary_stocks rows of that species: eggs and larvae; production had none on 31 Oct 2026); iNaturalist observations annotated with Life Stage (term 1: egg 7, larva 6, pupa 4, adult 2), research grade and needs ID, the first 200 by votes, up to 3 photos each; GBIF records whose `lifeStage` says the stage, with images, from datasets other than iNaturalist's (those are already there); a GBIF failure that looks passing (no answer, timeout, 429, 5xx) is tried once more after 1.5 s (`retried`). Unannotated iNaturalist photos are not found.
- Adults only: the Wings Atlas records' photos (the maps' `map_points_sanger`, `_gbif_other`, `_inaturalist` files, already read for the other tools; `compactPoint` keeps `img` and `obs` only on records with an http(s) photo, the project's wsrv.nl thumbnails unwrapped to their Drive file). Same taxon (synonyms resolved; a genus takes all its species), area or nearest first, at most 60 on offer; a record the APIs gave already (same iNaturalist observation or photo, same GBIF record, same URL) is left out (`counts.atlas_adult_already_shown`). They have no life stage or behaviour field: tiles say "Atlas", captions say "Wings Atlas record, no life stage recorded", and they are never read for eggs, larvae or pupae. Still there when the iNaturalist and GBIF APIs are down (the maps' files are kept 6 hours, and the last in-memory copy since the most recent server restart is used when the site does not answer).
- `sex`: iNaturalist's Sex annotation (term 9: 10 female, 11 male; filtered on the results), GBIF's `sex=`, the atlas record's sex (the workbook's for the project's specimens). Eggs, larvae and pupae have none: `sex_note` says so.
- What was not checked: `sources_not_checked` (e.g. `gbif:egg`, `inat:adult`, `atlas`, `project`) and `not_checked` (stages with no photo where a source failed): never "none". `none_for` only when every source answered. With eggs asked, `oviposition_note`: no source records egg-laying; give `host_plants` with the area named, else `country: "Ecuador"`, and `photo_sheet` as context (where eggs may be laid), never as oviposition.
- Answer: `counts` per source and stage, and 1–2 contact sheets (each stage gets its share of the 18 tiles; one photo per observation before a second). `send_sheet_photos` sends the tiles chosen: iNaturalist's `original` (at most 2048 px) with the photo's attribution and licence and the observation's link; a clutch photo as kept, credited to who took it; an atlas photo of the project's specimen with its CAM ID (Ithomiini Ikiam, project collection); any other atlas photo only with the photographer and licence of that photo's media entry on its GBIF record (`http://` photos through wsrv.nl), else it is in `failed` (the GBIF record API must answer for those; the project's are sent without it).
- Tithorea harmonia in the atlas files of 26 Sep 2026 (checked 2 Nov 2026, APIs made to fail): 785 records with a photo, 777 on offer after the atlas' own duplicates, 26 recorded female.
- Tithorea harmonia on 31 Oct 2026: eggs 0, pupae 0, larvae 10 annotated observations (Mindo, Orellana, Central America, Brazil); one annotated as larva shows a pupa: the chat judges by looking.

### `gallery_specimens`
Collection specimens with wing photos, from the workbook and Photo_links.

- Any of: `cam_ids` (up to 200), `species` (list, "Genus species"), `subspecies`, `genus`, `sex` (`male`, `female`), `locality` (words matched in `Collection_location`, accents ignored), `country`, `elevation_min`, `elevation_max`.
- `with_photos` (default true), `limit` (default 12, at most 60), `thumbnails` (default 4, at most 8: how many of them are shown in full in `markdown`).

Answer: per specimen
- the database's CAM ID, species, subspecies, sex, locality, country, date, elevation;
- its photos (`dorsal`, `ventral`: Drive file, full-size URL, and for the detailed ones the photo's `size`);
- `model`: the Wings Gallery's species and subspecies top 3 with probabilities, and the sex model's call with its confidence. `sex_trusted` is false, with `sex_not_trusted` saying why, when the call is not Supported (`sex_supported` 0: a leaning only) or the species is on `server/sex-model-unreliable.json` (Methona confusa: `sex_not_trusted_why` gives the list's reason). Past the detailed ones only the first answer of each;
- `envelope`: the text lines the envelope reader read, the CAM ID it read, the photo it is from;
- `differs`: where these disagree with the database (`species`; `subspecies` when the species agree and the database names one; `sex`; `envelope_cam`).

And: the count of all matches; a gallery URL; `models`: the names of the species, sex and envelope models, once; and `markdown`, ready to paste in the reply.

`markdown`: the first `thumbnails` specimens in full: the database's line, a 600 px photo, the wings crop of each photo and the envelope crop (each linked to its full photo), links to both full photos, the model output and the envelope lines, each marked "not checked", with "(not the database's)" where they disagree. The next 20 one plain line each (the database's line, the model's first species and sex, links to the photos); any more are counted. A detailed specimen is about 1,700 characters; the default answer (12 specimens, 4 in full) about 22,000 of the tools' 38,000. The preview and crop URLs are only in `markdown`.

The models' output is never given as a fact: the database is the record. The envelope reader misreads some lines (an upside-down reading reads no CAM ID; the answer then says the reading may be poor).

## URL formats

Maps (state in its URL: `src/stores/filterStore.js`, `viewStore.js` and, for the host-plant layer, `hostPlants.js` of ithomiini_maps; `docs/host-plant-links.md` there):

```
https://rapidspeciation.github.io/ithomiini_maps/?sp=Oleria%20onega&source=Sanger%20Institute,iNaturalist&map_view=-77.81000,-0.99000,11.00
```

- `sp`: species, comma separated (the app's `scientific_name`); `ssp`: subspecies; `genus`: genus list.
- `source`: the sources' names as the app shows them (`Sanger Institute`, `GBIF (UNAM)`, `GBIF (Other Institutions)`, `Dore et al. (2022)`, `iNaturalist`). Left out it shows only Sanger Institute, so the tools always set it.
- `country`: comma separated; it limits the host-plant layer too (records placed by coordinates). `locality`: a JSON array of exact locality names.
- `sex`: `male` or `female` (records without a sex are hidden then).
- Host-plant layer: `hp_bf` butterflies; `hp_taxa` plant slugs or names; `hp_level` (`species`, `genus`, `family`; default species); `hp_evidence` (`direct`, `literature`, `needs_check`; default the first two); `hp_conf` (`high`, `medium`, `low`) in a hand-written link. The layer is on when `hp_bf` or `hp_taxa` is given.
- `map_view`: `lng,lat,zoom` (5 decimals, zoom 2 decimals).

Wings Gallery (`src/utils/shareView.js`): `view=1` marks a shared view; arrays repeat their key.

```
https://rapidspeciation.github.io/Shiny_Ikiam_Wings_Gallery/?view=1&species=Hypomenitis%20libethris&sex=male
https://rapidspeciation.github.io/Shiny_Ikiam_Wings_Gallery/search?view=1&camids=CAM042388%20CAM042389
```

The collection tab filters by `species`, `subspecies`, `sex`, `family`, `tribe`; it has no locality or elevation filter, so a search by locality, elevation or CAM ID links to the search tab with the CAM IDs found (at most 150 in a URL).

Photos (no file is stored; the image proxy wsrv.nl makes each one from Drive's copy):

- full size: `https://lh3.googleusercontent.com/d/<file>=w1600`;
- preview: `https://wsrv.nl/?url=<the full-size URL, encoded>&w=600&output=jpg`;
- crop: `https://wsrv.nl/?url=<encoded https://lh3.googleusercontent.com/d/<file>=w<photo width>>&cx=..&cy=..&cw=..&ch=..&w=300&precrop&output=jpg`.

The crop is in pixels of the photo at the size the readers saw it (`size_json` of `envelope_readings`, else of `photo_wing_sides`: mostly 1600 x 1067, 1200, 899 or 1060, some upright 1200 x 1600). The wing box (`photo_wing_boxes.box_json`, fractions [x1, y1, x2, y2]) is multiplied by that size, with a 2 % margin; the envelope box (`bbox_json`) is already pixels, with 6 px. Two things checked on 29 Oct 2026:

- Drive's `=w1600` of an upright photo is 1536 x 2048 (larger than the 1200 x 1600 frame the boxes are in), so the crop asks Drive for the photo at its own width (`=w1200`).
- Without `precrop` the proxy first shrinks the photo to `w` and then drops a crop that falls outside it (the answer showed the whole photo at 300 px). With it, it crops and then shrinks.

An envelope reading's `turned` 180 is not used: 6 of 6 such Drive photos checked show the envelope upright as they are now. A photo without a size gets no crop (the photo and links are still given).

## Photos in the chat

`html_preview` does not work on claudeclaw (AppArmor blocks T3's browser, 29 Oct 2026); `html_render` does. The skill says to show photos with the tools' Markdown. T3 Code (0.1.3, the server's version) renders Markdown images with https sources in assistant replies: tested on 28 Oct 2026 with a reply holding a wsrv.nl and an lh3 thumbnail of CAM036744, both shown at 160 px (screenshot `docs/assistant-tools-maps-gallery-t3-images.png`). The page sets no Content-Security-Policy, so the images load inside the Asistente frame too. `gallery_specimens` therefore returns ready Markdown, each image linked to its full photo.

## Limits

- An answer stays within the tools' common budget (`server/tool-budget.mjs`, 38,000 characters).
- Map records carry no elevation; DEM elevation is the ground at the coordinates (90 m cells), not where the butterfly flew, and coordinates with a large uncertainty (iNaturalist obscured points) can be far off. Records without a workbook elevation beyond the 1,000-point DEM budget are counted as "elevation unknown" and left out of an elevation filter, and the answer says how many.
- Dore et al. records are 0.25° grid cells (about 28 km), with no locality name: they show where a species occurs, not a collecting site, so they are ranked apart (`grid_records`) and never as a "site".
- Locality names differ between sources; sites are grouped by distance, and named after their commonest name.
- Memory: the five occurrence files are about 117 MB of JSON; each is read one at a time and kept as a compact list (about 10 MB for all), in each tools worker that needed it. Plant chunks: about 0.4 MB each once read (7.5 MB of JSON each to download).
- Plant records: the maps' GBIF download, not GBIF today (Mesechites trifida: 51 in Ecuador in the download; GBIF counted 146 on 29 Oct 2026, with no coordinate filter). Records within a few km of a border can be placed in the neighbour country.
- The gallery tool shows only Collection_data specimens (the gallery's Collection tab), not Insectary or CRISPR.
