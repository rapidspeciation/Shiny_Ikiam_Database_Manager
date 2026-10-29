# Assistant tools: maps, host plants, Wings Gallery

Four read-only tools that let the assistant (Asistente tab) answer questions such as "where are the best places to find Oleria onega?", "what does Mechanitis feed on?" or "show me female Melinaea from Tena", with links that open the public sites already filtered. Code: `server/maps-tools.mjs`. Tests: `tests/maps-tools.test.mjs`.

None of them writes anything: they read the public sites' data files over HTTPS and the app's own database with `SELECT` only. What they keep is in memory, in the tools' worker (`server/assistant-host.mjs`: they are not in `MAIN_TOOLS`, so a 20 MB file read never holds up the app's thread).

## Data sources and how fresh they are

| Data | Where the tool reads it | How fresh |
|---|---|---|
| Occurrence records of the maps (`rapidspeciation.github.io/ithomiini_maps`) | `data/data_manifest.json` and the five `data/map_points_*.json` it lists: Sanger Institute (the project's own collection, 7,850 records), GBIF (UNAM) 21,562, GBIF (other institutions) 26,956, Dore et al. (2022) 28,927, iNaturalist 20,424 | As published by the maps' «Update Database» run (`update_websites`). The project's records follow the workbook when that run is started; the GBIF and iNaturalist downloads only when it runs with `gbif: true` (last on 26 Sep 2026, `generated_at` of the manifest). The tool re-reads a file at most every 6 hours, and only if its ETag changed. Each answer says the manifest's date and each file's Last-Modified. |
| Host plants | `data/host_plants/host_plant_associations.json` of the same site (863 butterfly-plant associations, 16 Jun 2026) | Changes only when the maps' host plant review is rebuilt. Same 6-hour check. |
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

### `maps_species`
The taxa recorded in a region, most records first ("what can we find around Mindo above 1,500 m?"). Same region, elevation, sources and years parameters; `genus` narrows it; `limit` default 30, at most 100. Each taxon with its records, sites, and last year seen, and a map URL.

### `host_plants`
Host plant associations from the maps' review, with sources.

- `butterfly`: species, subspecies or genus: its host plants.
- `plant`: plant family, genus or species: the Ithomiini recorded on it.
- `min_confidence`: `low`, `medium` (default), `high`; `needs_check` rows only with `include_unchecked: true`.

Answer: each association with the butterfly taxon, the plant (family, genus, species, as written in the source), host level (species, genus, family), evidence level (direct, literature), confidence, the citation, its DOI or URL, and the primary references (cut to 400 characters). Grouped and counted by plant genus (or butterfly genus). A note when only family-level or catalogue rows exist: the review says not to treat a catalogue row as observed local use.

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

Maps (state already in its URL, `src/stores/filterStore.js` and `viewStore.js` of ithomiini_maps; no change to that app was needed):

```
https://rapidspeciation.github.io/ithomiini_maps/?sp=Oleria%20onega&source=Sanger%20Institute,iNaturalist&map_view=-77.81000,-0.99000,11.00
```

- `sp`: species, comma separated (the app's `scientific_name`); `ssp`: subspecies; `genus`: genus list.
- `source`: the sources' names as the app shows them (`Sanger Institute`, `GBIF (UNAM)`, `GBIF (Other Institutions)`, `Dore et al. (2022)`, `iNaturalist`). Left out it shows only Sanger Institute, so the tools always set it.
- `country`: comma separated. `locality`: a JSON array of exact locality names.
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

T3 Code (0.1.3, the server's version) renders Markdown images with https sources in assistant replies: tested on 28 Oct 2026 with a reply holding a wsrv.nl and an lh3 thumbnail of CAM036744, both shown at 160 px (screenshot `docs/assistant-tools-maps-gallery-t3-images.png`). The page sets no Content-Security-Policy, so the images load inside the Asistente frame too. `gallery_specimens` therefore returns ready Markdown, each image linked to its full photo.

## Limits

- An answer stays within the tools' common budget (`server/tool-budget.mjs`, 38,000 characters).
- Map records carry no elevation; DEM elevation is the ground at the coordinates (90 m cells), not where the butterfly flew, and coordinates with a large uncertainty (iNaturalist obscured points) can be far off. Records without a workbook elevation beyond the 1,000-point DEM budget are counted as "elevation unknown" and left out of an elevation filter, and the answer says how many.
- Dore et al. records are 0.25° grid cells (about 28 km), with no locality name: they show where a species occurs, not a collecting site, so they are ranked apart (`grid_records`) and never as a "site".
- Locality names differ between sources; sites are grouped by distance, and named after their commonest name.
- Memory: the five occurrence files are about 117 MB of JSON; each is read one at a time and kept as a compact list (about 10 MB for all), in each tools worker that needed it.
- The gallery tool shows only Collection_data specimens (the gallery's Collection tab), not Insectary or CRISPR.
