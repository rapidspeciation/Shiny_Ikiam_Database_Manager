---
name: maps-gallery
description: Where to find a species (localities ranked by records in the Ithomiini maps, by region and elevation), which species live in a place, host plants of a butterfly or the Ithomiini on a plant (with where each plant grows in a country or area, its photos and a Wings Atlas link with the host-plant layer on), and collection specimens with their wing photos (Wings Gallery). Use it for "where can we find X?", "what lives around Y?", "what does X feed on?", "show me photos of X's host plants in Ecuador", "a map of these plants", "show me photos of X", with links that open the maps or the gallery already filtered.
---

# Maps, host plants and the Wings Gallery

Five read-only tools; none changes the workbook. Use them before any outside API: they hold the maps' butterfly records, host plants, the plants' GBIF records and photos, and build the map links. Call GBIF, iNaturalist or other sites only for what they do not have, and say so.

| Question | Tool |
|---|---|
| Where are the best places to find X (in a country, near a place, between two elevations)? | `maps_localities` |
| What has been recorded around a place, or above some elevation in a region? | `maps_species` |
| What plants does X use? Which Ithomiini are on this plant genus? | `host_plants` |
| Which of X's host plants grow in Ecuador (near Tena, in a box)? Photos of them, where to collect them, a map of their records | `host_plants` with `country`, `near` or `bbox` (one call) |
| Near Ikiam (or another point), which site should I go to first to collect X's host plants? Where around here? | `host_plant_sites` with `near` {lat, lng, km} (Ikiam is about -0.95, -77.86); `rank: "grid"` for cells instead of the project's localities |
| Show me specimens or photos of X (a sex, a locality, CAM IDs) | `gallery_specimens` |

## Answering

- **Where to find a species.** Give the first few sites with their records, years seen (a site with records over many years is a better bet than one busy day) and elevation, and each site's map link. Say which sources the records come from: the project's own collection (Sanger Institute), museum records (GBIF), Dore et al. (2022), iNaturalist. Dore et al. records are 0.25° grid cells (about 28 km): say where the species occurs from them, never a site to visit. End with the link to the map with all of them and the data's date (`freshness`).
- **Elevation.** The project's records use the workbook's ELEVATION; the others the ground's elevation at their coordinates (a 90 m DEM). Say so when an elevation band was asked, and how many records had no elevation.
- **Host plants.** Give each plant with its confidence and its citation and link. Catalogue rows (Beccaloni et al. 2008) are published records, often from far away: not evidence that the butterfly uses that plant at a given place. A genus- or family-level host means the plant species is unknown.
- **Host plants in an area.** One `host_plants` call with the area gives each plant's records there, years and top sites (coordinates and the nearest named collecting place), the best sites for all of them, photo thumbnails (`markdown`) and `map_url`: the Wings Atlas with the host-plant layer, those plants and the country on. Name the plants with no records there too. The records are the maps' GBIF download (its date in `records_note`), so they can be fewer than GBIF shows today. Always say that a plant growing somewhere is not evidence that the butterfly uses it there. Genus-level hosts are counted but left off the sites and map: their records are of any species of the genus.
- **Which site first.** `host_plant_sites` ranks the project's collection localities (or 1–2 km cells) by the host plants' records around them, nearer records weighing more. For each site you recommend give its score with the records behind it (within 2, 5 and 10 km), the nearest record (plant, distance, year), the oldest and newest years, its elevation and the project's records of the butterfly there; then its photo thumbnails (`markdown`) and the `map_url`. Always say: GBIF and iNaturalist records follow roads, stations and observers, so a high score can mean a well-sampled place rather than more plants; records from decades ago may be gone; and a plant record is not evidence that the butterfly uses the plant there. Few records behind a score (one or two) is a weak lead: say so.
- **Photos.** Put the tool's `markdown` in the reply as it is: the chat shows those thumbnails, each linking to its full photo, with the credit line. Add the gallery link for the rest. A specimen added to the workbook since the gallery's last update is in the answer but not yet on the gallery site (`update_websites` brings it).
- **Showing them.** On this server `html_preview` does not work (its browser is blocked): never use it. The `markdown` thumbnails in the reply are the way to show photos; `html_render` works if a laid-out page is really needed.
- Names: a species is "Genus species"; a subspecies "Genus species subspecies". A name the maps do not know comes back in `not_in_maps`: check the spelling with the person, or the workbook's Taxonomy sheet.
