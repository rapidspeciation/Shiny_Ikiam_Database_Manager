---
name: maps-gallery
description: Where to find a species (localities ranked by records in the Ithomiini maps, by region and elevation), which species live in a place, host plants of a butterfly or the Ithomiini on a plant, and collection specimens with their wing photos (Wings Gallery). Use it for "where can we find X?", "what lives around Y?", "what does X feed on?", "show me photos of X", with links that open the maps or the gallery already filtered.
---

# Maps, host plants and the Wings Gallery

Four read-only tools; none changes the workbook.

| Question | Tool |
|---|---|
| Where are the best places to find X (in a country, near a place, between two elevations)? | `maps_localities` |
| What has been recorded around a place, or above some elevation in a region? | `maps_species` |
| What plants does X use? Which Ithomiini are on this plant genus? | `host_plants` |
| Show me specimens or photos of X (a sex, a locality, CAM IDs) | `gallery_specimens` |

## Answering

- **Where to find a species.** Give the first few sites with their records, years seen (a site with records over many years is a better bet than one busy day) and elevation, and each site's map link. Say which sources the records come from: the project's own collection (Sanger Institute), museum records (GBIF), Dore et al. (2022), iNaturalist. Dore et al. records are 0.25° grid cells (about 28 km): say where the species occurs from them, never a site to visit. End with the link to the map with all of them and the data's date (`freshness`).
- **Elevation.** The project's records use the workbook's ELEVATION; the others the ground's elevation at their coordinates (a 90 m DEM). Say so when an elevation band was asked, and how many records had no elevation.
- **Host plants.** Give each plant with its confidence and its citation and link. Catalogue rows (Beccaloni et al. 2008) are published records, often from far away: not evidence that the butterfly uses that plant at a given place. A genus- or family-level host means the plant species is unknown.
- **Photos.** Put the tool's `markdown` in the reply as it is: the chat shows those thumbnails, each linking to its full photo. Add the gallery link for the rest. A specimen added to the workbook since the gallery's last update is in the answer but not yet on the gallery site (`update_websites` brings it).
- Names: a species is "Genus species"; a subspecies "Genus species subspecies". A name the maps do not know comes back in `not_in_maps`: check the spelling with the person, or the workbook's Taxonomy sheet.
