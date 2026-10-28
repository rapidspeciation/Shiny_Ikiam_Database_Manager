# Demo: five questions answered with the maps and gallery tools

Run on 28 Oct 2026 with `node scripts/maps-tools-demo.mjs <app.sqlite> <out>`: the tools of `server/maps-tools.mjs` on the published maps site (records of 14 Oct 2026; GBIF and iNaturalist downloaded 26 Sep 2026) and a read-only copy of the app's database (the workbook as of 14 Oct 2026). The tool calls and their results are real; the replies are written from them the way the assistant's instructions (`assistant/skills/maps-gallery/SKILL.md`) ask, not produced by the deployed assistant, which does not have these tools yet. Times are those of the demo on this PC; the first call reads the five occurrence files (about 3 s).

Screenshots: the thumbnails of question 5 in a T3 Code chat (`assistant-tools-maps-gallery-t3-images.png`) and the map link of question 1's first site (`assistant-tools-maps-gallery-maps-link.png`).

---

## 1. "Where are the best places to find Oleria onega in Ecuador?"

`maps_localities { taxa: ["Oleria onega"], country: ["Ecuador"], limit: 8 }` (2.9 s): 869 records of the species, 368 in Ecuador, 57 sites, plus 40 Dore et al. grid cells.

> Ikiam is by far the best bet: 125 records (124 from our own collection, one on iNaturalist) over five years, 2022 to 2026, at about 615 m. [Map of Ikiam](https://rapidspeciation.github.io/ithomiini_maps/?sp=Oleria%20onega&source=Sanger%20Institute%2CGBIF%20%28UNAM%29%2CGBIF%20%28Other%20Institutions%29%2CDore%20et%20al.%20%282022%29%2CiNaturalist&map_view=-77.86603%2C-0.94855%2C12.00)
>
> Next, far fewer records:
> 1. Añangu, Boca del Río (Yasuní): 10 records (3 ours, 7 museum records), 2005 to 2023.
> 2. Cavernas Templo de Ceremonia: 9, all ours, 2023 to 2026.
> 3. Pañacocha: 8 iNaturalist observations, 2007 to 2024.
> 4. Puerto Libre, Línea 2: 7, ours, all in 2022.
> 5. Orellana province (iNaturalist, place not precise): 5, 2013 to 2026.
> 6. Sacha Waysa: 4 from three sources, 2017 to 2023.
>
> Records come from our collection, GBIF museum records, Dore et al. (2022) and iNaturalist, as published on the maps on 14 Oct 2026. [All Ecuadorian sites on the map](https://rapidspeciation.github.io/ithomiini_maps/?sp=Oleria%20onega&source=Sanger%20Institute%2CGBIF%20%28UNAM%29%2CGBIF%20%28Other%20Institutions%29%2CDore%20et%20al.%20%282022%29%2CiNaturalist&country=Ecuador&map_view=-77.38725%2C-1.58772%2C5.00)

Checked: the first link opens the maps app on Ikiam with Oleria onega selected and all five sources on, 869 records (screenshot).

## 2. "Where can we find Hypomenitis libethris between 1000 and 2000 m?"

`maps_localities { taxa: ["Hypomenitis libethris"], elevation_min: 1000, elevation_max: 2000, limit: 6 }` (1.1 s, Open-Meteo DEM asked for the records without a workbook elevation): 94 records, 45 in the band, none without an elevation.

> Four collecting sites in that band, all from our own collection (elevations from the workbook):
> 1. Wildsumaco, 1,523 m: 7 records, 2022. [Map](https://rapidspeciation.github.io/ithomiini_maps/?sp=Hypomenitis%20libethris&source=Sanger%20Institute%2CGBIF%20%28UNAM%29%2CGBIF%20%28Other%20Institutions%29%2CDore%20et%20al.%20%282022%29%2CiNaturalist&map_view=-77.60886%2C-0.67464%2C12.00)
> 2. Finca Narupa bridge, 1,109 m: 4 records, 2022.
> 3. San Isidro, Macas, 1,311 m: 3 records, 2022 and 2024.
> 4. Narupa birding, 1,228 m: 3 records, 2023.
>
> Dore et al. (2022) also place it in 17 grid cells of about 28 km in that band (the most in southern Ecuador around -3.9, -78.6, and north of Tena around -0.6, -77.6), which says where it occurs but not where to collect. Note the records are few and mostly from one year; the two Narupa sites are about 1 km apart, so together they hold as many records as Wildsumaco. [All of them on the map](https://rapidspeciation.github.io/ithomiini_maps/?sp=Hypomenitis%20libethris&source=Sanger%20Institute%2CGBIF%20%28UNAM%29%2CGBIF%20%28Other%20Institutions%29%2CDore%20et%20al.%20%282022%29%2CiNaturalist&map_view=-77.88332%2C-1.43613%2C6.00)

## 3. "What species have been recorded within 10 km of Ikiam?"

`maps_species { near: { lat: -0.95, lng: -77.86, km: 10 }, limit: 15 }` (10 ms once the files are read): 1,443 records, 155 taxa, 19 records not identified to species (counted apart).

> 155 taxa in 1,443 records. The most recorded:
>
> | Species | Records | Last year |
> |---|---|---|
> | Oleria onega | 138 | 2026 |
> | Hyposcada illinissa | 97 | 2026 |
> | Godyris zavaleta | 92 | 2026 |
> | Ithomia salapia | 91 | 2026 |
> | Methona confusa | 80 (41 on iNaturalist) | 2026 |
> | Hypothyris euclea | 79 | 2025 |
> | Hyposcada anchiala | 73 | 2026 |
> | Oleria gunilla | 69 | 2026 |
> | Mechanitis messenoides | 58 | 2026 |
> | Pseudoscada florula | 50 | 2026 |
>
> Most records are our own collection's. The list also has Heliconius numata (42), which the maps carry among the project's records although it is not an Ithomiini. [The first 20 on the map](https://rapidspeciation.github.io/ithomiini_maps/?sp=Oleria%20onega%2CHyposcada%20illinissa%2CGodyris%20zavaleta%2CIthomia%20salapia%2CMethona%20confusa%2CHypothyris%20euclea%2CHyposcada%20anchiala%2COleria%20gunilla%2CMechanitis%20messenoides%2CPseudoscada%20florula%2COleria%20tigilla%2CHeliconius%20numata%2CNapeogenes%20inachia%2CMechanitis%20polymnia%2CHypothyris%20anastasia%2CMechanitis%20mazaeus%2CCeratinia%20tutia%2CBrevioleria%20arzalia%2CBrevioleria%20aelia%2CTithorea%20harmonia&source=Sanger%20Institute%2CGBIF%20%28UNAM%29%2CGBIF%20%28Other%20Institutions%29%2CDore%20et%20al.%20%282022%29%2CiNaturalist&map_view=-77.86000%2C-0.95000%2C11.00)

(The link holds the first 20 species of the answer, 10 more than the table shows.)

## 4. "What does Mechanitis polymnia feed on? And which Ithomiini use Cestrum?"

`host_plants { butterfly: "Mechanitis polymnia" }` (34 ms) and `host_plants { plant: "Cestrum" }` (1 ms).

> **Mechanitis polymnia**: 61 associations, all Solanaceae: Solanum (48), Cyphomandra (7), Lycopersicon (2), and one each of Brugmansia, Brunfelsia, Datura and Jaltomata. 36 are direct observations at species level, for example *Cyphomandra hartwegii* in Ecuador (Drummond & Brown 1987, ssp. dorissides), *Solanum asperum* in Costa Rica (Drummond & Brown 1987), *Cyphomandra velutina* in Brazil (Carvalho & Vasconcellos-Neto 2021). Most rows come from the Beccaloni et al. (2008) catalogue ([PDF](https://www.researchgate.net/profile/George-Beccaloni-2/publication/262099000_Catalogue_of_the_Hostplants_of_the_Neotropical_Butterflies_Catalogo_de_las_Plantas_Huesped_de_las_Mariposas_Neotropicales/links/5cc0d98a92851c8d2202f3f1/Catalogue-of-the-Hostplants-of-the-Neotropical-Butterflies-Catalogo-de-las-Plantas-Huesped-de-las-Mariposas-Neotropicales.pdf)), which records published hosts, many from Brazil or Central America: not proof of what it uses around Tena.
>
> **Cestrum**: 57 associations (medium confidence or better; 3 more of low or unchecked confidence left out), mostly Greta (19), Pseudoscada (7), Mcclungia (6), Episcada and Heterosais (5 each), Godyris and Pteronymia (4 each). From Ecuador: *Godyris zavaleta* on *Cestrum laevigatum* (Drummond & Brown 1987, ssp. matronalis, via Beccaloni et al. 2008), and *Pseudoscada timna* and *Heterosais giulia* on Cestrum not identified to species.

## 5. "Show me female Methona confusa from the collection, and Melinaea from Ikiam"

`gallery_specimens { species: ["Methona confusa"], sex: "female", limit: 8, thumbnails: 4 }` (10 ms): 12 females with photos. `gallery_specimens { genus: ["Melinaea"], locality: "Ikiam", limit: 5, thumbnails: 2 }`: 5.

> 12 females with photos; the newest four:
>
> [![CAM078677 d](https://wsrv.nl/?url=https%3A%2F%2Fdrive.google.com%2Fuc%3Fid%3D1z2RsDxSvTITIHuoGw5ecxYGMmuZV0MuB&w=160&output=webp)](https://lh3.googleusercontent.com/d/1z2RsDxSvTITIHuoGw5ecxYGMmuZV0MuB=w1600) [![CAM078677 v](https://wsrv.nl/?url=https%3A%2F%2Fdrive.google.com%2Fuc%3Fid%3D1cU5g2_kyk8Z6EBWrv2WZ5pGNr3pj2KQn&w=160&output=webp)](https://lh3.googleusercontent.com/d/1cU5g2_kyk8Z6EBWrv2WZ5pGNr3pj2KQn=w1600)  
> **CAM078677** Methona confusa, Panamanian segregate, female, 1.3 Km Sur de San Miguel Arriba, Valle de Antón, 497 m
>
> (CAM078594, CAM078593, CAM078592 follow the same way, from Loma Grande, Valle de Antón.)
>
> The four newest are from Panama; the Ecuadorian ones are CAM077552 (Puesto de Control Pindo) and CAM077318 (Ikiam), both ssp. psamathe. [All 12 in the gallery](https://rapidspeciation.github.io/Shiny_Ikiam_Wings_Gallery/?view=1&species=Methona%20confusa&sex=female)
>
> Melinaea from Ikiam: five Melinaea satevis maeonis (one male, four females) collected from 2023 to 2025 at about 615 m. [See them in the gallery](https://rapidspeciation.github.io/Shiny_Ikiam_Wings_Gallery/search?view=1&camids=CAM077728%20CAM077691%20CAM074854%20CAM072066%20CAM071580)

Checked in a browser: the first gallery link shows 13 Methona confusa females (one more than the tool found in the 14 Oct copy of the workbook: the live gallery is newer), the second the five CAM IDs. The thumbnail Markdown, pasted into a T3 Code chat, shows each specimen's two photos at 160 px, each linked to its full photo (screenshot).
