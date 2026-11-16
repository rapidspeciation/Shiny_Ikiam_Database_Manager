# Identificar thresholds, measured (16 Nov 2026)

## Answer first

**Franz asked:** the Wings Gallery reaches genus and species where the phone often stops at tribe or genus. Is that fixed?

1. **It is the threshold, not a weaker phone model.** At today's cut (0.80), the phone's small model names a species
   about as often as BioCLIP and the gallery's own model would at that cut, and with the same precision:
   - field photos: small 60 %, BioCLIP 61 %, gallery model 62 % of photos (all about 98 % right);
   - team photos held in the hand: small 46 %, BioCLIP 41 %, gallery model 40 % (97 to 99 % right).

   The gallery names a species on 100 % of photos only because it has no cut at all. On in-hand photos the 0.80 cut
   leaves about a third of photos at the tribe, which is what Franz sees.
2. **The small model is a little weaker in its first choice on field photos, not on in-hand ones.** Its top species is
   right on 82.7 % of field photos, against 87.0 % for BioCLIP and 86.8 % for the gallery. On in-hand photos it is
   right on 84.3 %, against 80.9 % and 81.4 % (n 236, so the intervals overlap). The difference shows mostly when the
   model is unsure: below a score of 0.5, the small model's top species is right 39 % of the time, BioCLIP's 52 %.
3. **The gallery's always-named species** is right on 86.4 % of all photos (field 86.8 %, in-hand 81.4 %), and its
   genus on 93.1 %. **When its score is below 0.5, the named species is right on 53.9 %** (field 52.9 %, in-hand
   60.8 %; 1 photo in 5 is in that range), and its genus is right on 77.9 %.
4. **New cuts by the preregistered rule** (species precision at least 90 % on the FIT half; CHECK half read once,
   all species checks passed):
   - species from **0.35** for the phone's small model and for BioCLIP (server and «Más preciso», one set: their
     answers differ on 2 of 2,872 photos);
   - species from 0.30 for the gallery model (descriptive only).
   - The **genus cut stays 0.95, flagged**: no grid value qualified. Once species is named from 0.35, only about 1 photo
     in 9 is left for the genus rung, and those are the hard ones.
5. **What that buys and costs.** Species coverage goes from about 60 % to about 89 % of field photos and from 41 to
   46 % to 81 to 86 % of in-hand photos. Overall precision of named species goes from about 98 % to about 90 %: 1
   named species in 10 is then wrong, against 1 in 50 today. A rule on overall precision hides the weakest band:
   species named between 0.35 and 0.5 are right only 56 to 67 % of the time (table B). In narrow 0.05 bands, a
   species is right about 9 times in 10 only from a score of about 0.70 to 0.80, for all three engines (0.60 to 0.70:
   79 to 88 %). If Franz wants every species shown as an answer to be that safe, the cut stays near 0.70 to 0.80.
   That is a product choice; the rule as written gives 0.35.
6. **Words for «Incierto (NN %)»** (table B). Below 0.3 the top species is right 1 time in 4 (small model) to 2 in 5
   (BioCLIP, gallery), and its genus 2 in 5 to 3 in 5. Between 0.3 and 0.5 the species is right about half the time
   (small model about 1 in 2, BioCLIP 2 in 3), and its genus 3 to 4 times in 5. Honest wording: below 0.3 "una de
   varias posibilidades"; 0.3 to 0.5 "posible, acierta más o menos la mitad de las veces"; the genus line can say
   "el género suele ser correcto" from 0.3 up.

## What was measured

- **Photos (none in any training, selection, locked or test set):** 2,636 iNaturalist field butterflies (1,634
  observers, observed 2026) and 236 team photos of 229 specimens held in the hand, each tied to a database row by a
  written field ID, wing label or CAM label (no model in the tie, all tiers kept). Every photo was checked against
  all 723,870 rows the heads and the small model trained on, by id, SHA-256, photo id and image feature: 300 field
  photos were dropped (190 in the top-ups or alias set, 110 near-duplicates). **No collection photo survived**: all
  282 museum photos of the old benchmark are in training. Details: PREREG.md, "Pool facts".
- **Engines:** E1 the phone's small model (S_B int8, phone fp16 head); E2 BioCLIP int8 + head 6fa6ab78 on the
  wing_seg_s2 crop (server fp32 head and phone fp16 head); E3 the gallery chain (BioCLIP fp32 + head b8127dbf on the
  V5N crop). E3 matches the live Space to within 5e-6 on 3 new photos and 2e-6 on the 13 Nov photo.
- **Scoring:** strict leaf names as shipped, no prior (the app's default side is "none"). Merged names (secondary)
  give the same cuts. Halves: field by observer, in-hand by collection day (D3). Every rule change (D1 to D4) was
  written in PREREG.md before any score was read.

## Limits

- The field photos are butterflies from around the world. On the 134 Ithomiini among them the picture is the same:
  top species right 90 to 91 % for all three engines; at 0.80 the small model names more of them (69 % against 58
  to 59 %).
- In-hand: 236 photos from 3 photographers, so intervals are wide (about ±5 points overall, ±15 in a band).
- The field set was read at earlier releases (development grade, not trained on). No collection photos.
- Every engine read the original file; both apps re-encode the photo before sending it to their servers (D4). The
  gallery's GPS prior (78 in-hand photos carry GPS) was not reproduced (D3).
- The tribe rung (0.80) was 100 % right wherever it answered (n 24 to 107 per engine and domain).

## Tables (precision % [Wilson 95 %], n; coverage = share of all photos)

**A. The ladder today (0.80 / 0.80) and at the new cuts, all photos (FIT + CHECK)**

| engine | domain | species at 0.80: coverage, precision | genus at 0.80 | tribe at 0.80 | species at new t_s: coverage, precision |
|---|---|---|---|---|---|
| E1 small | field | 60.2 %, 97.7 [97, 98] | 20.3 %, 99.1 | 3.7 % | 88.7 %, 90.1 [89, 91] |
| E1 small | in-hand | 46.2 %, 99.1 [95, 100] | 17.4 %, 95.1 | 32.6 % | 84.8 %, 90.5 [86, 94] |
| E2 BioCLIP | field | 61.3 %, 98.0 [97, 99] | 14.1 %, 97.3 | 3.9 % | 90.0 %, 92.3 [91, 93] |
| E2 BioCLIP | in-hand | 41.1 %, 96.9 [91, 99] | 14.0 %, 93.9 | 39.8 % | 80.9 %, 87.4 [82, 91] |
| E3 gallery | field | 61.7 %, 97.9 [97, 98] | 13.7 %, 97.5 | 4.1 % | 90.6 %, 91.8 [91, 93] |
| E3 gallery | in-hand | 39.8 %, 96.8 [91, 99] | 16.1 %, 100 | 39.4 % | 86.0 %, 86.7 [81, 91] |

CHECK half, field + in-hand, species at the new t_s (pass bar 88 %): E1 89.4 [87.6, 91.0] n 1,279; E2 91.2 [89.6,
92.7] n 1,302; E3 90.9 [89.2, 92.3] n 1,313. The genus rung had 4, 2 and 0 CHECK photos.

**B. The always-shown guess by score band, field + in-hand (n 2,872): top species right / its genus right**

| top-species score | E1 small | E2 BioCLIP | E3 gallery | share of photos |
|---|---|---|---|---|
| 0 to 0.3 | 23.2 [19, 29] / 38.0 | 37.5 [32, 44] / 61.3 | 40.2 [35, 46] / 63.7 | 9 to 10 % |
| 0.3 to 0.35 | 41.9 [30, 54] / 74.2 | 59.7 [47, 71] / 82.5 | (in 0.3 to 0.5) | 2 % |
| 0.35 to 0.5 | 56.0 [49, 62] / 85.0 | 66.7 [60, 73] / 92.2 | 67.7 [62, 73] / 92.1 (0.3 to 0.5) | 8 to 10 % |
| 0.5 to 0.8 | 81.7 [78, 85] / 95.5 | 84.6 [82, 87] / 95.4 | 83.6 [80, 86] / 95.3 | 21 to 22 % |
| 0.8 to 1 | 97.8 [97, 98] / 97.3 | 97.9 [97, 98] / 97.5 | 97.9 [97, 98] / 97.3 | 59 to 60 % |

In-hand only (n 236), same bands: E1 55.6 / 63.0 (n 27), 33.3 / 66.7 (n 9), 65.2 / 87.0 (n 23), 85.3 / 89.7 (n 68),
99.1 / 99.1 (n 109). E2 48.6 / 73.0 (n 37), 75.0 / 87.5 (n 8), 74.2 / 90.3 (n 31), 79.4 / 92.1 (n 63), 96.9 / 96.9
(n 97). E3 48.5 / 63.6 (n 33), 70.7 / 95.1 (n 41, 0.3 to 0.5), 82.3 / 92.7 (n 68), 96.8 / 96.8 (n 94).

**C. Genus line by the top species' genus total, field + in-hand: genus right**

| genus total | E1 | E2 | E3 |
|---|---|---|---|
| 0 to 0.3 | 18.3 (n 175) | 42.0 (n 150) | 45.6 (n 160) |
| 0.3 to 0.5 | 55.8 (n 120) | 78.5 (n 130) | 82.2 (n 152) |
| 0.5 to 0.8 | 86.0 (n 306) | 94.3 (n 474) | 93.4 (n 440) |
| 0.8 to 0.95 | 98.4 (n 735) | 97.6 (n 965) | 97.4 (n 972) |
| 0.95 to 1 | 97.3 (n 1,536) | 97.3 (n 1,153) | 97.4 (n 1,148) |

Full grids (every t from 0.30 to 0.95, FIT, CHECK and all, per domain, merged and strict): `tables.md`,
`results.json`. Thresholds: `thresholds.json`.
