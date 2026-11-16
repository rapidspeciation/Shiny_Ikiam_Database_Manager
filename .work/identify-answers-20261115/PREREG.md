# Identificar thresholds: rule fixed before any dev numbers (15 Nov 2026)

Written by the lane lead before any model was run on the evaluation photos. The measurement worker may add
data facts (pool sizes, leak checks) below the line, but may not change this rule after scoring starts. Any
deviation is logged in a dated "Deviations" section with its reason, before the scores it affects are read.

## Engines
- E1 phone small: student_SB_int8.onnx (182 px), the phone's default.
- E2 BioCLIP: BioCLIP 2.5-H int8 + head 6fa6ab78 on the wing_seg_s2 crop (server, and the phone's «Más preciso»
  with the fp16 head; one set of thresholds for both unless their named answers differ on more than 1 % of photos).
- E3 Wings Gallery: BioCLIP 2.5-H fp32 + head b8127dbf on the V5N crop (offline; parity checked against a few
  read-only Space calls at one call per 3 s or slower).

## Photos
- Labelled photos NOT in any locked or test split (field-sex TEST split, collection locked split, and the S1
  fresh set stays closed too) and NOT in the training or selection data of the head being scored (data_v1/v2 train
  and dev, alias set, top-ups): check by photo id, URL and file hash, and drop any overlap.
- Domains: field (live, iNat-type), in-hand (hand or forceps), collection (spread). Field + in-hand pooled is the
  primary pool; collection is reported, not used to choose.
- Truth not in the head's leaf list stays in: a named species is then wrong, a named genus is right only if the
  genus matches.
- Names are scored as the app shows them (strict). The 44 safe duplicate groups as synonyms: secondary only.
- Prior: the app's default setting for the engine (if the app applies an Andes-side prior only when the user picks
  a side, primary = no prior). True-side prior: secondary.
- Two halves split by observer (or by specimen/collector where there is no observer), seed 20261115: FIT and CHECK.

## Answers scored
- Species named when the top species' score >= t_s. Right if it is the true species.
- Genus named (species not named) when the genus total of the top species >= t_g. Right if it is the true genus.
  The top-genus variant is descriptive only.
- Tribe, subfamily: CLEAR 0.80 kept (S1-confirmed); report their precision descriptively.

## Choice of thresholds (per engine, on FIT, field + in-hand pooled)
- Grid: 0.30, 0.35, ..., 0.95.
- t_s = the smallest grid value where species precision >= 0.90 and its Wilson 95 % lower bound >= 0.87, and
  every higher grid value with >= 30 named photos also has precision >= 0.90.
- t_g = the same with genus precision >= 0.95 and Wilson lower bound >= 0.92, counting only photos where the species
  is not named at t_s (the genus rung as the ladder uses it).
- None qualifies: 0.95 for that rank, flagged.
- In-hand guard: when in-hand has >= 50 named photos at the chosen t, its precision must be >= target - 0.05;
  otherwise raise t one grid step at a time until it holds (or 0.95).

## Check (CHECK half, read once)
- Pass if precision at the chosen t is >= target - 0.02 (species 0.88, genus 0.93). Report Wilson intervals and
  coverage, per domain.
- A fail ships max(t, 0.80) for that engine and rank.
- The shipped thresholds are the passing values, rounded to the grid. No refit on FIT + CHECK.

## What is reported (the answer to Franz's question)
- Per engine: coverage and precision at species, genus and tribe for every grid value, both halves, per domain.
- Whether E1 reaches species or genus less often than E2 and E3 because of the threshold (same t, compare top-1
  accuracy and coverage) or because it is weaker (lower top-1 accuracy at the top choice regardless of t).
- How often the gallery's always-named top species is right, overall and when its score is below 0.5.

---

## Deviations

### D1 (15 Nov 2026, logged before any engine score on the evaluation photos was read)
Lead's change (Franz approved display-time name merges for the same release). No primary score had been read
when this was written (no engine had yet been run on any evaluation photo).
- **Primary scoring for E1 and E2 is the merged display:** each leaf's score is summed under its merged name before
  the species and genus totals are formed, and truth names go through the same map, so names are scored merged.
  The map: the 44 "all sources agree" groups of WingsClassificator `reports/taxonomy_guide_20261113/
  names_20261114T0340Z/duplicates.tsv` (every listed leaf -> `merged_name`), plus *Mechanitis messenoides
  holmgreni* and *Mechanitis mazaeus holmgreni* -> *Mechanitis messenoides deceptus*. NOT *Mechanitis polymnia
  proceriformis* / *eurydice* -> *dorissides*. Species of a merged leaf = the first two words of its merged name;
  genus = the first word.
- Truth names: a trinomial truth is mapped through the leaf map first; a binomial truth is mapped only when every
  head leaf of that species maps to one merged species (else it stays as it is).
- **Strict unmerged scoring becomes secondary** for E1 and E2 (it replaces "Names are scored as the app shows them
  (strict)" as the primary line). The "44 safe duplicate groups as synonyms: secondary only" line is superseded
  for E1 and E2.
- **E3 (gallery):** scored both ways, merged and strict, for comparison; the gallery itself does not merge, so its
  answer to Franz's question uses strict as shipped, and merged is the like-for-like comparison with E1/E2.
- thresholds.json states the scoring used (merged for E1 and E2).

### D2 (15 Nov 2026, logged before any engine score on the evaluation photos was read)
Lead's change (Franz): the name merges of D1 are cancelled; the apps keep strict leaf names until a retrained head
ships. No score of any kind (merged or strict) had been read when this was written: no engine had yet been run on an
evaluation photo, so nothing here was chosen with knowledge of the numbers.
- **Primary scoring returns to strict names as shipped** for E1, E2 and E3. The D1 merged display (same map as in D1)
  is **secondary** for all three engines.
- Product change: the app will ALWAYS show the top species and its genus as the guess, as the gallery does; t_s and
  t_g now decide where the label «Incierto (NN %)» starts. The rule for choosing t_s and t_g is unchanged.
- Added report (descriptive, no decision attached): per engine and per domain, how often the top species is right
  and how often the top species' genus is right, in the score bands 0-0.3, 0.3-0.5, 0.5-t_s and >= t_s (bands by the
  top species' score for the species line; by the top species' genus total for the genus line, with the same edges
  and t_g in place of t_s), with Wilson intervals, on FIT and CHECK and pooled.
- thresholds.json states "scoring": "strict".

### D3 (16 Nov 2026 UTC, logged before any engine score on the evaluation photos was read)
- **In-hand halves by collection day, not by photographer.** The team in-hand photos come from only 3
  photographers (99, 93 and 44 photos), so an observer split cannot make two comparable
  halves. They are split by collection day (63 days; all photos of one specimen fall on one day), seed 20261115.
  Field (iNaturalist) and collection photos keep the observer split.
- **E3 prior: none.** The gallery applies a location prior only from the photo's EXIF GPS or a place the user picks;
  3 of the downloaded field photos (of 1,216 checked so far) and 78 of 236 in-hand photos carry GPS. The gallery's
  coordinate prior (its GBIF cell files) is not reproduced here, so E3 is scored without a prior for every photo;
  the 78 GPS-tagged in-hand photos are flagged so their E3 numbers can be read with that limit.

## Pool facts (16 Nov 2026 UTC, written before any engine score on these photos was read)
- **Field (iNaturalist):** the 2,936 post-cutoff research-grade field butterflies of the V5N benchmark (observed
  2026; burned as a decision set for earlier releases, never a locked or test split). Checked against every row of
  the step-4 feature store (723,870 rows: data_v2 train and dev pools, alias train and dev, top-ups; the store the
  heads 6fa6ab78 and b8127dbf were trained from) by candidate id, image SHA-256, iNaturalist photo id in any URL of
  the step-4 inputs, and BioCLIP feature cosine. Dropped: 190 by SHA-256 (189 in the top-ups, 1 in the alias set)
  and 110 more by feature cosine >= 0.98 (rule fixed here before scoring: same image re-encoded or another photo of
  the same individual). **Kept 2,636 photos, 1,634 observers** (FIT 1,306, CHECK 1,330). All originals re-fetched
  from their public URLs and kept only when their SHA-256 equals the benchmark ledger's.
- **In-hand (team):** 236 photos of 229 specimens held in the hand, from the team's Google Photos and Wikiloc
  waypoints, tied to one database row by a written field ID, wing label or CAM label (no model involved in the
  tie; every validation tier kept, including 29 rows whose database species the earlier identifier disagreed with,
  so that no photo is selected by a model's answer). Truth = the database species. 3 photographers, 63 collection
  days (FIT 113, CHECK 123 by day, D3). 0 overlaps with the step-4 store by id, SHA-256 or cosine >= 0.98. 78 carry
  EXIF GPS. None is in the field-sex TEST split or the collection-sex locked split (their split column read only
  to exclude; 0 rows were in either).
- **Collection:** none clean. All 282 museum photos of the benchmark are in the training set (280 by id and SHA-256
  in gbif_c100, the other 2 at cosine 0.95-0.98). No collection domain is scored.
- **Phone small model (E1) training data:** the S_B student's 195,233 training rows (train3.jsonl) are all step-4
  rows except 3 (2 BoA plates, 1 alias photo; none in the pool by key), so the step-4 check covers E1 too.
- Not used: the S1 fresh set, the field-sex TEST split and the collection locked split stay closed (not opened).
  The database's photo-link sheet holds spread-wing collection photos (the training's pub_sanger source), not
  in-hand photos.

### D4 (16 Nov 2026 UTC, logged before any engine score on the evaluation photos was read)
- **Every engine reads the original photo file.** Both apps re-encode before sending to their servers (the gallery:
  canvas, longest edge at most 1,600 px, JPEG 0.85; Identificar online: canvas, JPEG 0.9), and a browser's
  resampling cannot be reproduced exactly here. The detector runs at 640 px and the encoder at 224 px, so the
  effect should be small; it is the same kind of difference for E2 (server) and E3, and none for the phone engines
  where the phone reads the photo itself. Parity with the live Space is checked on originals.

### D5 (16 Nov 2026, lead, written AFTER the scores were read: a product decision, not a confirmatory result)
The rule above chose the cut for a ladder that hides the species below it (overall precision of named species). On
15 Nov Franz changed the product: the top species is always shown, and the cut only decides whether it carries the
label «Incierto». A cut is then a claim about each single answer, so it should hold for the scores just above it,
not only on average. By the rule's own numbers, a species at 0.35 to 0.5 is right 56 to 67 % of the time, yet would be
shown without «Incierto». The lead therefore ships, for both app engines:
- species «Incierto» below **0.70**: narrow bands reach about 90 % right from 0.70 to 0.80 for all three engines
  (0.60 to 0.70: 79 to 88 %); 0.80 and up 97.8 to 97.9 %;
- genus «Incierto» below **0.80** (unchanged): genus right 97 to 98 % from 0.80, 86 to 94 % from 0.50 to 0.80 (table
  C). The rule's 0.95 is an artifact of scoring the genus rung only on photos the 0.35 species cut leaves over;
- tribe and subfamily 0.80 (unchanged; 100 % right where they answered).
These values were chosen with the numbers in view, so they are development grade, not checked on unseen photos.
The rule's own result (0.35 / 0.95) stays in thresholds.json and REPORT.md.
