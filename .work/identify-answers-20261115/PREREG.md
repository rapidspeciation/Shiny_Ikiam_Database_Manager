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
