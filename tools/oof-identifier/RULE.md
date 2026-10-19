# OOF species: pre-written rule (19 Oct 2026, written before any OOF result exists)

Evaluation set ("trusted label"): CAMs of the 18 Oct Photo_links set whose workbook species (Collection_data, else
insectary/CRISPR sheets, binomial of the recorded name, as build_meta.py) is a plain binomial (no sp./spp./cf./aff.,
no "?", no hybrid / " x "), is a species of the head's vocabulary (head_hier 749fbffd, 11,965 species), and, for CAMs
with training rows, equals the species of their training rows.

Predictions compared (both out-of-fold, same footing): 5 folds grouped by CAM, fold = sha256("oof20261019|" + CAM) % 5,
for every CAM (training or not). Fold k heads are trained with the mf_topup60 recipe, release setting, seed 1701,
without the training rows of fold k's CAMs, and predict every CAM of fold k.
  (a) feature averaging: the CAM's photos' features (identical copies once, as 18 Oct), mean, renormalised, single head;
  (b) paired view: a 2048-d head of the same recipe on [dorsal, ventral] / sqrt 2 (dorsal = mean of the d* photos,
      ventral = mean of the v* photos; a missing view duplicates the other; training rows of a CAM with a d and a v row
      become one paired row, every other row is duplicated).
Both after the Space's geographic prior (side of the Andes / Ecuador, eps 0.02).

Metrics on the evaluation set: species Top-1 (top species after the prior == workbook species), and calibration = ECE
of the species Top-1 confidence (15 equal-width bins).

Decision:
1. d = Top-1(b) - Top-1(a) in percentage points, n = (#CAMs b right) - (#CAMs a right).
2. If |d| < 0.2 pp or |n| < 10: tie -> (a) (simpler).
3. Otherwise the higher Top-1 wins, unless its ECE is worse than the other's by more than 2.0 pp, then (a).
The rule is applied once; no tuning on these results.
