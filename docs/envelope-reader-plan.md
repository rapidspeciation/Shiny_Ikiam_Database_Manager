# One envelope reader: plan (24 Oct 2026)

Goal: one text recognizer that reads every line of a specimen envelope (CAM ID, species, sex symbols ♂/♀,
locality, dates, codes) and replaces `camid_rec.onnx` in the Rename Photos app and the server's photo reader,
with CAM ID reading as good as today's model or better, at the same speed.

## Where things stand

- Two recognizers of the same kind (PaddleOCR PP-OCRv5 mobile, SVTR_LCNet, about 7.8 MB, the same 438-character
  alphabet incl. ♂/♀), fine-tuned on different lines:
  - CAM ID reader: CAM ID lines only, about 97 % right. ONNX, used in the browser (rename_photos_AI
    `web-app/public/models/camid_rec.onnx`) and on the server (photo-reader).
  - Full-text reader: all envelope lines, Paddle only (`~/.local/share/sanger-envelope-sam3-20260924/ocr-next/fulltext-train/best-model`),
    used once for the archive scan (curation-20260928). 3,821 training lines, mostly auto-labelled.
- Audit 23–24 Oct (lab database, 5,821 CAMs; scripts in `/tmp/envdates/`):
  - Agreement with the database: species 84–85 %, sex 85 %, locality 77 %, collection date 88 %.
  - Most differences are not misreads: notes record re-identifications; short locality names; reared
    butterflies' plain date is the death date; envelopes outside the frame.
  - OCR misreads: 13–25 % of species differences, about 3 % of locality ones, few dates.
- Orientation is the biggest reading fault. Sideways (90/270°) lines are turned one by one by the CAM ID
  model's confidence, which is close to random on other lines. 488 envelopes mix both turns, and upside-down
  lines invent ♂/♀. One turn per envelope gave +15 points on sex for those photos (about +1 point archive-wide).
- Labels: mostly as written. 86 labels (1.8 %) were changed to the database's spelling, which should be undone.

## Steps

1. **Done (24 Oct).** Lock the yardsticks before any training. These test sets are read once at the end.
   - Today's CAM ID test set (`ocr-next/camid-bigtest`), scored as the app uses it: exact ID, and the decision
     with the database lexicon (margin, rival).
   - The full-text test set (147 lines), with its labels re-checked.
   - A new test set: about 300 envelopes transcribed by a person exactly as written, in all four orientations
     and including the Brazil and Roman-numeral envelopes. Scored per field (CAM ID, species, sex, locality,
     each date), not only whole lines.
     **Done:** `~/.local/share/sanger-envelope-sam3-20260924/ocr-next/envelope-reader/testset` (selection.json:
     201 from the round-2 sealed half, 99 unused archive photos; gold from AI pre-reading in 12 batches of 25,
     40 confirmed by Franz). Today's reader (`envelope_reader_rec.onnx`, md5 8451e705, "new" in
     `results-final/scores/env300-new-*.json`) per field:

     | field | all 300: n | exact | normalized | Franz's 40: n | exact | normalized |
     |---|---|---|---|---|---|---|
     | CAM ID | 300 | 95.0 % | | 40 | 97.5 % | |
     | species | 225 | 60.0 % | 65.8 % | 31 | 48.4 % | 58.1 % |
     | locality | 198 | 51.5 % | 65.2 % | 28 | 50.0 % | 60.7 % |
     | sex | 300 | 84.0 % | | 40 | 80.0 % | |
     | collection date | 185 | 83.8 % | 87.9 % | 26 | 76.9 % | 76.0 % |
     | death date | 64 | 90.6 % | 88.5 % | 10 | 100 % | 100 % |
     | line CER | | 0.140 | | | 0.152 | |

     Species and locality are now the weakest fields.
   - Speed per line in the browser (WebAssembly and WebGPU) and on the server, for today's model.
2. One orientation per envelope, shared by training and reading. In `rename_photos_AI/web-app/src/lib/ocr`
   (the pipeline the browser, the server and the scans use), each envelope is read in the one turn
   (0/90/180/270) whose lines read most confidently together, with the CAM ID's turn and the photo's
   auto-rotation as priors. Training crops are cut the same way.
3. Training data, labelled as written:
   - Every CAM ID line from the CAM ID training sets, plus every full-text line.
   - Undo the 86 database-spelling labels. Keep abbreviations and the writer's slips; the parser and lexicon
     map them to database names.
   - Add hard cases: sideways handwriting; Spanish and Roman-numeral months; dates glued to times; ♂/♀ alone
     and inside lines; the misread species epithets the audit found (about 68).
   - New labels come from the reader plus a check by a person or a second model, never from the database alone,
     so the model does not learn to agree with the database.
4. Train on the Sanger cluster (sanger-cluster-ops skill; ML is not run on this PC).
   - Start from today's CAM ID model, so it keeps what it reads well. Keep the same architecture and alphabet,
     so the result can be dropped in.
   - Oversample CAM ID lines so they keep their weight.
   - Two or three variants (mix ratio, epochs); pick on the validation sets only.
5. Decide on the locked test sets, once:
   - It replaces today's model only if it scores CAM IDs at least as well (exact and with the lexicon) and
     is no slower per line.
   - Report the per-field gains on the 300-envelope set.
6. Ship:
   - Export to ONNX and check browser, Node and Paddle give the same output.
   - Replace `camid_rec.onnx` in rename_photos_AI.
   - The reader keeps every line's text, not only the CAM ID. Measure how much time reading the whole envelope
     adds per photo, and offer it in the app if it is noticeable.
   - Server: the photo readings worker stores each new photo's envelope text, so new photos appear in the
     archive review with their envelope fields.
7. Re-read the archive with the new model and orientation (archive photos on this PC, newer ones on Sanger),
   back up and re-import into the database, and compare the agreement table before and after. Test in the lab
   first; deploy after a go.

## Small fixes that do not wait for the model

- Merge the species parser fix `80d7806` (branch `envelope-species`, worktree `ithomiini_database-envspecies`):
  a genus read alone gives way to the full name read on the other photo (+47 CAMs).
- Count a short locality contained in the database's name as agreeing ("Arajuno" for "San Pedro de Arajuno"):
  about 80 % of locality differences go.
- Split epithets joined to another word ("inachiapozziana").

## Progress 24–25 Oct (after the locked-set review)

- **CAM ID picking (rename_photos_AI, branch envelope-orientation, commit 96a7689, local).** An ID is also taken when it ends a
  longer line ("CAM075192♀", "Ela-meme-1-CAM079134"), when letters of CAM are cut off ("AM…", "CM…", "M…" + 6 digits; only
  if the database knows it), and when a CAM piece and its missing digits are split on one row. Whole lines still come first.
  On dev photos only (378 orientation dev + 1,215 archive), renamed right: p3 341→343 and 1,160→1,161; reader A 346→350 and
  1,161→1,165; wrong renames unchanged.
- **Fresh locked test set, testset2** (`ocr-next/envelope-reader/testset2`). 597 photos of 296 CAMs, every archive specimen in
  the database list that no OCR work had used. It was frozen before any scoring (`freeze.json`). A blind visual check by eye
  found 3 photos without an envelope, 51 with a doubtful ID, 11 with no ID and only 2 printed labels. 533 are clean.
- **Read once with the picking fix, the database list and 2 threads:**

  | | renamed right | wrong renames | clean photos (533) | without no-envelope/doubtful/no-ID (535) | printed (2) | ms per photo (median) |
  |---|---|---|---|---|---|---|
  | p3 | 570 of 597 | 3 | 530, 0 wrong | 530, 2 wrong | 2 | 162 |
  | A | 571 of 597 | 4 | 532, 0 wrong | 532, 2 wrong | 1 | 163 |

  The raw reads without the database rule are exact on 570 (p3) and 571 (A) photos.
  - **Verdict: A does not replace p3.** It has one wrong rename more: CAM074749 read as CAM074744, on an envelope the eye
    check marked doubtful.
  - The 3 wrong renames both models share are photos filed under CAM074087 whose envelope says CAM074081.
  - Review page of the 30 misses: testset2/failreview, port 8914 (https://franz-gpdwin4.tailf5d2f1.ts.net:8514), unit
    camid-miss-review2. The old page's unit is stopped, and its verdicts.json is kept, with a backup.
- **v3 retrain** (printed barcode-label ID lines added; details in the train README). It is not clearly better than A:
  - 53 of 55 held-out printed lines (A 50), but 373 of 384 CAM ID validation lines (A 374) and worse on hard lines.
  - On dev photos it matches A with fewer wrong renames.
  - Second look at testset2 (not proof): 569 of 597, 4 wrong.
  - Not swapped in.
- **Next, for a decision:**
  - Keep p3 for CAM IDs.
  - Or use A, accepting +1 right and +1 wrong in 597.
  - Or collect more printed-label and hard lines labelled as written, then retrain.
- **Switch approved (25 Oct).** A has fewer wrong renames over all non-locked photos: 13 against p3's 16. Commit 7578bb2 on
  envelope-orientation (local):
  - The app, the photo worker, the server reader (`src/node/photoReader.ts`) and the CLI load `envelope_reader_rec.onnx` with
    its 440-entry `envelope_reader_rec_chars.json`.
  - Device-plan VERSION is 3, so each computer times the models again.
  - Tests: 100 passed, 12 opt-in skipped; typecheck clean.
  - The CLI with the app's default model reads the 378 dev photos exactly as the A run did (350 right, 3 wrong).
  - Not pushed or deployed.

## Round 4 (28 Oct): species and locality snapped to the workbook's names

Details: `~/.local/share/sanger-envelope-sam3-20260924/ocr-next/envelope-reader/round4/README.md`.

- **Where the errors came from.** On the 300 envelopes, before the fix: of 77 species misses, 39 were small misreads
  of a workbook name ("eunyanassa", "Aeliconius"), 24 big misreads, 8 lines out of order and 6 lines not found; of 69
  locality misses, 38 small, 24 big, 6 not found. The database parser already maps most small misreads to the right
  name (same species as from the gold lines on 191 of 222), so the misreads mostly hurt the text.
- **Fix, no retraining.** `snapLine` (`server/envelope-fields.mjs`) replaces a read word by the nearest word of the
  workbook's taxa and localities when it is close enough. CAM ID lines, digits, kept words (`envelope-keep-words.json`),
  cut and split names stay as read. `parseEnvelope` snaps each line first. Settings chosen on dev only; keep rule
  written before scoring.
- **300 envelopes, read once:** species 60.0 -> 69.3 % exact (65.8 -> 76.4 % normalised), locality 51.5 -> 54.5 %
  (65.2 -> 69.2 %); CAM ID, sex and dates unchanged; line CER 0.140 -> 0.136. Parser fields: species 191 -> 192 of 222,
  locality 141 -> 145 of 186. testset2 renames unchanged (571 right, 4 wrong): no line with a digit is changed.
- **Verdict: keep.** Not deployed. Left for the recognizer: big misreads on Brazil, French Guiana and Panama handwriting,
  3-letter names ("Kaw"), lines not found.
