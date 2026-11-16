# Identificar answers: round 1 fixes (15 Nov 2026)

After the first sol-loop round on 9f0a37e7..2762ee82. The design and the lab are as in ../ui_20261116T025034Z/NOTES.md;
this folder holds the screenshots of the fixed page (same script, `shots.mjs`, `checks.json`).

- S-F1 / O-F1: the headline is always the top species (`guessOf` removed), with its own % and its genus. A tribe or
  subfamily that clears its cut keeps its own checked row, now worded about the photo: «La foto muestra la tribu
  Ithomiini 86 %» («The photo shows tribe Ithomiini»). `phone_tribeOutside_es.png` / `desktop_tribeOutside_es.png`:
  Heliconius erato 12 % Incierto, Heliconius 12 % Incierto, then the photo's tribe Ithomiini 86 %. The comparison
  and the subspecies line follow the top species again. *Correction (round 2): the sex panel and `contribShown`
  still took the genus under a genus answer here; fixed in ui_20261116T034311Z.*
- O-F2: the green Ithomiini badge only when the species or the genus is clear (a clear Ithomiini tribe has its own
  row instead); otherwise the tribe · subfamily label is grey (`*_unsure_es.png`). «No parece una Ithomiini.» only when
  the species or genus is clear, or a clear tribe is another one.
- O-F3: `ranksAbove` now takes a species name (the genus and tribe branches and `shared` were unused) and lost
  their tests.
- O-F4: the check's label reads «Seguro» / "Certain", against «Incierto» / "Uncertain".
- O-F5: no change.

Checks: frontend 126 files, 1136 tests pass; typecheck and `npm run check` clean. Lab pages at 390 and 1440 px, no
horizontal scroll. The phone's own model is still not available on this lab (`desktop_phonemodel_error.png`).
