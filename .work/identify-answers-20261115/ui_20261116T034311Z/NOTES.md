# Identificar answers: round 2 fixes (15 Nov 2026)

On 4044f730 (the lead's cuts: species 0.70, genus and higher ranks 0.80, both engines). Design and lab as in
../ui_20261116T025034Z/NOTES.md; this folder holds the screenshots at the new cuts (same `shots.mjs`, `checks.json`).

- F1: the tribe / subfamily row reads «La foto parece de la tribu Ithomiini 88 %» («The photo looks like tribe
  Ithomiini»); the check's label is «Probable» / "Likely".
- O-F5: `contribShown` is always the headline's top species, so «Es esta» on it is sent as confirmed under a genus
  answer too (new view test). `sexState` now reads the top species as well: `sexApplies` only looks at the genus, so
  the old genus branch changed nothing; its 'genus' state is the sex model's own limit (an Ithomiini genus it was not
  trained on), commented as such. The earlier NOTES saying these kept the clear level are corrected.
- O-F7: the test title names the shipped cuts; the template comment no longer mentions «Posible tribu».
- O-F8: declined by the lead (no change).

Screenshots (Spanish, phone 390 px and desktop 1440 px): `*_species075_es.png` (75 %, no «Incierto»),
`*_genus_es.png` (55 % «Incierto», genus 86 % clear), `*_tribe_es.png` and `*_tribeOutside_es.png` (the new row
wording), plus the rest of the set and real server photos (`desktop_real_*`). No horizontal scroll; no testing line.

Checks: frontend 126 files, 1137 tests pass; typecheck and `npm run check` clean. The phone's own model is still not
available on this lab (`desktop_phonemodel_error.png`).
