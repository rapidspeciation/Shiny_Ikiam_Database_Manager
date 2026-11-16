# Identificar answers: the guess always named (15 Nov 2026)

Branch ui/identify-answers-20261115. Asked by Franz (15 Nov): show the tribe a species or genus belongs to, keep the
tribe / subfamily fallback, and "I prefer getting the guess even at low model confidence and just say uncertain or so"
(as the Wings Gallery shows its top species). The second ask replaced the first plan's separate «Lo más probable» line.

## What changed

- **The guess is always the headline**: the most probable species, in italics, with its score. Below the engine's
  species cut it is marked «Incierto» (amber tag under the %, the % in grey); above it, the % is green and nothing else
  is said.
- **Under an uncertain species, a small boxed list** (hairline dividers): its genus with the genus total, «Incierto»
  below the genus cut or a check above it; when the genus is not clear but a tribe or subfamily is, that row too, with
  its total and a check. Nothing clear: just the genus row, «Incierto». The old kickers («Posible género», «Posible
  tribu», «No estamos seguros. Lo más parecido:») and sublines («la especie no es segura», «el género no es seguro»)
  are gone; the kicker is always «Posible identificación».
- **The guess**: the top species. One exception: when a tribe or subfamily is the clear level and the top species is
  not in it (its many species share the score), the guess is the top listed species inside it, so the headline never
  contradicts the checked row (`guessOf`). `data-top` on the headline still gives the raw top species.
- **The tribe and subfamily label** (the guess's, from tools/identifier/taxonomy_6fa6ab78.json by name, so the
  server's answers and the phone's alike): «Ithomiini · Danainae» before the kicker. It is the Ithomiini badge (brand
  green) when the tribe is Ithomiini, a grey label otherwise («Heliconiini», then «No parece una Ithomiini.»); a
  subfamily alone reads «Subfamilia Dismorphiinae». Not shown when a tribe or subfamily row is the clear level (the row
  names it). Placeholder names (UNKNOWN_*, incertae sedis) never shown; a genus gets a rank only when all its species
  in the file share it.
- **The tribe / subfamily rung is permanent**: the TRIBE_RUNG flag (VITE_TRIBE_RUNG), the line «Respuesta de tribu y
  subfamilia activada para pruebas», its testid and its strings are gone. A rollback is a git revert.
- **Cuts per engine**, one line in frontend/src/lib/identifyResult.ts:
  `THRESHOLDS = { small: { species, genus, higher }, bioclip: { species, genus, higher } }`, all 0.80 for now (they now
  set where «Incierto» starts, and when a tribe or subfamily is shown as clear). `engineOf(source, kind)`: the server
  and the phone's «Más preciso» (`full`) are BioCLIP, the phone's default the small model. To apply the measurement's
  values: edit that line and the `toEqual` in identifyTribeRung.test.ts («has one set of cuts per engine»).
- **Subspecies line**: under a species that is not clear it is always «Si fuera esta especie, la subespecie más
  parecida: … (NN % de esa especie)» (before it was dropped under genus / tribe answers); a confident «Subespecie más
  probable» only under a clear species.
- Unchanged on purpose: the sex panel and what is kept with an identification (`contribShown`) still use the clear
  level (genus when only the genus is clear).

Files: frontend/src/lib/identifyResult.ts, lib/taxonomy.ts, views/IdentifyView.vue, locales/en/identify.ts, tests
(lib/__tests__/identifyTribeRung.test.ts, identifyResult.test.ts, views/__tests__/IdentifyView.panel.test.ts),
a comment in server/identify-photos.mjs.

## Checks

- Frontend: 126 test files, 1137 tests pass (vitest; no network, no AI); `npm run typecheck` and `npm run check` clean.
- Lab copy (below), headless Chromium at 390x844 (2x) and 1440x900; `checks.json` has what each page showed.
  - Real server identifications (lab identifier, head 6fa6ab78), photos from the lab's photo pack:
    species clear `desktop_real_dismorphia` (D. theucharila 91 %, «Subfamilia Dismorphiinae»); species uncertain,
    genus clear `desktop_real_herato` (H. erato 55 % Incierto, Heliconius 93 % ✓); both uncertain, tribe clear
    `desktop_real_oonega` (Hyposcada illinissa 39 %, genus 44 % Incierto, Ithomiini 97 % ✓) and `desktop_real_hanastasia`;
    nothing clear `desktop_real_mpolymnia` (8 %, genus 15 %) and `desktop_real_mmenophilus`.
  - The same four kinds from answers restored through the page's session store (`identify:last`, as the view tests
    do), phone and desktop, ES and EN, dark: `*_species_es`, `*_genus_*` (species uncertain, genus clear),
    `*_tribe_*` (both uncertain, tribe clear), `*_unsure_es` (nothing clear), `*_notIthomiini_es`.
  - No horizontal scroll at 390 px; no «para pruebas» text anywhere.
- Not seen in the lab: the phone's own model. This lab's identifier folder has no current phone model
  («This model is not available for now», `desktop_phonemodel_error.png`). The label and the ladder do not depend on
  where the photo was read; `engineOf` and the phone's taxonomy totals are covered by the lib tests.
- Noted, not changed: the guess's genus can be smaller than another genus (oonega: Hyposcada 44 % shown, Oleria 52 %
  first in «Las más parecidas»), as asked ("the top species and its genus").

## Lab

- Port 8893 of the lab PC (unit ithomiini-dev-8893, LOCAL_MODE on the lab snapshot, 72 h limit, started through
  systemd-run so the chat reaper leaves it), served tailnet only through `tailscale serve` on https port 9975:
  `https://<lab PC>.ts.net:9975/identificar/` (full address in the report to the lead). Admin `lab`, password in the
  instance folder's credentials.json (not here).
- Settings as the identify-panel lab: IDENTIFIER_DIR and PACKS_DIR of that lab, IDENTIFIER_IDLE_MINUTES=10.
- Stop: `tools/lab/dev-app.sh --stop --port 8893`, then `sudo tailscale serve --https=9975 off`.
- Screenshots again: `node shots.mjs <lab url> <credentials.json> <folder of jpg photos> <new output folder>`.
