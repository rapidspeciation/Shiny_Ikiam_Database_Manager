# Phone row form: grid by default, toggle to a form (1 Nov 2026)

Branch `ui-rowform-proto` (on live main e9df68f, prototype 733fdb6). Commits 055e4c1, 9c684ff, 3b9a043. Not pushed or deployed.

## What changed
- **Toggle** (Buscador toolbar, phones < 640 px only): one toggle button (clipboard icon, `aria-pressed`, filled green when the form is on, 44 px touch area), left of ⋯. A segmented two-button control did not fit at 360 px next to the search box and sheet chooser. The choice is kept per device in localStorage `ithomiini:phone-view` (`"form"`, or null for the grid). This replaces `proto:rowform`. The grid stays the default. The form only appears in the Buscador; SheetGrid gets a `form` prop instead of reading localStorage itself, so the other tabs are unchanged.
- **Slimmer header**: one line, with the ID in semibold, then «fila N · 227 de 13.376» in muted text (EN: "row N · 227 of 13,376"; numbers in the locale's format). The prototype's hard-coded Spanish «de» is gone.
- **Sections**: collapsible, with a sticky 44 px header. Each header shows its field count and an amber badge with the number of unsaved edits inside. Fold state is kept per sheet on the device (`ithomiini:form-sections`). A sheet whose columns all fall in one section gets a plain list. The desktop drawer is unchanged (flat list).
- **«Ir a…»** now opens at the top of the form, not just above the bottom bar. The input is focused, and the list's height follows `visualViewport` (`useKeyboard`), so the list ends above the keyboard and scrolls. Enter (`enterkeyhint=go`) jumps to the first match. Esc or ✕ closes it and returns focus to the button. It is a text input, so it has only the panel's own ✕, not a second native clear button.
- The form also shows new rows (first) and follows a row opened from search or a row just added. «Rellenar» and the grid help are hidden in form view. The phone search placeholder is now «Buscar»/"Search", and it is hidden below 380 px.
- Edits use the same path as before: `pending.setCell` / `updateCreate`, then the save bar (Revisar, Guardar en la hoja). Nothing new was written.

## Sections (rules in `frontend/src/lib/rowForm.ts`; the first rule that matches wins, and the sheet's ID columns always go to Identity)
Order, most used first. Insectary_data's 53 columns:
1. **Identidad / Identity** (11): Insectary_ID, Wild_Reared, CLUTCH NUMBER, Stock_of_origin, SPECIES, Sex, Research_purpose, Pedigree, LIFESTAGE, CAM_ID_CollData, CAM_ID
2. **En el campo / In the field** (1): Collection_location. In Collection_data it holds country, location, transect, bait, date, time, collector, weather and weight.
3. **Fechas y muerte / Dates and death** (4): Intro2Insectary_date, Death_date, Death_cause, Preservation_date
4. **Tubos y preservación / Tubes and preservation** (13): Tube_n_id/tissue, Tn_Preservation_medium, Preservation_medium, Preserved_Dead_Alive, Location_body
5. **Notas y fotos / Notes and photos** (3): Photo_dorsal, Photo_ventral, Notes_Insectary_data
6. **Racks y manifiestos / Racks and manifests** (8), *folded*
7. **Manifiesto (fórmulas) / Manifest (formulas)** (13 capitalised ToL columns), *folded*
8. **Otras columnas / Other columns**: anything else, shown open. In Collection_data that is only Select_TEMP; in Insectary_stocks it holds the counts.
Tests cover all 53 Insectary_data and 77 Collection_data columns, the edge cases (Collection_location vs COLLECTION_LOCATION, Preservation_date, Tube_1_rack), the folded defaults, and that every title has English.

## Proof
- `npm test`: 80 files, 724 tests pass (includes new `rowForm.test.ts`). `vue-tsc` passes. `npm run build` passes.
- Shots: `/home/franz/.local/share/ui-overhaul-20261031/rowform-shots/`
  - `20261101-074456-light/` and `20261101-074546-dark/`: 390 and 360, ES and EN. Each has `1-grid`, `2-form` (default sections), `3-form-folded`, `4-form-edit` (one staged edit with the save bar) and `5-goto-keyboard`. The keyboard in `5-goto-keyboard` is simulated by making the viewport 330 px shorter. `log.json` shows the choice kept on reload, the list's bottom above the visible bottom, and no blocked writes (non-GET requests were blocked in the browser and none were attempted).
  - The light/dark shots were taken before 3b9a043, so they still show the cut-off search placeholder ("Sea", "Bus") at 360 px. That change was not re-shot.
  - I looked at 9 shots.
- Emulator (Pixel 7, Android 15, Chrome, dev app on 8861 through adb reverse; MemAvailable was 8 GB with the dev app up, 4.1 GB with the emulator too): `20261101-075302-emulator-gboard/` passed everything with **Gboard**.
  - Grid is the default, and a real tap on the toggle shows the form.
  - «Ir a…» focuses its input and Gboard opens. "R5" was typed through adb. The input is at y 78–112 and the list ends at 415, below the visible bottom of 423. The second result could be tapped and jumped to R5D (row 13437).
  - Field edit: Notes_Insectary_data was tapped and stayed visible above Gboard. After typing and leaving the field, there was 1 dirty field and the save bar appeared. The edit was then dropped from localStorage, and no writes were made.
  - An earlier run with SwiftKey (the emulator's keyboard, restored afterwards) passed «Ir a…» the same way (`20261101-075126-emulator/`).
- The dev app and the emulator are stopped.

## Open
- Not reviewed by critics yet (Sol + fresh Opus), left to the lead.
- Chrome offered "Translate page" over the ES page on the emulator. That is the browser, not the app.
