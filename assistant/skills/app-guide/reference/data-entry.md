# Data-entry tabs

Base URL: `https://ithomiini-ikiam.com/`. Editing needs the editor,
reviewer or admin role. All tabs keep their pending edits in the save bar
(see SKILL.md "Saving").

## Grids, keys and dates (every tab with a table)

The tables behave like Google Sheets:

- Type over a selected cell to replace it; double click, Enter or F2 edits in
  place; Enter/Tab save and move (Shift goes back). «Supr» (Delete) clears the
  selection. Ctrl+D (or the «Rellenar» button in Buscador) copies the first
  selected row down the selection.
- Paste ranges from Excel/Sheets; one value pasted over several selected cells
  fills them all.
- **Fill handle**: the small square at the bottom-right of a selection; drag
  it down to copy. In the Colecta list, `Insectary_ID`, `CAM_ID` and
  `Tube_1_id` continue the series instead (O6D → O7D, CAM079895 → CAM079896);
  elsewhere it copies.
- Grey cells are formulas (read-only). A ▾ arrow marks a dropdown list: click
  it to open the list. A value outside a non-strict list is saved but flagged
  with a red corner ("se guarda igual; corrígelo si es un error").
- Click the **row number** to open the whole row vertically (best on phones):
  it has «Abrir en Google Sheets» (that exact row) and, in Insectary_data /
  Collection_data, «Corregir Insectary ID (…)» (moves a butterfly's data to
  the right pre-made row; «Ver cambios» first).
- Column headers sort; the «filtrar» box under a header filters (Buscador).
- **Counts as sums** in Insectary_stocks (`NUMBER OF EGGS`, `NUMBER OF LARVAE`,
  `NUMBER OF PUPA`, `NUMBER OF ADULTS`): type `12+15`, `=12+15` or `27-5`; the
  app writes the formula `=12+15`. No other formulas are accepted.
- **Dates are day first.** Grid cells take `14/08/2025`, `14-8-25`, `140825`
  or `14082025` (phone number pad), `14-Aug-25` / `14-ago-25`, or `2025-08-14`;
  years 1990–2099 only. Dates show as `14-Aug-25`. The date boxes above the
  grids (Colecta, Muertes, Tubos, Emergidos, Clutches, Historial, Revisión)
  also take `hoy` and `ayer`, and show the weekday ("sábado 27-Sep-26 · ayer").
- Phones: tap selects, a second tap on the selected cell edits, drag the
  circle to stretch the selection, and a bar at the bottom offers Copiar, Pegar, «Rellenar ↓», Borrar.

## Buscador — `#/tablas`

A text searched in every sheet, as Google Sheets' Ctrl+F, or any sheet whole
as a spreadsheet.

- «Buscar en todas las hojas»: any part of any cell, upper or lower case
  (Insectary ID, CAM, tube, clutch, species, a word of a note; dates as shown,
  `14-Aug-25`). The results list each sheet with the text, best first: the
  sheet where it is the row's ID (Insectary_data for an Insectary ID), then
  sheets where it fills a whole cell, then the rest. «Hoja primero» puts a
  sheet at the top.
- Each sheet shows the rows around its newest match (the one where it is an
  ID, when there is one), editable as in the whole sheet; the cells with the
  text are green and the match row is marked. Scrolling up or down loads more
  rows of that sheet, so neighbouring rows with the same mistake show up
  (tubes typed FS5… for FS50…). ↑ ↓ go to the other matches ("3 de 17").
- «Hoja completa» opens that sheet whole at the match row; «Resultados de
  «…»» goes back to the results.
- With the box empty: «Hoja» (grouped: Insectario, Campo, Cruces,
  Experimentos, Muestras, Fotos, Referencia; row counts in grey), «Filas
  vacías preasignadas» (also show the pre-made empty rows), «Revisión de
  datos» (the Revisión tab filtered to this sheet), «Añadir fila» (a new row
  at the top, marked «nueva»), «Crear filas preasignadas» (reviewer/admin:
  asks how many, max 500; copies the last pre-made row's formulas and
  dropdowns; in Insectary_data it writes the next IDs into the rows after the
  last ID, and when no rows are left PAS adds them), «Rellenar» (= Ctrl+D), reload,
  download CSV, open the sheet in Google Sheets.
- A banner warns when the sheet's header row changed in Google Sheets (a
  missing/duplicate column blocks reading and saving that sheet).

Links: `#/tablas?buscar=<text>` searches every sheet (with `&hoja=<sheet>`
that sheet comes first); `#/tablas?hoja=<sheet>&fila=<row>` opens a sheet at
a row. E.g.
`https://ithomiini-ikiam.com/#/tablas?hoja=Insectary_data&buscar=N4D`,
`https://ithomiini-ikiam.com/#/tablas?hoja=Collection_data&fila=8215`.
Sheet names are exact (`Insectary_data`, `Collection_data`,
`Insectary_stocks`, `SamplingDay_data`, `Wing_tissue`, `Photo_links`,
`Lists`, `Taxonomy_v18Jun25`, `CRISPR`, `Melinaea_crosses`, …; the full list
is in the «Hoja» menu). Default sheet: Insectary_data.

## Colecta — `#/colecta`

A day of field collection entered in bulk → Collection_data; butterflies taken
alive also get their Insectary_data row in the same save.

1. Header (what the outing shares; on phones it folds into one line, tap ✎):
   «Collection_date» (warns «¿Es hoy la fecha de la colecta?» when it is
   today), «Collector», «Identifier», «Rainfall» (starts `DY_(dry)`),
   «Cloud_cover» — they apply to rows added from now on.
2. «Collection_location», «Filas a añadir», «SPECIES (opcional)» (the same for
   all), «Release_Collect» (`Collected_Sent2Insectary`, `Collected_Preserved`,
   `Released_Unmarked`) → «Añadir N filas». Change the place and add more for a
   second site.
3. Each row: SPECIES, Subspecies_Form, Sex (`female`, `male`, `female ?`,
   `male ?`, `NOT_COLLECTED`), Release_Collect, Collection_time (hh:mm),
   then per fate: **insectary** → the next free pre-made `Insectary_ID` (to
   write on the wings, no CAM yet); **preserved** → the next `CAM_ID` (from
   Lists' Wild_indv_CAMid pool) and `Tube_1_id`, and Preservation_medium
   (`Flash frozen` by default); Purpose, notes, and per-row Collector,
   Identifier, Rainfall, Cloud_cover.
4. Boxes on the right: «Próximo CAM_ID» (warns when fewer than 50 are left),
   «Próximo tubo», «Próximo Insectary ID».
5. Views «Tabla» (spreadsheet, fill handle continues IDs) or «Formulario»
   (tick rows, «hasta aquí» for a run, then fill SPECIES/Sex/Release_Collect
   and «Aplicar»; «Quitar», «Desmarcar»; copy a row to apply it to others).
6. «Quitar vacías», «Vaciar lista». The list is kept **in this browser** until
   saved or emptied, even after closing the page.
7. «Guardar colecta (N)» → a summary («Guardar N mariposas»: date, places,
   ♀/♂ to the insectary, preserved CAM range and gaps, released, per species)
   → «Guardar en la hoja». Problems (repeated IDs, missing place…) block it and
   are listed next to the button.

Below: the last Collection_data records («ver más»), editable.
No link parameters.

## Muertes — `#/muertes`

Death date and cause of insectary butterflies (Insectary_data). Two modes,
switched with «Tarjetas | Tabla» (kept per browser): cards by default on touch
screens (phones either way up, tablets), the table on a PC. Both share the IDs
chosen, the date, the cause and preserved or not, and write the same cells.

**Tabla**:
1. «Insectary IDs»: type an exact ID + Enter, pick from the list, paste a
   list (`N1D N2D, N3D`) or a range (`B0D-B9D`, pre-made order). Warns when an
   ID already died ("B9 ya murió el …": probably a mistyped ID).
2. «Fecha de muerte» (today by default; weekday shown).
3. «Causa por defecto» (Death_cause list: Unknown, Eaten, Spider,
   Disappearance, Killed_Preserved, Deformed, Heat stroke, Other…).
4. «Sin preservar: CAM y tubos NA, tejidos y medios NOT_COLLECTED» (on by
   default): for causes other than Killed_Preserved and rows without CAM/tube,
   sets CAM, tubes, Preservation_date, Location_body, Preserved_Dead_Alive to
   `NA` and the tissues and media to `NOT_COLLECTED`.
5. «Escribir fecha y causa (N)» fills only **empty** cells of the chosen rows;
   check the rows shown under «IDs elegidos» and let it save.

Below: «Últimas N muertes registradas» («ver más»).

**Tarjetas**, in two levels like a shopping cart:
1. A search box (Insectary ID, CAM or tube; says alive or dead; for a worn
   wing, `A?B` = one character unreadable, `A[16]B` = one of two, and
   look-alikes such as 6/8 or B/D are offered with the doubtful character in
   amber; ♀ / ♂ / ? and a species under the box rank what is seen). Enter or
   a tap puts the butterfly in «Seleccionadas»; a pasted list or a range
   (`B0D-B9D`) puts them all; «Seleccionar varias» adds each ID tapped.
2. The panel (right column on a wide screen; on a phone one line above the
   cards, «Cambiar»): «Fecha de muerte» (Hoy / Ayer / a date), the cause as
   buttons, «Sin preservar | Preservada» (the medium and each one's CAM and
   tube, next free ones pre-filled) and a note. With no card open it is «Para
   las próximas mariposas»: every butterfly added afterwards starts with those
   values (choose Heat stroke once, then add ten IDs). A card tapped opens in
   it («Muerte de G7D») and changes that card only; a butterfly added alone
   opens by itself (one at a time).
3. Each card's «+ Añadir a muertes», or «Añadir todas» (Ctrl+Enter), writes
   and saves its death; × takes a card out, nothing saved.
4. «Registradas hoy»: the deaths saved from Muertes today, everyone's or
   «Mías», sorted by Insectary ID, emergence date or sheet row (↑/↓, kept in
   the browser) to copy them into the paper notebook. A tap opens one in the
   panel to correct its date or cause or add a note; ↶ «Deshacer» undoes its
   death (the Historial's preview and confirmation).
No link parameters.

## Tubos — `#/tubos`

CAM IDs and tubes for insectary butterflies (Insectary_data), and labels. Two
modes, «Tarjetas | Tabla» (kept per browser; cards by default). Both share the
butterflies chosen, the tissue, medium and dates, and write the same cells.

**Tarjetas**:
1. Search box (Insectary ID, CAM or tube; paste a list or a range `Z8D-A2E`).
   One card per butterfly, in the order added: tubes are handed out in that
   order. ✕ removes a card, › opens the whole row.
2. Options (on a phone folded into one line above the cards, «Cambiar»; on a
   wide screen the right column), for all cards or the selected ones (tap a
   card's name; its own values show in violet):
   - «Qué va en el tubo»: «Cuerpo entero» (`WHOLE_ORGANISM`; also
     Preservation_date, Death_date, Death_cause Killed_Preserved,
     Preserved_Dead_Alive, Location_body Ikiam where empty), «Corte de ala»
     (WING CLIP; the note `d/m/yy INI: Wing clip d/m/yy`), «Por partes» (one
     tube per tissue, «Otro tubo»), «No preservada» (CAM and tubes `NA`,
     tissues and media `NOT_COLLECTED`; rows without CAM or tube only).
   - The date (Hoy / Ayer / a date; amber when older than yesterday), the
     medium, «Tubos sin usar: ID NA, tejido y medio NOT_COLLECTED» (bodies).
   - «Tubos desde … · CAM desde …»: the rack the app picked and the next free
     CAM; «Cambiar» to pick another rack or type the first tube or CAM.
3. Each card: its CAM (kept when it has one) and tube boxes (letters `FS` in a
   menu, digits on the number pad), filled with the next free ones (grey,
   «siguiente libre», skipping IDs used in any sheet). A tube typed or
   scanned makes the next cards follow from it. Enter goes to the next card's
   tube. «Escanear» (Chrome on Android) reads tube codes with the camera, card
   after card.
4. Checked before Save: two letters + 8 digits (`FS9041542` offers «Usar
   FS90415421»; «Así está en la etiqueta» keeps an odd one), the same ID on
   two cards, an ID used in any sheet or in another unsaved change, no free
   tube column, a missing date.
5. «Guardar N mariposas» writes and saves at once, with «Deshacer» and
   «Imprimir etiquetas» (Code128: tube, ID · CAM, tissue).

**Tabla**:
1. «Insectary IDs» (as in Muertes; warns when a butterfly already has CAM and
   tube or died more than 7 days ago) → «Cargar» (replace the table) or
   «Añadir a la tabla». The tissue follows the rows: mostly dead →
   `WHOLE_ORGANISM`; alive → WING CLIP.
2. «CAM ID inicial» (suggested, with the date of its run), «Rack en uso
   (siguiente tubo libre)» (racks grouped «Insectario y cruces» / «Colectas y
   monitoreo»; the app picks one from the medium and whether the rows are
   crosses until you choose) «o escribe el tubo».
3. «Tejido por defecto», «Medio por defecto» (Flash frozen, Ethanol, DMSO…).
   Wing clips: «Fecha del corte de ala» (required) and «Iniciales (nota)»: the
   note `d/m/yy INI: Wing clip d/m/yy` is appended to Notes_Insectary_data.
   Whole body: «Preservation_date» (required; also sets Death_date,
   Death_cause Killed_Preserved if empty, Preserved_Dead_Alive, Location_body
   Ikiam) and «Si el tejido es WHOLE_ORGANISM: tubos siguientes NA…».
4. «Asignar IDs»: consecutive CAMs to rows without one and the next tube into
   each row's first empty tube slot. If the start is already used it stops and
   offers «Usar el siguiente libre: …».
5. «Imprimir etiquetas»; «Vaciar tabla» clears the loaded rows.

No link parameters.

## Emergidos — `#/emergidos`

New adults of a clutch into the next free pre-made rows of Insectary_data.

1. «CLUTCH NUMBER» (e.g. `994(6)`); the hint shows the clutch's species from
   Insectary_stocks and its sibling subspecies.
2. «Hembras», «Machos», «Sin sexo»; «Insectary ID inicial» (the next free
   pre-made ID; an earlier empty row can be chosen, with a warning);
   «Intro a insectario» (date).
3. «Preparar N filas» (or «Añadir una»): new rows with ID, clutch, Sex,
   Intro2Insectary_date, SPECIES = the clutch's prediction, Wild_Reared
   `Reared`, Stock_of_origin (deceptus/messenoides/intermedia for Mechanitis
   messenoides, else NA), Research_purpose `F1/F2 mutation rate` for hybrids.
4. If another subspecies emerged, change SPECIES in that row (only then).
   Write each ID on the wings.

On the cards (phones), «Siguiente ID» takes a typed ID (S8E, to leave S3E–S7E
for a colleague who wrote them on paper) or a gap from its list; the buttons
then give that ID and the next ones in order until «Volver al último». An ID
with data, held by someone else or past the pre-made rows is refused with the
reason and the next free one. «+ Preservados…» registers eggs, larvae (L1–L5,
prepupa) and pupae (Pupa day 1–12) preserved from the clutch, each with its
ID, CAM and tube and an optional note; «+ 1 más igual» adds one more like the
last ones.

Below: the butterflies already recorded from that clutch and the latest rows.
When few pre-made IDs are left a banner offers «Crear filas preasignadas»
(reviewer/admin). No link parameters.

## Clutches — `#/clutches`

Clutches (eggs laid) and their follow-up in Insectary_stocks.

1. «Clutch» (empty = the highest number + 1), «Especie», «Puesta» (date),
   «Huevos», «Dónde» (Insectary / Laboratory) → «Nuevo clutch».
2. Hatching and pupation: type HATCHING DATE, NUMBER OF LARVAE, PUPA DATE,
   NUMBER OF PUPA in the clutch's row (counts as sums: `=12+15`).
3. The grid shows clutches laid in the last 60 days not yet emerged; «ver los
   últimos 150» shows more.

On phones the tab shows cards (the default). Each card shows what to count
today and the next date expected (hatching, pupation, emergence, from the
species' usual days). In a clutch:

- −N asks whether they died, disappeared or were preserved; «Pasó» chooses
  the day (today, yesterday, another).
- Every event adds its dated, signed note to NOTES, shown before saving.
- Preserved larvae can be registered in Insectary_data at once: «Registrar N
  en Insectary_data» opens Emergidos' cards, with their IDs, CAMs and tubes.
- «Foto de hoy» or the camera next to an event adds photos. They stay in the
  app and do not go to the sheet.
- «Marcar como revisado» marks the clutch as checked for today.

Link: `#/clutches` (no parameters).

## Censo — `#/censo`

A census of one species in the insectary, replacing the notebook smileys:
the butterflies go into a small cage and are released one by one.

1. «Nuevo censo»: the date (today by default), then tap the species (its
   butterflies alive are counted beside it: Insectary_data rows with no
   Death_date and no Death_cause). A census already «En curso» for that species
   and day is joined; several phones mark it at once and see each other's marks.
2. For each butterfly: type the wing ID (`A?B`, `A[16]B` and look-alikes as in
   Muertes; ♀ / ♂ / ? if seen) and tap it, or Enter for the first: a big ☺ with
   «Deshacer». «Viva, pero se ve distinta…» keeps a sex or species doubt with a
   note; another species found in the cage, a butterfly recorded dead, or an ID
   in no row («Anotar … como hallazgo») are kept as findings, nothing corrected.
   «Aún sin ver» lists the rest (cards or table), each can be marked from there.
3. «Terminar el censo» → review: those not seen will get Death_date = census
   date and Death_cause `Disappearance`, plus the not-preserved block as in
   Muertes (CAM and tubes NA, tissues and media NOT_COLLECTED, when the row has
   no CAM or tube); «No contar» leaves one out (in another cage…). «Marcar N
   como desaparecidas» keeps them in the app, like Emergidos and Clutches, until
   «Guardar en Google Sheets».
4. Afterwards: «Para el cuaderno» lists the IDs in order with ☺ or
   «desaparecida d/m/yy» (copyable), «Ya lo pasé al cuaderno», and «Reabrir el
   censo» while the disappearances are not yet in Google Sheets. «Censos
   anteriores» keeps every census (date, species, who, counts, findings).

Link: `#/censo` (no parameters).
