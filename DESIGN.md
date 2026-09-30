# Interface direction

Rebuilt on 26 September 2026 after the user found the first interface hard to follow. The first version organised the app as a "field-station operations desk" with dashboards, cards and twenty workflow forms; the team could not see the spreadsheet they work with, and most forms stored data only in the app. The interface now follows the original Shiny database manager the team already knew.

## Principles

- **The spreadsheet is visible.** Every tab centres on an editable grid with the sheet's real column names and row numbers. Formula cells are grey and locked; unsaved cells are amber; cells needing review are red.
- **Tabs name the task.** Tablas, Colecta, Muertes, Tubos, Emergidos, Historial, Asistente, as in the original app (Registrar Muertes, Registrar Tubos, Registrar Emergidos, Buscador, Historial de Cambios).
- **Batch first.** Choose many IDs, set defaults once, fill the grid, review, save. Suggested IDs (Insectary, CAM, tube) follow the original app's rules and skip IDs already used.
- **One save step.** Changes stay on the device until "Guardar en la hoja"; the review dialog lists every change as before → after. A save is one history entry and can be undone as a unit or field by field.
- **Everything is written to the Sheet.** No workflow stores data only in the app.

## Layout

A green top bar holds the tabs, the test-copy badge and the user menu; on phones the tabs get their own scrolling row. Each tab has a toolbar of inputs and buttons above a full-height grid. Unsaved changes show a persistent bar at the bottom with Descartar, Revisar and Guardar. Tapping a row number opens the full row as a vertical form, which is how phones edit wide rows.

Keyboard: type on a selected cell to replace it, Enter or F2 to edit, Ctrl+C/Ctrl+V for ranges, Ctrl+D to fill down, Supr to clear. A cell bar above every grid, like Google Sheets' formula bar, shows the selected cell (column · row ID) and its whole text, wrapping up to five lines; it can be edited there like the cell (Enter saves, Esc cancels, Shift+Enter breaks the line in notes), and in the assistant's proposals it also shows the sheet's and the AI's values. Double-clicking a column's right border fits the column to its content, up to 480 px. Dates display as 14-Aug-25 (the original app's format) and accept 2025-08-14, 14/08/2025 or 14-ago-25.

Every list of values is one dropdown, in the grids and in the forms above them (ChoiceField): a white list filtered as you type, prefix matches first, the suggestion Enter or Tab takes marked in green. On phones the ▾ opens the list alone, without the keyboard.

## Visual system

Fira Sans, a deep green brand colour, neutral stone greys, amber for pending work and red only for errors. Controls are at least 36px high and the grid uses 13px text so wide sheets stay readable.
