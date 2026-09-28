# Ithomiini database

A phone and desktop app for collection, insectary, breeding, experiments, samples, and research records. The frontend and authenticated API run together on claudeclaw.

This repository used to hold the original Shiny app. It is kept in the [`shiny-app-archive`](https://github.com/rapidspeciation/Shiny_Ikiam_Database_Manager/tree/shiny-app-archive) branch (tag `shiny-app-final`). The [home and privacy pages](https://rapidspeciation.github.io/Shiny_Ikiam_Database_Manager/) are published from the `gh-pages` branch.

## Workflows

The interface follows the original [Shiny database manager](https://github.com/rapidspeciation/Shiny_Ikiam_Database_Manager/tree/shiny-app-archive): task tabs in a top bar, and an editable spreadsheet grid that shows the real sheet columns.

- **Tablas**: any sheet as a spreadsheet. Search all columns, filter each column, edit cells, paste ranges from Excel or Sheets, fill down (Ctrl+D), add rows, download CSV. Formula cells are grey and read-only. Tapping a row number opens the whole row as a form, which is the easiest way to edit on a phone.
- **Colecta**: new Collection_data individuals with session defaults (place, collector, date), suggested CAM IDs from the Lists pools and subspecies choices for the chosen species.
- **Monitoreo**: the monthly Ithomiini monitoring at Ikiam ([docs/monitoring.md](docs/monitoring.md)).
  - *Importar recorrido*: a Wikiloc GPX turns each waypoint note ("M1 Hyposcada illinissa ida hembra 9:20 0.5m NO id: B69") into a Collection_data row. The transect section comes from the GPS point, weather codes are translated and a repeated field mark counts as a recapture. The day's start and end go into SamplingDay_data.
  - *Resumen*: the tables of the monthly reports and the 30-preserved rule.
  - *Mapa*: satellite map with the four transect sections, the uploaded walks and their captures.
- **Muertes**: choose many Insectary IDs (type or paste a list), set the death date and cause, review the grid, save.
- **Tubos**: assign consecutive CAM and tube IDs with default tissue, medium and date, including the WHOLE_ORGANISM NA rule, and print barcode labels.
- **Emergidos**: new adults from a clutch go into the next pre-filled Insectary_data rows; SPECIES and location come from the sheet's formulas.
- **Historial**: every change as before → after, filterable by ID, person, sheet and date, with selected undo (and redo by undoing an undo).
- **Asistente**: questions answered from the records with row citations; proposed edits need approval.

Edits stay on the device (surviving reloads and lost coverage) until **Guardar en la hoja**, which writes them all at once as one history entry. This replaces the original "Guardar en local → Subir cambios" steps.

## Run and verify

Node 24 is required. The server has no third-party runtime packages; the frontend (Vue 3, Tabulator, Tailwind) is built with Vite into `web/`.

```sh
npm --prefix frontend install   # once
npm run check                   # syntax + frontend type check
npm test                        # server and frontend tests
npm run build                   # frontend → web/
LOCAL_MODE=1 SECURE_COOKIES=0 SETUP_TOKEN=your-private-setup-token npm start
```

Open `http://127.0.0.1:8794/ithomiini/`. For frontend development run the server, then `npm --prefix frontend run dev` and open `http://localhost:5173/ithomiini/` (API requests are proxied to port 8794). Local mode uses an isolated sheet adapter. For a private snapshot, set `SEED_FILE` to a JSON object containing `sheets`, keyed by exact sheet name, with `{row,cells}` Google grid rows. Keep snapshots outside Git.

For live use, configure `GOOGLE_CREDENTIALS_FILE` with a protected OAuth client and refresh token file and leave `LOCAL_MODE` unset. See [operations](docs/operations.md) and [deployment configuration](deploy/service.env.example). Never put credentials in frontend files or commit them.

## Test data

The production workbook is a read-only discovery source. Application writes target only the [personal test copy](https://docs.google.com/spreadsheets/d/19FXrunwWKK1pbyHqWNPcytmaDmyBQoK7yabzIdRQQYM/edit). Ownership and owner-only access were verified. Read/write/restore checks passed in both main data sheets; see [test evidence](docs/sandbox-test.md). The live adapter rejects any other spreadsheet ID.

Raw workbook exports, meeting documents, credentials, and local diagnostics stay under the ignored `.local/` directory or outside this repository.

## Context

See [the discovery notes](docs/discovery.md), [database findings](docs/database-findings.md), [meeting review](docs/meetings.md), and [schema](docs/workbook-schema.json) for the evidence. The [full feature catalog](docs/feature-catalog.md), [architecture](docs/architecture.md), [workflow details](docs/workflows.md), and [implementation plan](docs/implementation-plan.md) describe the intended application. The [history and selected undo proposal](docs/edit-history.md) supports routine editing through both the app and Google Sheets. [Recording language](CONTEXT.md) distinguishes biological observations from corrections. Delivery stages organize the work without limiting the final scope to the initial forms.
