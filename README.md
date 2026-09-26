# Ithomiini database

A phone and desktop app for collection, insectary, breeding, experiments, samples, and research records. The frontend and authenticated API run together on claudeclaw. The repository is private.

## Workflows

- Enter collected individuals with validated fields and current ID suggestions.
- Find a butterfly by its identifiers and show species, sex, origin, and recorded status.
- Record deaths, preservation, tissue tubes, and newly emerged butterflies.
- Link crosses, stocks, eggs, and pheromone samples to the relevant individuals.
- Explore records, photographs, and maps.
- Ask an AI assistant questions with links to the records supporting its answers.

Additional sections provide dated rounds, movements, treatments, custody and shipment observations, tasks, CSV import and export, attachments, reports, and reviewed AI proposals. Extra observations live in the app database and are labelled separately from spreadsheet records.

## Run and verify

Node 24 is required. There are no third-party runtime packages or build step.

```sh
npm run check
npm test
LOCAL_MODE=1 SECURE_COOKIES=0 SETUP_TOKEN=your-private-setup-token npm start
```

Open `http://127.0.0.1:8794/ithomiini/`. Local mode uses an isolated sheet adapter. For a private snapshot, set `SEED_FILE` to a JSON object containing `sheets`, keyed by exact sheet name, with `{row,cells}` Google grid rows. Keep snapshots outside Git.

For live use, configure `GOOGLE_CREDENTIALS_FILE` with a protected OAuth client and refresh token file and leave `LOCAL_MODE` unset. See [operations](docs/operations.md) and [deployment configuration](deploy/service.env.example). Never put credentials in frontend files or commit them.

## Test data

The production workbook is a read-only discovery source. Application writes target only the [personal test copy](https://docs.google.com/spreadsheets/d/19FXrunwWKK1pbyHqWNPcytmaDmyBQoK7yabzIdRQQYM/edit). Ownership and owner-only access were verified. Read/write/restore checks passed in both main data sheets; see [test evidence](docs/sandbox-test.md). The live adapter rejects any other spreadsheet ID.

Raw workbook exports, meeting documents, credentials, and local diagnostics stay under the ignored `.local/` directory or outside this repository.

## Context

See [the discovery notes](docs/discovery.md), [database findings](docs/database-findings.md), [meeting review](docs/meetings.md), and [schema](docs/workbook-schema.json) for the evidence. The [full feature catalog](docs/feature-catalog.md), [architecture](docs/architecture.md), [workflow details](docs/workflows.md), and [implementation plan](docs/implementation-plan.md) describe the intended application. The [history and selected undo proposal](docs/edit-history.md) supports routine editing through both the app and Google Sheets. [Recording language](CONTEXT.md) distinguishes biological observations from corrections. Delivery stages organize the work without limiting the final scope to the initial forms.
