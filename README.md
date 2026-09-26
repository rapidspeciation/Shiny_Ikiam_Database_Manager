# Ithomiini database

Discovery and sandbox for a phone-friendly collection and insectary application.

The discovery stage reviewed the live workbook, weekly meeting summaries, the previous Ikiam applications, and the Tiputini DataHub. The intended scope covers field, insectary, experiment, sample, and research workflows, with a GitHub Pages frontend and a separate backend/chatbot on claudeclaw.

## Intended workflows

- Enter collected individuals with validated fields and current ID suggestions.
- Find a butterfly by its identifiers and show species, sex, origin, and recorded status.
- Record deaths, preservation, tissue tubes, and newly emerged butterflies.
- Link crosses, stocks, eggs, and pheromone samples to the relevant individuals.
- Explore records, photographs, and maps.
- Ask an AI assistant questions with links to the records supporting its answers.

These are intended workflows, not implemented features.

## Test data

The production workbook is a read-only discovery source. Application writes will target the [personal test copy](https://docs.google.com/spreadsheets/d/19FXrunwWKK1pbyHqWNPcytmaDmyBQoK7yabzIdRQQYM/edit). Ownership and owner-only access were verified. Read/write/restore checks passed in both main data sheets; see [test evidence](docs/sandbox-test.md).

Raw workbook exports, meeting documents, credentials, and local diagnostics stay under the ignored `.local/` directory or outside this repository.

## Context

See [the discovery notes](docs/discovery.md), [database findings](docs/database-findings.md), [meeting review](docs/meetings.md), and [schema](docs/workbook-schema.json) for the evidence. The [full feature catalog](docs/feature-catalog.md), [architecture](docs/architecture.md), [workflow details](docs/workflows.md), and [implementation plan](docs/implementation-plan.md) describe the intended application. Delivery stages organize the work without limiting the final scope to the initial forms.
