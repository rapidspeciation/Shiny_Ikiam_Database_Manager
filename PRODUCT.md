# Ithomiini field and insectary app

<!-- impeccable:product-schema 1 -->

## Platform

web

## Stack

Implementation choice under the user's explicit instruction to build the agreed app autonomously: Node 24 with built-in HTTP, SQLite, crypto, and test modules; standards-based browser HTML, CSS, and JavaScript modules. This avoids new runtime package dependencies. Host the frontend and backend together on claudeclaw behind existing HTTPS.

## Users

Researchers and field/insectary staff recording and finding butterflies, clutches, experiments, and physical samples on phones, with desktop use for review, analysis, and administration.

## Product purpose

Replace repeated transcription from notebooks with immediate, attributable recording and an up-to-date working view. Support the full agreed capability catalog, including an AI workspace and selected undo.

## Operating context

The source workbook has 49 sheets with historical ID formats, duplicate marks, preallocated IDs, formulas, and linked research workflows. Both the app and Google Sheets will be edited routinely. Same-row concurrent edits are expected to be uncommon. The user wants inspiration from Tiputini's exploration and assistant rather than a copy of its separate project data.

## Capabilities and constraints

The full scope is docs/feature-catalog.md. The accepted write/history behavior is docs/edit-history.md. Initial app writes target only personal sandbox spreadsheet 19FXrunwWKK1pbyHqWNPcytmaDmyBQoK7yabzIdRQQYM. The production workbook is never a write target. Keep credentials and private data outside Git and separate from Tiputini. Support offline drafts, source-linked records, formula-preserving writes, explicit uncertainty, history and selective reversal.

## Evidence on hand

Current schema in docs/workbook-schema.json; meeting synthesis in docs/meetings.md; private exports and source documents under ignored .local/. The app implementation is in server/ and web/. The confirmed discussion supplies the product brief and implementation authorization; no new product interview is needed.

## Product principles

- Keep field recording fast and the selected specimen unmistakable.
- Preserve biological meaning and source evidence.
- Show saved, pending, and conflicted states accurately.
- Make correction and history ordinary parts of work.

## Working assumptions

Use a Spanish-first interface with an English option, Ecuador local dates, accessible large phone controls, and a bright high-contrast default suitable for field use. These interface choices are implementation assumptions based on the existing Spanish app and international research context, not additional user-supplied facts.
