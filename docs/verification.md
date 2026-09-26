# Release verification

Verified on 25 September 2026 in Ecuador, continuing into 26 September UTC. The application is hosted at `https://tbs-insect-gallery.duckdns.org/ithomiini/` and connects only to the personal test workbook.

## Automated checks

The final suite passed 21 tests and checked 24 JavaScript files.

`npm run check` checks the browser, server, scripts, and test files with Node's parser. `npm test` covers authentication and roles, HTTP operations, typed values and dates, preallocated formula IDs, field conflicts, selected undo, partial reversal recovery, row movement, synchronization during saves, private assistant threads, proposal review, reports, media request formats, and barcode encoding.

The full private workbook snapshot loaded into a disk-backed SQLite database in approximately 13 seconds. Observed record counts match the discovery profile: 8,019 collection rows and 12,891 insectary rows. The index contains 91,221 populated records across 25 supported tables, including reference and media records. That total is not a butterfly count.

## Live checks

- Created an authenticated temporary administrator and verified that anonymous requests cannot read records.
- Changed one Collection_data notes cell through the HTTPS API, verified the Google write, previewed undo, and restored the exact original value. An unrelated field remained unchanged.
- Created a test insectary observation in the existing unused N5D row. The ID and species formulas remained intact. Undo restored the blank observation fields and retained the reserved formulas.
- Generated complete overview, stage, cross, sample, quality, and weekly reports without the former 5,000-row truncation.
- Received a real model response with a source citation from a database query.
- Extracted a synthetic CAM label from an image and transcribed a synthetic field voice note. Both results required human review.
- Created a daily backup, restored it to a separate temporary database, and verified SQLite integrity, 99,634 cached rows, and its saved history. The temporary restore copy was then removed.
- Checked that the existing Tiputini site continued to return HTTP 200.

The initial large-response adapter caused noticeable pauses on the server. Reads now use bounded pages with a shared quota budget. Synchronization performs network reads outside the write queue and skips a stale fetched snapshot if an app save happened during that read. A regression test verifies this case. An unchanged workbook is skipped using its Drive revision.

## Browser checks

Chromium checks covered 14 desktop and phone routes with no page errors, failed application requests, or horizontal overflow. Screenshots were inspected together at 1440×1000 and 390×844. The review led to a persistent phone test-copy indicator, human-readable actor names, and useful recent specimen records.

A separate phone test installed the static app shell, disconnected the browser, reloaded the page, queued an observation, reconnected, revalidated the session, and verified exactly one saved event. API responses are not cached by the service worker. Drafts, offline metadata, and pending writes are scoped to the originating account.

The Impeccable detector flagged the warm background and colored shadows. The warm background was retained as the deliberate field-use palette; decorative shadows were changed to neutral colors. Accessible focus rings remain visible.

## Coverage and limits

All catalog areas have an interface through source-table forms, dedicated field/insectary actions, or dated app observations. Twenty workflow types cover field encounters, visits, death, preservation, rounds, movements, emergence, crosses, eggs, eligibility reviews, CRISPR, pheromones, dissections, life history, wings, DNA, custody, manifests, media, and data quality. Additional details that have no spreadsheet columns are labelled as app observations.

The app does not calculate scientific conclusions from unapproved definitions. Eligibility criteria, genotype evidence, and protocol-specific rate methods must be recorded by researchers. Direct Sheet snapshots cannot reveal every intermediate edit or its author. Simultaneous direct edits in the brief interval between a fresh read and a write remain a Google Sheets limitation. See [operations](operations.md) for deployment, recovery, and other limits.

Raw snapshots, screenshots, credential files, provider diagnostics, and verification account details stay in ignored `.local/` files or protected server directories. The temporary verification account is disabled before handoff. The user creates their own administrator account through a private setup link.
