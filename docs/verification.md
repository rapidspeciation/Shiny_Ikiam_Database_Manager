# Release verification

## Interface rebuild and backend review, 26 September 2026

The interface was rebuilt to follow the original Shiny manager (see DESIGN.md) and the backend was reviewed. The review confirmed, with local reproductions, and this release fixes:

- writes were never checked against the live header row, so an inserted column would shift every write;
- editing a row that moved in the Sheet before the next sync crashed with a database uniqueness error;
- a sync reading during a write could revert the app's copy and log a false external edit;
- the client-chosen action type became the history source, which also enabled formula writes and faked provenance;
- unknown field names were silently stored only in the app;
- tube and CAM IDs could be duplicated through edits;
- ten failed sign-ins by anyone behind the proxy locked out every user;
- each saved row cost three Google requests under a 60-per-minute quota, so a 50-row save took about two minutes and blocked other users;
- numeric-looking columns rejected workbook values such as `994(6)`, several date columns were typed as text or numbers, and pasted dates in `14-Aug-25` or `14/08/2025` form were refused;
- rows in sheets without identifier columns were relabelled after a row insertion;
- column filters on names containing dots, and malformed filters, failed.

A second independent review of the new code found, and this release also fixes: an edit of an unused row and a new row in the same save could target the same row; retrying a save whose outcome was unclear could create the new rows twice (the browser now keeps one request ID per set of changes, new rows wait while a save to that sheet is unconfirmed, and unconfirmed saves are re-checked automatically after a few seconds); syncs after rows were deleted and then inserted failed permanently on a database uniqueness error (this bug also exists in the first release); a row inserted at the top of a sheet without identifiers took the identity of the row below; edits typed while a save was running were dropped; one person's unsaved changes stayed on screen for the next person signing in on the same device; new rows could overwrite values typed into a free row in Google Sheets.

Automated checks: 37 server tests (14 new ones reproduce the issues above) and 6 frontend unit tests pass; `npm run check` passes the syntax and type checks. Browser checks against the full private snapshot in local mode, on desktop and phone sizes, had no page errors and covered: loading Insectary_data (620 KB compressed instead of 64 MB), pasting IDs and recording deaths for three butterflies, reviewing and saving, undoing from Historial, pasting into the grid with formula cells protected, creating two emerged adults in pre-filled rows N5D and N6D, assigning CAM and tube IDs with the WHOLE_ORGANISM rule and printing labels, and adding a collected individual with the next CAM ID from the Lists pool. `scripts/browser-smoke.mjs` repeats these checks against a running app.

Not yet verified against the live test Sheet or deployed; the earlier deployment below still runs the previous interface.

## First release, 25 September 2026

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
