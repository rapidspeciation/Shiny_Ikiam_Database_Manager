# Discovery

Started 25 September 2026, America/Guayaquil. This records findings before application implementation. Proposed workflows are not claims about features already built.

## Sources

- [Original Shiny database manager](https://github.com/rapidspeciation/Shiny_Ikiam_Database_Manager).
- [Wings gallery](https://github.com/rapidspeciation/Shiny_Ikiam_Wings_Gallery).
- [Ithomiini maps](https://github.com/rapidspeciation/ithomiini_maps).
- [Main workbook](https://docs.google.com/spreadsheets/d/1QZj6YgHAJ9NmFXFPCtu-i-1NDuDmAdMF2Wogts7S2_4/edit).
- [Meeting folder](https://drive.google.com/drive/folders/1VtSQ80TpomTybPGcfL1iexLeRkFJEAp-).
- T3 conversation `Plan Tiputini Datahub Prototype`, thread `e7c74b9a-f114-43ac-ab42-043567835a11`, 7–26 September 2026 UTC. Found in `/home/franz/.t3/userdata/state.sqlite` after inspecting both T3 installations. No matching thread was found in the Orchestrator V2 database.
- Tiputini's dated handoff at `/home/franz/Documents/Tiputini-datahub/app/docs/tiputini/README.md`, cut-off 25 September 2026.

## Existing applications

All three applications reference the same main workbook ID. The legacy manager's configured sheets are Collection_data, Photo_links, CRISPR, Insectary_data, Insectary_stocks, and Lists. Maps also uses Taxonomy_v18Jun25. The live workbook has more tabs than those applications use.

The current legacy manager code concentrates on deaths, tubes, and emergences. It stages edits locally and lets an authorized user commit them to Google Sheets. It matches insectary records by Insectary_ID and writes named columns. It does not implement the original prompt's Collection_data entry form. Its next insectary row follows preallocated identifiers after the last populated SPECIES row.

The gallery and maps publish processed JSON snapshots through manually triggered workflows. Their data is useful for display, but cannot establish the next unused live identifier. The gallery collection snapshot examined during discovery was last committed on 18 June 2026. Maps' processed specimen data was updated on 25 September 2026; it is filtered and cannot be compared directly with workbook row counts.

Implementation references:

- Manager `download_data.R:10–19` identifies the workbook and sheet GIDs; `Ikiam_DB_app.R:1285–1363` handles staged writes and `:1687–1691` selects the next emergence row.
- Gallery `scripts/process_data.py` downloads CSV sheets and joins photo links; `src/components/CollectionTab.vue` reads the generated collection JSON.
- Maps `scripts/process_data.py` downloads and processes sheets. `useCamidAutocomplete.js`, `GallerySearchSelect.vue`, and `useMobileLayout.js` are interaction references.

## Tiputini patterns to adapt

Tiputini groups exploration into photo, video, audio, table, map, time, and statistics views, with filters inside each view. Its chat uses saved threads and a separate results preview; the phone concept separates Chat and Results. These patterns can support specimen lookup and linked experiment records here without reproducing the entire research-station platform.

The September 25 handoff reports deployed exploration, document search, sharing, versioned plots, and personal Codex connection. The chatbot is restricted to administrators. It also identifies unfinished metadata and permissions work. Those reports describe Tiputini, not capabilities inherited by this new project. Personal Codex login there depends on its hosted application; it is not a ready-made browser-only chatbot for GitHub Pages.

## Hosting and Google access

GitHub Pages can host the interface. Publishing Pages from a private personal repository requires GitHub Pro, and the published site is publicly reachable. A private source repository does not protect data bundled into the site. GitHub Pages has no server-side runtime. [GitHub documentation](https://docs.github.com/en/pages/getting-started-with-github-pages/creating-a-github-pages-site).

A browser application can request a Google OAuth access token and call the Sheets API. Each user needs the necessary Google file permission. Tokens expire and renewal requires user interaction. The application needs its own OAuth configuration; the local gog login does not configure it. [Google token model](https://developers.google.com/identity/oauth2/web/guides/use-token-model).

The `drive.file` scope grants per-file access through an appropriate file-selection or opening flow. The broader `spreadsheets` scope covers the user's spreadsheets. [Sheets scopes](https://developers.google.com/workspace/sheets/api/scopes).

Reading the largest identifier and then writing the next identifier is not a single transaction. Two phones can choose the same identifier. A single Sheets request is atomic, but does not make separate reads and writes a transaction. [Sheets limits](https://developers.google.com/workspace/sheets/api/limits).

A small Apps Script service is a candidate for serialized ID assignment and writes while keeping Sheets as the main store. A script lock coordinates only cooperating script executions, so direct spreadsheet edits can bypass it. Caller authentication, deployment access, and quotas need a separate prototype. [Apps Script web apps](https://developers.google.com/apps-script/guides/web), [locks](https://developers.google.com/apps-script/reference/lock).

A managed API with a transactional database is another candidate if offline synchronization and stronger audit become requirements. A transaction in that database cannot atomically include a separate Sheets API call; Sheets would need to become a synchronized view or export. No provider has been selected.

An AI provider credential belongs in a hosted service. The initial assistant should answer questions from permitted records and cite those records. Proposed edits should go through the same validated entry operations as the forms. The user has not yet specified whether chat should write records.

## Original entry prompt to retain as requirements input

The prompt asks for collected/sent-to-insectary versus collected/preserved; sequential insectary and CAM identifiers; FS or FF tube identifiers; species and editable subspecies choices; identifier, collector, sex, location, collection date/time, weather, death and preservation fields, and notes. It requests defaults based on recent entries and ten newest records followed by a configurable Load more action.

The requested ID sequence increments the digit first, then the right letter, then the left letter. The alphabet includes Ñ. The live export shows that the historical sequence is already exhausted: Insectary_data row 12462 contains an assigned `9ZZ`, followed by `A0A` at row 12463. Later assigned records use a letter-digit-letter scheme. The original generator cannot be reused as the current allocation policy. Preallocated identifiers must be distinguished from identifiers assigned to specimens. A suggested ID is provisional until save succeeds.

Last-used collection or death dates can save typing but can also perpetuate stale dates. The form should make dates explicit and support a deliberate collection-session default. The original prompt is historical input, not proof that its column names or allowed values still match the workbook.

## Workbook instructions and source fields

The export contains 49 sheets, including several distinct crosses, mating, egg, life-history, and pheromone workflows. See [workflow proposals](workflows.md). A local read-only profile is generated by `scripts/profile_workbook.py`; the raw workbook and detailed output remain outside Git under `.local/discovery/`.

The [meeting review](meetings.md) covers all 23 numbered 2026 summaries found in the folder and 46 selected earlier summaries. It grounds the notebook-transfer problem, current clutch/cross practices, and sample workflows. The [sandbox test](sandbox-test.md) confirms actual API read/write/restore access in the personal copy. The [implementation plan](implementation-plan.md) combines these findings.

`Sheets_description!C16` defines Collection_data as field-collected butterflies, including wild-caught individuals introduced to the insectary. `C19` describes Lists as the dropdown source. The metadata itself has older names: `Columns_description!D19` documents a historical Collection_ID and reusing that mark on recapture, but the current collection headers distinguish FieldMark_ID, Insectary_ID, and CAM identifiers. Definitions must be checked against current headers and formulas.

`File_notes!C6:C8` asks maintainers to preserve structure and make insertions or deletions across whole rows. For this prototype, structural experiments belong in the personal sandbox copy. No source workbook structure has been changed.

Concrete formula references:

| Collection cell | Source in Insectary_data | Meaning |
| --- | --- | --- |
| AK2 | I:I, matched by Insectary_ID | Death_date |
| AL2 | M:M, matched by Insectary_ID | Preservation_date |
| AM2 | AA:AA, matched by Insectary_ID | Preservation_medium |
| AN2 | AB:AB, matched by Insectary_ID | Condition at preservation |

In the opposite direction, `Insectary_data!O2` looks up Collection_data.CAM_ID_insectary by Insectary_ID. Collection taxonomy, location, photos, tube racks, and manifests also contain formulas. A writable-field map needs to account for formulas at the individual cell, rather than assuming that an entire column is either entered or computed.

Lists contains separate CAM pools for different purposes. Its Insectary_batch column labels a historical June 2026 batch; it does not establish which batch is currently active or how identifiers should be allocated.
