# Full application implementation plan

The discovery and sandbox API test are complete. Application implementation has not started. The [full feature catalog](feature-catalog.md) defines the intended breadth, combining the original request, workbook, meeting evidence, and useful additions. Delivery stages organize the work; they do not limit the final application to the first forms. The user has chosen routine editing through both the app and Google Sheets, with a combined [history and selected undo design](edit-history.md).

## Product areas

| Section | Purpose |
| --- | --- |
| Today | Daily rounds, tasks, reminders, recent observations, and synchronization status |
| Field | Complete collection entry, sessions and sampling effort, recaptures, locations, taxonomy, and media |
| Insectary | Individual histories, cage/clutch views, movements, emergence, development, deaths, and preservation |
| Experiments | Crosses, egg outcomes, pedigrees, treatments, pheromones, CRISPR, sperm, life-history, and barcoding workflows |
| Samples | Tissue splits, tubes, racks and storage, labels/scanning, manifests, shipments, and sample custody |
| Explore | Search, linked tables, photographs, maps, charts, saved views, and scientific/weekly reports |
| Assistant | Record and document retrieval, analyses and saved results, voice/photo drafts, and reviewed edits |
| Administration | People and roles, corrections, data-quality review, imports, audit, backups, and configuration |

These are capability groups, not a requirement to put eight tabs on a small phone. The interface should keep frequent actions easy to reach while giving every cataloged workflow an appropriate place.

## Delivery sequence

Begin with shared identity, authentication, formula-safe operations, ID allocation, audit, retry handling, and a mobile application shell. Build complete field and insectary recording on those foundations, including lookup, deaths, collections, emergence, clutch views, and offline drafts. Then extend the same record/sample operations into all experiment and laboratory workflows in the catalog.

The UI should show the selected record's identifying context before save and a clear saved/pending/error result afterward. A repeated submission must not create a duplicate event. Ambiguous IDs should return candidates rather than silently selecting a row. A source formula must remain a formula after an update to a related record.

The user expects simultaneous edits to the same rows to be uncommon and has accepted the combined history/undo approach. Keep conflict handling lightweight: fresh value checks, coordinated app writes, and review when a conflict is detected. Undo must still check for later edits made at different times.

The meeting summaries show that cross pivots depend on Insectary_data, Insectary_stocks, and F1/F2_MutationRate being current. Connect operations across those tables explicitly. Sample inventory, lab workflows, reporting, and AI belong to the planned application, rather than a list of features to omit after an initial prototype. Search, scanning, tasks, and AI lookup can be introduced whenever the supporting records and access rules are ready.

## Hosting direction

Use GitHub Pages for the frontend and a separate backend/chatbot on the existing claudeclaw server, following the user's preferred architecture. The read-only host check succeeded and found spare memory and disk. [Architecture details](architecture.md) record the capacity snapshot and the cross-origin authentication work needed.

The backend handles authenticated reads/writes, coordinated app ID allocation, audit, synchronization, jobs, and AI tools. It has separate state and credentials from Tiputini. The frontend calls HTTPS; SSH is only for administration. Apps Script is an alternative for a narrower integration, not the primary proposal now.

Retain Sheets as the biological record store during the trial. App-owned drafts, jobs, chat, tasks, and audit can have separate storage. A journal and Sheets updates are not one atomic transaction, so partial failures need explicit recovery. Direct spreadsheet edits require reconciliation with the app. A future biological database migration is a separate decision.

Keep a write audit and request identifier in the sandbox design. Do not add columns to the original workbook merely to satisfy the new app. Any change to the production schema should be a separate migration after the prototype has established what is needed.

The assistant should retrieve linked records and protocols, analyze permitted data, create charts and reports, and prepare single or batch edits through the same validated operations as forms. Voice and photographs can produce editable drafts. Show affected records and old/new values before applying conversational changes. Existing Tiputini components can inform this implementation, but provider accounts and data permissions must be configured for this app.

## Acceptance checks across the scope

- A phone-sized interface finds both historical and current ID formats and displays species and sex without confusing preservation condition with life status.
- Every write is restricted to the personal test copy and permitted fields.
- Recording a death updates the intended insectary record; dependent collection formulas remain intact.
- Two simultaneous app submissions receive distinct IDs according to the current allocation policy. Routine direct Sheet allocations are reconciled and any detected collisions are surfaced.
- Retrying the same request has one effect, and the UI distinguishes saved data from a pending or failed submission.
- Current row allocation preserves prefilled IDs, validation, and calculated cells.
- Date-only entries and local collection times remain correct despite the workbook's UK locale and London time zone.
- A second authorized account can perform the intended actions; an unauthorized account cannot fetch or modify records.
- Field entry includes the original requested fields, editable suggestions, recent defaults, and configurable recent-row browsing.
- Cage/clutch views, recaptures, pedigrees, samples, photographs, and manifests resolve the same underlying individuals consistently.
- Offline drafts survive restart, show pending status, and provide a clear resolution for conflicting changes.
- Experimental outcomes preserve uncertainty. Eggs do not establish observed mating or fertilization, and analyses define denominators and exclusions.
- Voice/photo extraction and AI edits produce inspectable drafts and use the same permissions and validation as manual forms.
- Generated analyses and weekly reports retain source versions and distinguish observations, missing records, and proposed actions.
- Selected undo preserves unrelated edits, records a new linked reversal, checks known later field changes and dependencies, and clearly labels incomplete evidence from direct Sheet edits.
- Reconciliation distinguishes entered values/formulas from recalculated results and does not invent an editor or a complete sequence for snapshot-only differences.
- Backups, restoration, rollback, and separation from Tiputini are verified before relying on the app for daily work.

Detailed decisions include the current ID/reuse policy, role permissions, offline working sets, cage naming, notification channels, label hardware, missing experimental fields, AI providers, scientific analysis definitions, and the direct-edit observation mechanism. Routine direct Sheet editing remains supported. The catalog keeps these visible without discarding the broader intended scope. Existing data is not automatically cleaned or migrated to settle them.
