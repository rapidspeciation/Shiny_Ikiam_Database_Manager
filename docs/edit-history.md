# Writing, history, and selected undo

The user has accepted this history and selected-undo approach and expects simultaneous edits to the same rows to be uncommon. People will routinely edit through both the app and Google Sheets. This document records the accepted design. The deployed app, snapshot reconciliation, journal, and selected undo are described in [verification](verification.md). Direct Sheet history remains limited to observed snapshot differences.

## Expected use and implementation scope

Optimize for ordinary saves with automatic history and occasional selected reversals. Use lightweight checks of current record identity and target values, serialize cooperating app writes where needed, and show a clear review prompt when a conflict is detected. The low expected overlap supports a simple first implementation.

Keep later-edit checks for undo: an older change may have been superseded hours or days later even when nobody edited simultaneously. Durable history, formula preservation, safe retries, and reconciliation remain useful under this operating assumption. Add more elaborate collaboration controls only if observed use justifies them.

## Recommendation

Use one durable history service on claudeclaw. Log app writes before sending them to Google and verify the result afterward. Observe direct spreadsheet edits through a Sheet-side edit/change notifier plus backend reconciliation. Present both sources in one history screen, while recording their different evidence quality.

Undo is a new, linked reversal action. It preserves the original edit, records who requested the reversal, and changes only the selected information that is still eligible to reverse. Redo is also a new action, with the same checks. Neither restores an entire earlier spreadsheet snapshot during ordinary use.

## Why the old implementation needs replacement

The legacy Shiny app already recorded user, time, batch, field, and previous/new values. It provided selected undo, so the interaction is worth preserving. However, it stored values as strings, mapped rows using a local Insectary_ID snapshot, and wrote earlier values back without checking for intervening edits. Its history undo did not create a new audit entry. Rectangular writes could also include neighboring values from a stale local copy.

References in the original `Ikiam_DB_app.R`: change records at lines 379–402, writer at 1285–1599, pending undo at 2915–2965, and uploaded-history undo at 3209–3280. The replacement should use stable action selection, current record resolution, exact field updates, typed values and formulas, and recorded reversals.

## What the history records

An action groups one intent, such as recording a death and its cause, registering a batch of emergences, or correcting selected specimen descriptions. Each affected field has its own before/after detail.

| Information | Purpose |
| --- | --- |
| Action ID and submission ID | Bind selection to the exact action and recognize retries |
| Stable record identity and biological identifiers | Find the intended record after rows move; retain familiar Insectary/CAM/tube references |
| Workbook, sheet, field, and observed cell location | Explain and verify the destination without using row number as identity |
| Exact previous and resulting entered values | Preserve numbers, booleans, empty values, strings, and formula expressions |
| Initiating user and mechanism | Distinguish manual app entry, approved AI action, import, direct Sheet event, or reconciliation |
| Recorded/observed times and biological event dates | Separate entry timing from when the observation happened |
| Reason, attachments, and linked action | Explain a correction, withdrawal, undo, redo, or related sample operation |
| Known field history and relevant dependencies | Detect later changes, repeated-value changes, and records that depend on the action |
| Write outcome and evidence | Distinguish pending, verified applied, rejected, uncertain, conflicted, and reversed operations |

Use a transactional server database for this journal and its indexes, with backed-up append-only audit events. Operational status indexes may change; prior audit events remain. This is a conventional change journal, not a requirement to reconstruct the entire biological database from events or migrate away from Sheets.

## App write path

1. Authenticate the user and validate the named operation. Resolve the current record and permitted fields, including formula checks. The personal copy remains the only write target during the trial.
2. Record the requested action and exact before/after patch durably before dispatch. Serialize cooperating app operations that touch the same records or allocate IDs.
3. Read the current target values and identity again. If a detected change invalidates the request, return a conflict for review rather than using the stale values shown when the form opened.
4. Submit only the intended cells and field masks. Group related updates within the same spreadsheet into one supported atomic batch where possible, rather than rewriting rows or saving unrelated neighboring values.
5. Read back and reconcile the result before reporting it as verified. A timeout produces an uncertain operation, not an automatic failed/safe-to-repeat conclusion. Resume recovery after a service restart.
6. Persist the observed outcome and return a clear saved, pending verification, or conflict result to the phone.

Google documents atomic application of a single `spreadsheets.batchUpdate` request. It does not make that request atomic with a separate server journal, and it explicitly discusses collaborator interference. [Sheets batchUpdate](https://developers.google.com/workspace/sheets/api/reference/rest/v4/spreadsheets/batchUpdate).

A same-batch operation receipt can help recover an ambiguous timeout. Its identity, retention, storage limits, and collision handling need testing. Finding a matching retained receipt can establish that a batch applied; failing to find one does not prove nonapplication after external deletion or structural changes. Do not claim unconditional exactly-once execution from a receipt alone.

## Direct Sheet edits

An installable edit/change notifier can report changed ranges or structural activity promptly. A backend shadow of entered values and formulas supports comparison and catches changes after missed events. Periodic reconciliation remains necessary. The notifier is an observation mechanism, not the primary write service.

App writes already have their own journal and must not be counted again as new external edits during reconciliation. Match expected effects and receipts cautiously; simultaneous external activity can require a conflict or uncertainty entry rather than automatic attribution.

Google limits the evidence available for native edits:

- Script and API changes do not fire ordinary edit triggers.
- Edit-event `oldValue` and `value` are available only for single-cell edits.
- An event's editor identity is available only under permitted security conditions.
- A comparison between snapshots can detect a net difference while missing intermediate changes or a change that returns to the original value.

These limits mean bulk pastes, deletions, structural changes, third-party scripts, and rapid intervening edits need more cautious treatment. Store an observed range difference when that is the available evidence. Do not invent a precise editor, original edit time, typed prior value, or complete sequence. Formula recalculation alone should not be attributed as a user's edit to the calculated result. [Apps Script triggers](https://developers.google.com/apps-script/guides/triggers), [event fields](https://developers.google.com/apps-script/guides/triggers/events).

Native version history remains a complementary recovery tool. The Drive revisions API can omit or merge editor revisions, so it is not a complete per-cell audit feed. [Drive revisions](https://developers.google.com/workspace/drive/api/guides/manage-revisions).

## Selected undo rules

| Situation | Intended behavior |
| --- | --- |
| The target field still reflects the selected edit and its known history has no later change | Offer a checked reversal after a fresh read, subject to the direct-edit concurrency limitation below |
| Someone later changed another field on the same individual | Preserve that other field while reversing the selected eligible field |
| Someone later changed the same field | Show original, later, and current values; offer a deliberate new correction rather than silently overwriting the later change |
| A known later edit returned the field to the same text | Still treat it as later history; equal text alone does not establish eligibility |
| Several selected edits affect the same field | Reconstruct and preview their ordered chain; never apply independent old values in selection order |
| The target is a formula | Preserve or restore its entered formula expression; never replace it with the displayed result |
| A created record has later offspring, samples, photos, or experimental references | Show dependencies and offer a reviewed withdrawal/correction; do not delete it blindly or automatically cascade |
| A selected field is coupled to other fields by the operation's rules | Preview the required group and validate the resulting record; field selection cannot create an invalid biological record |
| A direct Sheet change has incomplete prior evidence | Offer only the supported correction/snapshot comparison with its uncertainty clearly shown; exact undo may be unavailable |
| The row was deleted or the identity is ambiguous | Stop automatic reversal and resolve identity and dependencies first |

Undoing a recording does not reverse a physical dissection, preservation, shipment, or death. For an already labeled individual or tube, reversing a mistaken registration should not automatically make the printed identifier available for reuse.

### Concurrency limit with routine direct editing

The documented Sheets update schema does not provide a general expected-value or expected-revision condition for a batch write. This conclusion follows from the API request schema. A separate read/compare/write can therefore race a direct human edit. A backend lock protects cooperating app operations only. [Batch request schema](https://developers.google.com/workspace/sheets/api/reference/rest/v4/spreadsheets/batchUpdate), [UpdateCellsRequest](https://developers.google.com/workspace/sheets/api/reference/rest/v4/spreadsheets/request).

Fresh reads, known field versions, post-write verification, reconciliation, and conservative conflict handling reduce the risk; they do not prove that no human edit occurred in the gap. The UI must describe a checked reversal, not promise perfect rollback under unrestricted concurrent native editing. Stronger prevention would require a mutually enforced editing protocol or access restriction during the operation, which is not assumed for this design.

## Stable record identity

Do not address historical edits solely by sheet row number or a potentially repeated biological mark. Give app records stable identities and verify their mapping to the live workbook. Row-attached developer metadata is one candidate: Google documents that it follows its associated row when locations move, and is removed when that row is deleted. Metadata keys are not unique. Copies, duplicate records, deletion, partial-column sorting, and metadata limits require explicit handling before selecting the final identity mechanism. [Developer metadata guide](https://developers.google.com/workspace/sheets/api/guides/metadata), [metadata resource](https://developers.google.com/workspace/sheets/api/reference/rest/v4/spreadsheets.developerMetadata).

## History interface

Provide a global history and an individual/sample timeline. Filter by date, editor when known, source, sheet, individual, field, action, and outcome. Expand an action to inspect its before/after fields and related records. Select actions or eligible fields, preview the proposed reversal, see conflicts and dependencies, then apply it with a reason. Each reversal links to its original action, and redo appears only when currently valid.

Routine authorized entries should save promptly. Bulk imports, AI-generated batches, and conflicted reversals can have explicit review. The old system's manual upload step need not delay every field observation.

## Verification scenarios for implementation

- Two app users change different fields, then one reverses their own edit without affecting the other.
- A human edits the same field in Sheets before or during an app save or reversal; known conflicts are reported, and observed race outcomes are retained without false success guarantees.
- A field changes A to B to A; a known later change prevents treating the old action as untouched.
- Several selected actions affect the same field and interleave with unselected actions.
- Direct single-cell entry, multi-cell paste, formula change, row insertion, row deletion, and external API writes are reconciled with accurate evidence labels.
- A lost response or process crash after the Sheet write is recovered without a blind duplicate action.
- A formula, date, number, empty cell, and literal string round-trip with their correct entered types.
- A reversed creation has dependent offspring or samples, and an issued physical label is not silently recycled.
- Journal backup/restore retains original actions, linked reversals, observed external changes, and uncertain states.
