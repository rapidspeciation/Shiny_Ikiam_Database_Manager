# Proposed first implementation

The discovery and sandbox API test are complete. Application implementation has not started. This plan combines the original request, the current workbook, and [meeting evidence](meetings.md).

## Application structure

| Section | Initial purpose |
| --- | --- |
| Today | Recent entries, clutch rounds, and entries still waiting to sync |
| Collection | Fast entry of a collected individual with session defaults and current taxonomy |
| Insectary | Find an individual, inspect species/sex/origin, record death or emergence, and inspect a clutch |
| Experiments | Crosses, egg outcomes, and linked pheromone/tissue samples |
| Explore | Filtered records, wing photographs, maps, and derived summaries |
| Assistant | Questions over permitted records with source links and saved results |

The first phone view should put specimen lookup and common recording actions within easy reach. Tiputini supplies useful examples for exploration and chat results. Its full hosted architecture is larger than this first application needs.

## First working slice

Implement authenticated lookup and a death-recording form against the personal copy, followed by new collection entry and batch emergence. This tests the user's most immediate actions and the formula-sensitive update path before adding the more varied experiment schemas. Include a clutch view early so staff retain the notebook's immediate overview of development and emergence.

The UI should show the selected record's identifying context before save and a clear saved/pending/error result afterward. A repeated submission must not create a duplicate event. Ambiguous IDs should return candidates rather than silently selecting a row. A source formula must remain a formula after an update to a related record.

Next, add clutch-round entry and cross/egg events. The meeting summaries show that the cross pivots depend on Insectary_data, Insectary_stocks, and F1/F2_MutationRate being current. Connect those operations explicitly instead of offering unrestricted cell editing. Pheromone extraction follows once the individual, treatment, and sample links are reliable.

## Hosting recommendation to test

Use a static phone interface and a small managed service for writes. GitHub Pages can host the interface if the personal GitHub plan supports private-repository Pages. Authentication must protect data access even though the site's assets are public.

An Apps Script service is the first candidate while Sheets remains the main database. A single script lock can serialize cooperating ID allocation and save operations. Prototype caller authentication, deployment access, browser communication, formula preservation, and retry behavior before committing to that design. A script lock does not coordinate direct manual edits to Sheets.

If reliable offline event synchronization, stronger transaction guarantees, or more demanding access controls become necessary, evaluate a managed API with a transactional store. That would be a larger data-model change because Sheets would become a synchronized view. The current task does not justify that migration yet.

Keep a write audit and request identifier in the sandbox design. Do not add columns to the original workbook merely to satisfy the new app. Any change to the production schema should be a separate migration after the prototype has established what is needed.

The chatbot needs a hosted AI connection with protected credentials and the same access rules as record lookup. Start with read-only questions, cited records, and derived tables or charts. Writing through chat remains a separate design decision. Personal Codex access in Tiputini is not a browser-only service that can be copied into GitHub Pages.

## Acceptance checks for the first slice

- A phone-sized interface finds both historical and current ID formats and displays species and sex without confusing preservation condition with life status.
- Every write is restricted to the personal test copy and permitted fields.
- Recording a death updates the intended insectary record; dependent collection formulas remain intact.
- Two simultaneous new-individual submissions receive distinct IDs according to the current allocation policy.
- Retrying the same request has one effect, and the UI distinguishes saved data from a pending or failed submission.
- Current row allocation preserves prefilled IDs, validation, and calculated cells.
- Date-only entries and local collection times remain correct despite the workbook's UK locale and London time zone.
- A second authorized account can perform the intended actions; an unauthorized account cannot fetch or modify records.

Offline behavior, operator roles, ID reuse rules, and any new fields needed for pheromone extraction remain questions for the prototype. The existing raw data should not be cleaned or migrated automatically to resolve those questions.
