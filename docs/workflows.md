# Workflows to prototype

These proposals follow the September 25 workbook export. The [sandbox write test](sandbox-test.md) passed for both main sheets. The [meeting review](meetings.md) identifies the recurring notebook-transfer delays and clutch/cross recording needs. These notes cover the initially examined workflows; the [feature catalog](feature-catalog.md) supplies the full scope and the [implementation plan](implementation-plan.md) orders delivery.

## Phone entry and lookup

The first screen should make it easy to find a butterfly or record a new event. Searching should accept the insectary mark, CAM identifier, or tube identifier and show the matching record's species, sex, origin, and relevant dates. When an identifier matches more than one record, show the candidates with enough context to choose. An identifier alone is not yet proven to be a globally unique key.

Collection entry should use Collection_data, Locations, Lists, and the current taxonomy. The actual location tab is `Location_data`. The current workbook names the cross-reference `CAM_ID_insectary`, rather than the historical prompt's `CAMID_insectary`. The notes field is `Notes_Collection_data`.

The two original collection choices remain useful, but the workbook also records `Mark_Released` and `Released_Unmarked`. Those require different fields from a preserved sample. FieldMark_ID, Insectary_ID, CAM_ID_insectary, and CAM_ID should remain distinct identifiers.

A repeated collection session can remember location, collector, weather, and collection date. Dates should remain visible. A new session should not silently inherit an old death or preservation date. Recent records can start with ten rows and a configurable Load more control, as requested.

## Deaths and preservation

Selecting a butterfly should establish the exact source sheet and row before editing. Insectary_data records Death_date and Death_cause separately from Preservation_date, preservation media, and tissue assignments. Collection_data has its own death and preservation columns, with formulas in some rows that refer to insectary records.

A death form should record the event once at its source. It should not overwrite the same value again in formula-driven copies. A missing death date does not by itself establish that a butterfly is alive. Disappearance, observed death, and deliberate preservation can have different meanings.

`Preserved_dead_alive` and `Preserved_Dead_Alive` concern condition at preservation. These must not be used as present-day life status. For sent-to-insectary records, Collection_data cells AK:AN can look up death and preservation values from Insectary_data by Insectary_ID.

## Emergence and stocks

Insectary_data links to a clutch through `CLUTCH NUMBER` and records Stock_of_origin, Wild_Reared, species, sex, and Intro2Insectary_date. Insectary_stocks holds clutch counts and dates for eggs, hatching, pupation, and emergence. Some stock counts are formulas calculated from individual records.

A batch emergence form can choose the clutch once, display its species and origin, then enter individual sex and date. Allocating identifiers and filling the next appropriate unused rows must preserve the existing preallocation and formulas. Counting every row with an identifier as a butterfly would include unused rows.

The old digit-letter-letter sequence has already reached an assigned `9ZZ`. Subsequent records use letter-digit-letter IDs beginning at `A0A`. The app must implement the current allocation policy, rather than restart the sequence in the historical prompt.

## Crosses and eggs

Cross experiments are distributed across several tabs with different schemas:

| Tab | Recorded workflow |
| --- | --- |
| Melinaea_crosses | Female/male pairing, start and finish, mating times, outcome, and derived clutch totals |
| Melinaea_eggs | Clutch, parents, laying date, egg counts, preservation, and sample tube |
| Stocks_Matings | Male/female IDs, mating date/time, and derived age and species checks |
| Crosses_Lys_x_Pol | Attempt, female/male IDs, treatment, mating and finish dates |
| Hybrid_Attempts | Individual, experiment/cage, start and maturity dates, partner, and mating date |
| F1/F2_MutationRate | Pedigree, parents and grandparents, clutches, pairing, and tissue samples |

These should share specimen lookup and date controls, but should not be forced into one generic spreadsheet form. A paired-individual selector can display recorded sex, species/form, origin, and age before a mating event is saved. A clutch can link a cross to eggs, stock observations, and emerged individuals.

## Pheromone samples

Pheromones_data records CAM_ID, Source, species/form, Wild_Reared, Treatment, location, and three tissue tube pairs. Some tube identifiers are formulas. The lookup needs to resolve both collected and reared specimens rather than searching Collection_data alone. A sample form can select the source individual and treatment, then record the relevant tissues while preserving calculated fields.

## Exploration and assistant

Adapt Tiputini's table, gallery, map, and saved-result patterns around biological records. Collection, insectary, and experiment filters should use their own definitions of species, dates, and status. The gallery and maps can supply interaction references, while current data comes from the permitted database.

The assistant can answer questions such as which records match an ID, which crosses involve a butterfly, or which samples lack a tube assignment. Answers should link to exact records and distinguish missing data from a biological fact. The expanded scope includes source-linked analyses, voice/photo drafts, and reviewed changes through the same operations as forms. Hosting now follows the proposed claudeclaw backend; the provider, detailed access rules, and tools remain implementation decisions.

## Decisions still requiring evidence

- How current identifier batches work, whether old marks are reused, and how to disambiguate a repeated mark.
- Which sheets and fields are the source for each write, and which fields are calculated.
- Which of the workflows established by the weekly meeting summaries field staff would prioritize in the first phone trial.
- Whether phones must record events without connectivity, and how queued submissions should behave when an identifier or record has changed.
- Who can enter, correct, or review records. Routine direct spreadsheet editing is part of the workflow; its [history and reversal limits](edit-history.md) must be visible.
- How much of the application should be publicly reachable, and whether the personal GitHub plan supports Pages from a private repository.
