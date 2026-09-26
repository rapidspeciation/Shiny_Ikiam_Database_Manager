# Database findings

Profiled a fresh XLSX export on 25 September 2026. Authenticated Sheets metadata independently confirmed 49 tabs. The full schema, with live sheet names and IDs, exported names, header positions, and formula-column counts, is in [workbook-schema.json](workbook-schema.json).

Excel export removes characters such as `/` and truncates long sheet names. For example, the live `F1/F2_MutationRate` tab becomes `F1F2_MutationRate` in the XLSX file. The schema reconciles all exported names against authenticated API metadata. App requests must use the live names or sheet IDs.

## Recorded rows and unused allocations

| Sheet | Rows with observational or participant evidence | Other relevant finding |
| --- | ---: | --- |
| Collection_data | 8,019 | 178 rows have an ID but no qualifying observational fields |
| Insectary_data | 12,891 | 525 rows have an ID but no qualifying observational fields |
| Pheromones_data | 257 | Every CAM reference resolves across collection and insectary CAM pools |
| Melinaea_crosses | 50 | Parent identifiers resolve to insectary records |
| Melinaea_eggs | 28 | Mother and father identifiers resolve to insectary records |
| Stocks_Matings | 110 | Male and female identifiers resolve to insectary records |
| Crosses_Lys_x_Pol | 106 | Headers begin on row 6, not row 1 |
| Hybrid_Attempts | 33 | Separate schema for experiment, cage, maturity, and partner |

These counts describe evidence-bearing spreadsheet rows, not confirmed unique butterflies or validated experimental units. The two main sheets require a nonempty literal observation field rather than only an ID or calculated value. Formula caches were read but not recalculated. Summary tabs without a usable table header were left unprofiled. Missing identifiers can be intentional for some collection workflows.

## Identifier policy has changed

The assigned `9ZZ` at Insectary_data row 12462 exhausts the historical digit-letter-letter format. Row 12463 starts a letter-digit-letter sequence with `A0A`. Among recorded individuals, 440 unique identifiers use this later format, through `N4D` in the exported snapshot. That maximum is historical evidence, not an ID reserved for a future save.

Older records also use other identifier formats. The main insectary sheet has 95 repeated identifier values among evidence-bearing rows. None of those repeated values are in the old digit-letter-letter format. Collection_data contains one repeated identifier matching the CAM-number pattern. An identifier search should return all matching records, with enough context to select the correct one. These findings do not establish that every repeated value is an error.

Sorting the old format must compare left letter, right letter, then digit. For the proposed Spanish alphabet, useful boundary checks are `9AA < 0AB`, `9AN < 0AÑ`, `9AÑ < 0AO`, and `9AZ < 0BA`. The current format and preallocation need their own policy rather than extending that historical sort blindly.

## Links between sheets

| Reference | Target | Observed coverage |
| --- | --- | --- |
| Insectary_data.CAM_ID_CollData | Collection_data.CAM_ID_insectary | 357 of 357 distinct nonempty references match |
| Pheromones_data.CAM_ID | Union of Collection_data.CAM_ID and Insectary_data.CAM_ID | 257 of 257 distinct references match |
| Melinaea_crosses female/male | Insectary_data.Insectary_ID | All distinct parent references match |
| Melinaea_eggs Mother ID/Father ID | Insectary_data.Insectary_ID | All distinct parent references match |
| Collection_data.Insectary_ID | Insectary_data.Insectary_ID | 1,588 of 1,970 distinct references match evidence-bearing insectary records |

The unmatched collection references require interpretation. They may reflect historical marks, unused or incomplete rows, or other workflow distinctions. The profile does not automatically label them invalid.

## Preserve source meanings

Collection_data separates SPECIES and Subspecies_Form. Insectary_data often combines species, forms, and hybrid parentage in SPECIES. A shared search can display both while preserving their original values.

The preservation value `Alive` occurs alongside recorded death dates in 5,211 collection rows and 1,798 insectary rows. It cannot be used to count currently living butterflies. A live-status view needs an explicit rule based on relevant events, with missing or contradictory evidence shown clearly.

There are many calculated cells, including death and preservation lookups between the main sheets. Some columns mix literal values and formulas. Writes should be based on a declared operation and an allowed cell list, with formula checks at the target cells. Whole-row replacement would risk losing existing calculations.

The copied workbook reports `en_GB` locale and `Europe/London` time zone. The phone workflow operates in Ecuador. The app should handle date-only observations and local event times explicitly; changing the workbook's time zone is a separate decision.

## Reproduce the profile

Keep a workbook export under the ignored `.local/discovery/` directory and run:

```sh
python3 scripts/profile_workbook.py .local/discovery/source-database.xlsx
```

This uses the Python standard library. JSON and Markdown aggregates are written beside the workbook. Raw rows and personal contact values are not exported to the reports.
