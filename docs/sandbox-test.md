# Google Sheets sandbox write test

Completed 25 September 2026 in Ecuador, 26 September 2026 at 02:54 UTC.

[Open the personal test spreadsheet](https://docs.google.com/spreadsheets/d/19FXrunwWKK1pbyHqWNPcytmaDmyBQoK7yabzIdRQQYM/edit).

The file is named `Ithomiini database - PERSONAL TEST COPY - 2026-09-25` and is owned by `franz.chandi@gmail.com` in My Drive. Drive permissions returned exactly one permission, the personal account as owner. The copy retains 49 sheets.

## Checks performed

| Check | Result |
| --- | --- |
| Distinct spreadsheet ID from production | Passed |
| Personal ownership and owner-only access | Passed |
| Read Collection_data and Insectary_data | Passed |
| Write and read back a temporary Unicode note in Collection_data!AX9000 | Passed |
| Clear the temporary note and verify the cell is blank | Passed |
| Write and read back a temporary Unicode note in Insectary_data!AF13400 | Passed |
| Clear the temporary note and verify the cell is blank | Passed |
| Source workbook modification time unchanged across the test | Passed |

AX is Notes_Collection_data; AF is Notes_Insectary_data. Both selected cells were read as formulas first and verified empty before the test. The writes used RAW values, including Ñ. Each temporary value was verified through a separate API read, then cleared in a cleanup step and read again. No tabs, rows, columns, or biological records were added.

Every mutating request targeted the verified sandbox ID. Production access used read-only commands. The source modification time was `2026-09-25T21:18:14.673Z` both before and after the test.

## What this establishes

Authenticated Sheets API access can edit and read back the main data sheets in the personal copy. This removes the need for an R server merely to perform spreadsheet edits.

This test used the local gog OAuth client. A deployed phone app still needs its own login configuration, validation, formula preservation, coordinated ID assignment, retry handling, and tests involving multiple users. Browser login, offline entry, concurrent allocation, and chatbot actions have not been implemented or tested.
