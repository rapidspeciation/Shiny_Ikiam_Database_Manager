# Instant updates from Google Sheets

Without this script, an edit made directly in Google Sheets reaches the app at
its next full read of the workbook: up to 5 minutes, plus about 2 minutes to
read it. With it, the app re-reads only the edited rows within a few seconds,
and open pages pick up the change on their next check (every 10 seconds).

It is optional: the app works without it. Installing it adds a script bound to
the team's workbook (visible under **Extensions → Apps Script**), whose triggers
run as the person who ran `setup`; agree it with the workbook's owner first.
The copy installed in the old test workbook sends no workbook ID: after the
switch to the team's workbook, change `SHEET_HOOK_SECRET` on the server so the
app stops accepting it (or delete its triggers). The current script sends its
workbook's ID and the app ignores reports from any other workbook.

## With gogcli

`./install.sh` creates the script inside the workbook (`WORKBOOK_ID`, the team's by default) or updates it, and
writes the secret into a `Config.gs` next to it. It needs a gog login that
includes the `appscript` service and the Apps Script API turned on at
https://script.google.com/home/usersettings. Then open the printed link, choose
`setup` and press **Run** once, accepting the permissions.

## By hand

1. Open the workbook → **Extensions → Apps Script**.
2. Replace the contents of `Code.gs` with `SheetEditHook.gs`. Save.
3. **Project Settings** (gear icon) → **Script Properties** → add
   `HOOK_SECRET` with the value of `SHEET_HOOK_SECRET` from
   `~/.config/ithomiini/service.env` on the server.
4. Back in the editor, choose the function `setup` and press **Run**. Accept the
   permissions (edit triggers and calls to an external address). The log should
   end with `202 {"accepted":0}`.

The triggers run as the person who ran `setup`, and fire for edits by anyone.
Changes that no trigger reports (a formula recalculating because another
sheet changed) still arrive with the 5-minute read.

To remove it: **Triggers** (clock icon) → delete both triggers.
