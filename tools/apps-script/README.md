# Instant updates from Google Sheets

Without this script, an edit made directly in Google Sheets reaches the app at
its next full read of the workbook: up to 5 minutes, plus about 2 minutes to
read it. With it, the app re-reads only the edited rows within a few seconds,
and open pages pick up the change on their next check (every 10 seconds).

Install it in the **test workbook** only, for now.

## With gogcli

`./install.sh` creates the script inside the test workbook (or updates it) and
writes the secret into a `Config.gs` next to it. It needs a gog login that
includes the `appscript` service and the Apps Script API turned on at
https://script.google.com/home/usersettings. Then open the printed link, choose
`setup` and press **Run** once, accepting the permissions.

## By hand

1. Open the test workbook → **Extensions → Apps Script**.
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
