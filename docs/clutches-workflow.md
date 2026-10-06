# Clutches: the team's workflow and the app (5 Oct 2026)

The project lead described how the team works with clutches on 5 Oct 2026.
This page compares each point with the Clutches and Emergidos tabs, before
and after the changes of that day, and says how each one works now.

## Gap analysis

| # | The team's workflow | Before (facc93f) | Now |
|---|---|---|---|
| 1 | About four people check different clutches at the same time | Done. Edits are staged and shown to everyone at once (server/staged.mjs, lib/staged.ts). Checks and events are polled every 20 s. A second edit of the same cell is refused until the person sees the first one. | Done. Photos and their counts are part of the same poll (`clutches/day`). |
| 2 | Register a clutch: eggs counted as a sum `=3+5+7`. Groups can be on different leaves or laid on different dates, each with its own date. | Partly. A new clutch took one number (`=12`). Later groups could be added with +N laid, but always as today. | Done. A new clutch takes groups (`3+5+7` → `=3+5+7`, with a + button because phone number pads have no +). In the editor, +N laid has a day: Today, Yesterday or another date. The note says it (`7 eggs laid on 4/10/26`). |
| 3 | Hatching per group or date: one egg can hatch a day later | Partly. +N hatched was recorded, always as today. | Done. Same day choice as above. The first hatch writes HATCHING DATE if it is empty, or if the new day is earlier. |
| 4 | Daily check: larvae alive, minus those that died, disappeared or were preserved, each labelled | Done. −N asks whether they died, disappeared or were preserved. | Done. The editor now also shows "To count today". |
| 5 | Counting convention: preserved larvae are not subtracted (20 larvae, 10 preserved, 5 pupated, 5 died → 15). A setting keeps the other rule available. | Missing as the default. The setting existed, but subtracting was the default. | Done. Not subtracting is the default on the server and in the frontend. The setting is kept, explained with this example, and Emergidos' saves follow it too. |
| 6 | Every event writes a dated, signed note in NOTES (died, disappeared, preserved as 3rd instar, hatched, pupated, emerged), and the person sees the text before saving | Missing. Events lived only in the app. Emergidos wrote one combined note ("2 larvae and 1 egg preserved 2/10"). | Done. Each event adds `d/m/yy INI: …` (lib/clutches `eventNote`). The text shows right after the event, while choosing "preserved", and as the amber pending line in NOTES. Saving to Google Sheets is a separate step. Undo removes the note again. Emergidos notes each day's adults and each group of larvae with their IDs. |
| 7 | Show per clutch: eggs not hatched, larvae to count today, and predicted hatching, pupation and emergence from each species' usual durations | Partly. Emergidos showed "emerging around" as pupa date + 8 days. | Done. Each card and the editor show what to count and the dates expected, from the median durations per species in the sheet (falling back to the genus, then all clutches). Emergidos uses the species' pupa duration. |
| 8 | Preserved larvae: each gets an Insectary_data row (LIFESTAGE default 3rd instar, CAM and Tube_1 suggested, Flash frozen, Sex NOT_COLLECTED). One panel serves both Clutches and Emergidos. | Partly. Emergidos had the cards (YoungPanel), with a 4th-instar default. Clutches only took IDs typed in a box. | Done. In Clutches, "preserved" → **Register N in Insectary_data** opens Emergidos' own cards for that clutch, full screen (EmergedCards `focus`, PreserveYoung). The IDs the cards took come back with the event and the note. The default stage is 3rd instar. |
| 9 | Review marks: the button should read as an action, then the card shows "✓ Checked by FCH" (in Spanish too) | Missing. The buttons read "Checked, no change" and "Verify…". | Done. The buttons read "Mark as checked" / «Marcar como revisado», "Mark as verified", and "Ask for a recheck…" / «Pedir verificación…». The editor's button reads "Save and mark as checked". The corner chip says "Checked by FCH". |
| 10 | Photos per clutch per day: several, from the camera or the gallery, each linked to an event or to the day, shown in the timeline (tap to zoom), stored on the server disk, resized, sent reliably on slow connections, never in the Sheet, backed up | Missing | Done (see below). |

Still open, by choice:

- Events and photos need a clutch that is already in the sheet. A clutch
  added in the app gets them once it has been saved to Google Sheets (as
  before).
- The app does not link each hatching to a particular laid group. The days
  are in the events and the notes.
- Before 5 Oct 2026, older clutches took pupated larvae off NUMBER OF LARVAE
  (`=27-2-11-3`). For those clutches, "To count today" can read 0 larvae
  when some are still alive. It is only a guide. Dates more than half a
  stage's duration overdue (at least 3 days) are not shown, so a clutch whose
  eggs dried does not show hatching forever.

## How it works now (phone)

1. **Clutches → a card.** The card shows the four counts, then
   "Count: 16 larvae", the next date expected ("pupa ≈ 12-Oct-26", bold when
   due), and a camera icon with today's photo count. Below are
   **Mark as checked** and **Ask for a recheck…**.
2. **In the editor**, a blue box at the top shows "To count today: 11 eggs
   not hatched" and "hatching ≈ 3-Oct-26 (due) · pupa ≈ 18-Oct-26 ·
   emergence ≈ 26-Oct-26". It also gives the days used ("Mechanitis
   lysimnia: egg 5 d · larva 15 d · pupa 8 d (its clutches)").
3. **Each count** has the row "When: Today · Yesterday · Other day", then the
   number box with **+N**, **−N** and **Counted**.
   - +3 hatched yesterday adds `=…+3`, sets HATCHING DATE if empty, and shows
     "Added to NOTES (saved with the clutch): 5/10/26 FCH: 3 larvae hatched
     on 4/10/26".
   - −5 asks Died / Disappeared / Preserved / Cancel. Died and Disappeared
     take the 5 off. Preserved keeps them counted (the default rule) and
     opens the stage buttons (3rd instar, 4th instar, other) with the note
     preview "5/10/26 FCH: 5 larvae preserved as 3rd instar".
   - **Register 5 in Insectary_data** opens one card per larva, with the next
     free Insectary IDs, the CAM and tube runs, and the batch panel (Flash
     frozen, rack, first CAM, Research_purpose). **Save** stages the rows.
     The editor then records the event with those IDs and adds the note
     `… (R0C, R1C, …)`.
   - **Undo** takes back the term, the event, its note and any date it set.
4. **Events and photos** (the timeline) lists the days, newest first. Each
   event has a camera button and a delete button. **Today's photo** opens a
   sheet with:
   - "What do they show?": "The clutch that day" or one of that day's events.
   - a caption: quick buttons (Dead larva, Sick larva, Eggs…) or typed text.
   - **Camera** and **Gallery** (several photos at once).

   Choosing a photo starts the upload. A row shows the thumbnail with "Making
   the photo smaller…", then "Sending… 34 %", "No connection: it retries by
   itself (2)" and "Photo saved". The thumbnail then appears under its event,
   or on the day. Tapping it opens the full-screen viewer (zoom, pan, turn,
   other photos as thumbnails). The viewer shows the event, the caption, who
   took it and when, and its size. The author or a reviewer can remove it.
5. **Footer**: **Mark as checked**, or **Save and mark as checked** when
   something changed. The ⚠ button asks someone to check it again.

## Photos: storage and transfer

- **On the phone.** The photo is decoded upright (the camera's EXIF
  orientation is applied) and fitted within 2560 px. It is encoded as JPEG
  quality 85 with a 480 px thumbnail at quality 80. The canvas writes no
  EXIF, so GPS and camera data never leave the phone.
- **Upload.** One upload carries the thumbnail followed by the photo, in
  256 kB chunks (`PUT /api/clutches/photo-uploads/<id>?offset=N`). The server
  acknowledges each chunk. If an offset does not match, the server answers
  409 with what it already has, so the phone resumes from there. Once all of
  it has arrived, `POST /api/clutches/photos` stores it; this call is
  idempotent by the upload's id.
- **Retries.** Uploads go one at a time across the whole page. They retry
  after 2, 5, 10 and 20 s, then every 30 s, and immediately when the phone is
  back online. After 8 tries they wait for **Retry**.
- **Tested in a headless phone browser.** Settings: about 400 kbit/s upload,
  300 ms latency, and the connection dropped for 8 s partway through. The
  upload resumed at the next chunk not yet acknowledged (offset 524288) and
  finished.
- **On the server** (server/clutch-photos.mjs):
  - The server checks that both parts are JPEGs and drops any APP1–APP15 or
    comment segment (EXIF, GPS, XMP, IPTC).
  - It uses Pillow only if a browser sent a photo still larger than 2560 px
    or turned by EXIF; without Pillow such a photo is refused.
  - Files go to `<dir>/<yyyy-mm>/<id>.jpg` and `<id>.thumb.jpg`. The default
    `<dir>` is `clutch-photos/` next to the database; `CLUTCH_PHOTO_DIR`
    overrides it, and the folder is in .gitignore. Unfinished uploads go to
    `<dir>/uploads/`, which is cleared after 3 days.
  - SQLite table `clutch_photos` records the clutch, day, linked event,
    caption, who, when, width, height, bytes and thumbnail bytes. Removing an
    event keeps its photos as the day's photos.
  - The photos are served only to signed-in users, as
    `private, immutable`.
- **Backup.** `scripts/backup.mjs` copies new photo files into
  `<BACKUP_DIR>/clutch-photos/` on every run. A photo never changes under its
  name, so each file is copied once and stays there even if it is removed in
  the app.
- **Sizes.**
  - A 12 MP phone photo (4080×3060) becomes 2560×1920. That is about
    0.6–1.2 MB for a typical scene: real notebook photos gave 600–650 kB.
    Worst case (pure noise) is about 2 MB.
  - The thumbnail is about 15–30 kB.
  - At ten photos a day, that is about 6–12 MB a day, or 2–4 GB a year.
  - At 400 kbit/s, a typical photo uploads in about 15–25 s.
