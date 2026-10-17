---
name: photos
description: The photos of the butterflies' wings, from the camera to the workbook — naming the files after the CAM_ID (the Rename Photos app, the app's «Photos» tab) and double-checking a batch before it is renamed (rename_photos_read, rename_photos_look, rename_photos_edit), photos uploaded without being renamed (drive_photos), showing butterflies' photos (show_photos), the folders on Google Drive and putting them in order (organize_photos: rename, move, remove duplicates, make folders, shortcuts), the Photo_links sheet that Photo_dorsal and Photo_ventral read (sync_photo_links), and moving the photos to another account to free space, and bringing the Wings Gallery and the maps up to date (update_websites). Use it when the person uploaded, renamed, moved or deleted photos, asks to update Photo_links or the photo links, says a photo shows Not Found or the wrong butterfly, asks which photos are missing, how to rename photos, where to put them, to tidy or delete photos on Drive, about the Drive space running out, about the Wings Gallery or the maps not showing new data, or presses «Review with Claude» in the «Photos» tab.
---

# Wing photos and Photo_links

## From the camera to the sheet

1. **Photograph** each preserved butterfly's wings twice, dorsal and ventral,
   with its envelope in the picture (the CAM_ID is read from it), the ruler
   and the colour card. The camera saves a JPG and a RAW file of each
   (details below).
2. **Rename** the files after the CAM_ID, on the computer, with the Rename
   Photos app (below).
3. **Upload** them to the project's Drive folder «Photos», into the folder of
   their collection (below).
4. **List** them in the Photo_links sheet: `sync_photo_links`, or «Read
   Drive» in the «Photos» tab's «Photo_links y sitios».
5. The sheets then show them: Photo_dorsal and Photo_ventral of
   Collection_data, Insectary_data and CRISPR are formulas that look the
   file's name up in Photo_links and link to it. The Wings Gallery
   (https://rapidspeciation.github.io/Shiny_Ikiam_Wings_Gallery/) reads the
   same sheets when its data is updated: `update_websites` (or the
   «Photos» tab, or the gallery's own «Update DB» tab).

Photos that are on Drive but not yet in Photo_links show `NOT FOUND`
(`Not Found` in Collection_data) in their row: step 4 is what makes them
appear.

## Taking the photos

From the team's «Photos Processing Protocol» (Google Doc, in the project's
Drive):

- The camera on the articulated arm, level (a bubble-level app on the phone,
  the camera's grid), the ruler at the bottom of the frame straight, and the
  pencilled cross of the board a little left of the centre: the colour card
  on the right takes more room than the envelope on the left.
- Manual focus on one wing in the centre, zoomed in; checked again often.
- One wing per quadrant, dorsal side first (the side that touches when the
  wings close; a male's hair pencils show on it). Then the wings are flipped,
  the left ones to the right and the right ones to the left, for the ventral
  photo. So each butterfly has two photos taken one after the other, dorsal
  then ventral.
- A remote shutter or the 5-second timer, so the camera does not shake.
- The photos are copied from the card into a folder per batch, named by its
  date, and renamed there before they are uploaded.

## File names

- `<CAM_ID>d.JPG` and `<CAM_ID>v.JPG`, the ID exactly as the row's CAM_ID
  has it: `CAM078038d.JPG`, and `M6285d.JPG` for a sample that keeps its
  collector's ID. Capitals do not matter; anything else does:
  `CAM077986_d.jpg`, `CAM077986d (1).JPG` and `CAM077986d.jpeg` are not found.
- The RAW file has the same name with its own ending (`CAM078038d.CR2`,
  `.ORF`).
- A wild butterfly taken to the insectary (`Collected_Sent2Insectary`) is
  looked up in Collection_data by its CAM_ID_insectary.
- CRISPR butterflies have more than two views (`CAM074023d1`, `v2`) and
  phone photos (`.HEIC`). The CRISPR sheet looks them up through Photo_links'
  last column, which reads `.jpg`/`.HEIC` as `.JPG` and `d1`/`v1` as `d`/`v`.
- When the CAM written on the envelope is wrong (an extra digit, a repeated
  CAM that was renumbered), the file takes the row's CAM. The envelope is
  corrected by hand; the photo does not need retaking.

## Folders on Drive

Everything is under «Photos»:

| Folder | Holds |
|---|---|
| Photos wings of collected butterflies | wild butterflies, in a folder per country and year (`PERU_2024`, `Brasil_2025`, `COLOMBIA_2025`…); Ecuador up to 2025 in `JPG_photos` and `Raw_photos` |
| Photos wings of insectary individuals | butterflies reared in the insectary (crosses, families) |
| Wings of reared butterflies / Wing clipped individuals | wing clips |
| Photos CRISPR butterflies | CRISPR butterflies |

- JPG and RAW files go in separate subfolders.
- A folder whose name ends in `_temp` is never listed: it is where photos
  wait that are not renamed yet, repeated or bad. A file whose name ends in
  `_todelete` is left out too.
- A new folder inside a listed one is listed by itself. A new folder beside
  them is offered by `sync_photo_links` (`notListed`) and listed once the
  person says so (`add`).
- A link follows the file, not its name: renaming or moving a file or a
  folder in Drive keeps the links. A copy is another file: when photos are
  copied to another account and the originals deleted (to free space), the
  rows are linked again by name with `sync_photo_links`.

## The Rename Photos app

It is the «Renombrar» part of the app's «Photos» tab (and, for people without an account,
https://fr4nzz.github.io/rename_photos_AI/). It runs in the person's
browser, on the photos in a folder of their computer, before they are
uploaded: the photos stay on the computer and it needs no key. Chrome or
Edge (Brave after switching on the setting the app shows); other browsers
can read the folder but not rename in it. In the tab it checks the IDs
against the workbook as it is now, and «Review with Claude» opens a chat
beside it that double-checks the batch (below).

1. «Open folder», then «Read photos»: it reads the CAM_ID on each envelope
   (about a second per photo) and turns each photo and its RAW file so the
   envelope reads upright.
2. «Review»: a card per butterfly with its photos side by side; those that
   need a look come first. Confirm or correct each (Enter, «Confirm», or one
   of the suggestions). The strip at the top shows the CAMs in the order the
   photos were taken, where gaps and repeats stand out.
3. «Rename Files». RAW files are renamed with their JPG. A log in the
   folder's `rename_files/` lets «Restore» undo it.

What sends a photo to review, which tells what to check:

- the CAM it read is not in the workbook (in the tab, the workbook as it is
  now; on its own page, a copy read once a day), so the butterflies' rows
  are typed before their photos are renamed;
- the CAM is far from those of the photos taken just before and after;
- the same CAM on photos that were not taken one after the other, or on more
  than two;
- its envelope points another way than the rest of its session (photos
  taken less than half an hour apart): the photo takes the session's
  rotation and is flagged, in case it was the camera that was turned.

It reads IDs written as `CAM` and six digits. Other IDs (the Colombian
`M6285`) are typed in the review, or read in its Gemini mode (the sparkle
button; it needs a Gemini API key, and its prompt names the ID's form).
Its repository, with how the reader works and how accurate it is:
https://github.com/Fr4nzz/rename_photos_AI.

## Reviewing a batch before it is renamed

The person pressed «Review with Claude» in the «Photos» tab, or asks
you to check the photos they are renaming. The tab is your hands: it stays
open on their computer, and the three tools read the batch from it, get
pictures of the photos you ask for, and correct its rows. Renaming is their
own click («Rename», or «Rename & rotate») once the batch is right.

1. `rename_photos_read`: the IDs in the order photographed (`order`), what
   does not add up (`checks`), and the photos to look at.
2. `rename_photos_look` at those photos' envelopes, several a call. An
   envelope has its ID in handwriting: `CAM` and six digits; an ID crossed
   out on it does not count. The digits most often mistaken for each other:
   1 and 7, 3 and 8, 4 and 9, 0 and 6, 5 and 6. The order helps as much as
   the handwriting: the envelopes come out of a box in a run of IDs, and a
   butterfly's two photos are taken one after the other.
3. `rename_photos_edit` what is clear; `leave` the rest for the person with
   the reason.
4. Look at a sample of the photos nothing flagged as well, a dozen or two
   spread over the batch: the reader is right on nearly all of those, and a
   wrong one in the sample means looking at more around it. The sample is
   for the IDs: the app's wing check (`wingCheck`) has compared every
   photo's wings with its view and its turn, and flags those that differ.

| Found | Most likely | What to do |
|---|---|---|
| `out-of-sequence`, or `not-in-database` with a `suggest` or `maybe` | a misread digit | read the envelope; set the ID when it fits the IDs before and after and the workbook has it |
| `no-reading` | the envelope is partly out of the frame, covered by a wing, or faint | `part: "photo"` shows the whole photo; set the ID you can read; one you cannot read is the ID of the single photo beside it only when that leaves both with two photos |
| `notTwoPhotos`, one photo | its other view carries another ID (often the flagged photo beside it), or was not taken | find it beside it; otherwise `leave`: the view is missing, to photograph again |
| `notTwoPhotos`, three or more | a view photographed twice (the first one blurred), or two butterflies given one ID | look at the whole photos: `skip` the worse of a repeat; two butterflies are for the person |
| `apart` | the later photos are misread, or one CAM is on two envelopes | read both envelopes; a CAM on two envelopes is for the person (skill `data-rules`, duplicates) |
| `notInWorkbook` without `maybe` | the butterfly's row is not typed yet (IDs running on after the workbook's last ones) | say which IDs: their rows are typed before the photos are uploaded |
| `alreadyPhotographed` | photographed again on purpose, or an older ID read by mistake | read the envelope; if it is right, ask whether the new photos replace the old ones |
| `turned` | an envelope laid down turned: the photo already follows its session | look at the whole photo; `rotate` it only if it shows turned |
| `turnUnsure` | the envelopes lay sideways, or point both ways: their direction does not say which way up the photos are | look at the photos of `lookAt` whole, and turn them all the same way if needed (below) |
| `repeated-id` | as `apart` | |
| `wing-side`: the d and v photos swapped | a view set by hand against the wings | look at both whole photos; when they agree with the wings, set `view` of each |
| `wing-side`: both photos one side, or some wings not turned over | the butterfly not turned over (or only some wings) before its second photo | `leave`: to photograph again |
| `wing-side`: a single photo against its name | its other photo carries another ID, or the view is missing | as `notTwoPhotos` |
| `wing-count` | a wing moved or fell between the two photos; or one the check missed (a glass wing on the dark half of the background) | look at both whole photos; a wing really missing from one is for the person |
| `wing-turn` | the photo turned otherwise than its wings ask | look at the whole photo; `rotate` it when its forewings are not on top |

- Set an ID yourself when the envelope reads clearly, the ID fits the run
  around it and the workbook has it. Otherwise `leave` it: a wrong name puts
  a photo on another butterfly's row.
- Which way up: a photo is landscape, the wings side by side with the
  forewings above the hindwings (the ruler and colour card are usually at
  the bottom, but not always). The envelope's direction does not count: it
  is often laid down turned. The camera does not move between shots, so one
  turn fits the whole batch: look at two or three whole photos
  (`part: "photo"`, as they will be once turned), and if they show standing
  or upside down, `rotate` every photo of the batch the same way. A photo
  turned otherwise than the rest is an envelope laid down turned, not a
  photo to turn.
- `view` follows the wings when they are sure (`viewFrom: "wings"`: a
  butterfly photographed underside first, or a CRISPR one with more views,
  d2 and v2), and the order taken otherwise, dorsal first. A view you set
  stays, and the ID's other photos take the views left.
- At the end say what you changed (file, from which ID to which), what is
  left for them and why, and whether they can rename now (nothing waiting).
- Then say where the files go, from `upload`: the folder for the photos and
  the one for the RAW files, each with its link and its place in Drive for
  desktop (`inDriveForDesktop`). They are the folders that hold the photos
  of the IDs nearest these; `alsoNear` is a second folder some of them point
  to, worth asking about. With `noFolderYet` the batch is a new collection:
  propose its folder (the country and year of `butterflies`, with `JPG` and
  `RAW` inside) and make it with `organize_photos` once they agree.
- `projectDrive` is the space left in the account they upload with. When the
  folder does not fit (`fits`), or the Drive is `nearlyFull`, say so before
  they start uploading and offer to free space (below, «Moving the photos to
  another account»).
- The rest of the way: once the upload is done, «Uploaded: update» in the
  tab reads those folders for Photo_links, and the same part of the tab
  updates the sites; or `sync_photo_links`, then `update_websites`, here.

## Photos uploaded to Drive without being renamed

Photos on Drive with the camera's names (IMG_…), found by a Photo_links
reading (`unnamed`) or named by the person, are read on the server:
`drive_photos` with the folder (its link, or its name or path). Each photo is
downloaded, its envelope read with the same reader as the Rename Photos app,
a small copy kept and the photo deleted; about a second a photo. A card
beside the chat shows the batch as it is read.

1. `drive_photos` with the `batch` once read: the IDs in the order taken
   (`order`), the checks, and `toLook`, only the photos that do not add up.
   The others are not to be looked at: two photos per ID, in the order of
   the IDs around them, read sure, turned as their session.
2. `drive_photos_look` at those, the envelope first (`what: "photo"` for the
   side, the wings, the turn; `box` for a part at full resolution: a faint
   envelope, a wing), and `drive_photos_edit` what is clear (one turn for
   all the photos, after a look at two or three whole ones: `turnAll`), as in
   «Reviewing a batch» above (the same digits mistaken, the same order of
   IDs). A photo not wanted (blurred, a repeat): `remove`; the reason in
   `note`.
3. Tell the person what you changed and what is left for them; they check
   the card (each tile has the ID, the view, a turn and «quitar») and press
   «Aplicar en Drive»: a plan of the files' changes (renames with the RAW
   files, turns, removals: the project account bins its own files; another
   account's are renamed …_todelete and the transfer sheet left ready).
   Then a Photo_links reading lists them.
4. IDs whose photos have to be taken again (a view missing, two photos of
   one side, an envelope that cannot be read): say them, and show them with
   `show_photos` (`notes` says why for each), so the person sees which
   butterflies to take out.

`show_photos` also serves any list of butterflies to look at: photos the
person asks about, two IDs to compare, the odd cases of a check.

## Updating the sites

The Wings Gallery and the maps keep their own copy of the data, made when
their «Update Database» run on GitHub reads the sheets: `update_websites`
starts it and says how it went. The gallery takes about a minute; the maps
5 to 10. A photo reaches the gallery once Photo_links lists it and the
workbook has recalculated, so the sites are updated after `sync_photo_links`,
not before.

## Updating Photo_links

`sync_photo_links` reads the Drive folders and compares them with the sheet:
about half a minute for every folder, a few seconds with `only` when the
person names the folder they uploaded to. Nothing is written until the person
agrees, in the chat or with «Apply» on the card that opens beside the chat
(«Proposed changes»): the card lists every row, a folder can be unticked
there to leave it for later, and a bar shows the writing as it goes (about
7 seconds per 400 rows; other people's saves wait their turn between two of
them, so say so before a large one).

Its answer is also a check of the new photos. Before asking whether to
write, say what it found, what it most likely is and what you suggest:

| Found | Most likely | What to do |
|---|---|---|
| `rename` | another way of writing the name (`CAM077986_d.jpg`) | rename to the name in `to`: `organize_photos` does a whole folder in one go (below) |
| `turned` (beside the checks) | a photo whose orientation tag differs from the rest of its folder that day: turned by mistake, or re-saved already turned (then it shows right) | look at them together (`photo_files_look`, 800); `organize_photos` `rotate` with its `degrees` only those that show turned |
| `unnamed` | the photo was not renamed (`IMG_5069.JPG`), or shows no envelope | look at it; rename it or move it to a `_temp` folder |
| `unknownId` | the butterfly's row is not typed yet (IDs running on after the sheet's last ones), or the ID was misread (it is `outOfOrder` too; `maybe` is the workbook ID one character away that fits) | ask which; a misread one is renamed |
| `outOfOrder` | a misread ID: `around` are the IDs photographed just before and after it | look at the photo's envelope |
| `otherFolder` | the same ID has photos in another folder (`other`): the same photo filed twice (`copy`), or another photo, so one of the two is misnamed or the CAM was used on two butterflies | a copy: remove the one in the wrong folder (`organize_photos`; the right folder is the one of the butterfly's country and year in its row); another photo: compare the two envelopes with the row |
| `unpaired` | the other view was not photographed, or carries another name (often the `outOfOrder` or `unknownId` photo beside it) | find it among those, or ask for the photo |
| `beforeCollection` | the ID belongs to another butterfly: this one was not collected yet | look at the envelope; check the row's date |
| `otherCountry` | a photo uploaded to the wrong folder, or one CAM used for two butterflies (skill `data-rules`, duplicates) | compare the envelope with both rows |
| `sameName.different` | two photos share a name: one is misnamed, or the CAM was used twice | compare the two photos |
| `notListed` | a new folder nobody asked to list | ask whether to list it (`add`) |

When a name is in doubt, look at the photo with `photo_files_look`: by its
name, its `url` or its ID, Drive's resized copy in about a second (1600 px
for an envelope's writing, `box` for a part). The photos around it (the IDs
taken just before and after) often settle it.

Photos with something to look at can be listed as they are and corrected
later: the row follows its file.

The sheet's own menu «📷 Photo Database Tools» (Append New Files Only, Full
Sync) writes the same rows from Google Sheets; either can be used.

## Putting the folders in order

`organize_photos` changes the files on Drive with the project's Google
account: it shows the plan as a card beside the chat («Proposed changes»),
every change listed, and the changes are made when the person presses
«Apply» on that card. Photo_links then follows the files changed by itself,
and the tool takes a plan's changes back when asked (`undo`).

| The person asks | The plan |
|---|---|
| to give a folder's photos the names the sheets look up | `fix: [{check: "rename", in: "PANAMA_2025"}]` |
| to delete from a folder the photos that are in another one too | `fix: [{check: "copies", in: <the wrong folder>, keptIn: <the right one>}]`: only the files that are the same file there (without `keptIn`: in any other listed folder; the answer counts them by the folder that keeps them) |
| to rename, move or remove these files | `operations`, one per file, named by its path, its name or its link |
| photos on Drive that show turned | `{action: "rotate", file, degrees}` (clockwise): the same file and link with its orientation tag changed, its RAW file too; Photo_links needs nothing |
| a new folder, and files moved into it | `{action: "folder", name, in}`, then `move` operations naming it |
| the photos of a list of butterflies gathered to share (a project's specimens) | `{action: "shortcut", ids: [CAM…], to: <folder>}`: shortcuts to their dorsal and ventral photos; the photos stay where they are |
| something only another account can do (delete its own files, take a folder's photos over) | `{action: "transfer", step: "delete" or "copy", in: <folder>}`: the team's sheet is left ready, and that account presses its button (below) |

- A photo renamed or removed takes its RAW file with it. A photo moved does
  not: its RAW file is moved with a second operation, to the RAW folder
  beside the new one.
- Removing: the project's account bins the files it owns (they stay 30 days
  in Drive's bin). Drive lets nobody bin another account's file, and most
  photos belong to the account they were copied to (below): those are
  renamed `…_todelete`, which takes them out of Photo_links, and their owner
  deletes them: the plan leaves the sheet «Transfer Ownership & Free Up
  Space» ready for their folder, and its answer (`otherAccountStep.by`) names
  the accounts that then open the sheet and press «Delete copied files». Say
  which of the two will happen (the plan counts them) and who is left to
  press the button.
- Two different photos named after one CAM are not copies: no fix removes
  them. Look at both envelopes, then rename or remove the wrong one by name.
- After changes that bring new files into the listed folders (a folder moved
  in), `sync_photo_links` lists them.

## Moving the photos to another account (space on Drive)

The project account's Drive fills up with the photos, the RAW files above
all. They are then handed to another account with more room (the curator's
institutional one) with the sheet «Transfer Ownership & Free Up Space», in
Drive › Photos. Drive cannot give a file to an account of another
organisation, so the file is copied and the original deleted:

1. The person whose account takes the photos over opens the sheet signed in
   to that account, writes the folder's ID (the last part of its link) in
   «Source Folder ID» and presses «Copy files». Every file of the folder that
   belongs to someone else is copied (the copy is theirs, with the original's
   name) and the original renamed `…_todelete`. «Process subfolders» set to 1
   takes the folders inside it too.
2. The first owner opens the sheet signed in to their account and presses
   «Delete copied files»: the `…_todelete` files they own go to the bin
   (mode 0; the space is freed when the bin is emptied) or are deleted for
   good (mode 1).

- The account that presses «Copy files» is the one that ends up owning the
  photos: pressed from the project's account it copies them the other way.
  «Delete copied files» only touches files named `…_todelete`.
- The sheet's cells are written from here, so the other person only opens it
  and presses the button: `organize_photos` with
  `{action: "transfer", step: "copy", in: <folder>}` before step 1, or
  `step: "delete"` before step 2 (`permanent: true` for mode 1, when the
  space is needed right away). The sheet holds one folder at a time; while a
  run of it is going on the plan says so and leaves the step out.
- The copies are new files, so the old links end with the originals.
  `sync_photo_links` moves each row to the copy of the same name; it can be
  run right after step 1, and the links never break.
- Step 2 for the project's own files can be done here:
  `organize_photos` `fix: [{check: "copied", in: <folder>}]` bins each
  `…_todelete` file whose copy is beside it (same name, same size), and leaves
  the sheet ready for the accounts that own the others. The bin still counts
  against the space until it is emptied in Drive; `permanent: true` deletes
  them for good instead, which frees the space at once and cannot be taken
  back.
- Which account owns each file is Photo_links' «Owner Email» column
  (`query`): the files still owned by the project's account are the ones
  that take its space.

When the space runs short (`drive_space`: `nearlyFull`, or a folder about to
be uploaded does not fit), offer this, a step at a time:

1. Say how much is left and which folders would free most (`ownByFolder`,
   as Drive itself says: it counts folders Photo_links does not list too),
   and who took the photos over the last time (`takenOverBefore`). Ask which
   folder to hand over. Originals already copied (`originalsAlreadyCopied`)
   are freed first: step 4 alone.
2. `organize_photos` with `{action: "transfer", step: "copy", in: <folder>}`:
   once they press «Apply», the sheet has the folder. Give them the sheet's
   link to send to the person of that account, who opens it signed in to it
   and presses «Copy files» (a few seconds a file: a folder of some hundred
   photos takes half an hour or more).
3. When they say the copy is done: `sync_photo_links`, so the rows point to
   the copies. A folder Photo_links does not list yet is listed with it
   (`add`): its photos then show in the sheets, and step 4 can find it.
4. Then `organize_photos` with
   `fix: [{check: "copied", in: <folder>, permanent: true}]`: the originals
   the project's account owns, each with its copy beside it, are deleted for
   good and the space is free. `drive_space` afterwards shows it.

## Which photos are missing

A preserved butterfly's photos are missing while its Photo_dorsal or
Photo_ventral has no link. `review_issues` kind `photo_missing` lists them
(those preserved in the last 30 days are left out: they are photographed
later), and says when Photo_links has the photo but the row does not find it
(another file name).

## Corrections

- A file named after the wrong CAM: rename it in Drive, the JPG and the RAW,
  then `sync_photo_links`.
- A CAM changed in the workbook: its files are renamed to the new CAM the
  same way.
- Photos taken again: the new files take the names, the old ones go to a
  `_temp` folder or the bin, then `sync_photo_links`.

## Curating the archive

`photo_facts` puts side by side what the archive's photos say about a
butterfly (its envelope as read by OCR, the Wings Gallery's prediction, the
side the wings show and whether the photo is turned) and what the workbook
says, with the species, sex and CAM_ID changes its notes record. It helps
when a CAM looks wrong (the envelope shows another ID), a species or sex
looks off, or the person wants to go through the archive's open cases:
without CAMs it lists the disagreements, the surest first. The envelope's
fields are readings with their confidence: the photo itself settles a case
(`show_photos`, or the link of the file). An envelope whose top strip only
was photographed has its CAM but no species: nothing to compare there.
