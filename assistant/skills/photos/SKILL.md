---
name: photos
description: The photos of the butterflies' wings, from the camera to the workbook — naming the files after the CAM_ID (the Rename Photos app), the folders on Google Drive, and the Photo_links sheet that Photo_dorsal and Photo_ventral read (sync_photo_links). Use it when the person uploaded, renamed, moved or deleted photos, asks to update Photo_links or the photo links, says a photo shows Not Found or the wrong butterfly, asks which photos are missing, how to rename photos, where to put them, or about the Wings Gallery.
---

# Wing photos and Photo_links

## From the camera to the sheet

1. **Photograph** each preserved butterfly's wings twice, dorsal and ventral,
   with its envelope in the picture (the CAM_ID is read from it), the ruler
   and the colour card. The camera saves a JPG and a RAW file of each.
2. **Rename** the files after the CAM_ID, on the computer, with the Rename
   Photos app (below).
3. **Upload** them to the project's Drive folder «Photos», into the folder of
   their collection (below).
4. **List** them in the Photo_links sheet: `sync_photo_links`.
5. The sheets then show them: Photo_dorsal and Photo_ventral of
   Collection_data, Insectary_data and CRISPR are formulas that look the
   file's name up in Photo_links and link to it. The Wings Gallery
   (https://rapidspeciation.github.io/Shiny_Ikiam_Wings_Gallery/) reads the
   same sheets when someone presses its «Update DB».

Step 4 is the one people forget: photos that are on Drive but not in
Photo_links show `NOT FOUND` (`Not Found` in Collection_data) in their row.

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

https://fr4nzz.github.io/rename_photos_AI/ runs in the person's browser, on
the photos in a folder of their computer, before they are uploaded. Nothing
is sent anywhere and it needs no key. Chrome or Edge (Brave after switching
on the setting the app shows); other browsers can read the folder but not
rename in it.

1. «Open folder», then «Run PaddleOCR»: it reads the CAM_ID on each envelope
   (about a second per photo) and turns each photo and its RAW file so the
   envelope reads upright.
2. «Review», filter «Needs review»: confirm or correct each photo it was not
   sure of (Enter, the check mark, or one of the suggestions). The strip at
   the top shows the CAMs in the order the photos were taken, where gaps and
   repeats stand out.
3. «Rename Files». RAW files are renamed with their JPG. A log in the
   folder's `rename_files/` lets «Restore» undo it.

What sends a photo to review, which tells what to check:

- the CAM it read is not in the workbook (the app reads the CAM_ID columns
  of Collection_data, Insectary_data and CRISPR once a day), so the
  butterflies' rows are typed before their photos are renamed;
- the CAM is far from those of the photos taken just before and after;
- the same CAM on photos that were not taken one after the other, or on more
  than two.

It reads IDs written as `CAM` and six digits. Other IDs (the Colombian
`M6285`) are typed in the review, or read in its Gemini mode (the sparkle
button; it needs a Gemini API key, and its prompt names the ID's form).
Its repository, with how the reader works and how accurate it is:
https://github.com/Fr4nzz/rename_photos_AI.

## Updating Photo_links

`sync_photo_links` reads the Drive folders and says what the sheet lacks;
nothing is written until the person agrees. From its answer, tell the person:

- how many photos are new, by folder;
- the new photos the sheets will not find, with the name each needs
  (`rename`), and those still carrying the camera's name (`unnamed`);
- photos named after an ID the workbook lacks (`unknownId`): the row is not
  typed yet, or the ID was misread (look at the envelope in the photo);
- files with the name of a photo already listed that are another photo
  (`sameName.different`): one of the two is misnamed, or the CAM was used on
  two butterflies (skill `data-rules`, duplicates);
- folders with specimen photos that are not listed (`notListed`): ask whether
  to list them.

Files with a name to correct can be listed as they are and renamed later:
the row follows its file at the next `sync_photo_links`. Many files to
rename the same way (an underscore to remove in a whole folder) can be
renamed on Drive with the project's Google account (skill `google-account`,
`gog drive rename <fileId> <newName>`): show the person the list of old and
new names first.

Thousands of new rows take several minutes to write, some hundred per save,
and the workbook recalculates its photo links after each one, so other
people's saves wait their turn meanwhile: say so before a large one.

The sheet's own menu «📷 Photo Database Tools» (Append New Files Only, Full
Sync) writes the same rows from Google Sheets; either can be used.

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
