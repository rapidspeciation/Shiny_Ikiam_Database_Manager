# Moving the photos to another account (space on Drive)

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
