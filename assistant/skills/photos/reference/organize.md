# Putting the folders in order

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
| something only another account can do (delete its own files, take a folder's photos over) | `{action: "transfer", step: "delete" or "copy", in: <folder>}`: the team's sheet is left ready, and that account presses its button ([drive-space.md](drive-space.md)) |

- A photo renamed or removed takes its RAW file with it. A photo moved does
  not: its RAW file is moved with a second operation, to the RAW folder
  beside the new one.
- Removing: the project's account bins the files it owns (they stay 30 days
  in Drive's bin). Drive lets nobody bin another account's file, and most
  photos belong to the account they were copied to ([drive-space.md](drive-space.md)): those are
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
