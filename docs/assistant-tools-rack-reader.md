# Assistant tools: tube racks from photos (28 Oct 2026)

For unloading samples at the LBI (Laboratorio de Biología Integrativa): the team fills a 96-tube rack (A1, B1 … H1, A2 …), puts it at -80 °C and must send the LBI its deposit sheet (Alex's Anexo1: one sheet per rack, «Caja <rack code> (8x12)», the tube ID on each well). Instead of photographing each tube's side barcode, they photograph the rack's bottom a few times and the assistant reads it.

| Tool | Does |
|---|---|
| `rack_read_photos` | photos (chat attachments) → rack code, tubes on A1..H12, missing wells, conflicts; saves a `reading` and puts the rack into this chat's deposit proposal at once |
| `rack_deposit` | the deposit's fields (responsable, freezer, per rack shelf, rack row, box), wells filled or said empty, the code's corner, a rack code not read; the grid and the rows follow |
| `rack_report` | the deposit → the Anexo1 .xlsx and its download link (`/api/rack-reports/<token>/<file>`, its maker or an admin, signed in); `drive: true` also a copy on Drive |
| `tubes_lookup` | tube IDs or a reading → the workbook rows (CAM, species, sex, tissue, medium), the tube-locations sheet's rack and well, IDs not found |

## The deposit in «Cambios propuestos» (29 Oct 2026)

`server/rack-deposit.mjs`, `server/assistant.mjs` (saveDeposit), the sidebar's `RackDepositHead.vue` and `RackSection.vue`. A rack read becomes, at once, a proposal of the same kind as a notebook page's (a row of `ai_proposals`, drafted with propose_changes' draft and applied with «Aplicar»): one per chat, holding every rack read there (`page_json.deposit`, with the photos beside it). Each rack is a section: its code, the corner the code is beside (assumed or confirmed, with a button to turn the rack), Anexo1's left block (freezer, shelf, rack code, rack row, box: empty ones amber), the 8 × 12 grid as seen from above (row A at the top, column 1 at the left) with each tube's ID, and its rows of the tube-locations sheet. A well is green (a new row), amber (its rack or well changes; the table shows the old value struck through), white (already there), hatched (said empty) or red (no tube yet: a click gives a box for the side label's ID, or «Vacío»). A grey dot: no sheet of the workbook has the tube (it needs its rows; never invented).

What it writes, only on «Aplicar», only in `Ithomiini_tube_locations_18Jun26`: new rows (Tube_ID, Rack_ID, Tube_location = the well, Sort going on from the sheet's largest, Batch only when the person gives one) for tubes the sheet does not have; Rack_ID and/or Tube_location of a tube it has on another rack or well. Tubes already where the photos have them are left alone; a tube on two rows of the sheet is not changed (listed). Rows the sheet puts on the rack whose tubes the photos do not show there are listed, never removed. The sheet has no freezer, shelf, rack row or box columns (checked in the production database on 29 Oct); the code fills such columns if the sheet ever has them. Any change to the deposit (a field, a well, the corner) works the rows out again: the person's own cell edits of those rows in the table do not survive it.

The production sheet is protected for the app's Google account (the whole sheet, `protected:Ithomiini_tube_locations_18Jun26`), so «Aplicar» is refused by Google until its owner lets the account edit it; the answer and the sidebar say so. The .xlsx works regardless.

Defaults: responsable = the person signed in (shown as assumed until changed), freezer LBI062; the rest empty and asked for. Edits from the sidebar: `POST /api/chat/proposals/<id>/rack` (as `rack_deposit`), files: `POST /api/chat/proposals/<id>/rack-files` (`{ drive: true }` for the Drive copy).

## Drive copy

On click only («Guardar en Google Drive», or `rack_report` with `drive: true`). The project account (`GOOGLE_DRIVE_CREDENTIALS_FILE`, jmithominii@gmail.com) uploads the .xlsx (`GoogleDrive.upload`, a multipart upload) into «Depósitos LBI (racks)»: `RACK_DRIVE_FOLDER` (its ID) when set, else that folder found or made in `RACK_DRIVE_PARENT`, else in the account's My Drive. On claudeclaw it is in the project's shared area, Ithomiini_IKIAM › Admin (beside «Formatos deposito temporal muestras ikiam»). Drive not answering: the answer says so and the download link still works.

## Reading

`server/rack-reader.mjs`. Pixels from ffmpeg (grey, longest side ≤ 4096); codes from zxing-cpp's WebAssembly build (zxing-wasm 3.1.6 reader, vendored in `server/vendor/zxing-wasm`: no npm package on the server). A whole photo decodes 1 code of 96: each tube's bright code square is found (blurred photo less its local mean, Otsu, connected components of a tube's size), cut out, enlarged to 200-600 px and decoded alone, then the grid is fitted (neighbour steps, a homography from lattice to photo) and the nodes not read are tried again harder where the grid puts them. The rack's code (FK…, FH…, FE…, SA…: the workbook's rack prefixes) sits one pitch beyond a corner tube. Photos are merged by tube ID (each photo's grid turned and shifted onto the first one's), so a tube needs to be read in one photo only.

Franz's photo of rack FK00274230 (2000 × 1125, 28 Oct): 94 of 96 tubes and the rack code in about 6 s on a PC (the lead chat's Python prototype: 92). A1 (FS50849233) is under glare and one tube's code is unreadable: both reported as missing.

## A1 (positions)

A photo of the bottom is a mirror of the rack seen from above, so with the grid found there are two ways to name the wells: A1 is at one of two corners. Since 29 Oct the rack's code decides by default: the team's racks have it beside A12 (FK00274230), so a code read beside a corner places the rack with that corner as A12, an assumption shown to the person (confirm, or turn the rack: the photo allows the code beside A12 or H1 on this rack, the corner's two mirror names). Tubes go into racks in any order, so the IDs' sequence below only checks it (`checked_by_ids`) and never decides alone; with no code read and nothing said, the person is asked (the IDs' hint, if any, is in the question). What the person says overrides the default:

1. **The tubes' IDs.** In the workbook's tube locations (Sanger's scans), the FluidX racks of pre-loaded sequential tubes go +1 from row A to B and +8 from a column to the next (e.g. FK00403695: 83 of 84 neighbour pairs; FK00405946: 74 of 75). When a rack's neighbours fit one way (≥ 80 % of ≥ 10 pairs) and not the other, A1 is proven. Franz's rack: 166 of 166 pairs one way, 0 the other.
2. **The person.** `a1_tube` (the tube in A1, printed on the rack's top), `corner_tube` {well: 'A12' | 'H1' | 'H12' | 'A1', id} (a tube at another corner) or `rack_code_beside` (the well the code sits beside: a photo allows only the corner's two mirror names, A12 or H1 on FK00274230; in the deposit, `rack_deposit`'s `code_beside` takes one of the rack's `code_options`).

A named tube the photos did not read decides nothing and blocks nothing: it is listed in `orientation.not_read` and in the notes, the IDs' proof still counts, and when nothing else decides, the question asks for the tube in A12 or H1. On Franz's FK00274230 the A1 tube sat under glare. Any named corner tube that was read places a partly filled rack's grid too: A1 and H12 can only be at the photo's top right or bottom left, A12 and H1 at its top left or bottom right, and the tube is taken when only one of the two keeps every tube inside the rack. A tube named at a corner where that well cannot be in a photo of the bottom is a problem, not a guess.

Until 29 Oct the rack code's corner alone was not taken as proof (FluidX lists the rack ID at A12 for some racks and at H12 for others). Franz confirmed the team's racks have it beside A12, and tubes go into deposit racks in any order (Alex's FK00404722 has no sequence), so the code's corner is now the default, shown as an assumption the person confirms (or turns). Without the code read and without the person's word, the tool returns photo positions (R1C1: the tube beside the rack code) and the question to ask; no deposit is made from such a reading.

## Report

`server/rack-tools.mjs` writes the .xlsx itself (a small zip writer, inline strings): the Anexo1 layout of the filled examples, A1 «Responsable: …», G1:R1 the title, row 2 headers and 1..12, rows 3..10 A..H, A3:E10 merged (freezer, shelf, rack code, rack row, box), thin borders, the template's column widths. Checked with openpyxl and LibreOffice. A well neither read, filled by hand (`fill`) nor said empty (`empty`) refuses the report. Files: `<rackReportsDir>` (default `rack-reports/` beside the database), one folder per reading or report with its owner.

Not yet: video input, 10x10 and other box formats.
