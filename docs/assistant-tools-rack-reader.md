# Assistant tools: tube racks from photos (28 Oct 2026)

For unloading samples at the LBI (Laboratorio de Biología Integrativa): the team fills a 96-tube rack (A1, B1 … H1, A2 …), puts it at -80 °C and must send the LBI its deposit sheet (Alex's Anexo1: one sheet per rack, «Caja <rack code> (8x12)», the tube ID on each well). Instead of photographing each tube's side barcode, they photograph the rack's bottom a few times and the assistant reads it.

| Tool | Does |
|---|---|
| `rack_read_photos` | photos (chat attachments) → rack code, tubes on A1..H12, missing wells, conflicts; saves a `reading` |
| `rack_report` | readings → the Anexo1 .xlsx; a download link (`/api/rack-reports/<token>/<file>`, its maker or an admin, signed in) |
| `tubes_lookup` | tube IDs or a reading → the workbook rows (CAM, species, sex, tissue, medium), the tube-locations sheet's rack and well, IDs not found |

## Reading

`server/rack-reader.mjs`. Pixels from ffmpeg (grey, longest side ≤ 4096); codes from zxing-cpp's WebAssembly build (zxing-wasm 3.1.6 reader, vendored in `server/vendor/zxing-wasm`: no npm package on the server). A whole photo decodes 1 code of 96: each tube's bright code square is found (blurred photo less its local mean, Otsu, connected components of a tube's size), cut out, enlarged to 200-600 px and decoded alone, then the grid is fitted (neighbour steps, a homography from lattice to photo) and the nodes not read are tried again harder where the grid puts them. The rack's code (FK…, FH…, FE…, SA…: the workbook's rack prefixes) sits one pitch beyond a corner tube. Photos are merged by tube ID (each photo's grid turned and shifted onto the first one's), so a tube needs to be read in one photo only.

Franz's photo of rack FK00274230 (2000 × 1125, 28 Oct): 94 of 96 tubes and the rack code in about 6 s on a PC (the lead chat's Python prototype: 92). A1 (FS50849233) is under glare and one tube's code is unreadable: both reported as missing.

## A1 (positions)

A photo of the bottom is a mirror of the rack seen from above, so with the grid found there are two ways to name the wells: A1 is at one of two corners. Evidence decides, or the person:

1. **The tubes' IDs.** In the workbook's tube locations (Sanger's scans), the FluidX racks of pre-loaded sequential tubes go +1 from row A to B and +8 from a column to the next (e.g. FK00403695: 83 of 84 neighbour pairs; FK00405946: 74 of 75). When a rack's neighbours fit one way (≥ 80 % of ≥ 10 pairs) and not the other, A1 is proven. Franz's rack: 166 of 166 pairs one way, 0 the other.
2. **The person.** `a1_tube` (the tube in A1, printed on the rack's top) or `rack_code_beside` ('A12' or 'H12').

The rack code's corner alone is not taken as proof: on Franz's rack the IDs put it beside A12, but FluidX lists the rack ID at A12 for some racks and at H12 for others. Once the team confirms it for the racks they use, `rack_code_beside` makes it a single word. Without proof, the tool returns photo positions (R1C1: the tube beside the rack code) and the question to ask; `rack_report` refuses such a reading. A deposit rack filled by hand from mixed runs (Alex's FK00404722) has no sequence, so expect the question there.

## Report

`server/rack-tools.mjs` writes the .xlsx itself (a small zip writer, inline strings): the Anexo1 layout of the filled examples, A1 «Responsable: …», G1:R1 the title, row 2 headers and 1..12, rows 3..10 A..H, A3:E10 merged (freezer, shelf, rack code, rack row, box), thin borders, the template's column widths. Checked with openpyxl and LibreOffice. A well neither read, filled by hand (`fill`) nor said empty (`empty`) refuses the report. Files: `<rackReportsDir>` (default `rack-reports/` beside the database), one folder per reading or report with its owner.

Not yet: video input, a copy on Drive (the app's Google login, franz.chandi@gmail.com, has the `drive.file` scope: it could create new files; `google.jmithominii.json` no longer refreshes), 10x10 and other box formats.
