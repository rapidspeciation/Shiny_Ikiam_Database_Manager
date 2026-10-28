---
name: rack-reader
description: A 96-tube rack photographed from below (tube bottoms with 2D codes) turned into its tube IDs on A1..H12, the LBI freezer deposit sheet (Anexo1 .xlsx, one sheet per rack) with a download link, and tube IDs looked up in the workbook. Use it when someone sends photos of a rack's bottom, asks which tubes were left in the -80 °C freezer, or needs the deposit Excel.
---

# Racks: photos → tubes → deposit sheet

Three tools; none changes the workbook or Drive.

1. `rack_read_photos` with the photos (attachment names, 1 to 6 of the same rack). It merges them by tube ID.
2. If `orientation.a1` is `unknown`: ask the question in `orientation.ask` (which tube is in A1, seen from the top where A1 is printed, or whether the rack code is beside A12 or H12) and call again with `a1_tube` or `rack_code_beside`. Never guess A1: the Excel goes to the LBI with positions.
3. `missing` wells: a tube not read in any photo. Ask for one more photo from another angle (send all photos again), or the ID from the tube's side label for `fill`, or whether the well is empty (`empty`).
4. `rack_report` with each rack's `reading`, `responsable`, `estanteria`, `fila_de_rack`, `caja` (ask for what the person has not said; the freezer defaults to LBI062). Put its `markdown` link in the reply as it is: it downloads the .xlsx for the person signed in to the app.
5. `tubes_lookup` with the `reading`: say how many tubes the workbook has and list the ones it does not (they still need their rows).

Say how many tubes were read from the photos, and that A1 was proven by the tube IDs or confirmed by the person.

## Photo protocol (for the team, Spanish)

Voltea el rack y apóyalo con los fondos de los tubos hacia arriba. Toma unas 3 fotos desde unos 40 a 50 cm, con el zoom (2x) para que el rack llene casi toda la foto y todos los tubos salgan enfocados (de cerca, el teléfono solo enfoca el centro). Que se vea el código del rack (el cuadrado impreso en una esquina). Cambia un poco el ángulo entre fotos (inclinando unos 10 a 15°, no más) para que el brillo de la luz caiga en tubos distintos; evita la luz directa del techo o del flash sobre el rack. Envía las fotos originales (no capturas de pantalla). Si el asistente dice que falta algún tubo, toma otra foto desde otro ángulo o dicta el número de la etiqueta lateral del tubo.
