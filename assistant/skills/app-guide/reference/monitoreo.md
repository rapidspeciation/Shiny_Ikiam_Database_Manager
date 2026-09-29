# Monitoreo — `#/monitoreo?vista=<sub-view>`

Monthly monitoring of the fixed Ikiam transects T1–T4 (Collection_data rows
with Purpose `Monitoring`, Collection_location `Ikiam`). Signed-in people
only. Sub-views, chosen with `vista` (default `resumen`):

| `vista` | Label | Who |
|---|---|---|
| `importar` | «Importar recorrido» | anyone signed in; adding/saving needs editor |
| `resumen` | «Reporte» | anyone signed in |
| `mapa` | «Mapa» | anyone signed in |
| `recapturas` | «Recapturas» | anyone signed in |
| `dudas` | «Dudas de emparejamiento» | editor, reviewer, admin (hidden otherwise) |

Changing sub-view keeps the other parameters but drops `individuo`.

## Importar recorrido — `?vista=importar`

A Wikiloc walk (one waypoint per butterfly, the note says species, sex, mark,
height, weather) becomes new Collection_data rows, reviewed before saving; the
track and points are kept for the map.

1. Get the walk, one of:
   - Paste the trail link in «Pega el enlace de una ruta de Wikiloc» →
     «Traer». A computer at home reads Wikiloc (status «activo» / «sin señal
     (…)»); if it is off the link waits in the queue. «Trabajos (N)» shows the
     jobs.
   - «Buscar nuevos»: checks the «Perfiles seguidos (N)» for new walks whose
     title contains the profile's pattern (e.g. "monitor"), assigned to that
     profile's collector. A profile is added with its link and «Recolector…»
     → «Seguir».
   - «Elegir GPX de Wikiloc» (a downloaded GPX file), or share the GPX / link
     to the installed app from the phone's share menu.
2. Walks read from Wikiloc wait under «Desde Wikiloc, por revisar (N)»: click
   one to open it. «Revisar de nuevo» (in «Ya en el mapa (N)») reopens an
   imported walk (e.g. notes corrected in Wikiloc).
3. Check «Fecha» and «Recolector» (from the followed profile, the GPX author
   or the title; «¿XX? (el último usado)» only offers the last one). A walk
   read from the public page has no GPS times, so SamplingDay_data is not
   completed; from a GPX, the day's SamplingDay_data row gets its start/end
   time (or a new row), and a wrong Date there offers «Corregir su Date a …».
4. The review table lists each point (Foto, Punto, Especie, Sexo, Hora,
   Altura, Clima, Marca, Revisión) with a tick to include it. «Revisión»
   flags: recapture (same mark, species and sex), a mark used for another
   species, the 30-preserved rule (the first 30 of each Ithomiini species are
   preserved, then marked), missing parts. The transect section comes from the
   GPS position. Weather codes from the note: `NO` = CD, `NC` = CL, `parches`
   = S&C, `sol` = S; `llovizna` = DZ, else DY.
5. «Añadir N filas a Collection_data»: preserved points get the next CAM and
   tube (as in Colecta); marked ones Release_Collect `Mark_Released`. Points
   without species get ID_status `To_identify`. The rows appear in the grid
   below, with each point's photos on the right; edit them there («Duplicar
   fila» copies the selected row without time, mark, CAM or tube).
6. **These rows wait for the person**: the save bar says «N filas del
   recorrido esperan a que pulses Guardar»; press «Guardar ya».
7. Other buttons: «Solo añadir las fotos al GPX ya subido de este día»,
   «Solo guardar el recorrido (mapa)» (rows already in the sheet), and «Pasar
   al mapa N ya registrados en la hoja» (walks whose rows are already typed:
   stores them on the map, adds no rows; doubtful pairings go to Dudas).

The assistant does the same with `queue_wikiloc` → `get_walk` →
`propose_changes(newRows)`; after the proposal is applied the walk goes on the
map with «Pasar al mapa … ya registrados en la hoja».

## Reporte — `?vista=resumen`

A live report; filters live in the link and scope every number and chart.

- «Periodo» (Todo, Últimos 12 meses, a year, Personalizado), «Desde»/«Hasta»
  (months), «Recolector», «Transecto» (T1–T4), «Especie», «Solo Ithomiini»,
  «Por subespecie»; «CSV» downloads the filtered rows as in the sheet. On
  phones they fold behind «Filtros».
- Cards: Individuos, Días de monitoreo, Individuos por día, Especies,
  Preservados · marcados, Recapturas, «Próxima marca» (the next mark to hand
  out). Tables: «Especies» (with the «Regla de 30» status per species),
  «Individuos por transecto y mes», «Recapturas» (mark, species, each capture
  with date · collector · transect · time). «Revisión de datos» lists rows with
  empty Purpose on monitoring days, marks recorded on two species, and
  recaptures written only in notes.

Parameters: `desde=YYYY-MM`, `hasta=YYYY-MM`, `rec=<collector initials>`
(e.g. `FCH`), `t=1..4`, `sp=<Genus species as in the sheet>`, `ith=1`,
`sub=1`. Example:
`https://ithomiini-ikiam.duckdns.org/#/monitoreo?vista=resumen&desde=2026-01&hasta=2026-06&t=2&ith=1`

## Mapa — `?vista=mapa`

Capture points of the stored walks on a satellite map, atlas-style filters
(nothing chosen = everything), all in the link («Compartir» copies it).

- Filters: «Fechas de monitoreo», «Especies», and chips «Año», «Recolector»,
  «Transecto», «Sexo», «Tipo»; «Quitar filtros (N)».
- «Mostrar como» Puntos / Grupos / Calor («Radio del calor»); «Colorear por»
  Especie / Sexo / Tipo (preservado, marcado, recaptura) / Recorrido;
  «Transectos T1–T4», «Trazados GPS de Wikiloc», «Unir recapturas».
- With dates chosen, «Recorridos de ese día» lists the walks (open in
  Wikiloc; «Quitar recorrido del mapa» for its uploader or a reviewer/admin —
  the sheet rows do not change).
- A chosen butterfly («Individuo B39 · …») links to «ver fotos» (Recapturas).

Parameters (lists comma-separated): `fechas=YYYY-MM-DD,…`, `anios=2025,2026`,
`recolectores=FCH,AA` (initials), `especies=Genus species,…`,
`transectos=1,2` (or `none`), `sexos=female,male,unknown`,
`tipos=preserved,marked,recapture`, `individuo=<MARK>|<Genus species>`,
`capa=grupos|calor` (default puntos), `color=sexo|tipo|recorrido` (default
especie), `trazado=0` (hide transects), `gps=1`, `unir=1`. Example:
`https://ithomiini-ikiam.duckdns.org/#/monitoreo?vista=mapa&individuo=B39|Mechanitis%20messenoides&unir=1`

## Recapturas — `?vista=recapturas`

Every marked butterfly caught more than once, with the photos of the first
capture and of each recapture side by side, to check mark and species by eye.
Recaptures that are not sheet rows (only in notes, or only in Wikiloc) are
shown and marked as such.

- «Marca o especie» search (e.g. `B39`), «Especies», «Ordenar» (Última captura
  más reciente, Más capturas, Más tiempo entre la primera y la última), «Solo
  con fotos». Each capture shows its row, distance between GPS points, note.
- «En el mapa» opens the map with that individual and «Unir recapturas»;
  «Todos los individuos» goes back. Photos enlarge; Esc closes, arrows move.

Parameter: `individuo=<MARK>|<Genus species>` opens that butterfly.

## Dudas de emparejamiento — `?vista=dudas`

Wikiloc points whose sheet row is not certain, each beside its photos and
note, with the rows it could be (editors).

- «Colector» filter; «Actualizar» re-matches and recomputes.
- «La nota no coincide con su fila»: already paired, but note and sheet
  disagree → «Sí es la fila N» (keeps the pairing; fix the sheet in Tablas if
  it is wrong).
- «¿Qué fila es?»: pick one of «Filas de ese día que puede ser» ((propuesta)
  marks the app's guess) or «No es ninguna». «guardado sin fila» = stored on
  the map without a row until chosen.
- Walks still «por revisar» are paired here point by point, then «Pasar al
  mapa».
- Re-matching all walks with the current method: «Aplicar N cambios»
  (reviewer/admin only).

These are the `walk_doubt` issues of `check_data`: the assistant can say which
rows fit and draft the question for the collector, but a person pairs them
here. Link: `https://ithomiini-ikiam.duckdns.org/#/monitoreo?vista=dudas`.
