# Identificar: the offline butterfly identifier as an app tab (design, 3 Nov 2026)

Converged after the sol-loop brainstorm round (GPT-6.1 Sol and Opus 5.5, both B / CHANGES on the first draft; their
findings and what was taken are in `LEDGER.md`). The draft and the critics' brief: `BRAINSTORM_BRIEF.md`.

## Who and what for

An average person with a phone in the forest, often without signal, wants to know what butterfly they photographed and
to check it against real photos. Team members (signed in) also want to know whether the project still needs
flash-frozen specimens of that species for reference genomes (target: 3 ♂ + 3 ♀ per subspecies, or per species
without subspecies, on each side of the Andes).

## Principles

- **The comparison is the result.** The AI's ranking only says where to start looking; the reference photos are how a
  person decides. The screen leads with the suggested name and puts "your photo | reference photo" right under it.
- **No accuracy words the data cannot back.** The head's scores are not calibrated on citizen photos. The headline
  says "Posible identificación", the score is small grey text, once explained as "una puntuación de la IA, no una
  garantía". When the species score is low but the genus is clear, the headline names the genus.
- **One status line.** Whether the identifier works without internet is said once, at the top. Other places only say
  what is missing locally ("No hay fotos guardadas de esta especie en el teléfono").
- **Plain words.** No file names, MB-on-the-wire, encoder, head, WebGPU or threads outside "Modelo y almacenamiento".
  Sizes are what a person pays: the download (mobile data) and the space on the phone.
- **Nothing pretends to be sent.** The person's choice ("Se parece a esta") is kept only on this phone and says so.

## Layout (phone first; desktop puts the comparison and the list side by side)

The tab sits in the app's green header like the others ("Identificar"), with the same language and user controls.

### A. First visit (no model on the phone)
```
[green app header ............................ Identificar]
Identificar una mariposa
Tome una foto y la app sugiere qué especie es. Funciona en este
teléfono, también sin internet, después de una descarga.

 (•) Rápido y liviano                        ← default
     Descarga unos 55 MB. Un poco menos preciso.
 ( ) Más preciso
     Descarga unos 440 MB, mejor con wifi. Ocupa unos 650 MB.

 [ Descargar ]          (progress: "23 de 55 MB", resumes after a cut)
 iPhone, not installed: "En iPhone, primero agregue la app a la pantalla de inicio
 (Compartir → Agregar a inicio) y descargue desde el ícono: lo descargado en Safari
 no se ve desde el ícono."
 Not enough space: "Hacen falta unos 650 MB libres; este teléfono tiene 400 MB."
```

### B. Ready
```
✓ Listo para usar sin internet · Rápido y liviano          Cambiar
[  📷 Tomar foto  ]  [  Elegir foto  ]
Alas abiertas, de cerca, que la mariposa llene la foto.
Lado de los Andes (opcional)  [No sé] [Este · Amazonía] [Oeste · Costa]
Ayuda a ordenar los resultados.
```

### C. Result
```
✓ Listo para usar sin internet · Rápido y liviano          Cambiar
(amber, only when no wings were found)
  No encontramos las alas con claridad: el resultado puede estar mal.
  Pruebe otra foto, más cerca y con las alas abiertas.

Posible identificación                       (or "Género Mechanitis; la especie no es segura")
Mechanitis messenoides   [Ithomiini]          (or "No parece una Ithomiini")
78 % · puntuación de la IA, no una garantía
Lado de los Andes: No sé  ·  cambiar          (re-ranks at once, no new photo)

┌ Su foto ──────────┐┌ Mechanitis messenoides ┐
│ (crop the AI used)││ (reference 1/3)  ‹ ›   │
└───────────────────┘└ © autor · CC BY-NC ────┘
 [Se parece a esta]   [No se parece: ver la siguiente ›]
 Su elección se guarda solo en este teléfono.

Otras posibilidades
▾ Mechanitis                 97 %
   (t) messenoides   78 %  ← comparando
   (t) polymnia      12 %
   (t) lysimnia       6 %
▸ Melinaea                    2 %
  Otros géneros               1 %

[team only] Congelados para genomas de referencia   (follows the compared species)
  Faltan: Oeste 2 ♂ y 3 ♀ · Este completo ✓
  ▸ detalle por subespecie y lado (meta 3 ♂ + 3 ♀; sin lado / sin sexo aparte)
  Inventario al 3 nov 2026

[ Otra foto ]
▸ Mis identificaciones (en este teléfono)
▸ Modelo y almacenamiento   (model in use, change or delete, space used, seconds per
                             photo, fotos para el campo, instalar en el teléfono)
```

Desktop (≥ 1024 px): left column = headline + comparison; right column = list, team panel, log.

## Reference photos

- Online: the iNaturalist CC-licensed photos the app already looks up (`/api/species-photos`), the species first;
  a genus fallback is labelled "Foto de otra especie del género X", never shown as the species. Under cross-origin
  isolation iNaturalist images cannot be shown directly, so they come through a same-origin, signed-in proxy limited to
  iNaturalist photo URLs (`/api/identify/ref-photo`). Each photo shown is kept on the phone (Cache Storage).
- Offline: "Fotos para el campo" in Modelo y almacenamiento downloads one small photo per Ithomiini species of the
  head (the genera the workbook marks Tribe = Ithomiini; 350 species in head b8127dbf). The size is shown after the
  download, measured, never a guess. Measured in the lab on 3 Nov 2026: 73 photos = 2.0 MB (about 28 KB each), so
  all 350 come to about 10 MB. The server looks each species up on iNaturalist once (one request a second, a few per
  species): the first complete set takes about half an hour after the tab is first used; the tab says how many are
  still being gathered. Other candidates offline: "No hay fotos guardadas de
  esta especie en el teléfono."
- Team specimen photos stay signed-in only and are not part of the field set.

## Frozen inventory (team)

`/api/frozen-stock` adds per species: counts by side (East, West, other: Center/NA/empty) × sex (M, F, unknown) and
by subspecies (Collection_data Subspecies_Form or the SPECIES third word; Insectary_data the SPECIES third word);
Insectary_data rows take their side from Collection_data's usual side for the same Collection_location. The panel
shows the gap first, sides where the head records the species in Ecuador or where stock exists, subspecies rows
without picking one (the model does not identify subspecies), and unknown side or sex as their own counts. The old
fields stay, so /prueba-offline/ keeps working.

## Architecture

- The models need cross-origin isolation (multi-threaded WebAssembly); the SPA cannot have it (T3 iframes, Google,
  Drive images). The tab is its own page `/identificar/`, served isolated like `/prueba-offline/`
  (`server/offline-id.mjs`), a small Vue app that renders the same `AppHeader`; any other tab navigates into the SPA
  (`../#/<tab>`), and the SPA's `/identificar` route goes to the page.
- Models, downloaded parts and the runtime are shared with `/prueba-offline/` (same cache, same URLs): a phone that
  downloaded there does not download again.
- The DOM-free engine (download, storage, load, run) is `lib/offlineEngine.ts`; result wording is pure functions in
  `lib/identifyResult.ts` (tested). The old test page is left untouched until the tab replaces it.
- The last result is kept for the browser session, so going to another tab and back restores it.
- Gate: the tab carries `tester: true` (Franz's account) and `IDENTIFY_FOR_EVERYONE` in `router.ts` opens it to
  every signed-in person. Opening it without an account is a separate decision (`NO_ACCOUNT_OPTIONS.md`).

## Not in this version

GPS to fill the side of the Andes (needs a bundled outline of the divide), recording choices on the server, a photo
tips sheet with example pictures, calibration of the genus-or-species headline cut (0.8) against held-out tops.
