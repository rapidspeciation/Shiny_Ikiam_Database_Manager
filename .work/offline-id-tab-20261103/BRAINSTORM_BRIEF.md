# Brainstorm brief: offline butterfly identifier as a tab of the Ithomiini app (round 1, design only)

Review only. Do not edit, commit, push, deploy, comment on PRs, or start subagents.

Repo (read-only for you): /home/franz/Documents/CodeProjs/ithomiini-offline-id-tab-20261103 (branch offline-id-tab-20261103, = wa-lab b5517a7).
Work under review: this brief's DRAFT DESIGN (below) for a new app tab. No code yet.

## Who it is for
An average Ecuadorian citizen or field volunteer with a phone, Spanish UI, often no signal in the forest. Also the
project's team members (signed in), who additionally need the flash-frozen specimen counts.

## Franz's requirements (owner, 3 Nov 2026, condensed)
- Move the local (in-browser, offline) butterfly identifier from the test page /prueba-offline/ (only Franz sees it) to
  a new tab of the main app, ready for deployment, gated to the tester flag for now (one switch to open it to all).
- UI for an average citizen: dedupe ("sin conexión" appears several times), no jargon / AI slop: file names and MB
  lines like "wing_seg_v5n.onnx (11 MB) · bioclip25h_visual_int8.onnx (608 MB) · head_b8127dbf.json … 454 MB por la
  red (comprimido)" mean nothing to non-devs. Drop the debug step "3. Enviar el resultado".
- Simple model choice framed by benefit ("Rápido y liviano" vs "Más preciso"), sizes in plain words.
- Photo → result: top predictions with honest confidence; a hierarchy/alternatives view; reference photos of the
  predicted species (and alternatives) to compare side by side and confirm or reject.
- Signed-in team: flash-frozen counts for the predicted taxon by sex, subspecies and side of the Andes against a
  target of 3 males + 3 females per species (or per subspecies) per side of the Andes (for reference genomes of all
  Ithomiini); show what is missing.
- Think about which reference photos can work offline (a small cached thumbnail set, sized honestly) vs online.
- Never overclaim accuracy (the small model is ~3.5 points less accurate on collection specimens; top-1 drops more).

## References (screenshots, read them as images)
Folder: /home/franz/Documents/CodeProjs/ithomiini-offline-id-tab-20261103/.work/offline-id-tab-20261103/shots-before/
- before-prueba-offline-phone.png: the current test page (what Franz complains about).
- app-home-phone.png / app-home-desktop.png: the main app (green top bar with tabs, Fira Sans, stone greys; shown
  signed-out in English by the screenshot browser; signed in it has many tabs: Buscador, Colecta, Monitoreo, …).
- ref-shiny-ai-identifier-*.png: Franz's older Shiny "AI Identifier" (upload card, toggles "Zoom to wings / Wing
  outlines / Show predictions"; results show the photo with the wing crop and a ranked list).
- ref-tbs-gallery-*.png: Franz's insect-AI gallery. Its Review view (not in the screenshot, described by Franz): a
  hierarchical prediction tree order→species with %, thumbnails per candidate, a large reference-photo gallery of
  the predicted species with compare side by side, and confirm/reject buttons.
- App design rules: DESIGN.md and PRODUCT.md at the repo root.

## Facts and constraints found in the code (check them if you doubt)
- Pipeline in the browser (frontend/src/lib/offlineId.ts, frontend/src/offline/main.ts): wing detector → crop →
  encoder → head → species probabilities (speciesTop). The head gives ~12k species (butterflies and moths, not only
  Ithomiini), aggregated from 15k leaves whose names are not shipped: so the hierarchy available offline is
  genus → species (genus = sum of its species), not subspecies. An optional "side of the Andes" (East/West) prior
  exists (geo bits per leaf; it also says which side a species is recorded on in Ecuador).
- Models: full encoder ≈ 650 MB download (≈ 440 MB on the wire compressed); small ≈ 60 MB with detector and head
  (≈ 54 MB on the wire). Cached in Cache Storage in 32 MB parts; works in airplane mode after download.
- The page MUST be cross-origin isolated (COOP/COEP require-corp) for multi-thread WebAssembly; the main SPA is not
  (it embeds T3 iframes, Google sign-in, Drive images; Safari lacks COEP credentialless). Plan: the tab is a second
  page /identificar/ (served isolated like /prueba-offline/, server/offline-id.mjs) that renders the SAME app header
  (AppHeader.vue, same tabs); clicking another tab navigates back into the SPA. Under COEP, cross-origin images
  (iNaturalist thumbnails) are blocked, so reference photos must come through a same-origin proxy/cache.
- Reference photos that exist: /api/species-photos (server/species-photos.mjs): iNaturalist CC-licensed photos of
  live butterflies with attribution (species → genus fallback) + the team's own specimen photos (team data,
  signed-in only). Models and photos are signed-in only today.
- Frozen inventory (server/frozen-stock.mjs, /api/frozen-stock): today species → {M, F, unknown}. The workbook has
  Collection_data Side_Andes (East 6490, West 652, Center 120, NA/empty rest) and Subspecies_Form; Insectary_data has
  subspecies in SPECIES (3rd word) and Collection_location but no Side_Andes (derivable from Collection_data's
  location → side). So per side × subspecies × sex counts are feasible.
- Confirm/reject: there is no server endpoint to record an identification; the debug "Copiar resultado" is removed.

## DRAFT DESIGN (critique and improve this)
Tab name "Identificar" (in the main tab row). Phone-first single column, max ~720 px, desktop two columns on result.

A. First visit (no model on the phone)
```
[app header with tabs …  Identificar]
Identifique una mariposa con una foto
Funciona en este teléfono, también sin internet, una vez descargado.
( ) Rápido y liviano    60 MB  · para cualquier teléfono · acierta un poco menos con ejemplares de colección
( ) Más preciso         650 MB · mejor con wifi y un teléfono reciente
[ Descargar ]                       ← progress bar "23 de 60 MB"; resumes if interrupted
```
B. Ready
```
Listo para usar sin internet ✓ (Rápido y liviano)    Cambiar
¿Dónde la encontró?  [Este de los Andes] [Oeste] [No sé]
[  Tomar foto  ]  [ Elegir foto ]
Consejo: alas abiertas, de frente, que llenen la foto.
```
C. Result
```
[your photo, crop the AI looked at]   La IA cree que es
                                      Mechanitis messenoides  (italic)
                                      78 %  ███████░░  "probable"   (words: >=80 muy probable, 50-80 probable, <50 dudoso)
                                      Género Mechanitis 97 %
[Comparar con fotos]                   ← side by side: your photo | reference photo carousel (credit + licence)
[Sí, es esta] [No es esta]             ← kept only on this phone ("Mis identificaciones", last 30)
Otras posibilidades   (thumb) M. polymnia 12 %  (thumb) M. lysimnia 6 % …   tap → compare that one
Por género ▸  Mechanitis 97 % ▸ messenoides 78, polymnia 12, lysimnia 6 · Melinaea 2 % ▸ …
[Team only] Congelados para genomas de referencia (meta 3 ♂ + 3 ♀ por lado de los Andes)
            Este:  ♂ 12/3 ✓  ♀ 3/3 ✓     Oeste: ♂ 1/3  ♀ 0/3 → faltan 2 ♂ y 3 ♀
            by subspecies rows when the species has them; "Inventario al 3 nov 2026"
Detalles técnicos ▸ (model, seconds per photo, storage used, delete models, install on home screen)
```
Reference photos offline: the photos of each result's top 5 are kept on the phone once seen online; plus an
optional "Fotos de referencia para el campo" pack (2 thumbnails × the project's ~480 Ithomiini species ≈ 15–20 MB)
— pack is a later step unless you argue otherwise. Offline without a cached photo: "Las fotos de referencia
necesitan internet".

## What I want from you
1. Critique the draft for an average citizen: what is confusing, redundant, jargon, dishonest or missing; what the
   result screen should lead with; how to show confidence honestly; how to word the model choice; how confirm/reject
   should behave when it is stored only on the phone (is it worth it at all?); the hierarchy view; where the team
   panel belongs; one-place offline status.
2. Brainstorm improvements (concrete, small enough to build in one day in Vue 3 + Tailwind in this repo).
3. Flag any constraint above you think is wrong (e.g. the separate isolated page, the proxy for photos).

Reply:
GRADE: A-F   VERDICT: APPROVE or CHANGES
F1 [blocker|should|nit] <screen/section> <claim>. Evidence: <...>. Fix: <smallest change>.
Then a short "IDEAS" list (each one line) and, if you would change the layout, an ASCII mockup of your version of
screen C (phone width).
