# Using the app without an account: options (design only, nothing enabled)

3 Nov 2026. Question from Franz: let people without an account install the app and identify butterflies offline
in the field; maybe also rename photos with on-device models only (OCR, side detector, orientation), with no access to
the database or to Claude; and what else could work without an account.

## What is behind the sign-in today, and why

| Piece | Today | Licence / reason | Size |
|---|---|---|---|
| Identificar page (`/identificar/`) and the test page | served to anyone (static) | none; it is only HTML and scripts | small |
| Wing detector `wing_seg_v5n.onnx` | signed in only (`/prueba-offline/models/`) | YOLO26n-seg fine-tuned with Ultralytics: **AGPL-3.0** | 11 MB |
| Full encoder `bioclip25h_visual_int8.onnx` | signed in only | BioCLIP 2.5-H weights are **MIT** (Hugging Face card `imageomics/bioclip-2.5-vith14`, checked 3 Nov 2026). The code comment "BioCLIP's weights are not for public download" (server/offline-id.mjs) is a choice, not a licence term | 608 MB (≈ 440 MB on the wire) |
| Small model `student_L1_int8.onnx` | signed in only | distilled from BioCLIP (MIT) through the head (CC BY-NC 4.0) on images with mixed, mostly non-commercial licences; ViT-S ImageNet-21k start (Apache-2.0). Safest to release it **CC BY-NC 4.0**, like the head | 22.5 MB |
| Head `head_b8127dbf` (json + fp16) | signed in only | **CC BY-NC 4.0**, already public on Hugging Face (`fr4nzzch/butterfly-id-classifier`) | 31 MB |
| Frozen inventory (`/api/frozen-stock`) | signed in only | team data (where specimens are kept) | 0.1 MB |
| Reference photos (`/api/species-photos`, `/api/identify/ref-photo`) | signed in only | iNaturalist photos under CC licences with credit; the team's specimen photos are team data | per photo |
| Rename Photos web app | **already public**: https://fr4nzz.github.io/rename_photos_AI/ (repo public, MIT, its README lists the AGPL YOLO and Apache PaddleOCR models) | its CAMID check uses `db/camids.json`, a public list of CAM IDs (112 KB) | models 39 MB |

Other constraints:
- **AGPL-3.0 of the YOLO models.** Giving the fine-tuned detector to the public over the network means offering its
  corresponding source under AGPL. The rename app already does that (public repo). The database app's repo
  (`Fr4nzz/ithomiini_database`) is **private**, so serving the wing detector to anonymous people from it needs either
  the detector (and its loading code) published under AGPL somewhere public (e.g. the rename app repo or the
  Hugging Face model repo, which already hosts the head), or an Ultralytics enterprise licence, or a non-AGPL detector.
- **Bandwidth.** Each full download is about 440 MB; the small one about 55 MB. Anonymous traffic to
  ithomiini-ikiam.com costs the server's egress and can crowd out the team. Hugging Face serves model files from its
  CDN with CORS, which a cross-origin-isolated page can fetch, so public files need not come from our server.
- **"Not Ithomiini" and tribe badge** read the workbook's Tribe column through the team's inventory snapshot. Without
  an account it needs a static list of Ithomiini genera shipped with the model (a few hundred bytes).
- **iPhone.** An installed web app keeps its storage apart from Safari: the download must happen inside the installed
  app (the tab already says so).

## Options for the identifier

**A. Anonymous offline identification with the small model (recommended).**
The `/identificar/` page as it is, with: the small model, wing detector and head downloadable without sign-in (from
Hugging Face, or a public route with a rate limit), a static Ithomiini genus list, no frozen panel, no team specimen
photos. Reference photos online through a public, rate-limited version of the iNaturalist proxy, and «Fotos para el
campo» as a static, prebuilt set (one CC-licensed photo per Ithomiini species with credit, measured size, hosted next
to the model). Installable as the app's PWA without logging in; the tab row shows only the open pages (Inicio and
Identificar). Cost: about 55 MB per person, once.
- Needs: the AGPL step for the detector; the CC BY-NC 4.0 notice for the head and student on an "Acerca de" line;
  a per-IP limit on the model route if it is served by us.

**B. A + the full model for anyone.** Licence-wise possible (MIT encoder), but 440 MB per person from our server is
the main cost; only sensible from Hugging Face. More accurate on collection specimens, not measured on citizen field
photos. Suggest: offer it only once A is used and the bandwidth is known.

**C. Keep it team-only** (today). Nothing to do; the citizen use case waits.

## Rename photos without an account

Feasible today with no new work: the public GitHub Pages app already reads CAMIDs on the computer (envelope detector
10 MB AGPL, line detector 5 MB and recognizer 8 MB Apache-2.0), rotates by the wing side classifier and wing segmenter
(6 MB + 11 MB; check the side classifier's base-model licence, not written in the repo docs), renames with undo, all
offline after the first load (≈ 39 MB). It never calls our database or Claude; its CAMID check uses the public
`camids.json` snapshot (which can go stale, and is itself public data: decide if that is acceptable).
What the app's «Fotos» tab adds for editors (Photo_links, Drive, the assistant) stays behind the sign-in.
Option: link the public app from the no-account Identificar page («¿Fotografía ejemplares con sobre? Renombre sus
fotos aquí»), rather than build a second copy.

## Other no-account features, grounded in what exists

1. **Where it lives**: from a result, «Ver dónde se ha registrado» opens the public Wings Atlas maps for the species
   (the public maps app already exists). Online only.
2. **The project's public gallery as references**: `/api/public/gallery/` and the Shiny Ikiam Wings Gallery are public;
   their specimen photos of the predicted species could be a second reference source for collection-style photos.
3. **Share a sighting**: «Mis identificaciones» export (CSV with date, the person's choice, the AI's suggestion) or
   an «Abrir en iNaturalist» link, so a citizen's record reaches a public database without our accounts.
4. **Inicio**: the natural-history summaries are already open without an account; the Identificar tab can link to the
   species' life-cycle line there when the project has it.

## Recommendation

Do A for the small model only, with the files on Hugging Face beside the head, a static field-photo set, and no team
data. Link the existing public rename app. Revisit B after a month of A's traffic.

## Decisions for Franz

1. Open identification to people without an account (A)? Yes / later / no.
2. Publish the small model (student) under CC BY-NC 4.0 next to the head on Hugging Face?
3. The AGPL wing detector: publish it and its loading code under AGPL (rename app repo or Hugging Face), buy an
   Ultralytics licence, or replace it?
4. Host the public model files on Hugging Face (no egress for us) or on ithomiini-ikiam.com with a rate limit?
5. The full model for anonymous users (B): not now / later from Hugging Face?
6. Is the public `camids.json` list in the rename app acceptable as it is (it is already public)?
7. For the team version: switch `IDENTIFY_FOR_EVERYONE` on (all signed-in members) once Franz has tried the tab
   on his phones?
