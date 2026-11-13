# Sign-in gates of Identificar, Buscar, Descargas and Compare (13 Nov 2026)

Branch `feat/public-reads-20261113` from `origin/main` 8d4e13b2. Rule (lead, from Franz's message): what only shows
public data goes public; writes stay behind the sign-in. Public = published by the Wings Gallery
(https://rapidspeciation.github.io/Shiny_Ikiam_Wings_Gallery/, its `data/*.json` and photos) or under a CC licence.

What the Wings Gallery publishes (checked 13 Nov 2026 on the live files):
- `data/collection.json` (9,754 rows of Collection_data): `CAM_ID`, `Species`, `Subspecies_Form`, `Sex`, locality,
  dates, coordinates, `Photo_dorsal`/`Photo_ventral`, `URLd`/`URLv`, `all_photos` and more.
- `data/photo_lookup.json` (6,207 CAMs): every Photo_links photo of a CAM as `{ Name, URL_to_view: drive.google.com/uc?id=<Drive file id> }`.
- `data/insectary.json`, `predictions*.json`, `sex_predictions*.json`, `wing_boxes.json` (model readings).

## Gates

| # | Gate (endpoint or code path) | Returns, field by field | Public? (where published) | Decision |
|---|---|---|---|---|
| 1 | `POST /api/species-photos` (server/index.mjs; server/species-photos.mjs `photosOf`). Was behind the API's sign-in check. Used by Identificar's candidates and Compare, and by Buscar («Más fotos de la especie», Museo) via lib/identifyRefs.ts `referencePhotos` | per name: `inat.taxon` {id, name, rank, common, url}; `inat.level`; `inat.synonymOf`; `inat.photos[]` {id, thumb, url, attribution, license, observation}; `pending`; `specimens[]` {cam, sex, dorsal, ventral} | iNaturalist part: photos filtered to a CC licence (`license_code` present), shown with their credit (CC licence). Specimens: CAM_ID and Sex are in collection.json; dorsal/ventral are Drive file ids of Photo_links photos, in photo_lookup.json | **Open** (server/public-reads.mjs `anonymousSpeciesPhotos`). Anonymous answer: fields whitelisted (anything a later server adds stays out until listed); specimens only with a public photo (Photo_links, not a raw camera file; the CAM's first public dorsal and ventral, else the specimen is left out); at most **20 names** a request (team: 300), each at most **120 characters**; a name the identifier's head (all of Lepidoptera: **11,965 names**, not only Ithomiini) or `identificar/curated.json` knows is read as their exact spelling (case and spacing variants are one name: one cache entry, one job), and only those may join the existing background queue (1 request/s) when not cached, **only while fewer than 20 names wait and at most 300 a day for all visitors together**; anything else is answered from the cache alone; keyed by each name exactly as sent; **120 requests / 15 min per address** |
| 2 | `GET /api/identify/ref-photo?u=` (server/identify-photos.mjs `sendRefPhoto`): the same-origin proxy of iNaturalist photos, thumbnails and the viewer's «large» | the image bytes of one iNaturalist photo (iNaturalist's photo hosts and sizes only, 3 MB cap) | CC-licensed iNaturalist photos (the ids come from gate 1's CC-only list or curated.json, itself built from them) | **Open** for photos the server lists: an iNaturalist photo id in `species_photo_cache` or in `curated.json`; any other URL 404 without a fetch (no open proxy). Cache ids count only CC-licensed photos, like the answer. One upstream GET per request, nothing fanned out; **3000 / 15 min per address**. Lets the viewer show «large» signed out |
| 3 | `GET /api/photo/<driveId>?w=400|1600` (server/photos.mjs): the project's specimen photos (Compare's Museo, Buscar's specimens) | image bytes of a Drive file, resized by Drive, cached | Photo_links files under a CAM name: listed in photo_lookup.json with their Drive URL | **Open** for files whose source is Photo_links (`publicPhotoFile`), except raw camera files (.cr2 .orf .nef .arw .raf .rw2 .dng: 7,414 of the lab's 17,753 Photo_links files, not in photo_lookup.json). Kept for the team: those, files only in the photo archive's manifest (envelope_readings), and `?card=` (a chat card's file not in Photo_links). **1500 / 15 min per address**; a photo not cached yet is fetched from Drive within **one budget shared by every visitor: 2 a second, 20 at once** (503 `BUSY` past it); cached photos are not counted. Same CSP sandbox and widths as for the team |
| 4 | «Más fotos» extras (frontend: lib/identifyRefs.ts `referencePhotos` returned only the static field set when signed out) | the server's extra iNaturalist photos (gate 1), in the curated order of `curated.json` (adults, then unsure, caterpillars, pupae, eggs) | CC licence | **Open**: `referencePhotos` asks the server online whether signed in or not (the `signedIn` option is gone); TaxonSearch's `signedIn` prop removed |
| 5 | Viewer's large photo (lib/bestPhotos.ts `viewerPhoto`: «large» only `signedIn && online`) | iNaturalist «large» through gate 2 | CC licence | **Open**: online is enough; offline or no iNaturalist id: the page's own photo, as before |
| 6 | Specimen and Sanger photos: Sanger wing crops (`identificar/wings/`, static), team specimens (gate 3) | wing crops: static files with `WING_CREDIT`; specimens: gate 3 | wing crops already served to everyone (static); specimens: photo_lookup.json | Wing crops: **already open**. Specimens: **open** via gates 1 and 3 |
| 7 | Cached lists used offline: `prueba-offline/refs.json` in Cache Storage (lib/identifyRefs.ts `kept`/`keep`) | the last server lists per name, iNaturalist photos only (specimens dropped before keeping) | CC licence | **Open**: signed-out visitors now keep and reuse them (before: only the static set) |
| 8 | Static Identificar files: `identificar/field/` (field set), `identificar/packs/` (photo packs), `identificar/curated.json`, `identificar/countries.json` | CC photos with credit; curated scores; country shapes | CC licence / project-made curation | **Already open** (served by `serveOfflineId` with `user: null`); unchanged |
| 9 | Phone models for signed-out visitors (Descargas; lib/publicModels.ts): small model, detector, head from Hugging Face | model files | head CC BY-NC 4.0 on Hugging Face | **Already open** (Hugging Face); unchanged |
| 10 | `/prueba-offline/models/*` (server/offline-id.mjs): the server's copies of the models, the sex model among them | model files | the sex model is not published anywhere (see Unclear) | **Keep** (signed-in only, 401) |
| 11 | In-hand sex model download and its estimate (IdentifyView `sexRow`, `sexState`, `downloadSexModel`; server `SEX` in offline-id.mjs, «Never public») | the experimental sex model (43 MB fp16 ONNX), its calibration | a model, not data; not published; marked «never public» by an earlier decision | **Keep**, listed under Unclear |
| 12 | Online identifier `POST /api/ai/identify-photo` (server/assistant.mjs → identify-photos.mjs `identifyUpload`) and with it the general sex line (`fieldSex`, IdentifyView `fieldSexState`), «Reprocesar» in the server's mode, the mode choice («En línea (servidor)») | species/subspecies/genus/tribe lists, crop, wing box, `fieldSex` | the answers are model readings of the person's own photo; the cost is the server's | **Keep**, listed under Unclear (cost) |
| 13 | `GET /api/frozen-stock` (server/frozen-stock.mjs) and StockPanel | per head species: flash-frozen specimens kept, by sex and side of the Andes, gone ones excluded | derived from Collection_data/Insectary_data columns the gallery publishes (Preservation_medium, Location_*) | **Keep**, listed under Unclear (a team planning panel) |
| 14 | `GET /api/identify/field-photos` | one CC photo per Ithomiini species with credit | CC licence | **Keep**: not used by Identificar, Buscar, Descargas or Compare (the field set is a static file, gate 8), and its call would queue every head species at once |
| 15 | Contributions: `POST /api/identify/contributions`, `GET …/mine`, `GET …/consent`, `DELETE …/<id>`, review queue; «Enviar», «Mis envíos» | writes and the person's own items | not public | **Keep** (401 signed out; tested). Signed-out «Enviar»: unchanged (the card shows only signed in, `contribFor`) |
| 16 | Monitoreo (`/api/monitoring/*`), `/api/sex-check`, every other `/api/*` | team data and writes | not public | **Keep** |

## Unclear (kept gated; for Franz)

1. **The experimental in-hand sex model** (gate 11). It is a model, not data, and the code calls it «never public»
   (server/offline-id.mjs `SEX`, an earlier decision). Opening it means publishing a model trained on the team's
   photos (licence to choose, and 43 MB per phone from our server, or a copy on Hugging Face). The sex predictions
   themselves are public on the gallery (`sex_predictions_app.json`).
2. **The online identifier and the general sex line** (gate 12). Per photo it uses the AI Identifier process: about
   2.3 GB of memory while it runs, 3 of the server's cores, one photo at a time in one queue shared with the team's
   photos (server/identifier.mjs). The time per photo on the server was not measured here, so the CPU an anonymous
   visitor would cost is unclear. Signed-out visitors keep the phone's model, which needs no server CPU.
3. **The frozen stock panel** (gate 13). Its counts come from columns the gallery publishes, but it is the team's
   «what to keep» planning view, not a public reading of the collection.

## Abuse limits (anonymous only; signed-in people keep theirs)

- Per address and 15 minutes (fixed window): photo lists 120, iNaturalist photos 3000, specimen photos 1500 (one
  campus or field-station network shares one address; the browser keeps each photo for 30 days). 429 with
  `Retry-After` past it. Buscar and Identificar then show one line: «Muchas visitas desde esta red. Inicie sesión
  para ver todas las fotos.» (EN «Many visits from this network. Sign in to see every photo.»): a list's 429 through
  the API, a photo's by asking a failed photo of the app's once more, at most 3 per batch (a taxon in Buscar, a
  candidate compared; lib/visitorLimit.ts). The line hides once signed in. A photo refused with 503 BUSY (the Drive
  budget below) is shown again once after its Retry-After (2 s), never more.
- The address: the last X-Forwarded-For entry when the connection is local (the one Caddy sets; a visitor's own
  first entries are never the key), else the connection's (deploy/Caddyfile.fragment notes it). The sign-in limits
  use the same address.
- The limiter sweeps expired windows at most once a minute and keeps at most 50,000 addresses, the oldest going first.
- Specimen photos not cached yet: one Drive budget for every visitor together, 2 a second with 20 at once (503 past
  it, said in one log line). Cached photos and signed-in people are not counted.
- 20 names a request (Identificar asks its 5 candidates, Buscar batches of 20), each at most 120 characters.
- No new live fan-out to iNaturalist's API: cached answers, or the existing background queue (1 request a second) for
  names the head or the curated list know, under their exact spelling; a made-up or re-cased name never makes a job
  of its own. The head is all of Lepidoptera (11,965 names), so a visitor's name joins the queue only while fewer
  than 20 names wait, and at most 300 a day for all visitors together (a bucket refilled over the day); past either,
  the cache alone answers (`pending: false`). The team's lookups have neither cap. The head's names are read again after a failed or empty read (each minute) and when its file changes
  (a new release); the curated list every 10 minutes.
- The photo proxy sends only photos the server lists, one upstream request per request.

## Tests (no AI, no live iNaturalist)

Round 1 critics (Sol 6/10, Opus 8/10) added: case variants of a cached name make no upstream call and those of an
uncached one share one job (Sol F1); raw files 404 and a specimen falls back to its JPEG or is left out (Opus F1);
the shared Drive budget, cached photos not counted (F2); higher photo limits and the 429 line (F3); a spoofed first
X-Forwarded-For entry (F4); the limiter's sweep and cap (F5); CC-only cache ids (F6); names over 120 characters
(F7); the head read again after a failure and on a new release (F8); the lookups awaited before teardown (F9).

Round 2 critics (Sol 8/10, Opus 8/10) added: a visitor's name not queued while 20 wait or past the day's 300, the
team's queued (Opus R1); a BUSY photo shown again once after Retry-After, and a 429 found after earlier 404s of another
batch (Sol R1, Opus R2); the line hidden once signed in (R3); the answer keyed by each name exactly as sent (R4: the
frontend sends catalogue spellings, but keying by the name as sent is what `referencePhotos` reads, whatever it sends).

- tests/public-reads.test.mjs: signed out, each opened route (species photos, ref-photo, photo); the 20-name cap (team
  300); the per-address limit with Retry-After, other addresses and the team not held; non-public fields absent
  (extra cache fields, emails, the collector, a manifest-only specimen, `?card=`); unknown photos 404 without a fetch;
  contributions (POST, mine, consent, DELETE), frozen stock, field photos, sex check, online identifier, Monitoreo
  and the models' route still 401.
- tests/auth-routes.test.mjs: `/photo/nope` for a visitor is now 404 (not listed) instead of 401.
- frontend: identifyRefs.test.ts (signed out and online the server's photos and specimens are asked and shown; a
  refusal falls back to the static set), bestPhotos.test.ts (`viewerPhoto` without `signedIn`).

## Lab and the signed-out check

- Lab copy of this branch: LOCAL_MODE on the lab snapshot (`tools/lab/dev-app.sh --port 8892`, the identifier and
  photo pack folders of the identify-panel lab), tailnet only, on port 9971 of the lab PC (`/identificar/`).
  Since round 1 it is started through `systemd-run --user` (no chat owns it, so the reaper leaves it; 72 h limit).
- Round 1 on the lab, signed out: «Mechanitis Polymnia» and «mechanitis polymnia» answered from one entry; a 121
  character name 400; a raw `.ORF` of Photo_links 404; the Buscar check below unchanged.
- Signed out, 390x844 (Playwright, headless): Buscar «Mechanitis polymnia» → «Más fotos de la especie» shows 11
  photos: the adults in the curated order (59300165, 59697583, 2602438, 581501869, 137194978), the project's 4
  specimens (`/api/photo`, 200), then the caterpillar (156258933) and the pupa (2090393) last. The viewer opens
  iNaturalist «large» through `/api/identify/ref-photo` (960x960). `POST /api/identify/contributions` signed out: 401.
- The 4 specimens' CAM_ID, Sex and Drive files were checked against the gallery's live collection.json and
  photo_lookup.json: all listed.
