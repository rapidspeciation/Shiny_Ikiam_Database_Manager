# Identificar for everyone: sol-loop ledger (3 Nov 2026)

Brief: PUBLIC_REVIEW_BRIEF.md. Round 1 on 2164cf3..7137043; round 2 on 7137043..<r1 fixes> plus the HF cards.

## Round 1: Sol C / CHANGES, Opus B / CHANGES

| critic | id | sev | finding | outcome |
|---|---|---|---|---|
| sol | F1 | blocker | team reference photos stay after signing out on the page; late answers merge | fixed: the page reloads when the session changes on it (IdentifyView onMounted watch); checked with sign-out.mjs: 5 team/proxy images before, 0 after, no panel, kept inventory removed |
| sol | F2 | blocker | source/ is not the complete corresponding source (no build, entry, export) | partly: export command, Ultralytics and onnxruntime-web versions, how the page calls the engine added; claim of completeness removed. Disagree that the whole app must be published here: Franz asked to keep it to the code that uses the detector and not to publish the database repo; left to Franz as a decision |
| sol/opus | F3 / F1 | should | encoder repo lacks the MIT notice | fixed: upstream LICENSE (Imageomics/bioclip-2) added, card points to it (HF 2bdc40b8) |
| sol/opus | F4 / F3 | should | photo credits without source and licence links | fixed: credit links to the iNaturalist photo page, licence to its CC deed (field set and team-session photos); test added |
| opus | F2 | should | detector card overclaims ("as that licence asks", "rest of the app does not use it") | fixed: both removed; plain description of the files |
| opus | F4 | should | About credits less than the classifier card | fixed: BoA, Sangay, Noreste, Cotacachi, GBIF, iNaturalist |
| opus | F5 | nit | session slower than 8 s falls to signed out | fixed by the same reload watch |
| opus | F6 | nit | expired session served from SW cache offline shows inventory | declined: rare, pre-existing; cleared at the next online start |
| opus | F7 | nit | "este teléfono" on desktop | fixed in the new About line; kept elsewhere (phone-first page) |
| opus | F8 | nit | MB units | declined |
| opus | F9 | nit | Safari untested | follow-up: one real-iPhone run of the small download |
| self | - | should | detector card said NMS inside the graph | fixed: the file is the end-to-end (TopK) export (metadata end2end True, Ultralytics 8.4.132), matched to the .pt on 12/12 photos (wingseg_v5_field FINDINGS §6 note) |

## Round 2: Sol B / CHANGES, Opus B+ / CHANGES

Sol: F1 resolved, F3 resolved, F4 resolved, F2 still open (no runnable entry or build config). Opus: F1-F4, F7 resolved;
F6, F8 declines and F9 follow-up accepted; F5 still open through N1.

| critic | id | sev | finding | outcome |
|---|---|---|---|---|
| sol | F2 | blocker | no runnable example with build config | fixed: source/example/ (package.json with onnxruntime-web 1.30.0, Vite, COOP/COEP config, copy-ort.mjs, index.html, main.ts, README). Built and run in Chromium from a clean copy: isolated, HF download, crop success, Hypomenitis libethris 91.1 % (as the app). HF commit 2ab9c3be |
| opus | N1 | should | late session (> 8 s) reloads every time on weak signal; inventory cleared on the timeout path | fixed: reload only when the session was known and changes; a late session is applied without reload; forgetStock only when the server answered. slow-session.mjs: session delayed 10 s, 1 load, panel shown |
| opus | N2 | nit | watch not stopped on unmount | fixed: stopped in onBeforeUnmount |
