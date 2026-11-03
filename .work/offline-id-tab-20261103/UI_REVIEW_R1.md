# UI critique, round 1: the implemented Identificar tab

Review only. Do not edit, commit, push, deploy, comment on PRs, or start subagents.

Repo: /home/franz/Documents/CodeProjs/ithomiini-offline-id-tab-20261103 (branch offline-id-tab-20261103)
Work: `git diff wa-lab..20628d6` (46 files). UI files: frontend/src/views/IdentifyView.vue,
frontend/src/components/identify/{RefCompare,StockPanel}.vue, frontend/src/lib/{identifyResult,identifyRefs,
identifyLog,offlineEngine}.ts, frontend/identificar/index.html, frontend/src/identify/*, frontend/src/tabs.ts,
server/{identify-photos,frozen-stock,offline-id}.mjs, frontend/public/sw.js.
Design it implements: .work/offline-id-tab-20261103/DESIGN.md. Brainstorm ledger: LEDGER.md (round 1 findings and
what was done).

Screenshots (Spanish UI, lab copy signed in as the tester; real models, a real collection photo of Hypothyris
ninonia wings): .work/offline-id-tab-20261103/shots-r1/
- a-setup-phone.png: first visit, model choice. b-ready-phone.png: after the download.
- c-result-phone.png, c-result-phone-details-open.png (team panel detail, log and «Modelo y almacenamiento» open),
  c-result-desktop.png (1280 px).
- d-offline-result-phone.png: the same after a reload in airplane mode (Playwright offline), identified again.
Before: .work/offline-id-tab-20261103/shots-before/before-prueba-offline-phone.png (the old test page).

Reader: an average Spanish-speaking citizen on a phone in the forest; team members also use the frozen panel.
Intent: Franz's brief (dedupe, no jargon, no debug step, benefit-framed model choice, honest confidence, hierarchy,
compare reference photos and confirm/reject, frozen counts by sex/subspecies/side vs 3♂+3♀, offline).
Out of scope: opening it without an account (a separate design doc), GPS side, server-side recording of choices.

Checks run: `npm test` (server node tests 959 pass after one route test was taught tabs.ts; frontend vitest 757
pass), `npm run build` (vue-tsc + vite) passes. Playwright: isolated page (crossOriginIsolated true), small model
download, identification 1.3 s, airplane-mode reload and identification again. Not checked: real phones, iOS, the
«Guardar fotos» field set end to end (the server must first look up ~400 species on iNaturalist at 1/s).

Read the surrounding code. Find bugs, broken callers, wrong or misleading claims, UI problems for the reader,
mismatch with intent. Hold to the smallest change that meets the intent; flag code or text the intent does not need.
Other paths, rare cases, small perf, extra compat and style: nits.
blocker = wrong or misleading for the intended case or reader. should = a real problem they will hit. nit = rest.
Reply:
GRADE: A-F   VERDICT: APPROVE or CHANGES
F1 [blocker|should|nit] <file:line or screen> <claim>. Evidence: <...>. Fix: <smallest change>. (stable ids)
