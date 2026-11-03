Review only. Do not edit, commit, push, deploy, comment on PRs, or start subagents. Do not message anyone.
Repo: /home/franz/Documents/CodeProjs/ithomiini-offline-id-tab-20261103 (branch offline-id-tab-20261103)
Work: `git -C <repo> diff 2164cf3 HEAD -- . ':!frontend/public/identificar/field/*.jpg' ':!frontend/public/identificar/field/*.png'`
(HEAD 7137043). The binary field photos are excluded; their list with credits is frontend/public/identificar/field/index.json.
Base: 2164cf3 (the Identificar tab as already reviewed, team-only).

Reader or user: a member of the public (no account) with a phone in the forest, often offline, who wants to identify a
butterfly; and the team (signed in), whose view must keep working.
Intent (Franz, 3 Nov 2026: make the models free and open weights, not for economic purposes; open the identifier to
signed-out users): signed-out people can open /identificar/, install the app, download either model (small
recommended) from Hugging Face, identify offline, compare with reference photos, paste (Ctrl/Cmd+V) or drop an image
(desktop). No frozen-stock panel, no team data, no database or assistant API calls when signed out. «Acerca de y
licencias» names model licences and credits. The page is cross-origin isolated (COEP require-corp), so HF fetches
must be CORS. IDENTIFY_FOR_EVERYONE on for signed-in members.
Out of scope: the earlier tab design (already reviewed), the server's own identify_species, the rename-photos app.

Published on Hugging Face (check the cards for licence and provenance claims that are wrong or overclaim):
- https://huggingface.co/fr4nzzch/butterfly-wing-detector-onnx (AGPL-3.0; corresponding source to be added under source/: offlineId.ts, offlineEngine.ts, offlinePhoto.ts, publicModels.ts, frozenStock.ts types)
- https://huggingface.co/fr4nzzch/butterfly-id-small-onnx (CC BY-NC 4.0; trained with KL through the CC BY-NC head)
- https://huggingface.co/fr4nzzch/bioclip-2.5-vith14-visual-int8-onnx (MIT, upstream imageomics/bioclip-2.5-vith14)
- https://huggingface.co/fr4nzzch/butterfly-id-classifier (existing, CC BY-NC 4.0; offline/ head export added)
Local copies of the cards: /tmp/claude-1000/-home-franz-Documents-CodeProjs-WingsClassificator/d6a854ca-63e0-45a5-ad19-33d688d96cd7/scratchpad/hf/*/README.md

Checks run: frontend vitest 86 files / 760 tests pass; vue-tsc and syntax check pass; tests/offline-id.test.mjs
pass (adds: list.json gives part hashes; CSP allows huggingface.co and *.hf.co). Playwright on a lab copy
(http://127.0.0.1:8797, scripts and results in .work/offline-id-tab-20261103/public-check/):
- signed out, Chromium, phone and desktop: HF download of the small model, identify, paste and drop each give a new
  result, 286 field photos (8.2 MB) saved, offline reload + identify works; requests to the team API: only
  /api/auth/session (requests-*.json). Screenshots lab-phone/, lab-desktop/.
- signed in after a signed-out download: no new download (same part hashes), server list used, frozen panel shown
  (lab-signed-in/).
- Gap: WebKit could not launch on this PC (missing libs), so Safari/iPhone is untested.
Sources to check against: frontend/src/lib/offlineEngine.ts, offlineId.ts, identifyRefs.ts, publicModels.ts,
views/IdentifyView.vue, components/identify/IdentifyAbout.vue, tabs.ts, components/AppHeader.vue,
server/offline-id.mjs, server/index.mjs (routes /identificar, /api/identify/*), frontend/public/sw.js.

Focus: (1) the signed-out experience (wording, sizes and time, errors, offline); (2) licence notices and credits
(app and HF cards: AGPL corresponding-source obligation, CC BY-NC, MIT credit, iNaturalist photo credits, ND
licences); (3) any path by which team data (frozen inventory, team specimen photos, workbook data) reaches a
signed-out user or the public repo, including caches left on a shared phone and the service worker.
Read the surrounding code or text. Find bugs, broken callers, wrong or misleading claims, mismatch with intent.
Hold to the smallest change that meets the intent. Flag code, text or guards the intent does not need.
Other paths, rare cases, small perf, extra compat and style: nits.
blocker = wrong or misleading for the intended case or reader. should = a real problem they will hit. nit = rest.
Reply:
GRADE: A-F   VERDICT: APPROVE or CHANGES
F1 [blocker|should|nit] <file:line or screen> <claim>. Evidence: <...>. Fix: <smallest change>. (stable ids)
