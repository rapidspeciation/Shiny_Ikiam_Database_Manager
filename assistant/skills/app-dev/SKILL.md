---
name: app-dev
description: Change the web app itself (Ikiam Insectary DB): its screens, grids, tools, texts or server. Use when the person asks to improve, fix or change how the app looks or works (e.g. "freeze the ID column in the proposals table", "the arrow keys should scroll the view", "add a button…"), or asks how a part of the app is built. Covers where the source is, how to build, test and deploy it, and the rules of the code.
---

# Changing the app (source, build, deploy)

The app's source is a git checkout on this server: **`/home/ubuntu/ithomiini/src`**
(GitHub `rapidspeciation/Shiny_Ikiam_Database_Manager`, branch `main`; you can
push to it). The running app is a *release* built from it in
`/home/ubuntu/ithomiini/releases/<time>`, and `current` points at it.

**Never edit files in `releases/` or `current/`** (built, minified bundles):
a hand patch there breaks the version check (the page keeps saying "new
version") and the next deploy wipes it. Always change the source.

## Steps

1. `cd /home/ubuntu/ithomiini/src && git pull --rebase` (other people and
   Franz's PC push to the same branch).
2. Say to the person in 2–4 lines what you will change, then change the
   source. Read the code around first and follow its style:
   - `frontend/src` (Vue 3 + TypeScript + Tailwind; grids are Tabulator 6.5
     through `components/SheetGrid.vue`, `CollectGrid.vue`,
     `components/assistant/ProposalSheet.vue` and `lib/gridKit.ts`).
   - `server/*.mjs` (Node 24, node:sqlite; the assistant's tools are in
     `server/assistant.mjs`, notebook matching in `server/notebook*.mjs`,
     the document tools in `server/knowledge.mjs`).
   - `assistant/` (this brief and the skills), `docs/`, `DESIGN.md`,
     `PRODUCT.md` (read these two before UI work).
3. Rules of the code:
   - Every visible text goes through the translation helper: `$t('Texto')` /
     `t('Texto')` with the Spanish text as key, and its English in
     `frontend/src/locales/en/<area>.ts` (a test fails if it is missing).
   - Dates are shown and typed day first (dd/mm/yyyy; `DateField`, never the
     browser's date box). Sheet names, column names and codes never change.
   - Speed matters: grids must stay fast with thousands of rows.
   - The app writes to the team's **real** Google Sheet: never test by saving
     data on the live site.
4. Check: `npm --prefix frontend ci` (once), `node scripts/check.mjs`,
   `npm test` — all must pass; add a test for new logic.
5. `git add` the files you changed, `git commit -m "<what and why, in one line>"`
   and `git push`.
6. Deploy: `scripts/deploy.sh` (it builds, tests, makes a new release,
   restarts the app — about a minute — and refreshes the T3 workspaces; it
   refuses if your commit is not on GitHub). The app restart does not stop
   T3 chats.
7. Verify: `curl -s https://ithomiini-ikiam.com/version.json` shows
   the new build; tell the person to reload the page (a banner offers it) and
   what to look at. If something broke, say so; the previous release is in
   `/home/ubuntu/ithomiini/shared/previous-release` and a fix goes through
   the same steps.

Small, focused changes are best: one request, one commit, one deploy.
