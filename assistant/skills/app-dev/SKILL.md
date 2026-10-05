---
name: app-dev
description: Change the web app itself (Ikiam Insectary DB): its screens, grids, tools, texts or server. Use when the person asks to improve, fix or change how the app looks or works (e.g. "freeze the ID column in the proposals table", "the arrow keys should scroll the view", "add a button…"), or asks how a part of the app is built. Covers where the source is, how to build, test and deploy it, and the rules of the code.
---

# Changing the app (source, build, deploy)

## Where things are

- **Source**: the git checkout `/home/ubuntu/ithomiini/src` (GitHub
  `rapidspeciation/Shiny_Ikiam_Database_Manager`, branch `main`). `git push`
  works through the server's deploy key, an SSH key for this repository only
  (remote `github-ithomiini:…`, Host `github-ithomiini` in `~/.ssh/config`),
  not a personal GitHub account. There is no `gh` CLI on the server.
- **Running app**: a *release* built from the source in
  `/home/ubuntu/ithomiini/releases/<time>`; `current` points at it. These are
  built, minified bundles: changes go in the source. A file patched by hand
  there breaks the version check (the page keeps saying "new version") and is
  wiped by the next deploy.

| Part | Where |
|---|---|
| Screens | `frontend/src`: Vue 3 + TypeScript + Tailwind. Grids are Tabulator 6.5 through `components/SheetGrid.vue`, `CollectGrid.vue`, `components/assistant/ProposalSheet.vue` and `lib/gridKit.ts` |
| Server | `server/*.mjs`: Node 24, node:sqlite. The assistant's tools: `server/assistant.mjs`; notebook matching: `server/notebook*.mjs`; document tools: `server/knowledge.mjs` |
| Assistant instructions | `assistant/`: `AGENTS.md` (the brief; `server/brief.mjs` fills in the person), `skills/`, `agents/` (the app shows them at `#/instrucciones`) |
| Design and docs | `DESIGN.md` and `PRODUCT.md` (read both before UI work), `docs/` |

## Rules of the code

- Every visible text goes through the translation helper: `$t('Texto')` /
  `t('Texto')` with the Spanish text as key, and its English in
  `frontend/src/locales/en/<area>.ts` (a test fails if it is missing).
- Dates are shown and typed day first (dd/mm/yyyy) with `DateField`, not the
  browser's date box. Sheet names, column names and codes stay as they are.
- Speed matters: grids stay fast with thousands of rows.
- The app writes to the team's **real** Google Sheet: test with the checks
  and tests, not by saving data on the live site.

## Steps

1. `cd /home/ubuntu/ithomiini/src && git pull --rebase` (others push to the
   same branch).
2. Tell the person in 2–4 lines what you will change, then change the source,
   following the style of the code around it.
3. Check: `npm --prefix frontend ci` (once), `node scripts/check.mjs`,
   `npm test`. All must pass; add a test for new logic.
4. `git add` the files you changed, `git commit -m "<what and why, in one line>"`
   and `git push`.
5. Deploy: first check that no proposal is being applied (`list_proposals`,
   status `applying`) and tell the person the app restarts for about a
   minute; an apply caught by the restart is left unconfirmed (saves made
   during the restart, and those waiting for Google, are kept and written by
   the new process). Then
   `scripts/deploy.sh`. It builds, tests, makes a new release,
   restarts the app (about a minute; T3 chats keep running) and refreshes the
   T3 workspaces; it stops if your commit is not on GitHub.
6. Verify: `curl -s https://ithomiini-ikiam.com/version.json` shows the new
   build. Tell the person to reload the page (a banner offers it) and what to
   look at. If something broke, say so: the previous release is named in
   `/home/ubuntu/ithomiini/shared/previous-release`, and a fix goes through
   the same steps.

Small, focused changes: one request, one commit, one deploy.
