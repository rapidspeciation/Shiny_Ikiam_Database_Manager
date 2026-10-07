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
| Server | `server/*.mjs`: Node 24, node:sqlite. The assistant's tools: `server/assistant.mjs`, run in a worker thread (`server/assistant-host.mjs`) that reads the database through `server/store-reader.mjs` (the Store's reads it lists, writes only to the proposals); notebook matching: `server/notebook*.mjs`; document tools: `server/knowledge.mjs` |
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

## What a change is for

A change the person asks for goes live in production here: it is the only
copy of the app they have to try it. If they don't like it, offer to revert
it. Ask first only when it changes a rule the whole team follows.

When the app makes the person or you work around it (a note standing in for
a highlight, empty rows chosen to avoid a protected column), name the change
in the app that would remove the workaround: it is usually the change to
make. Before adding an option the AI sets row by row, check whether the app
could know it by itself (e.g. which rows are dead, from Death_date). The
limits in the tests on tool description and answer sizes keep every chat
light: make a change smaller before raising one.

## Steps

1. Tell the person in 2–4 lines what will change.
2. Several chats share `/home/ubuntu/ithomiini/src`, so work in a copy of
   your own:
   `git -C /home/ubuntu/ithomiini/src fetch -q origin && git -C /home/ubuntu/ithomiini/src worktree add /home/ubuntu/ithomiini/work/<topic> -b chat/<topic> origin/main`,
   then, in it, `ln -s /home/ubuntu/ithomiini/src/frontend/node_modules frontend/`.
3. A subagent does the code, in the background, while you keep answering
   the person. Its brief: the problem with one real case (row, proposal),
   where to start in the code, the tests to add, the worktree to commit in,
   and "report files, tests and open questions in a few lines".
4. Check: `node scripts/check.mjs`, `node --test tests/<area>.test.mjs`, and
   `npm --prefix frontend test` for screens. A failing test describes
   something the change broke: fix the code, and change a test's
   expectation only where the behaviour asked for changes it (say so in the
   commit). For changes to sync, proposal tables or grids, measure before
   and after (`node tools/lab/proposal-load.mjs` works without Google) and
   put the numbers in the commit.
5. Release: in `/home/ubuntu/ithomiini/src`, `git pull --rebase`,
   `git merge chat/<topic>`, `git push`. Then read
   `curl -s localhost:8794/health`: deploy when `google.workbook.state` is
   `ok` and every number in `writing` is zero; otherwise wait and tell the
   person why. A deploy restarts the app for everyone, so changes from the
   same hour go out together: tell the person, then run `scripts/deploy.sh`
   once in the background. It runs the full tests (a failure stops it
   before anything changes), builds, restarts the app (T3 chats keep
   running) and refreshes the T3 workspaces; saves waiting for Google are
   kept and written by the new process.
6. Verify: `version.json` shows the new build; a minute later /health has
   `"status":"ok"` and an `eventLoop` p99 of a few ms; use the change once
   (e.g. the new option on this chat's proposal). Tell the person to reload
   and what to look at, and say what you could not see on a screen. If
   something broke, or the person wants it undone, `git revert` the commit
   and deploy again (`/home/ubuntu/ithomiini/shared/previous-release` names
   the release before). Remove the
   worktree when done (`git -C /home/ubuntu/ithomiini/src worktree remove /home/ubuntu/ithomiini/work/<topic>`).
