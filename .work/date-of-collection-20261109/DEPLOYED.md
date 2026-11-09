# Deployed: the assistant's tools say which column holds the collection date (9 Nov 2026)

Franz approved (9 Nov). His personal WhatsApp chat answered "¿en qué fechas se han hecho colectas en Río Pusuno?" with
four dates. Two of them, 17 Apr and 12 Oct 2023, came from Collection_data's DATE_OF_COLLECTION. The recorded dates
are Collection_date: 21 Jan and 18 Jul 2022.

## Cause

- The chat's first query asked for a column `Date`. `query` answered "no such column" and listed nearby names:
  Collection_date, DATE_OF_COLLECTION, Death_date, Preservation_date, DATE_OF_PRESERVATION. Nothing marked
  DATE_OF_COLLECTION as different. The chat took the column whose name reads "date of collection" and kept using
  it in later turns (query, find_records).
- DATE_OF_COLLECTION is a formula of the Sanger manifest block:
  `=IFS(AD="","",AD="NA","NA",AND(AD<DATE(2023,4,14),XLOOKUP($R,Lists!$AG$1:$AG$10,Lists!$AH$1:$AH$10,"NO")="NO"),AD+451,TRUE,AD)`.
  This adds 451 days to Collection_date for rows dated before 14 Apr 2023 whose genus is not in Lists' Genera_0176.
  That affects 759 rows, and DATE_OF_PRESERVATION 755. In Insectary_data, DATE_OF_COLLECTION is `=M`
  (Preservation_date). The team note is NOTA-DATE_OF_COLLECTION.md, beside this file.

## Fix (main 61edcf40, 12f43d4c, 5a91ed33; on team main and origin main)

- server/manifest-dates.mjs: a note added to the results that show these columns or are asked about them:
  - `query`: a first line, when the statement names them or its columns include them.
  - find_records, count_records (groupBy or filters), search_text, get_record, describe_sheet: `dateNote`.
- `query`'s column-not-found list: the recorded dates come first, and the manifest ones are marked
  "(manifest formula, not the recorded date: Collection_date)".
- data-rules: SKILL.md (Principles, Dates) and reference/field-collections.md say which column holds each date.
- Fixed context: unchanged. No tool description, skill frontmatter, brief or MCP instructions changed.
- tests/manifest-dates.test.mjs: 3 tests. All failed on 6e9b0b76 and pass now.
- Critic: one fresh Opus. It found count_records and search_text without the note, and the reared-butterfly wording;
  both fixed in 5a91ed33.

## Release

- Release **20261109T125547Z**. Previous: **20261109T033553Z**.
- deploy.sh results: server tests 1007 pass and 0 fail, frontend 794 pass, type check and build passed. Health ok,
  locally and at https://ithomiini-ikiam.com/health. All 9 workspaces were refreshed.
- The deploy lock `claudeclaw:/home/ubuntu/ithomiini/deploy.lock` was taken at 12:54:43Z and released as
  `deploy.lock.released-20261109T125822Z`.

## Smoke test

The test ran on claudeclaw in a throwaway copy of Franz's app workspace (not WhatsApp), using the
claude-ithomiini wrapper and Opus 5.5.

- Question: "en qué fechas se han hecho colectas en Río Pusuno?"
- First call: `query` on Collection_date.
- Answer: two dates, **21/01/2022 (25)** and **18/07/2022 (86)**.
- A direct MCP call on production checked the rest:
  - a query on DATE_OF_COLLECTION starts with the note;
  - `MIN(Date)` lists the marked manifest columns last;
  - count_records grouped by DATE_OF_COLLECTION has `dateNote`.
- Cleanup: the throwaway folder and its Claude session folder were removed. No trust entry was written; the tools
  were allowed with `--allowedTools`.

Eval: routing case R09 ("¿qué días se ha ido a colectar a Wildsumaco?", Collection_date, not DATE_OF_COLLECTION).
It is on local branch eval-r09-collection-date-20261109 (4ca9414b, on assistant-eval-20261104) and was handed to
the eval lead.

## Rollback

- Code and docs:
  `ssh claudeclaw 'cd /home/ubuntu/ithomiini && ln -sfn releases/20261109T033553Z current && cp current/deploy/ithomiini*.service /home/ubuntu/.config/systemd/user/ && systemctl --user daemon-reload && systemctl --user restart ithomiini.service'`
  then
  `ssh claudeclaw 'cd /home/ubuntu/ithomiini/current && node=$(sed -n "s/^ExecStart=\([^ ]*\/node\) .*/\1/p" deploy/ithomiini.service) && "$node" scripts/t3-provision.mjs --refresh-all'`
  (this puts back the previous data-rules skill in the workspaces).
- Lasting: `git revert 5a91ed33 12f43d4c 61edcf40` on main, then `scripts/deploy.sh`.
