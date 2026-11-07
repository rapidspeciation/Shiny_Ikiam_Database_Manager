# Deploy note: query copy dates only, `tubes` table (7 Nov 2026) — not deployed

Branch `fix/assistant-na-dates-tubes-20261107`, on top of the live commit 7048d726 (release 20261107T005332Z;
origin/main is two commits behind it at 44835582, team/main has 7048d726). Not pushed, not deployed.

What changes for the assistant (all chats, after the service restarts):
- The `query` copy (`shared/sheets.sqlite`) is rebuilt once (LAYOUT 3 → 4): date columns hold only dates; NA and other
  text in a date cell move to `<column>_text`; new `tubes` table. About +0.3 s per build and +16 % file size on the
  lab copy (3.06 → 3.36 s, 48 → 56 MB).
- `query` tool description: two new lines (date columns, tubes).
- data-rules `samples-ids.md`: what "the last tube" means (t3-provision copies skills to the chats' workspaces).

Deploy (from a PC checkout, as the lead):
1. `git -C <checkout> fetch team && git -C <checkout> merge --ff-only fix/assistant-na-dates-tubes-20261107` on main
   (main must contain 7048d726 first: `git merge --ff-only team/main`), then `git push origin main` and `git push team main`.
2. `scripts/deploy.sh` (runs the full tests, makes a new release, restarts, runs `t3-provision.mjs --refresh-all`).
3. Check: `ssh claudeclaw 'python3 -c "import sqlite3;c=sqlite3.connect(\"file:/home/ubuntu/ithomiini/shared/sheets.sqlite?mode=ro\",uri=True);print(c.execute(\"select count(*) from tubes\").fetchone(), c.execute(\"select count(*) from Insectary_data where Death_date >= \x272026-10-29\x27 and Death_date_text is null\").fetchone())"'`
   (the copy is rebuilt within a minute of the restart).

Rollback:
- `ssh claudeclaw 'cd /home/ubuntu/ithomiini && ln -sfn releases/20261107T005332Z current && systemctl --user restart ithomiini.service'`
- then `ssh claudeclaw 'cd /home/ubuntu/ithomiini/current && node=$(sed -n "s/^ExecStart=\([^ ]*\/node\) .*/\1/p" deploy/ithomiini.service) && "$node" scripts/t3-provision.mjs --refresh-all'`
- The old release's LAYOUT 3 differs from the copy's: it rebuilds the copy in its own layout on start.
- lasting: `git revert <this commit>` on main and `scripts/deploy.sh`.
