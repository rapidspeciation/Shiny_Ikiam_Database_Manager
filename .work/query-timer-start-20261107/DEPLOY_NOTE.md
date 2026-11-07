# Deploy note: the query limit starts when the query process is ready (7 Nov 2026)

Not deployed. Branch `fix/query-timer-start-20261107`, based on the live commit 7048d726 (team/main; origin/main is 2 commits behind).

Change: `server/query-tool.mjs`. The worker sends `{ ready: true }` once it is set up. A query's 5 s limit starts at
that point, not at `fork()`. A new process gets its own 30 s start limit (`QUERY_START_TIMEOUT_MS`), with the message
"The query process did not start within 30 s." Nothing else in the assistant changes; there is no config to set.

Deploy (lead, after review):
- `git -C <checkout> merge --ff-only fix/query-timer-start-20261107` on main, then push it to origin and team.
- `scripts/deploy.sh` (it runs the server and frontend tests, builds, swaps the release and restarts the service).
- Check: https://ithomiini-ikiam.com/health shows `"status":"ok"`, then run one assistant `query` call.

Rollback:
- `ssh claudeclaw 'cd /home/ubuntu/ithomiini && ln -sfn releases/20261107T005332Z current && systemctl --user restart ithomiini.service'`
  (20261107T005332Z is the release live on 7 Nov, commit 7048d726).
- Lasting: `git revert` of the commit "Query tool: the time limit starts when the query process is ready…" (the branch tip) on main and `scripts/deploy.sh`.
