# Deployed: Claude Code auto-memory off; its two facts moved into data-rules (8 Nov 2026)

Franz approved (8 Nov, via the orchestack maintainer chat): the app's documentation is the memory, not Claude memory.

- Release **20261108T224244Z** (team main e2d7aea6 on 991c5316). Previous: 20261108T013837Z. deploy.sh: server
  tests 1001 pass, frontend 794 pass, type check, build; health ok; all 9 workspaces refreshed. Deploy lock taken and
  released (`deploy.lock.released-<UTC>`).
- Archive (private, chmod 700, in no repo): `claudeclaw:/home/ubuntu/ithomiini/archive/claude-memory-20261108/`
  (angel-ojedamontes: MEMORY.md, pending-oct18-emergidos.md, polymnia-eurydice-proceriformis-same-clutch.md;
  franz-chandi: MEMORY.md, app-cannot-insert-rows.md; SOURCES.txt). The two memory folders under
  `/home/ubuntu/.claude/projects/-home-ubuntu-ithomiini-shared-t3-workspaces-{angel-ojedamontes,franz-chandi}/memory`
  were deleted. The pending-emergidos log is kept in the archive only.
- Server config (outside the repo; backups `*.bak-20261108-automemory`):
  `/home/ubuntu/.claude/settings.json` and `/home/ubuntu/.claude-ithomiini/settings.json` (autoMemoryEnabled false,
  env CLAUDE_CODE_DISABLE_AUTO_MEMORY=1), `/home/ubuntu/.local/bin/claude-ithomiini` (exports the variable), and the new
  drop-in `/home/ubuntu/.config/systemd/user/t3code.service.d/no-auto-memory.conf` (applies at T3's next restart; T3 not
  restarted).
- Repo: t3-provision writes the same two settings into every workspace's `.claude/settings.json`; ithomiini.service
  sets the variable for the app's `claude -p` (WhatsApp gate).
- Docs: `assistant/skills/data-rules/reference/insectary-individuals.md` (polymnia eurydice/proceriformis; Duplicates:
  the app cannot insert Insectary_data rows), `assistant/AGENTS.md` (a curator, PAS, inserts the row by hand).
- Row inserts: still refused. The app's last sync (8 Nov 22:11 UTC) records Insectary_data columns AG–BB as locked
  for its account on every row, so an inserted row reaches them; the only insert ever tried (1 Oct 23:11 UTC) failed;
  server/batch.mjs answers ROWS_PROTECTED ("pide a PAS"). Appending past the pre-made IDs only fills existing grid rows.
  The sheet's convention is plain ID + `.1` for the repeat (112 of 112 suffixed IDs), as data-rules says.
- Checks: throwaway workspaces on claudeclaw in T3's launch mode (stream-json, entrypoint t3-code). Forced-on controls
  (`--settings` with autoMemoryEnabled true and the variable 0) got the "# Memory" prompt section and a memory/ folder
  in both homes; the production paths (the claude-ithomiini wrapper + ~/.claude-ithomiini, the plain binary +
  ~/.claude) got neither. After the deploy, a throwaway chat given only the release's data-rules skill answered the
  polymnia case right (clutch kept, eurydice in SPECIES), again with no memory. Test folders and trust entries removed.

Rollback:
- code and docs: `ssh claudeclaw 'cd /home/ubuntu/ithomiini && ln -sfn releases/20261108T013837Z current && cp current/deploy/ithomiini*.service /home/ubuntu/.config/systemd/user/ && systemctl --user daemon-reload && systemctl --user restart ithomiini.service'`
  then `ssh claudeclaw 'cd /home/ubuntu/ithomiini/current && node=$(sed -n "s/^ExecStart=\([^ ]*\/node\) .*/\1/p" deploy/ithomiini.service) && "$node" scripts/t3-provision.mjs --refresh-all'`
  (an older t3-provision keeps the two workspace settings; remove them by hand or revert and redeploy)
- config: `ssh claudeclaw 'for f in /home/ubuntu/.claude/settings.json /home/ubuntu/.claude-ithomiini/settings.json /home/ubuntu/.local/bin/claude-ithomiini; do cp -a "$f.bak-20261108-automemory" "$f"; done; rm /home/ubuntu/.config/systemd/user/t3code.service.d/no-auto-memory.conf; systemctl --user daemon-reload'`
- memory files: `ssh claudeclaw 'cp -a /home/ubuntu/ithomiini/archive/claude-memory-20261108/angel-ojedamontes /home/ubuntu/.claude/projects/-home-ubuntu-ithomiini-shared-t3-workspaces-angel-ojedamontes/memory && cp -a /home/ubuntu/ithomiini/archive/claude-memory-20261108/franz-chandi /home/ubuntu/.claude/projects/-home-ubuntu-ithomiini-shared-t3-workspaces-franz-chandi/memory'`
- lasting: `git revert e2d7aea6` on main, then `scripts/deploy.sh`.
