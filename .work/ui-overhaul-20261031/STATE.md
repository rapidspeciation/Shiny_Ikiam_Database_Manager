# UI overhaul — lead state (times EC)

- 31 Oct 19:57 start. Meter: 7-day 46.0 % (resets Wed 4 Nov 20:00). Budget: ≤ ~20 % of the week.
- Worktree /home/franz/Documents/CodeProjs/ithomiini_database-uioverhaul, branch ui-overhaul-20261031 from origin/main 7392da7 (= live 20261031T190916Z).
- Screenshots and scripts outside git: /home/franz/.local/share/ui-overhaul-20261031/
- Deploy lock (new, announced to model lead): claudeclaw:/home/ubuntu/ithomiini/deploy.lock (holder, start, rollback).
- Children: T3 UI research → PRINCIPLES.md (Opus medium); inventory + audit → INVENTORY.md, AUDIT.md (Opus medium).
- Next: review PRINCIPLES + AUDIT → dark-theme foundation (tokens) → fix waves → phase 2 Wings Atlas (worktree off rapidspeciation/ithomiini_maps main; never touch ~/Documents/CodeProjs/ithomiini_maps).
- RULE (Franz, 31 Oct): tests that open T3 chats (Asistente, T3 frame) use the LAB T3 (:3775, home ~/.t3-ithomiini-lab), never the main T3 (:3774); archive any test threads made in the main T3. Put this in every child brief. (Sent to the audit child.)
- PRINCIPLES.md done (research child), committed 2cbd9d5 on ui-overhaul-20261031.
- Dark-theme foundation started early (Opus high) in worktree ithomiini_database-uidark, branch ui-dark-20261031 (from 2cbd9d5); merge into ui-overhaul after the audit.
- 20:1x audit child killed (OOM: PC 15 GB, other chats ~8 GB in 59 claude procs). Retry child started with memory rules (MemAvailable ≥2.5 GB, one app + one browser, heap 3072). Dark child told the same.
- Dark child also OOM-killed after 5 commits (316bdec..d136612, clean tree). Continuation child started (Opus high), serialised behind the audit: code/tests first, app/browser only at MemAvailable ≥3 GB.
- 20:24 systemd-oomd killed the main T3 service (all chats). Audit retry + dark continuation resumed in their own threads (context kept). Model lead told.
- RULES (model lead, after the 20:24 OOM; put in every child brief): ≤2 of my children running at once (queue the rest); MemAvailable ≥ 2.5 GB before an app instance, browser or build, else wait (no busy loop) or reuse the lab app :8795; one headless Chromium per child, closed after each batch of screenshots; builds/tests preferably on claudeclaw or one shared lab instance, not one per worktree.
- 1 Nov: model lead fixed the memory root cause (release 20261101T021806Z = main a6ad89e). 2-child cap lifted. Keep: child apps via tools/lab/dev-app.sh (on main a6ad89e; merge main into ui-overhaul first), one Chromium per child. Audit at 441 shots.
