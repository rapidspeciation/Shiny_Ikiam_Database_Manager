# Lane state: Identificar answers (lead)

Started 15 Nov 2026. claudeAgent meter at start: 61 % (reset Wed 18 Nov 19:59 EC).

## Running
- Measurement worker (Opus high, delegate "Measure Identificar thresholds"): PREREG.md rule (commit 9f0a37e7) on E1 small, E2 BioCLIP+6fa6ab78, E3 gallery b8127dbf. Output measure_<ts>/REPORT.md + thresholds.json.
- UI worker (Opus high, delegate "Identificar tribe label UI"): drop «para pruebas» and TRIBE_RUNG flag, tribe/subfamily label under species/genus, per-engine thresholds constant (0.80 placeholders), «Lo más probable» line, tests, lab screenshots ui_<ts>/.

## Next
1. Apply thresholds.json to the constant; check the «Lo más probable» wording against the band accuracy.
2. sol-loop: GPT-6.1 Sol (medium) + fresh Opus 5.5 (high) on the diff and the report.
3. Smoke signed out, no AI chats. Merge main, scripts/deploy.sh under the lock, rollback noted, readlink + /health.
4. done: to parent 61c26ad5 with the report link; NOTES.md in WingsClassificator.

## Added 15 Nov (parent, Franz approved): asks 4 and 5
- Ask 4 name merges: worker "Name merges in both identifiers" (worktree isolation, own sol-loop). Folded into the 1-3 release; measurement steered: primary scoring = merged display (PREREG D1, logged before scores).
- Gallery push after review (pushing deployed gallery work to main accepted).
- Ask 5 Confirmar panel MVP: worker "Confirmar con los rasgos MVP" (worktree isolation, own sol-loop). Ships separately: send parent "done: <lab link>" for Franz to try; deploy only after Franz tries it.
- Merge order into ui/identify-answers-20261115: UI worker, then merges branch, then thresholds, sol-loop on 1-3, deploy. Confirmar branch rebased later.

## Changed 15 Nov (Franz via parent)
- Ask 2: always name top species + genus as the guess; «Incierto (NN %)» below the measured thresholds; safe level (genus/tribe) beside it. UI worker and measurement worker steered (PREREG D2: primary back to strict names).
- Ask 4 cancelled: merges worker told to park its branches (no ship, no push). A separate lane retrains the head.
- Ask 5 approved: ui-loop + sol-loop rounds before the lab link; names read through the guide's name lookup (current and renamed leaves).
- Ask 4 parked: 364f2daf on t3/merge-taxonomy-leaf-names (script only). Handoff sent.
- UI worker done: 2762ee82 (always-guess + Incierto, tribe label, THRESHOLDS 0.80). Lab https://franz-gpdwin4.tailf5d2f1.ts.net:9975/identificar/ (stop: tools/lab/dev-app.sh --stop --port 8893; sudo tailscale serve --https=9975 off). sol-loop r1 (Sol medium, Opus high) on 9f0a37e7..2762ee82 running.
