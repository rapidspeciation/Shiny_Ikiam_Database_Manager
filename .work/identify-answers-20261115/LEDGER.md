# sol-loop ledger: Identificar guess (asks 1-3)

Models: GPT-6.1 Sol (codex, reasoning medium), Claude Opus 5.5 (claudeAgent, effort high). Usage: not exposed.

| Round | Diff | Sol | Opus |
|---|---|---|---|
| 1 | 9f0a37e7..2762ee82, 8 files +428 -111 | 6/10 | 8/10 |

| unit | round | critic | id | sev | outcome | reason |
|---|---|---|---|---|---|---|
| guess | 1 | Sol | F1 | blocker | fix | guessOf substitutes a lower species; Franz asked for the top species always |
| guess | 1 | Opus | F1 | should | fix (same as Sol F1) | lead decided by Franz's words |
| guess | 1 | Opus | F2 | should | fix | green badge / «No parece una Ithomiini» overclaim under unclear answers |
| guess | 1 | Opus | F3 | nit | fix | unused ranksAbove branches |
| guess | 1 | Opus | F4 | nit | fix | aria-label jargon |
| guess | 1 | Opus | F5 | nit | declined | taxonomy JSON load is needed for the label |
| 2 | 9f0a37e7..bdb4a98a, 8 files +370 -108 (r1 fixes + cuts) | 6/10 | 7/10 |

| guess | 2 | Sol+Opus | F1 | blocker/should | fix | «La foto muestra» and «Seguro» too strong -> «parece», «Probable» |
| guess | 2 | Sol+Opus | F2 | should | fixed 4044f730 | REPORT names shipped D5 cuts |
| guess | 2 | Opus | F3 | should | fixed 4044f730 | comment band misquoted |
| guess | 2 | Opus | F4 | should | fixed 4044f730 | D5 in-hand 0.70-0.80 numbers added |
| guess | 2 | Opus | F5 | should | fix | contribShown genus branch: confirmed flag inverted |
| guess | 2 | Opus | F6 | nit | fixed 4044f730 | report item 1 softened |
| guess | 2 | Opus | F7 | nit | fix | stale comments |
| guess | 2 | Opus | F8 | nit | declined | grey label names the guess's tribe |
| 3 | narrow: bdb4a98a..428cadfd | 10/10 | 9/10 |

| guess | 3 | Opus | N1 | nit | fixed | narrow_bands.md + script saved, cited in D5 |
| guess | 3 | Opus | N2 | nit | declined, follow-up | test reads Vue internals; works now |
| guess | 3 | Opus | N3 | nit | checked in smoke | Spanish-first for an es-EC visitor |
