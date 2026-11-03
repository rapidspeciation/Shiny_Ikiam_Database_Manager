# sol-loop ledger: Identificar tab (3 Nov 2026)

Critics: GPT-6.1 Sol (codex, reasoning medium) and Opus 5.5 (claude-ithomiini, effort medium), fresh per round.
Usage: not returned by the critics (T3 does not expose it); the ithomiini meter is in the final report.

## Round 1: brainstorm on the draft (BRAINSTORM_BRIEF.md)

Grades: Sol B / CHANGES, Opus B / CHANGES.

| critic | id | sev | finding (short) | outcome |
|---|---|---|---|---|
| sol | F1 | blocker | confidence words (muy probable…) unsupported | fixed: "Posible identificación", score small grey + "no una garantía" |
| opus | F3 | blocker | headline from raw softmax; use genus/species pattern | fixed: genus headline when species < 0.8 and genus ≥ 0.8; "No estamos seguros" otherwise. Cut not calibrated: listed as follow-up |
| sol | F2 | blocker | readiness must cover page + runtime, test airplane reload | fixed: ready = models stored and page/runtime cached; checked in the offline browser test |
| opus | F1 | blocker | iPhone: download in Safari invisible to installed app | fixed: iPhone not-installed warning above Descargar (not disabled: a person may only use Safari) |
| opus | F2 | blocker | no "no wings" / "not Ithomiini" states | fixed: amber note on crop fallback; Ithomiini badge / "No parece una Ithomiini" from the workbook's Tribe genera |
| sol | F8 | blocker | model does not identify subspecies; unknown stock ≠ zero | fixed: subspecies rows listed, none picked; unknown side/sex shown apart |
| sol/opus | F3/F9 | should | model wording, sizes, storage check | fixed: "Descarga unos N MB… Ocupa…", estimate() check before download |
| sol/opus | F4/F5 | should | same numbers 3 times; one grouped list | fixed: one genus-grouped list, no separate genus line |
| sol | F5 | should | genus-fallback photos must be labelled | fixed |
| sol/opus | F6 | should | offline photos empty in the field | fixed: "Fotos para el campo" (one per Ithomiini species), measured size |
| sol/opus | F7/F4 | should | Sí/No looks like it teaches the team | fixed: "Se parece a esta" / "No se parece: ver la siguiente", "solo en este teléfono"; local log |
| sol/opus | F9/F8 | should | side of Andes first and strong prior | fixed: photo first, side below, default No sé, re-rank on change |
| sol | F10 | should | failure states | fixed: errors with "Elegir otra foto", no list after a failed run |
| sol/opus | F11/F12 | should | navigation continuity, restore result | fixed: same header, result kept for the session |
| opus | F7 | should | "one switch" is more than a flag for anonymous use | agreed: switch is for signed-in people; anonymous in NO_ACCOUNT_OPTIONS.md |
| opus | F10 | should | one status chip | fixed |
| opus | F11 | should | team panel gap first, rule stated, follows pick | fixed |
| opus | F8 (GPS) | idea | "Usar mi ubicación" | declined for v1 (needs a divide outline); follow-up |
| opus | F13 | nit | waiting text | fixed ("la primera foto tarda más") |
| both | F12/F14 | — | isolated page + same-origin proxy are right | kept |

## Round 2 (UI round 1): the implemented tab (UI_REVIEW_R1.md, shots-r1/)

Grades: Sol C / CHANGES, Opus B+ / CHANGES.

| critic | id | sev | finding (short) | outcome |
|---|---|---|---|---|
| sol | F1 | blocker | "Listo sin internet" from model metadata only; page not cached when models came from /prueba-offline/ | fixed: `engine.pageOffline()` keeps and checks the page and runtime; the status needs it. Checked: models from the old page, then the tab, airplane reload, identification (shot-reuse) |
| sol | F2 | should | a failed second photo left the previous result on screen | fixed: a new photo clears the result first; checked with an unreadable file (shots-r2/e-failed-photo-phone.png) |
| sol | F3 | should | field-photo failures swallowed | fixed: failed count returned and shown with the retry button |
| sol | F4 | nit | "Todas las posibilidades" overpromises | fixed: "Las más parecidas" |
| opus | F1 | should | gap line hides which subspecies is short and the unassigned ones | fixed: subspecies named, "+N sin subespecie anotada"; test updated |
| opus | F2 | should | "no hay fotos guardadas" shown online too | fixed: online says "No encontramos fotos de esta especie." |
| opus | F3 | nit | log keeps old top after a side change | fixed |
| opus | F4 | nit | "Cambiar" wraps alone | fixed (nowrap with the model name) |
| opus | F5 | nit | side choice far from the score | fixed: "Lado de los Andes: No sé · cambiar" under the score |
| opus | F6 | nit | small option never says space taken | fixed ("ocupa unos N MB"); warning keeps the remaining download (correct after a partial download) |
| opus | F7 | nit | desktop left column short | declined: fine for v1 |
| opus | F8 | nit | Ithomiini badge needs the team snapshot | already in NO_ACCOUNT_OPTIONS.md (static genus list) |

## Round 3 (UI round 2): changes since round 1 (UI_REVIEW_R2.md, shots-r2/)

Grades: Sol A / APPROVE (F1-F4 resolved), Opus A- / APPROVE (F1-F6, F8 resolved; F7 declined, agreed).

| critic | id | sev | finding (short) | outcome |
|---|---|---|---|---|
| sol | N1 | nit | "(se cortó la conexión)" assumes the cause | fixed: removed |
| opus | N1 | should | amber "abra esta página…" flashes online while the page is kept | fixed: null while checking, "Preparando para usar sin internet…" muted |
| opus | N2 | nit | opened offline, then signal: amber line stays | fixed: going online runs refresh(), which keeps the page again |
| opus | N3 | nit | "+7 sin subespecie anotada" terse | fixed: "otros 7 del Este sin subespecie anotada; pueden contar" |
| opus | N4 | nit | "cambiar" scrolls but does not move focus | fixed: focuses the selected side |

## Round 4 (UI round 3, last): the fixes after round 2 (UI_REVIEW_R3.md, shots-r3/)

Grades: Sol A / APPROVE (Sol N1, Opus N1-N4 resolved), Opus A / APPROVE (Sol N1, Opus N1-N4 resolved).

| critic | id | sev | finding (short) | outcome |
|---|---|---|---|---|
| opus | M1 | nit | "del Este" repeats the side; the unassigned count includes no-sex specimens | fixed: side dropped; only ♂ and ♀ counted |
| opus | M2 | nit | comma expression in a template handler | fixed: `focusSide()` |
| self | - | nit | "p. m.." double period after the field-photo date | fixed |

Final: both critics approve; no blocker or should open. Final screenshots: shots-final/.

## Grades over the loop

| round | Sol | Opus |
|---|---|---|
| brainstorm (draft design) | B, CHANGES | B, CHANGES |
| UI 1 (implemented tab) | C, CHANGES | B+, CHANGES |
| UI 2 (fixes) | A, APPROVE | A-, APPROVE |
| UI 3 (last fixes) | A, APPROVE | A, APPROVE |
