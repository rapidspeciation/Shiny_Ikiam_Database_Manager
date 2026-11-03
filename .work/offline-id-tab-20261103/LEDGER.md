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
