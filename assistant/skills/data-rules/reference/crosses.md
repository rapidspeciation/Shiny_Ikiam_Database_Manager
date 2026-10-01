# Crosses, families, pedigree

Protocols in Drive (answer "what does the protocol say" with `search_knowledge`
and cite the document and date): "Protocol for Controlled Crosses and Families"
(M. polymnia; M. menophilus; polymnia × lysimnia F1, revised Jul–Aug 2026), the
2023 lys/pol crosses protocol, the F2 hybrid tissue protocol (2024).

## Notation

- The **female is written first**, always: cage cards, clutch notes
  (`U8A♀ + C8B♂`), hybrid names.
- Hybrid (F1) name = mother's subspecies `x` father's subspecies:
  `Mechanitis polymnia werneri x proceriformis` ("wer x pro" = ♀ werneri, west,
  × ♂ proceriformis, east; "pro x wer" is the reverse direction, since Oct 2024).
- A cross between a stock and a hybrid joins the parents with `VS` (Insectary
  species list: `Melinaea menophilus zaneka VS Melinaea menophilus zaneka x
  menophilus`); slides write a capital `X`. Use the exact list value; if the
  combination is not in the list, ask (PAS adds list values).
- Paper forms: `werxpro`, `werpro`, `proxwer`, `pol wer x procerifor`,
  `hibrido x hibrido`, `zaneka x hibrido`, a pedigree string
  `(zaneka) x (zaneka x menophilus)`.
- M. polymnia werneri was renamed chimborazona, but the insectary list (Lists
  `Insectary_species`) still has only the werneri forms: insectary sheets use
  the list value (`Mechanitis polymnia werneri x proceriformis`). Field rows'
  free-text Subspecies_Form already say `chimborazona`. Renaming the list is
  PAS's. Whether `X` or `VS` is right in a backcross name: **Ask** (PAS); use
  the list value meanwhile.

## Protocol steps the data should reflect

1. Pair: a virgin reared female from stock and (usually) a wild male. The
   **start date** is the day they are caged together; checks every hour
   8:00–16:00; a pair stays at most 2 weeks (Mtg Oct 2025).
2. Male: wing clip + CAM right after mating (flash frozen). Female: clip after
   she starts laying. Both parents: Research_purpose `F1/F2 mutation rate`,
   Pedigree `Yes` (a leftover `YES or NO` is the unfilled template).
3. No eggs in 1–2 weeks: the female goes back to her **species stock (not the
   virgin stock)**; the male may be reused in a new attempt with its own start
   date, and the old attempt is written "descartado". A parent not used: note
   it (sheet and notebook) and change his Pedigree.
4. All eggs of one mating = one clutch number, batches `N(k)`
   ([clutches.md](clutches.md)); parents in the clutch NOTES, female first;
   Generation F1/F2/Backcross.
5. A male with no tissue preserved: his F1s are not used for sampling (their
   purpose changes).
6. Offspring: F1 larvae are preserved at the 4th instar per the protocol;
   eggs and younger larvae that look about to die are preserved early (the
   eggs and 3rd instars of Sep 2026). Each gets an Insectary_ID and `LIFESTAGE`
   ([insectary-individuals.md](insectary-individuals.md)). Dead 1st–2nd
   instar larvae of families are preserved as well (dead and live siblings
   are wanted), tied to clutch, stage and date.
7. A parent that dies: preserve whatever remains (wings at least). Found dead
   → medium per the rules in [samples-ids.md](samples-ids.md).

## Where the data goes

- **Insectary_data**: each parent and offspring (clip, CAM, tubes, purpose).
- **Insectary_stocks**: the clutch, with parents in NOTES.
- **F1_F2_MutationRate**: one row per family/offspring with grandparents,
  parents and clutches (many formulas from Insectary_data). Its last rows are
  from Sep 2025: the 2026 F1 crosses are added when the crosses notebook is
  typed (until then they are only in stocks notes).
- **Melinaea_crosses** (pairs: start/finish, reason, eggs, hatch) and
  **Melinaea_eggs** (preserved eggs per clutch with mother, father, tube and a
  reason: fungus, shrivelled, no hatch) for Melinaea only.
- Stocks_Matings, Hybrid_Attempts, Crosses_Lys_x_Pol: dormant; don't write.

## Mating-table conventions

- Start = day caged together; Finish = the last day the pair (or the female
  alone) stayed. Unknown or not observed = `NA`; `-` for the grandparents of
  wild or pure-stock parents. PAS noted that `NA` in mating columns is
  ambiguous (no mating vs not seen): say which when you know.
- Reasons for ending: "female dead", "male dead", "both dead", "no mating
  occur - male dead", time limit. Female and male death dates are separate.
- The status of a parent in the 2026 tables: "wc (still alive)", "whole body",
  "Not preserved", "wc(disappear)", or a CAM.

## Reading cage cards and the crosses notebook

- Cage card: direction (`zaneka x menophilus`), date, `7RO♂ + 4RS♀`,
  `Start:` / `end:`, `WC □` (wing clip done when ticked), "Cuarentena",
  "descartado". Colours mark later updates. Stock-cage cards list IDs per
  batch with their entry date.
- Crosses notebook block: ♀ intro date, ♀ ID, ♀ last date, free text. "♂
  founded by spider 15/May CAM… FS…" = the male was found dead (Spider) and
  preserved with that CAM and tube: propose the death for the male's row and
  ask for his ID if not written (take it from the cage card or envelope).
- When the sheet lags, the death, CAM and tube of cross animals may exist only
  in this notebook.

## Decided

- Eggs: as many as possible. The mother is preserved alive (flash frozen) when
  she stops laying or seems about to die, before she is eaten or disappears
  (the protocols' thresholds, > 10 to > 60 eggs, are not a cut-off).
- Media: flash frozen is preferred (F2 families included) unless a note says
  why not.

## Protocols disagree (ask, citing both)

Dead mated male's body: ethanol + wings in an envelope (polymnia, lys/pol) vs
flash frozen (menophilus): **Ask** AA · female clip timing (right after
mating, Jan 2024, vs after laying, all later protocols: follow the later) · a
non-laying female: back to stock vs 15-day quarantine · minimum F2 family
(> 40 vs ≥ 50). Female and male maturity ages are protocol advice, not data
fields.
