# Field collections (Collection_data)

One row per capture (a monitoring recapture is a row of its own). Monitoring
walks, marks and recaptures: skill **monitoring**; the insectary side of live
butterflies: [insectary-individuals.md](insectary-individuals.md).

## Record kinds

The kind is decided by `Release_Collect` first, then `Purpose`.

| Kind | Release_Collect | Tell-tale values |
|---|---|---|
| Preserved in the field (trip) | `Collected_Preserved` | Purpose `NA` (or Pheromones, Trapping…), CAM_ID + Tube_1_id, Insectary_ID `NA` |
| Preserved on a monitoring walk | `Collected_Preserved` | Purpose `Monitoring`, place `Ikiam`, FieldMark_ID `NA` |
| Taken alive to the insectary | `Collected_Sent2Insectary` | an Insectary_ID (pre-made, written on the wing), **no CAM_ID**, a twin row in Insectary_data |
| Marked and released | `Mark_Released` | FieldMark_ID (M/A/B series), Purpose `Monitoring`, CAM and tubes `NA` |
| Released unmarked | `Released_Unmarked` | like marked, FieldMark_ID `NA`; rare (skill **monitoring**) |

## Templates (current practice, Ecuador rows since Sep 2025)

### Preserved in the field (local team)

| Column | Value |
|---|---|
| CAM_ID, Tube_1_id | the next local wild CAM; its tube |
| Tube_1_tissue | `WHOLE_ORGANISM` |
| Tube_2–4 | `NA`; their tissues `NOT_COLLECTED` (older rows with `NA` stay) |
| Preservation_medium | `Flash frozen` |
| Preserved_dead_alive | `Alive`; `Dead` when found dead or dying, with a note "Preserved dead ~2h" |
| Death_date = Preservation_date | the collection day (the next day if kept overnight; still `Alive`) |
| Splitted_body | `No` |
| Location_Head, _Torax, _abdomen, _Legs, _wings | `Ikiam` (Location_WholeBody blank) |
| Insectary_ID, CAM_ID_insectary, FieldMark_ID | `NA` |
| Butterfly_weight | in g, 3 decimals; `NA` if not weighed, with the reason in a note |
| Flight_height | `NA` on trips |

### Taken alive

- **At entry**: Release_Collect, Insectary_ID, FieldMark_ID `NA`, SPECIES +
  Subspecies_Form, Identifier, ID_status, Sex (always definite: no `?`),
  Collection_location, Transect `NA`, Bait `NA`, Forest_stratum `NA`,
  Collection_date, Collection_time (or `NA`), Collector, Rainfall,
  Cloud_cover (`NA` when not noted), Flight_height `NA`, CAM_ID `NA`, Purpose
  `NA`. The rest stays **blank** until the butterfly dies in the insectary.
- **Lookup formulas**: Death_date, Preservation_date, Preservation_medium and
  Preserved_dead_alive come from the Insectary_data twin by formula
  (`=XLOOKUP(D…, Insectary_data!A:A, Insectary_data!I:I, "")`): keep them; no
  values are typed there. Rows from Aug 2026 lack these formulas (blank even
  where the twin has died): Revisión → Sugerencias lists them («Fórmulas que
  faltan»), and a proposal fills them (`missingFormulas`). Rows the app
  creates get them.
- **At death, not preserved**: CAM_ID_insectary `NA`, Tube_1_id `NA`, tissues
  `NOT_COLLECTED`, weight, Splitted_body and Location_* `NA`.
- **At death, preserved**: CAM_ID_insectary = the insectary CAM, Tube_1_id =
  the insectary tube, `WHOLE_ORGANISM`, Splitted_body `No`, Location_*
  `Ikiam`; CAM_ID stays `NA`.

### Others

- **Marked and released, released unmarked**: skill **monitoring**.
- **Expedition rows abroad** (Sanger layout: split bodies, DMSO/AllProtect,
  Location "Sanger - TOL704 freezer") are entered by the expedition leads;
  local rows follow the templates above.
- **Pheromone wild males** (Purpose `Pheromones`, all male): weight `NA`,
  their own tube run, the medium as in [samples-ids.md](samples-ids.md).

## Session values

- **Collector / Identifier**: the list value `INI - Full name` (`AA - …`,
  `PAS - …`). Unknown: `NA - Missing data`. `CR` is two people in the list:
  ask which. A new person is added to the list first (PAS). Collector is per
  row (who caught it); Identifier is usually one person per trip; on
  monitoring Identifier = Collector.
- **ID_status**: `COMPLETE` (with Identifier), `Complete_but_verify`,
  `Incomplete_genus_only` / tribe / family, `To_identify` (species blank, no
  Identifier; completed after the photos). Prefer an honest incomplete status
  over a guessed species.
- **Collection_location**: a Location_data value (strict). Notebook initials
  are the location whose initials match among recent collections
  (`C.T.C` / "Cavernas" = Cavernas Templo de Ceremonia). Río Pusuno is the old
  name of Suchipakari; "Mariposario Ikiam" = found inside the insectary garden.
  Near-duplicate names exist ("Apuya Y " with a space, "Apuya 2.7km" / "Apuya km
  2.7"): use the one the recent rows use. A new place needs coordinates: ask.
- **Collection_date** day first, the capture day; `NA` only when truly
  unknown. A live butterfly's Intro2Insectary_date equals it (all 2026 rows),
  even when it was caged a day or two later.
- **Collection_time** `hh:mm` 24 h (trips 09:00–15:15; monitoring 09:00–11:30);
  `NA` when not noted. A time like 02:41 is a PM typed as AM: ask.
- **Rainfall / Cloud_cover**: list values; codes and paper shorthand in the
  skill **monitoring**. Rain is per day, cloud per row.
- **Flight_height** in metres with a decimal point (0–3 typical); >5 is cm.

## Species and sex

- `SPECIES` = a binomial of Taxonomy_v18Jun25; a name not there shows
  `NOT_FOUND` in Family. A butterfly known only to family ("Riodinidae") gets
  ID_status `To_identify` / `Incomplete_*`, not the family as SPECIES.
- `Subspecies_Form` free text in the local vocabulary: deceptus, intermedia,
  messenoides; proceriformis, eurydice; salapia, derasa; janarilla; zaneka,
  menophilus; ida; matronalis; ecuadorina; lota; psamathe; tigilla…;
  `(No subspecies described)` for monotypic species. No trailing spaces;
  `messnoides` → messenoides.
- `Sex`: `female ?` / `male ?` only on preserved rows. Sexed by genitalia
  (Methona, Oleria tigilla…) → note "Sexed by genitalia".
- Insectary_data writes the trinomial ("Mechanitis messenoides deceptus" =
  SPECIES + Subspecies_Form) with `female`/`male`/`NA`.

## IDs on a trip

- CAMs: the **local** wild block (not the expeditions' block), next = last
  local + 1 in row order; tubes +1 in the same order. Rows are typed grouped by
  species, then fate, so CAM order is not capture-time order.
- Tubes: no size rule; take the tube on the label ([samples-ids.md](samples-ids.md)).
- Live butterflies get the next free pre-made Insectary_IDs, consecutive
  within a trip (reserved for the field team after that day's emergences).
- Details and pools: [samples-ids.md](samples-ids.md).

## Formula columns

Taxonomy (Family, Subfamily, Tribe, Genus), place, photo, rack, manifest and
Sanger (STS) columns are formulas, and Data_entry_order is protected. A row,
new or existing, leaves them to their formulas: the proposal shows what each
one will give, and when that is the value wanted nothing is typed there; a
value is typed over a formula only when the formula gives something else,
and that is said. The manifest columns are kept by PAS. A Death/Preservation
lookup keyed on an Insectary_ID `NA` or blank pulls another butterfly's data:
point it out; those values are not this butterfly's.

## Wild-caught twins

A butterfly taken alive has **two rows with the same Insectary_ID**: this one
and Insectary_data (`Wild-caught`, CLUTCH NUMBER `NA`, Stock_of_origin `NA`,
species, sex, Intro2Insectary_date = Collection_date). Propose both in the same
proposal; a correction of species or sex goes to both. Entry is often split:
whoever preserves types CAM and tubes, whoever holds the field envelope types
collector, place, time and weather: a half-complete row waits for the envelope
(say whose).
