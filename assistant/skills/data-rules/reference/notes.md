# Notes columns (Notes_Insectary_data, Notes_Collection_data, NOTES, Notes)

## Format

- `d/m/yy INI: text`: the day the note is **written** (today) and the initials
  of the person you work for (`29/9/26 FCH: Wing clip 27/9/26`). The event's
  date goes inside the text ("preserved 14/9", "Wing clip d/m/yy"). This short
  form is the preferred one and the most used (2026: nearly all insectary and
  stocks notes; `21May26 PAS` is PAS's and KG's style in Collection_data).
  Each app user has their own initials (the tools use them); collectors'
  initials are the Lists `Abbreviation` column.
- Several notes in one cell are joined with ` | `, newest last; an existing
  note is never overwritten. The tools add the prefix and the joiner: give
  only the new text (`{"replace": …}` only when the person asks to rewrite).
- Other people's older notes use their own styles (`13-12-24 MJS:`,
  `8 OCT 24 KG:`, `21May26 PAS …`, `16/9/2026 AA:`): leave them as they are.
- **English.** The team types notes in English (99.8 % of insectary notes;
  field and stocks notes too), translating the page faithfully ("3 pupas
  muertas" → "3 pupae dead", "parece que están enfermas" → "larvae look sick"),
  keeping IDs, codes, names and places as written. Always English, even when
  the page is in Spanish; the owner note is "Butterflies of Oda/Esteban" (old
  rows' "mariposas de …" stay).

## Must be noted

- A change of species, subspecies, sex, ID_status or an ID/CAM/tube: "from X
  to Y" (and who verified: an expert who only confirms is credited in the
  note; one who corrects becomes the Identifier).
- Why a sample is in ethanol instead of flash frozen; "Preserved dead ~2h",
  "Preserved while dying".
- A wing clip's date (no column): `Wing clip d/m/yy`.
- An identification made from non-wing characters (abdominal lines, antennal
  clubs), with a link to the photo in Drive "Supplementary_images_for_Notes"
  named by CAM.
- "Sexed by genitalia"; why the weight is missing (scale away, battery).
- A mixed clutch (e.g. polymnia + deceptus larvae), eggs found outside, where
  field eggs/larvae came from.
- A cross parent not used, a mating partner ("Mating with 3ZQ", "Used for
  production of F1, mating with U8A female").
- Marks on the abdomen dissolved by ethanol; sample incidents per sample
  (a freezer or shipper thaw with hours and temperature).
- Collaborator sample codes in parentheses after the CAM ("CAM074922
  (TP-24-02)").
- "Selected for REFERENCE GENOME".

## Never in notes

- A value that has its own column: death cause, medium ("ethanol", "flash
  frozen"), tube or CAM codes, research purpose ("pheromone"), a wild
  butterfly's collector, time, weather and place (they go in its
  Collection_data row), the generation, the room code (`ins/este`).
- Your own assumptions or doubts ("fecha supuesta", "29/6?", "~2/7"): those go
  in your reply to the person.
- A restatement of what the row already says, or of the existing note in
  another language.
- "Monitoring Wikiloc ID: Mn" unless the person asks for it (the walk's point
  number, not a mark).

## Standard phrases (keep the team's wording)

Insectary_data: "Only found wings", "Body eaten", "Deformed wings, can not
fly", "Emerged incomplete", "Preserved in ultrafridge at -80ºC", "Preserved in
dryshipper", "F2 preserved for pheromons", "With white flower" / "Without white
flower" (pheromone treatment; paper CFB / SFB), "Larvae 4th instar",
"prepupae", "Larvae founded dead, first instar", "Comes from CRISPR control
#n", "Eggs found outside insectary on d/m/yy", "marked with lines in the
abdomen", "hair pencils cut", "emerged in cage of parents".

Insectary_stocks: see [clutches.md](clutches.md) ("Some eggs with fungi",
"no hatch", "female dead → clutch to stock", "U8A♀ + C8B♂").

Collection_data: "Preserved dead ~2h", "Sexed by genitalia", "Recapture",
"The scale's battery ran out, so the individual could not be weighed",
"Butterfly sent to insectary for live photos", a trap point ("Trap: A7_S").

SamplingDay_data: rain, fallen trees, people on the trail, "No butterflies
collected".
