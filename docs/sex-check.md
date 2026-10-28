# The sex check in the «Fotos» tab

Before a batch of wing photos is uploaded to Drive, «Comprobar N mariposas» (above the Rename Photos frame) checks
each butterfly that has a `d` and a `v` photo in the batch. Three sexes are compared:

| source | where it comes from | how sure |
|---|---|---|
| **model** | the collection sex model of 19 Oct 2026 (a small CNN on the 4 androconial strips, cut with the nano wing chain's boxes; `identifier/sex/`, the same model and fold split as the readings) | a **Supported** call (the released screen: P ≥ 0.85, the species has at least 3 sure out-of-fold butterflies of each sex, and the Wilson lower bound of the model's accuracy on that species is ≥ 0.80) is right about 98.8 % of the time (50 % of butterflies get one). Any other call is only a leaning. |
| **envelope** | the envelope reader's full text of both photos (`photo-reader --text`), read into fields by `server/envelope-fields.mjs` (♂/♀, male/female, macho/hembra) | the reader sometimes writes a ♂/♀ that is not there (♂/♀ exact on 93.1 % of envelopes). It counts only at confidence ≥ 0.6 and when the envelope does not read both signs; a lone sign with no species line is already counted at half its confidence. |
| **database** | the `Sex` of the CAM's rows in Collection_data and Insectary_data | `male`/`female` (a trailing `?` is ignored); anything else is empty. |

## The flags (`decideSex` in `server/sex-check.mjs`)

In this order:

1. The database's rows have different sexes: **check** (amber).
2. The database has a sex:
   - a Supported model call against it: **disagree** (red, the likely errors). The line says whether the envelope
     agrees with the model ("the database is probably wrong") or with the database ("look at the wings").
   - a Supported model call that is the database's: **agree**. If the envelope reads the other sign, the line says
     the reader probably misread it; it is not flagged.
   - no Supported call, and the envelope (sure, as above) against the database: **check** (amber). This is weaker
     evidence, because the reader can invent a sign.
   - otherwise: **uncertain** (grey). The model's leaning is shown, never as an error.
3. The database has no sex: **check** with the sex to fill in when a Supported model call or a sure envelope gives
   one (both, if they disagree, are shown); otherwise **uncertain**, or **none** when nothing reads a sex.

The panel lists the red and amber ones first ("ver todas" shows the rest) and counts each kind.

### Species the model is not trusted on

`server/sex-model-unreliable.json` lists species whose model calls never count as sure, whatever the screen says.
Their call is shown as "unreliable for this species" (the reason on hover): it never makes a **disagree**, never an
**agree**, and never suggests a sex for an empty database. The envelope against the database still counts as above.
The species is the database's (genus and species), else the envelope's.

- **Methona confusa**: training labels before 15 May 2023 were all set to male (females also have the dorsal
  hindwing hair-pencils, Hill & Tipan 2008), and the model finds only 2 of 6 recorded females
  (WingsClassificator `reports/sex_label_review_20261027`, commit 7a8ca24c).

To add a species: one entry under `species` with `why` and `source` (a test checks both are there).

## How it runs

- The photos stay on the person's computer. For each butterfly the frame gives the page its `d` and `v` photo as it
  will be once turned, at 1600 px (the size of Drive's copies, which the model's readings use), and the page sends
  them to `POST /api/sex-check`. Nothing is kept on the server once read.
- The server reads one butterfly at a time, in two child processes at nice 10 that end after 5 minutes without work:
  the readings worker's `only: 'sex'` job (nano wing chain, then the Python worker's op `sex`: the CNN alone, without
  the image encoder or torch) and the photo reader with `--text` (envelope). About 0.5 to 0.8 s a butterfly on the
  PC once started; the first one also starts both processes (about 2 s).
- It needs what the background readings need: the photo reader with the wing chain (`scripts/build-rename-app.sh`)
  and the identifier with its `sex/` model (`SEX_SRC=… bash tools/identifier/setup.sh`). `GET /api/sex-check` says
  whether it can run.
- A butterfly whose wings the chain cannot read (no wing found, a photo the chain had to turn) is "uncertain: the
  model could not read the wings".

## Demo

`tools/sex-check/demo.mjs` runs the same code on photo sets already on Drive against a copy of the database (see
its header). On the lab's copy (19 Oct), nine butterflies:

```
CAM077514  Mechanitis polymnia  model ♂ 1 Supported       envelope ·        database ♀  -> disagree
CAM070881  Hyposcada illinissa  model ♀ 1 Supported       envelope ·        database ♂  -> disagree
CAM077388  Hypothyris euclea    model ♀ 0.999 Supported   envelope ♀ 0.958  database ♂  -> disagree, envelope agrees with model
CAM071443  Oleria baizana       model ♀ 1 Supported       envelope ♀ 1      database ♂  -> disagree, envelope agrees with model
CAM071511  Oleria baizana       model ♂ 1 Supported       envelope ♂ 1      database ♂  -> agree
CAM074960  Hypothyris euclea    model ♀ 0.999 Supported   envelope ♀ 0.902  database ♀  -> agree
CAM074752  Pteronymia alina     model ♀ 1 Supported       envelope ♀ 1      database ♀  -> agree
CAM071393  Oleria santineza     model ♂ 0.985 (not Supp.) envelope ♂ 1      database ♂  -> uncertain
CAM072592  Nessaea hewitsonii   model ♂ 0.891 (not Supp.) envelope ·        database ♂  -> uncertain
```

The first four are from the sex model's list of label candidates (`release_20261019_sexcnn/label_review_candidates.csv`,
Supported calls against the recorded sex). None of them has been checked by a person yet.

The same nine at 1100 px (what a frame without the `side` option sends) gave the same flags, but the model's
probabilities moved (CAM074960: P(male) 0.001 at 1600 px, 0.14 at 1100 px), and the envelope reader found a lone ♀/♂
on CAM077514 and CAM070881 at 1100 px that it missed at 1600 px (the line finder did not see the sign). Not yet
measured on more butterflies.
