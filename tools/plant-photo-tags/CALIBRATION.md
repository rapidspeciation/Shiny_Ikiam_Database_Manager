# Pass bar (31 Oct 2026; written before the calibration photos were looked at, committed with the results; the labels are the assistant's (Claude), not a person's)

Decision the tools make: a photo's view, one of leaves (leaves clearly visible), habit (whole plant / vine / shrub),
flower (flower close-up, leaves absent or marginal), fruit, herbarium, other. The tool prefers leaves|habit for the
first photo of a species and flower for a second.

Calibration: 40 field photos of the Wings Atlas host-plant gallery, drawn at random (seed 7) from the taxa of
Tithorea harmonia's hosts and 10 other random taxa; labelled by eye before the model's tags are looked at.

Pass (per candidate model, zero-shot CLIP prompts):
1. Precision of "leaves or habit" (model) against "leaves or habit" (eye): at least 85 %.
2. Of the photos the eye calls flower close-ups, at most 1 in 10 tagged leaves or habit by the model.
3. Recall of "flower" at least 70 % (a second flower photo is a bonus; a wrong one is only a missed bonus).
A model that fails 1 or 2 is not used; the tools then keep the field/herbarium rule only.
Among passing models, the cheaper (ViT-B/32 over BioCLIP) wins ties within 5 points.

## Results
- Calibration (seed 7), prompts v1: ViT-B/32 precision 87 %, flowers as leaves/habit 2/19, flower recall 53 % (fails 2, 3);
  BioCLIP 2 precision 80 %, flower recall 0 % (fails). Prompts revised once (fruit prompts were taking flowers).
- Prompts v2 on calibration: ViT-B/32 100 % / 0 of 19 / 74 %; ViT-L/14 (laion2b_s32b_b82k) 86 % / 2 of 19 / 89 %.
- Fresh validation (seed 11, 15 other taxa, labelled before scoring), prompts v2: ViT-B/32 83 % / 3 of 14 / 71 % (fails);
  ViT-L/14 top-1 90 % / 2 of 14 / 79 % (fails 2).
- Threshold chosen on calibration only: ViT-L/14 with p(leaves or habit) >= 0.8 (calibration 90 % / 1 of 19 / 89 %).
  Validation: 100 % (17/17) / 0 of 14 / 79 %: passes all three. Used: ViT-L/14, prompts v2, threshold 0.8.

## What the numbers support (pooled, both sets of 40, ViT-L/14, threshold 0.8)
- 26 of 27 photos tagged leaves or habit showed them; 1 of 33 flower close-ups was tagged leaves or habit; 28 of 33
  flowers were tagged flower. The validation set had been seen before the threshold was chosen, so it is not an
  untouched holdout, and 80 photos labelled by one person are a small sample (17/17 has a 95 % lower bound near
  80 %): few errors seen, not a measured rate.
- Since 31 Oct 2026 the tags only pre-sort the candidates of a contact sheet (the nearest leaves-or-plant photo and
  a flower are kept among a plant's tiles); the chat looks at the sheet and picks. Only the 6,620 photos of the
  first part were tagged (all of Tithorea harmonia's hosts); the rest were not, on purpose.
