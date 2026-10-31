# Plant photo tags

What each photo of the Wings Atlas host-plant gallery shows (leaves, habit = the whole plant, flower, fruit,
herbarium, other), so `host_plants` with `send_photos` can send a plant's leaves or whole plant first and a flower
second (server/maps-tools.mjs `photosToSend`). Made once, offline, and kept in the tools' cache folder
(`<photo cache>/maps-cache/plant-photo-tags-v1.json`; production: `/home/ubuntu/ithomiini/shared/photo-cache/maps-cache/`).
Without the file the tools still put living plants before herbarium sheets.

1. `node inventory.mjs <dir>`: every gallery item of the 288 taxa (`items.json`; 31 Oct 2026: 87,900 items,
   65,326 field photos, 57,634 distinct iNaturalist photos).
2. A list `[{key: iNaturalist photo number, url: …/small.jpg}]` of the field photos (not `image_is_fallback`).
3. `python classify.py vitl14 <list.json> <tags.json>`: open_clip ViT-L-14 (laion2b_s32b_b82k), zero-shot with the
   prompts in the file, 240 px photos; about 8 photos/s on this PC's Radeon iGPU (ROCm), about 2 h for all.
4. `node merge.mjs <dir> <out.json>` → the tools' file `{photos: {number: [view, p(leaves or habit), p(flower)]}}`.
   A photo counts as leaves or habit when that probability is at least 0.8 (`TAG_LEAVES`).

Calibration (`CALIBRATION.md`): the pass bar was written before looking; 40 photos labelled by eye chose the prompts
and the threshold; a fresh 40 (`validation-*.json`) checked them: 17 of 17 photos called leaves or habit were, no
flower close-up was called leaves, 79 % of flowers were called flower. `score.mjs` scores a tags file against labels.
