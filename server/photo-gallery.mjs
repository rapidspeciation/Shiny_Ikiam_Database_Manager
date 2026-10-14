// show_photos: the photos of some butterflies, by their CAM ID, beside the chat (a card of Cambios
// propuestos, `sync_json` of type 'gallery'), each with the note the assistant gives: the IDs whose
// photos have to be taken again, odd cases to look at. The photos are those Photo_links lists, shown
// through the app's cached copies (server/photos.mjs); a view the sheet lacks is said so.

import { driveIdOf, knownId, photoLinkRows, specimenName, specimens } from './photo-links.mjs';

export const GALLERY_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'show_photos',
      description: [
        "Shows butterflies' wing photos beside the chat, by CAM ID: each ID with its dorsal and ventral photos side by side (as Photo_links lists them) and a note; a view that is not listed is said so. For the IDs whose photos have to be taken again, photos to compare, or odd cases the person should see.",
        '- `ids`: the CAM IDs (up to 120). `notes`: { "<CAM_ID>": "why it is shown" }. `title`: what the list is.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          ids: { type: 'array', items: { type: 'string' } },
          notes: { type: 'object', additionalProperties: { type: 'string' } },
          title: { type: 'string' },
        },
        required: ['ids'],
      },
    },
  },
];
export const GALLERY_TOOL_NAMES = new Set(GALLERY_TOOLS.map(t => t.function.name));

const MAX = 120;
const PHOTO = /\.(jpe?g|png|heic)$/i;

/** The listed photos of these IDs: [{ id, note, photos: [{ view, name, file, url, folder }], missing: [views] }]. */
export function galleryOf(store, ids, notes = {}) {
  const known = new Set(specimens(store).keys());
  const wanted = new Map();
  for (const raw of ids.slice(0, MAX)) {
    const id = String(raw ?? '').trim().toUpperCase().replace(/\s+/g, '');
    if (id && !wanted.has(id)) wanted.set(id, { id, note: String(notes?.[raw] ?? notes?.[id] ?? '').slice(0, 300) || undefined, photos: [], inWorkbook: known.has(id) });
  }
  for (const r of photoLinkRows(store)) {
    if (r.values.Type !== 'File' || !PHOTO.test(String(r.values.Name ?? ''))) continue;
    const parsed = specimenName(r.values.Name);
    if (!parsed) continue;
    const id = knownId(parsed.id, new Set(wanted.keys()));
    const entry = id && wanted.get(id);
    if (!entry) continue;
    const file = driveIdOf(r.values.URL);
    if (file) entry.photos.push({ view: `${parsed.view}${parsed.extra}`, name: r.values.Name, file, url: r.values.URL, folder: String(r.values['Full Path'] ?? '').split('/').slice(0, -1).join('/') });
  }
  return [...wanted.values()].map(e => {
    e.photos.sort((a, b) => a.view.localeCompare(b.view));
    const views = new Set(e.photos.map(p => p.view[0]));
    return { ...e, missing: ['d', 'v'].filter(v => !views.has(v)) };
  });
}

/** show_photos for one app: `show(card, context)` keeps the card and answers its link. */
export function createGallery({ store, show }) {
  return {
    run(args = {}, user, context) {
      const ids = Array.isArray(args.ids) ? args.ids : [];
      if (!ids.length) return { error: 'ids: the CAM IDs to show' };
      const items = galleryOf(store, ids, args.notes ?? {});
      const title = String(args.title ?? '').trim().slice(0, 120) || `${items.length} butterflies`;
      const shown = show({ type: 'gallery', title, items }, context);
      return {
        shown: items.length,
        ...shown,
        withoutPhotos: items.filter(i => !i.photos.length).map(i => i.id),
        missingAView: items.filter(i => i.photos.length && i.missing.length).map(i => `${i.id} (${i.missing.join(', ')})`),
        notInWorkbook: items.filter(i => !i.inWorkbook).map(i => i.id),
      };
    },
  };
}
