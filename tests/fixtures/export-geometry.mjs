// Picture geometry for hand-made and fake app answers (tests/maps-render.test.mjs, fake-playwright.mjs), as the maps
// app's window.ithomiiniMapsExport() returns it (ithomiini_maps-uioverhaul docs/field-sites.md): view.overlays,
// records[].x/y/in_picture and sites[].lng/lat/x/y/radius/label_boxes/hidden_labels/occluded/occluded_by.
import { projectPoint } from '../../server/maps-render.mjs';

/** Overlays as the app drew them for the original auto frame (legend moved to the bottom right). CSS px of the canvas. */
export const OVERLAYS = Object.freeze([
  { kind: 'title', x: 0, y: 0, width: 760, height: 31 },
  { kind: 'legend', x: 542, y: 360, width: 208, height: 151 },
  { kind: 'scale', x: 624.83, y: 521, width: 125.17, height: 25 },
  { kind: 'attribution', x: 548.31, y: 550, width: 211.69, height: 20 },
]);
/** The legend in its default bottom-left corner. */
export const LEGEND_BOTTOM_LEFT = Object.freeze({ kind: 'legend', x: 10, y: 409, width: 208, height: 151 });

const overlap = (a, b) => a.x < b.x + b.width && b.x < a.x + a.width && a.y < b.y + b.height && b.y < a.y + a.height;
const touches = ({ x, y, radius }, r) => {
  const dx = x - Math.max(r.x, Math.min(x, r.x + r.width));
  const dy = y - Math.max(r.y, Math.min(y, r.y + r.height));
  return dx * dx + dy * dy <= radius * radius;
};

/**
 * `result` with the geometry the app would report for it: each marker at its first member record, `radius` px, its
 * label (if any) in a 120 x 22 box just above it; records projected, in_picture as the app defines it.
 */
export function withGeometry(result, { overlays = OVERLAYS, radius = 10 } = {}) {
  const view = { ...result.view, overlays: overlays.map(o => ({ ...o })) };
  const frame = view.frame;
  const recordOf = new Map(result.records.map(r => [`${r.source}|${r.id}`, r]));
  const owner = new Map();
  const sites = result.sites.map(site => {
    const first = recordOf.get(site.members?.[0]?.record_ids?.[0]);
    const lng = site.lng ?? first?.lng ?? 0;
    const lat = site.lat ?? first?.lat ?? 0;
    const { x, y } = projectPoint([lng, lat], view);
    const text = site.label ?? null;
    const label_boxes = text ? [{ x: x - 60, y: y - radius - 26, width: 120, height: 22 }] : [];
    const covered = label_boxes.filter(b => view.overlays.some(o => overlap(b, o)));
    const clear = label_boxes.filter(b => !covered.includes(b) && b.x >= 0 && b.y >= 0 && b.x + b.width <= frame.width && b.y + b.height <= frame.height);
    const occluded_by = [...new Set(view.overlays.filter(o => touches({ x, y, radius }, o) || covered.some(b => overlap(b, o))).map(o => o.kind))];
    const out = { ...site, lng, lat, label: clear.length ? text : null, labels: clear.length ? [text] : [], x, y, radius, label_boxes, hidden_labels: label_boxes.length - clear.length, occluded: occluded_by.length > 0, occluded_by };
    for (const m of site.members ?? []) for (const id of m.record_ids ?? []) owner.set(id, out);
    return out;
  });
  const records = result.records.map(r => {
    const { x, y } = projectPoint([Number(r.lng), Number(r.lat)], view);
    const site = owner.get(`${r.source}|${r.id}`);
    const in_picture = x >= 0 && x <= frame.width && y >= 0 && y <= frame.height && !view.overlays.some(o => x >= o.x && x <= o.x + o.width && y >= o.y && y <= o.y + o.height) && !!site && !site.occluded;
    return { ...r, x, y, in_picture };
  });
  const hidden = sites.filter(site => site.occluded);
  view.legend_placement = { corner: 'bottom-right', placements_tried: 2, occluded_sites: hidden.length, occluded_records: hidden.reduce((t, site) => t + (site.members ?? []).reduce((n, m) => n + (m.record_ids?.length ?? 0), 0), 0) };
  return { ...result, title_truncated: false, view, records, sites, capture_attempts: 1 };
}
