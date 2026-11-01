// The Wings Atlas host-plant gallery: every item of every taxon's pages. node inventory.mjs <outDir>
import { writeFileSync } from 'node:fs';
const out = process.argv[2];
const BASE = 'https://rapidspeciation.github.io/ithomiini_maps/data/host_plants/';
const get = async p => { for (let i = 0; i < 4; i++) { try { const r = await fetch(BASE + p); if (r.ok) return r.json(); } catch {} await new Promise(r => setTimeout(r, 1000 * (i + 1))); } throw new Error(p); };
const m = await get('host_plant_layers_manifest.json');
const galleries = m.metadata.gallery_indexes_by_taxon;
const pages = [];
const slugs = Object.keys(galleries);
for (let i = 0; i < slugs.length; i += 16) {
  await Promise.all(slugs.slice(i, i + 16).map(async slug => { const idx = await get(galleries[slug]); for (const p of idx.pages ?? []) pages.push({ slug, p }); }));
}
console.log('taxa', slugs.length, 'pages', pages.length);
const items = [];
for (let i = 0; i < pages.length; i += 24) {
  await Promise.all(pages.slice(i, i + 24).map(async ({ slug, p }) => {
    const page = await get(p);
    for (const it of page.items ?? []) items.push({ slug, gbif: String(it.gbifID ?? it.id ?? ''), img: it.image_url, ctx: it.photo_context, basis: it.basisOfRecord, fallback: it.image_is_fallback === true, source: it.source });
  }));
}
writeFileSync(`${out}/items.json`, JSON.stringify({ generated_at: m.metadata.generated_at, items }));
const c = {};
for (const it of items) c[`${it.ctx}|${it.basis}`] = (c[`${it.ctx}|${it.basis}`] ?? 0) + 1;
console.log('items', items.length, 'records', new Set(items.map(i => i.gbif)).size, JSON.stringify(c));
