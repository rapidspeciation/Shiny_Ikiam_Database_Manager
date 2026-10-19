#!/usr/bin/env node
// The reference photos of reference_photos (server/reference-photos.mjs): field and specimen photos of
// the team's species from butterfly sites of Ecuador, scraped on Franz's computer (ithomiini_maps'
// specimen classifier: sangay.eu, noreste.eu, cotacachi.eu, loja-zamora.eu; amazonian-butterflies.net
// and neotropicalbutterflies.com where those lack a species). Up to 6 field and 4 specimen photos a
// species, spread over the sites, sexes and sides, each 640 px, with index.json saying what each is.
//
//   node scripts/reference-photos.mjs build [--out /tmp/reference-photos-build] [--db <app.sqlite>]
//        [--maps <ithomiini_maps dir>] [--inventory <inventory.jsonl>] [--sharp <photo-reader dir>]
//   node scripts/reference-photos.mjs upload [--from /tmp/reference-photos-build] [--host claudeclaw]
//        [--to /home/ubuntu/ithomiini/shared/reference-photos]
//
// The species: those the workbook names (Collection_data, Insectary_data, CRISPR), those the Wings
// Gallery's model gives the archive's photos (photo_predictions), and the Ithomiini of the taxonomy
// sheet, read from a copy of the app's database (--db: the lab's by default). The photo types come
// from the sites' own sections (the wing models' inventory, WingsClassificator wingseg_v5_field:
// field = live, specimen = specimen or variation; maps, immatures and drawings left out), the same
// photo on two sites is taken once. The server reads the set from REFERENCE_PHOTOS_DIR
// (reference-photos/ beside its database); `upload` copies it there with rsync.

import { spawnSync } from 'node:child_process';
import { createReadStream, existsSync, mkdirSync, readdirSync, rmSync, statSync, writeFileSync } from 'node:fs';
import { createRequire } from 'node:module';
import { homedir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { createInterface } from 'node:readline';
import { DatabaseSync } from 'node:sqlite';
import { fileURLToPath } from 'node:url';
import { speciesOf, sexOf } from '../server/reference-photos.mjs';

const here = dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
const option = (name, fallback) => {
  const i = args.indexOf(name);
  return i >= 0 ? args[i + 1] : fallback;
};
const command = args[0];

const FIELD_MAX = 6;
const SPECIMEN_MAX = 4;
const SIDE = 640;
const MIN_SIDE = 220;
const SOURCES = {
  sangay: { manifest: 'papillons_eu/sangay', ecuador: true },
  noreste: { manifest: 'papillons_eu/noreste', ecuador: true },
  cotacachi: { manifest: 'papillons_eu/cotacachi', ecuador: true },
  'loja-zamora': { manifest: 'papillons_eu/loja-zamora', ecuador: true },
  'amazonian-butterflies': { manifest: 'latam/amazonian', ecuador: false },
  neotropicalbutterflies: { manifest: 'latam/neotropicalbutterflies', ecuador: false },
};
/** A genus and its species (not "Haeterini tribe", "Riodinidae family"). */
const BINOMIAL = /^[A-Z][a-z]+ (?!tribe$|family$|subfamily$|genus$)[a-z][a-z-]+$/;
const MODEL_MIN = 0.05;
const KIND = { live: 'field', specimen: 'specimen', variation: 'specimen' };
const MAP = /mainmap|\bcarte[_-]|distribution|^_\d+_\d+\./i;
const GROUP = { obs_live: 'live', live: 'live', obs_specimen: 'specimen', specimen: 'specimen', variation: 'variation' };

const lines = async path => {
  const out = [];
  for await (const line of createInterface({ input: createReadStream(path) })) if (line.trim()) out.push(JSON.parse(line));
  return out;
};
const words = s =>
  String(s ?? '')
    .replace(/[_]+/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
const title = s => words(s).replace(/(^|[\s-])(\p{L})/gu, (m, a, b) => a + b.toUpperCase());

/** The species the set is made for: { binomial: why }. */
function vocabulary(dbPath) {
  const db = new DatabaseSync(dbPath, { readOnly: true });
  const out = new Map();
  const add = (name, why) => {
    const s = speciesOf(name);
    if (s && BINOMIAL.test(s.binomial) && !out.has(s.binomial)) out.set(s.binomial, why);
  };
  for (const r of db
    .prepare("SELECT json_extract(values_json, '$.SPECIES') s FROM records WHERE sheet IN ('Collection_data','Insectary_data','CRISPR') AND missing=0 AND row_num>1")
    .all())
    add(r.s, 'workbook');
  try {
    // The model's candidates for the archive's photos, those it gives at least MODEL_MIN.
    for (const r of db.prepare('SELECT species_json FROM photo_predictions').all())
      for (const [name, p] of JSON.parse(r.species_json ?? '[]')) if (p >= MODEL_MIN) add(name, 'model');
  } catch {
    // a database without the archive's predictions
  }
  for (const r of db
    .prepare("SELECT values_json v FROM records WHERE sheet LIKE 'Taxonomy%' AND missing=0 AND json_extract(values_json, '$.taxon_rank')='species' AND json_extract(values_json, '$.tribe')='Ithomiini'")
    .all())
    add(JSON.parse(r.v).species, 'taxonomy');
  db.close();
  return out;
}

/** Sex from a specimen photo's file name ("…_femelle_33mm…"). */
const sexInName = name => (/(?:^|[_.\-\s])(?:femelle|female|hembra|fem)(?:[_.\-\s]|$)/i.test(name) ? 'female' : /(?:^|[_.\-\s])(?:male|m[aâ]le|macho)(?:[_.\-\s]|$)/i.test(name) ? 'male' : null);

/** Every usable photo of the sources: { species, subspecies, type, source, path, sha, sex, view, place, page }. */
async function candidates(maps, inventoryPath) {
  const images = join(maps, 'data/specimen_classifier/images');
  const meta = new Map();
  for (const [source, s] of Object.entries(SOURCES)) {
    const path = join(maps, 'data/specimen_classifier/scrape', `${s.manifest}_image_manifest.jsonl`);
    if (!existsSync(path)) continue;
    for (const r of await lines(path)) {
      const local = r.local_path?.startsWith('/') ? r.local_path : join(maps, r.local_path ?? '');
      meta.set(local, { ...r, source });
    }
  }
  // The photo types and checksums of the inventory; without it, the sites' sections and the file names.
  const inventory = existsSync(inventoryPath) ? (await lines(inventoryPath)).filter(r => SOURCES[r.source]) : null;
  const rows = inventory
    ? inventory.map(r => ({ path: r.path, kind: r.kind, sha: r.sha256, source: r.source }))
    : [...meta.entries()].map(([path, m]) => ({ path, kind: MAP.test(path.split('/').at(-1)) ? 'map' : (GROUP[m.image_group] ?? 'other'), sha: null, source: m.source }));
  const out = [];
  for (const r of rows) {
    const type = KIND[r.kind];
    if (!type || !r.path.startsWith(images) || !existsSync(r.path)) continue;
    const m = meta.get(r.path) ?? {};
    const folder = r.path.split('/').at(-2).split('__');
    const named = speciesOf(`${m.genus ?? folder[0]} ${m.species ?? folder[1]}`);
    if (!named || !BINOMIAL.test(named.binomial)) continue;
    const sub = words(m.subspecies ?? folder[2]);
    // A subspecies the workbook may name as a species (Melinaea marsaeus mothone: Melinaea mothone).
    const raised = /^[a-z][a-z-]+$/.test(sub) ? speciesOf(`${named.binomial.split(' ')[0]} ${sub}`)?.binomial : null;
    const name = r.path.split('/').at(-1);
    // The place's name only ("Shushufindi-29.08.18-Felipecampos": Shushufindi); a code is no place.
    const locality = words(m.locality).replace(/\s*-+\s*[\d.].*$/, '').trim();
    const place = [/[a-z]{3}/i.test(locality) ? title(locality) : '', title(m.province)].filter(Boolean).join(', ');
    out.push({
      species: named.binomial,
      subspecies: sub && sub !== '-' && !/species overview|type [a-z]$/i.test(sub) ? sub : null,
      raised: raised && raised !== named.binomial ? raised : null,
      type,
      source: r.source,
      path: r.path,
      sha: r.sha,
      sex: sexOf(m.sex) ?? (type === 'specimen' ? sexInName(name) : null),
      view: /^(dorsal|ventral)$/.test(m.view ?? '') ? m.view : null,
      place: place || (m.country && !SOURCES[r.source].ecuador ? title(m.country) : null),
      page: m.page_url ?? null,
    });
  }
  return out;
}

/** Up to `max` photos, spread over sites, sexes, sides and subspecies; Ecuador's sites first. */
function pick(list, max, usable) {
  const chosen = [];
  const count = (key, value) => chosen.filter(c => c[key] === value).length;
  const left = [...list].sort((a, b) => a.path.localeCompare(b.path));
  while (chosen.length < max && left.length) {
    let best = -1;
    let bestScore = Infinity;
    left.forEach((c, i) => {
      const score =
        (SOURCES[c.source].ecuador ? 0 : 100) +
        count('source', c.source) * 2 +
        count('sex', c.sex) +
        count('view', c.view) +
        count('subspecies', c.subspecies) * 0.5 +
        (c.sex ? 0 : 0.5);
      if (score < bestScore) [best, bestScore] = [i, score];
    });
    const [c] = left.splice(best, 1);
    if (usable(c)) chosen.push(c);
  }
  return chosen;
}

async function build() {
  const out = resolve(option('--out', '/tmp/reference-photos-build'));
  const dbPath = option('--db', join(homedir(), '.cache/ithomiini-lab/app-workbook/app.sqlite'));
  const maps = option('--maps', join(homedir(), 'Documents/CodeProjs/ithomiini_maps'));
  const inventory = option('--inventory', resolve(maps, '../WingsClassificator/reports/autoresearch_20261013/wingseg_v5_field/data/inventory.jsonl'));
  const readerDir = option('--sharp', process.env.PHOTO_READER_DIR ?? resolve(here, '../photo-reader'));
  if (!existsSync(join(readerDir, 'node_modules/sharp'))) throw new Error(`No sharp in ${readerDir}/node_modules (--sharp <photo-reader dir>).`);
  const sharp = createRequire(join(readerDir, 'photo-reader.mjs'))('sharp');
  const wanted = vocabulary(dbPath);
  const all = await candidates(maps, inventory);
  // Each species' photos, the same photo on two sites once. A species the sites have as a subspecies
  // of another (raised since) takes that subspecies' photos, saying under which name the site has them.
  const seen = new Set();
  const bySpecies = new Map();
  const add = (species, c) => {
    if (c.sha && seen.has(`${species}|${c.sha}`)) return;
    if (c.sha) seen.add(`${species}|${c.sha}`);
    (bySpecies.get(species) ?? bySpecies.set(species, []).get(species)).push(c);
  };
  const named = new Set(all.map(c => c.species));
  for (const c of all) {
    if (wanted.has(c.species)) add(c.species, c);
    if (c.raised && wanted.has(c.raised) && !named.has(c.raised)) add(c.raised, { ...c, siteName: [c.species, c.subspecies].join(' '), subspecies: null });
  }
  rmSync(out, { recursive: true, force: true });
  mkdirSync(join(out, 'images'), { recursive: true });
  const index = {
    version: 1,
    built: new Date().toISOString().slice(0, 10),
    note: 'Field and specimen photos of butterfly sites of Ecuador, for comparing species (scripts/reference-photos.mjs).',
    sources: Object.fromEntries(Object.keys(SOURCES).map(s => [s, { ecuador: SOURCES[s].ecuador }])),
    species: {},
  };
  const sizes = new Map();
  const usable = async c => {
    const meta = await sharp(c.path).metadata().catch(() => null);
    return meta && Math.min(meta.width, meta.height) >= MIN_SIDE;
  };
  let photos = 0;
  for (const [species, list] of [...bySpecies].sort((a, b) => a[0].localeCompare(b[0]))) {
    const ok = new Map();
    for (const c of list) ok.set(c.path, await usable(c));
    const field = pick(
      list.filter(c => c.type === 'field'),
      FIELD_MAX,
      c => ok.get(c.path),
    );
    const specimen = pick(
      list.filter(c => c.type === 'specimen'),
      SPECIMEN_MAX,
      c => ok.get(c.path),
    );
    if (!field.length && !specimen.length) continue;
    const folder = species.replace(/\s+/g, '_');
    mkdirSync(join(out, 'images', folder), { recursive: true });
    const entries = [];
    for (const [i, c] of [...field, ...specimen].entries()) {
      const file = `images/${folder}/${c.source}-${c.type}-${i + 1}.jpg`;
      const info = await sharp(c.path).rotate().resize(SIDE, SIDE, { fit: 'inside', withoutEnlargement: true }).jpeg({ quality: 80, mozjpeg: true }).toFile(join(out, file));
      sizes.set(c.source, (sizes.get(c.source) ?? 0) + 1);
      entries.push(
        Object.fromEntries(
          Object.entries({ file, source: c.source, type: c.type, species, subspecies: c.subspecies, siteName: c.siteName, sex: c.sex, view: c.view, place: c.place, page: c.page, w: info.width, h: info.height }).filter(
            ([, v]) => v !== null && v !== undefined,
          ),
        ),
      );
      photos++;
    }
    index.species[species] = entries;
  }
  writeFileSync(join(out, 'index.json'), JSON.stringify(index));
  const bytes = du(out);
  const covered = Object.keys(index.species);
  const why = { workbook: 0, model: 0, taxonomy: 0 };
  const whyCovered = { workbook: 0, model: 0, taxonomy: 0 };
  for (const [s, w] of wanted) {
    why[w]++;
    if (index.species[s]) whyCovered[w]++;
  }
  const count = type => Object.values(index.species).reduce((n, list) => n + list.filter(e => e.type === type).length, 0);
  const summary = {
    out,
    megabytes: Math.round((bytes / 1024 ** 2) * 10) / 10,
    species: { wanted: wanted.size, covered: covered.length, wantedBy: why, coveredBy: whyCovered },
    photos: { all: photos, field: count('field'), specimen: count('specimen') },
    bySource: Object.fromEntries(sizes),
    notCovered: [...wanted.keys()].filter(s => !index.species[s] && wanted.get(s) === 'workbook'),
  };
  writeFileSync(join(out, 'summary.json'), JSON.stringify(summary, null, 1));
  console.log(JSON.stringify(summary, null, 1));
}

function du(dir) {
  let n = 0;
  for (const f of readdirSync(dir, { withFileTypes: true })) n += f.isDirectory() ? du(join(dir, f.name)) : statSync(join(dir, f.name)).size;
  return n;
}

function upload() {
  const from = resolve(option('--from', '/tmp/reference-photos-build'));
  const host = option('--host', 'claudeclaw');
  const to = option('--to', '/home/ubuntu/ithomiini/shared/reference-photos');
  if (!existsSync(join(from, 'index.json'))) throw new Error(`No index.json in ${from}: build it first.`);
  // The pictures first, the index last: the server never reads an index whose pictures are not there yet.
  const rsync = extra => {
    const r = spawnSync('rsync', ['-a', '--chmod=Du=rwx,Dgo=,Fu=rw,Fgo=', ...extra], { stdio: 'inherit' });
    if (r.status !== 0) throw new Error(`rsync failed (${r.status})`);
  };
  rsync(['--delete', '--exclude=index.json', '--exclude=summary.json', `${from}/`, `${host}:${to}/`]);
  rsync([join(from, 'index.json'), `${host}:${to}/index.json`]);
  console.log(`Copied to ${host}:${to}`);
}

if (command === 'build') await build();
else if (command === 'upload') upload();
else {
  console.error('usage: node scripts/reference-photos.mjs build|upload [options] (see the top of this file)');
  process.exit(2);
}
