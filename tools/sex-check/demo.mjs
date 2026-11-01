// The «Fotos» tab's sex check (server/sex-check.mjs) on photo sets already on Drive, as the tab would send them:
// each CAM's d and v photos (Drive's 1600 px copy, the size the tab sends), checked against a copy of the app's
// database. Prints one line a butterfly and writes the answers as JSON.
//
//   node tools/sex-check/demo.mjs --db <app.sqlite> --reader <photo-reader dir> --identifier <dir with venv/ and sex/>
//        --out <answers.json> [--photos <cache dir>] CAM077514 CAM071511 …
import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';
import { DatabaseSync } from 'node:sqlite';
import { driveUrls } from '../../server/photos.mjs';
import { createSexCheck } from '../../server/sex-check.mjs';

const arg = (name, fallback = null) => {
  const i = process.argv.indexOf(`--${name}`);
  return i >= 0 ? process.argv[i + 1] : fallback;
};
const cams = process.argv.slice(2).filter(a => /^CAM\d{6}$/.test(a));
const db = new DatabaseSync(arg('db'), { readOnly: true });
const store = { db };
const cache = arg('photos', join(process.env.HOME, '.cache', 'ithomiini-sexcheck', 'demo'));
mkdirSync(cache, { recursive: true });

/** The CAM's d and v JPG photos as Photo_links lists them. */
function fileIds(cam) {
  const out = {};
  for (const r of db.prepare("SELECT values_json FROM records WHERE sheet='Photo_links' AND missing=0 AND values_json LIKE ?").all(`%${cam}%`)) {
    const v = JSON.parse(r.values_json);
    const m = new RegExp(`^${cam}([dv])\\.(?:jpe?g)$`, 'i').exec(v.Name ?? '');
    const id = /\/d\/([\w-]+)/.exec(v.URL ?? '')?.[1];
    if (m && id) out[m[1].toLowerCase()] ??= id;
  }
  return out;
}
async function photo(cam, view, id) {
  const path = join(cache, `${cam}_${view}.jpg`);
  if (existsSync(path)) return readFileSync(path);
  for (const url of driveUrls(id, 1600)) {
    const r = await fetch(url, { redirect: 'follow' });
    if (r.ok && /^image\//.test(r.headers.get('content-type') ?? '')) {
      const bytes = Buffer.from(await r.arrayBuffer());
      writeFileSync(path, bytes);
      return bytes;
    }
  }
  throw new Error(`${cam}${view}: no copy from Drive`);
}

const check = createSexCheck({ store, readerDir: arg('reader'), identifierDir: arg('identifier') });
const answers = [];
const sign = s => (s === 'male' ? '♂' : s === 'female' ? '♀' : '·');
for (const cam of cams) {
  const ids = fileIds(cam);
  if (!ids.d || !ids.v) {
    console.log(`${cam}  no d and v photos in Photo_links`);
    continue;
  }
  const out = await check.check({ cam, d: await photo(cam, 'd', ids.d), v: await photo(cam, 'v', ids.v) });
  answers.push(out);
  const m = out.model.sex ? `${sign(out.model.sex)} ${out.model.p}${out.model.supported ? ' Supported' : ' (not Supported)'}` : out.model.status;
  const e = out.envelope ? `${sign(out.envelope.sex)} ${out.envelope.conf}${out.envelope.sure ? '' : ' weak'}` : '·';
  console.log(`${cam}  ${(out.species ?? '').padEnd(24)} model ${m.padEnd(22)} envelope ${e.padEnd(10)} database ${sign(out.database.sex)}  -> ${out.flag} ${out.reasons.join(',')}  ${out.ms} ms`);
}
check.close();
if (arg('out')) writeFileSync(arg('out'), `${JSON.stringify(answers, null, 1)}\n`);
