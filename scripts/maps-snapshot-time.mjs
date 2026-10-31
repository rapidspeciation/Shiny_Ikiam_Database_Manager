// maps_snapshot against the published maps, without the app: a cold call (data and tiles fetched) and warm ones.
// node scripts/maps-snapshot-time.mjs <folder for the pictures> ['<json args>' ...]
import { createMapsTools } from '../server/maps-tools.mjs';

const [dir, ...asked] = process.argv.slice(2);
if (!dir) throw new Error('Give a folder for the pictures.');
const calls = asked.length ? asked.map(a => JSON.parse(a)) : [{ species: ['Mechanitis polymnia', 'Tithorea harmonia', 'Mechanitis messenoides'], sex: 'male', lang: 'es' }];
const tools = createMapsTools({ snapshotDir: dir });
for (const [i, args] of [...calls, ...calls].entries()) {
  const t = Date.now();
  const out = await tools.run('maps_snapshot', args);
  const ms = Date.now() - t;
  if (out.error) throw new Error(out.error);
  console.log(JSON.stringify({ call: i < calls.length ? 'first' : 'again', ms, timing: out.timing_ms, image: out.image, kb: Math.round(out.image_bytes / 1024), sites: out.sites.slice(0, 5).map(s => `${s.rank}. ${s.site} (${s.country}) ${JSON.stringify(s.by_sex)}`), map_url: out.map_url }));
}
