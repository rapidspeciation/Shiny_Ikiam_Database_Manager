// Precompresses the offline identifier page's model parts (server/offline-id.mjs precompressParts) into a new
// offline/parts-<stamp>/ folder; the app picks it up by itself (no restart). --if-needed: only when a model has
// no parts made from its current bytes (scripts/deploy.sh runs it so on every deploy).
//   node tools/identifier/precompress_offline_parts.mjs [--if-needed] /home/ubuntu/ithomiini/shared/identifier
import { modelsWithoutParts, precompressParts } from '../../server/offline-id.mjs';

const args = process.argv.slice(2);
const ifNeeded = args.includes('--if-needed');
const dir = args.find(a => !a.startsWith('--'));
if (!dir) {
  console.error('usage: precompress_offline_parts.mjs [--if-needed] <identifier dir>');
  process.exit(2);
}
if (ifNeeded) {
  const missing = await modelsWithoutParts(dir);
  if (!missing.length) {
    console.log('Offline identifier parts: up to date.');
    process.exit(0);
  }
  console.log(`Offline identifier parts: compressing (${missing.join(', ')} without parts).`);
}
const t = Date.now();
const { dir: out, manifest } = await precompressParts(dir, { log: ifNeeded ? () => {} : line => console.log(line) });
const sum = key => Object.values(manifest.files).reduce((a, f) => a + f.parts.reduce((b, p) => b + p[key], 0), 0);
console.log(`${out}: plain ${sum('bytes')} B, zstd ${sum('zst')} B, gzip ${sum('gz')} B, ${Math.round((Date.now() - t) / 1000)} s`);
