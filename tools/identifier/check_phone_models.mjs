// What the phone's model list (Identificar → Descargas, /prueba-offline/models/list.json) offers from an identifier
// folder: each file's role, size, version and SHA-256, and why something is not offered (an invalid sex-model
// or small-model manifest, a file whose size or SHA-256 is not the manifest's). Read-only: hashes the files, changes
// nothing. Run it after dropping in a new sex or small model (its .onnx and offline/sex-model.json or
// offline/student-model.json; model-versions HANDOFF.md).
//   node tools/identifier/check_phone_models.mjs /home/ubuntu/ithomiini/shared/identifier
// Exit 1 when the sex model is not offered, a manifest is invalid, or a manifest's file is missing or not its bytes.
import { createHash } from 'node:crypto';
import { createReadStream } from 'node:fs';
import { stat } from 'node:fs/promises';
import { join } from 'node:path';
import { contentVersion, phoneModels, SEX_MANIFEST, STUDENT_MANIFEST } from '../../server/offline-id.mjs';

const dir = process.argv[2];
if (!dir) {
  console.error('usage: check_phone_models.mjs <identifier dir>');
  process.exit(2);
}
const sha = file =>
  new Promise((ok, fail) => {
    const h = createHash('sha256');
    createReadStream(file, { highWaterMark: 1 << 20 })
      .on('data', b => h.update(b))
      .on('end', () => ok(h.digest('hex')))
      .on('error', fail);
  });
const { models, problems } = await phoneModels(dir);
const manifest = await stat(join(dir, SEX_MANIFEST)).catch(() => null);
console.log(`Sex model: ${manifest ? SEX_MANIFEST : `no ${SEX_MANIFEST}, model B (built in)`}`);
const smallManifest = await stat(join(dir, STUDENT_MANIFEST)).catch(() => null);
console.log(`Small model: ${smallManifest ? STUDENT_MANIFEST : `no ${STUDENT_MANIFEST}, model S_B (built in)`}`);
let sexOffered = false;
for (const m of models) {
  const file = join(dir, m.path);
  const s = await stat(file).catch(() => null);
  if (!s?.isFile()) {
    console.log(`${m.role.padEnd(9)} ${m.name}: MISSING${m.optional ? ' (optional: not offered)' : ''}`);
    if (m.role === 'sex' || (m.role === 'student' && smallManifest)) problems.push(`${m.path} is missing`);
    continue;
  }
  const got = await sha(file);
  const bad = (m.bytes && s.size !== m.bytes ? [`size ${s.size}, manifest ${m.bytes}`] : []).concat(m.sha256 && got !== m.sha256 ? [`sha256 ${got}, manifest ${m.sha256}`] : []);
  console.log(`${m.role.padEnd(9)} ${m.name}: ${s.size} B, version "${m.version ?? contentVersion(got)}", sha256 ${got}${bad.length ? `: NOT OFFERED (${bad.join('; ')})` : ''}`);
  if (m.role === 'sex') {
    if (bad.length) problems.push(`${m.name}: ${bad.join('; ')}`);
    else sexOffered = true;
    if (m.calibration) console.log(`          calibration ${JSON.stringify(m.calibration)}`);
  }
  if (m.role === 'student' && smallManifest && bad.length) problems.push(`${m.name}: ${bad.join('; ')}`);
  if (m.input) console.log(`          input ${m.input.size} x ${m.input.size} (${m.input.values})`);
}
for (const p of problems) console.error(`Problem: ${p}`);
process.exit(sexOffered && !problems.length ? 0 : 1);
