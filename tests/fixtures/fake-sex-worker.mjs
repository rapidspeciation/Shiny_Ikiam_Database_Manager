// Stand-ins for the sex check's two children (tests/sex-check.test.mjs): the readings worker's `only: 'sex'` job
// and the photo reader with --text. Each "photo" is JSON: { pMale, supported, line: [text, conf] }; the worker
// answers the d photo's model call (and echoes the species it was given), the reader the photo's envelope line.
import { readFileSync } from 'node:fs';
import { createInterface } from 'node:readline';

const reader = process.argv.includes('--reader');
const say = o => process.stdout.write(`${JSON.stringify(o)}\n`);
say({ ready: true, pid: process.pid });
for await (const line of createInterface({ input: process.stdin })) {
  const job = JSON.parse(line);
  if (reader) {
    const photo = JSON.parse(readFileSync(job.path, 'utf8'));
    say({ id: job.id, camid: photo.camid ?? null, conf: 0.99, ...(photo.line ? { text: { turn: 0, lines: [{ text: photo.line[0], conf: photo.line[1], turn: 0 }] } } : {}) });
    continue;
  }
  if (job.only !== 'sex' || !job.sex) {
    say({ id: job.id, error: 'not a sex job' });
    continue;
  }
  const d = JSON.parse(readFileSync(job.photos.find(p => p.fileId === job.sex.d).path, 'utf8'));
  const sex = d.pMale === undefined ? { status: 'not_scored:no_wings' } : { pMale: d.pMale, supported: d.supported, status: `species:${job.species}`, kind: 'cnn', crops: 4, fold: 2 };
  say({ id: job.id, cam: job.cam, wings: {}, ident: { id: '1', sex }, downloads: {}, ms: 1 });
}
