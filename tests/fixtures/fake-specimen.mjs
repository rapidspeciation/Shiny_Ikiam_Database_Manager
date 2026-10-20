// A stand-in for tools/identifier/worker.py's "specimen" op (tests/readings.test.mjs, the readings worker): reads
// each photo's file, "Genus species p", and answers its lines; the butterfly's species is its first photo's.
import { readFileSync } from 'node:fs';
import { createInterface } from 'node:readline';

const say = o => process.stdout.write(`${JSON.stringify(o)}\n`);
say({ ready: true, pid: process.pid });
for await (const line of createInterface({ input: process.stdin })) {
  const job = JSON.parse(line);
  const photos = {};
  for (const p of job.photos) {
    const [genus, species, prob] = readFileSync(p.path, 'utf8').trim().split(' ');
    photos[p.key] = { species: [[`${genus} ${species}`, Number(prob)]], subspecies: [], genus: [[genus, Number(prob)]], crop: 'success', wingBox: null, ms: 1 };
  }
  const first = Object.values(photos)[0];
  say({ id: job.id, photos, combined: first ? { ...first, used: job.photos.map(p => p.key) } : null, sex: job.sex ? { pMale: 0.2, status: 'below_confidence', supported: false, fold: 1, side: job.side, wings: job.sex.wings } : null, ms: 2 });
}
