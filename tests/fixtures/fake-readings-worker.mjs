// A stand-in for server/readings-worker.mjs (tests/readings.test.mjs): it plans the batch from the database as the
// real one does (server/readings.mjs nextBatch), and makes its answers up from the photos' Drive IDs. An ID with
// "BAD" cannot be read (its wings and species fail), "SLOW" answers after 300 ms, "CRASH" ends the process.
// `--log <file>`: each butterfly's job is appended there (what the reading was asked).
import { appendFileSync } from 'node:fs';
import { DatabaseSync } from 'node:sqlite';
import { nextBatch } from '../../server/readings.mjs';

const arg = name => (process.argv.includes(`--${name}`) ? process.argv[process.argv.indexOf(`--${name}`) + 1] : null);
const logFile = arg('log');
const say = o => process.stdout.write(`${JSON.stringify(o)}\n`);
const MODEL = 'wingseg_v5n_640 f3c4f755 + wing_side_cls 838b9371 + wingside.json 48a70be6';
say({ ready: true, pid: process.pid, models: { wings: MODEL } });

const wing = (kind, half, box, score) => ({ box, kind, half, pDorsal: 0.99, pForewing: kind === 'forewing' ? 0.98 : 0.02, score });
const square = ([x0, y0, x1, y1]) => [x0, y0, x1, y0, x1, y1, x0, y1].map((v, i) => Math.round(v * 1000));
function wingsOf(name) {
  const dorsal = /d$/.test(name);
  const wings = [
    wing('forewing', 'left', [0.1, 0.1, 0.3, 0.3], 0.95),
    wing('forewing', 'right', [0.4, 0.1, 0.6, 0.3], 0.94),
    wing('hindwing', 'left', [0.12, 0.35, 0.28, 0.5], 0.93),
    wing('hindwing', 'right', [0.42, 0.35, 0.58, 0.5], 0.92),
    wing('extra', 'unknown', [0.8, 0.8, 0.95, 0.95], 0.4),
  ];
  return {
    side: dorsal ? 'dorsal' : 'ventral',
    p: dorsal ? 0.995 : 0.004,
    fore: { side: dorsal ? 'dorsal' : 'ventral', p: dorsal ? 0.996 : 0.003 },
    hind: { side: dorsal ? 'dorsal' : 'ventral', p: dorsal ? 0.994 : 0.005 },
    missing: [],
    turnK: 0,
    pTurn: 0.999,
    size: [1600, 1067],
    wings,
    outlines: wings.map(w => [{ forewing: 'fw', hindwing: 'hw', extra: 'x' }[w.kind], w.half[0], square(w.box)]),
  };
}
const tops = name => ({ species: [[`Genus ${name}`, 0.8], ['Genus other', 0.1]], subspecies: [[`Genus ${name} sub`, 0.7]], genus: [['Genus', 0.95]] });

const db = new DatabaseSync(arg('db'), { readOnly: true });
const plan = nextBatch(db, { size: Number(arg('batch')), now: Number(arg('now')) });
db.close();
say({ plan });
for (const cam of plan?.cams ?? []) {
  const job = { cam: cam.cam, side: cam.side, ecuador: cam.ecuador, species: cam.species, photos: cam.photos.map(p => ({ fileId: p.fileId, name: p.name, dup: `${p.suffix}|${p.sizeKb ?? ''}`, wings: p.wings })), sex: cam.sex?.d ? cam.sex : null };
  if (logFile) appendFileSync(logFile, `${JSON.stringify(job)}\n`);
  if (job.photos.some(p => p.fileId.includes('CRASH'))) process.exit(5);
  if (job.photos.some(p => p.fileId.includes('SLOW'))) await new Promise(resolve => setTimeout(resolve, 300));
  const wings = {};
  const photos = {};
  for (const p of job.photos) {
    if (p.wings) wings[p.fileId] = p.fileId.includes('BAD') ? { error: 'Drive answered 404' } : wingsOf(p.name);
    photos[p.fileId] = p.fileId.includes('BAD') ? { error: 'not downloaded' } : { ...tops(p.name), crop: 'success', wingBox: [0.1, 0.1, 0.6, 0.5], ms: 5 };
  }
  const used = job.photos.filter(p => !p.fileId.includes('BAD')).map(p => p.fileId);
  say({
    id: job.cam,
    cam: job.cam,
    wings,
    ident: {
      photos,
      combined: used.length ? { ...tops(job.cam), used } : null,
      sex: job.sex ? { pMale: 0.93, status: 'species_supported', supported: true, fold: 2 } : null,
    },
    downloads: {},
    ms: 1,
  });
}
