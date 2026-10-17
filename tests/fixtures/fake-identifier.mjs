// A stand-in for tools/identifier/worker.py (tests/identifier.test.mjs): the same lines on stdin and
// stdout. The photo's file says what to answer: "Genus species p" (a name and its probability),
// "slow" (no answer), "crash" (the process ends), or anything else (an error for that photo).
// `--idle <s>`: ends after that long without photos, as the real one does.
import { readFileSync } from 'node:fs';
import { createInterface } from 'node:readline';

const idle = Number(process.argv[process.argv.indexOf('--idle') + 1] || 600);
let timer = null;
const quiet = () => {
  clearTimeout(timer);
  timer = setTimeout(() => {
    process.stdout.write(`${JSON.stringify({ idle: true })}\n`);
    process.exit(0);
  }, idle * 1000);
};
const say = o => process.stdout.write(`${JSON.stringify(o)}\n`);
say({ ready: true, pid: process.pid });
// What libraries print goes to stderr in the real one; a stray line here must be ignored.
process.stdout.write('Loading something...\n');
quiet();
let loaded = false;
createInterface({ input: process.stdin }).on('line', line => {
  const job = JSON.parse(line);
  quiet();
  let text;
  try {
    text = readFileSync(job.path, 'utf8').trim();
  } catch (e) {
    return say({ id: job.id, error: `FileNotFoundError: ${job.path}` });
  }
  if (text === 'slow') return;
  if (text === 'crash') process.exit(3);
  const m = /^(\S+) (\S+) ([\d.]+)$/.exec(text);
  if (!m) return say({ id: job.id, error: 'UnidentifiedImageError: cannot identify image file' });
  const p = Number(m[3]);
  const out = {
    id: job.id,
    species: [[`${m[1]} ${m[2]}`, p], [`${m[1]} other`, (1 - p) * 0.9], ['Tiny one', 0.004]],
    subspecies: [[`${m[1]} ${m[2]} ${m[2]}`, p * 0.8]],
    genus: [[m[1], Math.min(1, p + 0.05)]],
    top: [`${m[1]} ${m[2]} ${m[2]}`, p * 0.8],
    crop: job.box ? 'manual_bbox_square_letterbox' : text.includes('centre') ? 'centre_crop_fallback' : 'success',
    wingBox: null,
    prior: job.side ?? null,
    ms: 5,
    pid: process.pid,
  };
  if (!loaded) out.loadMs = 10;
  loaded = true;
  say(out);
});
process.stdin.on('end', () => process.exit(0));
