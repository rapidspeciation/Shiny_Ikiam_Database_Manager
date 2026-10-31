// The «Fotos» tab's sex check: before a batch of wing photos goes to Drive, each butterfly's d and v photos are
// read here for three sexes, compared, and the disagreements flagged (docs/sex-check.md says how):
//   - the collection sex model's (19 Oct 2026, the CNN on the 4 androconial strips, identifier/sex), with its
//     Supported screen: a Supported call is about 98.8 % right, any other is only a leaning;
//   - the envelope's (the Rename Photos app's envelope reader with its full text, photo-reader --text, read into
//     fields by server/envelope-fields.mjs): the reader invents ♂/♀ on some lines (exact on 93.1 % of envelopes),
//     so on its own it is weaker evidence;
//   - the database's (the Sex of the CAM's rows of Collection_data and Insectary_data).
// The photos are on the person's computer: the tab sends each photo as it will be once turned (1600 px), and
// nothing is kept once read. One butterfly at a time, in two child processes at nice 10 that end after
// IDLE_MS without work: the readings worker (server/readings-worker.mjs, its `only: 'sex'` job: the nano wing
// chain's boxes and the sex model, no species) and the photo reader (envelope text).
//
//   POST /api/sex-check { cam, d: <jpeg base64>, v: <jpeg base64> } -> checkOne's answer
//   GET  /api/sex-check -> { available, why? }

import { spawn } from 'node:child_process';
import { existsSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { setPriority, tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { createInterface } from 'node:readline';
import { envelopeOfReadings } from './envelope-fields.mjs';
import { photoFactsData } from './photo-facts.mjs';

const WORKER = new URL('./readings-worker.mjs', import.meta.url).pathname;
const IDLE_MS = 5 * 60_000;
const ANSWER_MS = 5 * 60_000;
const CAM = /^CAM\d{6}$/;
/** An envelope's sex counts from this confidence (as photo_facts' own envelope-vs-database cases: SURE.sex). */
export const ENVELOPE_SURE = 0.6;

const text = v => (v === null || v === undefined ? '' : String(v).trim());
const binomial = name => text(name).toLowerCase().split(/\s+/).slice(0, 2).join(' ');

/**
 * Species whose model calls are never taken as sure (server/sex-model-unreliable.json: Methona confusa, whose
 * training labels were wrong): { 'genus species': { why, source } }.
 */
export const UNRELIABLE = Object.fromEntries(
  Object.entries(JSON.parse(readFileSync(new URL('./sex-model-unreliable.json', import.meta.url), 'utf8')).species).map(([name, v]) => [binomial(name), v]),
);
/** The entry of a species in the list, or null. */
export const unreliableFor = (species, list = UNRELIABLE) => (species ? (list[binomial(species)] ?? null) : null);
const sexOf = v => {
  const s = text(v).toLowerCase().replace(/\s*\?$/, '');
  return s === 'female' || s === 'male' ? s : null;
};
const r3 = n => Math.round(n * 1000) / 1000;

// ------------------------------------------------------------ the rule
/**
 * The three sexes of a butterfly compared (docs/sex-check.md).
 * model: the sex model's answer ({ pMale, supported, status } | { status: 'not_scored:…' } | null);
 * envelope: the envelope's sex field ({ value, conf, both?, alone?, lines } | null);
 * database: the Sex of the CAM's rows ([{ sheet, row, sex: 'male' | 'female' | null, raw }]);
 * unreliable: the species' entry in server/sex-model-unreliable.json ({ why }) when the model is not to be
 * trusted for it: its call is shown (model.unreliable) but never sure, so it neither flags nor agrees.
 * -> { flag, reasons, model, envelope, database, suggest? }
 *   flag 'disagree': a Supported model call against the database: very likely an error, look before uploading;
 *        'check': weaker evidence against the database, or the sexes of the database's rows differ, or the
 *                 database has no sex where the model (Supported) or the envelope reads one;
 *        'agree': a Supported model call that is the database's;
 *        'uncertain': no Supported model call, and nothing else against the database;
 *        'none': nothing to compare (no model call, no envelope sex, no database sex).
 */
export function decideSex({ model = null, envelope = null, database = [], unreliable = null } = {}) {
  const scored = model && typeof model.pMale === 'number';
  const m = scored
    ? { sex: model.pMale >= 0.5 ? 'male' : 'female', p: r3(Math.max(model.pMale, 1 - model.pMale)), supported: !!model.supported && !unreliable, status: model.status, ...(unreliable ? { unreliable: unreliable.why ?? true } : {}) }
    : { sex: null, status: model?.status ?? 'not_scored' };
  const e = envelope?.value
    ? { sex: envelope.value, conf: r3(envelope.conf ?? 0), sure: (envelope.conf ?? 0) >= ENVELOPE_SURE && !envelope.both, ...(envelope.both ? { both: true } : {}), ...(envelope.alone ? { alone: true } : {}), line: envelope.lines?.[0] ?? null }
    : null;
  const sexes = [...new Set(database.map(r => r.sex).filter(Boolean))];
  const d = { sex: sexes.length === 1 ? sexes[0] : null, rows: database.map(r => ({ sheet: r.sheet, row: r.row, value: r.raw })), ...(sexes.length > 1 ? { differ: true } : {}) };
  const out = (flag, reasons, extra = {}) => ({ flag, reasons: unreliable && m.sex ? [...reasons.filter(r => r !== 'model_not_supported'), 'model_unreliable_species'] : reasons, model: m, envelope: e, database: d, ...extra });
  const modelSure = m.sex && m.supported;
  const envSure = e?.sure ? e.sex : null;

  if (d.differ) return out('check', ['database_rows_differ']);
  if (d.sex) {
    if (modelSure && m.sex !== d.sex) {
      const why = ['model_vs_database'];
      if (envSure === m.sex) why.push('envelope_agrees_with_model');
      else if (envSure === d.sex) why.push('envelope_agrees_with_database');
      return out('disagree', why, { suggest: m.sex });
    }
    if (modelSure) return out('agree', envSure && envSure !== d.sex ? ['envelope_misread_likely'] : []);
    if (envSure && envSure !== d.sex) return out('check', ['envelope_vs_database']);
    return out('uncertain', m.sex ? ['model_not_supported'] : ['model_not_scored']);
  }
  // No sex in the database: what to fill in, when something reads one.
  if (modelSure && envSure && envSure !== m.sex) return out('check', ['model_vs_envelope', 'database_empty']);
  if (modelSure) return out('check', ['database_empty'], { suggest: m.sex });
  if (envSure) return out('check', ['database_empty', 'only_envelope'], { suggest: envSure });
  if (m.sex || e) return out('uncertain', ['database_empty']);
  return out('none', []);
}

/** The CAM's rows with their Sex and its species (for the model's Supported screen). */
export function databaseOf(store, cam) {
  const facts = photoFactsData(store);
  const rows = facts.byCam.get(cam) ?? [];
  const species = rows.map(r => text(r.values.SPECIES).split(/\s+/).slice(0, 2).join(' ')).find(s => /^[A-Z][a-z]+ [a-z-]+$/.test(s)) ?? null;
  return { rows: rows.map(r => ({ sheet: r.sheet, row: r.row, sex: sexOf(r.values.Sex), raw: text(r.values.Sex) })), species, lexicon: facts.lexicon };
}

// ------------------------------------------------------------ the child processes
/** A child that answers one JSON line per line asked (matched by id), started on first use, ended when idle. */
function lineChild(argv, { spawnImpl, nice, ready = o => o.ready === true }) {
  let state = null;
  let seq = 0;
  const start = () => {
    const proc = spawnImpl(process.execPath, argv, { stdio: ['pipe', 'pipe', 'pipe'] });
    try {
      if (proc.pid && nice) setPriority(proc.pid, nice);
    } catch {
      // as it is
    }
    const s = { proc, waiting: new Map(), ended: false, timer: null };
    s.started = new Promise((resolve, reject) => {
      s.ok = resolve;
      s.no = reject;
    });
    s.started.catch(() => {});
    proc.stderr.on('data', d => {
      const line = String(d).trim();
      if (/error|traceback/i.test(line)) process.stderr.write(`Sex check: ${line.slice(0, 300)}\n`);
    });
    const end = why => {
      if (s.ended) return;
      s.ended = true;
      if (state === s) state = null;
      s.no(new Error(why));
      for (const [, w] of s.waiting) w.reject(new Error(why));
      s.waiting.clear();
    };
    proc.on('error', e => end(`could not start (${e.code ?? e.message})`));
    proc.on('exit', code => end(`stopped (${code ?? 'killed'})`));
    proc.stdin.on('error', () => {});
    createInterface({ input: proc.stdout }).on('line', line => {
      let o;
      try {
        o = JSON.parse(line);
      } catch {
        return;
      }
      if (ready(o)) return s.ok();
      const w = o.id !== undefined && s.waiting.get(String(o.id));
      if (!w) return;
      s.waiting.delete(String(o.id));
      w.resolve(o);
    });
    return s;
  };
  return {
    async ask(job) {
      state ??= start();
      const s = state;
      clearTimeout(s.timer);
      await s.started;
      const id = `s${++seq}`;
      try {
        return await new Promise((resolve, reject) => {
          const timer = setTimeout(() => reject(new Error(`no answer in ${ANSWER_MS / 60_000} minutes`)), ANSWER_MS);
          s.waiting.set(id, { resolve: o => (clearTimeout(timer), resolve(o)), reject: e => (clearTimeout(timer), reject(e)) });
          s.proc.stdin.write(`${JSON.stringify({ ...job, id })}\n`);
        });
      } finally {
        if (state === s && !s.waiting.size) s.timer = setTimeout(() => s.proc.stdin.end(), IDLE_MS);
      }
    },
    close() {
      if (state) {
        clearTimeout(state.timer);
        state.proc.stdin.end();
        state.proc.kill();
      }
      state = null;
    },
  };
}

/**
 * The sex check for one app. readerDir: the photo reader (photo-reader.mjs, wing-chain.mjs, models/);
 * identifierDir: the identifier with its sex/ model (tools/identifier/setup.sh with SEX_SRC). workerArgs and
 * readerArgs replace the children's argv (tests, the lab).
 */
export function createSexCheck({ store, readerDir = null, identifierDir = null, workerArgs = null, readerArgs = null, spawnImpl = spawn, nice = 10 } = {}) {
  readerDir &&= resolve(readerDir);
  identifierDir &&= resolve(identifierDir);
  const why = () => {
    if (workerArgs && readerArgs) return null;
    if (!readerDir || !existsSync(join(readerDir, 'wing-chain.mjs')) || !existsSync(join(readerDir, 'photo-reader.mjs'))) return 'no photo reader with the wing chain (scripts/build-rename-app.sh)';
    if (!identifierDir || !existsSync(join(identifierDir, 'venv', 'bin', 'python'))) return 'no identifier (tools/identifier/setup.sh)';
    if (!existsSync(join(identifierDir, 'sex', 'model.json'))) return 'no sex model (tools/identifier/setup.sh with SEX_SRC)';
    return null;
  };
  const worker = lineChild(workerArgs ?? [WORKER, '--reader', readerDir, '--identifier', identifierDir, '--threads', '2'], { spawnImpl, nice });
  const reader = lineChild(readerArgs ?? [join(readerDir ?? '', 'photo-reader.mjs'), '--models', join(readerDir ?? '', 'models'), '--threads', '2', '--text'], { spawnImpl, nice });
  let queue = Promise.resolve();

  async function read(cam, files, species) {
    const work = mkdtempSync(join(tmpdir(), 'ithomiini-sexcheck-'));
    try {
      const paths = {};
      for (const view of ['d', 'v']) {
        paths[view] = join(work, `${view}.jpg`);
        writeFileSync(paths[view], files[view], { mode: 0o600 });
      }
      const [wings, ...envelopes] = await Promise.all([
        worker.ask({
          cam,
          species,
          only: 'sex',
          photos: ['d', 'v'].map(view => ({ fileId: view, path: paths[view], wings: true })),
          sex: { d: 'd', v: 'v' },
        }),
        ...['d', 'v'].map(view => reader.ask({ path: paths[view] }).catch(e => ({ error: e.message }))),
      ]);
      return { wings, envelopes };
    } finally {
      rmSync(work, { recursive: true, force: true });
    }
  }

  /** One butterfly: its CAM and its d and v photos (JPEG bytes, as they will be once turned). */
  async function checkOne({ cam, d, v }) {
    const started = Date.now();
    cam = text(cam).toUpperCase();
    if (!CAM.test(cam)) throw Object.assign(new Error('Not a CAM ID'), { status: 400 });
    const db = databaseOf(store, cam);
    const { wings, envelopes } = await read(cam, { d, v }, db.species);
    const model = wings.error ? { status: `not_scored:${wings.error}`.slice(0, 200) } : (wings.ident?.sex ?? { status: wings.ident?.error ? `not_scored:${wings.ident.error}`.slice(0, 200) : 'not_scored' });
    const readings = envelopes
      .map((r, i) => ({ name: i ? 'v' : 'd', text: (r.text?.lines ?? []).map(l => [l.text, l.conf]), camid: r.camid ?? null, conf: r.conf ?? null }))
      .filter(r => r.text.length);
    const envelope = readings.length ? envelopeOfReadings(readings, db.lexicon).envelope : null;
    // The species for the list of species the model is not trusted on: the database's, else the envelope's.
    const species = db.species ?? envelope?.species?.value ?? null;
    const verdict = decideSex({ model, envelope: envelope?.sex ?? null, database: db.rows, unreliable: unreliableFor(species) });
    return {
      cam,
      ...verdict,
      species,
      envelopeCam: envelope?.camid?.value ?? null,
      ...(model.kind ? { modelInfo: { pMale: model.pMale, status: model.status, crops: model.crops, fold: model.fold } } : {}),
      ms: Date.now() - started,
    };
  }

  return {
    status: () => {
      const no = why();
      return no ? { available: false, why: no } : { available: true };
    },
    /** One at a time for the whole app (the server has 4 cores). */
    check(job) {
      const no = why();
      if (no) return Promise.reject(Object.assign(new Error(`The sex check is not available here: ${no}`), { status: 503 }));
      const run = queue.then(() => checkOne(job));
      queue = run.catch(() => {});
      return run;
    },
    close() {
      worker.close();
      reader.close();
    },
  };
}
