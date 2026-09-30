#!/usr/bin/env node
// Benchmark: a model transcribes the lab's notebook photos in the lab T3 (tools/lab/t3.sh),
// through the lab app's tools (tools/lab/app.sh), and its proposals are scored cell by
// cell against the snapshot's current values of those rows (the human-corrected data).
//   node tools/lab/bench.mjs <model> [effort] [--cases id,prefix…] [--parallel 3] [--timeout 40] [--dry]
//   node tools/lab/bench.mjs --history          the results so far, model × case
// <model>: opus | sonnet | gpt-6.1-sol, or a name as T3's model picker shows it ("GPT-6-Sol").
// [effort]: low | medium | high | xhigh | max | ultra (as the model offers; default: T3's default).
// Every case gets its own new thread (in parallel), the same prompt, and its photos.
// Output: $LAB/results/<run>/ (run.json, scores.json, errors.csv, table.md) and a line per case
// in $LAB/results/history.jsonl.
import { readFileSync, existsSync, mkdirSync, appendFileSync } from 'node:fs';
import { DatabaseSync } from 'node:sqlite';
import { homedir } from 'node:os';
import { join } from 'node:path';
import { LAB, labPath, loadCases, loadSnapshot, groundTruth, norm, same, writePrivate, readJson } from './lib.mjs';

const PLAYWRIGHT = process.env.LAB_PLAYWRIGHT || join(homedir(), '.local/share/ithomiini-wikiloc/node_modules/playwright-core/index.mjs');
const CHROMIUM = process.env.LAB_CHROMIUM || '/usr/bin/chromium';
const T3 = `http://127.0.0.1:${process.env.LAB_T3_PORT || 3775}`;
const T3_HOME = process.env.LAB_T3_HOME || join(homedir(), '.t3-ithomiini-lab');
const PROJECT = 'Ithomiini · Lab';
const MODELS = {
  opus: 'Claude Opus 5.5',
  'opus-5.5': 'Claude Opus 5.5',
  sonnet: 'Claude Sonnet 5.5',
  'sonnet-5.5': 'Claude Sonnet 5.5',
  'gpt-6.1-sol': 'GPT-6.1-Sol',
  sol: 'GPT-6.1-Sol',
  'gpt-6-sol': 'GPT-6-Sol',
};
const EFFORTS = { low: 'Low', medium: 'Medium', high: 'High', xhigh: 'Extra High', max: 'Max', ultra: 'Ultra' };

// ------------------------------------------------------------------ arguments
const argv = process.argv.slice(2);
const flag = (name, fallback) => {
  const i = argv.indexOf(name);
  if (i < 0) return fallback;
  const value = argv[i + 1];
  argv.splice(i, 2);
  return value;
};
if (argv.includes('--history')) {
  printHistory();
  process.exit(0);
}
const dry = argv.includes('--dry') && argv.splice(argv.indexOf('--dry'), 1);
const only = flag('--cases', '');
const parallel = Number(flag('--parallel', 3));
const timeoutMin = Number(flag('--timeout', 40));
const [modelArg, effortArg] = argv;
if (!modelArg) {
  console.error('Usage: bench.mjs <model> [effort] [--cases id,prefix] [--parallel 3] [--timeout 40] [--dry] | --history');
  process.exit(2);
}
const modelName = MODELS[modelArg.toLowerCase()] ?? modelArg;
if (/fable/i.test(modelName)) throw new Error('Fable models are not used in this lab');
const effort = effortArg ? (EFFORTS[effortArg.toLowerCase()] ?? effortArg) : null;

const cases = loadCases().filter(
  c => !only || only.split(',').some(p => c.id === p || c.id.startsWith(p)),
);
if (!cases.length) throw new Error(`No case matches ${only}`);
const runId = `${new Date().toISOString().replace(/[-:]/g, '').slice(0, 13)}-${modelName.replace(/\W+/g, '').toLowerCase()}${effort ? '-' + effort.replace(/\W+/g, '').toLowerCase() : ''}`;
const outDir = labPath('results', runId);

/** The same prompt for every model; only the tag and the photo count change. */
function prompt(kase, tag) {
  const n = kase.photos.length;
  return [
    `[${tag}] Prueba de transcripción del laboratorio local.`,
    `Transcribe ${n > 1 ? `estas ${n} fotos` : 'esta foto'} del cuaderno a la hoja ${kase.sheet}, siguiendo la skill digitalizar-cuaderno.`,
    'Lee SOLO las fotos: no completes lo que no puedes leer con valores de la hoja ni de otras hojas (busca en la hoja solo para ubicar cada fila por su ID o número de clutch; las celdas de estas filas están vacías, es normal).',
    `Luego usa match_notebook (una propuesta por página, con title "${tag}"); si match_notebook no sirve para estas fotos, usa propose_changes con reason "${tag}". Incluye todas las columnas que se ven en la foto.`,
    'NO apliques nada (nunca apply_proposal) y no hagas preguntas: termina con un resumen corto de lo que propusiste y de las celdas dudosas.',
  ].join(' ');
}

// ------------------------------------------------------------------ preflight
const snapshot = loadSnapshot();
const truth = Object.fromEntries(cases.map(c => [c.id, groundTruth(snapshot, c)]));
const appDbFile = labPath('app', 'app.sqlite');
const credentials = readJson(labPath('credentials.json'));
const health = await fetch(`${credentials.url}/health`).then(r => r.json()).catch(() => null);
if (!health) throw new Error('The lab app is not running: tools/lab/app.sh --bg');
if (!(await fetch(T3).then(r => r.ok).catch(() => false))) throw new Error('The lab T3 is not running: tools/lab/t3.sh --bg');
{
  // The cases' cells must still be empty in the lab app (a model that applied a proposal fills them).
  const db = new DatabaseSync(appDbFile, { readOnly: true });
  const filled = [];
  for (const kase of cases)
    for (const r of truth[kase.id]) {
      const rec = db.prepare('SELECT values_json FROM records WHERE sheet = ? AND row_num = ?').get(kase.sheet, r.row);
      const values = JSON.parse(rec?.values_json ?? '{}');
      for (const f of Object.keys(r.values)) if (norm(values[f], f) !== '') filled.push(`${kase.id} ${r.label} ${f}`);
    }
  db.close();
  if (filled.length)
    throw new Error(`${filled.length} benchmark cells are not empty in the lab app (e.g. ${filled[0]}): restart tools/lab/app.sh`);
}
for (const kase of cases)
  for (const photo of kase.photos)
    if (!existsSync(labPath('photos', photo))) throw new Error(`${kase.id}: missing photo ${photo}`);

// ------------------------------------------------------------------ start the threads
const { chromium } = await import(PLAYWRIGHT);
const token = readFileSync(labPath('t3-admin-token'), 'utf8').trim();
const pairing = await fetch(`${T3}/api/auth/pairing-token`, {
  method: 'POST',
  headers: { authorization: `Bearer ${token}`, 'content-type': 'application/json' },
  body: JSON.stringify({ label: `bench ${runId}`, scopes: ['orchestration:read', 'orchestration:operate', 'terminal:operate', 'review:write', 'relay:read'] }),
}).then(r => (r.ok ? r.json() : Promise.reject(new Error(`T3 pairing: ${r.status}`))));
mkdirSync(join(outDir, 'shots'), { recursive: true, mode: 0o700 });
const browser = await chromium.launch({ executablePath: CHROMIUM, headless: true });
const context = await browser.newContext({ viewport: { width: 1600, height: 950 } });
{
  const page = await context.newPage();
  await page.goto(`${T3}/pair#token=${encodeURIComponent(pairing.credential)}`);
  await page.waitForURL(url => !/\/pair/.test(url.pathname), { timeout: 30000 }).catch(() => {});
  await page.close();
}

async function startThread(kase) {
  const tag = `LAB ${runId} ${kase.id}`;
  const out = { case: kase.id, tag, prompt: prompt(kase, tag) };
  const page = await context.newPage();
  const shot = name => page.screenshot({ path: join(outDir, 'shots', `${kase.id}-${name}.png`) }).catch(() => {});
  try {
    await page.goto(T3);
    const newThread = page.locator(`button[aria-label="New thread in ${PROJECT}"]`);
    await newThread.waitFor({ timeout: 30000 });
    await newThread.click();
    await page.waitForURL(/\/draft\//, { timeout: 15000 });

    // The model, by its exact name in the picker.
    await page.locator('button[data-chat-provider-model-picker]').click();
    await page.locator('input[placeholder="Search models..."]').fill(modelName.replace(/^Claude /, ''));
    await page.waitForTimeout(1000);
    const options = page.locator('[role=option]');
    const texts = await options.evaluateAll(els => els.map(e => e.innerText.replace(/\s+/g, ' ').trim()));
    const index = texts.findIndex(t => t === modelName || t.startsWith(modelName + ' '));
    if (index < 0) throw new Error(`Model ${modelName} not in the picker: ${texts.join(' | ')}`);
    await options.nth(index).click();
    await page.waitForTimeout(800);
    out.picked = (await page.locator('button[data-chat-provider-model-picker]').innerText()).trim();
    if (out.picked !== modelName) throw new Error(`Picked ${out.picked}, not ${modelName}`);

    // The reasoning effort (the first menu item with that label).
    const effortButton = page.locator('button[data-composer-shortcut]').first();
    if (effort) {
      await effortButton.click();
      const item = page.locator('[role=menuitemradio]').filter({ hasText: new RegExp(`^\\s*${effort}(\\s|$)`) }).first();
      await item.waitFor({ timeout: 5000 }).catch(() => {
        throw new Error(`${modelName} has no effort ${effort}`);
      });
      await item.click();
      await page.keyboard.press('Escape').catch(() => {});
      await page.waitForTimeout(400);
    }
    out.effort = (await effortButton.innerText()).trim();
    if (effort && !out.effort.startsWith(effort)) throw new Error(`Effort is ${out.effort}, not ${effort}`);

    // Photos, prompt, send.
    await page.locator('input[type=file]').first().setInputFiles(kase.photos.map(p => labPath('photos', p)));
    await page.waitForTimeout(3000 + 1500 * kase.photos.length);
    await page.locator('[data-testid=composer-editor], [data-lexical-editor]').first().click();
    await page.keyboard.insertText(out.prompt);
    await page.waitForTimeout(500);
    await shot('before-send');
    if (dry) return { ...out, dry: true };
    out.sentAt = new Date().toISOString();
    await page.locator('button[aria-label="Send message"]').click();
    await page.waitForURL(url => !/\/draft\//.test(url.pathname), { timeout: 60000 });
    out.threadId = (page.url().match(/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/g) ?? []).pop() ?? null;
    await page.waitForTimeout(2000);
    await shot('sent');
    return out;
  } catch (e) {
    await shot('error');
    return { ...out, error: e.message.slice(0, 400) };
  } finally {
    await page.close();
  }
}

const started = [];
const queue = [...cases];
await Promise.all(
  Array.from({ length: Math.max(1, parallel) }, async () => {
    for (let kase = queue.shift(); kase; kase = queue.shift()) {
      const r = await startThread(kase);
      console.log(`${r.case}: ${r.error ? 'ERROR ' + r.error : r.dry ? 'ready (dry run)' : `sent (${r.picked} · ${r.effort}) thread ${r.threadId}`}`);
      started.push(r);
    }
  }),
);
await browser.close();
const run = { runId, model: modelName, effort, startedAt: new Date().toISOString(), lab: LAB, threads: started };
writePrivate(join(outDir, 'run.json'), JSON.stringify(run, null, 1));
if (dry) process.exit(0);

// ------------------------------------------------------------------ wait for the turns
const t3db = () => new DatabaseSync(join(T3_HOME, 'userdata', 'state.sqlite'), { readOnly: true });
const deadline = Date.now() + timeoutMin * 60000;
const live = started.filter(s => s.threadId);
for (;;) {
  const db = t3db();
  let open = 0;
  for (const s of live) {
    const turns = db.prepare('SELECT state, requested_at, completed_at FROM projection_turns WHERE thread_id = ? ORDER BY row_id').all(s.threadId);
    const thread = db.prepare('SELECT pending_approval_count, pending_user_input_count, model_selection_json FROM projection_threads WHERE thread_id = ?').get(s.threadId);
    s.turns = turns.map(t => ({ ...t }));
    s.modelSelection = thread?.model_selection_json ? JSON.parse(thread.model_selection_json) : null;
    s.waitingOnPerson = Boolean(thread?.pending_approval_count || thread?.pending_user_input_count);
    const done = turns.length && turns.every(t => ['completed', 'error', 'interrupted'].includes(t.state));
    s.state = done ? turns.at(-1).state : s.waitingOnPerson ? 'waiting' : 'running';
    if (!done && !s.waitingOnPerson) open++;
  }
  db.close();
  if (!open || Date.now() > deadline) break;
  process.stdout.write(`\r${new Date().toTimeString().slice(0, 8)} ${open} of ${live.length} threads still working…`);
  await new Promise(r => setTimeout(r, 15000));
}
process.stdout.write('\n');

// ------------------------------------------------------------------ fetch the proposals and score
const db = t3db();
const appDb = new DatabaseSync(appDbFile, { readOnly: true });
const iso = t => (t ? new Date(t).getTime() : null);
const scores = [];
const errorsCsv = [['case', 'row', 'field', 'read', 'truth', 'kind']];
for (const s of started) {
  const kase = cases.find(c => c.id === s.case);
  const rows = truth[s.case];
  const score = { runId, model: modelName, effort: s.effort ?? effort, case: s.case, threadId: s.threadId ?? null, state: s.state ?? (s.error ? 'not started' : '?') };
  if (s.error) score.error = s.error;
  // Every proposal the thread made: ids in its tool results, or the tag in the proposal's title.
  const ids = new Set();
  if (s.threadId) {
    const texts = [
      ...db.prepare('SELECT payload_json t FROM projection_thread_activities WHERE thread_id = ?').all(s.threadId),
      ...db.prepare('SELECT text t FROM projection_thread_messages WHERE thread_id = ?').all(s.threadId),
    ];
    for (const { t } of texts) for (const m of String(t ?? '').matchAll(/proposalId\\?"?\s*[:=]\s*\\?"?([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})/g)) ids.add(m[1]);
    const answers = db.prepare("SELECT text FROM projection_thread_messages WHERE thread_id = ? AND role = 'assistant' ORDER BY created_at").all(s.threadId);
    score.answer = answers.at(-1)?.text?.slice(0, 2000) ?? null;
    score.tools = db
      .prepare("SELECT payload_json FROM projection_thread_activities WHERE thread_id = ? AND kind = 'tool.completed' ORDER BY created_at, sequence")
      .all(s.threadId)
      .map(a => {
        const p = JSON.parse(a.payload_json);
        return String(p.data?.toolName ?? p.data?.item?.tool ?? p.title ?? '').replace(/^mcp__ithomiini__/, '');
      });
  }
  const proposals = appDb
    .prepare(`SELECT id, reason, status, changes_json, created_at FROM ai_proposals WHERE id IN (${[...ids].map(() => '?').join(',') || "''"}) OR reason LIKE ? ORDER BY created_at`)
    .all(...ids, `%${s.tag}%`)
    .filter(p => p.status !== 'discarded');
  score.proposals = proposals.map(p => ({ id: p.id, status: p.status, reason: p.reason }));
  if (proposals.some(p => p.status !== 'pending')) score.warning = 'A proposal was applied: restart tools/lab/app.sh before the next run';
  const read = new Map(); // "sheet row" → values
  let outside = 0;
  for (const p of proposals)
    for (const c of JSON.parse(p.changes_json)) {
      const target = c.create ? rows.find(r => same(r.label, c.label, '')) : rows.find(r => r.row === c.row && c.sheet === kase.sheet);
      if (!target) {
        outside++;
        continue;
      }
      read.set(target.row, { ...read.get(target.row), ...c.values });
    }
  let total = 0,
    correct = 0,
    wrong = 0,
    missing = 0,
    filledTotal = 0,
    filledCorrect = 0;
  const errors = [];
  for (const r of rows) {
    const got = read.get(r.row);
    for (const [field, value] of Object.entries(r.values)) {
      total++;
      const has = norm(value, field) !== '';
      filledTotal += has;
      const proposed = got && Object.hasOwn(got, field) ? got[field] : undefined;
      const readValue = proposed?.formula ?? proposed;
      if (proposed === undefined) {
        if (!has) correct++;
        else {
          missing++;
          errors.push({ row: r.label, field, read: null, truth: norm(value, field), kind: 'missing' });
        }
      } else if (same(readValue, value, field)) {
        correct++;
        filledCorrect += has;
      } else {
        wrong++;
        errors.push({ row: r.label, field, read: norm(readValue, field), truth: norm(value, field), kind: 'wrong' });
      }
    }
  }
  const turns = s.turns ?? [];
  const secs = turns.length && turns.every(t => t.completed_at) ? turns.reduce((n, t) => n + (iso(t.completed_at) - iso(t.requested_at)) / 1000, 0) : null;
  Object.assign(score, { correct, total, wrong, missing, filledCorrect, filledTotal, rows: rows.length, rowsProposed: read.size, rowsOutside: outside, secs, errors, modelSelection: s.modelSelection ?? null });
  for (const e of errors) errorsCsv.push([s.case, e.row, e.field, e.read ?? '', e.truth, e.kind]);
  scores.push(score);
  appendFileSync(
    labPath('results', 'history.jsonl'),
    JSON.stringify({ runId, at: run.startedAt, model: modelName, effort: score.effort, case: s.case, state: score.state, correct, total, filledCorrect, filledTotal, wrong, missing, rowsOutside: outside, secs, snapshot: readFileSync(labPath('snapshot.taken'), 'utf8').trim() }) + '\n',
    { mode: 0o600 },
  );
}
db.close();
appDb.close();

const csv = errorsCsv.map(r => r.map(v => (/[",\n]/.test(String(v)) ? `"${String(v).replaceAll('"', '""')}"` : v)).join(',')).join('\n');
const table = [
  `# ${runId}: ${modelName}${effort ? ' · ' + effort : ''}`,
  '',
  '| case | state | correct / cells | filled cells right | wrong | missing | rows found | time |',
  '|---|---|---|---|---|---|---|---|',
  ...scores.map(s => `| ${s.case} | ${s.state} | ${s.correct}/${s.total} (${pct(s.correct, s.total)}) | ${s.filledCorrect}/${s.filledTotal} | ${s.wrong} | ${s.missing} | ${s.rowsProposed}/${s.rows}${s.rowsOutside ? ` +${s.rowsOutside}` : ''} | ${s.secs ? Math.round(s.secs) + ' s' : '–'} |`),
  '',
  `All: ${sum('correct')}/${sum('total')} (${pct(sum('correct'), sum('total'))}); filled cells ${sum('filledCorrect')}/${sum('filledTotal')} (${pct(sum('filledCorrect'), sum('filledTotal'))})`,
].join('\n');
writePrivate(join(outDir, 'scores.json'), JSON.stringify(scores, null, 1));
writePrivate(join(outDir, 'errors.csv'), csv + '\n');
writePrivate(join(outDir, 'table.md'), table + '\n');
console.log(table);
for (const s of scores) if (s.warning || s.error) console.log(`${s.case}: ${s.warning ?? s.error}`);
console.log(`\nDetails: ${outDir} (errors.csv: every wrong or missing cell)`);

function sum(key) {
  return scores.reduce((n, s) => n + (s[key] ?? 0), 0);
}
function pct(a, b) {
  return b ? `${Math.round((1000 * a) / b) / 10} %` : '–';
}

/** The results history, model × case: the latest run of each, correct/total and time. */
function printHistory() {
  const file = labPath('results', 'history.jsonl');
  if (!existsSync(file)) return console.log('No results yet');
  const lines = readFileSync(file, 'utf8').trim().split('\n').map(l => JSON.parse(l));
  const models = [...new Set(lines.map(l => `${l.model}${l.effort ? ' · ' + l.effort : ''}`))];
  const caseIds = [...new Set(lines.map(l => l.case))].sort();
  const latest = new Map();
  for (const l of lines) latest.set(`${l.model}${l.effort ? ' · ' + l.effort : ''}\u0000${l.case}`, l);
  console.log(`| model | ${caseIds.join(' | ')} | all |`);
  console.log(`|---|${caseIds.map(() => '---').join('|')}|---|`);
  for (const m of models) {
    const cells = caseIds.map(c => latest.get(`${m}\u0000${c}`));
    const ok = cells.reduce((n, l) => n + (l?.correct ?? 0), 0);
    const all = cells.reduce((n, l) => n + (l?.total ?? 0), 0);
    console.log(`| ${m} | ${cells.map(l => (l ? `${l.correct}/${l.total} ${l.secs ? Math.round(l.secs) + 's' : ''}` : '')).join(' | ')} | ${pct(ok, all)} |`);
  }
  console.log(`\n${lines.length} results in ${file}`);
}
