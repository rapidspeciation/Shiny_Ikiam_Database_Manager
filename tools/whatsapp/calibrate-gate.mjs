#!/usr/bin/env node
// Calibrates the WhatsApp gate (server/whatsapp-gate.mjs) against labelled group cases, beside the
// rule filter the relay uses without it (server/whatsapp.mjs: groupWake, camMismatches, protocolQuestion).
//   node tools/whatsapp/calibrate-gate.mjs <new output folder> <cases.jsonl>... [--repeats 2] [--db app.sqlite]
// A case: { id, group ("DM" for a DM: left out), context: [{ role, text }], incoming: [{ role, text }],
//   should_reply: "yes" | "no" | "only-if-tagged" }. yes → should wake; the rest → should not (untagged).
// A case of category "stranger" or "new_unknown_member" is a sender not on the list: the relay drops it
// before the rules and the gate, which counts as not waking.
// `--db`: an app database for the CAM check (read-only). Writes <folder>/items.jsonl and summary.json (mode 600).
import { existsSync, mkdirSync, readFileSync, readdirSync, writeFileSync } from 'node:fs';
import { isAbsolute, join, resolve } from 'node:path';
import { DatabaseSync } from 'node:sqlite';
import { camMismatches, groupWake, protocolQuestion } from '../../server/whatsapp.mjs';
import { haikuGate } from '../../server/whatsapp-gate.mjs';

const args = process.argv.slice(2);
const flag = (name, fallback = null) => {
  const i = args.indexOf(name);
  if (i < 0) return fallback;
  const v = args[i + 1];
  args.splice(i, 2);
  return v;
};
const repeats = Number(flag('--repeats', 2));
const dbFile = flag('--db');
const [outDir, ...files] = args;
if (!outDir || !files.length || !isAbsolute(outDir)) {
  console.error('Usage: calibrate-gate.mjs <new absolute output folder> <cases.jsonl>... [--repeats 2] [--db app.sqlite]');
  process.exit(2);
}
if (existsSync(outDir) && readdirSync(outDir).length) {
  console.error(`Refused: ${outDir} is not empty.`);
  process.exit(2);
}
mkdirSync(outDir, { recursive: true, mode: 0o700 });

let lookupCam = () => null;
let genera = new Set();
if (dbFile) {
  const db = new DatabaseSync(dbFile, { readOnly: true });
  const cam = db.prepare("SELECT values_json FROM records WHERE sheet = 'Collection_data' AND missing = 0 AND label = ? LIMIT 1");
  lookupCam = id => {
    const row = cam.get(id);
    if (!row) return null;
    const v = JSON.parse(row.values_json);
    return { genus: v.Genus ?? null, species: v.SPECIES ?? null, sex: v.Sex ?? null };
  };
  genera = new Set(
    db
      .prepare("SELECT DISTINCT json_extract(values_json, '$.Genus') g FROM records WHERE sheet = 'Collection_data' AND missing = 0")
      .all()
      .map(r => r.g)
      .filter(g => typeof g === 'string' && /^[A-Z][a-z]{3,}$/.test(g)),
  );
}

const cases = files.flatMap(f =>
  readFileSync(resolve(f), 'utf8')
    .split('\n')
    .filter(l => l.trim())
    .map(l => JSON.parse(l)),
);
const items = cases.filter(c => c.group !== 'DM');
const bot = { jids: [], name: /\b(ithobot|asistente|assistant)\b/i, outboxIds: () => false };
const gate = haikuGate();
const msg = text => ({ text, mentions: [], quotedId: null, quotedSender: null });

const rows = [];
for (const c of items) {
  const want = c.should_reply === 'yes';
  const lines = [...(c.context ?? [])];
  const row = { id: c.id, want, rule: null, candidate: null, gate: [] };
  if (['stranger', 'new_unknown_member'].includes(c.category)) {
    rows.push({ ...row, dropped: 'not on list', gate: Array.from({ length: repeats }, () => ({ wake: false, calls: [] })) });
    console.log(`${c.id} want=${want ? 'wake' : 'quiet'} dropped (not on list)`);
    continue;
  }
  // The relay's rules on each incoming line; the case wakes when any line does.
  for (const m of c.incoming) {
    row.rule ??= groupWake(msg(m.text), bot);
    if (!row.candidate && camMismatches(m.text, lookupCam, genera).length) row.candidate = 'cam';
    if (!row.candidate && protocolQuestion(m.text)) row.candidate = 'question';
  }
  for (let k = 0; k < repeats; k++) {
    const run = { wake: false, calls: [] };
    if (!row.rule) {
      const seen = [...lines];
      for (const m of c.incoming) {
        const r = await gate({ chatName: c.group, context: seen.slice(-8).map(l => ({ name: l.role, text: l.text })), line: { name: m.role, text: m.text } });
        run.calls.push({ wake: r.wake ?? null, reason: r.reason ?? null, error: r.error ?? null, costUsd: r.costUsd ?? null, ms: r.ms });
        if (r.wake) run.wake = true;
        seen.push(m);
      }
    } else run.wake = true;
    row.gate.push(run);
  }
  rows.push(row);
  console.log(`${c.id} want=${want ? 'wake' : 'quiet'} rule=${row.rule ?? '-'} cand=${row.candidate ?? '-'} gate=${row.gate.map(g => (g.wake ? 'W' : '.')).join('')}`);
}

const score = wakeOf => {
  const yes = rows.filter(r => r.want);
  const no = rows.filter(r => !r.want);
  const recall = yes.filter(wakeOf).length / yes.length;
  const falseWake = no.filter(wakeOf).length / no.length;
  return {
    recall: Math.round(recall * 1000) / 10,
    falseWake: Math.round(falseWake * 1000) / 10,
    missed: yes.filter(r => !wakeOf(r)).map(r => r.id),
    falseWakes: no.filter(wakeOf).map(r => r.id),
  };
};
const calls = rows.flatMap(r => r.gate.flatMap(g => g.calls));
const cost = calls.reduce((s, c) => s + (c.costUsd ?? 0), 0);
const summary = {
  items: rows.length,
  shouldWake: rows.filter(r => r.want).length,
  shouldStayQuiet: rows.filter(r => !r.want).length,
  rulesOnly: score(r => Boolean(r.rule)),
  ruleFilter: score(r => Boolean(r.rule || r.candidate)),
  gate: Array.from({ length: repeats }, (_, k) => score(r => r.gate[k].wake)),
  gateCalls: calls.length,
  gateErrors: calls.filter(c => c.error).length,
  costUsd: Math.round(cost * 1e5) / 1e5,
  costPerCallUsd: calls.length ? Math.round((cost / calls.length) * 1e6) / 1e6 : null,
  msPerCall: calls.length ? Math.round(calls.reduce((s, c) => s + c.ms, 0) / calls.length) : null,
};
writeFileSync(join(outDir, 'items.jsonl'), rows.map(r => JSON.stringify(r)).join('\n') + '\n', { mode: 0o600 });
writeFileSync(join(outDir, 'summary.json'), JSON.stringify(summary, null, 1), { mode: 0o600 });
console.log(JSON.stringify(summary, null, 1));
