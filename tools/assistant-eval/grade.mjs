// Programmatic grader for the assistant eval: a task's `checks` against what the chat did.
// Each check has a weight `w` (default 1); score = passed weight / total weight.
// Checks:
//   includes: [s..]       every string in the answer (accents, case, *_` markup and thousands dots ignored)
//   includesAny: [s..]    at least one
//   excludes: [s..]       none
//   regex: "..."          the answer matches (case-insensitive)
//   noRegex: "..."        the answer does not match
//   tool: name            the chat called that tool (MCP name without prefix) at least once
//   toolAny: [names]      one of them
//   noTool: name | [names]   none of them was called
//   maxTools: n           at most n tool calls (Bash counted)
//   noToolErrors: true    no failed tool call
//   toolArgs: { tool | [tools], match: {arg: regex} }   a call of that tool (one of them) whose arguments match
//   block: { kind: images|files, min, max, exists, src, caption, captionNot }   the reply's [images]/[files] block as the
//                         WhatsApp bridge reads it: count, local files present, src/caption regexes (every item)
//   proposal: { sheet, row | id, field, equals | matches | blank }   a drafted (not discarded) change has it
//   noApply: true         no proposal of the chat was applied
//   proposalOnly: { sheet, rows?, ids?, fields }   every drafted change stays inside these rows/ids and fields
// Text checks read the chat's final answer (the last message of the last run: what WhatsApp sends);
// `turn: n` (1-based) reads the final message of that turn instead, `scope: 'all'` every message.
// A proposal check reads the row's effective final state: the chat's live proposals folded in update order.
// `blank` passes on an untouched field only with `seedBlank: true`, which the task author verified in the seed.
import { existsSync } from 'node:fs';
import { parseReply } from '../whatsapp/bridge-lib.mjs';

const norm = s =>
  String(s ?? '')
    .normalize('NFD')
    .replace(/[̀-ͯ]/g, '')
    .replace(/[*_`]/g, '')
    .replace(/(\d)\.(\d{3})\b/g, '$1$2')
    .toLowerCase();

const SERIAL0 = Date.UTC(1899, 11, 30);
function sameDay(value, isoDay) {
  if (value == null || value === '') return false;
  if (typeof value === 'number') return new Date(SERIAL0 + value * 864e5).toISOString().slice(0, 10) === isoDay;
  const s = String(value).trim();
  if (/^\d{4}-\d{2}-\d{2}/.test(s)) return s.slice(0, 10) === isoDay;
  const m = /^(\d{1,2})\/(\d{1,2})\/(\d{4})$/.exec(s);
  if (m) return `${m[3]}-${m[2].padStart(2, '0')}-${m[1].padStart(2, '0')}` === isoDay;
  if (/^\d{5}$/.test(s)) return sameDay(Number(s), isoDay);
  return false;
}

/**
 * The effective final state of one row: the chat's live (non-discarded) proposals folded in the order they
 * were last changed, later values over earlier ones. `before` keeps the first recorded "before" of each field.
 */
function rowState(proposals, spec) {
  const values = {};
  const before = {};
  let seen = false;
  for (const p of proposals.filter(p => p.status !== 'discarded'))
    for (const c of p.changes) {
      if (c.sheet !== spec.sheet) continue;
      if (spec.row != null && c.row !== spec.row) continue;
      if (spec.id != null && String(c.label ?? '') !== spec.id && String(c.values?.Insectary_ID ?? '') !== spec.id) continue;
      seen = true;
      Object.assign(values, c.values ?? {});
      for (const [k, v] of Object.entries(c.before ?? {})) if (!Object.hasOwn(before, k)) before[k] = v;
    }
  return { seen, values, before };
}

const isBlank = v => v == null || v === '' || v === 'NA';

function proposalHas(proposals, spec, today) {
  const st = rowState(proposals, spec);
  if (!st.seen) return { pass: false, got: null };
  if (!Object.hasOwn(st.values, spec.field)) {
    if (!spec.blank) return { pass: false, got: '(field not proposed)' };
    if (spec.seedBlank) return { pass: true, got: '(untouched, blank in seed)' };
    if (Object.hasOwn(st.before, spec.field)) return { pass: isBlank(st.before[spec.field]), got: st.before[spec.field] };
    return { pass: false, got: '(untouched, seed value unknown)' };
  }
  const v = st.values[spec.field];
  if (spec.blank) return { pass: isBlank(v), got: v };
  if (spec.equals === '$TODAY') return { pass: sameDay(v, today), got: v };
  if (spec.equals != null) return { pass: norm(v).trim() === norm(spec.equals).trim(), got: v };
  if (spec.matches != null) return { pass: new RegExp(spec.matches, 'i').test(String(v)), got: v };
  return { pass: false, got: v };
}

/** A call of `spec.tool` whose arguments match every `spec.match` {key: regex} (a value that is not a string is
 * matched as JSON; a truncated input falls back to the raw text after "key":). */
function toolArgsHit(tools, spec) {
  const names = [spec.tool].flat();
  const calls = tools.filter(t => names.includes(t.name));
  if (!calls.length) return { pass: false, detail: `no ${names.join('/')} call` };
  for (const t of calls) {
    let args = null;
    try {
      args = JSON.parse(t.input ?? '');
      if (typeof args === 'string') args = JSON.parse(args);
    } catch {}
    const ok = Object.entries(spec.match ?? {}).every(([k, re]) => {
      const rx = new RegExp(re, 'i');
      if (args && typeof args === 'object') return args[k] != null && rx.test(typeof args[k] === 'string' ? args[k] : JSON.stringify(args[k]));
      const m = new RegExp(`"${k}"\\s*:\\s*("(?:[^"\\\\]|\\\\.)*"|\\[[^\\]]*\\]?|[^,}]+)`).exec(t.input ?? '');
      return m != null && rx.test(m[1]);
    });
    if (ok) return { pass: true };
  }
  return { pass: false, detail: `args ${calls.map(t => (t.input ?? '').slice(0, 120)).join(' ; ')}` };
}

/** The [images] or [files] block of the final reply, as the WhatsApp bridge reads it (parseReply): count within
 * min..max, local files present, every src matching `src`, every caption matching `caption` and none `captionNot`. */
function blockCheck(text, spec, checkFiles = true) {
  const kind = spec.kind ?? 'images';
  const got = parseReply(text, { max: 50 })[kind];
  const min = spec.min ?? 1;
  const max = spec.max ?? (kind === 'images' ? 5 : 3);
  const bad = [];
  if (got.length < min || got.length > max) bad.push(`${got.length} ${kind} (want ${min}..${max})`);
  for (const g of got) {
    if (checkFiles && spec.exists !== false && g.src.startsWith('/') && !existsSync(g.src)) bad.push(`missing file ${g.src}`);
    if (spec.src && !new RegExp(spec.src, 'i').test(g.src)) bad.push(`src ${g.src.slice(-60)}`);
    if (spec.caption && !new RegExp(spec.caption, 'i').test(g.caption)) bad.push(`caption without ${spec.caption}: ${g.caption.slice(0, 80)}`);
    if (spec.captionNot && new RegExp(spec.captionNot, 'i').test(g.caption)) bad.push(`caption claims ${spec.captionNot}: ${g.caption.slice(0, 80)}`);
  }
  return { pass: bad.length === 0, detail: bad.length ? bad.join('; ') : undefined };
}

export function grade(task, run, { today = new Date().toLocaleDateString('en-CA', { timeZone: 'America/Guayaquil' }), checkFiles = true } = {}) {
  const textFor = c => norm(c.scope === 'all' ? run.allText ?? run.finalText : c.turn ? run.turnTexts?.[c.turn - 1] : run.finalText ?? '');
  const tools = run.tools ?? [];
  const results = [];
  for (const c of task.checks ?? []) {
    const w = c.w ?? 1;
    let pass;
    let detail;
    if (c.includes) {
      const missing = c.includes.filter(s => !textFor(c).includes(norm(s)));
      pass = missing.length === 0;
      detail = missing.length ? `missing ${missing.join(', ')}` : undefined;
    } else if (c.includesAny) pass = c.includesAny.some(s => textFor(c).includes(norm(s)));
    else if (c.excludes) pass = !c.excludes.some(s => textFor(c).includes(norm(s)));
    else if (c.regex) pass = new RegExp(c.regex, 'i').test(textFor(c));
    else if (c.noRegex) pass = !new RegExp(c.noRegex, 'i').test(textFor(c));
    else if (c.tool) pass = tools.some(t => t.name === c.tool);
    else if (c.toolAny) pass = tools.some(t => c.toolAny.includes(t.name));
    else if (c.noTool) pass = !tools.some(t => [c.noTool].flat().includes(t.name));
    else if (c.maxTools != null) pass = tools.length <= c.maxTools;
    else if (c.noToolErrors) {
      const bad = tools.filter(t => t.failed);
      pass = bad.length === 0;
      detail = bad.length ? bad.map(t => t.name).join(', ') : undefined;
    } else if (c.proposal) {
      const r = proposalHas(run.proposals ?? [], c.proposal, today);
      pass = r.pass;
      detail = `got ${JSON.stringify(r.got)}`;
    } else if (c.proposalOnly) {
      const o = c.proposalOnly;
      const stray = (run.proposals ?? [])
        .filter(p => p.status !== 'discarded')
        .flatMap(p => p.changes)
        .filter(ch => ch.sheet !== o.sheet || (o.rows && !o.rows.includes(ch.row)) || (o.ids && !o.ids.includes(String(ch.label))) || Object.keys(ch.values ?? {}).some(f => !o.fields.includes(f)));
      pass = stray.length === 0;
      detail = stray.length ? `unexpected: ${stray.map(ch => `${ch.sheet}:${ch.label ?? ch.row}:${Object.keys(ch.values ?? {}).join('+')}`).join(', ')}` : undefined;
    } else if (c.noApply) pass = !(run.proposals ?? []).some(p => p.status === 'applied' || p.appliedAt);
    else if (c.toolArgs) {
      const r = toolArgsHit(tools, c.toolArgs);
      pass = r.pass;
      detail = r.pass ? undefined : r.detail;
    } else if (c.block) {
      const r = blockCheck(run.finalText ?? '', c.block, checkFiles);
      pass = r.pass;
      detail = r.detail;
    } else throw new Error(`unknown check ${JSON.stringify(c)}`);
    results.push({ check: c, w, pass, detail });
  }
  const total = results.reduce((s, r) => s + r.w, 0);
  return { score: total ? results.filter(r => r.pass).reduce((s, r) => s + r.w, 0) / total : 1, results };
}
