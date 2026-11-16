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
//   anyOf: [checks]       at least one of these checks passes
//   maxTools: n           at most n tool calls (Bash counted)
//   noToolErrors: true    no failed tool call
//   okAfterErrors: name | [names]   a call of these tools that worked, after the last one of them that failed
//   toolArgs: { tool | [tools], match: {arg: regex} }   a call of that tool (one of them) whose arguments match
//   noToolArgs: { tool | [tools], match }   no such call
//   skill: name           the chat loaded that skill (a Skill call, or a Read of skills/<name>/SKILL.md)
//   noSkill: name | [names]   it loaded none of them
//   maxCalls: { tool | [tools], max }   at most `max` calls of these tools (e.g. ToolSearch round trips)
//   block: { kind: images|files, min, max, exists, src, caption, captionNot, negationOk }   the reply's [images]/[files] block as the
//                         WhatsApp bridge reads it: count, local files present, src/caption regexes (every item)
//   proposal: { sheet, row | id, field, equals | matches | blank }   a drafted (not discarded) change has it
//   noApply: true         no proposal of the chat was applied
//   proposalOnly: { sheet, rows?, ids?, fields }   every drafted change stays inside these rows/ids and fields
//   numbers: true | { allow: [n..], turn?, scope? }   every number the answer writes is in a tool result of the chat
//                         (all its calls, not only the graded ones), a prompt or `allow`; dates, times, years, IDs
//                         and codes excepted (numbersIn). A total added up in the head fails, and so do ♀/♂ parts
//                         that do not add up to the number they follow on their line (partsMismatch).
// Routing cases (tier 'routing') are graded on their first `stopAfter` tool calls only (default 3): the run is
// interrupted there (run.mjs), and calls the model started before the stop took effect are not graded.
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

/** Whether `text` matches `re`. With `negationOk`, a match whose own clause (the text since the last "(", ",", ";",
 * ".", ":" or "|", at most 40 characters back) has a negation (no, sin, ni, nunca, not, without, no muestra…) is a
 * denial, not a claim: "Planta hospedera (no muestra oviposición)" passes, "hembra poniendo huevos" does not. */
const NEGATION = /(^|[^\p{L}])(no|sin|ni|nunca|not|without|never)([^\p{L}]|$)/iu;
export function claims(text, re, negationOk = false) {
  const rx = new RegExp(re, 'gi');
  for (const m of String(text ?? '').matchAll(rx)) {
    if (!negationOk) return true;
    const before = text.slice(Math.max(0, m.index - 40), m.index);
    const clause = before.split(/[(,;.:|]/).pop();
    if (!NEGATION.test(clause)) return true;
  }
  return false;
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
    if (spec.captionNot && claims(g.caption, spec.captionNot, spec.negationOk)) bad.push(`caption claims ${spec.captionNot}: ${g.caption.slice(0, 80)}`);
  }
  return { pass: bad.length === 0, detail: bad.length ? bad.join('; ') : undefined };
}

// The number check (no AI): every number a reply writes must be one the chat's tool results (or the prompts, or
// `allow`) hold, so a total added up in the model's head fails (16 Nov 2026: «41 ♀» where the per-species counts
// 10, 13, 9, 12 and 4 make 48). Dates, times, years, IDs and codes are not numbers here.
const MONTH = '(?:ene|enero|feb|febrero|mar|marzo|abr|abril|may|mayo|jun|junio|jul|julio|ago|agosto|sep|sept|septiembre|set|oct|octubre|nov|noviembre|dic|diciembre|jan|january|february|apr|april|june|july|aug|august|september|october|november|dec|december)\\b\\.?';
const DATE_PATTERNS = [
  /https?:\/\/\S+/gi,
  /\b\d{4}-\d{1,2}-\d{1,2}(?:[T ]\d{1,2}:\d{2}(?::\d{2})?(?:\.\d+)?Z?)?/g,
  /\b\d{1,2}[/.-]\d{1,2}[/.-]\d{2,4}\b/g,
  /\b\d{1,2}\/\d{1,2}\b/g,
  /\b\d{1,2}:\d{2}(?::\d{2})?\b/g,
  // «14-oct», «1 al 14-oct», «del 1 al 14 de octubre», «31 Oct»; «octubre 31», «Oct 31».
  new RegExp(`\\b\\d{1,2}(?:\\s*(?:al|a|y|-|–|to|and)\\s*\\d{1,2})?(?:\\s*-\\s*|\\s+de\\s+|\\s+)${MONTH}`, 'gi'),
  new RegExp(`\\b${MONTH}\\s+\\d{1,2}\\b`, 'gi'),
  // A day named alone at the end of a clause: «octubre se acabó el 14.», «del 3 al 14,».
  /\b(?:el|del|al)\s+(?:[12]?\d|3[01])(?=\s*(?:[.,;:)!?]|$|\s+(?:y|al|a|hasta)\s+(?:el\s+)?\d))/gim,
];
/** A token with letters and digits together (CAM070903, A7E.1, 5VB, FS90415684, T2, n=3) is an ID or code. */
const CODE = /[\p{L}_#=][\p{L}\p{N}_.#=/-]*\p{N}[\p{L}\p{N}_.#=/-]*|\p{N}+[\p{L}_][\p{L}\p{N}_.#=/-]*/gu;
/** A number as written: 2.752 / 2,752 / 2 752 thousands, 0,8 / 0.8 decimals. */
const NUMBER = /(?<![\p{L}\p{N}.,])\d{1,3}(?:(?:[.,]|\s(?=\d{3}\b))\d{3})+(?![\p{N}]|[.,]\d)|(?<![\p{L}\p{N}.,])\d+(?:[.,]\d+)?(?![\p{L}\p{N}]|[.,]\d)/gu;
function numberValue(text) {
  const t = text.replace(/\s/g, '');
  if (/^\d{1,3}([.,])\d{3}(\1\d{3})*$/.test(t) && !/^0[.,]/.test(t)) return Number(t.replace(/[.,]/g, ''));
  return Number(t.replace(',', '.'));
}

const blank = m => ' '.repeat(m.length);
/** The text without markup, and the same text with dates, times, IDs, codes and list markers blanked (same length). */
function masked(text) {
  const plain = String(text ?? '').replace(/[*_`~]/g, '');
  let s = plain;
  for (const re of DATE_PATTERNS) s = s.replace(re, blank);
  return { plain, s: s.replace(CODE, blank).replace(/^(\s*)(\d+[.)])(?=\s)/gm, (_, a, b) => a + blank(b)) };
}

/** The numbers a text writes (text, value, index), with dates, times, years (1900–2100), IDs, codes and list markers ("1. ") left out. */
export function numbersIn(text, { years = false } = {}) {
  const { s } = masked(text);
  const out = [];
  for (const m of s.matchAll(NUMBER)) {
    const v = numberValue(m[0]);
    if (!Number.isFinite(v)) continue;
    if (!years && Number.isInteger(v) && v >= 1900 && v <= 2100 && /^\d{4}$/.test(m[0])) continue;
    out.push({ text: m[0], value: v, index: m.index });
  }
  return out;
}

const SEX_UNIT = [['female', /^(?:♀|females?\b|hembras?\b)/i], ['male', /^(?:♂|males?\b|machos?\b)/i], ['na', /^(?:sin\s+sexo\b|NA\b)/]];
/**
 * Parts against their whole, needing no data: in one line, a number followed by its ♀, ♂ (and sin sexo) parts that
 * do not add up to it («98 emergidas: 41 ♀, 49 ♂ y 1 sin sexo» is 91). Parts that add up to another number of the
 * line belong to that one. → [{ whole, parts, sum }]
 */
export function partsMismatch(text) {
  const { plain } = masked(text);
  const bad = [];
  for (const line of plain.split('\n')) {
    const nums = numbersIn(line).map(n => {
      const after = line.slice(n.index + n.text.length).replace(/^\s+/, '');
      return { ...n, unit: SEX_UNIT.find(([, re]) => re.test(after))?.[0] ?? null };
    });
    nums.forEach((whole, i) => {
      if (whole.unit || !Number.isInteger(whole.value)) return;
      const parts = [];
      for (const x of nums.slice(i + 1)) {
        if (!x.unit || parts.some(p => p.unit === x.unit)) break;
        parts.push(x);
      }
      const units = new Set(parts.map(p => p.unit));
      if (parts.length < 2 || !units.has('female') || !units.has('male') || parts[0].index - whole.index > 60) return;
      const sum = parts.reduce((n, p) => n + p.value, 0);
      if (sum !== whole.value && !nums.some(x => x.value === sum)) bad.push({ whole: whole.value, parts: parts.map(p => p.value), sum });
    });
  }
  return bad;
}

/** Every number the sources hold (tool results, prompts), as values; IDs' digits do not count. */
export function sourceNumbers(texts) {
  const set = new Set();
  for (const t of texts) for (const { value } of numbersIn(t, { years: true })) set.add(value);
  return set;
}

/**
 * `numbers` check: the numbers of the graded text that no source supports. spec: true or { allow: [numbers],
 * turn, scope } (`allow`: figures the brief or a document gives). Sources: every settled tool result of the chat
 * (`run.tools[].result`) and the prompts (`run.prompts`). → { pass, detail }.
 */
export function numbersCheck(run, text, spec = {}) {
  const allow = (spec === true ? [] : spec.allow ?? []).map(Number);
  const supported = sourceNumbers([...(run.tools ?? []).map(t => t.result ?? ''), ...(run.prompts ?? [])]);
  for (const a of allow) supported.add(a);
  const bad = numbersIn(text).filter(n => !supported.has(n.value));
  const noResults = !(run.tools ?? []).some(t => t.result);
  const parts = partsMismatch(text);
  const details = [
    ...(bad.length ? [`not in the tool results${noResults ? ' (no tool result recorded)' : ''}: ${[...new Set(bad.map(n => n.text))].join(', ')}`] : []),
    ...parts.map(p => `parts ${p.parts.join(' + ')} = ${p.sum}, not ${p.whole}`),
  ];
  return { pass: details.length === 0, detail: details.length ? details.join('; ') : undefined };
}

/** Whether a tool call loaded skill `name`: the Skill tool with that skill, or a Read of its SKILL.md. */
function loadsSkill(t, name) {
  if (t.name === 'Skill') {
    const m = /"skill"\s*:\s*"([^"]+)"/.exec(String(t.input ?? '').replace(/\\"/g, '"'));
    return m != null && m[1].replace(/^.*:/, '') === name;
  }
  return t.name === 'Read' && String(t.input ?? '').includes(`skills/${name}/SKILL.md`);
}

/** The tool calls a check sees: all of them, or a routing case's first `stopAfter`. */
export const gradedTools = (task, tools = []) => (task?.tier === 'routing' ? tools.slice(0, task.stopAfter ?? 3) : tools);

/** The graded calls the interrupt cut before they settled (run.mjs marks such a sample invalid). */
export const cutGraded = (task, tools = []) => gradedTools(task, tools).filter(t => t.cut);

/**
 * Re-grading a stored routing row (run.mjs --regrade): its run was stopped after `row.stopAfter` calls, so it is
 * graded on at most those (min of the task's k and the row's), and it is invalid when the task's k is now larger
 * than the stop (the calls after it were never made) or a graded call was cut. → { task, invalid? }
 */
export function regradeRouting(task, row, tools = []) {
  if (task?.tier !== 'routing') return { task };
  const k = task.stopAfter ?? 3;
  const ran = row.stopped ? (row.stopAfter ?? k) : Infinity;
  const t = { ...task, stopAfter: Math.min(k, ran) };
  if (k > ran) return { task: t, invalid: `stopped after ${ran} calls; the task now grades ${k}` };
  const cut = cutGraded(t, tools);
  return cut.length ? { task: t, invalid: `graded call cut by the interrupt: ${cut.map(c => c.name).join(', ')}` } : { task: t };
}

export function grade(task, run, { today = new Date().toLocaleDateString('en-CA', { timeZone: 'America/Guayaquil' }), checkFiles = true } = {}) {
  const textFor = c => norm(c.scope === 'all' ? run.allText ?? run.finalText : c.turn ? run.turnTexts?.[c.turn - 1] : run.finalText ?? '');
  const tools = gradedTools(task, run.tools ?? []);
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
    else if (c.anyOf) pass = c.anyOf.some(sub => grade({ checks: [sub] }, { ...run, tools }, { today, checkFiles }).score === 1);
    else if (c.skill) pass = tools.some(t => loadsSkill(t, c.skill));
    else if (c.noSkill) {
      const hit = [c.noSkill].flat().filter(n => tools.some(t => loadsSkill(t, n)));
      pass = hit.length === 0;
      detail = hit.length ? `loaded ${hit.join(', ')}` : undefined;
    } else if (c.maxCalls) {
      const n = tools.filter(t => [c.maxCalls.tool].flat().includes(t.name)).length;
      pass = n <= c.maxCalls.max;
      detail = pass ? undefined : `${n} calls`;
    }
    else if (c.noTool) pass = !tools.some(t => [c.noTool].flat().includes(t.name));
    else if (c.maxTools != null) pass = tools.length <= c.maxTools;
    else if (c.okAfterErrors) {
      const calls = tools.filter(t => [c.okAfterErrors].flat().includes(t.name));
      const lastFailed = calls.map(t => !!t.failed).lastIndexOf(true);
      pass = calls.some((t, i) => !t.failed && i > lastFailed);
      detail = pass ? undefined : calls.length ? 'nothing worked after the last failed call' : 'no call';
    } else if (c.noToolErrors) {
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
    } else if (c.noToolArgs) {
      pass = !toolArgsHit(tools, c.noToolArgs).pass;
      detail = pass ? undefined : 'a matching call was made';
    } else if (c.block) {
      const r = blockCheck(run.finalText ?? '', c.block, checkFiles);
      pass = r.pass;
      detail = r.detail;
    } else if (c.numbers) {
      const raw = c.scope === 'all' ? run.allText ?? run.finalText : c.turn ? run.turnTexts?.[c.turn - 1] : run.finalText;
      const r = numbersCheck({ ...run, tools: run.tools ?? [] }, raw ?? '', c.numbers);
      pass = r.pass;
      detail = r.detail;
    } else throw new Error(`unknown check ${JSON.stringify(c)}`);
    results.push({ check: c, w, pass, detail });
  }
  const total = results.reduce((s, r) => s + r.w, 0);
  return { score: total ? results.filter(r => r.pass).reduce((s, r) => s + r.w, 0) / total : 1, results };
}
