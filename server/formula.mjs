// A small evaluator for the Google Sheets formulas the workbook actually uses,
// so a proposal can show what a formula cell will give once its row takes the
// proposed values (server/formula-gives.mjs). It knows the shapes found in the
// sheets' formula columns: IF, IFS, AND/OR/NOT, comparisons, &, arithmetic,
// XLOOKUP, VLOOKUP and INDEX/MATCH (exact) against other sheets, COUNTIF, ISERROR,
// IFERROR, IFNA, ISTEXT, ISBLANK, HYPERLINK (its label), text functions (LEFT,
// MID, RIGHT, LEN, FIND, SEARCH, UPPER, LOWER, TRIM, CHAR, CODE, CONCAT,
// REGEXMATCH, REGEXREPLACE), TEXT of a date, DATE, ROW, ROUND, ABS, VALUE, TRUE/FALSE.
// Anything else (an approximate MATCH, TEXT with a number format, an unknown
// function or column) throws Unsupported: the caller then keeps the sheet's own
// value. Formulas as the Sheets API gives them: English names, commas (a ";" is
// read as a comma too).
//
// Pure: the cells are read through a context { cell(sheet, col, row),
// find(sheet, col, value, r1, r2), count(sheet, col, value, r1, r2) } where
// `sheet` is null for the formula's own sheet and `col` a zero-based index.

export class Unsupported extends Error {}
/** A formula error (#N/A, #VALUE!, #REF!…), a value like the others. */
export class FormulaError {
  constructor(code) {
    this.code = code;
  }
  toString() {
    return this.code;
  }
}
const NA = new FormulaError('#N/A');
const VALUE = new FormulaError('#VALUE!');
const REF = new FormulaError('#REF!');
const DIV0 = new FormulaError('#DIV/0!');
export const isError = v => v instanceof FormulaError;

/** Zero-based index of a column's letters (A → 0, AA → 26). */
export function columnIndex(letters) {
  let n = 0;
  for (const ch of letters.toUpperCase()) n = n * 26 + (ch.charCodeAt(0) - 64);
  return n - 1;
}
const MAX_ROW = 10_000_000;

// ------------------------------------------------------------------ tokens
const CELL = String.raw`\$?[A-Za-z]{1,3}(?:\$?\d+|\{-?\d+\})`;
const COLUMN = String.raw`\$?[A-Za-z]{1,3}`;
const SHEET = String.raw`(?:'(?:[^']|'')+'|[A-Za-z_][A-Za-z0-9_.]*)!`;
const TOKEN = new RegExp(
  [
    String.raw`(?<space>\s+)`,
    String.raw`(?<str>"(?:[^"]|"")*")`,
    String.raw`(?<num>\d+(?:\.\d+)?(?:[eE][+-]?\d+)?|\.\d+)`,
    String.raw`(?<err>#(?:REF!|N\/A|VALUE!|DIV\/0!|NAME\?|NUM!|NULL!|ERROR!))`,
    // A range or a cell, maybe of another sheet: Lists!$AG$1:$AG$10, Collection_data!D:D, $Q12, U{0}.
    String.raw`(?<ref>(?:${SHEET})?(?:${CELL}(?::${CELL})?|${COLUMN}:${COLUMN})(?![A-Za-z0-9_(]))`,
    String.raw`(?<fn>[A-Za-z_][A-Za-z0-9_.]*)(?=\s*\()`,
    String.raw`(?<bool>(?:TRUE|FALSE)(?![A-Za-z0-9_]))`,
    String.raw`(?<op><>|<=|>=|[-+*/^&=<>(),;%])`,
  ].join('|'),
  'iy',
);

/**
 * A formula written relative to its own row: every row number that is not
 * absolute ($) becomes {offset} from `row` (U13001 in row 13001 → U{0}), so the
 * same formula in every row has one text (and one parse). String literals stay.
 */
export function relativeFormula(text, row) {
  return String(text)
    .split('"')
    .map((part, i) =>
      i % 2
        ? part
        : part.replace(
            /(?<![A-Za-z0-9_$.'])(\$?[A-Za-z]{1,3})(\d+)(?![A-Za-z0-9_(!])/g,
            (_, col, n) => `${col}{${Number(n) - row}}`,
          ),
    )
    .join('"');
}

function tokenize(text) {
  const out = [];
  TOKEN.lastIndex = 0;
  const source = String(text).replace(/^\s*=/, '');
  while (TOKEN.lastIndex < source.length) {
    const at = TOKEN.lastIndex;
    const m = TOKEN.exec(source);
    if (!m) throw new SyntaxError(`Unexpected "${source.slice(at, at + 12)}" at ${at + 1}`);
    const [type, value] = Object.entries(m.groups).find(([, v]) => v !== undefined);
    if (type !== 'space') out.push({ type, value, at });
  }
  return out;
}

// ------------------------------------------------------------------ parsing
function parseRef(text) {
  let sheet = null;
  let rest = text;
  const bang = text.lastIndexOf('!');
  if (bang >= 0) {
    sheet = text.slice(0, bang).replace(/^'|'$/g, '').replaceAll("''", "'");
    rest = text.slice(bang + 1);
  }
  const corner = part => {
    const m = /^\$?([A-Za-z]{1,3})(?:(\$?)(\d+)|\{(-?\d+)\})?$/.exec(part);
    return { col: columnIndex(m[1]), row: m[3] !== undefined ? { abs: Number(m[3]) } : m[4] !== undefined ? { rel: Number(m[4]) } : null };
  };
  const [a, b] = rest.split(':').map(corner);
  if (!b) return { t: 'ref', sheet, col: a.col, row: a.row };
  return { t: 'range', sheet, c1: a.col, c2: b.col, r1: a.row, r2: b.row };
}

/** The syntax tree of a formula (with or without its "="). Throws SyntaxError. */
export function parseFormula(text) {
  const tokens = tokenize(text);
  let i = 0;
  const peek = () => tokens[i];
  const isOp = (...ops) => peek()?.type === 'op' && ops.includes(peek().value);
  const expect = op => {
    if (!isOp(op)) throw new SyntaxError(`Expected "${op}"${peek() ? ` at ${peek().at + 1}` : ' at the end'}`);
    i++;
  };
  const binary = (next, ops) => () => {
    let left = next();
    while (isOp(...ops)) {
      const op = tokens[i++].value;
      left = { t: 'op', op, a: left, b: next() };
    }
    return left;
  };
  const primary = () => {
    const tok = tokens[i++];
    if (!tok) throw new SyntaxError('The formula ends too soon');
    switch (tok.type) {
      case 'num':
        return { t: 'lit', v: Number(tok.value) };
      case 'str':
        return { t: 'lit', v: tok.value.slice(1, -1).replaceAll('""', '"') };
      case 'bool':
        return { t: 'lit', v: tok.value.toUpperCase() === 'TRUE' };
      case 'err':
        return { t: 'lit', v: new FormulaError(tok.value.toUpperCase()) };
      case 'ref':
        return parseRef(tok.value);
      case 'fn': {
        expect('(');
        const args = [];
        if (!isOp(')'))
          for (;;) {
            // An empty argument (IF(A1,,"x")) is blank.
            args.push(isOp(',', ';', ')') ? { t: 'lit', v: null } : comparison());
            if (isOp(',', ';')) i++;
            else break;
          }
        expect(')');
        return { t: 'fn', name: tok.value.toUpperCase(), args };
      }
      case 'op':
        if (tok.value === '(') {
          const inner = comparison();
          expect(')');
          return inner;
        }
        if (tok.value === '-' || tok.value === '+') return { t: 'neg', sign: tok.value, a: unary() };
    }
    throw new SyntaxError(`Unexpected "${tok.value}" at ${tok.at + 1}`);
  };
  const unary = primary;
  const percent = () => {
    let a = unary();
    while (isOp('%')) {
      i++;
      a = { t: 'op', op: '/', a, b: { t: 'lit', v: 100 } };
    }
    return a;
  };
  const power = binary(percent, ['^']);
  const product = binary(power, ['*', '/']);
  const sum = binary(product, ['+', '-']);
  const concat = binary(sum, ['&']);
  const comparison = binary(concat, ['=', '<>', '<', '>', '<=', '>=']);
  const tree = comparison();
  if (i < tokens.length) throw new SyntaxError(`Unexpected "${tokens[i].value}" at ${tokens[i].at + 1}`);
  return tree;
}

const parsed = new Map();
/** parseFormula, kept for the same (relative) text. */
function treeOf(template) {
  let tree = parsed.get(template);
  if (!tree) {
    tree = parseFormula(template);
    if (parsed.size > 5000) parsed.clear();
    parsed.set(template, tree);
  }
  return tree;
}

// ------------------------------------------------------------------ values
const blank = v => v === null || v === undefined || v === '';
function toNumber(v) {
  if (isError(v)) throw v;
  if (blank(v)) return 0;
  if (typeof v === 'number') return v;
  if (typeof v === 'boolean') return v ? 1 : 0;
  const text = String(v).trim();
  if (/^-?(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?$/.test(text)) return Number(text);
  throw VALUE;
}
function toText(v) {
  if (isError(v)) throw v;
  if (blank(v)) return '';
  if (typeof v === 'boolean') return v ? 'TRUE' : 'FALSE';
  if (typeof v === 'number') return Number.isInteger(v) ? String(v) : String(Number(v.toPrecision(15)));
  return String(v);
}
function toBool(v) {
  if (isError(v)) throw v;
  if (blank(v)) return false;
  if (typeof v === 'boolean') return v;
  if (typeof v === 'number') return v !== 0;
  const text = String(v).trim().toUpperCase();
  if (text === 'TRUE') return true;
  if (text === 'FALSE') return false;
  throw VALUE;
}
const rank = v => (typeof v === 'number' ? 0 : typeof v === 'boolean' ? 2 : 1);
/** Sheets' comparison: blank is 0, "" or FALSE as the other side needs; text without case; numbers < text < booleans. */
function compare(a, b) {
  if (blank(a) && blank(b)) return 0;
  if (blank(a)) a = typeof b === 'number' ? 0 : typeof b === 'boolean' ? false : '';
  if (blank(b)) b = typeof a === 'number' ? 0 : typeof a === 'boolean' ? false : '';
  if (rank(a) !== rank(b)) return rank(a) - rank(b);
  if (typeof a === 'string') {
    const [x, y] = [a.toLowerCase(), b.toLowerCase()];
    return x < y ? -1 : x > y ? 1 : 0;
  }
  return a < b ? -1 : a > b ? 1 : 0;
}
/** The key a lookup matches on (exact match: text without case, a number only with a number). */
export const lookupKey = v =>
  blank(v) ? '' : typeof v === 'number' ? `n:${v}` : typeof v === 'boolean' ? `b:${v}` : `s:${String(v).toLowerCase()}`;

// ------------------------------------------------------------------ evaluation
/**
 * The value of `formula` written in `row` of its sheet, read through `ctx`:
 * a number, text, boolean, null (blank) or a FormulaError. Throws Unsupported
 * when it uses something this evaluator does not know.
 */
export function evaluateFormula(formula, row, ctx) {
  let tree;
  try {
    tree = treeOf(relativeFormula(formula, row));
  } catch (e) {
    throw new Unsupported(e.message);
  }
  try {
    const v = evaluate(tree, row, ctx);
    return v && typeof v === 'object' && !isError(v) ? valueOfRange(v, row, ctx) : v;
  } catch (e) {
    if (isError(e)) return e;
    throw e;
  }
}

const rowOf = (r, own) => (r ? ('abs' in r ? r.abs : own + r.rel) : null);
/** A range's bounds: whole columns run from row 1. */
function bounds(node, own) {
  const r1 = rowOf(node.r1, own) ?? 1;
  const r2 = rowOf(node.r2, own) ?? MAX_ROW;
  return { sheet: node.sheet, c1: node.c1, c2: node.c2, r1, r2 };
}
/** A range used where one value is wanted: its cell in the formula's row (a column), else #VALUE!. */
function valueOfRange(range, own, ctx) {
  if (range.c1 === range.c2 && range.r1 <= own && own <= range.r2) return ctx.cell(range.sheet, range.c1, own);
  throw VALUE;
}

function evaluate(node, own, ctx) {
  switch (node.t) {
    case 'lit':
      if (isError(node.v)) throw node.v;
      return node.v;
    case 'ref': {
      const row = rowOf(node.row, own);
      if (row === null) return { sheet: node.sheet, c1: node.col, c2: node.col, r1: 1, r2: MAX_ROW };
      if (row < 1) throw REF;
      const v = ctx.cell(node.sheet, node.col, row);
      if (isError(v)) throw v;
      return v;
    }
    case 'range':
      return bounds(node, own);
    case 'neg': {
      const n = toNumber(scalar(node.a, own, ctx));
      return node.sign === '-' ? -n : n;
    }
    case 'op':
      return operate(node.op, scalar(node.a, own, ctx), scalar(node.b, own, ctx));
    case 'fn':
      return call(node.name, node.args, own, ctx);
  }
  throw new Unsupported(`node ${node.t}`);
}
function scalar(node, own, ctx) {
  const v = evaluate(node, own, ctx);
  return v && typeof v === 'object' && !isError(v) ? valueOfRange(v, own, ctx) : v;
}
function rangeArg(node, own, ctx) {
  const v = evaluate(node, own, ctx);
  if (!v || typeof v !== 'object' || isError(v)) throw new Unsupported('a range was expected');
  return v;
}

function operate(op, a, b) {
  switch (op) {
    case '&':
      return toText(a) + toText(b);
    case '+':
      return toNumber(a) + toNumber(b);
    case '-':
      return toNumber(a) - toNumber(b);
    case '*':
      return toNumber(a) * toNumber(b);
    case '/': {
      const d = toNumber(b);
      if (d === 0) throw DIV0;
      return toNumber(a) / d;
    }
    case '^':
      return toNumber(a) ** toNumber(b);
  }
  if (isError(a)) throw a;
  if (isError(b)) throw b;
  const c = compare(a, b);
  return { '=': c === 0, '<>': c !== 0, '<': c < 0, '>': c > 0, '<=': c <= 0, '>=': c >= 0 }[op];
}

/** The first row of a one-column range holding `value` (exact), or null. */
function findIn(range, value, ctx) {
  if (range.c1 !== range.c2) throw new Unsupported('a lookup over several columns');
  return ctx.find(range.sheet, range.c1, value, range.r1, range.r2);
}

function call(name, args, own, ctx) {
  const arg = i => (args[i] ? scalar(args[i], own, ctx) : null);
  const need = (min, max = min) => {
    if (args.length < min || args.length > max) throw new Unsupported(`${name} with ${args.length} arguments`);
  };
  const caught = i => {
    try {
      return arg(i);
    } catch (e) {
      if (isError(e)) return e;
      throw e;
    }
  };
  switch (name) {
    case 'IF':
      need(2, 3);
      return toBool(arg(0)) ? arg(1) : args.length > 2 ? arg(2) : false;
    case 'IFS':
      if (args.length < 2 || args.length % 2) throw new Unsupported('IFS with an odd number of arguments');
      for (let i = 0; i < args.length; i += 2) if (toBool(arg(i))) return arg(i + 1);
      throw NA;
    case 'AND':
    case 'OR': {
      if (!args.length) throw new Unsupported(name);
      const all = args.map((_, i) => toBool(arg(i)));
      return name === 'AND' ? all.every(Boolean) : all.some(Boolean);
    }
    case 'NOT':
      need(1);
      return !toBool(arg(0));
    case 'TRUE':
    case 'FALSE':
      need(0);
      return name === 'TRUE';
    case 'IFERROR': {
      need(1, 2);
      const v = caught(0);
      return isError(v) ? arg(1) : v;
    }
    case 'IFNA': {
      need(2);
      const v = caught(0);
      if (isError(v) && v.code === '#N/A') return arg(1);
      if (isError(v)) throw v;
      return v;
    }
    case 'ISERROR':
      need(1);
      return isError(caught(0));
    case 'ISNA': {
      need(1);
      const v = caught(0);
      return isError(v) && v.code === '#N/A';
    }
    case 'ISBLANK':
      need(1);
      return blank(arg(0)) && arg(0) !== '';
    case 'ISTEXT': {
      need(1);
      const v = caught(0);
      return typeof v === 'string' && v !== '';
    }
    case 'ISNUMBER':
      need(1);
      return typeof caught(0) === 'number';
    case 'HYPERLINK':
      need(1, 2);
      return args.length > 1 ? arg(1) : arg(0);
    case 'CONCATENATE':
      return args.map((_, i) => toText(arg(i))).join('');
    case 'UPPER':
      need(1);
      return toText(arg(0)).toUpperCase();
    case 'LOWER':
      need(1);
      return toText(arg(0)).toLowerCase();
    case 'TRIM':
      need(1);
      return toText(arg(0)).trim().replace(/ {2,}/g, ' ');
    case 'LEN':
      need(1);
      return toText(arg(0)).length;
    case 'LEFT':
    case 'RIGHT': {
      need(1, 2);
      const text = toText(arg(0));
      const n = args.length > 1 ? toNumber(arg(1)) : 1;
      if (n < 0) throw VALUE;
      return name === 'LEFT' ? text.slice(0, n) : n ? text.slice(-n) : '';
    }
    case 'MID': {
      need(3);
      const start = toNumber(arg(1));
      const n = toNumber(arg(2));
      if (start < 1 || n < 0) throw VALUE;
      return toText(arg(0)).substr(start - 1, n);
    }
    case 'FIND': {
      need(2, 3);
      const at = toText(arg(1)).indexOf(toText(arg(0)), (args.length > 2 ? toNumber(arg(2)) : 1) - 1);
      if (at < 0) throw VALUE;
      return at + 1;
    }
    case 'CHAR':
      need(1);
      return String.fromCharCode(toNumber(arg(0)));
    case 'CODE': {
      need(1);
      const text = toText(arg(0));
      if (!text) throw VALUE;
      return text.charCodeAt(0);
    }
    case 'DATE': {
      need(3);
      const ms = Date.UTC(toNumber(arg(0)), toNumber(arg(1)) - 1, toNumber(arg(2)));
      return Math.round((ms - Date.UTC(1899, 11, 30)) / 864e5);
    }
    case 'XLOOKUP': {
      need(3, 6);
      // Exact match (0) searched from the first row (1), the defaults: the only ones worked out.
      if (args.length > 4 && toNumber(arg(4)) !== 0) throw new Unsupported('XLOOKUP with another match mode');
      if (args.length > 5 && toNumber(arg(5)) !== 1) throw new Unsupported('XLOOKUP with another search mode');
      const key = arg(0);
      const look = rangeArg(args[1], own, ctx);
      const result = rangeArg(args[2], own, ctx);
      if (result.c1 !== result.c2) throw new Unsupported('XLOOKUP returning several columns');
      const at = findIn(look, key, ctx);
      if (at === null) {
        if (args.length > 3) return arg(3);
        throw NA;
      }
      return ctx.cell(result.sheet, result.c1, result.r1 + (at - look.r1));
    }
    case 'VLOOKUP': {
      need(4);
      if (toBool(arg(3)) !== false) throw new Unsupported('VLOOKUP without exact match');
      const key = arg(0);
      const range = rangeArg(args[1], own, ctx);
      const n = toNumber(arg(2));
      if (n < 1 || range.c1 + n - 1 > range.c2) throw REF;
      const at = findIn({ ...range, c2: range.c1 }, key, ctx);
      if (at === null) throw NA;
      return ctx.cell(range.sheet, range.c1 + n - 1, at);
    }
    case 'ROW': {
      need(0, 1);
      if (!args.length) return own;
      const node = args[0];
      if (node.t === 'ref' && node.row) return rowOf(node.row, own);
      if (node.t === 'range') return bounds(node, own).r1;
      throw new Unsupported('ROW of a whole column');
    }
    case 'TEXT': {
      need(2);
      const v = arg(0);
      const format = toText(arg(1));
      if (typeof v !== 'number') return toText(v);
      const out = dateText(v, format);
      if (out === undefined) throw new Unsupported(`TEXT with the format ${format}`);
      return out;
    }
    case 'CONCAT':
      need(2);
      return toText(arg(0)) + toText(arg(1));
    case 'ROUND': {
      need(1, 2);
      const f = 10 ** (args.length > 1 ? toNumber(arg(1)) : 0);
      return Math.round(toNumber(arg(0)) * f) / f;
    }
    case 'ABS':
      need(1);
      return Math.abs(toNumber(arg(0)));
    case 'VALUE':
      need(1);
      return toNumber(arg(0));
    case 'SEARCH': {
      need(2, 3);
      const at = toText(arg(1)).toLowerCase().indexOf(toText(arg(0)).toLowerCase(), (args.length > 2 ? toNumber(arg(2)) : 1) - 1);
      if (at < 0) throw VALUE;
      return at + 1;
    }
    case 'REGEXMATCH':
    case 'REGEXREPLACE': {
      need(name === 'REGEXMATCH' ? 2 : 3);
      let pattern;
      try {
        pattern = new RegExp(toText(arg(1)), name === 'REGEXREPLACE' ? 'g' : '');
      } catch {
        throw new Unsupported(`${name} with this expression`);
      }
      return name === 'REGEXMATCH' ? pattern.test(toText(arg(0))) : toText(arg(0)).replace(pattern, toText(arg(2)));
    }
    case 'MATCH': {
      need(2, 3);
      if (args.length < 3 || toNumber(arg(2)) !== 0) throw new Unsupported('MATCH without exact match');
      const look = rangeArg(args[1], own, ctx);
      const at = findIn(look, arg(0), ctx);
      if (at === null) throw NA;
      return at - look.r1 + 1;
    }
    case 'INDEX': {
      need(2, 3);
      const range = rangeArg(args[0], own, ctx);
      const n = toNumber(arg(1));
      const c = args.length > 2 ? toNumber(arg(2)) : 1;
      if (range.c1 !== range.c2 && args.length < 3) throw new Unsupported('INDEX of several columns without a column');
      if (n < 1 || range.r1 + n - 1 > range.r2 || c < 1 || range.c1 + c - 1 > range.c2) throw REF;
      return ctx.cell(range.sheet, range.c1 + c - 1, range.r1 + n - 1);
    }
    case 'COUNTIF': {
      need(2);
      const range = rangeArg(args[0], own, ctx);
      const criterion = arg(1);
      if (typeof criterion === 'string' && /^\s*[<>=]/.test(criterion)) throw new Unsupported('COUNTIF with an operator');
      if (typeof criterion === 'string' && /[*?~]/.test(criterion)) throw new Unsupported('COUNTIF with wildcards');
      if (range.c1 !== range.c2) throw new Unsupported('COUNTIF over several columns');
      return ctx.count(range.sheet, range.c1, criterion, range.r1, range.r2);
    }
  }
  throw new Unsupported(`function ${name}`);
}
const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
/** TEXT(serial, format) for date formats (dd-mmm-yy, yyyy-mm-dd, d/m/yyyy…); undefined for any other format. */
function dateText(serial, format) {
  if (!/^(?:yyyy|yy|mmm|mm|m|dd|d|[-/ .])+$/i.test(format)) return undefined;
  const d = new Date(Date.UTC(1899, 11, 30) + Math.floor(serial) * 864e5);
  const parts = {
    yyyy: String(d.getUTCFullYear()),
    yy: String(d.getUTCFullYear()).slice(-2),
    mmm: MONTHS[d.getUTCMonth()],
    mm: String(d.getUTCMonth() + 1).padStart(2, '0'),
    m: String(d.getUTCMonth() + 1),
    dd: String(d.getUTCDate()).padStart(2, '0'),
    d: String(d.getUTCDate()),
  };
  return format.replace(/yyyy|yy|mmm|mm|m|dd|d/gi, t => parts[t.toLowerCase()]);
}

/** A formula's value as a cell shows it: errors as their code (#N/A), blank as null. */
export const shownResult = v => (isError(v) ? v.code : v === '' || v === undefined ? null : v);
