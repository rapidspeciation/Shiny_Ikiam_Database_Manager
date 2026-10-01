// What a Google Sheets formula gives, worked out on the app's copy of the
// workbook: enough of the language for the formulas the team copies down
// (lookups across sheets, IF/IFS, text and date helpers, arithmetic), so a
// suggested formula can say what it would show today. A formula using
// anything else gives `undefined` (unknown), never a guess.
//
//   evaluateFormula('=XLOOKUP(D12,Insectary_data!A:A,Insectary_data!P:P,"")', { sheet: 'Collection_data', row: 12, book })
//
// `book` reads cells by sheet, row and column index (0 = A):
//   { cell(sheet, row, column) → value | null, lastRow(sheet) → number, has(sheet) → boolean }
// and may cache lookup indexes in `book.indexes` (a Map, made when missing).
// Values: numbers (dates are serial numbers), strings, booleans, null (blank);
// Sheets errors are { error: '#N/A' }.

const UNSUPPORTED = Symbol('unsupported');
const unsupported = () => {
  throw UNSUPPORTED;
};
const err = code => ({ error: code });
const isErr = v => !!v && typeof v === 'object' && 'error' in v;
const NA = err('#N/A');
const VALUE = err('#VALUE!');

const letterIndex = letters => [...letters].reduce((n, c) => n * 26 + c.charCodeAt(0) - 64, 0) - 1;

/** The tokens of a formula (without its "="). */
function tokenize(text) {
  const out = [];
  let i = 0;
  const ref =
    /^(?:('(?:[^']|'')+'|[A-Za-z_][\w.]*)!)?(?:(\$?[A-Z]{1,3}\$?\d+)(?::(\$?[A-Z]{1,3}\$?\d+))?|(\$?[A-Z]{1,3}):(\$?[A-Z]{1,3}))(?![\w(])/;
  while (i < text.length) {
    const rest = text.slice(i);
    const ch = text[i];
    if (/\s/.test(ch)) {
      i++;
      continue;
    }
    if (ch === '"') {
      let j = i + 1,
        s = '';
      for (;;) {
        if (j >= text.length) unsupported();
        if (text[j] === '"') {
          if (text[j + 1] === '"') {
            s += '"';
            j += 2;
            continue;
          }
          break;
        }
        s += text[j++];
      }
      out.push({ t: 'str', v: s });
      i = j + 1;
      continue;
    }
    let m;
    if ((m = /^#(?:REF!|N\/A|VALUE!|DIV\/0!|NAME\?|NUM!|ERROR!)/i.exec(rest))) {
      out.push({ t: 'err', v: m[0].toUpperCase() });
      i += m[0].length;
      continue;
    }
    if ((m = ref.exec(rest))) {
      const sheet = m[1] ? (m[1].startsWith("'") ? m[1].slice(1, -1).replaceAll("''", "'") : m[1]) : null;
      const cell = s => {
        const c = /^\$?([A-Z]{1,3})\$?(\d+)$/.exec(s);
        return { col: letterIndex(c[1]), row: Number(c[2]) };
      };
      if (m[2]) {
        const a = cell(m[2]);
        const b = m[3] ? cell(m[3]) : a;
        out.push({ t: 'ref', sheet, c0: a.col, c1: b.col, r0: a.row, r1: b.row });
      } else
        out.push({
          t: 'ref',
          sheet,
          c0: letterIndex(m[4].replace('$', '')),
          c1: letterIndex(m[5].replace('$', '')),
          r0: null,
          r1: null,
        });
      i += m[0].length;
      continue;
    }
    if ((m = /^\d+(?:\.\d+)?(?:[eE][+-]?\d+)?/.exec(rest))) {
      out.push({ t: 'num', v: Number(m[0]) });
      i += m[0].length;
      continue;
    }
    if ((m = /^([A-Za-z_][\w.]*)\s*\(/.exec(rest))) {
      out.push({ t: 'fn', v: m[1].toUpperCase() });
      i += m[0].length;
      continue;
    }
    if ((m = /^(TRUE|FALSE)(?![\w(])/i.exec(rest))) {
      out.push({ t: 'bool', v: m[1].toUpperCase() === 'TRUE' });
      i += m[0].length;
      continue;
    }
    if ((m = /^(<>|<=|>=|[-+*/^&=<>(),;%])/.exec(rest))) {
      out.push({ t: 'op', v: m[1] === ';' ? ',' : m[1] });
      i += m[1].length;
      continue;
    }
    unsupported();
  }
  return out;
}

/** The expression tree of a formula. */
function parse(formula) {
  const text = String(formula).trim();
  if (!text.startsWith('=') || text.length < 2) unsupported();
  const tokens = tokenize(text.slice(1));
  let p = 0;
  const peek = () => tokens[p];
  const isOp = v => peek()?.t === 'op' && peek().v === v;
  const expect = v => {
    if (!isOp(v)) unsupported();
    p++;
  };
  const LEVELS = [['=', '<>', '<', '>', '<=', '>='], ['&'], ['+', '-'], ['*', '/'], ['^']];
  const binary = level => {
    if (level === LEVELS.length) return unary();
    let left = binary(level + 1);
    while (peek()?.t === 'op' && LEVELS[level].includes(peek().v)) {
      const op = tokens[p++].v;
      left = { k: 'bin', op, a: left, b: binary(level + 1) };
    }
    return left;
  };
  const unary = () => {
    if (isOp('-')) {
      p++;
      return { k: 'neg', a: unary() };
    }
    if (isOp('+')) {
      p++;
      return unary();
    }
    let node = primary();
    while (isOp('%')) {
      p++;
      node = { k: 'bin', op: '/', a: node, b: { k: 'lit', v: 100 } };
    }
    return node;
  };
  const primary = () => {
    const tok = tokens[p++];
    if (!tok) unsupported();
    if (tok.t === 'num' || tok.t === 'str' || tok.t === 'bool') return { k: 'lit', v: tok.v };
    if (tok.t === 'err') return { k: 'lit', v: err(tok.v) };
    if (tok.t === 'ref') return { k: 'ref', ...tok };
    if (tok.t === 'fn') {
      const args = [];
      if (!isOp(')'))
        for (;;) {
          // An empty argument (IF(A1,,1)) is a blank.
          args.push(isOp(',') || isOp(')') ? { k: 'lit', v: null } : binary(0));
          if (isOp(',')) {
            p++;
            continue;
          }
          break;
        }
      expect(')');
      return { k: 'fn', name: tok.v, args };
    }
    if (tok.t === 'op' && tok.v === '(') {
      const inner = binary(0);
      expect(')');
      return inner;
    }
    return unsupported();
  };
  const tree = binary(0);
  if (p !== tokens.length) unsupported();
  return tree;
}

const parsed = new Map();
function treeOf(formula) {
  if (!parsed.has(formula)) {
    if (parsed.size > 5000) parsed.clear();
    let tree;
    try {
      tree = parse(formula);
    } catch (e) {
      if (e !== UNSUPPORTED) throw e;
      tree = null;
    }
    parsed.set(formula, tree);
  }
  return parsed.get(formula);
}

// Coercions as Sheets does them.
const blank = v => v === null || v === undefined;
function toNumber(v) {
  if (isErr(v)) return v;
  if (blank(v)) return 0;
  if (typeof v === 'number') return v;
  if (typeof v === 'boolean') return v ? 1 : 0;
  const s = String(v).trim();
  return s !== '' && Number.isFinite(Number(s)) ? Number(s) : VALUE;
}
function numberText(n) {
  if (Number.isInteger(n)) return String(n);
  return String(Number(n.toPrecision(15)));
}
function toText(v) {
  if (blank(v)) return '';
  if (typeof v === 'number') return numberText(v);
  if (typeof v === 'boolean') return v ? 'TRUE' : 'FALSE';
  return String(v);
}
function toBool(v) {
  if (isErr(v)) return v;
  if (blank(v)) return false;
  if (typeof v === 'boolean') return v;
  if (typeof v === 'number') return v !== 0;
  const s = String(v).toUpperCase();
  if (s === 'TRUE') return true;
  if (s === 'FALSE' || s === '') return false;
  return VALUE;
}
/** -1, 0, 1 as Sheets orders values: numbers < text < booleans; text case-insensitively. */
function compare(a, b) {
  if (blank(a)) a = typeof b === 'string' ? '' : typeof b === 'boolean' ? false : 0;
  if (blank(b)) b = typeof a === 'string' ? '' : typeof a === 'boolean' ? false : 0;
  const rank = v => (typeof v === 'number' ? 0 : typeof v === 'string' ? 1 : 2);
  if (rank(a) !== rank(b)) return rank(a) < rank(b) ? -1 : 1;
  if (typeof a === 'string') {
    const x = a.toLowerCase(),
      y = b.toLowerCase();
    return x < y ? -1 : x > y ? 1 : 0;
  }
  return a < b ? -1 : a > b ? 1 : 0;
}
/** The key a value is found by in an exact lookup (text case-insensitively, "1" is not 1). */
const lookupKey = v =>
  blank(v) || v === ''
    ? 'blank'
    : typeof v === 'number'
      ? `n:${v}`
      : typeof v === 'boolean'
        ? `b:${v}`
        : `s:${String(v).toLowerCase()}`;

const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
const EPOCH = Date.UTC(1899, 11, 30);
/** TEXT(serial, format) for the date formats the workbook uses. */
function dateText(serial, format) {
  const d = new Date(EPOCH + Math.floor(serial) * 864e5);
  const parts = {
    yyyy: String(d.getUTCFullYear()),
    yy: String(d.getUTCFullYear()).slice(-2),
    mmm: MONTHS[d.getUTCMonth()],
    mm: String(d.getUTCMonth() + 1).padStart(2, '0'),
    m: String(d.getUTCMonth() + 1),
    dd: String(d.getUTCDate()).padStart(2, '0'),
    d: String(d.getUTCDate()),
  };
  if (!/^(?:yyyy|yy|mmm|mm|m|dd|d|[-/ .])+$/i.test(format)) return undefined;
  return format.replace(/yyyy|yy|mmm|mm|m|dd|d/gi, t => parts[t.toLowerCase()]);
}

class Evaluator {
  constructor(book, sheet, row) {
    this.book = book;
    this.sheet = sheet;
    this.row = row;
    book.indexes ??= new Map();
  }
  range(node) {
    const sheet = node.sheet ?? this.sheet;
    if (!this.book.has(sheet)) unsupported();
    const whole = node.r0 === null;
    return {
      sheet,
      c0: Math.min(node.c0, node.c1),
      c1: Math.max(node.c0, node.c1),
      r0: whole ? 1 : Math.min(node.r0, node.r1),
      r1: whole ? Math.max(1, this.book.lastRow(sheet)) : Math.max(node.r0, node.r1),
    };
  }
  /** A value from an expression: a single-cell reference gives its cell; a bigger range cannot be a value. */
  value(node) {
    if (node.k === 'ref') {
      const r = this.range(node);
      if (r.c0 !== r.c1 || r.r0 !== r.r1) unsupported();
      return this.book.cell(r.sheet, r.r0, r.c0);
    }
    return this.eval(node);
  }
  rangeArg(node) {
    if (node?.k !== 'ref') unsupported();
    return this.range(node);
  }
  /** row → first row holding each key in one column of a range (cached on the book). */
  index(r, column) {
    const key = `${r.sheet}\u0000${column}\u0000${r.r0}\u0000${r.r1}`;
    let index = this.book.indexes.get(key);
    if (!index) {
      index = { first: new Map(), count: new Map() };
      for (let row = r.r0; row <= r.r1; row++) {
        const k = lookupKey(this.book.cell(r.sheet, row, column));
        if (!index.first.has(k)) index.first.set(k, row);
        index.count.set(k, (index.count.get(k) || 0) + 1);
      }
      this.book.indexes.set(key, index);
    }
    return index;
  }
  eval(node) {
    switch (node.k) {
      case 'lit':
        return node.v;
      case 'ref':
        return this.value(node);
      case 'neg': {
        const v = toNumber(this.value(node.a));
        return isErr(v) ? v : -v;
      }
      case 'bin':
        return this.binary(node);
      case 'fn':
        return this.call(node.name, node.args);
    }
    return unsupported();
  }
  binary({ op, a, b }) {
    const x = this.value(a);
    const y = this.value(b);
    if (isErr(x)) return x;
    if (isErr(y)) return y;
    if (op === '&') return toText(x) + toText(y);
    if (['=', '<>', '<', '>', '<=', '>='].includes(op)) {
      const c = compare(x, y);
      return { '=': c === 0, '<>': c !== 0, '<': c < 0, '>': c > 0, '<=': c <= 0, '>=': c >= 0 }[op];
    }
    const n = toNumber(x),
      m = toNumber(y);
    if (isErr(n)) return n;
    if (isErr(m)) return m;
    if (op === '+') return n + m;
    if (op === '-') return n - m;
    if (op === '*') return n * m;
    if (op === '/') return m === 0 ? err('#DIV/0!') : n / m;
    if (op === '^') return n ** m;
    return unsupported();
  }
  call(name, args) {
    const v = i => this.value(args[i]);
    const has = i => i < args.length;
    const text = i => {
      const x = v(i);
      return isErr(x) ? x : toText(x);
    };
    const num = i => toNumber(v(i));
    switch (name) {
      case 'IF': {
        const c = toBool(v(0));
        if (isErr(c)) return c;
        return c ? v(1) : has(2) ? v(2) : false;
      }
      case 'IFS':
        for (let i = 0; i + 1 < args.length; i += 2) {
          const c = toBool(v(i));
          if (isErr(c)) return c;
          if (c) return v(i + 1);
        }
        return NA;
      case 'AND':
      case 'OR': {
        let out = name === 'AND';
        for (let i = 0; i < args.length; i++) {
          const c = toBool(v(i));
          if (isErr(c)) return c;
          out = name === 'AND' ? out && c : out || c;
        }
        return out;
      }
      case 'NOT': {
        const c = toBool(v(0));
        return isErr(c) ? c : !c;
      }
      case 'IFERROR': {
        const x = v(0);
        return isErr(x) ? (has(1) ? v(1) : null) : x;
      }
      case 'IFNA': {
        const x = v(0);
        return isErr(x) && x.error === '#N/A' ? v(1) : x;
      }
      case 'ISERROR':
        return isErr(v(0));
      case 'ISNA': {
        const x = v(0);
        return isErr(x) && x.error === '#N/A';
      }
      case 'ISBLANK':
        return blank(v(0));
      case 'ISTEXT':
        return typeof v(0) === 'string';
      case 'ISNUMBER':
        return typeof v(0) === 'number';
      case 'XLOOKUP': {
        if (has(4) && toNumber(v(4)) !== 0) unsupported();
        if (has(5) && toNumber(v(5)) !== 1) unsupported();
        const key = v(0);
        if (isErr(key)) return key;
        const look = this.rangeArg(args[1]);
        const result = this.rangeArg(args[2]);
        if (look.c0 !== look.c1 || result.c0 !== result.c1) unsupported();
        const row = this.index(look, look.c0).first.get(lookupKey(key));
        if (row === undefined) return has(3) ? v(3) : NA;
        return this.book.cell(result.sheet, result.r0 + (row - look.r0), result.c0);
      }
      case 'VLOOKUP': {
        if (!has(3) || toBool(v(3)) !== false) unsupported();
        const key = v(0);
        if (isErr(key)) return key;
        const r = this.rangeArg(args[1]);
        const n = num(2);
        if (isErr(n)) return n;
        if (n < 1 || r.c0 + n - 1 > r.c1) return err('#REF!');
        const row = this.index(r, r.c0).first.get(lookupKey(key));
        return row === undefined ? NA : this.book.cell(r.sheet, row, r.c0 + n - 1);
      }
      case 'MATCH': {
        if (!has(2) || toNumber(v(2)) !== 0) unsupported();
        const key = v(0);
        if (isErr(key)) return key;
        const r = this.rangeArg(args[1]);
        if (r.c0 !== r.c1) unsupported();
        const row = this.index(r, r.c0).first.get(lookupKey(key));
        return row === undefined ? NA : row - r.r0 + 1;
      }
      case 'INDEX': {
        const r = this.rangeArg(args[0]);
        const i = has(1) ? num(1) : 0;
        const j = has(2) ? num(2) : 0;
        if (isErr(i)) return i;
        if (isErr(j)) return j;
        const width = r.c1 - r.c0 + 1;
        const row = i ? r.r0 + i - 1 : r.r0 === r.r1 ? r.r0 : unsupported();
        const col = j ? r.c0 + j - 1 : width === 1 ? r.c0 : unsupported();
        if (row > r.r1 || col > r.c1) return err('#REF!');
        return this.book.cell(r.sheet, row, col);
      }
      case 'COUNTIF': {
        const r = this.rangeArg(args[0]);
        const crit = v(1);
        if (isErr(crit)) return crit;
        // Criteria with operators or wildcards ("<5", "a*") are not worked out.
        if (typeof crit === 'string' && /^[<>=]|[*?~]/.test(crit)) unsupported();
        if (blank(crit) || crit === '') unsupported();
        let n = 0;
        for (let c = r.c0; c <= r.c1; c++) n += this.index(r, c).count.get(lookupKey(crit)) || 0;
        return n;
      }
      case 'ROW':
        return has(0) ? this.rangeArg(args[0]).r0 : this.row;
      case 'TEXT': {
        const x = v(0);
        if (isErr(x)) return x;
        const format = text(1);
        if (typeof x !== 'number') return toText(x);
        const out = dateText(x, format);
        return out === undefined ? unsupported() : out;
      }
      case 'CONCATENATE':
      case 'CONCAT': {
        let out = '';
        for (let i = 0; i < args.length; i++) {
          const x = text(i);
          if (isErr(x)) return x;
          out += x;
        }
        return out;
      }
      case 'LEFT':
      case 'RIGHT': {
        const s = text(0);
        if (isErr(s)) return s;
        const n = has(1) ? num(1) : 1;
        if (isErr(n)) return n;
        return name === 'LEFT' ? s.slice(0, n) : n ? s.slice(-n) : '';
      }
      case 'MID': {
        const s = text(0);
        const start = num(1),
          n = num(2);
        for (const x of [s, start, n]) if (isErr(x)) return x;
        return s.substr(start - 1, n);
      }
      case 'LEN': {
        const s = text(0);
        return isErr(s) ? s : s.length;
      }
      case 'UPPER':
      case 'LOWER':
      case 'TRIM': {
        const s = text(0);
        if (isErr(s)) return s;
        return name === 'UPPER' ? s.toUpperCase() : name === 'LOWER' ? s.toLowerCase() : s.trim().replace(/ +/g, ' ');
      }
      case 'CHAR': {
        const n = num(0);
        return isErr(n) ? n : String.fromCharCode(n);
      }
      case 'CODE': {
        const s = text(0);
        return isErr(s) ? s : s ? s.charCodeAt(0) : VALUE;
      }
      case 'ROUND': {
        const n = num(0);
        const d = has(1) ? num(1) : 0;
        if (isErr(n)) return n;
        if (isErr(d)) return d;
        const f = 10 ** d;
        return Math.round(n * f) / f;
      }
      case 'ABS': {
        const n = num(0);
        return isErr(n) ? n : Math.abs(n);
      }
      case 'VALUE': {
        const n = num(0);
        return n;
      }
      case 'HYPERLINK':
        return has(1) ? v(1) : v(0);
      case 'DATE': {
        const [y, m, d] = [num(0), num(1), num(2)];
        for (const x of [y, m, d]) if (isErr(x)) return x;
        return Math.round((Date.UTC(y, m - 1, d) - EPOCH) / 864e5);
      }
      case 'FIND':
      case 'SEARCH': {
        const needle = text(0),
          hay = text(1);
        if (isErr(needle)) return needle;
        if (isErr(hay)) return hay;
        const from = has(2) ? num(2) : 1;
        if (isErr(from)) return from;
        const at =
          name === 'FIND' ? hay.indexOf(needle, from - 1) : hay.toLowerCase().indexOf(needle.toLowerCase(), from - 1);
        return at < 0 ? VALUE : at + 1;
      }
      case 'REGEXMATCH':
      case 'REGEXREPLACE': {
        const s = text(0);
        const re = text(1);
        if (isErr(s)) return s;
        if (isErr(re)) return re;
        let pattern;
        try {
          pattern = new RegExp(re, name === 'REGEXREPLACE' ? 'g' : '');
        } catch {
          return unsupported();
        }
        if (name === 'REGEXMATCH') return pattern.test(s);
        const by = text(2);
        return isErr(by) ? by : s.replace(pattern, by);
      }
    }
    return unsupported();
  }
}

/**
 * What `formula` gives on row `row` of `sheet`: a value (null for blank, or
 * { error }), or undefined when it uses something not worked out here.
 */
export function evaluateFormula(formula, { sheet, row, book }) {
  const tree = treeOf(formula);
  if (!tree) return undefined;
  try {
    const out = new Evaluator(book, sheet, row).value(tree);
    return out === undefined ? null : out;
  } catch (e) {
    if (e === UNSUPPORTED) return undefined;
    throw e;
  }
}

export const isFormulaError = isErr;

/** A value as the sheet shows it, for comparing a typed cell with what a formula gives. */
export function sameShown(a, b) {
  if (isErr(a) || isErr(b)) return isErr(a) && isErr(b) && a.error === b.error;
  const text = v => (blank(v) ? '' : typeof v === 'number' ? numberText(v) : String(v).trim());
  return text(a) === text(b);
}

/**
 * A book (see the header) over rows as server/checks.mjs sheetRows() gives
 * them: Map sheet → [{ row, values }], read through each sheet's column map
 * (`columnsOf(sheet)` → Map field → column index) and header row.
 * `overlay` (Map "sheet\0row\0column" → value) is read first: the values
 * formulas not written yet would give, so a formula reading the row above
 * (Data_entry_order) sees them.
 */
export function rowsBook(sheets, { columnsOf, headerRow, overlay = new Map() }) {
  const fieldsBy = new Map();
  const byRow = new Map();
  const fieldsOf = sheet => {
    if (!fieldsBy.has(sheet)) {
      const out = [];
      for (const [field, column] of columnsOf(sheet) ?? []) out[column] ??= field;
      fieldsBy.set(sheet, out);
    }
    return fieldsBy.get(sheet);
  };
  const rowsOf = sheet => {
    if (!byRow.has(sheet)) byRow.set(sheet, new Map((sheets.get(sheet) ?? []).map(r => [r.row, r])));
    return byRow.get(sheet);
  };
  return {
    overlay,
    has: sheet => sheets.has(sheet),
    lastRow: sheet => sheets.get(sheet)?.at(-1)?.row ?? 1,
    cell(sheet, row, column) {
      const key = `${sheet}\u0000${row}\u0000${column}`;
      if (overlay.has(key)) return overlay.get(key);
      const field = fieldsOf(sheet)[column];
      if (field === undefined) return null;
      if (row === headerRow(sheet)) return field;
      const value = rowsOf(sheet).get(row)?.values[field];
      return value === undefined || value === '' ? null : value;
    },
  };
}
