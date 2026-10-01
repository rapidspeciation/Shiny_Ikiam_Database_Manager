// Dates that cannot be right and whose intended day can be read: the checks'
// dates that are no date or lie in the future, when the checks found no fix
// (bad_date, future_date), and typed dates before 2009 (the project's records
// start later). A year typed with one wrong digit (2926 for 2026, 2002 for
// 2022) is read back to every year from 2009 to now that differs in one digit;
// a month written in Dutch or English ("13-mrt-26") or a day without its year
// ("9/30") is read as a date. The reading closest to the dates of the rows
// around (the same column, 10 rows up and down) is suggested: likely when it is
// the only reading within 120 days of them, otherwise to check.

import { msg, tpl } from '../messages.mjs';
import { moduleMap, parseDateText } from '../schema.mjs';

const EPOCH = Date.UTC(1899, 11, 30);
const MIN_YEAR = 2009;
const serialOf = (y, m, d) => {
  const ms = Date.UTC(y, m - 1, d);
  const date = new Date(ms);
  return date.getUTCMonth() === m - 1 && date.getUTCDate() === d ? Math.round((ms - EPOCH) / 864e5) : null;
};
const partsOf = serial => {
  const d = new Date(EPOCH + Math.round(serial) * 864e5);
  return [d.getUTCFullYear(), d.getUTCMonth() + 1, d.getUTCDate()];
};
const isSerial = v => typeof v === 'number' && Number.isFinite(v);
// Month names the sheet's own reader does not know: Dutch and the longer forms.
const MONTH = { mrt: 3, mei: 5, okt: 10, sept: 9, set: 9, maa: 3 };
const ENGLISH = ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'];
const SPANISH = ['ene', 'feb', 'mar', 'abr', 'may', 'jun', 'jul', 'ago', 'sep', 'oct', 'nov', 'dic'];
const monthOf = word => {
  const w = word.toLowerCase();
  return MONTH[w] ?? MONTH[w.slice(0, 3)] ?? (ENGLISH.indexOf(w.slice(0, 3)) + 1 || SPANISH.indexOf(w.slice(0, 3)) + 1 || null);
};

/** Readings of a typed value: [{ serial, how }] with how a key of HOW. */
function readings(value, today, reference) {
  const out = [];
  const todayYear = partsOf(today)[0];
  const inRange = s => s !== null && s <= today && partsOf(s)[0] >= MIN_YEAR;
  if (isSerial(value) && value > 0 && value < 2958466) {
    const [y, m, d] = partsOf(value);
    const digits = String(y).padStart(4, '0');
    for (let i = 0; i < 4; i++)
      for (let k = 0; k <= 9; k++) {
        const year = Number(digits.slice(0, i) + k + digits.slice(i + 1));
        if (year === y || year < MIN_YEAR || year > todayYear) continue;
        const s = serialOf(year, m, d);
        if (inRange(s)) out.push({ serial: s, how: 'year' });
      }
    return out;
  }
  if (typeof value !== 'string') return out;
  const t = value.trim();
  let m;
  if ((m = /^(\d{1,2})[-/ .]([A-Za-z]{3,9})\.?[-/ .](\d{2}|\d{4})$/.exec(t))) {
    const month = monthOf(m[2]);
    const year = m[3].length === 2 ? 2000 + Number(m[3]) : Number(m[3]);
    const s = month ? serialOf(year, month, Number(m[1])) : null;
    if (inRange(s)) out.push({ serial: s, how: 'month' });
    return out;
  }
  if ((m = /^(\d{1,2})[/.-](\d{1,2})$/.exec(t))) {
    // A day without its year: day/month or month/day, in the year closest to the rows around.
    const [a, b] = [Number(m[1]), Number(m[2])];
    const near = reference ? partsOf(reference)[0] : todayYear;
    for (const [day, month] of a === b ? [[a, b]] : [[a, b], [b, a]])
      for (const year of [near - 1, near, near + 1]) {
        const s = serialOf(year, month, day);
        if (inRange(s)) out.push({ serial: s, how: 'noYear' });
      }
    return out;
  }
  const parsed = parseDateText(t);
  if (parsed && inRange(parsed)) out.push({ serial: parsed, how: 'text' });
  return out;
}
/** "NA`", "N/A.": NA with a stray character. */
const strayNA = value => typeof value === 'string' && /^\W*N\/?A\W*$/i.test(value.trim()) && value.trim() !== 'NA';

const middle = list => (list.length ? list.sort((a, b) => a - b)[list.length >> 1] : null);
const plausible = (v, today) => isSerial(v) && v <= today && partsOf(v)[0] >= MIN_YEAR;
/**
 * The day to compare with: the middle of the same column's dates in the rows
 * around, or of the row's other dates (a run of rows typed the same wrong way).
 */
function referenceDate(rows, index, field, dateFields, today) {
  const near = [];
  for (let i = Math.max(0, index - 10); i < Math.min(rows.length, index + 11); i++)
    if (i !== index && plausible(rows[i].values[field], today)) near.push(rows[i].values[field]);
  if (near.length) return middle(near);
  return middle(dateFields.filter(f => f !== field).map(f => rows[index].values[f]).filter(v => plausible(v, today)));
}

const HOW = {
  year: tpl('un dígito del año cambiado'),
  month: tpl('mes escrito en otro idioma'),
  noYear: tpl('día sin año'),
  text: tpl('fecha escrita como texto'),
};

export default {
  id: 'dates',
  title: tpl('Fechas imposibles'),
  describe: tpl(
    'Fechas que no son fechas, en el futuro o antes de 2009, cuando se puede leer la fecha que se quiso escribir: un año con un dígito cambiado (2926 por 2026), un mes en otro idioma («13-mrt-26») o un día sin año («9/30»). Se sugiere la lectura más cercana a las fechas de las filas de alrededor; probable si es la única a menos de 120 días de ellas.',
  ),
  suggest(ctx) {
    const cells = [];
    const seen = new Set();
    for (const issue of ctx.issues())
      if ((issue.kind === 'bad_date' || issue.kind === 'future_date') && !issue.fix && issue.recordId) {
        cells.push({ recordId: issue.recordId, field: issue.field });
        seen.add(`${issue.recordId}:${issue.field}`);
      }
    // Typed dates between 2000 and 2008: the checks accept them, but the project's records start later.
    const early = Math.round((Date.UTC(MIN_YEAR, 0, 1) - EPOCH) / 864e5);
    for (const [sheet, rows] of ctx.sheets) {
      const dates = moduleMap.get(sheet)?.fields.filter(f => f.type === 'date').map(f => f.key) ?? [];
      for (const row of dates.length ? rows : [])
        for (const field of dates) {
          const v = row.values[field];
          if (row.observed && !row.formulas[field] && isSerial(v) && v >= 36526 && v < early && !seen.has(`${row.id}:${field}`))
            cells.push({ recordId: row.id, field });
        }
    }
    const position = new Map();
    for (const rows of ctx.sheets.values()) rows.forEach((r, i) => position.set(r.id, i));
    const out = [];
    for (const { recordId, field } of cells) {
      const row = ctx.byId.get(recordId);
      if (!row) continue;
      const rows = ctx.sheets.get(row.sheet);
      const value = row.values[field];
      const base = { sheet: row.sheet, row: row.row, recordId: row.id, label: row.label, field };
      if (strayNA(value)) {
        out.push({ ...base, current: value, suggested: 'NA', certainty: 'likely', reason: msg('«{value}» es NA con un carácter de más', { value }) });
        continue;
      }
      const dateFields = moduleMap.get(row.sheet).fields.filter(f => f.type === 'date').map(f => f.key);
      const reference = referenceDate(rows, position.get(row.id), field, dateFields, ctx.today);
      const options = readings(value, ctx.today, reference)
        .map(o => ({ ...o, gap: reference === null ? null : Math.abs(o.serial - reference) }))
        .sort((a, b) => (a.gap ?? 0) - (b.gap ?? 0));
      const close = options.filter(o => o.gap !== null && o.gap <= 120);
      const best = options[0];
      const current = ctx.shown(row.sheet, field, value);
      // A month written out names its date; a changed digit or a missing year must land near the rows around.
      const named = best && (best.how === 'month' || best.how === 'text') && options.length === 1 && (best.gap ?? 0) <= 120;
      out.push({
        ...base,
        current: typeof value === 'number' && typeof current === 'number' ? String(current) : current,
        suggested: best ? ctx.iso(best.serial) : null,
        certainty: named || (close.length === 1 && close[0] === best) ? 'likely' : 'check',
        reason: !best
          ? msg('{field} «{value}» no se puede leer como una fecha entre 2009 y hoy', { field, value: String(current) })
          : reference === null
            ? msg('{field} «{value}»: {how}; no hay fechas cerca para comparar', {
                field,
                value: String(current),
                how: msg(HOW[best.how]),
              })
            : msg('{field} «{value}»: {how}; las filas de alrededor tienen {near} ({others} lecturas más)', {
                field,
                value: String(current),
                how: msg(HOW[best.how]),
                near: ctx.iso(reference),
                others: options.length - 1,
              }),
      });
    }
    return out;
  },
};
