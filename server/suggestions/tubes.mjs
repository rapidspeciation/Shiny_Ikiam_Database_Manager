// Tube IDs with a digit too few or too many (FluidX: two letters and, for each
// prefix, the number of digits its tubes almost always have; FS3886683 typed
// for FS63886683). A digit is put back (or taken out) where the result joins
// the run of tubes already in the workbook: the readings that land next to
// tubes of the same rack (within 30) and are not used yet. Typed runs of
// consecutive rows, as a whole day typed with the same slip, are read together:
// the same correction for all of them, which must keep the run in one piece and
// touch the rack's tubes at one of its ends. One reading touching (≤ 3 away) is
// likely; several, or only near ones, need a look at the physical label.

import { msg, tpl } from '../messages.mjs';
import { TUBE_FIELD } from '../verifications.mjs';

const TUBE = /^([A-Z]{2})(\d+)$/;
/** The reason, for one tube or a run, with or without other readings close by. */
const WHY = {
  one: tpl('{tube} tiene {n} dígitos (los {prefix} tienen {length}); {fixed} queda a {gap} de tubos ya usados'),
  oneMore: tpl(
    '{tube} tiene {n} dígitos (los {prefix} tienen {length}); {fixed} queda a {gap} de tubos ya usados, y {others} lecturas más a menos de 30',
  ),
  run: tpl(
    '{tube} tiene {n} dígitos (los {prefix} tienen {length}); leídas juntas las {run} filas seguidas, {fixed} queda a {gap} de tubos ya usados',
  ),
  runMore: tpl(
    '{tube} tiene {n} dígitos (los {prefix} tienen {length}); leídas juntas las {run} filas seguidas, {fixed} queda a {gap} de tubos ya usados, y {others} lecturas más a menos de 30',
  ),
};
const NEAR = 30;
const TOUCHING = 3;

/** Every number one digit away in length: a digit put in anywhere, or one taken out. */
function readings(digits, length) {
  const out = new Set();
  if (digits.length === length - 1)
    for (let i = 0; i <= digits.length; i++) for (let d = 0; d <= 9; d++) out.add(`${digits.slice(0, i)}${d}${digits.slice(i)}`);
  if (digits.length === length + 1) for (let i = 0; i < digits.length; i++) out.add(digits.slice(0, i) + digits.slice(i + 1));
  return [...out].filter(r => r.length === length);
}
/** Where a reading came from: the same edit applied to another number of the run. */
function editOf(from, to) {
  if (to.length > from.length)
    for (let i = 0; i <= from.length; i++) if (`${from.slice(0, i)}${to[i]}${from.slice(i)}` === to) return { at: i, digit: to[i] };
  if (to.length < from.length) for (let i = 0; i < from.length; i++) if (from.slice(0, i) + from.slice(i + 1) === to) return { at: i, drop: true };
  return null;
}
const applyEdit = (digits, e) => (e.drop ? digits.slice(0, e.at) + digits.slice(e.at + 1) : `${digits.slice(0, e.at)}${e.digit}${digits.slice(e.at)}`);

/** Distance from n to the closest number in a sorted list. */
function distance(sorted, n) {
  let lo = 0,
    hi = sorted.length;
  while (lo < hi) {
    const mid = (lo + hi) >> 1;
    if (sorted[mid] < n) lo = mid + 1;
    else hi = mid;
  }
  return Math.min(lo < sorted.length ? sorted[lo] - n : Infinity, lo > 0 ? n - sorted[lo - 1] : Infinity);
}

export default {
  id: 'tubes',
  title: tpl('Tubos con un dígito de más o de menos'),
  describe: tpl(
    'Tubos FluidX con un dígito menos (o más) que los demás de su prefijo, como FS3886683 por FS63886683. Se pone (o quita) el dígito donde el tubo queda junto a los tubos de la misma gradilla que ya están en el libro; las filas seguidas escritas el mismo día se corrigen juntas. Probable si una sola lectura toca la serie; si no, revisar la etiqueta.',
  ),
  suggest(ctx) {
    // Tubes as typed (formulas copy them from another sheet): the usual length per prefix and the numbers in use.
    const cells = [];
    const lengths = new Map();
    for (const rows of ctx.sheets.values())
      for (const row of rows) {
        if (!row.observed) continue;
        for (const [field, value] of Object.entries(row.values)) {
          if (!TUBE_FIELD.test(field) || row.formulas[field] || typeof value !== 'string') continue;
          const m = TUBE.exec(value.trim());
          if (!m) continue;
          const count = lengths.get(m[1]) || lengths.set(m[1], new Map()).get(m[1]);
          count.set(m[2].length, (count.get(m[2].length) || 0) + 1);
          cells.push({ row, field, prefix: m[1], digits: m[2] });
        }
      }
    const usual = new Map(
      [...lengths].map(([prefix, count]) => {
        const [best, n] = [...count].sort((a, b) => b[1] - a[1])[0];
        const total = [...count.values()].reduce((a, b) => a + b, 0);
        return [prefix, n / total >= 0.9 && n >= 50 ? best : null];
      }),
    );
    const used = new Map();
    for (const c of cells)
      if (c.digits.length === usual.get(c.prefix)) (used.get(c.prefix) || used.set(c.prefix, []).get(c.prefix)).push(Number(c.digits));
    for (const list of used.values()) list.sort((a, b) => a - b);
    const taken = new Set(cells.map(c => `${c.prefix}${c.digits}`));

    // Odd-length tubes in runs: same sheet and column, consecutive rows, consecutive numbers.
    const odd = cells
      .filter(c => usual.get(c.prefix) && Math.abs(c.digits.length - usual.get(c.prefix)) === 1)
      .sort((a, b) => a.row.sheet.localeCompare(b.row.sheet) || a.field.localeCompare(b.field) || a.row.row - b.row.row);
    const runs = [];
    for (const c of odd) {
      const run = runs.at(-1);
      const last = run?.at(-1);
      if (
        last &&
        last.row.sheet === c.row.sheet &&
        last.field === c.field &&
        last.prefix === c.prefix &&
        last.digits.length === c.digits.length &&
        c.row.row - last.row.row <= 2 &&
        Math.abs(Number(c.digits) - Number(last.digits)) <= 3
      )
        run.push(c);
      else runs.push([c]);
    }

    const out = [];
    // Longer runs first: their likely readings join the rack, so a lone tube typed after them can touch it.
    runs.sort((a, b) => b.length - a.length);
    for (const run of runs) {
      const { prefix } = run[0];
      const length = usual.get(prefix);
      const rack = used.get(prefix) || used.set(prefix, []).get(prefix);
      // Each edit that turns the first tube into a reading, tried on the whole run.
      const options = [];
      for (const first of readings(run[0].digits, length)) {
        const edit = editOf(run[0].digits, first);
        if (!edit) continue;
        const fixed = run.map(c => applyEdit(c.digits, edit));
        if (fixed.some(f => f.length !== length || taken.has(`${prefix}${f}`))) continue;
        const nums = fixed.map(Number);
        // The run stays a run: its numbers move together.
        if (nums.some((n, i) => n - nums[0] !== Number(run[i].digits) - Number(run[0].digits))) continue;
        const gap = Math.min(...nums.map(n => distance(rack, n)));
        if (gap <= NEAR) options.push({ fixed, gap });
      }
      options.sort((a, b) => a.gap - b.gap);
      const touching = options.filter(o => o.gap <= TOUCHING);
      const best = options[0];
      const certainty = touching.length === 1 ? 'likely' : 'check';
      if (certainty === 'likely' && run.length > 1) {
        rack.push(...best.fixed.map(Number));
        rack.sort((a, b) => a - b);
        for (const f of best.fixed) taken.add(`${prefix}${f}`);
      }
      run.forEach((c, i) => {
        const current = `${prefix}${c.digits}`;
        const suggested = best ? `${prefix}${best.fixed[i]}` : null;
        out.push({
          sheet: c.row.sheet,
          row: c.row.row,
          recordId: c.row.id,
          label: c.row.label,
          field: c.field,
          current,
          suggested,
          certainty,
          reason: !best
            ? msg('{tube} tiene {n} dígitos (los {prefix} tienen {length}) y ninguna lectura queda junto a tubos ya usados', {
                tube: current,
                n: c.digits.length,
                prefix,
                length,
              })
            : msg(WHY[`${run.length > 1 ? 'run' : 'one'}${options.length > 1 ? 'More' : ''}`], {
                  tube: current,
                  n: c.digits.length,
                  prefix,
                  length,
                  run: run.length,
                  fixed: suggested,
                  gap: best.gap,
                  others: options.length - 1,
                }),
          ...(run.length > 1 ? { group: `tubes:${run[0].row.id}:${c.field}` } : {}),
        });
      });
    }
    return out;
  },
};
