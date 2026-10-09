// How large one tool answer may be. Claude Code refuses a tool result much over
// 25k tokens (answers of 60k characters and more were refused), and JSON of IDs
// and codes runs at 2–3 characters a token: every answer stays within
// RESULT_BUDGET characters. A tool that pages (find_records, review_issues, the
// history) cuts its own list where it can say how to go on; fitResult is the
// last step for every answer: it drops the trailing items of its longest lists
// and says so (`truncated`, `next`), instead of an error.

export const RESULT_BUDGET = 38000;

const size = value => JSON.stringify(value).length;

/** The most items of a list that keep build(items) within budget (0 when none fits). */
function most(total, budget, build) {
  let low = 0,
    high = total;
  while (low < high) {
    const mid = Math.ceil((low + high) / 2);
    if (size(build(mid)) <= budget) low = mid;
    else high = mid - 1;
  }
  return low;
}

/**
 * `out` with only the first items of its list `key` that fit in `budget`
 * characters (all of them when they fit). `extra(kept)` adds the fields that
 * say how to go on; it is measured too. Returns { out, kept, cut }.
 */
export function fitList(out, key, budget = RESULT_BUDGET, extra = () => ({})) {
  const list = out[key];
  if (!Array.isArray(list) || size(out) <= budget) return { out, kept: list?.length ?? 0, cut: 0 };
  const build = kept => ({ ...out, [key]: list.slice(0, kept), ...extra(kept) });
  const kept = most(list.length, budget, build);
  return { out: build(kept), kept, cut: list.length - kept };
}

const listAt = (out, [key, inner]) => (inner ? out[key][inner] : out[key]);
const withList = (out, [key, inner], list) => (inner ? { ...out, [key]: { ...out[key], [inner]: list } } : { ...out, [key]: list });

/**
 * An answer within the budget: as it is when it fits; else its longest lists
 * (its own, or those of an object it holds) cut at the end, with
 * `truncated: true` and `next` saying what was left out and how to ask for
 * it (`narrow`: how to ask for less with this tool).
 */
export function fitResult(out, { budget = RESULT_BUDGET, narrow = 'ask for less (filters, fewer columns, a smaller limit) or page with offset' } = {}) {
  if (!out || typeof out !== 'object' || Array.isArray(out) || size(out) <= budget) return out;
  const paths = [];
  for (const [key, value] of Object.entries(out))
    if (Array.isArray(value)) paths.push([key]);
    else if (value && typeof value === 'object')
      for (const [inner, list] of Object.entries(value)) if (Array.isArray(list)) paths.push([key, inner]);
  paths.sort((a, b) => size(listAt(out, b)) - size(listAt(out, a)));
  // Room for the note on what was cut.
  const room = budget - 400;
  let current = out;
  const cuts = [];
  for (const path of paths) {
    const list = listAt(current, path);
    const kept = most(list.length, room, k => withList(current, path, list.slice(0, k)));
    if (kept < list.length) {
      current = withList(current, path, list.slice(0, kept));
      cuts.push(`${path.join('.')}: the first ${kept} of ${list.length}`);
    }
    if (size(current) <= room) break;
  }
  if (size(current) > room) return { error: `The answer was too long (${size(out)} characters). To get it in parts, ${narrow}.` };
  return { ...current, truncated: true, next: `Shown: ${cuts.join('; ')}. For the rest, ${narrow}.` };
}
