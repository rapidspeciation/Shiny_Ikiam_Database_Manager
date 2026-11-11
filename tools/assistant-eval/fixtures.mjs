// Seeded fixtures of the assistant eval: cells of the eval app's sheets set before a task and put back after it.
// All cells are written first and then waited for together: `query` reads a replica that shows a save about 35 s
// later (measured 8 Nov), so writing and waiting one cell at a time cost D17 (12 cells) about 7 min before its chat
// and 7 more after it, with run.mjs silent the whole time.

const key = c => `${c.sheet}:${c.row}:${c.field}`;

/**
 * Writes `cells` ({ sheet, row, field, value }) in order with `edit(cell)` (throws on failure), then polls
 * `read(cell)` → the replica's value until every written cell shows its value (the last write of a cell counts).
 * Cells whose write failed are not waited for; the error names them after the others have been waited for.
 * → { ms, polls }
 */
export async function setCells(cells, { edit, read, tries = 40, pauseMs = 5000, sleep = ms => new Promise(r => setTimeout(r, ms)) }) {
  const t0 = Date.now();
  const want = new Map();
  const failed = [];
  for (const c of cells) {
    try {
      await edit(c);
      want.set(key(c), c);
    } catch (e) {
      want.delete(key(c));
      failed.push(`${key(c)}: ${e.message ?? e}`);
    }
  }
  let polls = 0;
  for (; want.size && polls < tries; polls++) {
    if (polls) await sleep(pauseMs);
    for (const [k, c] of [...want]) if (String(await read(c)).trim() === String(c.value ?? '').trim()) want.delete(k);
  }
  const unseen = [...want.values()].map(c => `${key(c)} never showed ${JSON.stringify(String(c.value ?? ''))}`);
  if (failed.length || unseen.length) throw new Error(`fixtures: ${[...failed, ...unseen].join('; ')}`);
  return { ms: Date.now() - t0, polls };
}
