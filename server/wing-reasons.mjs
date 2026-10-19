// What the wing chain's readings of a butterfly's photos say together: each photo's row of photo_wing_sides
// (side, turn, what to look at) and the d/v pair check of the CAM, as the 18 Oct 2026 bundles worked them out
// (photo_readings_20261018 build_bundles.py, ported from ithomiini_maps scripts/dorsal_ventral/12_evaluate.py;
// no shape-anomaly or capture-sequence reasons). The turn is told when the Rename Photos app would trust it
// (checks.ts wingTurn: >= 3 wings, P(turn) >= 0.9). Used by the background readings (server/readings.mjs) for the
// CAMs whose photos it read, with the photos read before taken from the table.

import { unionBox } from './envelope-import.mjs';

const ORIENT = ['upright', 'turned 90 deg counter-clockwise', 'upside down', 'turned 90 deg clockwise'];
const SWAP_Q = 0.05;
const NAME = /^(CAM\d{5,7}|M\d{4,5})\s*[_-]?\s*(.*)$/i;
const sig = z => 1 / (1 + Math.exp(-z));
const r4 = v => (v === null || v === undefined ? null : Math.round(Number(v) * 10000) / 10000);
const f2 = v => Number(v).toFixed(2);

/** "CAM036744d (2)" → { cam: 'CAM036744', suffix: 'd (2)' }; null for other names. */
export function nameParts(stem) {
  const m = NAME.exec(String(stem ?? '').trim());
  return m ? { cam: m[1].toUpperCase(), suffix: m[2].trim().toLowerCase() } : null;
}
const labelOf = s => (s.startsWith('d') ? 1 : s.startsWith('v') ? 0 : -1);

/**
 * A stored photo_wing_sides row (its columns) → the reading wingRows takes; null for a row with an error.
 */
export function readingOfRow(row) {
  if (!row || row.error || row.side === undefined) return null;
  const parse = t => {
    try {
      return t ? JSON.parse(t) : null;
    } catch {
      return null;
    }
  };
  const wings = parse(row.wings_json);
  if (!Array.isArray(wings)) return null;
  return {
    side: row.side,
    p: row.p_dorsal,
    fore: { side: row.fore_side ?? 'absent', p: row.p_forewings },
    hind: { side: row.hind_side ?? 'absent', p: row.p_hindwings },
    wings,
    missing: parse(row.missing_json) ?? [],
    turnK: row.turn_k ?? 0,
    pTurn: row.p_turn ?? 0,
    size: parse(row.size_json),
    outlines: parse(row.outlines_json),
  };
}

/**
 * The rows of one CAM's photos. `photos`: [{ name (stem), fileId, folder, reading | error }], a reading being
 * { side, p, fore: { side, p }, hind: { side, p }, wings: [{ box (fractions as shown), kind, half, pDorsal,
 * pForewing, score }], missing, turnK, pTurn, size (as shown), outlines }. Returns { rows (importBundle's
 * `wings` rows), boxes ([name, box, conf] of photo_wing_boxes) }.
 */
export function wingRows(cam, photos, { source, model } = {}) {
  const ps = photos.map(p => {
    const suffix = nameParts(p.name)?.suffix ?? '';
    return { ...p, suffix, label: labelOf(suffix) };
  });
  const good = ps.filter(a => a.reading && !a.error);
  const lateral = ps.length > 0 && ps.every(a => /^v\d$/.test(a.suffix));
  const camReasons = [];
  const kinds = new Map();
  const labs = good.map(a => a.label);
  if (!lateral && good.length) {
    if (!labs.includes(1) || !labs.includes(0)) camReasons.push(`suffix-missing:${!labs.includes(1) ? 'no d' : ''}${!labs.includes(0) ? 'no v' : ''}`);
    if (good.length === 1) camReasons.push('single-photo');
  }
  const pair = !lateral && good.length === 2 && [...labs].sort().join() === '0,1';
  let pairOk = false;
  if (pair) {
    const d = good[0].label === 1 ? good[0] : good[1];
    const v = d === good[0] ? good[1] : good[0];
    const verdicts = {};
    for (const [g, key] of [
      ['forewings', 'fore'],
      ['hindwings', 'hind'],
    ]) {
      const pd = d.reading[key].p;
      const pv = v.reading[key].p;
      if (pd === null || pd === undefined || pv === null || pv === undefined) continue;
      const sd = d.reading[key].side;
      const sv = v.reading[key].side;
      const q = sig(Math.log(pd / (1 - pd + 1e-12) + 1e-12) - Math.log(pv / (1 - pv + 1e-12) + 1e-12));
      verdicts[g] = sd === sv && (sd === 'dorsal' || sd === 'ventral') ? `same-${sd}` : q < SWAP_Q ? 'opposite-to-suffix' : q >= 0.5 ? 'ok' : 'unsure';
    }
    const vs = new Set(Object.values(verdicts));
    pairOk = vs.size > 0 && [...vs].every(x => x === 'ok');
    const one = vs.size === 1 ? [...vs][0] : null;
    if (vs.size && one === 'opposite-to-suffix') {
      camReasons.push('suffix-swapped');
      kinds.set(d, 'suffix swapped').set(v, 'suffix swapped');
    } else if (vs.size && one?.startsWith('same')) {
      camReasons.push(`both-photos-${one.slice(5)}`);
      kinds.set(d, 'both photos same side').set(v, 'both photos same side');
    } else if ([...vs].some(x => x !== 'ok' && x !== 'unsure')) {
      const txt = Object.entries(verdicts)
        .filter(([, x]) => x !== 'ok' && x !== 'unsure')
        .map(([g, x]) => `${g} ${x.replace('same-', 'same side in both photos: ')}`)
        .join(', ');
      camReasons.push(`some-wings-not-flipped(${txt})`);
      kinds.set(d, `mixed: ${txt}`).set(v, `mixed: ${txt}`);
    }
  } else if (!lateral && good.length >= 2) {
    const decided = good.filter(x => x.reading.side === 'dorsal' || x.reading.side === 'ventral');
    const sides = new Set(decided.map(x => x.reading.side));
    if (decided.length === good.length && sides.size === 1) {
      camReasons.push(`all-photos-${[...sides][0]}`);
      for (const x of good) kinds.set(x, 'all photos same side');
    } else if (decided.length && !good.some(x => x.reading.side === 'dorsal')) camReasons.push('no-dorsal-photo');
    else if (decided.length && !good.some(x => x.reading.side === 'ventral')) camReasons.push('no-ventral-photo');
  }
  const rows = [];
  const boxes = [];
  for (const a of ps) {
    const row = { name: a.name, fileId: a.fileId, cam, suffix: a.suffix || '?', folder: a.folder ?? null, source, model };
    if (!a.reading || a.error) {
      rows.push({ ...row, error: String(a.error ?? 'not read').slice(0, 300), review: false });
      continue;
    }
    const x = a.reading;
    const trusted = x.wings.length >= 3 && x.pTurn >= 0.9 && x.turnK !== 0;
    const reasons = [];
    if (!lateral) {
      if (a.label < 0) reasons.push('no-d/v-suffix');
      if (x.side === 'mixed') {
        reasons.push(`mixed-side(fore=${f2(x.fore.p)},hind=${f2(x.hind.p)})`);
        if (!pair && !kinds.has(a)) kinds.set(a, 'mixed sides');
      } else if (x.side === 'unsure' && pairOk) {
        // the pair says it is fine
      } else if (x.side === 'unsure') {
        reasons.push(`side-unsure(p=${f2(x.p)})`);
        if (!kinds.has(a)) kinds.set(a, 'unsure');
      } else if (a.label >= 0 && x.p !== null && x.p !== undefined && (x.side === 'dorsal') !== (a.label === 1))
        reasons.push(`side-disagrees-with-suffix(p=${f2(x.p)})`);
      if (!x.wings.length) reasons.push('no-wing-found');
      else if (x.missing?.length) reasons.push(`wings=${x.wings.length}(absent:${x.missing.join('+')})`);
    }
    if (trusted) reasons.push(`orientation:${ORIENT[x.turnK]}(p=${f2(x.pTurn)})`);
    rows.push({
      ...row,
      side: lateral ? null : x.side,
      pDorsal: r4(x.p),
      pForewings: r4(x.fore.p),
      pHindwings: r4(x.hind.p),
      wings: x.wings.length,
      ...(trusted ? { turn: ORIENT[x.turnK], degrees: 90 * x.turnK } : {}),
      ...(kinds.has(a) ? { pair: kinds.get(a) } : {}),
      ...(reasons.length ? { reasons: reasons.join('; ') } : {}),
      ...(camReasons.length ? { camReasons: camReasons.join('; ') } : {}),
      review: !!(reasons.length || camReasons.length),
      foreSide: x.fore.side,
      hindSide: x.hind.side,
      turnK: x.turnK,
      pTurn: r4(x.pTurn),
      missing: x.missing ?? [],
      wingList: x.wings,
      size: x.size,
      ...(x.outlines ? { outlines: x.outlines } : {}),
    });
    const u = unionBox(x.wings.map(w => ({ ...w, conf: w.score ?? 1 })));
    if (u) boxes.push([a.name, u.box, u.conf]);
  }
  return { rows, boxes };
}
