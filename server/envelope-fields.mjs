// What a butterfly's envelope says, as fields: the full-text OCR lines of an archive photo
// (envelope_readings.text_json: [[text, confidence], …], server/envelope-import.mjs) read into
// species (matched against the workbook's own names), sex, locality (against the workbook's
// localities), dated events, the insectary code, a CRISPR number and the specimen ID; and the
// species, sex and CAM_ID corrections written in the sheets' notes. The OCR is right on about nine
// lines in ten, so every field keeps its confidence (the line's OCR confidence times how close the
// match to a workbook name is) and the lines it came from.

const fold = value =>
  String(value ?? '')
    .normalize('NFD')
    .replace(/[̀-ͯ]/g, '')
    .toLowerCase();
const round = (n, d = 2) => Math.round(n * 10 ** d) / 10 ** d;
const NONE = /^(?:na|n\/a|none|null|not_found|not_provided|not_collected|unknown|-)?$/i;

/** 1 − edit distance / longer length (1: the same, 0: nothing alike); below `floor` it may answer 0 early. */
export function similarity(a, b, floor = 0) {
  if (a === b) return 1;
  const [n, m] = [a.length, b.length];
  if (!n || !m) return 0;
  const longest = Math.max(n, m);
  const budget = Math.floor((1 - floor) * longest);
  if (Math.abs(n - m) > budget) return 0;
  // A lower bound first (letters one has and the other lacks, counted in 32 bins): most pairs end here.
  if (floor > 0) {
    BINS.fill(0);
    for (let i = 0; i < n; i++) BINS[a.charCodeAt(i) & 31]++;
    for (let j = 0; j < m; j++) BINS[b.charCodeAt(j) & 31]--;
    let more = 0,
      less = 0;
    for (let k = 0; k < 32; k++) BINS[k] > 0 ? (more += BINS[k]) : (less -= BINS[k]);
    if (Math.max(more, less) > budget) return 0;
  }
  if (ROW_A.length <= m) [ROW_A, ROW_B] = [new Int32Array(m * 2 + 2), new Int32Array(m * 2 + 2)];
  let prev = ROW_A,
    row = ROW_B;
  for (let j = 0; j <= m; j++) prev[j] = j;
  for (let i = 1; i <= n; i++) {
    row[0] = i;
    let best = i;
    const ca = a.charCodeAt(i - 1);
    for (let j = 1; j <= m; j++) {
      const v = Math.min(prev[j] + 1, row[j - 1] + 1, prev[j - 1] + (ca === b.charCodeAt(j - 1) ? 0 : 1));
      row[j] = v;
      if (v < best) best = v;
    }
    if (best > budget) return 0;
    [prev, row] = [row, prev];
  }
  return 1 - prev[m] / longest;
}
const BINS = new Int32Array(32);
let ROW_A = new Int32Array(64);
let ROW_B = new Int32Array(64);

// ------------------------------------------------------------ the workbook's names
/**
 * The names the envelopes are matched against.
 *  - taxa: [{ name: "Genus species" or "Genus species subspecies…", n }] (n: rows using it, 0 for a
 *    name only in the taxonomy list); "Genus sp." and genus-only names give the genus.
 *  - localities: [{ name, n }].
 */
export function buildLexicon({ taxa = [], localities = [] } = {}) {
  const genera = new Map();
  const epithets = new Map();
  const genusOf = name => {
    const key = fold(name);
    return genera.get(key) ?? genera.set(key, { name, key, n: 0, species: new Map() }).get(key);
  };
  for (const { name, n = 0 } of taxa) {
    const words = String(name ?? '').trim().split(/\s+/).filter(Boolean);
    if (!words.length || !/^[A-Z][a-z]+$/.test(words[0])) continue;
    const genus = genusOf(words[0]);
    genus.n += n;
    if (words.length < 2 || /^sp\.?$|^spp\.?$|^cf\.?$/i.test(words[1]) || !/^[a-z-]+$/.test(words[1])) continue;
    const key = fold(words[1]);
    let species = genus.species.get(key);
    if (!species) {
      species = { name: `${genus.name} ${words[1]}`, epithet: key, genus, n: 0, subs: new Map() };
      genus.species.set(key, species);
      (epithets.get(key) ?? epithets.set(key, []).get(key)).push(species);
    }
    species.n += n;
    const rest = words.slice(2).join(' ');
    if (rest && !NONE.test(rest)) {
      const sub = fold(rest);
      const entry = species.subs.get(sub) ?? species.subs.set(sub, { name: `${species.name} ${rest}`, key: sub, n: 0 }).get(sub);
      entry.n += n;
    }
  }
  const places = [];
  const seen = new Map();
  for (const { name, n = 0 } of localities) {
    const text = String(name ?? '').trim();
    if (!text || NONE.test(text)) continue;
    const key = fold(text);
    if (seen.has(key)) {
      seen.get(key).n += n;
      continue;
    }
    const place = { name: text, key, tokens: words(key), n };
    seen.set(key, place);
    places.push(place);
  }
  // Each word of a subspecies → the subspecies having it.
  const subWords = new Map();
  for (const genus of genera.values())
    for (const species of genus.species.values())
      for (const sub of species.subs.values())
        for (const w of words(sub.key)) if (w.length >= 3) (subWords.get(w) ?? subWords.set(w, []).get(w)).push({ species, sub });
  return { genera, epithets, subWords, places, memo: new Map() };
}

/** The words of a folded text (letters, digits, abbreviations with their dot). */
const words = text =>
  String(text)
    .replace(/[,;:/()[\]{}"'*?!¿¡]+/g, ' ')
    .split(/[\s-]+/)
    .map(w => w.replace(/^[-.]+|-+$/g, ''))
    .filter(w => w && w !== '-');

const memo = (lexicon, key, make) => {
  if (lexicon.memo.has(key)) return lexicon.memo.get(key);
  const value = make();
  if (lexicon.memo.size > 200_000) lexicon.memo.clear();
  lexicon.memo.set(key, value);
  return value;
};

/**
 * The keys of a map that can come within `floor` of a word: of a length within reach, and not lacking
 * more letters than the edits allowed (letters counted in 32 bins, as in similarity), from the keys
 * grouped by length once with their counts.
 */
function nearKeys(lexicon, name, map, word, floor) {
  let byLength = lexicon.byLength?.[name];
  if (!byLength) {
    byLength = [];
    for (const key of map.keys()) {
      const bins = new Int16Array(32);
      for (let i = 0; i < key.length; i++) bins[key.charCodeAt(i) & 31]++;
      (byLength[key.length] ??= []).push([key, bins]);
    }
    (lexicon.byLength ??= {})[name] = byLength;
  }
  const mine = new Int16Array(32);
  for (let i = 0; i < word.length; i++) mine[word.charCodeAt(i) & 31]++;
  const out = [];
  for (let n = Math.ceil(word.length * floor); n <= Math.floor(word.length / floor); n++) {
    const budget = Math.floor((1 - floor) * Math.max(n, word.length));
    for (const [key, bins] of byLength[n] ?? []) {
      let more = 0,
        less = 0;
      for (let k = 0; k < 32 && more <= budget && less <= budget; k++) {
        const d = mine[k] - bins[k];
        if (d > 0) more += d;
        else less -= d;
      }
      if (more <= budget && less <= budget) out.push(key);
    }
  }
  return out;
}

/** Only letters (OCR of a scribble, codes and numbers are no names). */
const nameLike = word => /^[a-z]+$/.test(word);

function genusMatch(lexicon, word) {
  if (word.length < 4 || !nameLike(word)) return null;
  return memo(lexicon, `g|${word}`, () => {
    let best = null;
    for (const key of nearKeys(lexicon, 'genera', lexicon.genera, word, 0.75)) {
      const genus = lexicon.genera.get(key);
      const sim = similarity(word, genus.key, 0.7);
      if (sim >= 0.75 && (!best || sim > best.sim || (sim === best.sim && genus.n > best.genus.n))) best = { genus, sim };
    }
    return best;
  });
}
/** The species of `genus` a word reads as: { species, sim } or null. */
function epithetMatch(genus, word) {
  if (word.length < 3) return null;
  let best = null;
  for (const species of genus.species.values()) {
    const sim = similarity(word, species.epithet, 0.65);
    if (sim >= 0.7 && (!best || sim > best.sim || (sim === best.sim && species.n > best.species.n))) best = { species, sim };
  }
  return best;
}
/** A species epithet read without its genus: the species of any genus, the one in use most. */
function anyEpithet(lexicon, word) {
  if (word.length < 5 || !nameLike(word)) return null;
  return memo(lexicon, `e|${word}`, () => {
    let best = null;
    for (const key of nearKeys(lexicon, 'epithets', lexicon.epithets, word, 0.85)) {
      const list = lexicon.epithets.get(key);
      const sim = similarity(word, key, 0.8);
      if (sim < 0.85) continue;
      for (const species of list)
        if (!best || sim > best.sim || (sim === best.sim && species.n > best.species.n)) best = { species, sim };
    }
    return best;
  });
}
/** How close a word is to a word of any subspecies (0 below 0.85). */
function anySub(lexicon, word) {
  if (word.length < 3 || !nameLike(word)) return 0;
  // Three letters ("ida"): only as written.
  if (word.length === 3) return lexicon.subWords.has(word) ? 1 : 0;
  return memo(lexicon, `s|${word}`, () => {
    let best = 0;
    for (const key of nearKeys(lexicon, 'subWords', lexicon.subWords, word, 0.85)) {
      const sim = similarity(word, key, 0.8);
      if (sim > best) best = sim;
    }
    return best >= 0.85 ? best : 0;
  });
}
/** A subspecies read without genus and species ("intermedia"): the species that has it and uses it most, { species, sub, sim } or null. */
function subAlone(lexicon, word) {
  if (word.length < 5 || !nameLike(word)) return null;
  return memo(lexicon, `a|${word}`, () => {
    let best = null;
    for (const key of nearKeys(lexicon, 'subWords', lexicon.subWords, word, 0.9)) {
      const list = lexicon.subWords.get(key);
      const sim = similarity(word, key, 0.85);
      if (sim < 0.9) continue;
      for (const hit of list)
        if (hit.sub.key === key && (!best || sim > best.sim || (sim === best.sim && hit.sub.n > best.sub.n))) best = { ...hit, sim };
    }
    return best;
  });
}
/** The subspecies of `species` the words read as (one word, or several: "werneri x proceriformis"). */
function subMatch(species, list) {
  if (!species.subs.size || !list.length) return null;
  let best = null;
  const tries = new Set(list.map(w => w.text));
  for (let i = 0; i < list.length; i++)
    for (let j = i + 2; j <= Math.min(list.length, i + 3); j++) tries.add(list.slice(i, j).map(w => w.text).join(' '));
  for (const text of tries) {
    if (text.length < 3 || text === 'x') continue;
    for (const sub of species.subs.values()) {
      const sim = similarity(text, sub.key, 0.65);
      if (sim >= 0.75 && (!best || sim > best.sim || (sim === best.sim && sub.n > best.sub.n) || (sim === best.sim && sub.key.length > best.sub.key.length)))
        best = { sub, sim, text };
    }
  }
  if (!best) return null;
  const used = new Set(best.text.split(' '));
  return { ...best, words: list.filter(w => used.has(w.text)) };
}

/** How well a line reads as one of the workbook's localities: { place, score } or null. */
export function localityMatch(lexicon, text) {
  const key = fold(text).replace(/\s+/g, ' ').trim();
  if (key.replace(/[^a-z]/g, '').length < 3) return null;
  return memo(lexicon, `l|${key}`, () => {
    const mine = words(key);
    let best = null;
    for (const place of lexicon.places) {
      const whole = similarity(key, place.key, 0.6);
      // Word by word: the line's words found among the place's (an abbreviation "pto." matching "puerto"),
      // and how much of the place's name the line covers.
      let matched = 0;
      let weight = 0;
      const covered = new Set();
      for (const w of mine) {
        const stem = w.endsWith('.') ? w.slice(0, -1) : null;
        let top = 0;
        let at = -1;
        place.tokens.forEach((t, i) => {
          const s = stem && stem.length >= 1 && t.startsWith(stem) && t.length > stem.length ? 0.9 : similarity(w, t, 0.6);
          if (s > top) [top, at] = [s, i];
        });
        weight += w.length;
        if (top >= 0.7) {
          matched += top * w.length;
          covered.add(at);
        }
      }
      const significant = place.tokens.filter(t => t.length > 2 || /\d/.test(t));
      const coverage = significant.length ? significant.filter(t => covered.has(place.tokens.indexOf(t))).length / significant.length : 0;
      const byWords = weight ? (matched / weight) * (0.75 + 0.25 * coverage) : 0;
      const score = Math.max(whole, byWords);
      if (score >= 0.7 && (!best || score > best.score + 1e-9 || (Math.abs(score - best.score) < 1e-9 && place.n > best.place.n))) best = { place, score };
    }
    return best;
  });
}

// ------------------------------------------------------------ the lines
const MONTHS = { jan: 1, ene: 1, feb: 2, mar: 3, apr: 4, abr: 4, may: 5, jun: 6, jul: 7, aug: 8, ago: 8, sep: 9, set: 9, oct: 10, nov: 11, dec: 12, dic: 12 };
const DATE = /(?<![\d/])(\d{1,2})(?:\s*[/.-]\s*(\d{1,2})\s*[/.-]\s*|\s*[/.\s-]\s*([A-Za-z]{3,4})\.?\s*[/.\s-]\s*)(\d{4}|\d{2})(?![\d])/g;
/** "3/12/22", "14.1.2022", "9/Sep/2024", "30/AUG/2025" (day first) → "2022-12-03", or null. */
export function isoDate(day, month, year) {
  const m = /^\d+$/.test(month) ? Number(month) : MONTHS[month.slice(0, 3).toLowerCase()];
  let y = Number(year);
  if (year.length === 2) y += 2000;
  const d = Number(day);
  if (!m || y < 2000 || y > 2035) return null;
  const date = new Date(Date.UTC(y, m - 1, d));
  return date.getUTCMonth() === m - 1 && date.getUTCDate() === d ? date.toISOString().slice(0, 10) : null;
}
/** What a date on a line is, from the words before it. */
const DATE_KIND = [
  [/wing\s*-?\s*cl?ip|wingclip|ala\s*cortada/i, 'wingClip'],
  [/p?reserv/i, 'preserved'],
  [/death|muert/i, 'death'],
  [/emerg/i, 'emerged'],
  [/inject/i, 'injected'],
];
const kindOf = text => DATE_KIND.find(([re]) => re.test(text))?.[1] ?? null;
const SPECIMEN_ID = /(?:\bC\s?|\b)A\s?M\s?([0-9O]{5,7})\b|\b([A-Z]{2}\d{2})\s?[-_]\s?(\d{1,4})\b/g;
const CODE = [
  /reared\s*id\s*[:.]?\s*([A-Z0-9]{2,5})/i,
  /\(\s*([A-Z]{1,2}\s?\d{1,3}|\d{1,2}\s?[A-Z]{1,3})\s*\)?\s*$/,
  /^\s*\(?\s*ID\s*[:.]\s*([A-Z0-9]{2,5})\s*\)?\s*$/i,
];

/**
 * One photo's envelope as fields. `lines`: [[text, confidence], …] in the order read; `camid`: the
 * CAM_ID reader's answer for the photo ([id, confidence]), preferred to a CAM line of the full text.
 * Each field is { value, conf, lines } (the lines it was read from); dates [{ kind, value, conf, line }].
 */
export function parseEnvelope(lines, lexicon, { camid = null } = {}) {
  const out = { dates: [] };
  const texts = [];
  let pendingKind = null;
  let sexes = [];
  for (const raw of Array.isArray(lines) ? lines : []) {
    const [text0, c0] = Array.isArray(raw) ? raw : [raw, 1];
    let text = String(text0 ?? '').trim();
    const conf = Number.isFinite(c0) ? c0 : 1;
    if (!text) continue;
    // Sex symbols, alone or after a species word.
    for (const symbol of text.match(/[♀♂]/g) ?? []) sexes.push({ value: symbol === '♀' ? 'female' : 'male', conf, line: text });
    for (const word of text.match(/\b(?:fe)?male\b|\bhembra\b|\bmacho\b/gi) ?? [])
      sexes.push({ value: /^(?:female|hembra)$/i.test(word) ? 'female' : 'male', conf, line: text });
    text = text.replace(/[♀♂]|\b(?:fe)?male\b|\bhembra\b|\bmacho\b/gi, ' ').trim();
    if (!text) continue;
    // CRISPR marks: "CRISPR #171", "Crispr 55 (25/7/23)".
    const crispr = /c?ris?pr\s*#?\s*(\d{1,4})/i.exec(text);
    if (crispr) {
      if (!out.crispr || conf > out.crispr.conf) out.crispr = { value: `#${crispr[1]}`, conf, lines: [String(text0)] };
      text = text.replace(crispr[0], ' ');
    }
    // Specimen IDs (CAM…, ME22-106): the photo's CAM_ID reading is preferred below.
    for (const m of text.matchAll(SPECIMEN_ID)) {
      const value = m[1] ? `CAM${m[1].replace(/O/g, '0')}` : `${m[2]}_${m[3]}`;
      if (!out.idLine || conf > out.idLine.conf) out.idLine = { value, conf, lines: [String(text0)] };
    }
    text = text.replace(SPECIMEN_ID, ' ');
    // The insectary's code of a reared butterfly: "(F77)", "(Reared ID: 4MT)", "ID: X14".
    for (const re of CODE) {
      const m = re.exec(text);
      if (!m) continue;
      const value = m[1].replace(/\s+/g, '').toUpperCase();
      if (/\d/.test(value) && (!out.code || conf > out.code.conf)) out.code = { value, conf, lines: [String(text0)] };
      text = text.replace(m[0], ' ');
      break;
    }
    // Dates, with what they are from the words around them ("Death: 27/11/22", "wing clip:" on its own line before one).
    const kindHere = kindOf(text);
    let dated = false;
    for (const m of text.matchAll(DATE)) {
      const value = isoDate(m[1], m[2] ?? m[3], m[4]);
      if (!value) continue;
      out.dates.push({ kind: kindHere ?? pendingKind ?? 'date', value, conf, line: String(text0) });
      dated = true;
    }
    if (dated) {
      pendingKind = null;
      continue;
    }
    if (kindHere) {
      pendingKind = kindHere;
      continue;
    }
    text = text.replace(DATE, ' ').replace(/\b\d+(?:[.,]\d+)?\s*(?:g|mg|km|h|hs|m)\b|\b\d{1,2}:\d{2}\b/gi, ' ');
    if (/^[\s\d/.,:;()#~+-]*$/.test(text)) continue;
    texts.push({ text, conf, raw: String(text0) });
  }

  // The specimen ID: the CAM_ID reader's, else a line of the full text.
  if (camid && camid[0]) out.camid = { value: String(camid[0]).toUpperCase(), conf: camid[1] ?? null, lines: [] };
  else if (out.idLine) out.camid = out.idLine;
  delete out.idLine;

  // Sex: the symbol read; both symbols on one envelope (one may be invented on a tiny crop): the surer, at half its confidence.
  if (sexes.length) {
    sexes.sort((a, b) => b.conf - a.conf);
    const top = sexes[0];
    const both = sexes.some(s => s.value !== top.value);
    out.sex = { value: top.value, conf: round(both ? top.conf / 2 : top.conf, 3), lines: [top.line], ...(both ? { both: true } : {}) };
  }

  // Each remaining line: a name of the taxonomy, or a locality.
  const taxonLines = [];
  // Lines that read as neither ("seba ♀": a short species word on its own): their words may still be the species of a genus read.
  const loose = [];
  const placeLines = [];
  for (const line of texts) {
    const ws = words(fold(line.text));
    const abbreviated = /^\s*[A-Z]\s?\.\s?(?:[a-z]\s?\.\s?[a-z]{3,}|[a-z]{3,})/.test(line.text);
    // How much of the line reads as names of the taxonomy (each word: a genus, a species or a subspecies).
    const scores = ws
      .filter(w => w.length >= 3 && w !== 'x')
      .map(w => Math.max(genusMatch(lexicon, w)?.sim ?? 0, (anyEpithet(lexicon, w)?.sim ?? 0) - 0.02, anySub(lexicon, w)));
    const most = scores.length ? Math.max(...scores) : 0;
    const share = scores.length ? scores.reduce((a, b) => a + b, 0) / scores.length : 0;
    const place = localityMatch(lexicon, line.text);
    if (abbreviated || (most >= 0.8 && (!place || share > place.score))) taxonLines.push({ ...line, words: ws, abbreviated });
    else if (place) placeLines.push({ ...line, place });
    else loose.push({ ...line, words: ws, abbreviated: false });
  }
  out.species = speciesOf(taxonLines, lexicon, loose);
  // A symbol alone on a strip with no name of the taxonomy (photos of the envelope's top only): the reader
  // writes ♀/♂ on tiny crops without one now and then, so it counts at half its confidence.
  if (out.sex && !out.species && /^[\s♀♂?]*$/.test(out.sex.lines[0])) out.sex = { ...out.sex, conf: round(out.sex.conf / 2, 3), alone: true };

  // Locality: the line (or two lines read as one: "Cavernas Templo" / "Ceremonia") closest to a workbook locality.
  let place = null;
  for (let i = 0; i < placeLines.length; i++) {
    const one = placeLines[i];
    const candidates = [{ score: one.place.score, place: one.place.place, used: [one] }];
    const next = placeLines[i + 1];
    if (next) {
      const joined = localityMatch(lexicon, `${one.text} ${next.text}`);
      if (joined && joined.score > Math.max(one.place.score, next.place.score)) candidates.push({ score: joined.score, place: joined.place, used: [one, next] });
    }
    for (const c of candidates) if (!place || c.score > place.score) place = c;
  }
  if (place)
    out.locality = {
      value: place.place.name,
      conf: round(Math.min(...place.used.map(l => l.conf)) * place.score, 3),
      read: place.used.map(l => l.text.trim()).join(' '),
      lines: place.used.map(l => l.raw),
    };
  return out;
}

/** Genus, species and subspecies from the lines that name a taxon. */
function speciesOf(lines, lexicon, loose = []) {
  if (!lines.length) return null;
  const all = lines.flatMap(l => l.words.map(text => ({ text, line: l })));
  const confOf = used => Math.min(...[...new Set(used.map(w => w.line))].map(l => l.conf));
  const result = (genus, species, sub, sims, used) => {
    const name = sub ? sub.sub.name : species ? species.name : genus.name;
    return {
      value: name,
      genus: genus.name,
      ...(species ? { species: species.name } : {}),
      conf: round(confOf(used) * Math.min(...sims), 3),
      lines: [...new Set(used.map(w => w.line.raw))],
    };
  };
  // "M.m. deceptus": the genus and species by their initials, the subspecies written out;
  // "M. polymnia proceriformis": the genus by its initial.
  const short = lines.find(l => l.abbreviated);
  if (short) {
    const both = /^\s*([A-Z])\s?\.\s?([a-z])\s?\.\s?([a-z][a-z\s-]+)/.exec(short.text);
    const one = both ? null : /^\s*([A-Z])\s?\.\s?([a-z]{3,})\b\s*([a-z][a-z\s-]*)?/.exec(short.text);
    const g = (both ?? one)?.[1].toLowerCase();
    let best = null;
    if (both) {
      const rest = words(fold(both[3])).map(text => ({ text, line: short }));
      for (const genus of lexicon.genera.values()) {
        if (!genus.key.startsWith(g)) continue;
        for (const species of genus.species.values()) {
          if (!species.epithet.startsWith(both[2])) continue;
          const sub = subMatch(species, rest);
          if (sub && (!best || sub.sim > best.sims[0] || (sub.sim === best.sims[0] && species.n > best.species.n))) best = { genus, species, sub, sims: [sub.sim] };
        }
      }
    } else if (one) {
      const epithet = fold(one[2]);
      const rest = [...words(fold(one[3] ?? '')).map(text => ({ text, line: short })), ...all.filter(w => w.line !== short)];
      for (const genus of lexicon.genera.values()) {
        if (!genus.key.startsWith(g)) continue;
        const e = epithetMatch(genus, epithet);
        if (!e || e.sim < 0.8) continue;
        const sub = subMatch(e.species, rest);
        const score = e.sim + (sub ? 0.01 : 0);
        if (!best || score > best.score || (score === best.score && e.species.n > best.species.n))
          best = { genus, species: e.species, sub, sims: sub ? [e.sim, sub.sim] : [e.sim], score };
      }
    }
    if (best) return result(best.genus, best.species, best.sub, best.sims, [{ line: short }, ...(best.sub?.words ?? [])]);
  }
  // The genus words (a word that reads better as a species, "polymnia" ≠ Polygonia, is not one), each with
  // its species among the other words; a genus with its species first ("Callicorina pulchra (Temenis)").
  const genusHits = [];
  for (const w of all) {
    const g = genusMatch(lexicon, w.text);
    if (!g || (g.sim < 1 && (anyEpithet(lexicon, w.text)?.sim ?? 0) >= g.sim)) continue;
    let speciesHit = null;
    for (const other of all) {
      if (other === w || other.text === w.text) continue;
      const e = epithetMatch(g.genus, other.text);
      if (e && (!speciesHit || e.sim > speciesHit.sim)) speciesHit = { ...e, word: other };
    }
    if (!speciesHit)
      for (const l of loose)
        for (const text of l.words) {
          const e = text.length >= 3 ? epithetMatch(g.genus, text) : null;
          if (e && e.sim >= 0.85 && (!speciesHit || e.sim > speciesHit.sim)) speciesHit = { ...e, word: { text, line: l } };
        }
    genusHits.push({ ...g, word: w, speciesHit, score: g.sim + (speciesHit ? 1 + speciesHit.sim : 0) });
  }
  genusHits.sort((a, b) => b.score - a.score || (b.speciesHit?.species.n ?? 0) - (a.speciesHit?.species.n ?? 0) || b.genus.n - a.genus.n);
  const top = genusHits[0];
  let found = null;
  if (top?.speciesHit)
    found = { genus: top.genus, species: top.speciesHit.species, sims: [top.sim, top.speciesHit.sim], used: [top.word, top.speciesHit.word], rest: all.filter(w => w !== top.word && w !== top.speciesHit.word) };
  else if (top) {
    found = { genus: top.genus, species: null, sims: [top.sim], used: [top.word], rest: all.filter(w => w !== top.word) };
    // A genus and a subspecies without its species ("Mechanitis / deceptus"): the species that has it.
    let best = null;
    for (const species of found.genus.species.values()) {
      const sub = subMatch(species, found.rest);
      if (sub && sub.sim >= 0.85 && (!best || sub.sim > best.sub.sim || (sub.sim === best.sub.sim && species.n > best.species.n))) best = { species, sub };
    }
    if (best) return result(found.genus, best.species, best.sub, [...found.sims, best.sub.sim], [...found.used, ...best.sub.words]);
  }
  // A species word of another genus than the one written ("Mechanitis isocomma": Melinaea isocomma; a genus
  // renamed, "Phoebis statira": Aphrissa statira), or no genus word ("hyris euclea /ninonia"): the species by
  // its word, a little less sure when a genus was written.
  if (!found?.species) {
    let speciesHit = null;
    for (const w of all) {
      if (found && w === found.used[0]) continue;
      const e = anyEpithet(lexicon, w.text);
      if (e && (!speciesHit || e.sim > speciesHit.sim)) speciesHit = { ...e, word: w };
    }
    // A word that is also a subspecies used more than the species it reads as ("intermedia": Mechanitis messenoides intermedia, not Macrosoma intermedia).
    let alone = null;
    if (!found)
      for (const w of all) {
        const hit = subAlone(lexicon, w.text);
        if (hit && (!alone || hit.sim > alone.sim || (hit.sim === alone.sim && hit.sub.n > alone.sub.n))) alone = { ...hit, word: w };
      }
    if (alone && (!speciesHit || alone.sub.n > speciesHit.species.n))
      return result(alone.species.genus, alone.species, { sub: alone.sub, sim: alone.sim, words: [alone.word] }, [alone.sim], [alone.word]);
    if (speciesHit && speciesHit.sim >= (found ? 0.9 : 0.85))
      found = {
        genus: speciesHit.species.genus,
        species: speciesHit.species,
        sims: found ? [speciesHit.sim * 0.85] : [speciesHit.sim],
        used: [...(found?.used ?? []), speciesHit.word],
        rest: all.filter(w => w !== speciesHit.word && !(found?.used ?? []).includes(w)),
      };
  }
  if (!found) return null;
  // The nominal subspecies repeats the species' word ("Ithomia hyala / hyala"): only the word read as the species is left out.
  const sub = found.species ? subMatch(found.species, found.rest) : null;
  return result(found.genus, found.species, sub, sub ? [...found.sims, sub.sim] : found.sims, sub ? [...found.used, ...sub.words] : found.used);
}

// ------------------------------------------------------------ a butterfly's photos together
const FIELDS = ['species', 'sex', 'locality', 'code', 'crispr', 'camid'];
/**
 * The envelope of a butterfly from its photos' readings ([{ name, text, camid, conf }]): each
 * field the surest reading of it, `differ` the fields its photos read differently, the dates
 * of all photos once each.
 */
export function envelopeOfReadings(readings, lexicon) {
  const parsed = readings.map(r => ({ name: r.name, fields: parseEnvelope(r.text, lexicon, { camid: r.camid ? [r.camid, r.conf] : null }) }));
  const out = { photos: parsed.length };
  const differ = [];
  for (const field of FIELDS) {
    const seen = parsed.map(p => p.fields[field]).filter(Boolean);
    if (!seen.length) continue;
    const best = seen.reduce((a, b) => ((b.conf ?? 0) > (a.conf ?? 0) ? b : a));
    const agree = seen.filter(f => sameValue(field, f, best)).length;
    out[field] = { ...best, seen: agree };
    if (agree < seen.length) differ.push(field);
  }
  const dates = new Map();
  for (const p of parsed)
    for (const d of p.fields.dates) {
      const key = `${d.kind}|${d.value}`;
      if (!dates.has(key) || dates.get(key).conf < d.conf) dates.set(key, d);
    }
  out.dates = [...dates.values()].sort((a, b) => a.value.localeCompare(b.value));
  if (differ.length) out.differ = differ;
  return { envelope: out, perPhoto: parsed };
}
const binomialOf = name => fold(name).split(/\s+/).slice(0, 2).join(' ');
function sameValue(field, a, b) {
  if (field === 'species') return binomialOf(a.value) === binomialOf(b.value);
  return a.value === b.value;
}

// ------------------------------------------------------------ the notes
const SPECIES_CHANGE = [
  // "Species ID changed from X (identified by Y) to Z", "Species name has been changed from X …", "Species Id. was changed by PAS from X to Z", "Species ID updated from X to Z"
  /(?:species|spp?\.?|taxon)\b[^;|¦]{0,40}?\b(?:chang(?:e|ed)|updated|corrected)\b[^;|¦]{0,25}?\b(?:from|for)\s+\*?(.+?)\*?\s*(?:\((?:identified|id)[^)]*\)\s*|,\s*identified by [^,]+,\s*)?\s+to\s+\*?([A-Z][^*;|¦(,]+?)\*?(?=\s*(?:[.;|¦(,]|$|\s(?:after|following|as|because|based|on|in)\b))/i,
  // "Changed ID species from X to Z", "ID species changed from X to Z", "changed species for X to Z"
  /(?:chang(?:e|ed)\s+id\s+species|id\s+species\s+changed|chang(?:e|ed)\s+species(?:\s+id)?)\s+(?:from|for)\s+(.+?)\s+to\s+([A-Z](?:\.\s*)?[^.;|¦(,]+?)(?=\s*(?:[.;|¦(,]|$|\s(?:and|after|following)\b))/i,
  // "originally identified as X, but has been reidentified as Z"
  /(?:originally|initially|first)\s+identif\w*\s+as\s+([A-Z][^,;|¦]+?)(?:,|\s+but|\s+upon|\s+and)[^;|¦]*?re-?identified\s+as\s+([A-Z](?:\.\s*)?[^.;|¦(,]+?)(?=\s*(?:[.;|¦(,]|$|\s(?:after|following)\b))/i,
];
const SPECIES_FROM_ONLY = /(?:(?:originally|initially|erroneously|wrongly)\s+identif\w*|misidentified)\s+as\s+([A-Z](?:\.\s*)?[^,;.|¦(]+?)(?=\s*(?:[.;|¦(,]|$|\s(?:upon|and|but)\b))/i;
const SPECIES_CHECKED = /\b(?:id\.?\s+verified|verified\s+by|re-?identified|curated\s+by|species\s+(?:and\s+ssp\.\s+)?id\s+curated|spp\.?\s+id\.?\s+verified)\b/i;
const SEX_CHANGE = [
  /sex\b[^.;|¦]{0,30}?\b(?:changed|corrected)\b[^.;|¦]{0,30}?\bfrom\s+(female|male)\s+to\s+(female|male)/i,
  /changed\s+the\s+sex\b[^.;|¦]{0,40}?\bfrom\s+(female|male)\s+to\s+(female|male)/i,
  /correct(?:ed)?\s+(?:the\s+)?sex\b[^.;|¦]{0,20}?\b(female|male)\s+to\s+(female|male)/i,
  /changed\s+sex\s+to\s+(female|male)/i,
];
/** A name as people write it (capital first: "M. confusa", "Godyris sp."), not a word of a sentence ("complete", "female"). */
const NAMED = /^\*?[A-Z]/;
const CAM_CHANGE = /CAM_?ID\s+(?:changed|corrected)\s+from\s+(CAM\d{5,7})\s+to\s+(CAM\d{5,7})/i;

/** The sentence of a note around a match: what is said about it, not the whole note. */
function sentenceOf(note, index, length) {
  const start = Math.max(note.lastIndexOf('|', index), note.lastIndexOf('¦', index), note.lastIndexOf(';', index), 0);
  const ends = ['|', '¦', ';'].map(c => note.indexOf(c, index + length)).filter(i => i >= 0);
  const end = ends.length ? Math.min(...ends) : note.length;
  return note.slice(start ? start + 1 : 0, end).trim().slice(0, 300);
}

/** "M. grandior", "H. alema chrysodonia", "Hypothiris mamercus mamercus" as the workbook writes the name. */
export function resolveName(text, lexicon, hintGenus = null) {
  const clean = String(text ?? '').replace(/[*_]/g, ' ').replace(/\s+/g, ' ').trim();
  const m = /^([A-Z])\.\s*([a-z][a-z-]+)(?:\s+([a-z][a-z\s-]*))?$/.exec(clean);
  if (m) {
    const genera = [...lexicon.genera.values()].filter(g => g.key.startsWith(m[1].toLowerCase()) && g.species.has(fold(m[2])));
    genera.sort((a, b) => (hintGenus && fold(hintGenus) === b.key) - (hintGenus && fold(hintGenus) === a.key) || b.n - a.n);
    if (genera[0]) return `${genera[0].name} ${m[2]}${m[3] ? ` ${m[3].trim()}` : ''}`;
  }
  const parsed = speciesOf([{ text: clean, conf: 1, raw: clean, words: words(fold(clean)), abbreviated: false }], lexicon);
  return parsed?.species ? parsed.value : clean;
}

/**
 * The corrections a note holds: [{ field: 'species' | 'sex' | 'camid', from, to, text }] (`text`: its
 * sentence); `checked` when it says the species was verified or re-identified without a from → to.
 */
export function noteCorrections(note, lexicon = null, hintGenus = null) {
  const text = String(note ?? '');
  if (!text.trim()) return [];
  const out = [];
  const name = value => (lexicon ? resolveName(value, lexicon, hintGenus) : String(value).replace(/[*]/g, '').trim());
  let species = false;
  for (const re of SPECIES_CHANGE) {
    const all = new RegExp(re.source, 'gi');
    for (const m of text.matchAll(all)) {
      if (!NAMED.test(m[1].trim()) || !NAMED.test(m[2].trim())) continue;
      const [from, to] = [name(m[1]), name(m[2])];
      if (out.some(c => c.field === 'species' && c.from === from && c.to === to)) continue;
      out.push({ field: 'species', from, to, text: sentenceOf(text, m.index, m[0].length) });
      species = true;
    }
  }
  if (!species) {
    const m = SPECIES_FROM_ONLY.exec(text);
    if (m && NAMED.test(m[1].trim()) && !/^(?:fe)?male\b/i.test(m[1].trim())) out.push({ field: 'species', from: name(m[1]), to: null, text: sentenceOf(text, m.index, m[0].length) });
    else {
      const checked = SPECIES_CHECKED.exec(text);
      if (checked) out.push({ field: 'species', checked: true, text: sentenceOf(text, checked.index, checked[0].length) });
    }
  }
  for (const re of SEX_CHANGE) {
    const m = re.exec(text);
    if (!m) continue;
    out.push({ field: 'sex', from: m[2] ? m[1].toLowerCase() : null, to: (m[2] ?? m[1]).toLowerCase(), text: sentenceOf(text, m.index, m[0].length) });
    break;
  }
  for (const m of text.matchAll(new RegExp(CAM_CHANGE.source, 'gi')))
    out.push({ field: 'camid', from: m[1].toUpperCase(), to: m[2].toUpperCase(), text: sentenceOf(text, m.index, m[0].length) });
  return out;
}

export { fold, binomialOf };
