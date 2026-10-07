import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { saveTrack, saveWalk } from '../server/monitoring.mjs';
import * as source from '../server/suggestions/wikiloc-transects.mjs';
import { analyse, geometry, longestRun, transectCertainty, walkTiming } from '../server/suggestions/wikiloc-transects.mjs';
import { CERTAINTIES, suggestionSources } from '../server/suggestions/index.mjs';

/** The shape every suggestion source returns (server/suggestions/index.mjs). */
const validSuggestion = s =>
  !!s &&
  typeof s.sheet === 'string' &&
  Number.isInteger(s.row) &&
  typeof s.recordId === 'string' &&
  typeof s.field === 'string' &&
  'current' in s &&
  'suggested' in s &&
  CERTAINTIES.includes(s.certainty) &&
  typeof s.reason === 'string';
import { monitoringRowsCsv, pointsCsv, walksGpx, wikilocCorrections } from '../server/monitoring-export.mjs';

const serial = iso => Math.round((Date.parse(`${iso}T00:00:00Z`) - Date.UTC(1899, 11, 30)) / 864e5);
const AA = 'AA - Alex Arias';
const DAY = '2025-09-08';
// Places on the trail (vertices of frontend/src/lib/transects.ts).
const T4 = [-0.951082, -77.869329]; // ~100 m inside T4
const T3 = [-0.952262, -77.867649]; // middle of T3
const T2 = [-0.952739, -77.865047]; // middle of T2
const T1 = [-0.951874, -77.86443]; // middle of T1
const T4_T3 = [-0.951959, -77.868315]; // the T4/T3 boundary
const editor = { id: 'e1', username: 'editor', role: 'editor' };

const row = (n, time, values = {}) => {
  const [h, m] = time.split(':').map(Number);
  return {
    row: n,
    values: {
      Purpose: 'Monitoring',
      Collection_location: 'Ikiam',
      Collector: AA,
      Collection_date: serial(DAY),
      Collection_time: (h * 60 + m) / 1440,
      FieldMark_ID: 'NA',
      SPECIES: 'Oleria onega',
      Sex: 'male',
      ...values,
    },
  };
};

/**
 * One walk of 8 Sep 2025 (AA): blank, agreeing, disagreeing and boundary
 * sections, a note with the wrong hour ("10:12" between 9:10 and 9:14), a
 * point without a row, a row without a point, and a day with no walk.
 */
async function fixture() {
  const sheets = new LocalSheets({
    Collection_data: [
      row(2, '9:10', { FieldMark_ID: 'B1', Transect_section: '' }), // blank, point deep in T4
      row(3, '10:12', { FieldMark_ID: 'B2', Transect_section: 4 }), // the note's hour is wrong; point in T3
      row(4, '9:14', { FieldMark_ID: 'B3', Transect_section: 2 }), // agrees
      row(5, '9:16', { FieldMark_ID: 'B4', Transect_section: 'NA' }), // point on the T4/T3 boundary
      row(6, '9:20', { FieldMark_ID: 'B5', Transect_section: 4 }), // says T4, point deep in T1
      row(7, '9:30', { SPECIES: 'Methona confusa' }), // no point that day
      row(8, '9:00', { Collection_date: serial('2025-09-10'), Transect_section: '' }), // a day without walk
    ],
  });
  const s = new Store({ localMode: true }, { sheets });
  await s.sync({ sheets: ['Collection_data'] });
  const id = n => s.db.prepare("SELECT id FROM records WHERE sheet='Collection_data' AND row_num=?").get(n).id;
  const point = ([lat, lon], text, minutes, n) => ({ lat, lon, text, minutes, ...(n ? { row: n, recordId: id(n) } : {}) });
  saveTrack(
    s,
    {
      requestId: 'request-wt-1',
      date: DAY,
      collector: AA,
      name: 'Monitoreo 8/9/2025',
      track: [
        [...T4, 600, null],
        [...T1, 590, null],
      ],
      captures: [
        point(T4, 'B1 9:10 male onega', 550, 2),
        point(T3, 'B2 10:12 male onega', 612, 3),
        point(T2, 'B3 9:14 male onega', 554, 4),
        point(T4_T3, 'B4 9:16 male onega', 556, 5),
        point(T1, 'B5 9:20 male onega', 560, 6),
        point(T2, '9:25 female', 565, null),
      ],
    },
    editor,
  );
  return { s, id };
}
const find = (list, rowNum, field) => list.find(x => x.row === rowNum && x.field === field);

test('trail geometry: strong inside a section, weak near a boundary or far from the trail', () => {
  assert.equal(geometry({ distance: 3, margin: 100 }), 'strong');
  assert.equal(geometry({ distance: 20, margin: 100 }), 'fair');
  assert.equal(geometry({ distance: 3, margin: 2 }), 'weak');
  assert.equal(transectCertainty({ pairing: 'mark', pos: { distance: 3, margin: 100 }, current: null }), 'certain');
  assert.equal(transectCertainty({ pairing: 'order', pos: { distance: 3, margin: 100 }, current: null }), 'likely');
  // A value the walker typed is only overruled for sure well inside another section.
  assert.equal(transectCertainty({ pairing: 'mark', pos: { distance: 3, margin: 30 }, current: 2 }), 'likely');
  assert.equal(transectCertainty({ pairing: 'mark', pos: { distance: 3, margin: 60 }, current: 2 }), 'certain');
  assert.equal(transectCertainty({ pairing: 'tie', pos: { distance: 3, margin: 60 }, current: 2 }), 'check');
});

test('the walk order: the longest forward run, an hour off between two points, a point added later', () => {
  assert.deepEqual(longestRun([550, 612, 554, 556, 560]), [0, 2, 3, 4]);
  const walk = {};
  const p = minutes => ({ walk, capture: {}, note: { minutes } });
  const points = [550, 612, 554, 556, 560, 565, 570, 575, 580, 530].map(p);
  const timing = walkTiming(points);
  assert.equal(timing.get(points[1]).slip, 552);
  assert.equal(timing.get(points[2]).outOfOrder, false);
  // The last point is before the one before it, with nothing after: added later, maybe elsewhere.
  assert.equal(timing.get(points[9]).slip, null);
  assert.equal(timing.get(points[9]).outOfOrder, true);
  // Walks whose notes mostly do not follow their order are not judged.
  const messy = [600, 550, 620, 540, 610].map(p);
  assert.ok([...walkTiming(messy).values()].every(x => !x.ordered && !x.outOfOrder && x.slip === null));
});

test('Wikiloc points suggest the transect section, the right hour, and list what has no partner', async () => {
  const { s } = await fixture();
  const { suggestions, discrepancies, daysWithoutWalk, counts } = analyse(s);

  const blank = find(suggestions, 2, 'Transect_section');
  assert.equal(blank.suggested, 4);
  assert.equal(blank.current, null);
  assert.equal(blank.certainty, 'certain');
  assert.match(blank.reason, /^Punto de Wikiloc en T4 \(0 m del sendero, \d+ m del límite más cercano\); emparejado por marca$/);
  assert.deepEqual(blank.reasonMsg.vars.how, { key: 'marca' });

  // Agrees: nothing to suggest.
  assert.equal(find(suggestions, 4, 'Transect_section'), undefined);
  // On the boundary: to check, and "NA" counts as blank.
  const edge = find(suggestions, 5, 'Transect_section');
  assert.equal(edge.certainty, 'check');
  assert.equal(edge.current, 'NA');
  // The row says T4, the point is deep in T1: the walker's value overruled for sure.
  const wrong = find(suggestions, 6, 'Transect_section');
  assert.deepEqual([wrong.current, wrong.suggested, wrong.certainty], [4, 1, 'certain']);
  assert.match(wrong.reason, /^La fila dice T4; el punto de Wikiloc está en T1/);

  // "10:12" between 9:10 and 9:14, copied into the row: it was 9:12. Its place is right (T3, not the row's T4).
  const hour = find(suggestions, 3, 'Collection_time');
  assert.equal(Math.round(hour.suggested * 1440), 552);
  assert.equal(hour.certainty, 'likely');
  assert.match(hour.reason, /parece 9:12 \(hora equivocada\)/);
  assert.equal(find(suggestions, 3, 'Transect_section').suggested, 3);

  assert.ok(suggestions.every(validSuggestion));
  assert.deepEqual(
    discrepancies.map(d => [d.kind, d.row]).sort(),
    [
      ['point-without-row', null],
      ['row-without-point', 7],
    ],
  );
  assert.deepEqual(
    daysWithoutWalk.map(d => [d.date, d.rows, d.blankSections]),
    [[serial('2025-09-10'), 1, 1]],
  );
  assert.equal(counts.agree, 1);
  assert.equal(counts.filled, 2);
  assert.equal(counts.differ, 2);
  // Cached until something changes.
  assert.equal(analyse(s), analyse(s));
  s.close();
});

test('a walk still waiting for review is paired on the fly', async () => {
  const sheets = new LocalSheets({
    Collection_data: [row(2, '9:20', { FieldMark_ID: 'B69', SPECIES: 'Hyposcada illinissa', Sex: 'female', Collection_date: serial('2026-09-26'), Collector: 'FCH - Franz Chandi' })],
  });
  const s = new Store({ localMode: true }, { sheets });
  await s.sync({ sheets: ['Collection_data'] });
  await saveWalk(
    s,
    {
      url: 'https://es.wikiloc.com/rutas-senderismo/monitoreo-ithomidos-fch-26-sep-2026-288058472',
      name: 'Monitoreo ithomidos FCH 26 SEP 2026',
      date: '2026-09-26',
      collector: 'FCH - Franz Chandi',
      track: [],
      waypoints: [{ lat: T4[0], lon: T4[1], text: 'M1 Hyposcada illinissa ida hembra 9:20 0.5m NO id: B69', photos: [] }],
    },
    editor,
  );
  const [s1] = analyse(s).suggestions;
  assert.deepEqual([s1.row, s1.field, s1.suggested, s1.certainty, s1.evidence.walk.stored], [2, 'Transect_section', 4, 'certain', false]);
  s.close();
});

test('the registry lists the source, which suggests what analyse finds', async () => {
  const { s } = await fixture();
  assert.ok(suggestionSources().some(x => x.id === 'wikiloc-transects'));
  assert.deepEqual(await source.suggest({ store: s }), analyse(s).suggestions);
  s.close();
});

test('downloads: monitoring rows with their point, points and GPX per walk or all', async () => {
  const { s } = await fixture();
  const rows = monitoringRowsCsv(s);
  assert.match(rows.type, /^text\/csv/);
  const lines = rows.body.replace(/^\uFEFF/, '').trim().split('\r\n');
  const header = lines[0].split(',');
  assert.equal(header[0], 'Sheet_row');
  assert.ok(header.includes('Transect_section') && header.at(-1) === 'Wikiloc_photos');
  assert.equal(lines.length, 1 + 7);
  const two = lines[1].split(',');
  assert.equal(two[0], '2');
  assert.equal(two[header.indexOf('Collection_date')], DAY);
  assert.equal(two[header.indexOf('Collection_time')], '9:10');
  // Negative coordinates are numbers, not text a spreadsheet would read as a formula.
  assert.equal(two[header.indexOf('Wikiloc_latitude')], String(T4[0]));
  assert.equal(two[header.indexOf('Wikiloc_transect_section')], '4');

  const corrections = wikilocCorrections(s);
  const { inventory } = corrections;
  assert.equal(corrections.counts.points, 6);
  assert.equal(corrections.points, undefined, 'the points only in the downloads');
  assert.equal(inventory.stored, 1);
  assert.equal(inventory.points, 6);
  const walk = inventory.walks[0].id;
  const points = pointsCsv(s, walk);
  assert.equal(points.name, `wikiloc_points_${DAY}_AA.csv`);
  assert.equal(points.body.trim().split('\r\n').length, 1 + 6);
  assert.equal(pointsCsv(s).body, points.body);

  const gpx = walksGpx(s, walk);
  assert.equal(gpx.name, 'wikiloc_2025-09-08_AA.gpx');
  assert.match(gpx.body, /^<\?xml/);
  assert.equal((gpx.body.match(/<wpt /g) || []).length, 6);
  assert.equal((gpx.body.match(/<trkpt /g) || []).length, 2);
  assert.match(gpx.body, /<desc>2025-09-08 AA - Alex Arias · Collection_data row 2 · Oleria onega male B1 · T4<\/desc>/);
  assert.throws(() => walksGpx(s, 'nope'), { code: 'WALK_NOT_FOUND' });
  s.close();
});
