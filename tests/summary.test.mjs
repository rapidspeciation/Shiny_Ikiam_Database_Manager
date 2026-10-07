import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { activity, clutchStage, marks, sessions, summaryHere, todaySerial, upcoming } from '../server/summary.mjs';

const today = todaySerial();

async function fixture() {
  const sheets = new LocalSheets({
    Collection_data: [
      {
        row: 2,
        values: {
          Release_Collect: 'Mark_Released',
          FieldMark_ID: 'A12',
          SPECIES: 'Oleria onega',
          Collection_location: 'Ikiam',
          Purpose: 'Monitoring',
          Collection_date: today - 3,
          Collector: 'FCH - Franz Chandi',
          DECIMAL_LATITUDE: -0.9,
          COLLECTOR_EMAIL: 'someone@example.org',
        },
      },
      {
        row: 3,
        values: {
          Release_Collect: 'Collected_Preserved',
          SPECIES: 'Mechanitis polymnia',
          Collection_location: 'Apuya Y',
          Collection_date: today - 10,
          CAM_ID: 'CAM000001',
        },
      },
    ],
    SamplingDay_data: [{ row: 2, values: { Date: today - 3, Location: 'Ikiam', Purpose: 'Monitoring', Collectors_initials: 'FCH' } }],
    Insectary_data: [
      { row: 2, values: { Insectary_ID: 'A0A', Wild_Reared: 'Wild-caught', SPECIES: 'Mechanitis polymnia', Sex: 'female', Intro2Insectary_date: today - 5 } },
      { row: 3, values: { Insectary_ID: 'A1A', Wild_Reared: 'Wild-caught', SPECIES: 'Mechanitis polymnia', Sex: 'male', Intro2Insectary_date: today - 400 } },
      { row: 4, values: { Insectary_ID: 'A2A', Wild_Reared: 'Wild-caught', SPECIES: 'Mechanitis polymnia', Sex: 'male', Intro2Insectary_date: today - 20, Death_date: today - 2, Death_cause: 'Spider' } },
    ],
    Insectary_stocks: [
      { row: 2, values: { 'CLUTCH NUMBER': 900, SPECIES: 'Mechanitis lysimnia', 'DATE LAID': today - 20, 'NUMBER OF EGGS': 30, 'HATCHING DATE': today - 15, 'NUMBER OF LARVAE': 25 } },
      { row: 3, values: { 'CLUTCH NUMBER': 901, SPECIES: 'Mechanitis lysimnia', 'DATE LAID': today - 2, 'NUMBER OF EGGS': 12 } },
      { row: 4, values: { 'CLUTCH NUMBER': 800, SPECIES: 'Mechanitis lysimnia', 'DATE LAID': today - 300, 'NUMBER OF EGGS': 40 } },
    ],
    CRISPR: [
      { row: 2, values: { 'CRISPR_No.': 1, 'Eggs_No.': 1, CRISPR_date: today - 50, Stock_of_origin: 'Mechanitis lysimnia', Hatch_date: today - 45, Mutant: 'Yes' } },
      { row: 3, values: { 'CRISPR_No.': 1, 'Eggs_No.': 2, CRISPR_date: today - 50, Stock_of_origin: 'Mechanitis lysimnia', Hatch_date: 'NA', Mutant: 'NA' } },
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({
    sheets: ['Collection_data', 'SamplingDay_data', 'Insectary_data', 'Insectary_stocks', 'CRISPR'],
  });
  return store;
}

test('visitors get the natural history; the team counts and the insectary need a session', async () => {
  const store = await fixture();
  // Visitors get rates and proportions only, no counts of butterflies collected or reared.
  const open = summaryHere(store, { signedIn: false });
  assert.equal(open.team, null);
  const nature = open.nature;
  assert.equal(nature.facts.places, 2);
  assert.deepEqual(nature.deaths, [{ name: 'Arañas', percent: 100 }]);
  assert.ok(!JSON.stringify(nature).includes('someone@example.org'));

  const { team } = summaryHere(store, { signedIn: true });
  assert.equal(team.latestIds.clutch, '902');
  assert.equal(team.latestIds.mark.last, 'A12');
  assert.equal(team.collections.total, 2);
  assert.equal(team.monitoring.individuals, 1);
  assert.equal(team.crispr.hatched, 1);
  const insectary = team.insectary;
  // A0A is alive; A1A has no death date but entered 400 days ago; A2A died.
  assert.equal(insectary.alive, 1);
  assert.equal(insectary.stale, 1);
  assert.equal(insectary.deaths30, 1);
  // Clutch 800 is too old to be in progress.
  assert.equal(insectary.clutches, 2);
  assert.deepEqual(insectary.stages.larva, { clutches: 1, n: 25 });
  assert.deepEqual(insectary.stages.egg, { clutches: 1, n: 12 });
  store.close();
});

test('a clutch is at its latest recorded stage', () => {
  assert.deepEqual(clutchStage({ 'NUMBER OF EGGS': 10 }), { stage: 'egg', n: 10 });
  assert.deepEqual(clutchStage({ 'NUMBER OF EGGS': 10, 'HATCHING DATE': 46000 }), { stage: 'larva', n: 10 });
  assert.deepEqual(clutchStage({ 'NUMBER OF EGGS': 10, 'NUMBER OF LARVAE': 8, 'PUPA DATE': 46010, 'NUMBER OF PUPA': 6 }), {
    stage: 'pupa',
    n: 6,
  });
});

test('the hour of day is corrected by the hours people were out searching', () => {
  const at = (h, m = 0) => (h + m / 60) / 24;
  const capture = (collector, time) => ({ Collection_date: 46000, Collection_location: 'Ikiam', Collector: collector, Collection_time: time });
  const list = sessions(
    [
      // A wrote down 8:00–12:00 in SamplingDay_data.
      ...[at(10, 10), at(10, 20), at(10, 40), at(11, 30)].map(t => capture('AA - Ana', t)),
      // B did not: the effort is the first to the last capture (10:05–10:55).
      ...[at(10, 5), at(10, 30), at(10, 55)].map(t => capture('BB - Beto', t)),
      // A single capture is not a session.
      capture('CC - Carla', at(15)),
    ],
    [{ Date: 46000, Collectors_initials: 'AA', Start_time: at(8), End_time: at(12) }],
  );
  assert.equal(list.length, 2);
  const byHour = Object.fromEntries(activity(list, 0).map(h => [h.hour, h]));
  assert.equal(byHour[8].perHour, 0);
  assert.equal(byHour[10].perHour, 3.3); // 6 captures in 1 + 50/60 hours
  assert.equal(byHour[11].perHour, 1);
  assert.equal(byHour[10].share, 85.7); // 6 of the 7 captures
  assert.equal(byHour[15], undefined);
});

test('the last mark is the highest in the series in use, not a later recapture', () => {
  const row = (id, day) => ({ FieldMark_ID: id, Collection_date: 46000 + day });
  assert.deepEqual(marks([row('A99', 1), row('B67', 5), row('B68', 6), row('B12', 7)]), {
    last: 'B68',
    date: '2025-12-15',
    next: 'B69',
  });
  assert.equal(marks([row('B99', 1)]).next, 'C1');
  assert.equal(marks([row('NA', 1)]), null);
});

test('coming hatchings, pupations and emergences use the species median for the current stage', () => {
  const T = 46300;
  // Five finished clutches: 5 days as eggs, 15 as larvae, 8 as pupae.
  const done = Array.from({ length: 5 }, (_, i) => ({
    'CLUTCH NUMBER': 900 + i,
    SPECIES: 'Mechanitis lysimnia',
    'DATE LAID': T - 100,
    'HATCHING DATE': T - 95,
    'PUPA DATE': T - 80,
    'EMERGENCE DATE': T - 72,
  }));
  const now = [
    { 'CLUTCH NUMBER': 1, SPECIES: 'Mechanitis lysimnia', 'DATE LAID': T - 4, 'NUMBER OF EGGS': 20 },
    { 'CLUTCH NUMBER': 2, SPECIES: 'Mechanitis lysimnia eurydice', 'DATE LAID': T - 20, 'HATCHING DATE': T - 14, 'NUMBER OF LARVAE': 9 },
    { 'CLUTCH NUMBER': 3, SPECIES: 'Mechanitis lysimnia', 'DATE LAID': T - 30, 'HATCHING DATE': T - 25, 'PUPA DATE': T - 10 },
    { 'CLUTCH NUMBER': 4, SPECIES: 'Mechanitis lysimnia', 'DATE LAID': T - 3, 'NUMBER OF EGGS': 5, 'HATCHING DATE': T - 1 },
  ];
  const out = upcoming([...done, ...now], T);
  const byClutch = Object.fromEntries(out.items.map(i => [i.clutch, i]));
  assert.equal(byClutch['1'].event, 'hatch');
  assert.equal(byClutch['1'].inDays, 1);
  assert.equal(byClutch['1'].n, 20);
  // A subspecies uses its species' times.
  assert.equal(byClutch['2'].event, 'pupate');
  assert.equal(byClutch['2'].inDays, 1);
  // Its pupae were due 2 days ago: late, but still listed.
  assert.equal(byClutch['3'].event, 'emerge');
  assert.equal(byClutch['3'].inDays, -2);
  // Hatched yesterday: pupation is 14 days away, beyond the week listed.
  assert.equal(byClutch['4'], undefined);
  assert.equal(out.items.length, 3);
  assert.equal(out.late.length, 0);
});
