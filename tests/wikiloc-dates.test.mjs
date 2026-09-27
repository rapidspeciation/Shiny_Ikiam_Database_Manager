import test from 'node:test';
import assert from 'node:assert/strict';
import { walkDate } from '../tools/wikiloc/lib.mjs';

// Real titles from the team's Wikiloc profiles, with Wikiloc's "Fecha de realización".
const cases = [
  ['Monitoreo ithomidos sendero Ikiam FCH 14 mayo 2025', 'mayo 2025', '2025-05-14'],
  ['Monitoreo 21/sep/2026', 'septiembre 2026', '2026-09-21'],
  ['Monitoreo 17 de agosto 2026', 'agosto 2026', '2026-08-17'],
  ['Monitoreo 2/2 22/07/2026', 'julio 2026', '2026-07-22'],
  ['Monitoreo 1 10/03/2025', 'marzo 2025', '2025-03-10'],
  ['Monitoreo 27/4/2024', 'abril 2025', '2025-04-27'],
  ['Monitoreo 19/20/2025', 'febrero 2025', '2025-02-19'],
  ['Monitoreo 16/082024', 'agosto 2024', '2024-08-16'],
  ['Monitoreo 9/72024 AA', 'julio 2024', '2024-07-09'],
  ['Monitoreo 11 de noviembre', 'noviembre 2024', '2024-11-11'],
  ['Monitoreo 13noviembre23', 'noviembre 2023', '2023-11-13'],
  ['Monitoreo 13 sep', 'septiembre 2023', '2023-09-13'],
  ['Monitoro 12/7', 'julio 2023', '2023-07-12'],
  ['Monitoreo 18/06', 'junio 2023', '2023-06-18'],
  ['Monitoreo 12-2-23 2da parte', 'febrero 2023', '2023-02-12'],
  ['Monitoreo 12/nov2025', 'noviembre 2025', '2025-11-12'],
  ['Monitoreo 27 aug 2025', 'agosto 2025', '2025-08-27'],
  ['Monitoreo Ikiam 3 de Junio Alex Arias', 'junio 2024', '2024-06-03'],
  ['Monitoreo 12 sept 2025', 'septiembre 2025', '2025-09-12'],
  ['monitoreo 17/2/25', 'febrero 2025', '2025-02-17'],
];

test('walk dates come from the title day and the month Wikiloc recorded', () => {
  for (const [title, recorded, expected] of cases) assert.equal(walkDate(title, recorded), expected, title);
  assert.equal(walkDate('Apuya', 'mayo 2025'), null);
  // No day in the title: the upload day, only when it is in the recorded month.
  assert.equal(walkDate('Censo de itómidos Ikiam FCH', 'febrero 2023', '2023-02-14T20:10+0100'), '2023-02-14');
  assert.equal(walkDate('Censo de itómidos Ikiam FCH', 'febrero 2023', '2023-03-01T10:00+0100'), null);
});
