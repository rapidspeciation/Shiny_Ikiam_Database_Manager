// The eval's no-AI number check (tools/assistant-eval/grade.mjs `numbers`): a number the chat's tool results do not
// hold fails the case. P1 (16 Nov 2026): the per-species female counts were 10, 13, 9, 12 and 4, and the reply said
// «41 ♀»; they make 48.
import test from 'node:test';
import assert from 'node:assert/strict';
import { grade, numbersCheck, numbersIn, partsMismatch } from '../tools/assistant-eval/grade.mjs';

const PER_SPECIES =
  'SPECIES\tSex\tn\nIthomia salapia salapia\tfemale\t10\nIthomia salapia salapia\tmale\t15\nIthomia salapia salapia\t\t1\n' +
  'Mechanitis messenoides deceptus\tfemale\t13\nMechanitis messenoides deceptus\tmale\t10\nMechanitis polymnia proceriformis\tfemale\t9\n' +
  'Mechanitis polymnia proceriformis\tmale\t13\nMechanitis messenoides intermedia\tfemale\t12\nMechanitis messenoides intermedia\tmale\t6\n' +
  'Mechanitis lysimnia\tfemale\t4\nMechanitis lysimnia\tmale\t5\n(Copy of 2026-10-31 00:40 UTC, current: nothing was saved or synced since.)';
// The sums worked out with python, as the brief asks (a Bash call's output).
const PY = { name: 'Bash', result: '98 48 49 1\n[26, 23, 22, 18, 9]' };
const WITH_TOTALS = [PER_SPECIES, PY];
const LIST =
  '- _Ithomia salapia salapia_: 26 (10 ♀, 15 ♂, 1 sin sexo)\n- _Mechanitis messenoides deceptus_: 23 (13 ♀, 10 ♂)\n' +
  '- _Mechanitis polymnia proceriformis_: 22 (9 ♀, 13 ♂)\n- _Mechanitis messenoides intermedia_: 18 (12 ♀, 6 ♂)\n- _Mechanitis lysimnia_: 9 (4 ♀, 5 ♂)';
const run = (finalText, results, prompts = ['¿cuántas emergidas tuvimos en octubre y de qué especies?']) => ({
  finalText,
  turnTexts: [finalText],
  prompts,
  tools: results.map(result => (typeof result === 'string' ? { name: 'query', result } : result)),
  proposals: [],
});

test('P1: «41 ♀» added up from 10, 13, 9, 12 and 4 fails; 48 from a python sum passes', () => {
  const wrong = `En octubre hubo *98* emergidas en la base: *41 ♀*, *49 ♂* y 1 sin sexo.\n\n${LIST}`;
  const bad = numbersCheck(run(wrong, WITH_TOTALS), wrong);
  assert.equal(bad.pass, false);
  assert.match(bad.detail, /^not in the tool results: 41; parts 41 \+ 49 \+ 1 = 91, not 98$/);
  const right = `${LIST}\n\nEn total *98*: *48 ♀*, *49 ♂* y 1 sin sexo.`;
  assert.equal(numbersCheck(run(right, WITH_TOTALS), right).pass, true);
  // The same right reply without the python sums: 98, 48 and 49 were added up in the head, so it fails too.
  const unsupported = numbersCheck(run(right, [PER_SPECIES]), right);
  assert.equal(unsupported.pass, false);
  assert.match(unsupported.detail, /: 26, 23, 22, 18, 98, 48, 49$/);
  // Through grade(): the check fails the sample.
  assert.equal(grade({ checks: [{ numbers: true }] }, run(wrong, WITH_TOTALS)).score, 0);
  assert.equal(grade({ checks: [{ numbers: true }] }, run(right, WITH_TOTALS)).score, 1);
});

test('dates, times, years, IDs and codes are not numbers to check', () => {
  const text =
    'Según la base al 31/10/2026, copia de 2026-10-31 00:40. Las fechas van del 1 al 14-oct y del 3 de noviembre; ' +
    'la última el 14/10. CAM070903, A7E.1, W0B.1, 5VB, FS90415684 y el transecto T2 en 2025. Ver https://ithomiini-ikiam.com/#/x?row=77.\n1. primero';
  assert.deepEqual(numbersIn(text), []);
  assert.deepEqual(numbersIn('Hay 2.752 desapariciones (25 %), 0,8 % y 1,264 silvestres').map(n => n.value), [2752, 25, 0.8, 1264]);
  assert.deepEqual(numbersIn('*48* hembras, 49 machos').map(n => n.value), [48, 49]);
});

test('numbers in the prompt, in allow, and in any tool result are supported; no result recorded is said', () => {
  const r = run('Del stock de deceptus hay 21 hembras de las 30 que pediste; la mediana es 22 días.', ['{"total":21}'], ['¿hay 30 hembras de deceptus?']);
  assert.equal(numbersCheck(r, r.finalText).pass, false);
  assert.equal(numbersCheck(r, r.finalText, { allow: [22] }).pass, true);
  const none = numbersCheck({ finalText: 'Son 7.', tools: [] }, 'Son 7.');
  assert.match(none.detail, /no tool result recorded/);
  // The turn option reads that turn's answer.
  const turns = { ...run('Son 48.', WITH_TOTALS), turnTexts: ['Son 41.', 'Son 48.'] };
  assert.equal(grade({ checks: [{ numbers: true, turn: 1 }] }, turns).score, 0);
  assert.equal(grade({ checks: [{ numbers: true, turn: 2 }] }, turns).score, 1);
});

test('parts against their whole: «98: 41 ♀, 49 ♂ y 1 sin sexo» is 91, caught without any data', () => {
  const wrong = `En octubre hubo *98* emergidas en la base: *41 ♀*, *49 ♂* y 1 sin sexo.\n\n${LIST}`;
  assert.deepEqual(partsMismatch(wrong), [{ whole: 98, parts: [41, 49, 1], sum: 91 }]);
  // Every species line adds up; so does the right total.
  assert.deepEqual(partsMismatch(`${LIST}\n\nEn total 98: 48 hembras, 49 machos y 1 sin sexo.`), []);
  // Parts that add up to another number of the line belong to it.
  assert.deepEqual(partsMismatch('Del clutch 997 salieron 172 filas (96 ♀, 76 ♂).'), []);
  // Through the numbers check, even when every number is in the tool results.
  const r = run(wrong, [...WITH_TOTALS, '41']);
  const out = numbersCheck(r, wrong);
  assert.equal(out.pass, false);
  assert.match(out.detail, /parts 41 \+ 49 \+ 1 = 91, not 98/);
});

test('«el 14» naming a day is not a number (r11 false positive)', () => {
  assert.deepEqual(numbersIn('Sí, octubre se acabó el 14. Las fechas van del 3 al 14, y desde el 2 y el 5.'), []);
  assert.deepEqual(numbersIn('Hay 14 hembras y el 14 % sin sexo').map(n => n.value), [14, 14]);
});

test('numbers next to each other in tool output stay apart; n=48 is a number; a label before ♀/♂ parts is no whole', () => {
  // python prints sums separated by spaces, query by tabs: «63 90 153» is three numbers, «1\t761» two.
  const r = run('En total 153: 63 ♀ y 90 ♂, y 761 de Ecuador.', [{ name: 'Bash', result: '63 90 153 [23, 62, 14, 9, 45]' }, 'SPECIES\tec\tn\nMechanitis messenoides\t1\t761']);
  assert.equal(numbersCheck(r, r.finalText).pass, true);
  assert.deepEqual(numbersIn('63 90 153').map(n => n.value), [63, 90, 153]);
  // A compact list or CSV row supports its numbers.
  assert.equal(numbersCheck(run('Son 13 y 48.', ['[10,13,9]', '48,49']), 'Son 13 y 48.').pass, true);
  // n=41 is a count, not a code: unsupported 41 fails, supported 48 passes.
  assert.equal(numbersCheck(run('Hembras: n=41.', [PER_SPECIES]), 'Hembras: n=41.').pass, false);
  assert.equal(numbersCheck(run('Son 48 hembras.', ['n=48']), 'Son 48 hembras.').pass, true);
  // «Jaula 2: 4 ♀ y 5 ♂», «En 30 días hubo 48 ♀ y 49 ♂»: the parts are not of the number before them.
  assert.deepEqual(partsMismatch('Jaula 2: 4 ♀ y 5 ♂\nRack 12: 10 hembras, 15 machos\nEn 30 días hubo 48 ♀ y 49 ♂'), []);
  assert.deepEqual(partsMismatch('Jaula 12: 4 ♀ y 5 ♂\nEl clutch 997 dio 40 ♀ y 30 ♂'), []);
  const cage = numbersCheck({ tools: [{ result: 'Jaula 12: 4 ♀ y 5 ♂' }] }, 'Jaula 12: 4 ♀ y 5 ♂');
  assert.equal(cage.pass, true);
});
