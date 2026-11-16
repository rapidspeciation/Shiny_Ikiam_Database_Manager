// The eval's no-AI number check (tools/assistant-eval/grade.mjs `numbers`): a number the chat's tool results do not
// hold fails the case. P1 (16 Nov 2026): the per-species female counts were 10, 13, 9, 12 and 4, and the reply said
// «41 ♀»; they make 48.
import test from 'node:test';
import assert from 'node:assert/strict';
import { grade, numbersCheck, numbersIn } from '../tools/assistant-eval/grade.mjs';

const PER_SPECIES =
  'SPECIES\tSex\tn\nIthomia salapia salapia\tfemale\t10\nIthomia salapia salapia\tmale\t15\nIthomia salapia salapia\t\t1\n' +
  'Mechanitis messenoides deceptus\tfemale\t13\nMechanitis messenoides deceptus\tmale\t10\nMechanitis polymnia proceriformis\tfemale\t9\n' +
  'Mechanitis polymnia proceriformis\tmale\t13\nMechanitis messenoides intermedia\tfemale\t12\nMechanitis messenoides intermedia\tmale\t6\n' +
  'Mechanitis lysimnia\tfemale\t4\nMechanitis lysimnia\tmale\t5\n(Copy of 2026-10-31 00:40 UTC, current: nothing was saved or synced since.)';
// As the query tool prints them (server/query-tool.mjs sumsText).
const WITH_TOTALS =
  `${PER_SPECIES}\n(Totals over all 11 rows: n 98.)\n(n by Sex: male 49, female 48, (empty) 1.)\n` +
  '(n by SPECIES: Ithomia salapia salapia 26, Mechanitis messenoides deceptus 23, Mechanitis polymnia proceriformis 22, Mechanitis messenoides intermedia 18, Mechanitis lysimnia 9.)';
const LIST =
  '- _Ithomia salapia salapia_: 26 (10 ♀, 15 ♂, 1 sin sexo)\n- _Mechanitis messenoides deceptus_: 23 (13 ♀, 10 ♂)\n' +
  '- _Mechanitis polymnia proceriformis_: 22 (9 ♀, 13 ♂)\n- _Mechanitis messenoides intermedia_: 18 (12 ♀, 6 ♂)\n- _Mechanitis lysimnia_: 9 (4 ♀, 5 ♂)';
const run = (finalText, results, prompts = ['¿cuántas emergidas tuvimos en octubre y de qué especies?']) => ({
  finalText,
  turnTexts: [finalText],
  prompts,
  tools: results.map(result => ({ name: 'query', result })),
  proposals: [],
});

test('P1: «41 ♀» added up from 10, 13, 9, 12 and 4 fails; 48 from a tool total passes', () => {
  const wrong = `En octubre hubo *98* emergidas en la base: *41 ♀*, *49 ♂* y 1 sin sexo.\n\n${LIST}`;
  const bad = numbersCheck(run(wrong, [WITH_TOTALS]), wrong);
  assert.equal(bad.pass, false);
  assert.match(bad.detail, /: 41$/);
  const right = `${LIST}\n\nEn total *98*: *48 ♀*, *49 ♂* y 1 sin sexo.`;
  assert.equal(numbersCheck(run(right, [WITH_TOTALS]), right).pass, true);
  // The same right reply without the tool's totals: 98, 48 and 49 were added up, so it fails too.
  const unsupported = numbersCheck(run(right, [PER_SPECIES]), right);
  assert.equal(unsupported.pass, false);
  assert.match(unsupported.detail, /: 26, 23, 22, 18, 98, 48, 49$/);
  // Through grade(): the check fails the sample.
  assert.equal(grade({ checks: [{ numbers: true }] }, run(wrong, [WITH_TOTALS])).score, 0);
  assert.equal(grade({ checks: [{ numbers: true }] }, run(right, [WITH_TOTALS])).score, 1);
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
  const turns = { ...run('Son 48.', [WITH_TOTALS]), turnTexts: ['Son 41.', 'Son 48.'] };
  assert.equal(grade({ checks: [{ numbers: true, turn: 1 }] }, turns).score, 0);
  assert.equal(grade({ checks: [{ numbers: true, turn: 2 }] }, turns).score, 1);
});
