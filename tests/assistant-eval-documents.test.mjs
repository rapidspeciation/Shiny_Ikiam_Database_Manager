// The key-documents eval cases (tools/assistant-eval/tasks/documents.json) graded on written answers, no AI:
// the 18 Nov baseline answers fail, answers with the contract in force and the July 2025 letters pass.
import test from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { grade } from '../tools/assistant-eval/grade.mjs';

const tasks = JSON.parse(readFileSync(new URL('../tools/assistant-eval/tasks/documents.json', import.meta.url), 'utf8')).tasks;
const task = id => tasks.find(t => t.id === id);
const score = (id, finalText) => grade(task(id), { finalText, turnTexts: [finalText], tools: [], proposals: [] }, { checkFiles: false }).score;

test('K01: the 2024-0443 contract with its link or file and its validity to 7 Jan 2028', () => {
  // The baseline answer (18 Nov): the old contract, «none in force».
  assert.ok(score('K01', 'No encontré el archivo. El contrato vigente era hasta el 14/04/2026 (acta 115); no hay contrato vigente ahora.') < 1);
  assert.equal(score('K01', 'El más reciente es el *MAATE-DBI-CM-2024-0443* (firmado el 7 de enero de 2025, vigente hasta el 7 de enero de 2028): https://drive.google.com/file/d/XYZ/view'), 1);
  assert.equal(score('K01', 'Contrato marco MAATE-DBI-CM-2024-0443.pdf en Drive, vigencia 3 años: 07/01/2025 a 07/01/2028.'), 1);
  assert.ok(score('K01', 'Es el MAATE-DBI-CM-2024-0443, vigente hasta 2028: https://drive.google.com/file/d/XYZ/view') < 1, 'the day is needed');
});

test('K02: the July 2025 letters, and that they cite expired contracts (or no letter yet for 2024-0443)', () => {
  assert.ok(score('K02', 'Sí: en la base hay 63 colectores en la lista Collector.') < 1);
  assert.equal(score('K02', 'Sí: «Carta MAE para salidas campo Jul2025» y «Carta MAE salida campo Jul2025», en la carpeta Contrato Marco 2025. Ojo: citan los contratos 2023-0286 y 2021-0176, ya vencidos.'), 1);
  assert.equal(score('K02', 'Hay dos cartas de julio de 2025 con los colectores. Para el contrato 2024-0443 todavía no hay carta.'), 1);
  assert.ok(score('K02', 'Hay dos cartas de julio de 2025 con los colectores.') < 1, 'the expired contracts must be said');
});
