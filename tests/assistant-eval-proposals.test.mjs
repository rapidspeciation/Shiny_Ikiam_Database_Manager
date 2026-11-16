// The eval grader's proposal checks (tools/assistant-eval/grade.mjs `proposal`): a drafted, not discarded change
// has the value; `$TODAY` is the run's day also as a Sheets date serial; `blank` passes only on an empty value.
// Proposals are written by hand as the chat's live proposals look; expected results are worked out by hand.
import test from 'node:test';
import assert from 'node:assert/strict';
import { grade } from '../tools/assistant-eval/grade.mjs';

const TODAY = '2026-11-08';
// 8 Nov 2026 as a Google Sheets date: days since 30 Dec 1899 (worked out apart: Python's date difference).
const SERIAL_TODAY = 46334;
const change = (values, more = {}) => ({ sheet: 'Collection_data', row: 7, label: 'CAM074001', values, ...more });
const live = (...changes) => ({ status: 'pending', changes });
/** Whether the one proposal check passes on these proposals. */
const passes = (spec, proposals) => {
  const { results } = grade({ checks: [{ proposal: { sheet: 'Collection_data', row: 7, ...spec } }] }, { finalText: '', tools: [], proposals }, { today: TODAY });
  assert.equal(results.length, 1);
  return results[0].pass;
};

test('proposal equals: the drafted value, case, accents and spaces aside; another value fails', () => {
  const proposals = [live(change({ Sex: 'Male', Notes_Collection_data: ' Ala rota ' }))];
  assert.equal(passes({ field: 'Sex', equals: 'male' }, proposals), true);
  assert.equal(passes({ field: 'Notes_Collection_data', equals: 'ála rota' }, proposals), true);
  assert.equal(passes({ field: 'Sex', equals: 'female' }, proposals), false);
  assert.equal(passes({ field: 'Collector', equals: 'FCH - Franz Chandi' }, proposals), false, 'a field not proposed');
  assert.equal(passes({ field: 'Sex', equals: 'male', row: 8 }, proposals), false, 'another row');
  // By ID: the change's label or its Insectary_ID.
  assert.equal(passes({ row: undefined, id: 'CAM074001', field: 'Sex', equals: 'male' }, proposals), true);
});

test('proposal: a discarded proposal does not count, and the last live one wins', () => {
  const discarded = { status: 'discarded', changes: [change({ Sex: 'male' })] };
  assert.equal(passes({ field: 'Sex', equals: 'male' }, [discarded]), false, 'only a discarded draft');
  const proposals = [live(change({ Sex: 'male' })), discarded, live(change({ Sex: 'female' }))];
  assert.equal(passes({ field: 'Sex', equals: 'female' }, proposals), true);
  assert.equal(passes({ field: 'Sex', equals: 'male' }, proposals), false, 'the earlier value was replaced');
  // A discarded draft listed after the live one does not replace it.
  assert.equal(passes({ field: 'Sex', equals: 'female' }, [live(change({ Sex: 'female' })), discarded]), true);
});

test('proposal equals $TODAY: the run day as a Sheets serial, ISO or dd/mm/yyyy; the day before, or mm/dd, fails', () => {
  const day = value => passes({ field: 'Death_date', equals: '$TODAY' }, [live(change({ Death_date: value }))]);
  assert.equal(day(SERIAL_TODAY), true);
  assert.equal(day(String(SERIAL_TODAY)), true);
  assert.equal(day('2026-11-08'), true);
  assert.equal(day('8/11/2026'), true);
  assert.equal(day(SERIAL_TODAY - 1), false);
  assert.equal(day(SERIAL_TODAY + 1), false);
  assert.equal(day('11/8/2026'), false, '11 August');
  assert.equal(day(''), false);
});

test('proposal blank: an empty or NA value passes, a value fails; an untouched field only when known blank', () => {
  const blank = (proposals, more = {}) => passes({ field: 'Death_date', blank: true, ...more }, proposals);
  assert.equal(blank([live(change({ Death_date: '' }))]), true);
  assert.equal(blank([live(change({ Death_date: 'NA' }))]), true);
  assert.equal(blank([live(change({ Death_date: null }))]), true);
  assert.equal(blank([live(change({ Death_date: SERIAL_TODAY }))]), false);
  assert.equal(blank([live(change({ Death_date: 'NOT_COLLECTED' }))]), false);
  // The field not in the proposal: blank only if the seed had it blank (seedBlank) or its recorded before was.
  const untouched = before => [live(change({ Sex: 'male' }, { before }))];
  assert.equal(blank(untouched({}), { seedBlank: true }), true);
  assert.equal(blank(untouched({ Death_date: '' })), true);
  assert.equal(blank(untouched({ Death_date: 46300 })), false);
  assert.equal(blank(untouched({})), false, 'seed value unknown');
});

/** Whether one check of another kind passes on these proposals. */
const check = (c, proposals) => {
  const { results } = grade({ checks: [c] }, { finalText: '', tools: [], proposals }, { today: TODAY });
  assert.equal(results.length, 1);
  return results[0].pass;
};

test('proposalOnly: every live draft stays in the rows and fields allowed; a discarded stray one does not count', () => {
  const only = proposals => check({ proposalOnly: { sheet: 'Collection_data', rows: [7], fields: ['Sex'] } }, proposals);
  assert.equal(only([live(change({ Sex: 'female' }))]), true);
  assert.equal(only([live(change({ Sex: 'female' }, { row: 8 }))]), false, 'another row');
  assert.equal(only([live(change({ Sex: 'female', Death_date: SERIAL_TODAY }))]), false, 'another field');
  assert.equal(only([live(change({ Sex: 'female' }, { sheet: 'Insectary_data' }))]), false, 'another sheet');
  assert.equal(only([live(change({ Sex: 'female' })), { status: 'discarded', changes: [change({ Sex: 'male' }, { row: 8 })] }]), true);
});

test('noApply: the chat applied nothing; a proposal applied, or with an appliedAt, fails', () => {
  const none = proposals => check({ noApply: true }, proposals);
  assert.equal(none([]), true);
  assert.equal(none([live(change({ Sex: 'female' }))]), true);
  assert.equal(none([{ status: 'applied', changes: [change({ Sex: 'female' })] }]), false);
  assert.equal(none([{ status: 'pending', appliedAt: '2026-11-08T15:00:00Z', changes: [change({ Sex: 'female' })] }]), false);
});
