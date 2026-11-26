// A day named in a text the server builds (server/messages.mjs dateMsg): the Spanish text has it as the app writes
// days everywhere (7-Nov-26, frontend/src/lib/dates.ts), and its descriptor carries the day itself, so the interface
// writes it in the reader's language.
import test from 'node:test';
import assert from 'node:assert/strict';
import { dateMsg, msg, withoutMsgs } from '../server/messages.mjs';

test('a day in a server text: the app\'s one date shape in Spanish, and the day itself for the interface', () => {
  const m = msg('{species} llegó a {limit} preservadas el {date}', { species: 'Oleria onega', limit: 30, date: dateMsg('2026-01-07') });
  assert.equal(m.text, 'Oleria onega llegó a 30 preservadas el 7-Ene-26');
  assert.deepEqual(m.msg.vars, { species: 'Oleria onega', limit: 30, date: { date: '2026-01-07' } });
});

test('several days are each written the same way, joined; no day is a dash', () => {
  const m = msg('Censo del {dates}', { dates: ['2026-10-01', '2026-10-02'].map(dateMsg) });
  assert.equal(m.text, 'Censo del 1-Oct-26, 2-Oct-26');
  assert.deepEqual(m.msg.vars.dates, [{ date: '2026-10-01' }, { date: '2026-10-02' }]);
  assert.equal(msg('último {date}', { date: dateMsg(null) }).text, 'último —');
  assert.equal(msg('último {date}', { date: dateMsg(undefined) }).msg.vars.date, '—');
});

test('the assistant reads the Spanish text, without the descriptors', () => {
  const alert = { text: msg('Visto el {date}.', { date: dateMsg('2026-11-12') }).text, textMsg: { key: 'Visto el {date}.', vars: { date: { date: '2026-11-12' } } } };
  assert.deepEqual(withoutMsgs(alert), { text: 'Visto el 12-Nov-26.' });
});
