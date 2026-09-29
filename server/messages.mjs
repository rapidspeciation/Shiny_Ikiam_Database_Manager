// Texts the server builds for people (Historial summaries, Revisión problems…)
// in Spanish, plus a descriptor the interface translates: { key, vars }, where
// key is the Spanish template with {placeholders} and its English is in
// frontend/src/locales/en/server-built.ts (keyed by the same template). Both
// come from the same template, so they cannot drift.
//
//   const m = msg('{value} también está en {places}', { value: 'A0D', places: ['Tube_2_id', 'R4D'] })
//   m.text → 'A0D también está en Tube_2_id, R4D'
//   m.msg  → { key: '{value} también está en {places}', vars: { value: 'A0D', places: ['Tube_2_id', 'R4D'] } }
//
// A var can be another msg (a Spanish word the interface translates too, e.g.
// msg('sin especie')) or a list, joined with ", ". Values from the sheets (IDs,
// species, column names, codes) go as plain strings and are never translated.
// Every template is a literal inside msg(…), msgn(…) or tpl(…): the frontend's
// i18n test scans them and fails when one has no English.

const isMsg = v => !!v && typeof v === 'object' && !Array.isArray(v) && typeof v.text === 'string' && v.msg;
const textOf = v => (Array.isArray(v) ? v.map(textOf).join(', ') : isMsg(v) ? v.text : v === null || v === undefined ? '' : String(v));
const descOf = v => (Array.isArray(v) ? v.map(descOf) : isMsg(v) ? v.msg : v);

/** The Spanish text of a template and its descriptor: { text, msg: { key, vars } }. */
export function msg(key, vars = {}) {
  const text = key.replace(/\{(\w+)\}/g, (all, k) => (k in vars ? textOf(vars[k]) : all));
  const sent = Object.fromEntries(
    Object.entries(vars)
      .filter(([, v]) => v !== undefined)
      .map(([k, v]) => [k, descOf(v)]),
  );
  return { text, msg: Object.keys(sent).length ? { key, vars: sent } : { key } };
}
/** Singular or plural template by n; {n} is filled with n. */
export const msgn = (n, one, many, vars = {}) => msg(n === 1 ? one : many, { n, ...vars });
/** { [name]: text, [name + 'Msg']: descriptor } from a msg(), or { [name]: text } from a plain text. */
export const textFields = (name, m) => (typeof m === 'string' ? { [name]: m } : { [name]: m.text, [`${name}Msg`]: m.msg });
/** An Error with a msg() or plain message (index.mjs sends messageMsg with it). */
export const msgError = (message, props = {}) =>
  Object.assign(new Error(typeof message === 'string' ? message : message.text), props, typeof message === 'string' ? {} : { messageMsg: message.msg });

/** Marks a template kept in a table (msg(TABLE[x]) later), so the i18n test finds it. */
export const tpl = key => key;

/**
 * A copy without the descriptors (fields named …Msg, at any depth), for the
 * assistant's tools: it reads the Spanish text and replies in the person's language.
 */
export function withoutMsgs(value) {
  if (Array.isArray(value)) return value.map(withoutMsgs);
  if (!value || typeof value !== 'object') return value;
  return Object.fromEntries(Object.entries(value).filter(([k]) => !k.endsWith('Msg')).map(([k, v]) => [k, withoutMsgs(v)]));
}
