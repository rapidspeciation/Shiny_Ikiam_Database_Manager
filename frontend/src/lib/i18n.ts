/**
 * The interface language: English by default, Spanish on request (the EN/ES
 * button in the header, remembered on the device). The Spanish text written
 * in the code is the key; its English is in src/locales/en/*.ts (one file per
 * area). A text missing there shows in Spanish (a test lists them).
 *
 *   t('Guardar')                       → "Save"
 *   t('{n} filas nuevas', { n: 3 })     → "3 new rows"
 *   tn(n, '{n} fila', '{n} filas')     → singular or plural
 *
 * Templates use $t / $tn; scripts import t / tn. Values computed in scripts
 * must read t inside a computed or a function, so they follow the language.
 * Sheet names, column names and codes (Insectary_ID, DY_(dry)…) are never
 * translated.
 *
 * Texts the server builds (Historial summaries, Revisión problems, errors with
 * values) come with a descriptor { key, vars } (server/messages.mjs): tm(m) or
 * tx(text, m) show it; the English of its templates is in locales/en/server-built.ts.
 */
import { ref, watch } from 'vue'
import { en } from '../locales/en'
import { setTranslator } from './translate.ts'

export type Locale = 'en' | 'es'
const KEY = 'ui:locale'
const stored = typeof localStorage !== 'undefined' ? localStorage.getItem(KEY) : null
export const locale = ref<Locale>(stored === 'es' ? 'es' : 'en')
if (typeof document !== 'undefined') document.documentElement.lang = locale.value
watch(locale, value => {
  if (typeof localStorage !== 'undefined') localStorage.setItem(KEY, value)
  if (typeof document !== 'undefined') document.documentElement.lang = value
})

type Vars = Record<string, string | number | null | undefined>
const fill = (text: string, vars?: Vars) =>
  vars ? text.replace(/\{(\w+)\}/g, (all, k: string) => (k in vars ? String(vars[k] ?? '') : all)) : text

/** The text in the chosen language (Spanish is the key). */
export function t(es: string, vars?: Vars): string {
  if (locale.value !== 'en') return fill(es, vars)
  const english = en[es]
  if (english !== undefined) return fill(english, vars)
  // A server message with values in it (e.g. an error), learned from the descriptor that came with it.
  const known = vars ? undefined : learned.get(es)
  return known ? tm(known) : fill(es, vars)
}

/**
 * A text the server built (server/messages.mjs): its Spanish template with
 * {placeholders} and the values. A value can be another descriptor (a Spanish
 * word, translated too) or a list (joined with ", "); sheet values are plain.
 */
export interface Msg {
  key: string
  vars?: Record<string, MsgVar>
}
export type MsgVar = string | number | boolean | null | Msg | MsgVar[]
const varText = (v: MsgVar): string =>
  Array.isArray(v) ? v.map(varText).join(', ') : v !== null && typeof v === 'object' ? tm(v) : String(v ?? '')
/** A server descriptor in the chosen language (its Spanish template when there is no English). */
export function tm(m: Msg): string {
  const vars = m.vars ? Object.fromEntries(Object.entries(m.vars).map(([k, v]) => [k, varText(v)])) : undefined
  return fill(locale.value === 'en' ? (en[m.key] ?? m.key) : m.key, vars)
}
/** A server text in the chosen language: from its descriptor when it came with one, else through t(). */
export const tx = (text: string, m?: Msg | null) => (m ? tm(m) : t(text))

/** Spanish server texts → their descriptor, so t(message) translates messages with values too. */
const learned = new Map<string, Msg>()
export function learnMsg(text: unknown, m: Msg | null | undefined) {
  if (typeof text === 'string' && m?.key && m.key !== text) learned.set(text, m)
}
/** Singular or plural by n; {n} is filled with n. */
export function tn(n: number, one: string, many: string, vars?: Vars): string {
  return t(n === 1 ? one : many, { n, ...vars })
}
/** The locale for Intl (dates, numbers): en-GB keeps day-first dates. */
export const intlLocale = () => (locale.value === 'en' ? 'en-GB' : 'es-EC')

// Files shared with the server (dates.ts) translate through lib/translate.ts.
setTranslator({ t, intlLocale })
