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
  localStorage.setItem(KEY, value)
  document.documentElement.lang = value
})

type Vars = Record<string, string | number | null | undefined>
const fill = (text: string, vars?: Vars) =>
  vars ? text.replace(/\{(\w+)\}/g, (all, k: string) => (k in vars ? String(vars[k] ?? '') : all)) : text

/** The text in the chosen language (Spanish is the key). */
export function t(es: string, vars?: Vars): string {
  return fill(locale.value === 'en' ? (en[es] ?? es) : es, vars)
}
/** Singular or plural by n; {n} is filled with n. */
export function tn(n: number, one: string, many: string, vars?: Vars): string {
  return t(n === 1 ? one : many, { n, ...vars })
}
/** The locale for Intl (dates, numbers): en-GB keeps day-first dates. */
export const intlLocale = () => (locale.value === 'en' ? 'en-GB' : 'es-EC')

// Files shared with the server (dates.ts) translate through lib/translate.ts.
setTranslator({ t, intlLocale })
