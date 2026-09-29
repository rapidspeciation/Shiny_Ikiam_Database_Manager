/**
 * Translation for files the server also loads (dates.ts through monitoring.ts),
 * which cannot import Vue: calls go to the translator that lib/i18n.ts
 * registers in the browser. On the server nothing is registered, so the
 * Spanish text is used (with its {placeholders} filled).
 */
type Vars = Record<string, string | number | null | undefined>
type Translator = { t: (es: string, vars?: Vars) => string; intlLocale: () => string }

const fill = (text: string, vars?: Vars) =>
  vars ? text.replace(/\{(\w+)\}/g, (all, k: string) => (k in vars ? String(vars[k] ?? '') : all)) : text
let current: Translator = { t: fill, intlLocale: () => 'es-EC' }

export function setTranslator(translator: Translator) {
  current = translator
}
export const t = (es: string, vars?: Vars) => current.t(es, vars)
export const tn = (n: number, one: string, many: string, vars?: Vars) => current.t(n === 1 ? one : many, { n, ...vars })
export const intlLocale = () => current.intlLocale()
