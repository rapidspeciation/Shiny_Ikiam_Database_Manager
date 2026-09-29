import { readdirSync, readFileSync, statSync } from 'node:fs'
import { join } from 'node:path'
import { describe, expect, it } from 'vitest'
import { locale, t, tn } from '../i18n'
import { en } from '../../locales/en'

const SRC = join(__dirname, '..', '..')
function files(dir: string): string[] {
  return readdirSync(dir).flatMap(name => {
    const path = join(dir, name)
    if (statSync(path).isDirectory()) return name === '__tests__' || name === 'locales' ? [] : files(path)
    return /\.(vue|ts)$/.test(name) ? [path] : []
  })
}
/** Every literal passed to t / $t (first argument) and tn / $tn (second and third) in the source. */
function keys(): string[] {
  const out = new Set<string>()
  const str = String.raw`'((?:[^'\\\n]|\\.)*)'|"((?:[^"\\\n]|\\.)*)"`
  const one = new RegExp(String.raw`(?<![\w$.])\$?t\(\s*(?:` + str + ')', 'g')
  const plural = new RegExp(String.raw`(?<![\w$.])\$?tn\([^,()]{1,80},\s*(?:` + str + String.raw`)\s*,\s*(?:` + str + ')', 'g')
  const unescape = (v: string) => v.replace(/\\(.)/g, '$1')
  for (const file of files(SRC)) {
    const text = readFileSync(file, 'utf8')
    for (const m of text.matchAll(one)) out.add(unescape(m[1] ?? m[2]))
    for (const m of text.matchAll(plural)) {
      out.add(unescape(m[1] ?? m[2]))
      out.add(unescape(m[3] ?? m[4]))
    }
  }
  return [...out]
}

describe('i18n', () => {
  it('English by default, Spanish on request, placeholders filled', () => {
    locale.value = 'en'
    expect(t('Guardar')).toBe('Save')
    expect(t('texto sin traducción {x}', { x: 1 })).toBe('texto sin traducción 1')
    locale.value = 'es'
    expect(t('Guardar')).toBe('Guardar')
    locale.value = 'en'
    expect(tn(1, '{n} fila', '{n} filas')).toBe('1 row')
    expect(tn(3, '{n} fila', '{n} filas')).toBe('3 rows')
  })
  it('every text passed to t() has its English', () => {
    const missing = keys().filter(k => /[a-záéíóúñ]/i.test(k) && !(k in en))
    expect(missing).toEqual([])
  })
  it('no empty translations, same placeholders in both languages', () => {
    for (const [es, text] of Object.entries(en)) {
      expect(text.trim(), es).not.toBe('')
      const vars = (s: string) =>
        [...s.matchAll(/\{(\w+)\}/g)]
          .map(m => m[1])
          .sort()
          .join(',')
      expect(vars(text), es).toBe(vars(es))
    }
  })
})
