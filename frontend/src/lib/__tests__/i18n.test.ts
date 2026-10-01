import { readdirSync, readFileSync, statSync } from 'node:fs'
import { join } from 'node:path'
import { describe, expect, it } from 'vitest'
import { learnMsg, locale, t, tm, tn, tx } from '../i18n'
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

/**
 * Every template the server builds a text from (server/messages.mjs): literals
 * in msg(…) / tpl(…) (first argument) and msgn(…) (second and third).
 */
function serverTemplates(): string[] {
  const dir = join(SRC, '..', '..', 'server')
  const out = new Set<string>()
  const str = String.raw`'((?:[^'\\\n]|\\.)*)'`
  const one = new RegExp(String.raw`(?<![\w$.])(?:msg|tpl)\(\s*` + str, 'g')
  const plural = new RegExp(String.raw`(?<![\w$.])msgn\([^,()]{1,80},\s*` + str + String.raw`\s*,\s*` + str, 'g')
  // Including the folders of server/ (server/suggestions/).
  const modules = (d: string): string[] =>
    readdirSync(d).flatMap(n => (statSync(join(d, n)).isDirectory() ? modules(join(d, n)) : n.endsWith('.mjs') ? [join(d, n)] : []))
  for (const path of modules(dir)) {
    const text = readFileSync(path, 'utf8')
    for (const m of text.matchAll(one)) out.add(m[1])
    for (const m of text.matchAll(plural)) out.add(m[1]).add(m[2])
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
  it('every template the server builds texts from has its English', () => {
    const templates = serverTemplates()
    expect(templates.length).toBeGreaterThan(100)
    expect(templates.filter(k => /[a-záéíóúñ]/i.test(k) && !(k in en))).toEqual([])
  })
  it('server descriptors: nested words and lists translated, sheet values kept, Spanish unchanged', () => {
    const m = {
      key: '{head}: {items} y {more} más',
      vars: { head: { key: '{n} filas restauradas', vars: { n: 10 } }, items: ['A0D–A8D', 'R4D'], more: 2 },
    }
    locale.value = 'en'
    expect(tm(m)).toBe('10 rows restored: A0D–A8D, R4D and 2 more')
    expect(tx('10 filas restauradas: …', m)).toBe('10 rows restored: A0D–A8D, R4D and 2 more')
    expect(tx('Sin cambios guardados')).toBe('No saved changes')
    // An error with values: t(message) finds it through the descriptor that came with it.
    learnMsg('Otra persona cambió Sex en la hoja', { key: 'Otra persona cambió {field} en la hoja', vars: { field: 'Sex' } })
    expect(t('Otra persona cambió Sex en la hoja')).toBe('Someone else changed Sex in the sheet')
    locale.value = 'es'
    expect(tm(m)).toBe('10 filas restauradas: A0D–A8D, R4D y 2 más')
    expect(t('Otra persona cambió Sex en la hoja')).toBe('Otra persona cambió Sex en la hoja')
    locale.value = 'en'
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
