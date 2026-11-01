import { readFileSync } from 'node:fs'
import { resolve } from 'node:path'
import { nextTick } from 'vue'
import { describe, expect, it, vi } from 'vitest'
import { THEME_COLOR, THEME_KEY, applyTheme, oppositeTheme, parsePref, resolveTheme, theme, themePref } from '../theme'

describe('theme preference', () => {
  it('reads anything unknown as the system choice', () => {
    expect(parsePref('dark')).toBe('dark')
    expect(parsePref('light')).toBe('light')
    expect(parsePref(null)).toBe('system')
    expect(parsePref('blue')).toBe('system')
  })
  it('follows the system only while on System', () => {
    expect(resolveTheme('system', true)).toBe('dark')
    expect(resolveTheme('system', false)).toBe('light')
    expect(resolveTheme('light', true)).toBe('light')
    expect(resolveTheme('dark', false)).toBe('dark')
  })
  it('puts the theme, the form controls’ scheme and the browser bar colour on the page', () => {
    const meta = document.createElement('meta')
    meta.name = 'theme-color'
    document.head.append(meta)
    applyTheme('dark')
    expect(document.documentElement.dataset.theme).toBe('dark')
    expect(document.documentElement.style.colorScheme).toBe('dark')
    expect(meta.getAttribute('content')).toBe(THEME_COLOR.dark)
    applyTheme('light')
    expect(meta.getAttribute('content')).toBe(THEME_COLOR.light)
    meta.remove()
  })
  it('remembers a picked theme on the device and forgets it on System', async () => {
    themePref.value = 'dark'
    expect(theme.value).toBe('dark')
    expect(document.documentElement.dataset.theme).toBe('dark')
    await nextTick()
    expect(localStorage.getItem(THEME_KEY)).toBe('dark')
    themePref.value = 'light'
    await nextTick()
    expect(localStorage.getItem(THEME_KEY)).toBe('light')
    expect(document.documentElement.dataset.theme).toBe('light')
    themePref.value = 'system'
    await nextTick()
    expect(localStorage.getItem(THEME_KEY)).toBeNull()
  })
  it('the toggle goes to the opposite of the theme in effect', () => {
    expect(oppositeTheme('light')).toBe('dark')
    expect(oppositeTheme('dark')).toBe('light')
  })
  it('follows an OS change live while on System, and ignores it once a theme is chosen', async () => {
    const listeners: ((e: { matches: boolean }) => void)[] = []
    vi.stubGlobal('matchMedia', () => ({ matches: false, addEventListener: (_: string, f: never) => listeners.push(f) }))
    vi.resetModules()
    const fresh = await import('../theme')
    expect(fresh.theme.value).toBe('light')
    listeners.forEach(f => f({ matches: true }))
    expect(fresh.theme.value).toBe('dark')
    expect(document.documentElement.dataset.theme).toBe('dark')
    fresh.themePref.value = 'light'
    expect(fresh.theme.value).toBe('light')
    listeners.forEach(f => f({ matches: false }))
    listeners.forEach(f => f({ matches: true }))
    expect(fresh.theme.value).toBe('light')
    fresh.themePref.value = 'system'
    expect(fresh.theme.value).toBe('dark')
    vi.unstubAllGlobals()
  })
  it('keeps the boot script in step (same key and bar colours)', () => {
    const boot = readFileSync(resolve(import.meta.dirname, '../../../public/theme-boot.js'), 'utf8')
    expect(boot).toContain(`'${THEME_KEY}'`)
    expect(boot).toContain(THEME_COLOR.light)
    expect(boot).toContain(THEME_COLOR.dark)
  })
})
