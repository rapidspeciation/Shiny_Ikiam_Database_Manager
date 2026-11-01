/**
 * Light or dark: the system's choice by default, or the one picked in the user menu (System / Light / Dark),
 * remembered on the device. public/theme-boot.js sets the same attribute before the first paint, so a reload
 * in dark never flashes light; this module keeps it right afterwards and follows the system while on System.
 *
 * The colours themselves are role tokens in style.css (:root and [data-theme="dark"]); maps and charts read
 * them through cssColor() and rebuild when `theme` changes.
 */
import { ref, watch } from 'vue'

export type ThemePref = 'system' | 'light' | 'dark'
export type Theme = 'light' | 'dark'
/** Also read by public/theme-boot.js: keep the two in step. */
export const THEME_KEY = 'ui:theme'
/** The browser bar's colour per theme: the header green in both themes (as theme-boot.js). */
export const THEME_COLOR: Record<Theme, string> = { light: '#1f4d3a', dark: '#1f4d3a' }

/** A stored value, anything unknown being the system's choice. */
export const parsePref = (stored: string | null | undefined): ThemePref =>
  stored === 'light' || stored === 'dark' ? stored : 'system'
export const resolveTheme = (pref: ThemePref, systemDark: boolean): Theme =>
  pref === 'system' ? (systemDark ? 'dark' : 'light') : pref

const page = typeof document !== 'undefined' && typeof window !== 'undefined'
const query = page && window.matchMedia ? window.matchMedia('(prefers-color-scheme: dark)') : null
const read = () => {
  try {
    return parsePref(localStorage.getItem(THEME_KEY))
  } catch {
    return 'system'
  }
}

export const themePref = ref<ThemePref>(page ? read() : 'system')
const systemDark = ref(!!query?.matches)
export const theme = ref<Theme>(resolveTheme(themePref.value, systemDark.value))

/** Puts the theme on the page: the attribute the tokens hang on, the form controls' scheme and the browser bar. */
export function applyTheme(value: Theme, root: HTMLElement = document.documentElement) {
  root.dataset.theme = value
  root.style.colorScheme = value
  const meta = root.ownerDocument.querySelector('meta[name="theme-color"]')
  if (meta) meta.setAttribute('content', THEME_COLOR[value])
}

if (page) {
  applyTheme(theme.value)
  query?.addEventListener?.('change', event => (systemDark.value = event.matches))
  // Another tab changed it.
  window.addEventListener('storage', event => {
    if (event.key === THEME_KEY) themePref.value = parsePref(event.newValue)
  })
  // Synchronous, so the attribute is on the page before any component reacts to `theme` and reads cssColor().
  watch(
    [themePref, systemDark],
    ([pref, dark]) => {
      theme.value = resolveTheme(pref, dark)
    },
    { flush: 'sync' },
  )
  watch(themePref, pref => {
    try {
      if (pref === 'system') localStorage.removeItem(THEME_KEY)
      else localStorage.setItem(THEME_KEY, pref)
    } catch {
      // Private mode without storage: the choice lasts for the page.
    }
  })
  watch(theme, value => applyTheme(value), { flush: 'sync' })
}

/**
 * A role token's colour as the page shows it now (`cssColor('--fg')`), for what cannot read CSS
 * variables itself: ECharts, Leaflet's SVG markers, canvases. Read it inside a computed or a function
 * that runs again when `theme` changes.
 */
export function cssColor(name: string, fallback = '#000'): string {
  if (!page) return fallback
  const value = getComputedStyle(document.documentElement).getPropertyValue(name).trim()
  return value || fallback
}
