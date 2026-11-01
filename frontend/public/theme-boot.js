// Light or dark before the first paint, so a reload in dark never flashes light (src/lib/theme.ts keeps it
// right afterwards). A plain script loaded in <head>: the pages' CSP allows no inline scripts.
;(function () {
  var pref = null
  try {
    pref = localStorage.getItem('ui:theme')
  } catch (e) {}
  var dark = pref === 'dark' || (pref !== 'light' && window.matchMedia && matchMedia('(prefers-color-scheme: dark)').matches)
  var root = document.documentElement
  root.setAttribute('data-theme', dark ? 'dark' : 'light')
  root.style.colorScheme = dark ? 'dark' : 'light'
  var meta = document.querySelector('meta[name="theme-color"]')
  if (meta) meta.setAttribute('content', '#1f4d3a')
})()
