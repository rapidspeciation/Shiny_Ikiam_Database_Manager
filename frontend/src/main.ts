import { createApp } from 'vue'
import { createPinia } from 'pinia'
import App from './App.vue'
import { router } from './router'
import './style.css'

createApp(App).use(createPinia()).use(router).mount('#app')

/**
 * A page left open across a deploy asks for the previous build's scripts when
 * a tab is opened. If one can't be loaded, reload once to get the new build
 * (unsaved changes and the Colecta list are kept on this device). At most every
 * 30 s, so a real outage does not reload in a loop.
 */
function reloadForNewBuild() {
  const last = Number(sessionStorage.getItem('ithomiini:reloaded') || 0)
  if (Date.now() - last < 30_000) return false
  sessionStorage.setItem('ithomiini:reloaded', String(Date.now()))
  window.location.reload()
  return true
}
window.addEventListener('vite:preloadError', event => {
  if (reloadForNewBuild()) event.preventDefault()
})
router.onError(error => {
  if (/dynamically imported module|Importing a module script failed|error loading dynamically/i.test(String(error?.message))) reloadForNewBuild()
})

if (import.meta.env.PROD && 'serviceWorker' in navigator) navigator.serviceWorker.register('./sw.js').catch(() => {})
