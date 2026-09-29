import { createApp } from 'vue'
import { createPinia } from 'pinia'
import App from './App.vue'
import { router } from './router'
import './style.css'
import { updateAvailable, watchForUpdates } from './lib/updates'
import { t, tn } from './lib/i18n'

const app = createApp(App)
// $t / $tn in every template: the interface in English or Spanish (lib/i18n.ts).
app.config.globalProperties.$t = t
app.config.globalProperties.$tn = tn
app.use(createPinia()).use(router).mount('#app')

// A tab whose script belongs to a build replaced since the page opened: ask to reload (lib/updates.ts).
window.addEventListener('vite:preloadError', event => {
  event.preventDefault()
  updateAvailable.value = true
})
router.onError(error => {
  if (/dynamically imported module|Importing a module script failed|error loading dynamically/i.test(String(error?.message)))
    updateAvailable.value = true
})
watchForUpdates()

if (import.meta.env.PROD && 'serviceWorker' in navigator) navigator.serviceWorker.register('./sw.js').catch(() => {})
