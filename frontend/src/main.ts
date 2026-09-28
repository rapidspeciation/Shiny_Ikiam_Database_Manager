import { createApp } from 'vue'
import { createPinia } from 'pinia'
import App from './App.vue'
import { router } from './router'
import './style.css'
import { updateAvailable, watchForUpdates } from './lib/updates'

createApp(App).use(createPinia()).use(router).mount('#app')

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
