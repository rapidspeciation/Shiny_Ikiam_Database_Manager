/**
 * The Identificar tab's page (identificar/index.html; server/offline-id.mjs sends it cross-origin isolated so its
 * models can use threads). The same header as the app, with the same tabs: any tab but this one opens in the app
 * (../#/<tab>), so this router only knows its own route.
 */
import { createApp } from 'vue'
import { createPinia } from 'pinia'
import { createRouter, createWebHashHistory } from 'vue-router'
import IdentifyApp from './IdentifyApp.vue'
import IdentifyView from '../views/IdentifyView.vue'
import { tabs } from '../tabs'
import { appRoot } from '../lib/appRoot'
import '../style.css'
import '../lib/theme'
import { t, tn } from '../lib/i18n'

const routes = [
  { path: '/', redirect: '/identificar' },
  { path: '/identificar', name: 'identify', component: IdentifyView },
  // The app's other pages: real routes so the header's links resolve, left at once for the app.
  ...tabs.filter(tab => tab.path !== '/identificar').map(tab => ({ path: tab.path, component: IdentifyView })),
  { path: '/:pathMatch(.*)*', component: IdentifyView },
]
const router = createRouter({ history: createWebHashHistory(), routes })
router.beforeEach(to => {
  if (to.path === '/identificar' || to.path === '/') return true
  location.assign(`${appRoot()}#${to.fullPath}`)
  return false
})

const app = createApp(IdentifyApp)
app.config.globalProperties.$t = t
app.config.globalProperties.$tn = tn
app.use(createPinia()).use(router).mount('#app')

if (import.meta.env.PROD && 'serviceWorker' in navigator) navigator.serviceWorker.register(new URL('sw.js', appRoot()).href).catch(() => {})
