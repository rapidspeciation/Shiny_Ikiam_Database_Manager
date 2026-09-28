import { createRouter, createWebHashHistory } from 'vue-router'
import LoginView from './views/LoginView.vue'

/** `open`: visible without an account (summaries only). */
export const tabs = [
  { path: '/inicio', name: 'home', label: 'Inicio', open: true, component: () => import('./views/HomeView.vue') },
  { path: '/tablas', name: 'tables', label: 'Tablas', component: () => import('./views/TablesView.vue') },
  { path: '/colecta', name: 'collect', label: 'Colecta', component: () => import('./views/CollectView.vue') },
  { path: '/monitoreo', name: 'monitoring', label: 'Monitoreo', open: true, component: () => import('./views/MonitoringView.vue') },
  { path: '/muertes', name: 'deaths', label: 'Muertes', component: () => import('./views/DeathsView.vue') },
  { path: '/tubos', name: 'tubes', label: 'Tubos', component: () => import('./views/TubesView.vue') },
  { path: '/emergidos', name: 'emerged', label: 'Emergidos', component: () => import('./views/EmergedView.vue') },
  { path: '/posturas', name: 'clutches', label: 'Posturas', component: () => import('./views/ClutchesView.vue') },
  { path: '/historial', name: 'history', label: 'Historial', component: () => import('./views/HistoryView.vue') },
  { path: '/asistente', name: 'assistant', label: 'Asistente', component: () => import('./views/AssistantView.vue') },
]

/** Pages a visitor without an account can open. */
export const openPaths = new Set(tabs.filter(t => 'open' in t && t.open).map(t => t.path))

export const router = createRouter({
  history: createWebHashHistory(),
  routes: [
    { path: '/', redirect: '/inicio' },
    ...tabs.map(({ path, name, component }) => ({ path, name, component })),
    { path: '/usuarios', name: 'users', component: () => import('./views/UsersView.vue') },
    { path: '/activar', name: 'activate', component: () => import('./views/ActivateView.vue') },
    { path: '/entrar', name: 'login', component: LoginView },
    { path: '/:pathMatch(.*)*', redirect: '/inicio' },
  ],
})
