import { createRouter, createWebHashHistory } from 'vue-router'

export const tabs = [
  { path: '/tablas', name: 'tables', label: 'Tablas', component: () => import('./views/TablesView.vue') },
  { path: '/colecta', name: 'collect', label: 'Colecta', component: () => import('./views/CollectView.vue') },
  { path: '/monitoreo', name: 'monitoring', label: 'Monitoreo', component: () => import('./views/MonitoringView.vue') },
  { path: '/muertes', name: 'deaths', label: 'Muertes', component: () => import('./views/DeathsView.vue') },
  { path: '/tubos', name: 'tubes', label: 'Tubos', component: () => import('./views/TubesView.vue') },
  { path: '/emergidos', name: 'emerged', label: 'Emergidos', component: () => import('./views/EmergedView.vue') },
  { path: '/posturas', name: 'clutches', label: 'Posturas', component: () => import('./views/ClutchesView.vue') },
  { path: '/historial', name: 'history', label: 'Historial', component: () => import('./views/HistoryView.vue') },
  { path: '/asistente', name: 'assistant', label: 'Asistente', component: () => import('./views/AssistantView.vue') },
]

export const router = createRouter({
  history: createWebHashHistory(),
  routes: [
    { path: '/', redirect: '/tablas' },
    ...tabs.map(({ path, name, component }) => ({ path, name, component })),
    { path: '/usuarios', name: 'users', component: () => import('./views/UsersView.vue') },
    { path: '/activar', name: 'activate', component: () => import('./views/ActivateView.vue') },
    { path: '/:pathMatch(.*)*', redirect: '/tablas' },
  ],
})
