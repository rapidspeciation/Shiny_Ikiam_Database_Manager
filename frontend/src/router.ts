import { createRouter, createWebHashHistory } from 'vue-router'
import LoginView from './views/LoginView.vue'

/** `open`: visible without an account (natural-history summaries only); `editors`: only for people who edit. */
export const tabs = [
  { path: '/inicio', name: 'home', label: 'Inicio', open: true, component: () => import('./views/HomeView.vue') },
  { path: '/tablas', name: 'tables', label: 'Buscador', component: () => import('./views/TablesView.vue') },
  { path: '/colecta', name: 'collect', label: 'Colecta', component: () => import('./views/CollectView.vue') },
  { path: '/monitoreo', name: 'monitoring', label: 'Monitoreo', component: () => import('./views/MonitoringView.vue') },
  { path: '/muertes', name: 'deaths', label: 'Muertes', component: () => import('./views/DeathsView.vue') },
  { path: '/tubos', name: 'tubes', label: 'Tubos', component: () => import('./views/TubesView.vue') },
  { path: '/emergidos', name: 'emerged', label: 'Emergidos', component: () => import('./views/EmergedView.vue') },
  { path: '/clutches', name: 'clutches', label: 'Clutches', component: () => import('./views/ClutchesView.vue') },
  { path: '/historial', name: 'history', label: 'Historial', component: () => import('./views/HistoryView.vue') },
  { path: '/asistente', name: 'assistant', label: 'Asistente', component: () => import('./views/AssistantView.vue') },
  // Not a daily task: last.
  { path: '/revision', name: 'review', label: 'Revisión', editors: true, component: () => import('./views/RevisionView.vue') },
]

/** Pages a visitor without an account can open. */
export const openPaths = new Set(tabs.filter(t => 'open' in t && t.open).map(t => t.path))
/** Account pages shown on their own (no header), with or without a session: invitation and password reset. */
export const accountPaths = new Set(['/activar', '/recuperar', '/restablecer'])

export const router = createRouter({
  history: createWebHashHistory(),
  routes: [
    { path: '/', redirect: '/inicio' },
    // The tab was called Posturas.
    { path: '/posturas', redirect: '/clutches' },
    ...tabs.map(({ path, name, component }) => ({ path, name, component })),
    { path: '/usuarios', name: 'users', component: () => import('./views/UsersView.vue') },
    // Notebook photos are digitized in the Asistente tab (T3 Code); old links to the digitizer land there.
    { path: '/cuaderno', redirect: '/asistente' },
    // Cambios propuestos on their own browser tab (a second monitor, a phone), without the app's header:
    // /propuestas/<id> one proposal, /propuestas?chat=<thread> one chat's, /propuestas the chat open in T3.
    // Signed out, the sign-in form shows in its place and the page follows (App.vue).
    { path: '/propuestas/:id?', name: 'proposals', component: () => import('./views/ProposalsView.vue'), meta: { bare: true } },
    // What the assistant is told (brief, skills, subagents, tools) and its history; opened from the Asistente bar.
    { path: '/instrucciones', name: 'instructions', component: () => import('./views/InstructionsView.vue'), meta: { tab: '/asistente' } },
    { path: '/activar', name: 'activate', component: () => import('./views/ActivateView.vue') },
    // Forgotten password: ask for a link, then open it.
    { path: '/recuperar', name: 'forgot', component: () => import('./views/ForgotPasswordView.vue') },
    { path: '/restablecer', name: 'reset', component: () => import('./views/ResetPasswordView.vue') },
    { path: '/entrar', name: 'login', component: LoginView },
    { path: '/:pathMatch(.*)*', redirect: '/inicio' },
  ],
})
