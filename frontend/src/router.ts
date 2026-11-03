import { createRouter, createWebHashHistory } from 'vue-router'
import LoginView from './views/LoginView.vue'

import { accountPaths, openPaths, tabs } from './tabs'

export { accountPaths, openPaths, tabs }

export const router = createRouter({
  history: createWebHashHistory(),
  routes: [
    { path: '/', redirect: '/inicio' },
    // The tab was called Posturas.
    { path: '/posturas', redirect: '/clutches' },
    // The tab was called Renombrar fotos.
    { path: '/renombrar', redirect: '/fotos' },
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
    // The offline identifier test (a page of its own: it needs cross-origin isolation and WebAssembly, server/offline-id.mjs).
    {
      path: '/prueba-offline',
      redirect: () => {
        location.replace(new URL('prueba-offline/', location.href.split('#')[0]).href)
        return '/inicio'
      },
    },
    { path: '/:pathMatch(.*)*', redirect: '/inicio' },
  ],
})
