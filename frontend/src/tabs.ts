/**
 * The app's tabs (the header's row), apart from the router so the Identificar page (identificar/, its own document:
 * src/identify/main.ts) renders the same header without making the app's router.
 */
import { appRoot } from './lib/appRoot'

/**
 * The Identificar tab for every signed-in person (on since 3 Nov 2026); false: only the offline identifier's tester
 * (session.isOfflineIdTester). It is also open without an account (`open` below): the models then come from
 * Hugging Face, with no team data (Franz, 3 Nov 2026).
 */
export const IDENTIFY_FOR_EVERYONE = true

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
  { path: '/censo', name: 'census', label: 'Censo', component: () => import('./views/CensusView.vue') },
  { path: '/historial', name: 'history', label: 'Historial', component: () => import('./views/HistoryView.vue') },
  // The offline butterfly identifier: a page of its own (cross-origin isolated for its models, server/offline-id.mjs);
  // this route only goes there. `tester`: shown to the tester only until IDENTIFY_FOR_EVERYONE. `open`: also without an account.
  { path: '/identificar', name: 'identify', label: 'Identificar', open: true, tester: true, component: () => import('./views/IdentifyRedirect.vue') },
  // The wing photos: named after the ID on their envelope on the person's computer, then listed in Photo_links and on the sites.
  { path: '/fotos', name: 'rename', label: 'Fotos', editors: true, component: () => import('./views/RenameView.vue') },
  { path: '/asistente', name: 'assistant', label: 'Asistente', component: () => import('./views/AssistantView.vue') },
  // Not a daily task: last.
  { path: '/revision', name: 'review', label: 'Revisión', editors: true, component: () => import('./views/RevisionView.vue') },
]

/** Pages a visitor without an account can open. */
export const openPaths = new Set(tabs.filter(t => 'open' in t && t.open).map(t => t.path))
/** Account pages shown on their own (no header), with or without a session: invitation and password reset. */
export const accountPaths = new Set(['/activar', '/recuperar', '/restablecer'])


/** The Identificar page's address (beside the app's index.html). */
export const identifyPage = () => new URL('identificar/', appRoot()).href
