/**
 * English for the interface, keyed by the Spanish text in the code
 * (lib/i18n.ts). One file per area, so they can be edited separately.
 */
import account from './account'
import assistant from './assistant'
import common from './common'
import deaths from './deaths'
import entry from './entry'
import history from './history'
import home from './home'
import monitoring from './monitoring'
import review from './review'
import server from './server'
import serverBuilt from './server-built'

export const en: Record<string, string> = Object.assign(
  {},
  common,
  entry,
  deaths,
  monitoring,
  home,
  review,
  history,
  assistant,
  account,
  server,
  serverBuilt,
)
