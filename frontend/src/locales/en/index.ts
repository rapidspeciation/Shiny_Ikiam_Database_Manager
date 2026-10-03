/**
 * English for the interface, keyed by the Spanish text in the code
 * (lib/i18n.ts). One file per area, so they can be edited separately.
 */
import account from './account'
import assistant from './assistant'
import clutches from './clutches'
import common from './common'
import deaths from './deaths'
import entry from './entry'
import history from './history'
import home from './home'
import instructions from './instructions'
import monitoring from './monitoring'
import review from './review'
import search from './search'
import server from './server'
import serverBuilt from './server-built'

export const en: Record<string, string> = Object.assign(
  {},
  common,
  entry,
  deaths,
  clutches,
  monitoring,
  home,
  review,
  history,
  search,
  assistant,
  instructions,
  account,
  server,
  serverBuilt,
)
