/**
 * English for the interface, keyed by the Spanish text in the code
 * (lib/i18n.ts). One file per area, so they can be edited separately.
 */
import account from './account'
import assistant from './assistant'
import census from './census'
import clutches from './clutches'
import collect from './collect'
import common from './common'
import deaths from './deaths'
import emerged from './emerged'
import entry from './entry'
import google from './google'
import history from './history'
import home from './home'
import instructions from './instructions'
import monitoring from './monitoring'
import photoCuration from './photo-curation'
import photoFiles from './photo-files'
import racks from './racks'
import review from './review'
import search from './search'
import server from './server'
import serverBuilt from './server-built'
import sexCheck from './sex-check'
import speciesPhotos from './species-photos'
import tubes from './tubes'

export const en: Record<string, string> = Object.assign(
  {},
  common,
  entry,
  deaths,
  census,
  emerged,
  google,
  tubes,
  racks,
  clutches,
  collect,
  monitoring,
  photoCuration,
  photoFiles,
  sexCheck,
  speciesPhotos,
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
