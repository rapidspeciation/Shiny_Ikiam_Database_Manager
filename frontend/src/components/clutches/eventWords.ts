import { eventText, type ClutchEvent, type EventKind, type Stage } from '../../lib/clutches'
import { t } from '../../lib/i18n'

/** The events' words in the person's language: "Larvas: −2 murieron". */
const KIND: Record<EventKind, () => string> = {
  laid: () => t('puestos'),
  hatched: () => t('eclosionaron'),
  pupated: () => t('pupas nuevas'),
  emerged: () => t('emergieron'),
  died: () => t('murieron'),
  disappeared: () => t('desaparecieron'),
  preserved: () => t('se preservaron'),
  not_hatched: () => t('no eclosionaron'),
  correction: () => t('corrección'),
  transfer: () => t('traspaso'),
}
const STAGE: Record<Stage, () => string> = {
  egg: () => t('Huevos'),
  larva: () => t('Larvas'),
  pupa: () => t('Pupas'),
  adult: () => t('Adultos'),
}
export const kindWord = (kind: EventKind) => KIND[kind]?.() ?? kind
export const stageWord = (stage: Stage) => STAGE[stage]?.() ?? stage
/** "Larvas: −2 se preservaron (M0E, N9E) · for life history". */
export const eventLine = (e: Pick<ClutchEvent, 'kind' | 'count' | 'ids' | 'note' | 'stage' | 'term'>) =>
  `${stageWord(e.stage)}: ${eventText(e, x => kindWord(x.kind))}`
