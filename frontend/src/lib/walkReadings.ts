import { reactive } from 'vue'

/**
 * The AI Identifier's reading of each Wikiloc photo (by photo ID), as Monitoreo → Importar
 * follows it (GET api/chat/walks/<id>/identify) while a walk's proposal is open: the review
 * table's photo pane shows it under the row's photo.
 */
export interface PhotoReading {
  status: 'not_started' | 'queued' | 'reading' | 'done' | 'failed'
  species?: [string, number][]
  subspecies?: [string, number][]
  crop?: string
  error?: string
}
export const photoReadings = reactive(new Map<string, PhotoReading>())

/** «Hyposcada illinissa 92 % · Oleria agarista 3 %»: the first names above 1 %. */
export function readingText(list: [string, number][] | undefined, most = 3): string {
  return (list ?? [])
    .slice(0, most)
    .map(([name, p]) => `${name} ${Math.round(p * 100)} %`)
    .join(' · ')
}
