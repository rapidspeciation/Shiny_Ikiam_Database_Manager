/** GET /api/summary: the home page ("Inicio"). */
import { locale } from './i18n'

export interface Named {
  name: string
  n: number
}
/** An ID the team labels with: the last one used, its date, and the next free one. */
export interface BoardEntry {
  last: string
  date: string | null
  next: string | null
}

/** A clutch whose next stage is due: its eggs hatch, its larvae pupate or its pupae emerge. */
export interface Coming {
  clutch: string
  species: string | null
  event: 'hatch' | 'pupate' | 'emerge'
  /** Eggs, larvae or pupae in it now, if recorded. */
  n: number | null
  since: string
  expected: string
  /** Days from today (negative: already late). */
  inDays: number
  where: string | null
}

export interface Team {
  upcoming: { aheadDays: number; lateDays: number; items: Coming[]; late: Coming[] }
  latestIds: {
    insectaryCam: BoardEntry | null
    collectionCam: BoardEntry | null
    mark: BoardEntry | null
    insectaryId: string | null
    clutch: string | null
  }
  collections: {
    total: number
    preserved: number
    toInsectary: number
    marked: number
    species: number
    genera: number
    places: number
    lastDate: string | null
    last12: number
    months: string[]
    byMonth: Record<'preserved' | 'insectary' | 'marked' | 'released', number[]>
    byYear: { year: string; n: number }[]
    topSpecies: Named[]
    topPlaces: { name: string; n: number; species: number; last: string | null }[]
  }
  monitoring: {
    individuals: number
    days: number
    species: number
    marked: number
    preserved: number
    first: string | null
    last: string | null
    months: string[]
    perMonth: number[]
    daysPerMonth: number[]
    topSpecies: Named[]
  }
  crispr: {
    experiments: number
    eggs: number
    hatched: number
    pupae: number
    adults: number
    mutants: number
    checked: number
    first: string | null
    last: string | null
    stocks: {
      name: string
      experiments: number
      eggs: number
      hatched: number
      pupae: number
      adults: number
      mutants: number
      checked: number
    }[]
  }
  crosses: {
    lines: { name: string; P: number; F1: number; F2: number; Backcross: number; total: number }[]
    individuals: number
    melinaea: {
      couples: number
      mated: number
      clutches: number
      directions: { name: string; couples: number; mated: number; clutches: number }[]
    }
    matings: { total: number; bySpecies: Named[] }
    lysimniaPolymnia: { couples: number; attempts: number; mated: number }
    clutchesByGeneration: Named[]
  }
  insectary: {
    aliveDays: number
    clutchDays: number
    alive: number
    aliveFemale: number
    aliveMale: number
    aliveWild: number
    stale: number
    aliveBySpecies: { name: string; female: number; male: number; other: number; wild: number; reared: number; total: number }[]
    clutches: number
    stages: Record<'egg' | 'larva' | 'pupa', { clutches: number; n: number }>
    clutchesBySpecies: { name: string; clutches: number; egg: number; larva: number; pupa: number }[]
    arrivals30: number
    deaths30: number
    deathCauses30: Named[]
    deathSpecies30: Named[]
    months: string[]
    deathsPerMonth: number[]
    preservedPerMonth: number[]
  }
}

/** Natural history, open to visitors: rates and proportions, never how many butterflies were collected or reared. */
export interface Nature {
  facts: { ithomiini: number; species: number; places: number; since: number | null; elevation: [number, number] | null }
  lifeCycle: { name: string; egg: number; larva: number; pupa: number; total: number }[]
  activity: { hour: number; perHour: number; effortHours: number; share: number }[]
  sessions: number
  weather: Record<'clouds' | 'rain', { code: string; label: string; sessions: number; perHour: number }[]>
  seasons: { month: number; perDay: number | null; species: number | null }[]
  species: {
    name: string
    sites: { name: string; perDay: number }[]
    elevation: [number, number] | null
    female: number | null
  }[]
  sites: { name: string; species: number; ithomiini: number; days: number; elevation: number | null }[]
  deaths: { name: string; percent: number }[]
}

export interface Summary {
  today: string
  nature: Nature
  /** Only for signed-in people. */
  team: Team | null
}

export const MONTHS = ['Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic']
const MONTHS_EN = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
/** Short month names in the interface language. */
export const monthNames = () => (locale.value === 'en' ? MONTHS_EN : MONTHS)
export const monthLabel = (m: string) => `${monthNames()[Number(m.slice(5)) - 1]} ${m.slice(2, 4)}`
export const dateLabel = (d: string | null) =>
  d ? `${Number(d.slice(8))} ${monthNames()[Number(d.slice(5, 7)) - 1]} ${d.slice(0, 4)}` : '—'
export const pct = (a: number, b: number) => (b ? `${Math.round((100 * a) / b)} %` : '—')
export const table = (head: string[], rows: (string | number | null)[][]) => ({
  head,
  rows: rows.map(r => r.map(c => c ?? '—')),
})
