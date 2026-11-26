import { intlLocale, t, tn } from './translate.ts'

// Google Sheets stores dates as serial day numbers counted from 1899-12-30.
// The original Shiny app displayed them as "14-Aug-25"; we keep that format.
const EPOCH = Date.UTC(1899, 11, 30)
const DAY = 86_400_000
const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']

export function serialToIso(serial: number): string {
  return new Date(EPOCH + Math.round(serial) * DAY).toISOString().slice(0, 10)
}

export function isoToSerial(iso: string): number {
  return Math.round((Date.parse(`${iso}T00:00:00Z`) - EPOCH) / DAY)
}

/** Sheets serial days the app accepts as dates: 1 Jan 1990 to 31 Dec 2099 (a year typed 92026 is refused). */
export const FIRST_SERIAL = 32874
export const LAST_SERIAL = 73051
export const inDateRange = (serial: number) => Number.isFinite(serial) && serial >= FIRST_SERIAL && serial < LAST_SERIAL

/**
 * An ISO date from a date box as a serial, or null when it is not a real date
 * between 1990 and 2099 (a date box accepts years such as 92026).
 */
export function serialFromIso(iso: string): number | null {
  const m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(iso.trim())
  if (!m) return null
  const serial = fromParts(+m[1], +m[2], +m[3])
  return serial !== null && inDateRange(serial) ? serial : null
}

/** "sábado 27-Sep-26 · ayer": the weekday makes a wrong day easy to notice. */
export function dayLabel(iso: string, today = todayIso()): string {
  const serial = serialFromIso(iso)
  if (serial === null) return ''
  const weekday = weekdayOf(serialToIso(serial))
  const ago = (serialFromIso(today) ?? serial) - serial
  const when =
    ago === 0
      ? t('hoy')
      : ago === 1
        ? t('ayer')
        : ago > 1
          ? tn(ago, 'hace {n} día', 'hace {n} días')
          : ago === -1
            ? t('mañana')
            : tn(-ago, 'en {n} día', 'en {n} días')
  return `${weekday} ${formatSerial(serial)} · ${when}`
}

export function formatSerial(serial: number): string {
  // Never "NaN-undefined-N": a broken value says so.
  if (!Number.isFinite(serial)) return t('fecha no válida')
  const d = new Date(EPOCH + Math.round(serial) * DAY)
  return `${d.getUTCDate()}-${MONTHS[d.getUTCMonth()]}-${String(d.getUTCFullYear()).slice(2)}`
}

// ---------------------------------------------------------------- dates in sentences, tiles and lists
// One shape everywhere outside the grids: the grid's own (14-Aug-25), with the month in the interface language
// (14-Ago-25 in Spanish). Grid cells keep formatSerial: they show what the sheet shows and what people type back.
const MONTHS_ES = ['Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic']
/** Short month names in the interface language (Spanish on the server). */
export const monthNames = () => (intlLocale().startsWith('en') ? MONTHS : MONTHS_ES)

const dayOf = (year: number, month: number, day: number) => `${day}-${monthNames()[month - 1]}-${String(year).slice(-2)}`
/**
 * A day as the app writes it in sentences, tiles and lists: 7-Nov-26 (7-Ene-26 in Spanish). Takes an ISO day
 * (2026-11-07, or a timestamp's first ten characters) or a Sheets serial; `empty` for no date or one it cannot read.
 */
export function dayText(date: string | number | null | undefined, empty = '—'): string {
  if (date === null || date === undefined || date === '') return empty
  const iso = typeof date === 'number' ? (Number.isFinite(date) ? serialToIso(date) : '') : date
  const m = /^(\d{4})-(\d{2})-(\d{2})/.exec(iso)
  if (!m || +m[2] < 1 || +m[2] > 12 || +m[3] < 1 || +m[3] > 31) return empty
  return dayOf(+m[1], +m[2], +m[3])
}

/**
 * The same text with hyphens a line never breaks at, for a date inside a sentence or a narrow box («…preservada
 * el 22-Sep-26…» was cut after «22-»). parseDateInput reads them as plain hyphens, should one be copied into a cell.
 */
export const unbroken = (text: string) => text.replace(/-/g, '\u2011')

const ECUADOR_CLOCK = new Intl.DateTimeFormat('en-US', {
  timeZone: 'America/Guayaquil',
  year: 'numeric',
  month: 'numeric',
  day: 'numeric',
  hour: '2-digit',
  minute: '2-digit',
  hourCycle: 'h23',
})
/** A moment's day and time in Ecuador, or null when it is not a moment. */
function ecuadorParts(at: string | number | Date | null | undefined) {
  if (at === null || at === undefined || at === '') return null
  const time = at instanceof Date ? at.getTime() : typeof at === 'number' ? at : Date.parse(at)
  if (Number.isNaN(time)) return null
  const p = Object.fromEntries(ECUADOR_CLOCK.formatToParts(time).map(x => [x.type, x.value]))
  return { day: dayOf(+p.year, +p.month, +p.day), time: `${p.hour}:${p.minute}` }
}
/** When something happened, in Ecuador's time: 7-Nov-26 14:05. '' when it is not a moment. */
export function momentText(at: string | number | Date | null | undefined): string {
  const p = ecuadorParts(at)
  return p ? `${p.day} ${p.time}` : ''
}
/** The same, without the day when it is today in Ecuador (a list of what just arrived): 14:05. */
export function recentText(at: string | number | Date | null | undefined, now: number | Date = Date.now()): string {
  const p = ecuadorParts(at)
  if (!p) return ''
  return p.day === ecuadorParts(now)?.day ? p.time : `${p.day} ${p.time}`
}

/** An ISO date as the team types it, day first: 2026-05-26 → 26/05/2026. */
export function dayFirst(iso: string): string {
  const [y, m, d] = iso.split('-')
  return d && m && y ? `${d}/${m}/${y}` : ''
}

/** "martes", for an ISO date: the weekday helps notice a wrong collection date. */
export function weekdayOf(iso: string): string {
  const time = Date.parse(`${iso}T12:00:00Z`)
  return Number.isNaN(time) ? '' : new Intl.DateTimeFormat(intlLocale(), { weekday: 'long', timeZone: 'UTC' }).format(time)
}

/** Today in Ecuador, as an ISO date. */
export function todayIso(): string {
  return new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date())
}

/**
 * Parses what a person types into a date cell: 14-Aug-25, 14/08/2025,
 * 140825, 2025-08-14 or a serial number, and without the year as the notebook
 * writes it: 27/9, 27-9, 27.9, 27-sep, 27 sept, 27sep, 27 de septiembre. A date
 * without its year is the latest one that is not more than two months ahead
 * (27/12 typed in January is last December). Returns a serial, or null.
 */
export function parseDateInput(text: string, today = todayIso()): number | null {
  const serial = readDate(text, today)
  return serial !== null && inDateRange(serial) ? serial : null
}

function readDate(text: string, today: string): number | null {
  // A date copied from a sentence of the app has hyphens that do not break (see `unbroken`).
  const s = text.trim().toLowerCase().replace(/\u2011/g, '-').replace(/\s+de\s+/g, ' ')
  if (!s) return null
  if (/^\d{4,5}(\.\d+)?$/.test(s)) return Math.round(Number(s))
  let m = /^(\d{4})-(\d{1,2})-(\d{1,2})$/.exec(s)
  if (m) return fromParts(+m[1], +m[2], +m[3])
  // A month by name, English or Spanish, short or whole: 27-sep-26, 27 septiembre 2026, 27sep.
  m = /^(\d{1,2})[-/. ]?([a-zñ]{3,10})\.?(?:[-/. ]?(\d{2}|\d{4}))?$/.exec(s)
  if (m) {
    const month = monthByName(m[2])
    if (!month) return null
    return m[3] ? fromParts(fullYear(+m[3]), month, +m[1]) : withoutYear(month, +m[1], today)
  }
  m = /^(\d{1,2})[/.-](\d{1,2})[/.-](\d{2}|\d{4})$/.exec(s)
  if (m) return fromParts(fullYear(+m[3]), +m[2], +m[1])
  // Day and month only, as the notebook writes it: 27/9, 27-9, 27.9, 27 9.
  m = /^(\d{1,2})[/.\- ](\d{1,2})$/.exec(s)
  if (m) return withoutYear(+m[2], +m[1], today)
  // Digits only, day first (a phone's number pad has no "/"): 280926 or 28092026.
  m = /^(\d{2})(\d{2})(\d{2}|\d{4})$/.exec(s)
  if (m) return fromParts(fullYear(+m[3]), +m[2], +m[1])
  return null
}

/** A day and month without the year: this year's, unless that is more than two months ahead (then last year's). */
function withoutYear(month: number, day: number, today: string): number | null {
  const year = Number(today.slice(0, 4))
  const serial = fromParts(year, month, day)
  const now = serialFromIso(today)
  if (serial !== null && now !== null && serial - now > 61) return fromParts(year - 1, month, day)
  return serial
}

/** A month by its English or Spanish name or its first three letters (sep, sept, septiembre, ago, august). */
function monthByName(name: string): number {
  const english = ['january', 'february', 'march', 'april', 'may', 'june', 'july', 'august', 'september', 'october', 'november', 'december']
  const spanish = ['enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio', 'julio', 'agosto', 'septiembre', 'octubre', 'noviembre', 'diciembre']
  const found = [english, spanish, ['', '', '', '', '', '', '', '', 'setiembre']].map(list =>
    list.findIndex(full => full && (full === name || (name.length >= 3 && full.startsWith(name)))),
  )
  const at = found.find(i => i >= 0)
  return at === undefined ? 0 : at + 1
}
function fullYear(y: number) {
  return y < 100 ? 2000 + y : y
}
function fromParts(y: number, m: number, d: number): number | null {
  if (m < 1 || m > 12 || d < 1 || d > 31) return null
  const ms = Date.UTC(y, m - 1, d)
  if (new Date(ms).getUTCDate() !== d) return null
  return Math.round((ms - EPOCH) / DAY)
}

/** One day of a month's calendar: its ISO date, its day number, and whether it belongs to the month shown. */
export interface CalendarDay {
  iso: string
  day: number
  inMonth: boolean
}
/**
 * The six weeks a month's calendar shows, Monday first (as calendars in
 * Ecuador and Europe): the month's days with the end of the month before and
 * the start of the next one around them. `month` is 1–12.
 */
export function calendarDays(year: number, month: number): CalendarDay[] {
  const first = Date.UTC(year, month - 1, 1)
  // getUTCDay: 0 Sunday … 6 Saturday → days back to Monday.
  const back = (new Date(first).getUTCDay() + 6) % 7
  const out: CalendarDay[] = []
  for (let i = 0; i < 42; i++) {
    const d = new Date(first + (i - back) * DAY)
    out.push({ iso: d.toISOString().slice(0, 10), day: d.getUTCDate(), inMonth: d.getUTCMonth() === month - 1 })
  }
  return out
}
/** The month before or after (`step` months away): { year, month } with month 1–12. */
export function shiftMonth(year: number, month: number, step: number): { year: number; month: number } {
  const index = year * 12 + (month - 1) + step
  return { year: Math.floor(index / 12), month: (index % 12) + 1 }
}
