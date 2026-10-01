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
 * 140825, 2025-08-14 or a serial number. Returns a serial, or null if unreadable.
 */
export function parseDateInput(text: string): number | null {
  const serial = readDate(text)
  return serial !== null && inDateRange(serial) ? serial : null
}

function readDate(text: string): number | null {
  const s = text.trim()
  if (!s) return null
  if (/^\d{4,5}(\.\d+)?$/.test(s)) return Math.round(Number(s))
  let m = /^(\d{4})-(\d{1,2})-(\d{1,2})$/.exec(s)
  if (m) return fromParts(+m[1], +m[2], +m[3])
  m = /^(\d{1,2})[-/ ]([A-Za-z]{3})[a-z]*[-/ ](\d{2}|\d{4})$/.exec(s)
  if (m) {
    const month = MONTHS.findIndex(x => x.toLowerCase() === m![2].toLowerCase()) + 1 || spanishMonth(m[2])
    return month ? fromParts(fullYear(+m[3]), month, +m[1]) : null
  }
  m = /^(\d{1,2})[/.-](\d{1,2})[/.-](\d{2}|\d{4})$/.exec(s)
  if (m) return fromParts(fullYear(+m[3]), +m[2], +m[1])
  // Digits only, day first (a phone's number pad has no "/"): 280926 or 28092026.
  m = /^(\d{2})(\d{2})(\d{2}|\d{4})$/.exec(s)
  if (m) return fromParts(fullYear(+m[3]), +m[2], +m[1])
  return null
}

function spanishMonth(abbr: string): number {
  return ['ene', 'feb', 'mar', 'abr', 'may', 'jun', 'jul', 'ago', 'sep', 'oct', 'nov', 'dic'].indexOf(abbr.toLowerCase()) + 1
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
