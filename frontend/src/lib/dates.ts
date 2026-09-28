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

const WEEKDAYS = ['domingo', 'lunes', 'martes', 'miércoles', 'jueves', 'viernes', 'sábado']
/** "sábado 27-Sep-26 · ayer": the weekday makes a wrong day easy to notice. */
export function dayLabel(iso: string, today = todayIso()): string {
  const serial = serialFromIso(iso)
  if (serial === null) return ''
  const weekday = WEEKDAYS[new Date(EPOCH + serial * DAY).getUTCDay()]
  const ago = (serialFromIso(today) ?? serial) - serial
  const when = ago === 0 ? 'hoy' : ago === 1 ? 'ayer' : ago > 1 ? `hace ${ago} días` : ago === -1 ? 'mañana' : `en ${-ago} días`
  return `${weekday} ${formatSerial(serial)} · ${when}`
}

export function formatSerial(serial: number): string {
  // Never "NaN-undefined-N": a broken value says so.
  if (!Number.isFinite(serial)) return 'fecha no válida'
  const d = new Date(EPOCH + Math.round(serial) * DAY)
  return `${d.getUTCDate()}-${MONTHS[d.getUTCMonth()]}-${String(d.getUTCFullYear()).slice(2)}`
}

/** Today in Ecuador, as an ISO date. */
export function todayIso(): string {
  return new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date())
}

/**
 * Parses what a person types into a date cell: 14-Aug-25, 14/08/2025,
 * 2025-08-14 or a serial number. Returns a serial, or null if unreadable.
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
