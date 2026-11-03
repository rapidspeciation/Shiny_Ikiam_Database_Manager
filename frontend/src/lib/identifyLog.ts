/**
 * «Mis identificaciones»: the last results on this phone (the square the AI looked at, small; the AI's first
 * suggestion; the person's own choice; the side of the Andes). Kept only here (localStorage), never sent.
 */
export interface LogEntry {
  at: string
  thumb: string
  ai: string
  p: number
  pick: string | null
  side: 'East' | 'West' | null
}
const KEY = 'identify:log'
export const LOG_MAX = 30

export function readLog(): LogEntry[] {
  try {
    const v = JSON.parse(localStorage.getItem(KEY) || '[]')
    return Array.isArray(v) ? v : []
  } catch {
    return []
  }
}

/** Adds an entry first (or replaces the one of the same time: a choice changed), keeping the last LOG_MAX. */
export function writeLog(entry: LogEntry, log = readLog()): LogEntry[] {
  const next = [entry, ...log.filter(e => e.at !== entry.at)].slice(0, LOG_MAX)
  try {
    localStorage.setItem(KEY, JSON.stringify(next))
  } catch {
    /* full or private mode: the log stays as it was */
  }
  return next
}

export function clearLog() {
  try {
    localStorage.removeItem(KEY)
  } catch {
    /* nothing kept */
  }
}
