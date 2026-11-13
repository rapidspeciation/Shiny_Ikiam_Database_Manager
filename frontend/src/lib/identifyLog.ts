/**
 * «Mis identificaciones»: the last results on this phone (the square the AI looked at, small; the AI's first
 * suggestion; the person's own choice; the side of the Andes; the sex estimate; the models that read it). Kept only
 * here (localStorage), never sent. The photo and the whole result, to load an item again, are in
 * lib/identifyHistory.ts (`saved`: they are there).
 */
export interface LogEntry {
  at: string
  thumb: string
  ai: string
  p: number
  pick: string | null
  side: 'East' | 'West' | null
  /** The sex estimate's call and its % (in-hand or general line), when there was one. */
  sex?: { call: 'female' | 'male' | 'unsure'; pct: number | null } | null
  /** The identifier and the sex model that read it (lib/identifyHistory.ts Versions). */
  v?: { id: string | null; sex: string | null }
  /** The photo and the result are kept (lib/identifyHistory.ts): the item loads again. */
  saved?: boolean
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

/** Adds an entry first, or replaces the one of the same time where it is (a choice changed, an item loaded again), keeping the last LOG_MAX. */
export function writeLog(entry: LogEntry, log = readLog()): LogEntry[] {
  const i = log.findIndex(e => e.at === entry.at)
  const next = (i < 0 ? [entry, ...log] : log.map(e => (e.at === entry.at ? entry : e))).slice(0, LOG_MAX)
  try {
    localStorage.setItem(KEY, JSON.stringify(next))
  } catch {
    /* full or private mode: the log stays as it was */
  }
  return next
}

/** The whole list as it is now (an item taken out). */
export function writeLogAll(log: LogEntry[]) {
  try {
    localStorage.setItem(KEY, JSON.stringify(log.slice(0, LOG_MAX)))
  } catch {
    /* full or private mode */
  }
}

export function clearLog() {
  try {
    localStorage.removeItem(KEY)
  } catch {
    /* nothing kept */
  }
}
