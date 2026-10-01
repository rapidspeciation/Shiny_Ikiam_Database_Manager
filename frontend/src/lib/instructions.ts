/**
 * The AI instructions page (views/InstructionsView.vue): what the assistant
 * is told, as GET api/instructions gives it (server/instructions.mjs).
 */
import { dayFirst } from './dates'

export interface Commit {
  commit: string
  date: string
  author: string
  subject: string
}
export interface McpTool {
  name: string
  description: string
  inputSchema: JsonSchema
}
export interface JsonSchema {
  type?: string
  description?: string
  enum?: unknown[]
  items?: JsonSchema
  properties?: Record<string, JsonSchema>
  required?: string[]
  anyOf?: JsonSchema[]
}
export interface Entry {
  id: string
  group: 'brief' | 'skills' | 'agents' | 'tools'
  kind: 'markdown' | 'code' | 'tools'
  title: string
  skill?: string
  meta?: Record<string, string> | null
  content?: string
  tools?: McpTool[]
  lastChanged: string | null
  history: Commit[] | null
}
export interface InstructionsPage {
  historySource: 'git' | 'release' | null
  historyHead: string | null
  entries: Entry[]
}

/** A commit's time as written in its own zone, day first: 2026-09-30T20:55:45-05:00 → 30/09/2026 20:55. */
export const commitTime = (iso: string) => `${dayFirst(iso.slice(0, 10))} ${iso.slice(11, 16)}`.trim()

/** The page's address for an entry. */
export const entryHref = (id: string) => `#/instrucciones?archivo=${encodeURIComponent(id)}`

/**
 * A relative link in a file (reference/notes.md, ../SKILL.md, clutches.md#counts)
 * → the entry it names, if the page has it.
 */
export function resolveLink(fromId: string, href: string, ids: Set<string>): string | null {
  const path = href.split('#')[0]
  if (!path || /^[a-z]+:/i.test(path)) return null
  const parts = path.startsWith('/') ? [] : fromId.split('/').slice(0, -1)
  for (const part of path.split('/')) {
    if (part === '..') parts.pop()
    else if (part && part !== '.') parts.push(part)
  }
  const id = parts.join('/')
  return ids.has(id) ? id : null
}

export interface Param {
  name: string
  type: string
  required: boolean
  description: string
  depth: number
}

const typeOf = (s: JsonSchema): string => {
  if (s.enum) return s.enum.map(v => JSON.stringify(v)).join(' | ')
  if (s.anyOf) return s.anyOf.map(typeOf).join(' | ')
  if (s.type === 'array') return s.items ? `${typeOf(s.items)}[]` : 'array'
  return s.type ?? 'any'
}

/** A tool's parameters as rows, nested ones under their parent (lines[].values). */
export function toolParams(schema: JsonSchema, prefix = '', depth = 0): Param[] {
  const required = new Set(schema.required ?? [])
  return Object.entries(schema.properties ?? {}).flatMap(([key, s]) => {
    const name = `${prefix}${key}`
    const row = { name, type: typeOf(s), required: required.has(key), description: s.description ?? '', depth }
    const inner = s.type === 'array' && s.items?.properties ? [s.items, `${name}[].`] : s.properties ? [s, `${name}.`] : null
    return [row, ...(inner ? toolParams(inner[0] as JsonSchema, inner[1] as string, depth + 1) : [])]
  })
}
