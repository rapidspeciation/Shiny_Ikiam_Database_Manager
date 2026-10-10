import type { ApiErrorBody } from './types'
import { learnMsg, t } from './i18n'

export class ApiError extends Error {
  code: string
  status: number
  details: unknown
  constructor(status: number, body: ApiErrorBody) {
    super(body.message)
    this.code = body.code
    this.status = status
    this.details = body.details
    // Messages with values come with their descriptor: t(message) then shows them in the interface language.
    learnMsg(body.message, body.messageMsg)
    learnItems((body.details as { items?: unknown } | undefined)?.items)
  }
}

/** Learns the descriptors of refused items (a save's cells): { message, messageMsg }. */
export function learnItems(items: unknown) {
  if (Array.isArray(items)) for (const item of items) learnMsg(item?.message, item?.messageMsg)
}

let csrf: string | null = null
export function setCsrf(token: string | null) {
  csrf = token
}
/** The session's token, for requests not made through api() (a photo's chunks, sent with their progress). */
export const csrfToken = () => csrf

/** This page load: Cambios propuestos does not fetch again a list its own edits changed (server/assistant.mjs). */
export const pageId = `${Date.now().toString(36)}.${Math.random().toString(36).slice(2, 12)}`

/** Requests are relative to the page, so the app works under any base path. */
export async function api<T>(
  path: string,
  options: { method?: string; body?: unknown; signal?: AbortSignal } = {},
): Promise<T> {
  const method = options.method || 'GET'
  const headers: Record<string, string> = { 'x-ithomiini-page': pageId }
  if (options.body !== undefined) headers['content-type'] = 'application/json'
  if (method !== 'GET' && csrf) headers['x-csrf-token'] = csrf
  let response: Response
  try {
    response = await fetch(`api/${path}`, {
      method,
      headers,
      credentials: 'same-origin',
      body: options.body === undefined ? undefined : JSON.stringify(options.body),
      signal: options.signal,
    })
  } catch {
    throw new ApiError(0, { code: 'OFFLINE', message: t('Sin conexión con el servidor') })
  }
  const text = await response.text()
  const data = text ? JSON.parse(text) : {}
  if (!response.ok) throw new ApiError(response.status, data.error || { code: 'SERVER_ERROR', message: response.statusText })
  return data as T
}

export const requestId = () => crypto.randomUUID()

/**
 * A GET asked as the page starts, before the page that shows it is built (main.ts: Inicio's
 * summary, at the same time as the session); that page takes it once with `early(path)`, or
 * asks again when it is gone or older than a minute.
 */
const asked = new Map<string, { at: number; answer: Promise<unknown> }>()
export function askEarly(path: string) {
  const answer = api(path)
  answer.catch(() => {})
  asked.set(path, { at: Date.now(), answer })
}
export function early<T>(path: string): Promise<T> {
  const kept = asked.get(path)
  asked.delete(path)
  return kept && Date.now() - kept.at < 60_000 ? (kept.answer as Promise<T>) : api<T>(path)
}
