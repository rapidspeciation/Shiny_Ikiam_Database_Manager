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

/** Requests are relative to the page, so the app works under any base path. */
export async function api<T>(path: string, options: { method?: string; body?: unknown } = {}): Promise<T> {
  const method = options.method || 'GET'
  const headers: Record<string, string> = {}
  if (options.body !== undefined) headers['content-type'] = 'application/json'
  if (method !== 'GET' && csrf) headers['x-csrf-token'] = csrf
  let response: Response
  try {
    response = await fetch(`api/${path}`, {
      method,
      headers,
      credentials: 'same-origin',
      body: options.body === undefined ? undefined : JSON.stringify(options.body),
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
