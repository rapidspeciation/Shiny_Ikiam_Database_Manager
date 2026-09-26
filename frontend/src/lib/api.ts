import type { ApiErrorBody } from './types'

export class ApiError extends Error {
  code: string
  status: number
  details: unknown
  constructor(status: number, body: ApiErrorBody) {
    super(body.message)
    this.code = body.code
    this.status = status
    this.details = body.details
  }
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
    throw new ApiError(0, { code: 'OFFLINE', message: 'Sin conexión con el servidor' })
  }
  const text = await response.text()
  const data = text ? JSON.parse(text) : {}
  if (!response.ok) throw new ApiError(response.status, data.error || { code: 'SERVER_ERROR', message: response.statusText })
  return data as T
}

export const requestId = () => crypto.randomUUID()
