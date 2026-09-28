import { reactive } from 'vue'

/** A single transient message shown at the bottom of the screen. */
export const notice = reactive({ text: '', kind: 'info' as 'info' | 'error' | 'success', id: 0 })

/** `ms`: how long it stays (shorter where it would cover the inputs someone is about to use). */
export function notify(text: string, kind: 'info' | 'error' | 'success' = 'info', ms?: number) {
  const id = ++notice.id
  Object.assign(notice, { text, kind })
  setTimeout(
    () => {
      if (notice.id === id) notice.text = ''
    },
    ms ?? (kind === 'error' ? 9000 : 4500),
  )
}

/** Readable Spanish message for an API error. */
export function errorText(e: unknown): string {
  const err = e as { code?: string; message?: string; details?: { items?: { message: string }[] } }
  const messages: Record<string, string> = {
    OFFLINE: 'Sin conexión. Los cambios siguen guardados en este dispositivo.',
    BATCH_CONFLICT: 'Algunos cambios necesitan revisión; no se guardó nada.',
    WRITE_UNCERTAIN: 'No se pudo confirmar la escritura en Google Sheets. Revisa el Historial en unos minutos.',
    WRITE_REJECTED: 'Google Sheets rechazó el cambio; no se guardó nada.',
    RATE_LIMITED: 'Demasiados intentos. Espera 15 minutos.',
    AUTH_REQUIRED: 'La sesión expiró. Vuelve a iniciar sesión.',
    FORBIDDEN: 'Tu usuario no tiene permiso de edición.',
  }
  const first = err.details?.items?.[0]?.message
  return messages[err.code || ''] ? `${messages[err.code!]}${first ? ` ${first}` : ''}` : err.message || 'Error inesperado'
}
