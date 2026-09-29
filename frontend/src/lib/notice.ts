import { reactive } from 'vue'
import { t } from './i18n'

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

/** Readable message for an API error, in the interface language. */
export function errorText(e: unknown): string {
  const err = e as { code?: string; message?: string; details?: { items?: { message: string }[] } }
  const messages: Record<string, string> = {
    OFFLINE: t('Sin conexión. Los cambios siguen guardados en este dispositivo.'),
    BATCH_CONFLICT: t('Algunos cambios necesitan revisión; no se guardó nada.'),
    WRITE_UNCERTAIN: t('No se pudo confirmar la escritura en Google Sheets. Revisa el Historial en unos minutos.'),
    WRITE_REJECTED: t('Google Sheets rechazó el cambio; no se guardó nada.'),
    RATE_LIMITED: t('Demasiados intentos. Espera 15 minutos.'),
    AUTH_REQUIRED: t('La sesión expiró. Vuelve a iniciar sesión.'),
    FORBIDDEN: t('Tu usuario no tiene permiso de edición.'),
  }
  // Server messages are Spanish; their English is in locales/en/server.ts (unknown ones stay Spanish).
  const first = err.details?.items?.[0]?.message
  const known = messages[err.code || '']
  return known ? `${known}${first ? ` ${t(first)}` : ''}` : err.message ? t(err.message) : t('Error inesperado')
}
