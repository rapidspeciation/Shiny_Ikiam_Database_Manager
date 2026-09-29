import type { t, tn } from './lib/i18n'

declare module 'vue' {
  interface ComponentCustomProperties {
    /** The text in the interface language (lib/i18n.ts). */
    $t: typeof t
    $tn: typeof tn
  }
}
export {}
