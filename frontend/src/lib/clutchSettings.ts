import { reactive } from 'vue'

/**
 * The team's settings for clutches (server/clutches.mjs), as the server last
 * sent them with the clutches' state (useClutchDay, which Clutches and
 * Emergidos both use). Until then, the convention the saves have always
 * followed: preserved larvae (and eggs) are taken off their count.
 */
export const clutchSettings = reactive({ subtractPreserved: true })

export function applyClutchSettings(settings: { subtractPreserved?: unknown } | null | undefined) {
  if (typeof settings?.subtractPreserved === 'boolean') clutchSettings.subtractPreserved = settings.subtractPreserved
}
