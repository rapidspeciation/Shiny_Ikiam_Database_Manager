import { reactive } from 'vue'

/**
 * The team's settings for clutches (server/clutches.mjs), as the server last
 * sent them with the clutches' state (useClutchDay, which Clutches and
 * Emergidos both use). Until then, the team's convention since 5 Oct 2026:
 * preserved larvae (and eggs) stay in their count, which holds the larvae used.
 */
export const clutchSettings = reactive({ subtractPreserved: false })

export function applyClutchSettings(settings: { subtractPreserved?: unknown } | null | undefined) {
  if (typeof settings?.subtractPreserved === 'boolean') clutchSettings.subtractPreserved = settings.subtractPreserved
}
