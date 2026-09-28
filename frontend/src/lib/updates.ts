import { ref } from 'vue'

declare const __BUILD_ID__: string

/**
 * A new version of the app was deployed while this page was open. The page
 * keeps working until it needs a file of the new build (opening a tab it has
 * not opened yet); then, or as soon as the new version is noticed, a banner
 * asks to reload. Unsaved changes and the Colecta list stay on this device.
 */
export const updateAvailable = ref(false)

export async function checkForUpdate() {
  if (__BUILD_ID__ === 'dev' || updateAvailable.value) return
  try {
    const response = await fetch('version.json', { cache: 'no-store' })
    if (!response.ok) return
    const { build } = (await response.json()) as { build?: string }
    if (build && build !== __BUILD_ID__) updateAvailable.value = true
  } catch {
    // Offline: checked again later.
  }
}

/** Checks when the person comes back to the page, and every 5 minutes while it is open. */
export function watchForUpdates() {
  document.addEventListener('visibilitychange', () => document.visibilityState === 'visible' && checkForUpdate())
  window.addEventListener('focus', () => checkForUpdate())
  setInterval(() => document.visibilityState === 'visible' && checkForUpdate(), 5 * 60_000)
}
