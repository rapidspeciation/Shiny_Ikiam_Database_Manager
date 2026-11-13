/**
 * The signed-out limits (server/public-reads.mjs). One network (a campus, a field station) shares one address, and past
 * its photo lists or photos in 15 minutes the server answers 429: Buscar and Identificar then say so in one line
 * («Muchas visitas desde esta red…», hidden once signed in) instead of photos quietly missing. When every visitor's
 * Drive budget is spent for a moment the server answers 503 BUSY with Retry-After: that photo is tried once more after
 * it. A list's answer comes through api(); a photo's cannot be read from the <img>, so a photo of the app's that fails
 * is asked once more (photoFailed), at most PROBES photos per batch (a taxon in Buscar, a candidate compared).
 */
import { ref } from 'vue'
import { ApiError } from './api'

/** True once the server refused this network for the visitors' limit (until signed in or the page reloads). */
export const visitorLimited = ref(false)

/** A list request's error: the visitors' limit or not. */
export function noteLimit(err: unknown) {
  if (err instanceof ApiError && err.status === 429) visitorLimited.value = true
}

export const PROBES = 3
const probes = new Map<string, number>()
const retried = new Set<string>()
const APP_PHOTO = /(^|\/)api\/(photo\/|identify\/ref-photo)/

export type Probe = { kind: 'limited' } | { kind: 'busy'; wait: number } | { kind: 'other' } | { kind: 'skipped' }

/**
 * Why a photo of the app's did not load, asked once more: 'limited' (429, the line shows), 'busy' (503, `wait` ms from
 * Retry-After, 1 to 5 s), 'other'; 'skipped' for another site's photo, once the line shows, or past the batch's PROBES.
 */
export async function probePhoto(src: string, batch = ''): Promise<Probe> {
  if (visitorLimited.value || !APP_PHOTO.test(src)) return { kind: 'skipped' }
  const n = probes.get(batch) ?? 0
  if (n >= PROBES) return { kind: 'skipped' }
  if (probes.size > 100) probes.clear()
  probes.set(batch, n + 1)
  try {
    const r = await fetch(src, { credentials: 'same-origin', cache: 'no-store' })
    if (r.status === 429) {
      visitorLimited.value = true
      return { kind: 'limited' }
    }
    if (r.status === 503) return { kind: 'busy', wait: Math.min(5, Math.max(1, Number(r.headers.get('retry-after')) || 2)) * 1000 }
  } catch {
    /* no answer: nothing to learn */
  }
  return { kind: 'other' }
}

/**
 * A photo that failed: `drop` it now; if the server was busy, `restore` it once after Retry-After (the page shows it
 * again and the browser asks again). A photo is retried once per page load: no loop.
 */
export function photoFailed(src: string, batch: string, { drop, restore }: { drop: (src: string) => void; restore: (src: string) => void }) {
  drop(src)
  void probePhoto(src, batch).then(p => {
    if (p.kind !== 'busy' || retried.has(src)) return
    retried.add(src)
    setTimeout(() => restore(src), p.wait)
  })
}

/** For tests. */
export function resetVisitorLimit() {
  visitorLimited.value = false
  probes.clear()
  retried.clear()
}
