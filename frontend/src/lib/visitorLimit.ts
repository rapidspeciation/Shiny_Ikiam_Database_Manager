/**
 * The signed-out limit (server/public-reads.mjs): one network (a campus, a field station) shares one address, and past
 * its photo lists or photos in 15 minutes the server answers 429. Buscar and Identificar then say so in one line
 * («Muchas visitas desde esta red…») instead of photos quietly missing. A list's 429 comes through api(); a photo's
 * cannot be read from the <img>, so the first few photos of the app that fail are asked once more to learn why.
 */
import { ref } from 'vue'
import { ApiError } from './api'

/** True once the server refused this network for the visitors' limit (for this page load). */
export const visitorLimited = ref(false)

/** A list request's error: the visitors' limit or not. */
export function noteLimit(err: unknown) {
  if (err instanceof ApiError && err.status === 429) visitorLimited.value = true
}

const PROBES = 3
let probes = 0
/** A photo that did not load: one of the app's own (specimen or iNaturalist through the proxy) is asked again, at most PROBES times a page. */
export function probePhoto(src: string) {
  if (visitorLimited.value || probes >= PROBES || !/(^|\/)api\/(photo\/|identify\/ref-photo)/.test(src)) return
  probes++
  fetch(src, { credentials: 'same-origin', cache: 'no-store' })
    .then(r => {
      if (r.status === 429) visitorLimited.value = true
    })
    .catch(() => {})
}

/** For tests. */
export function resetVisitorLimit() {
  visitorLimited.value = false
  probes = 0
}
