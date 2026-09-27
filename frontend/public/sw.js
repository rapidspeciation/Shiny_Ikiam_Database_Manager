// Keeps the app usable without coverage in the field: the app files and the
// last copy of each sheet are served from cache when the network is down.
// Edits are never queued here; they stay in the page's pending changes until saved.
const CACHE = 'ithomiini-v4'
const READS = /\/api\/(auth\/session|bootstrap|table|ids|monitoring\/tracks)(\?|$)/

self.addEventListener('install', event => {
  event.waitUntil(caches.open(CACHE).then(cache => cache.addAll(['./', './index.html'])))
  self.skipWaiting()
})

self.addEventListener('activate', event => {
  event.waitUntil(
    caches
      .keys()
      .then(keys => Promise.all(keys.filter(k => k !== CACHE).map(k => caches.delete(k))))
      .then(() => self.clients.claim()),
  )
})

self.addEventListener('message', event => {
  // Sent on sign-out so another person on this device cannot read cached sheets.
  if (event.data === 'clear') event.waitUntil(caches.delete(CACHE))
})

async function networkFirst(request) {
  const cache = await caches.open(CACHE)
  try {
    const response = await fetch(request)
    if (response.ok) cache.put(request, response.clone())
    return response
  } catch {
    const cached = await cache.match(request)
    if (cached) return cached
    throw new Error('offline')
  }
}

async function cacheFirst(request) {
  const cache = await caches.open(CACHE)
  const cached = await cache.match(request)
  if (cached) return cached
  const response = await fetch(request)
  if (response.ok) cache.put(request, response.clone())
  return response
}

self.addEventListener('fetch', event => {
  const { request } = event
  const url = new URL(request.url)
  if (request.method !== 'GET' || url.origin !== location.origin) return
  if (READS.test(url.pathname + url.search)) event.respondWith(networkFirst(request))
  // Assets, fonts and monitoring photos never change once published.
  else if (/\/assets\/|\/fonts\/|\/api\/monitoring\/photos\//.test(url.pathname)) event.respondWith(cacheFirst(request))
  else if (request.mode === 'navigate') event.respondWith(networkFirst(request))
})
