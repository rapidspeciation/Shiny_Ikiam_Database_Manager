// Keeps the app usable without coverage in the field: the app files and the
// last copy of each sheet are served from cache when the network is down.
// Edits are never queued here; they stay in the page's pending changes until saved.
const CACHE = 'ithomiini-v6'
// What was shared to the installed app (a GPX file or a Wikiloc link), kept
// until the Importar screen picks it up.
const INBOX = 'ithomiini-share'
// The offline identifier test page (/prueba-offline/): the page, its scripts and runtime, and its models (put there
// by the page itself, in parts). Kept on sign-out and across app versions: nothing personal, 700 MB to download.
const OFFLINE_ID = 'ithomiini-prueba-offline'
const READS = /\/api\/(auth\/session|bootstrap|table|ids|monitoring\/tracks|clutches\/(state|day))(\?|$)/

self.addEventListener('install', event => {
  event.waitUntil(caches.open(CACHE).then(cache => cache.addAll(['./', './index.html'])))
  self.skipWaiting()
})

self.addEventListener('activate', event => {
  event.waitUntil(
    caches
      .keys()
      .then(keys => Promise.all(keys.filter(k => k !== CACHE && k !== INBOX && k !== OFFLINE_ID).map(k => caches.delete(k))))
      .then(() => self.clients.claim()),
  )
})

self.addEventListener('message', event => {
  // Sent on sign-out so another person on this device cannot read cached sheets.
  // The offline identifier keeps its models (700 MB, nothing personal) but not the frozen-stock inventory.
  if (event.data === 'clear')
    event.waitUntil(
      Promise.all([
        caches.delete(CACHE),
        caches.delete(INBOX),
        caches.open(OFFLINE_ID).then(c => c.delete(new URL('prueba-offline/models/frozen-stock.json', self.registration.scope).href)),
      ]),
    )
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
  // The offline test page keeps its own scripts in its cache (the app's is emptied on sign-out).
  const cached = (await cache.match(request)) || (await (await caches.open(OFFLINE_ID)).match(request))
  if (cached) return cached
  const response = await fetch(request)
  // Never keep a page under a script's name (the server used to answer a missing old script with the page).
  if (response.ok && !(response.headers.get('content-type') || '').startsWith('text/html')) cache.put(request, response.clone())
  return response
}

/**
 * Android's share menu posts here (see share_target in manifest.webmanifest):
 * the shared GPX or link is kept and the app opens on Importar recorrido.
 */
async function receiveShare(request) {
  const form = await request.formData()
  const file = form.get('file')
  const shared = {
    at: Date.now(),
    title: String(form.get('title') || ''),
    text: String(form.get('text') || ''),
    url: String(form.get('url') || ''),
    file: file && typeof file !== 'string' && file.size < 20_000_000 ? { name: file.name, text: await file.text() } : null,
  }
  const inbox = await caches.open(INBOX)
  await inbox.put('./shared', new Response(JSON.stringify(shared), { headers: { 'content-type': 'application/json' } }))
  return Response.redirect('./#/monitoreo?vista=importar&compartido=1', 303)
}

/** The offline test page: its runtime (ort/; onnxruntime-web pinned, a new version needs OFFLINE_ID renamed) from the cache first, the rest from the network first. */
async function offlineId(request, url) {
  const cache = await caches.open(OFFLINE_ID)
  if (url.pathname.includes('/prueba-offline/ort/')) {
    const cached = await cache.match(request)
    if (cached) return cached
  }
  try {
    const response = await fetch(request)
    if (response.ok && response.status === 200) cache.put(request, response.clone())
    return response
  } catch {
    const cached = await cache.match(request, { ignoreSearch: request.mode === 'navigate' })
    if (cached) return cached
    throw new Error('offline')
  }
}

self.addEventListener('fetch', event => {
  const { request } = event
  const url = new URL(request.url)
  if (request.method === 'POST' && url.origin === location.origin && url.pathname.endsWith('/share-target')) {
    event.respondWith(receiveShare(request))
    return
  }
  if (request.method !== 'GET' || url.origin !== location.origin) return
  // Its models are read by the page from the cache itself; their download (ranges) goes straight to the server.
  if (url.pathname.includes('/prueba-offline/')) {
    if (!url.pathname.includes('/prueba-offline/models/')) event.respondWith(offlineId(request, url))
    return
  }
  if (READS.test(url.pathname + url.search)) event.respondWith(networkFirst(request))
  // Assets, fonts and monitoring photos never change once published.
  else if (/\/assets\/|\/fonts\/|\/api\/monitoring\/photos\//.test(url.pathname)) event.respondWith(cacheFirst(request))
  else if (request.mode === 'navigate') event.respondWith(networkFirst(request))
})
