const VERSION='ithomiini-shell-v2';
const SHELL=['./','./index.html','./styles.css','./app.js','./api.js','./forms.js','./icons.js','./barcode.js','./mark.svg','./fonts/fira-book.ttf','./fonts/fira-medium.ttf','./fonts/fira-semibold.ttf','./fonts/noto-serif.ttf'];
self.addEventListener('install',event=>{event.waitUntil(caches.open(VERSION).then(cache=>cache.addAll(SHELL)).then(()=>self.skipWaiting()))});
self.addEventListener('activate',event=>{event.waitUntil(caches.keys().then(keys=>Promise.all(keys.filter(key=>key.startsWith('ithomiini-shell-')&&key!==VERSION).map(key=>caches.delete(key)))).then(()=>self.clients.claim()))});
self.addEventListener('fetch',event=>{
  const request=event.request;
  if(request.method!=='GET')return;
  const url=new URL(request.url);
  if(url.origin!==self.location.origin||url.pathname.includes('/api/'))return;
  const shellUrl=new URL('./index.html',self.registration.scope);
  if(request.mode==='navigate'){
    event.respondWith(fetch(request,{cache:'no-cache'}).then(async response=>{if(response.ok){const cache=await caches.open(VERSION);await cache.put(shellUrl,response.clone())}return response}).catch(async()=>await caches.match(shellUrl)||Response.error()));
    return;
  }
  event.respondWith(fetch(request,{cache:'no-cache'}).then(async response=>{if(response.ok){const cache=await caches.open(VERSION);await cache.put(request,response.clone())}return response}).catch(async()=>await caches.match(request)||Response.error()));
});
