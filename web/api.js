const metaBase = document.querySelector('meta[name="app-base-path"]')?.content || '/ithomiini/';
const base = '/' + metaBase.replace(/^\/+|\/+$/g, '') + '/';
const outboxKey = 'ithomiini.outbox.v2';
const draftKey = 'ithomiini.drafts.v2';
let csrf = '';
let owner = '';

export class ApiError extends Error {
  constructor(message, code='request_failed', status=0, details=null){super(message);this.name='ApiError';this.code=code;this.status=status;this.details=details}
}
export const requestId = () => crypto.randomUUID();
export const apiUrl = path => base + path.replace(/^\/+/, '');
export const setCsrf = value => { csrf = value || ''; };
export const getCsrf = () => csrf;
export const setUserScope = value => {owner=String(value||'')};

export async function request(path,{method='GET',body,signal,queue=false}={}){
  const mutation = !['GET','HEAD'].includes(method);
  const payload = mutation && body && typeof body === 'object' ? {...body} : body;
  if (mutation && payload && !payload.requestId && !path.startsWith('/api/auth/') && !path.includes('/preview')) payload.requestId=requestId();
  if (queue && !navigator.onLine){return queueRequest(path,method,payload)}
  let response;
  try {
    response = await fetch(apiUrl(path),{method,credentials:'same-origin',headers:{...(body?{'Content-Type':'application/json'}:{}),...(mutation&&csrf?{'X-CSRF-Token':csrf}:{})},body:body?JSON.stringify(payload):undefined,signal});
  } catch(error){
    if(queue && error instanceof TypeError) return queueRequest(path,method,payload);
    throw new ApiError(navigator.onLine?'No se pudo conectar con el servidor. Intenta otra vez.':'Sin conexión. El trabajo sin guardar permanece en este dispositivo.','network',0);
  }
  if (response.status === 204) return {};
  const type=response.headers.get('content-type')||'';
  const data=type.includes('json')?await response.json().catch(()=>({})):await response.text();
  if(!response.ok){const detail=data?.error||{};throw new ApiError(detail.message||`La solicitud falló (${response.status}).`,detail.code||'request_failed',response.status,detail.details)}
  if(mutation&&data&&['uncertain','pending','partial','needs_review'].includes(data.status)){
    const message='El servidor aún no ha verificado este cambio. Revisa el historial antes de repetirlo.';
    if(queue)queueReview(path,method,payload,message,data);
    throw new ApiError(message,'NEEDS_REVIEW',409,{response:data,requestId:payload?.requestId});
  }
  return data;
}

function scoped(key){return owner?`${key}:${owner}`:null}
function readLocal(key){const name=scoped(key);if(!name)return {};try{return JSON.parse(localStorage.getItem(name)||'{}')}catch{return {}}}
function writeLocal(key,value){const name=scoped(key);if(!name)throw new ApiError('Inicia sesión antes de guardar un borrador.','auth_required',401);localStorage.setItem(name,JSON.stringify(value));window.dispatchEvent(new CustomEvent('ithomiini:local-change'))}
export function getDraft(key){return readLocal(draftKey)[key]||null}
export function saveDraft(key,value){const drafts=readLocal(draftKey);drafts[key]={value,savedAt:new Date().toISOString()};writeLocal(draftKey,drafts)}
export function clearDraft(key){const drafts=readLocal(draftKey);delete drafts[key];writeLocal(draftKey,drafts)}
export function listDrafts(){return Object.entries(readLocal(draftKey)).map(([key,draft])=>({key,...draft}))}
export function getOutbox(){const rows=readLocal(outboxKey);return Array.isArray(rows)?rows:[]}
export function discardQueued(id){writeLocal(outboxKey,getOutbox().filter(item=>item.id!==id))}
function queueRequest(path,method,body){const item={id:requestId(),path,method,body,createdAt:new Date().toISOString(),status:'pending'};writeLocal(outboxKey,[...getOutbox(),item]);return {queued:true,status:'pending',requestId:body?.requestId||item.id}}
function queueReview(path,method,body,error,details){const existing=getOutbox();if(existing.some(item=>item.body?.requestId===body?.requestId))return;writeLocal(outboxKey,[...existing,{id:requestId(),path,method,body,createdAt:new Date().toISOString(),status:'conflict',error,details}])}
export async function flushOutbox(onProgress=()=>{}){
  if(!navigator.onLine||!owner)return;
  const scope=owner;
  const rows=getOutbox();
  for(const item of rows){
    if(owner!==scope)break;
    if(item.status==='conflict') continue;
    try{
      const result=await request(item.path,{method:item.method,body:item.body});
      if(owner!==scope)break;
      discardQueued(item.id);onProgress({item,result,status:'saved'});
    }catch(error){
      if(owner!==scope)break;
      const current=getOutbox();const index=current.findIndex(row=>row.id===item.id);
      if(index<0)continue;
      current[index]={...current[index],status:[400,409,422].includes(error.status)?'conflict':'pending',error:error.message,details:error.details};writeLocal(outboxKey,current);onProgress({item,error,status:current[index].status});
      if(error.status===401||error.status===403||error.code==='network') break;
    }
  }
}
export async function download(path,filename){
  const response=await fetch(apiUrl(path),{credentials:'same-origin'});
  if(!response.ok)throw new ApiError('No se pudo descargar el archivo.','download_failed',response.status);
  const blob=await response.blob();const link=document.createElement('a');link.href=URL.createObjectURL(blob);link.download=filename;link.click();setTimeout(()=>URL.revokeObjectURL(link.href),1000);
}
export function safeJson(value){try{return JSON.stringify(value)}catch{return ''}}
