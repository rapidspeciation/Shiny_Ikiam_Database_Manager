import http from 'node:http';
import { readFileSync, statSync, createReadStream, existsSync, chmodSync } from 'node:fs';
import { join, resolve, extname, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { randomUUID } from 'node:crypto';
import { backup } from 'node:sqlite';
import { Store } from './store.mjs';
import { SANDBOX_ID, moduleMap, validateValues } from './schema.mjs';
import { setup, login, getSession, checkCsrf, revoke, cookie, publicUser, requireAdmin, createUser, updateUser, csrfForSession } from './auth.mjs';

const here=dirname(fileURLToPath(import.meta.url));
const webRoot=resolve(here,'../web');
const now=()=>new Date().toISOString();
const fail=(code,message,status=400,details)=>Object.assign(new Error(message),{code,status,details});
const json=(res,status,value,headers={})=>{res.writeHead(status,{'content-type':'application/json; charset=utf-8','cache-control':'no-store','x-content-type-options':'nosniff',...headers});res.end(JSON.stringify(value));};
const mime={'.html':'text/html; charset=utf-8','.js':'text/javascript; charset=utf-8','.mjs':'text/javascript; charset=utf-8','.css':'text/css; charset=utf-8','.svg':'image/svg+xml','.png':'image/png','.webp':'image/webp','.jpg':'image/jpeg','.ico':'image/x-icon','.json':'application/json'};

export function configFromEnv(env=process.env) {
  return { host:env.APP_HOST||'127.0.0.1',port:Number(env.APP_PORT||8794),basePath:normalizeBase(env.APP_BASE_PATH||'/ithomiini'),
    databasePath:env.DATABASE_PATH||join(here,'../.local/app.sqlite'),googleCredentialsFile:env.GOOGLE_CREDENTIALS_FILE,
    setupToken:env.SETUP_TOKEN,localMode:env.LOCAL_MODE==='1',seedFile:env.SEED_FILE,
    secureCookies:env.SECURE_COOKIES!=='0',spreadsheetId:SANDBOX_ID,
    syncIntervalMs:Number(env.SYNC_INTERVAL_MS||300000),
    aiApiKey:env.AI_API_KEY||env.OPENAI_API_KEY,aiModel:env.AI_MODEL||env.OPENAI_MODEL,
    aiBaseUrl:env.AI_BASE_URL,geminiApiKey:env.GEMINI_API_KEY,
    knowledgeRoots:env.KNOWLEDGE_DIR?[env.KNOWLEDGE_DIR]:[],
    ai:{baseUrl:env.ITHOMIINI_AI_BASE_URL||env.AI_BASE_URL,model:env.ITHOMIINI_AI_MODEL||env.AI_MODEL,
      apiKeyFile:env.ITHOMIINI_AI_API_KEY_FILE,apiKey:env.ITHOMIINI_AI_API_KEY||env.AI_API_KEY,
      transcriptionModel:env.ITHOMIINI_AI_TRANSCRIPTION_MODEL,transcriptionMode:env.ITHOMIINI_AI_TRANSCRIPTION_MODE,
      visionModel:env.ITHOMIINI_AI_VISION_MODEL} };
}
function normalizeBase(value){const raw='/'+String(value).replace(/^\/+|\/+$/g,'');return raw==='/'?'/':raw;}
function routePath(url,base){const path=new URL(url,'http://localhost').pathname;return base!=='/'&&path.startsWith(base+'/')?path.slice(base.length):path===base?'/':path;}
async function bodyOf(req) {
  const pieces=[];let size=0;
  for await(const chunk of req){size+=chunk.length;if(size>15*1024*1024)throw fail('BODY_TOO_LARGE','Request body is too large',413);pieces.push(chunk);}
  if(!size)return {};
  try{return JSON.parse(Buffer.concat(pieces).toString('utf8'));}catch{throw fail('INVALID_JSON','Request body must be valid JSON');}
}
function requireId(body){if(typeof body.requestId!=='string'||body.requestId.length<8)throw fail('REQUEST_ID_REQUIRED','A unique requestId is required');}
function checkOrigin(req){
  if(req.headers['sec-fetch-site']==='cross-site')throw fail('ORIGIN_FORBIDDEN','Cross-site requests are not allowed',403);
  if(!req.headers.origin)return;
  let origin;try{origin=new URL(req.headers.origin);}catch{throw fail('ORIGIN_FORBIDDEN','Invalid Origin header',403);}
  const host=String(req.headers['x-forwarded-host']||req.headers.host||'').split(',')[0].trim();
  if(origin.host!==host)throw fail('ORIGIN_FORBIDDEN','Cross-origin requests are not allowed',403);
}
function requireEditor(user){if(!['editor','reviewer','admin'].includes(user.role))throw fail('FORBIDDEN','Editor role required',403);}
function cleanTask(body,old={}){
  const title=body.title===undefined?old.title:String(body.title).trim();if(!title||title.length>200)throw fail('INVALID_TASK','Task title is required');
  const status=body.status??old.status??'open';if(!['open','in_progress','blocked','done','cancelled'].includes(status))throw fail('INVALID_TASK','Invalid task status');
  return {title,description:body.description===undefined?old.description??null:String(body.description).slice(0,5000),dueDate:body.dueDate===undefined?old.dueDate??null:body.dueDate,assignee:body.assignee===undefined?old.assignee??null:body.assignee,status,recordId:body.recordId===undefined?old.recordId??null:body.recordId};
}
function csvRows(text) {
  const rows=[];let row=[],cell='',quoted=false;
  for(let i=0;i<text.length;i++){
    const c=text[i];if(quoted){if(c==='"'&&text[i+1]==='"'){cell+='"';i++;}else if(c==='"')quoted=false;else cell+=c;}
    else if(c==='"')quoted=true;else if(c===','){row.push(cell);cell='';}else if(c==='\n'){row.push(cell.replace(/\r$/,''));rows.push(row);row=[];cell='';}else cell+=c;
  }
  if(quoted)throw fail('INVALID_CSV','CSV has an unclosed quote');
  if(cell||row.length){row.push(cell.replace(/\r$/,''));rows.push(row);}
  return rows;
}
function csvEscape(value){if(typeof value==='number'||typeof value==='boolean')return String(value);const raw=value==null?'':String(value);const text=/^[=+\-@\t]/.test(raw)?`'${raw}`:raw;return /[",\r\n]/.test(text)?`"${text.replaceAll('"','""')}"`:text;}

export async function createApp(config={},options={}) {
  config={...configFromEnv({}),...config};config.basePath=normalizeBase(config.basePath||'/ithomiini');
  if(config.spreadsheetId && config.spreadsheetId!==SANDBOX_ID)throw new Error('Only the personal sandbox workbook is permitted');
  let seed=options.seed;
  if(!seed&&config.localMode&&config.seedFile){seed=JSON.parse(readFileSync(config.seedFile,'utf8'));seed=seed.sheets||seed;}
  const store=options.store||new Store(config,{sheets:options.sheets,seed});
  const assistantFile=new URL('./assistant.mjs',import.meta.url);
  let assistant=null;
  if(existsSync(fileURLToPath(assistantFile))) {
    const {createAssistant}=await import(assistantFile.href);
    assistant=createAssistant({store,config});
  }
  const loginAttempts=new Map();
  const server=http.createServer(async(req,res)=>{
    const requestId=randomUUID();res.setHeader('x-request-id',requestId);
    try {
      const url=new URL(req.url,'http://localhost'),path=routePath(req.url,config.basePath),method=req.method;
      if(method==='GET'&&path==='/health')return json(res,200,{status:'ok',sync:store.syncStatus.state});
      if(!path.startsWith('/api/'))return serveStatic(req,res,path,config.basePath);
      if(method!=='GET'&&method!=='HEAD')checkOrigin(req);
      const session=getSession(store,req.headers.cookie);
      if(method==='GET'&&path==='/api/auth/session')return json(res,200,{user:session?.user||null,csrf:session?csrfForSession(store,session):null,setupRequired:!store.db.prepare("SELECT 1 FROM users WHERE role='admin' AND active=1").get()});
      const body=['POST','PATCH','PUT','DELETE'].includes(method)?await bodyOf(req):{};
      if(method==='POST'&&path==='/api/auth/setup') {
        const user=setup(store,body,config);const auth=login(store,{username:user.username,password:body.password});
        return json(res,201,{user:auth.user,csrf:auth.csrf},{'set-cookie':cookie(auth.token,{path:config.basePath,secure:config.secureCookies})});
      }
      if(method==='POST'&&path==='/api/auth/login') {
        const key=req.socket.remoteAddress||'unknown';const entry=loginAttempts.get(key)||{count:0,until:0};
        if(entry.count>=10&&entry.until>Date.now())throw fail('RATE_LIMITED','Too many login attempts',429);
        try {const auth=login(store,body);loginAttempts.delete(key);return json(res,200,{user:auth.user,csrf:auth.csrf},{'set-cookie':cookie(auth.token,{path:config.basePath,secure:config.secureCookies})});}
        catch(e){loginAttempts.set(key,{count:entry.count+1,until:Date.now()+15*60_000});throw e;}
      }
      if(!session)throw fail('AUTH_REQUIRED','Sign in required',401);
      if(method!=='GET'&&method!=='HEAD')checkCsrf(session,req.headers['x-csrf-token']);
      const user=session.user,query=Object.fromEntries(url.searchParams.entries());
      if(method==='POST'&&path==='/api/auth/logout'){revoke(store,session);return json(res,200,{ok:true},{'set-cookie':cookie('',{path:config.basePath,secure:config.secureCookies})});}
      if(method==='GET'&&path==='/api/bootstrap')return json(res,200,{user,csrf:csrfForSession(store,session),modules:store.listModules(),stats:store.getStats(),options:{},sync:store.syncStatus,settings:{language:'es',sandbox:true,sandboxLabel:'Copia personal de pruebas',sheetUrl:`https://docs.google.com/spreadsheets/d/${SANDBOX_ID}/edit`,basePath:config.basePath}});
      if(method==='GET'&&path==='/api/records')return json(res,200,store.searchRecords({...query,filters:query.filters?JSON.parse(query.filters):{}}));
      if(method==='GET'&&/^\/api\/records\/[^/]+$/.test(path)) {
        const record=store.getRecord(decodeURIComponent(path.split('/')[3]));if(!record)throw fail('RECORD_NOT_FOUND','Record not found',404);
        const related=relatedRecords(store,record);
        return json(res,200,{record,related,history:store.getHistory({recordId:record.id,limit:50}).actions});
      }
      if(method==='POST'&&path==='/api/records'){requireEditor(user);return json(res,201,await store.createRecord(body,user));}
      if(method==='PATCH'&&/^\/api\/records\/[^/]+$/.test(path)){requireEditor(user);return json(res,200,await store.updateRecord(decodeURIComponent(path.split('/')[3]),body,user));}
      if(method==='DELETE'&&/^\/api\/records\/[^/]+$/.test(path)){
        requireEditor(user);requireId(body);const record=store.getRecord(decodeURIComponent(path.split('/')[3]));if(!record)throw fail('RECORD_NOT_FOUND','Record not found',404);
        const event=addEvent(store,{kind:'withdrawal',recordId:record.id,values:{reason:body.reason||'Withdrawn after review'},requestId:body.requestId},user);
        return json(res,200,{record,event,status:'recorded_in_app'});
      }
      if(method==='POST'&&path==='/api/actions'){requireEditor(user);requireId(body);return json(res,200,await domainAction(store,body,user));}
      if(method==='GET'&&path==='/api/history')return json(res,200,store.getHistory(query));
      if(method==='POST'&&path==='/api/history/preview')return json(res,200,store.previewUndo(body));
      if(method==='POST'&&path==='/api/history/undo')return json(res,200,await store.undo(body,user));
      if(method==='GET'&&path==='/api/tasks')return json(res,200,{tasks:store.listTasks()});
      if(method==='POST'&&path==='/api/tasks'){
        requireEditor(user);requireId(body);const prior=store.db.prepare('SELECT value FROM settings WHERE key=?').get(`request:${body.requestId}`);if(prior)return json(res,200,JSON.parse(prior.value));
        const task=cleanTask(body),id=randomUUID(),stamp=now();store.db.prepare('INSERT INTO tasks VALUES(?,?,?,?,?,?,?,?,?,?)').run(id,task.title,task.description,task.dueDate,task.assignee,task.status,task.recordId,user.id,stamp,stamp);
        const result={task:store.task(store.db.prepare('SELECT * FROM tasks WHERE id=?').get(id))};store.setSetting(`request:${body.requestId}`,JSON.stringify(result));return json(res,201,result);
      }
      if(method==='PATCH'&&/^\/api\/tasks\/[^/]+$/.test(path)){
        requireEditor(user);requireId(body);const id=path.split('/')[3],old=store.db.prepare('SELECT * FROM tasks WHERE id=?').get(id);if(!old)throw fail('TASK_NOT_FOUND','Task not found',404);
        const task=cleanTask(body,store.task(old));store.db.prepare('UPDATE tasks SET title=?,description=?,due_date=?,assignee=?,status=?,record_id=?,updated_at=? WHERE id=?').run(task.title,task.description,task.dueDate,task.assignee,task.status,task.recordId,now(),id);
        return json(res,200,{task:store.task(store.db.prepare('SELECT * FROM tasks WHERE id=?').get(id))});
      }
      if(method==='GET'&&path==='/api/events')return json(res,200,{events:store.listEvents(query)});
      if(method==='POST'&&path==='/api/events'){requireEditor(user);requireId(body);return json(res,201,{event:addEvent(store,body,user)});}
      if(method==='GET'&&path==='/api/options')return json(res,200,{options:optionsFor(store,query.module,query.field,query.q,query.species)});
      if(method==='GET'&&path==='/api/suggestions')return json(res,200,suggestionsFor(store,query.module));
      if(method==='GET'&&path==='/api/sync')return json(res,200,store.syncStatus);
      if(method==='POST'&&path==='/api/sync'){requireEditor(user);return json(res,200,await store.sync({force:true}));}
      if(method==='POST'&&path==='/api/import/preview'){requireEditor(user);return json(res,200,previewImport(store,body,user));}
      if(method==='POST'&&path==='/api/import/apply'){requireEditor(user);requireId(body);return json(res,200,await applyImport(store,body,user));}
      if(method==='GET'&&path==='/api/export'){
        const mod=moduleMap.get(query.module);if(!mod)throw fail('MODULE_NOT_FOUND','Unknown module',404);
        if(query.format&&query.format!=='csv')throw fail('INVALID_FORMAT','Only CSV is supported');
        const records=store.searchRecords({module:mod.id,limit:500000}).records;
        const fields=mod.fields.map(f=>f.key);
        const csv=[fields.map(csvEscape).join(','),...records.map(r=>fields.map(k=>csvEscape(r.values[k])).join(','))].join('\r\n');
        res.writeHead(200,{'content-type':'text/csv; charset=utf-8','content-disposition':`attachment; filename="${mod.id.replaceAll('/','_')}.csv"`,'cache-control':'no-store'});return res.end(csv);
      }
      if(method==='GET'&&path==='/api/attachments')return json(res,200,{attachments:listAttachments(store,query.recordId)});
      if(method==='POST'&&path==='/api/attachments'){requireEditor(user);requireId(body);return json(res,201,{attachment:addAttachment(store,body,user)});}
      if(method==='GET'&&/^\/api\/attachments\/[^/]+\/content$/.test(path)){
        const attachment=store.getAttachment(path.split('/')[3]);if(!attachment)throw fail('ATTACHMENT_NOT_FOUND','Attachment not found',404);
        res.writeHead(200,{'content-type':attachment.mimeType,'content-disposition':`inline; filename="${attachment.name.replace(/["\r\n]/g,'_')}"`,'x-content-type-options':'nosniff','cache-control':'private, no-store'});return res.end(attachment.data);
      }
      if(path==='/api/admin/users'&&method==='GET'){requireAdmin(user);return json(res,200,{users:store.db.prepare('SELECT * FROM users ORDER BY username').all().map(publicUser)});}
      if(path==='/api/admin/users'&&method==='POST'){requireAdmin(user);requireId(body);return json(res,201,{user:createUser(store,body)});}
      if(/^\/api\/admin\/users\/[^/]+$/.test(path)&&method==='PATCH'){requireAdmin(user);requireId(body);return json(res,200,{user:updateUser(store,path.split('/')[4],body,user)});}
      if(path==='/api/admin/status'&&method==='GET'){requireAdmin(user);return json(res,200,{sync:store.syncStatus,stats:store.getStats(),pending:store.db.prepare("SELECT count(*) n FROM actions WHERE status IN ('pending','uncertain')").get().n,databasePath:config.databasePath,sandbox:true});}
      if(path==='/api/admin/backup'&&method==='POST'){
        requireAdmin(user);requireId(body);if(config.databasePath===':memory:')throw fail('BACKUP_UNAVAILABLE','In-memory database cannot be backed up',409);
        const path=resolve(dirname(config.databasePath),`backup-${new Date().toISOString().replace(/[:.]/g,'-')}.sqlite`);await backup(store.db,path);
        chmodSync(path,0o600);
        return json(res,200,{path,createdAt:now()});
      }
      if(assistant){const answer=await assistant.handle({method,path,body,user,query});if(answer)return json(res,answer.status||200,answer.body,answer.headers);}
      throw fail('NOT_FOUND','Route not found',404);
    } catch(e){
      const status=Number(e.status)||500;
      if(status>=500)console.error(`[${requestId}]`,e.stack||e.message);
      return json(res,status,{error:{code:e.code||'SERVER_ERROR',message:status>=500&&!e.code?'Server error':e.message, ...(e.details?{details:e.details}:{})}});
    }
  });
  const ready=options.skipInitialSync?Promise.resolve(store.syncStatus):new Promise(resolve=>setImmediate(resolve))
    .then(()=>store.sync()).then(()=>store.recoverPending()).catch(e=>{console.error('Initial sync failed:',e.message);return store.syncStatus;});
  const interval=config.syncIntervalMs>0?setInterval(()=>store.sync().catch(e=>console.error('Scheduled sync failed:',e.message)),config.syncIntervalMs):null;
  interval?.unref();
  return {server,store,ready,listen:(port=config.port,host=config.host)=>new Promise(resolve=>server.listen(port,host,()=>resolve(server.address()))),close:async()=>{if(interval)clearInterval(interval);await new Promise(resolve=>server.close(resolve));store.close();}};
}

function serveStatic(req,res,path,base) {
  if(req.method!=='GET'&&req.method!=='HEAD')throw fail('METHOD_NOT_ALLOWED','Method not allowed',405);
  const clean=decodeURIComponent(path).replace(/^\/+/,''),target=resolve(webRoot,clean||'index.html');
  if(!target.startsWith(webRoot+'/')&&target!==webRoot)throw fail('NOT_FOUND','File not found',404);
  const file=existsSync(target)&&statSync(target).isFile()?target:join(webRoot,'index.html');
  if(!existsSync(file))throw fail('NOT_FOUND','Frontend is not built',404);
  res.writeHead(200,{'content-type':mime[extname(file)]||'application/octet-stream','x-content-type-options':'nosniff','content-security-policy':"default-src 'self'; object-src 'none'; base-uri 'self'; frame-ancestors 'none'; img-src 'self' data: blob: https:; connect-src 'self'; font-src 'self'; style-src 'self' 'unsafe-inline'; script-src 'self' 'unsafe-inline'",'cache-control':file.endsWith('index.html')?'no-cache':'public, max-age=3600'});
  if(req.method==='HEAD')return res.end();createReadStream(file).pipe(res);
}
function relatedRecords(store,record) {
  const identityField=/^(CAM_ID(?:_.*)?|Insectary_ID|FieldMark_ID|Tube_(?:[1-5]_)?id|CLUTCH NUMBER|Clutch_No\.|Female|Male|Mother ID|Father ID|female Id|male Id|male_ID|female_ID)$/i;
  const normalize=value=>String(value??'').trim().toUpperCase();
  const ids=[...new Set(Object.entries(record.values).filter(([key,value])=>identityField.test(key)&&value!=null&&normalize(value).length>=2&&!['NA','N/A'].includes(normalize(value))).map(([,value])=>normalize(value)))].slice(0,8);
  const seen=new Set([record.id]),related=[];
  for(const value of ids){for(const hit of store.searchRecords({q:value,limit:150}).records){
    const exact=Object.entries(hit.values).some(([key,candidate])=>identityField.test(key)&&normalize(candidate)===value);
    if(exact&&!seen.has(hit.id)){seen.add(hit.id);related.push(hit);}if(related.length>=30)return related;
  }}
  return related;
}
function addEvent(store,body,user) {
  if(typeof body.kind!=='string'||!body.kind.trim())throw fail('INVALID_EVENT','Event kind required');
  const old=store.db.prepare('SELECT * FROM events WHERE request_id=?').get(body.requestId);if(old)return {id:old.id,kind:old.kind,recordId:old.record_id,values:JSON.parse(old.values_json),actor:old.actor,createdAt:old.created_at,source:'app'};
  if(body.recordId&&!store.getRecord(body.recordId))throw fail('RECORD_NOT_FOUND','Record not found',404);
  const id=randomUUID(),stamp=now();store.db.prepare('INSERT INTO events(id,kind,record_id,values_json,actor,created_at,request_id) VALUES(?,?,?,?,?,?,?)').run(id,body.kind,body.recordId||null,JSON.stringify(body.values||{}),user.id,stamp,body.requestId);
  return {id,kind:body.kind,recordId:body.recordId||null,values:body.values||{},actor:user.id,createdAt:stamp,source:'app'};
}
async function domainAction(store,body,user) {
  const type=String(body.type||'').trim();if(!type)throw fail('INVALID_ACTION','Action type required');
  const outcomes=[];
  if(body.recordId && ['death','preservation'].includes(type)) {
    const source=store.getRecord(body.recordId);if(!source)throw fail('RECORD_NOT_FOUND','Record not found',404);
    if(source.sheet==='Collection_data') {
      const link=source.values.Insectary_ID;
      const matches=link?store.searchRecords({module:'Insectary_data',q:String(link),limit:100}).records.filter(r=>r.values.Insectary_ID===link):[];
      if(matches.length!==1)throw fail('IDENTITY_CONFLICT','Choose the linked insectary record before recording this event',409,{matches:matches.map(r=>({id:r.id,row:r.row,label:r.label}))});
      const target=matches[0],map={Death_date:'Death_date',Death_cause:'Death_cause',Preservation_date:'Preservation_date',Preservation_medium:'Preservation_medium',Preserved_dead_alive:'Preserved_Dead_Alive',Preserved_Dead_Alive:'Preserved_Dead_Alive'};
      const values=Object.fromEntries(Object.entries(body.values||{}).map(([key,value])=>[map[key],value]));
      if(Object.keys(values).some(key=>!key))throw fail('INVALID_FIELD','This field has no sheet-backed insectary destination');
      const result=await store.updateRecord(target.id,{values,expectedVersion:body.expectedVersion,requestId:body.requestId,reason:body.reason||`${type} linked from Collection_data ${source.row}`},user,type);
      return {records:[result.record],action:result.action,status:result.status,sourceRecordId:source.id};
    }
  }
  if(Array.isArray(body.records)) {
    for(let i=0;i<body.records.length;i++){
      const item=body.records[i];
      try{outcomes.push(item.id?await store.updateRecord(item.id,{values:item.values,expectedVersion:item.expectedVersion,requestId:`${body.requestId}:${i}`,reason:body.reason},user,type):await store.createRecord({module:item.module||body.module,values:item.values,requestId:`${body.requestId}:${i}`,reason:body.reason},user,type));}
      catch(e){if(outcomes.length)throw fail('PARTIAL_ACTION','Some records were saved; review the listed actions before retrying',409,{applied:outcomes.map(x=>({recordId:x.record?.id,actionId:x.action?.id,status:x.status})),failedIndex:i,cause:e.code||'ERROR'});throw e;}
    }
  } else if(body.recordId&&body.values&&Object.keys(body.values).every(k=>moduleMap.get(store.getRecord(body.recordId)?.sheet)?.fields.some(f=>f.key===k))) {
    outcomes.push(await store.updateRecord(body.recordId,{values:body.values,expectedVersion:body.expectedVersion,requestId:body.requestId,reason:body.reason},user,type));
  } else if(body.module&&body.values&&Object.keys(body.values).every(k=>moduleMap.get(body.module)?.fields.some(f=>f.key===k))) {
    outcomes.push(await store.createRecord({module:body.module,values:body.values,requestId:body.requestId,reason:body.reason},user,type));
  } else {
    const event=addEvent(store,{kind:type,recordId:body.recordId,values:body.values,requestId:body.requestId},user);
    return {records:[],event,action:null,status:'recorded_in_app'};
  }
  return {records:outcomes.map(o=>o.record),actions:outcomes.map(o=>o.action),action:outcomes[0]?.action,status:outcomes.some(o=>o.status!=='verified')?'partial':'verified'};
}
function optionsFor(store,module,field,q='',species='') {
  const mod=moduleMap.get(module);if(!mod||!mod.fields.some(f=>f.key===field))throw fail('INVALID_FIELD','Known module and field required');
  const rows=store.db.prepare('SELECT values_json FROM records WHERE sheet=? AND missing=0 AND observed=1 ORDER BY row_num DESC').all(module);
  const counts=new Map();
  const add=value=>{if(value!=null&&value!==''&&!['NA','N/A'].includes(String(value).trim().toUpperCase())&&String(value).toLowerCase().includes(String(q).toLowerCase()))counts.set(String(value),(counts.get(String(value))||0)+1);};
  for(const row of rows){const values=JSON.parse(row.values_json);if(field==='Subspecies_Form'&&species&&values.SPECIES!==species)continue;add(values[field]);}
  const listField={Identifier:'Abbr_name',Collector:'Abbr_name',CAM_ID_insectary:'InsectaryWild&Reared_CAMid'}[field];
  if(listField)for(const row of store.db.prepare('SELECT values_json FROM records WHERE sheet=? AND missing=0 ORDER BY row_num').all('Lists'))add(JSON.parse(row.values_json)[listField]);
  if(field==='Collection_location')for(const row of store.db.prepare('SELECT values_json FROM records WHERE sheet=? AND missing=0 ORDER BY row_num').all('Location_data'))add(JSON.parse(row.values_json).Collection_location);
  if(field==='Sex')for(const value of ['male','female','unknown'])add(value);
  let options=[...counts].map(([value,count])=>({value,count}));
  if(field==='CAM_ID_insectary'){
    const numeric=value=>Number(/^CAM(\d+)$/i.exec(String(value))?.[1]??-1);
    const last=Math.max(-1,...rows.map(row=>numeric(JSON.parse(row.values_json)[field])));
    options.sort((a,b)=>(numeric(a.value)>last?0:1)-(numeric(b.value)>last?0:1)||numeric(a.value)-numeric(b.value));
  }
  return options.slice(0,100);
}
function suggestionsFor(store,module) {
  const mod=moduleMap.get(module);if(!mod)throw fail('MODULE_NOT_FOUND','Unknown module',404);
  const recent=store.db.prepare('SELECT values_json FROM records WHERE sheet=? AND missing=0 AND observed=1 ORDER BY row_num DESC LIMIT 500').all(module).map(r=>JSON.parse(r.values_json));
  const defaults={Collection_data:['Country','Side_Andes','Collection_location','Transect_section','Collector','Identifier'],Insectary_data:['Collection_location'],SamplingDay_data:['Location','Collectors_initials','DataLogger']};
  const values={};for(const key of defaults[module]||[]){const latest=recent.find(r=>r[key]!=null&&r[key]!=='');if(latest)values[key]=latest[key];}
  if(module==='Insectary_data')values.Insectary_ID=store.suggestInsectaryId();
  if(['Collection_data','Insectary_data'].includes(module)){
    const camIds=store.db.prepare('SELECT values_json FROM records WHERE sheet=? AND missing=0 AND observed=1 ORDER BY row_num DESC').all(module).map(r=>JSON.parse(r.values_json).CAM_ID);
    const latestCam=camIds.find(id=>/^CAM\d+$/i.test(id||''));
    if(latestCam){const used=new Set(camIds);let n=Number(latestCam.slice(3)),candidate;do{candidate=`CAM${String(++n).padStart(latestCam.length-3,'0')}`;}while(used.has(candidate));values.CAM_ID=candidate;}
  }
  const options=Object.fromEntries(mod.fields.filter(f=>/(?:species|subspecies|location|collector|identifier|CAM_ID_insectary|rainfall|cloud_cover|sex|medium|purpose|stock|cage)/i.test(f.key)).map(f=>[f.key,optionsFor(store,module,f.key)]));
  if(['Collection_data','Insectary_data'].includes(module)){
    const recentTube=recent.flatMap(r=>Object.entries(r).filter(([k,v])=>/^Tube_[1-5]_id$/.test(k)&&/^FS\d+$/i.test(v||'')).map(([,v])=>v))[0];
    if(recentTube){
      const used=new Set();for(const sheet of ['Collection_data','Insectary_data'])for(const row of store.db.prepare('SELECT values_json FROM records WHERE sheet=? AND missing=0 AND observed=1').all(sheet))for(const [k,v] of Object.entries(JSON.parse(row.values_json)))if(/^Tube_[1-5]_id$/.test(k)&&/^FS\d+$/i.test(v||''))used.add(v);
      let n=Number(recentTube.slice(2));const tubeIds=[];while(tubeIds.length<100){const candidate=`FS${String(++n).padStart(recentTube.length-2,'0')}`;if(!used.has(candidate))tubeIds.push(candidate);}
      options.tubeIds=tubeIds;for(const field of mod.fields.filter(f=>/^Tube_[1-5]_id$/.test(f.key)))options[field.key]=tubeIds;
    }
  }
  let target=null;
  const identityKey=mod.identityFields.find(key=>values[key]!=null);
  if(identityKey){
    const matches=store.db.prepare('SELECT * FROM records WHERE sheet=? AND missing=0 AND observed=0 AND json_extract(values_json,?)=? LIMIT 2').all(module,`$.${JSON.stringify(identityKey)}`,values[identityKey]);
    if(matches.length===1)target=store.hydrate(matches[0]);
  }
  const formulas=target?.formulas||{};
  for(const key of Object.keys(formulas))if(key!=='Insectary_ID'&&key!=='CAM_ID')delete values[key];
  return {values,options,readonlyFields:Object.keys(formulas),formulas,targetRow:target?.row||null};
}
function previewImport(store,body,user) {
  const mod=moduleMap.get(body.module);if(!mod)throw fail('MODULE_NOT_FOUND','Unknown module',404);
  if(typeof body.csv!=='string'||body.csv.length>5_000_000)throw fail('INVALID_CSV','CSV must be under 5 MB');
  const data=csvRows(body.csv),headers=data.shift()||[],errors=[],rows=[],seen=new Set();
  if(new Set(headers).size!==headers.length)errors.push({row:1,message:'CSV header contains duplicate fields'});
  for(const [i,cells] of data.entries()){
    if(cells.length!==headers.length){errors.push({row:i+2,message:'Column count differs from header'});continue;}
    const values={};for(let c=0;c<headers.length;c++){
      const field=mod.fields.find(f=>f.key===headers[c]);
      if(!field)errors.push({row:i+2,field:headers[c],message:'Unknown field'});
      else if(cells[c]!==''){
        const n=field.type==='number'?Number(cells[c]):null;
        if(field.type==='number'&&!Number.isFinite(n))errors.push({row:i+2,field:headers[c],message:'Number is invalid'});
        else if(field.type==='date'){
          const text=cells[c].trim();
          if(/^\d{4}-\d{2}-\d{2}$/.test(text)){
            const ms=Date.parse(`${text}T00:00:00Z`);
            if(!Number.isFinite(ms)||new Date(ms).toISOString().slice(0,10)!==text)errors.push({row:i+2,field:headers[c],message:'Date must be a real YYYY-MM-DD date'});
            else values[headers[c]]=(ms-Date.UTC(1899,11,30))/86_400_000;
          } else if(Number.isFinite(Number(text)))values[headers[c]]=Number(text);
          else errors.push({row:i+2,field:headers[c],message:'Date must be YYYY-MM-DD or a Sheets serial'});
        } else values[headers[c]]=field.type==='number'?n:cells[c];
      }
    }
    try{validateValues(body.module,values);}catch(e){errors.push({row:i+2,message:e.message});}
    const key=mod.identityFields.find(k=>values[k]);
    if(key){
      const marker=`${key}:${values[key]}`;
      const duplicate=store.db.prepare('SELECT 1 FROM records WHERE sheet=? AND missing=0 AND json_extract(values_json,?)=? LIMIT 1').get(mod.id,`$.${JSON.stringify(key)}`,values[key]);
      if(duplicate||seen.has(marker))errors.push({row:i+2,field:key,message:'Possible existing identifier; review before import'});
      seen.add(marker);
    }
    if(Object.keys(values).length)rows.push(values);
  }
  const id=randomUUID();store.db.prepare('INSERT INTO import_previews(id,module,rows_json,errors_json,actor,created_at) VALUES(?,?,?,?,?,?)').run(id,body.module,JSON.stringify(rows),JSON.stringify(errors),user.id,now());
  return {previewId:id,rows:rows.slice(0,50),rowCount:rows.length,errors};
}
async function applyImport(store,body,user) {
  const preview=store.db.prepare('SELECT * FROM import_previews WHERE id=?').get(body.previewId);
  if(!preview||preview.actor!==user.id)throw fail('PREVIEW_NOT_FOUND','Import preview not found',404);
  if(preview.applied)throw fail('IMPORT_APPLIED','Import was already applied',409);
  const errors=JSON.parse(preview.errors_json);if(errors.length)throw fail('IMPORT_INVALID','Import has validation errors',409,{errors});
  const rows=JSON.parse(preview.rows_json),results=[];
  for(let i=0;i<rows.length;i++){
    try{results.push(await store.createRecord({module:preview.module,values:rows[i],requestId:`${body.requestId}:${i}`,reason:'CSV import'},user,'import'));}
    catch(e){if(results.length)throw fail('PARTIAL_IMPORT','Some rows were saved; review them before retrying',409,{applied:results.map(x=>({recordId:x.record?.id,actionId:x.action?.id,status:x.status})),failedIndex:i,cause:e.code||'ERROR'});throw e;}
  }
  store.db.prepare('UPDATE import_previews SET applied=1 WHERE id=?').run(preview.id);
  return {created:results.length,records:results.map(r=>r.record),status:'verified'};
}
function listAttachments(store,recordId) {
  const rows=recordId?store.db.prepare('SELECT * FROM attachments WHERE record_id=? ORDER BY created_at DESC').all(recordId):store.db.prepare('SELECT * FROM attachments ORDER BY created_at DESC LIMIT 100').all();
  return rows.map(r=>({id:r.id,recordId:r.record_id,name:r.name,mimeType:r.mime_type,size:r.data.length,createdBy:r.created_by,createdAt:r.created_at}));
}
function addAttachment(store,body,user) {
  if(body.recordId&&!store.getRecord(body.recordId))throw fail('RECORD_NOT_FOUND','Record not found',404);
  const allowed=new Set(['image/jpeg','image/png','image/webp','application/pdf','audio/mpeg','audio/mp4','audio/webm','audio/ogg','text/plain']);
  if(!allowed.has(body.mimeType))throw fail('INVALID_MIME','File type is not supported');
  if(typeof body.dataBase64!=='string'||body.dataBase64.length>14_000_000||!/^[-A-Za-z0-9+/=\s]+$/.test(body.dataBase64))throw fail('INVALID_ATTACHMENT','Invalid attachment data');
  const data=Buffer.from(body.dataBase64,'base64');if(!data.length||data.length>10*1024*1024)throw fail('INVALID_ATTACHMENT','Attachment must be under 10 MB');
  const name=String(body.name||'attachment').replace(/[\\/\0\r\n]/g,'_').slice(0,160),id=randomUUID(),stamp=now();
  store.db.prepare('INSERT INTO attachments VALUES(?,?,?,?,?,?,?)').run(id,body.recordId||null,name,body.mimeType,data,user.id,stamp);
  return {id,recordId:body.recordId||null,name,mimeType:body.mimeType,size:data.length,createdBy:user.id,createdAt:stamp};
}

if(process.argv[1]&&resolve(process.argv[1])===fileURLToPath(import.meta.url)) {
  const app=await createApp(configFromEnv());
  const address=await app.listen();
  console.log(`Ithomiini app listening on ${address.address}:${address.port}${app.store.config.basePath||'/ithomiini'}`);
}
