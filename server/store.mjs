import { DatabaseSync } from 'node:sqlite';
import { randomUUID } from 'node:crypto';
import { mkdirSync, chmodSync } from 'node:fs';
import { dirname } from 'node:path';
import { modules, moduleMap, labelFor, validateValues, comparable, nextInsectaryId, makeSourceUrl } from './schema.mjs';
import { GoogleSheets, LocalSheets, rowValues } from './sheets.mjs';

const json = value => JSON.stringify(value);
const parse = value => value ? JSON.parse(value) : null;
const now = () => new Date().toISOString();
const error = (code, message, status = 400, details) => Object.assign(new Error(message), { code, status, details });

export class Store {
  constructor(config = {}, { sheets, seed } = {}) {
    this.config = config;
    const dbPath = config.databasePath || ':memory:';
    if (dbPath !== ':memory:') mkdirSync(dirname(dbPath), { recursive: true });
    this.db = new DatabaseSync(dbPath);
    if(dbPath!==':memory:')chmodSync(dbPath,0o600);
    this.db.exec(`PRAGMA journal_mode=WAL; PRAGMA foreign_keys=ON;
      CREATE TABLE IF NOT EXISTS users(id TEXT PRIMARY KEY, username TEXT UNIQUE NOT NULL, display_name TEXT NOT NULL, role TEXT NOT NULL, salt TEXT NOT NULL, password_hash TEXT NOT NULL, active INTEGER NOT NULL DEFAULT 1, created_at TEXT NOT NULL);
      CREATE TABLE IF NOT EXISTS sessions(id_hash TEXT PRIMARY KEY, user_id TEXT NOT NULL, csrf_hash TEXT NOT NULL, expires_at TEXT NOT NULL, created_at TEXT NOT NULL, FOREIGN KEY(user_id) REFERENCES users(id));
      CREATE TABLE IF NOT EXISTS records(id TEXT PRIMARY KEY, sheet TEXT NOT NULL, row_num INTEGER NOT NULL, values_json TEXT NOT NULL, formulas_json TEXT NOT NULL, identity_json TEXT NOT NULL, label TEXT NOT NULL, version INTEGER NOT NULL, updated_at TEXT NOT NULL, missing INTEGER NOT NULL DEFAULT 0, observed INTEGER NOT NULL DEFAULT 1, UNIQUE(sheet,row_num));
      CREATE INDEX IF NOT EXISTS records_sheet ON records(sheet,missing,row_num);
      CREATE TABLE IF NOT EXISTS actions(id TEXT PRIMARY KEY, request_id TEXT UNIQUE, actor TEXT NOT NULL, source TEXT NOT NULL, created_at TEXT NOT NULL, status TEXT NOT NULL, reason TEXT, reverses TEXT, result_json TEXT);
      CREATE TABLE IF NOT EXISTS changes(id TEXT PRIMARY KEY, action_id TEXT NOT NULL, record_id TEXT NOT NULL, sheet TEXT NOT NULL, row_num INTEGER NOT NULL, field TEXT NOT NULL, before_json TEXT, after_json TEXT, FOREIGN KEY(action_id) REFERENCES actions(id));
      CREATE INDEX IF NOT EXISTS changes_field ON changes(record_id,field);
      CREATE TABLE IF NOT EXISTS events(id TEXT PRIMARY KEY, kind TEXT NOT NULL, record_id TEXT, values_json TEXT NOT NULL, actor TEXT NOT NULL, created_at TEXT NOT NULL, request_id TEXT UNIQUE, source TEXT NOT NULL DEFAULT 'app');
      CREATE TABLE IF NOT EXISTS tasks(id TEXT PRIMARY KEY, title TEXT NOT NULL, description TEXT, due_date TEXT, assignee TEXT, status TEXT NOT NULL, record_id TEXT, created_by TEXT NOT NULL, created_at TEXT NOT NULL, updated_at TEXT NOT NULL);
      CREATE TABLE IF NOT EXISTS attachments(id TEXT PRIMARY KEY, record_id TEXT, name TEXT NOT NULL, mime_type TEXT NOT NULL, data BLOB NOT NULL, created_by TEXT NOT NULL, created_at TEXT NOT NULL);
      CREATE TABLE IF NOT EXISTS import_previews(id TEXT PRIMARY KEY, module TEXT NOT NULL, rows_json TEXT NOT NULL, errors_json TEXT NOT NULL, actor TEXT NOT NULL, created_at TEXT NOT NULL, applied INTEGER NOT NULL DEFAULT 0);
      CREATE TABLE IF NOT EXISTS undo_plans(request_id TEXT PRIMARY KEY, actor TEXT NOT NULL, selection_json TEXT NOT NULL, plan_json TEXT NOT NULL, status TEXT NOT NULL, created_at TEXT NOT NULL);
      CREATE TABLE IF NOT EXISTS settings(key TEXT PRIMARY KEY, value TEXT NOT NULL);
      CREATE TABLE IF NOT EXISTS audit(id TEXT PRIMARY KEY, kind TEXT NOT NULL, detail_json TEXT NOT NULL, created_at TEXT NOT NULL);
    `);
    if(!this.db.prepare('PRAGMA table_info(records)').all().some(c=>c.name==='observed'))this.db.exec('ALTER TABLE records ADD COLUMN observed INTEGER NOT NULL DEFAULT 1');
    this.sheets = sheets || (config.localMode ? new LocalSheets(seed || {}) : new GoogleSheets(config));
    this.localMode = this.sheets instanceof LocalSheets;
    this.queue = Promise.resolve();
    this.syncPromise = null;
    this.writeEpoch = new Map();
    this.syncStatus = { state: this.localMode ? 'offline_seed' : 'not_synced', lastSync: this.getSetting('lastSync'), source: this.localMode ? 'local' : 'google', spreadsheetId: this.sheets.spreadsheetId };
  }
  close() { this.db.close(); }
  getSetting(key) { return this.db.prepare('SELECT value FROM settings WHERE key=?').get(key)?.value || null; }
  setSetting(key,value) { this.db.prepare('INSERT INTO settings(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value').run(key,String(value)); }
  runExclusive(fn) { const result = this.queue.then(fn); this.queue = result.catch(() => {}); return result; }
  bumpWrite(sheet) { this.writeEpoch.set(sheet,(this.writeEpoch.get(sheet)||0)+1); }
  listModules() {
    return modules.map(m => ({ ...m, fields: m.fields.map(({ column, ...f }) => f), recordCount: this.db.prepare('SELECT count(*) n FROM records WHERE sheet=? AND missing=0 AND observed=1').get(m.id).n }));
  }
  hydrate(row) {
    if (!row) return null;
    const values = parse(row.values_json), formulas = parse(row.formulas_json);
    return { id: row.id, sheet: row.sheet, module: row.sheet, row: row.row_num, values, formulas, label: row.label,
      kind: moduleMap.get(row.sheet)?.group || 'record', updatedAt: row.updated_at, version: row.version,
      sourceUrl: makeSourceUrl(row.sheet, row.row_num, this.sheets.spreadsheetId), missing: Boolean(row.missing), observed:Boolean(row.observed) };
  }
  getRecord(id) { return this.hydrate(this.db.prepare('SELECT * FROM records WHERE id=?').get(id)); }
  getRecordBySheetRow(sheet,row) { return this.hydrate(this.db.prepare('SELECT * FROM records WHERE sheet=? AND row_num=?').get(sheet,row)); }
  searchRecords({ module, sheet, q = '', limit = 50, offset = 0, filters = {}, observedOnly = true } = {}) {
    const selected = module || sheet;
    if (selected && !moduleMap.has(selected)) throw error('MODULE_NOT_FOUND','Unknown module',404);
    limit = Math.min(Math.max(Number(limit) || 50, 1), 100000); offset = Math.max(Number(offset) || 0, 0);
    const clauses = ['missing=0']; const args = [];
    if(observedOnly!==false&&observedOnly!=='false')clauses.push('observed=1');
    if (selected) { clauses.push('sheet=?'); args.push(selected); }
    if (q) { clauses.push('(label LIKE ? OR values_json LIKE ?)'); args.push(`%${q}%`, `%${q}%`); }
    for (const [field, value] of Object.entries(filters || {})) {
      if (!selected || !moduleMap.get(selected).fields.some(f => f.key === field)) throw error('INVALID_FILTER','Filter needs a known module and field');
      clauses.push('json_extract(values_json, ?) = ?'); args.push(`$.${field.replaceAll('"','\\"')}`, value);
    }
    const where = clauses.join(' AND ');
    const total = this.db.prepare(`SELECT count(*) n FROM records WHERE ${where}`).get(...args).n;
    const rows = this.db.prepare(`SELECT * FROM records WHERE ${where} ORDER BY updated_at DESC,row_num DESC LIMIT ? OFFSET ?`).all(...args,limit,offset);
    return { records: rows.map(r => this.hydrate(r)), total, offset, limit };
  }
  getStats() {
    const rows = this.db.prepare('SELECT sheet,count(*) count FROM records WHERE missing=0 AND observed=1 GROUP BY sheet').all();
    return { totalRecords: rows.reduce((n,r)=>n+r.count,0), byModule: Object.fromEntries(rows.map(r=>[r.sheet,r.count])), openTasks: this.db.prepare("SELECT count(*) n FROM tasks WHERE status NOT IN ('done','cancelled')").get().n, lastSync: this.syncStatus.lastSync };
  }
  listEvents({ kind, recordId, limit = 200 } = {}) {
    const clauses=[]; const args=[]; if(kind){clauses.push('kind=?');args.push(kind);} if(recordId){clauses.push('record_id=?');args.push(recordId);}
    return this.db.prepare(`SELECT * FROM events ${clauses.length?'WHERE '+clauses.join(' AND '):''} ORDER BY created_at DESC LIMIT ?`).all(...args,Math.min(Number(limit)||200,1000)).map(r=>({id:r.id,kind:r.kind,recordId:r.record_id,values:parse(r.values_json),actor:r.actor,createdAt:r.created_at,source:r.source}));
  }
  listTasks() { return this.db.prepare('SELECT * FROM tasks ORDER BY CASE status WHEN \'done\' THEN 1 ELSE 0 END,due_date,created_at DESC').all().map(r=>this.task(r)); }
  task(r) { return {id:r.id,title:r.title,description:r.description,dueDate:r.due_date,assignee:r.assignee,status:r.status,recordId:r.record_id,createdBy:r.created_by,createdAt:r.created_at,updatedAt:r.updated_at}; }
  getHistory({ recordId, q, source, limit = 50, offset = 0 } = {}) {
    const clauses=[]; const args=[];
    if(recordId){clauses.push('EXISTS(SELECT 1 FROM changes c WHERE c.action_id=a.id AND c.record_id=?)');args.push(recordId);}
    if(q){clauses.push('(a.reason LIKE ? OR a.actor LIKE ?)');args.push(`%${q}%`,`%${q}%`);}
    if(source){clauses.push('a.source=?');args.push(source);}
    const where=clauses.length?'WHERE '+clauses.join(' AND '):'';
    const total=this.db.prepare(`SELECT count(*) n FROM actions a ${where}`).get(...args).n;
    limit=Math.min(Number(limit)||50,500);offset=Math.max(Number(offset)||0,0);
    const actions=this.db.prepare(`SELECT a.* FROM actions a ${where} ORDER BY a.created_at DESC LIMIT ? OFFSET ?`).all(...args,limit,offset).map(r=>this.action(r));
    return {actions,total,limit,offset};
  }
  action(r) {
    const changes=this.db.prepare('SELECT * FROM changes WHERE action_id=? ORDER BY rowid').all(r.id).map(c=>({id:c.id,recordId:c.record_id,sheet:c.sheet,row:c.row_num,field:c.field,before:parse(c.before_json),after:parse(c.after_json)}));
    const editor=this.db.prepare('SELECT display_name FROM users WHERE id=?').get(r.actor);
    return {id:r.id,requestId:r.request_id,actor:r.actor,actorName:editor?.display_name||null,source:r.source,createdAt:r.created_at,status:r.status,reason:r.reason,reverses:r.reverses,changes};
  }
  actionByRequest(requestId) { const row=this.db.prepare('SELECT * FROM actions WHERE request_id=?').get(requestId); return row ? {action:this.action(row),status:row.status,...(parse(row.result_json)||{})} : null; }
  persistRecord(record) {
    this.db.prepare(`INSERT INTO records(id,sheet,row_num,values_json,formulas_json,identity_json,label,version,updated_at,missing,observed) VALUES(?,?,?,?,?,?,?,?,?,?,?)
      ON CONFLICT(id) DO UPDATE SET row_num=excluded.row_num,values_json=excluded.values_json,formulas_json=excluded.formulas_json,identity_json=excluded.identity_json,label=excluded.label,version=excluded.version,updated_at=excluded.updated_at,missing=excluded.missing,observed=excluded.observed`)
      .run(record.id,record.sheet,record.row,json(record.values),json(record.formulas),json(this.identity(record.sheet,record.values)),record.label,record.version,record.updatedAt,record.missing?1:0,this.hasObservation(record.sheet,record.values,record.formulas)?1:0);
  }
  identity(sheet, values) { const mod=moduleMap.get(sheet); return Object.fromEntries(mod.identityFields.filter(k=>values[k]!=null&&values[k]!=='').map(k=>[k,values[k]])); }
  fingerprint(sheet, values) { return json(this.identity(sheet,values)); }
  async sync({ sheets = modules.map(m=>m.id), force = false } = {}) {
    if(this.syncPromise)return this.syncPromise;
    const run=this.performSync({sheets,force});this.syncPromise=run;
    try{return await run;}finally{this.syncPromise=null;}
  }
  async performSync({sheets,force}) {
      const full=sheets.length===modules.length;
      const revision=full&&this.sheets.revision?await this.sheets.revision():null;
      if(full&&!force&&revision&&revision===this.getSetting('sourceRevision')&&this.syncStatus.lastSync) {
        this.syncStatus={...this.syncStatus,state:'ok',checkedAt:now(),unchanged:true};
        return this.syncStatus;
      }
      this.syncStatus={...this.syncStatus,state:'syncing'};
      let added=0,changed=0,moved=0,missing=0,skipped=0;
      try {
        for(const sheet of sheets) {
          const mod=moduleMap.get(sheet); if(!mod) throw error('MODULE_NOT_FOUND','Unknown module',404);
          const epoch=this.writeEpoch.get(sheet)||0;
          const rows=await this.sheets.readSheet(sheet);
          const current=rows.filter(r=>r.row>mod.headerRow).map(r=>({row:r.row,...rowValues(sheet,r)})).filter(r=>Object.values(r.values).some(v=>v!==null&&v!=='' )||Object.keys(r.formulas).length);
          await this.runExclusive(async()=>{
          if(epoch!==(this.writeEpoch.get(sheet)||0)){skipped++;return;}
          this.db.exec('BEGIN IMMEDIATE');
          try {
          const old=this.db.prepare('SELECT * FROM records WHERE sheet=? AND missing=0').all(sheet);
          const byRow=new Map(old.map(r=>[r.row_num,r]));
          const byIdentity=new Map();
          for(const r of old){const key=this.fingerprint(sheet,parse(r.values_json));if(key!=='{}')byIdentity.set(key,[...(byIdentity.get(key)||[]),r]);}
          const seen=new Set();
          let displaced=-1_000_000_000;
          for(const item of current) {
            let found=byRow.get(item.row);
            if(found&&seen.has(found.id))found=null;
            const identity=this.fingerprint(sheet,item.values);
            if(found && this.fingerprint(sheet,parse(found.values_json))!==identity && Object.keys(parse(found.identity_json)).length) {
              const candidates=(byIdentity.get(identity)||[]).filter(o=>!seen.has(o.id));
              found=candidates.length===1?candidates[0]:null;
            }
            if(!found && identity!=='{}') {
              const candidates=(byIdentity.get(identity)||[]).filter(o=>!seen.has(o.id));
              if(candidates.length===1) found=candidates[0];
            }
            const previous=found&&this.hydrate(found);
            const record={id:found?.id||randomUUID(),sheet,row:item.row,values:item.values,formulas:item.formulas,
              label:labelFor(sheet,item.values),version:previous?.version||1,updatedAt:previous?.updatedAt||now(),missing:false};
            if(found && found.row_num!==item.row) moved++;
            if(previous) {
              const diffs=[];
              for(const field of mod.fields) {
                const before=previous.formulas[field.key]?{formula:previous.formulas[field.key]}:previous.values[field.key];
                const after=item.formulas[field.key]?{formula:item.formulas[field.key]}:item.values[field.key];
                if(comparable(before)!==comparable(after)) diffs.push({field:field.key,before,after});
              }
              if(diffs.length) {
                record.version++ ;record.updatedAt=now();changed++;
                this.recordExternalChanges(record,diffs);
              }
            } else added++;
            // A moved row may currently be occupied by a different old record. Shift that old mapping aside first.
            if(found && found.row_num!==item.row) this.db.prepare('UPDATE records SET row_num=? WHERE id=?').run(displaced--,found.id);
            const occupant=this.db.prepare('SELECT id FROM records WHERE sheet=? AND row_num=? AND id<>?').get(sheet,item.row,record.id);
            if(occupant) this.db.prepare('UPDATE records SET row_num=? WHERE id=?').run(displaced--,occupant.id);
            this.persistRecord(record);seen.add(record.id);
          }
          for(const prior of old) if(!seen.has(prior.id)) { this.db.prepare('UPDATE records SET missing=1,row_num=? WHERE id=?').run(displaced--,prior.id); missing++; }
          this.db.exec('COMMIT');
          } catch(e){this.db.exec('ROLLBACK');throw e;}
          });
        }
        this.syncStatus={...this.syncStatus,state:skipped?'stale':this.localMode?'offline_seed':'ok',lastSync:skipped?this.syncStatus.lastSync:now(),checkedAt:now(),added,changed,moved,missing,skipped};
        if(!skipped){this.setSetting('lastSync',this.syncStatus.lastSync);if(full&&revision)this.setSetting('sourceRevision',revision);}
        return this.syncStatus;
      } catch(e) { this.syncStatus={...this.syncStatus,state:'error',error:e.message}; throw e; }
  }
  recordExternalChanges(record,diffs) {
    const id=randomUUID(),created=now();
    this.db.prepare('INSERT INTO actions(id,request_id,actor,source,created_at,status,reason,reverses,result_json) VALUES(?,?,?,?,?,?,?,?,?)').run(id,null,'unknown','sheet_reconciliation',created,'observed','Snapshot comparison; intermediate edits and editor unknown',null,null);
    for(const d of diffs) this.db.prepare('INSERT INTO changes(id,action_id,record_id,sheet,row_num,field,before_json,after_json) VALUES(?,?,?,?,?,?,?,?)').run(randomUUID(),id,record.id,record.sheet,record.row,d.field,json(d.before),json(d.after));
  }
  validateRole(user) { if(!user||!['editor','reviewer','admin'].includes(user.role)) throw error('FORBIDDEN','Editing requires an editor role',403); }
  requireRequestId(requestId) { if(typeof requestId!=='string'||requestId.length<8||requestId.length>160) throw error('REQUEST_ID_REQUIRED','A unique requestId is required'); }
  async createRecord({module,values,requestId,reason},user,source='app') {
    this.validateRole(user);this.requireRequestId(requestId);
    return this.runExclusive(async()=> {
      const prior=this.actionByRequest(requestId);if(prior)return prior;
      const mod=moduleMap.get(module);if(!mod)throw error('MODULE_NOT_FOUND','Unknown module',404);
      const clean=validateValues(module,values);
      if(module==='Insectary_data' && !clean.Insectary_ID) clean.Insectary_ID=this.suggestInsectaryId();
      let placeholder=null;
      for(const key of mod.identityFields.filter(k=>clean[k]!=null)) {
        const candidates=this.db.prepare('SELECT * FROM records WHERE sheet=? AND missing=0 AND observed=0 AND json_extract(values_json,?)=? LIMIT 2').all(module,`$.${JSON.stringify(key)}`,clean[key]);
        if(candidates.length>1)throw error('IDENTITY_CONFLICT','More than one unused row has this identifier',409,{field:key});
        if(candidates.length===1){placeholder=this.hydrate(candidates[0]);break;}
      }
      const highest=this.db.prepare('SELECT max(row_num) n FROM records WHERE sheet=? AND missing=0').get(module).n||mod.headerRow;
      const target=await this.sheets.readRow(module,placeholder?.row||highest+1);
      if(['Collection_data','Insectary_data'].includes(module)) {
        const unique=Object.entries(clean).filter(([key,value])=>value!=null&&(key==='Insectary_ID'&&module==='Insectary_data'||key==='CAM_ID'||/^Tube_[1-5]_id$/.test(key)));
        for(const [key,value] of unique){
          let occupied;
          if(key.startsWith('Tube_')){
            const paths=Array.from({length:5},(_,i)=>`$.Tube_${i+1}_id`);
            occupied=this.db.prepare(`SELECT 1 FROM records WHERE sheet=? AND missing=0 AND observed=1 AND (${paths.map(()=> 'json_extract(values_json,?)=?').join(' OR ')}) LIMIT 1`).get(module,...paths.flatMap(path=>[path,value]));
          } else occupied=this.db.prepare('SELECT 1 FROM records WHERE sheet=? AND missing=0 AND observed=1 AND json_extract(values_json,?)=? LIMIT 1').get(module,`$.${key}`,value);
          if(occupied)throw error('DUPLICATE_ID',`${key} is already recorded`,409,{field:key,value});
        }
      }
      const before=rowValues(module,target);
      if(this.hasObservation(module,before.values,before.formulas))throw error('ROW_OCCUPIED','Target row has observations',409);
      const patch={...clean};
      for(const key of Object.keys(patch)) {
        if(comparable(before.values[key])===comparable(patch[key]))delete patch[key];
        else if(before.formulas[key])throw error('FORMULA_CELL','The target cell contains a formula',409,{field:key});
      }
      if(!Object.keys(patch).length)throw error('INVALID_VALUES','New record needs observation fields');
      const record={id:placeholder?.id||randomUUID(),sheet:module,row:target.row,values:{...before.values,...clean},formulas:before.formulas,label:labelFor(module,{...before.values,...clean}),version:(placeholder?.version||0)+1,updatedAt:now()};
      const changes=Object.entries(patch).map(([field,after])=>({field,before:before.values[field]??null,after}));
      const actionId=this.beginAction({requestId,user,source,reason,record,changes});
      try {
        this.bumpWrite(module);
        await this.sheets.writeCells(module,target.row,patch);
        const check=rowValues(module,await this.sheets.readRow(module,target.row));
        if(!Object.entries(patch).every(([k,v])=>comparable(check.formulas[k]?{formula:check.formulas[k]}:check.values[k])===comparable(v))) {
          this.finishAction(actionId,'uncertain',null);throw error('WRITE_UNCERTAIN','Sheet write could not be verified',503,{actionId});
        }
        record.values=check.values;record.formulas=check.formulas;this.persistRecord(record);
        const result={record,action:this.action(this.db.prepare('SELECT * FROM actions WHERE id=?').get(actionId)),status:'verified'};
        this.finishAction(actionId,'verified',result);result.action.status='verified';return result;
      } catch(e) { if(e.code!=='WRITE_UNCERTAIN') this.finishAction(actionId,'uncertain',null); throw e; }
    });
  }
  hasObservation(module,values,formulas={}) {
    const evidence={
      Collection_data:['SPECIES','Collection_date','Collection_location','Sex','Collector','Death_date','Insectary_ID','FieldMark_ID'],
      Insectary_data:['SPECIES','Intro2Insectary_date','Collection_location','Sex','Death_date','Stock_of_origin','CLUTCH NUMBER'],
      Pheromones_data:['CAM_ID'],Melinaea_crosses:['Female','Male'],Melinaea_eggs:['Mother ID','Father ID','Tube_ID'],
      Crosses_Lys_x_Pol:['female Id','male Id'],Stocks_Matings:['male_ID','female_ID'],
      Insectary_stocks:['CLUTCH NUMBER','SPECIES'],Location_data:['Collection_location']
    }[module]||Object.keys(values);
    return evidence.some(k=>{
      const v=values[k],text=String(v??'').trim().toUpperCase();
      return !formulas[k]&&v!==null&&v!==undefined&&!['','NA','N/A','NONE','NULL','BLANK','NOT_COLLECTED'].includes(text)&&!text.startsWith('#');
    });
  }
  suggestInsectaryId() {
    const recent=this.db.prepare("SELECT json_extract(values_json,'$.Insectary_ID') id FROM records WHERE sheet='Insectary_data' AND missing=0 AND observed=1 ORDER BY row_num DESC LIMIT 1000").all();
    const observed=recent.find(r=>/^[A-ZÑ]\d[A-ZÑ]$/.test(r.id||''));
    let next=nextInsectaryId(observed?.id);
    const exists=this.db.prepare("SELECT 1 FROM records WHERE sheet='Insectary_data' AND missing=0 AND observed=1 AND json_extract(values_json,'$.Insectary_ID')=? LIMIT 1");
    while(exists.get(next)) next=nextInsectaryId(next);
    return next;
  }
  async updateRecord(id,{values,expectedVersion,requestId,reason},user,source='app',reverses=null) {
    this.validateRole(user);this.requireRequestId(requestId);
    return this.runExclusive(async()=>this.updateLocked(id,{values,expectedVersion,requestId,reason},user,source,reverses));
  }
  async updateLocked(id,{values,expectedVersion,requestId,reason},user,source='app',reverses=null) {
    const prior=this.actionByRequest(requestId);if(prior)return prior;
    let record=this.getRecord(id);if(!record||record.missing)throw error('RECORD_NOT_FOUND','Record is unavailable',404);
    if(expectedVersion!==undefined && Number(expectedVersion)!==record.version)throw error('VERSION_CONFLICT','Record changed since it was opened',409,{current:record});
    const clean=validateValues(record.sheet,values,{allowFormula:source==='undo',normalize:source!=='undo'});
    if(!Object.keys(clean).length)throw error('INVALID_VALUES','No fields to update');
    const live=await this.resolveLiveRow(record);
    if(live.row!==record.row) { this.db.prepare('UPDATE records SET row_num=? WHERE id=?').run(live.row,id);record.row=live.row; }
    const before=rowValues(record.sheet,live);
    const changes=[];
    for(const [field,after] of Object.entries(clean)) {
      if(before.formulas[field])throw error('FORMULA_CELL','The target cell contains a formula',409,{field});
      const known=record.formulas[field]?{formula:record.formulas[field]}:record.values[field];
      const actual=before.formulas[field]?{formula:before.formulas[field]}:before.values[field];
      if(comparable(known)!==comparable(actual))throw error('EXTERNAL_CONFLICT','The source cell changed in Google Sheets',409,{field,known,actual});
      if(typeof actual==='string'&&values[field]===actual)continue;
      if(comparable(actual)!==comparable(after))changes.push({field,before:actual,after});
    }
    if(!changes.length)return {record,action:null,status:'unchanged'};
    const patch=Object.fromEntries(changes.map(c=>[c.field,c.after]));
    const actionId=this.beginAction({requestId,user,source,reason,record,changes,reverses});
    try {
      this.bumpWrite(record.sheet);
      await this.sheets.writeCells(record.sheet,record.row,patch);
      const check=rowValues(record.sheet,await this.sheets.readRow(record.sheet,record.row));
      if(!changes.every(c=>comparable(check.formulas[c.field]?{formula:check.formulas[c.field]}:check.values[c.field])===comparable(c.after))) {
        this.finishAction(actionId,'uncertain',null);throw error('WRITE_UNCERTAIN','Sheet write could not be verified',503,{actionId});
      }
      record.values=check.values;record.formulas=check.formulas;record.version++;record.updatedAt=now();record.label=labelFor(record.sheet,record.values);
      this.persistRecord(record);
      const result={record,action:this.action(this.db.prepare('SELECT * FROM actions WHERE id=?').get(actionId)),status:'verified'};
      this.finishAction(actionId,'verified',result);result.action.status='verified';return result;
    } catch(e) {if(e.code!=='WRITE_UNCERTAIN')this.finishAction(actionId,'uncertain',null);throw e;}
  }
  async resolveLiveRow(record) {
    const at=await this.sheets.readRow(record.sheet,record.row);
    const current=rowValues(record.sheet,at),values=current.values;
    const expected=this.identity(record.sheet,record.values);
    if(!Object.keys(expected).length){
      if(comparable(record.values)!==comparable(values)||comparable(record.formulas)!==comparable(current.formulas))throw error('IDENTITY_CONFLICT','Identity-free row changed; review its source location before editing',409);
      return at;
    }
    if(comparable(this.identity(record.sheet,values))===comparable(expected))return at;
    const rows=await this.sheets.readSheet(record.sheet);
    const matches=rows.filter(r=>comparable(this.identity(record.sheet,rowValues(record.sheet,r).values))===comparable(expected));
    if(matches.length!==1)throw error('IDENTITY_CONFLICT','The source row moved, disappeared, or has an ambiguous identity',409,{matches:matches.map(r=>r.row)});
    return matches[0];
  }
  beginAction({requestId,user,source,reason,record,changes,reverses}) {
    const id=randomUUID();
    this.db.exec('BEGIN IMMEDIATE');
    try {
      this.db.prepare('INSERT INTO actions(id,request_id,actor,source,created_at,status,reason,reverses,result_json) VALUES(?,?,?,?,?,?,?,?,?)').run(id,requestId,user.id||user.username,source,now(),'pending',reason||null,reverses||null,null);
      for(const c of changes)this.db.prepare('INSERT INTO changes(id,action_id,record_id,sheet,row_num,field,before_json,after_json) VALUES(?,?,?,?,?,?,?,?)').run(randomUUID(),id,record.id,record.sheet,record.row,c.field,json(c.before),json(c.after));
      this.db.exec('COMMIT');
    } catch(e) { this.db.exec('ROLLBACK');throw e; }
    return id;
  }
  finishAction(id,status,result) {
    this.db.prepare('UPDATE actions SET status=?,result_json=? WHERE id=?').run(status,result?json({record:result.record,status:result.status}):null,id);
    this.db.prepare('INSERT INTO audit(id,kind,detail_json,created_at) VALUES(?,?,?,?)').run(randomUUID(),'action_status',json({actionId:id,status}),now());
  }
  async recoverPending() {
    const rows=this.db.prepare("SELECT * FROM actions WHERE status IN ('pending','uncertain')").all();
    let recovered=0;
    for(const row of rows) {
      const changes=this.action(row).changes;
      if(!changes.length)continue;
      const record=this.getRecord(changes[0].recordId);
      try {
        const sheet=record?.sheet||changes[0].sheet,targetRow=record?.row||changes[0].row;
        const liveRow=record?await this.resolveLiveRow(record):await this.sheets.readRow(sheet,targetRow);
        const live=rowValues(sheet,liveRow);
        if(changes.every(c=>comparable(live.formulas[c.field]?{formula:live.formulas[c.field]}:live.values[c.field])===comparable(c.after))) {
          const restored=record||{id:changes[0].recordId,sheet,row:liveRow.row,version:0};
          const alreadyStored=record&&changes.every(c=>comparable(record.formulas[c.field]?{formula:record.formulas[c.field]}:record.values[c.field])===comparable(c.after));
          restored.values=live.values;restored.formulas=live.formulas;if(!alreadyStored)restored.version++;
          restored.label=labelFor(sheet,live.values);restored.updatedAt=now();this.persistRecord(restored);
          this.finishAction(row.id,'verified',{record:restored,status:'verified'});recovered++;
        }
      } catch { /* Keep uncertain when source cannot be read. */ }
    }
    return {recovered};
  }
  previewUndo({actionIds,changeIds}={}) {
    if(!Array.isArray(actionIds)||!actionIds.length)throw error('INVALID_SELECTION','Select at least one action');
    const selected=[];
    for(const actionId of actionIds) {
      const row=this.db.prepare('SELECT * FROM actions WHERE id=?').get(actionId);
      if(!row||row.status!=='verified')throw error('INVALID_SELECTION','Action is not verified',409);
      selected.push(...this.action(row).changes.filter(c=>!changeIds||changeIds.includes(c.id)).map(c=>({...c,actionId,ordinal:this.db.prepare('SELECT rowid n FROM changes WHERE id=?').get(c.id).n})));
    }
    const grouped=new Map();for(const c of selected){const key=`${c.recordId}\u0000${c.field}`;grouped.set(key,[...(grouped.get(key)||[]),c]);}
    const changes=[],conflicts=[];
    for(const chain of grouped.values()) {
      chain.sort((a,b)=>a.ordinal-b.ordinal);
      const first=chain[0],last=chain.at(-1),record=this.getRecord(first.recordId);
      const lastOrdinal=this.db.prepare(`SELECT max(rowid) n FROM changes WHERE id IN (${chain.map(()=>'?').join(',')})`).get(...chain.map(c=>c.id)).n;
      const later=this.db.prepare(`SELECT c.id FROM changes c JOIN actions a ON a.id=c.action_id WHERE c.record_id=? AND c.field=? AND c.rowid>? AND a.status IN ('verified','observed') AND c.id NOT IN (${chain.map(()=>'?').join(',')}) LIMIT 1`).get(first.recordId,first.field,lastOrdinal,...chain.map(c=>c.id));
      const current=record?.formulas[first.field]?{formula:record.formulas[first.field]}:record?.values[first.field];
      const item={recordId:first.recordId,field:first.field,before:current,after:first.before,selectedChangeIds:chain.map(c=>c.id)};
      if(!record||record.missing||later||comparable(current)!==comparable(last.after)||chain.some((c,i)=>i&&comparable(c.before)!==comparable(chain[i-1].after))) {
        conflicts.push({...item,reason:!record||record.missing?'missing_record':later?'later_field_edit':'value_or_chain_changed'});
      } else changes.push(item);
    }
    return {changes,conflicts,eligible:conflicts.length===0&&changes.length>0};
  }
  async undo({actionIds,changeIds,requestId,reason},user) {
    this.validateRole(user);this.requireRequestId(requestId);
    return this.runExclusive(async()=>{
      const selection={actionIds,changeIds:changeIds||null};
      let planRow=this.db.prepare('SELECT * FROM undo_plans WHERE request_id=?').get(requestId);
      if(planRow){
        if(planRow.actor!==(user.id||user.username)||comparable(parse(planRow.selection_json))!==comparable(selection))throw error('REQUEST_ID_CONFLICT','Undo requestId belongs to another selection',409);
      } else {
        const preview=this.previewUndo({actionIds,changeIds});
        if(!preview.eligible)throw error('UNDO_CONFLICT','Selected changes need review',409,preview);
        const plan=[...Map.groupBy(preview.changes,c=>c.recordId)].map(([recordId,items],index)=>({
          recordId,requestId:`${requestId}:${index}`,fields:items.map(item=>({
            field:item.field,before:item.before,after:item.after,
            ordinal:this.db.prepare('SELECT max(rowid) n FROM changes WHERE record_id=? AND field=?').get(recordId,item.field).n||0
          }))
        }));
        this.db.prepare('INSERT INTO undo_plans(request_id,actor,selection_json,plan_json,status,created_at) VALUES(?,?,?,?,?,?)').run(requestId,user.id||user.username,json(selection),json(plan),'pending',now());
        planRow=this.db.prepare('SELECT * FROM undo_plans WHERE request_id=?').get(requestId);
      }
      const plan=parse(planRow.plan_json),outputs=[];
      for(let i=0;i<plan.length;i++){
        const item=plan[i],prior=this.actionByRequest(item.requestId);
        if(prior?.status==='verified'){
          outputs.push({record:this.getRecord(item.recordId),action:prior.action,status:'verified'});
          continue;
        }
        try{
          if(prior)throw error('WRITE_UNCERTAIN','An earlier undo write needs recovery before retry',409,{actionId:prior.action.id,status:prior.status});
          const record=this.getRecord(item.recordId);
          if(!record||record.missing)throw error('RECORD_NOT_FOUND','Undo target is unavailable',409);
          for(const field of item.fields){
            const current=record.formulas[field.field]?{formula:record.formulas[field.field]}:record.values[field.field];
            if(comparable(current)!==comparable(field.before))throw error('UNDO_CONFLICT','A selected field changed after the undo was planned',409,{recordId:item.recordId,field:field.field,expected:field.before,current});
            const later=this.db.prepare("SELECT c.id FROM changes c JOIN actions a ON a.id=c.action_id WHERE c.record_id=? AND c.field=? AND c.rowid>? AND a.status IN ('verified','observed','pending','uncertain') LIMIT 1").get(item.recordId,field.field,field.ordinal);
            if(later)throw error('UNDO_CONFLICT','A later edit touched a selected field',409,{recordId:item.recordId,field:field.field,changeId:later.id});
          }
          const values=Object.fromEntries(item.fields.map(field=>[field.field,field.after]));
          const result=await this.updateLocked(item.recordId,{values,requestId:item.requestId,reason},user,'undo',actionIds.join(','));
          if(result.status!=='verified')throw error('UNDO_CONFLICT','Undo did not produce a verified change',409,{recordId:item.recordId,status:result.status});
          outputs.push(result);
        }catch(e){
          this.db.prepare('UPDATE undo_plans SET status=? WHERE request_id=?').run('partial',requestId);
          throw error('PARTIAL_UNDO','Undo stopped; review saved reversals and the remaining conflict',409,{applied:outputs.map(x=>({recordId:x.record?.id,actionId:x.action?.id,status:x.status})),failedIndex:i,cause:e.code||'ERROR',conflict:e.details||null});
        }
      }
      this.db.prepare('UPDATE undo_plans SET status=? WHERE request_id=?').run('verified',requestId);
      return {records:outputs.map(x=>x.record),actions:outputs.map(x=>x.action),status:'verified'};
    });
  }
  async applyProposal(changes,{user,requestId,reason}={}) {
    this.validateRole(user);this.requireRequestId(requestId);
    if(!Array.isArray(changes)||!changes.length)throw error('INVALID_PROPOSAL','No proposed changes');
    const outputs=[];
    for(let i=0;i<changes.length;i++){
      const c=changes[i];
      try {outputs.push(await this.updateRecord(c.recordId,{values:c.values,expectedVersion:c.expectedVersion,requestId:`${requestId}:${i}`,reason},user,'ai_approved'));}
      catch(e){if(outputs.length)throw error('PARTIAL_APPLY','Some proposal changes were saved; review the listed actions before retrying',409,{applied:outputs.map(x=>({recordId:x.record?.id,actionId:x.action?.id,status:x.status})),failedIndex:i,cause:e.code||'ERROR'});throw e;}
    }
    return {records:outputs.map(x=>x.record),actions:outputs.map(x=>x.action),status:'verified'};
  }
  getAttachment(id) {
    const r=this.db.prepare('SELECT * FROM attachments WHERE id=?').get(id);
    return r && {id:r.id,recordId:r.record_id,name:r.name,mimeType:r.mime_type,data:r.data,createdBy:r.created_by,createdAt:r.created_at};
  }
}

export function createStore(config,options) { return new Store(config,options); }
