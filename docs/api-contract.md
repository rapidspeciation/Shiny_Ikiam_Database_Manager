# Implementation API contract

Native Node 24 ESM server, no third-party runtime dependencies. The Vue frontend in frontend/ is built into web/ and served by the same process. Same origin, configurable APP_BASE_PATH=/ithomiini, upstream server may receive path with or without base due reverse proxy. JSON APIs below are relative to the base. GET /health is public; other data requires session. Cookie session + CSRF token for mutations, role checks. Bodies use JSON, errors `{error:{code,message,details?}}`; success objects below. Request IDs for all mutations.

## Shared record shape

`{id, sheet, row, values:{[exactHeader]:typedValue}, formulas:{[exactHeader]:expression}, label, kind, updatedAt, version}`. Stable app ID is distinct from biological mark. Source links available as `sourceUrl`. A module `{id,label,labelEn,group,sheet,fields:[{key,label,type,required?,options?,readonly?,suggestions?}],identityFields,recordCount}`. Schema derives from docs/workbook-schema.json and operation-specific overrides.

## Grid endpoints (added 26 September 2026)

- GET /api/table?module= -> `{module,revision,columns,headerProblems,rows:[{id,row,version,observed,v:[values in column order],f:[formula column indexes]}]}`. The whole sheet, including unused pre-filled rows (`observed:false`). Gzip-compressed and cached until the local copy changes; supports `If-None-Match` (304).
- POST /api/records/batch `{requestId,reason?,edits:[{id,values,expected?,expectedVersion?}],creates:[{clientId?,module,values}]}` -> `{status:'verified'|'unchanged',action,actions,records,created:[{clientId,recordId}]}`. The only write path: edits, new rows and undo all use it. Every affected row is read live in one request, checked, written in one atomic `batchUpdate`, and verified in one read, whatever the batch size (max 500 rows). `expected` holds the value the person saw for each edited field; a cell changed by someone else is a conflict, other cells of the same row are not. New rows go into the pre-filled row with the given Insectary_ID, or the next unused rows after the last recorded one. Conflicts reject the whole batch with 409 `BATCH_CONFLICT` and `details.items:[{id|clientId,field?,code,message}]`; codes include EXTERNAL_CONFLICT, FORMULA_CELL, DUPLICATE_ID (Insectary_ID and CAM_ID per sheet; tube IDs across Collection_data and Insectary_data), HEADER_MISMATCH, ROW_MOVED, ROW_CHANGED, NO_FREE_ROW, INVALID_FIELD. A Google rejection (4xx) marks the action `failed` and the same requestId may be retried; an unknown outcome marks it `uncertain`.
- GET /api/monitoring/tracks -> `{tracks:[{id,date,collector,name,createdBy,createdAt,track:[[lat,lon,ele,time]],captures:[{lat,lon,ele,text,seq,species,subspecies,sex,minutes,height,cloud,markId,recapture,section}]}]}`. POST (editor) `{requestId,date:'YYYY-MM-DD',collector?,name?,track,captures}` -> `{track,duplicate}`; the same file uploaded again returns the stored copy (200, `duplicate:true`). DELETE /api/monitoring/tracks/:id removes a track (uploader, reviewer or admin). Tracks are app data; they are never written to the workbook.
- GET /api/ids?kind=insectary|cam|tube[&start=&count=] -> `{suggestions:[{value,label,medium?}]}` or, with `start`, `{sequence:[...]}` of consecutive unused IDs. Insectary IDs are the pre-filled IDs after the last recorded butterfly; CAM and tube suggestions are the next number after each run of consecutive IDs (tubes per prefix and preservation medium), newest runs first, as in the Shiny app.
- POST /api/admin/recover re-checks uncertain writes; POST /api/admin/actions/:id/resolve `{status:'failed'|'verified'}` settles one by hand.
- GET /api/history also accepts `actor`, `sheet`, `status`, `from`, `to`; `q` matches notes, row IDs, fields and values. Each change carries the row `label`; each action carries `reversedBy`.

## Core endpoints

- POST /api/auth/login `{username,password}` -> `{user,csrf}`; POST /api/auth/logout; GET /api/auth/session -> `{user,csrf,setupRequired?}`; POST /api/auth/setup `{token,username,password,displayName}` one-time admin bootstrap, never public open registration.
- GET /api/bootstrap -> `{user,csrf,modules,stats,options,sync,settings}`. Include role, language, sandbox label, sheet URL.
- GET /api/records?module=&q=&limit=&offset=&filters=<JSON> -> `{records,total,offset,limit}`. Also `sheet=` accepted. GET /api/records/:id -> `{record,related,history}`.
- POST /api/records `{module,values,requestId}` -> `{record,action,status}`. PATCH /api/records/:id `{values,expectedVersion,requestId,reason}` same result. DELETE /api/records/:id performs reviewed withdrawal, not blind source row removal.
- POST /api/actions `{type,recordId?,values,records?,requestId,reason?}` for older clients. `type` must be one of death, preservation, collection, emergence, tubes, correction, edit; it is recorded in the reason, never as the history source. Values must be sheet columns (unknown fields are rejected, not stored in the app). Runs through the batch write.
- GET /api/history?recordId=&q=&source=&limit=&offset= -> `{actions,total}`. Actions contain `{id,actor,source,createdAt,status,changes:[{id,recordId,field,before,after}],reverses?,reason}`. POST /api/history/preview `{actionIds,changeIds?}` -> `{changes,conflicts,eligible}`. POST /api/history/undo `{actionIds,changeIds?,requestId,reason}`; redo is undo of reversal with same checks.
- GET /api/tasks -> `{tasks}`; POST /api/tasks; PATCH /api/tasks/:id; fields title,description,dueDate,assignee,status,recordId.
- GET /api/events?kind=&recordId= -> `{events}`; POST /api/events `{kind,recordId?,values,requestId}` for app-owned rounds, movements, extraction details, custody and extra records. These are real persisted records, labelled source app, not pretending to be unsupported Sheets columns.
- GET /api/options?module=&field=&q= -> `{options}`; GET /api/suggestions?module= -> `{values,options}` uses current allocation and latest observed defaults, not max preallocated IDs.
- GET /api/sync -> status; POST /api/sync refresh and reconcile external Sheet differences, attributes unknown when unknown; backend scheduled sync.
- POST /api/import/preview `{module,csv}` -> `{rows,errors,previewId}`; POST /api/import/apply `{previewId,requestId}`. GET /api/export?module=&format=csv downloads permitted rows.
- GET /api/attachments?recordId=; POST /api/attachments `{recordId?,name,mimeType,dataBase64}` -> metadata; GET /api/attachments/:id/content authenticated. Validate MIME/size/path.
- GET /api/admin/users; POST /api/admin/users `{username,password,displayName,role}`; PATCH role/reset via explicit fields. GET /api/admin/status; POST /api/admin/backup. Admin only.

## Assistant/reports plugin

Core exports/injects context `{db,config,store,sheets,user,body,query}`. Agent implements `createAssistant({store,config})` returning `handle({method,path,body,user,query})`; returns `{status,body,headers?}` or null for other routes. Core mounts after auth/CSRF. Store exposes `searchRecords({module,q,limit,offset,filters})`, `getRecord(id)`, `listModules()`, `getHistory(options)`, `getStats()`, `listEvents(options)`, `listTasks()`, `proposeChanges(changes)` if available; plugin can use db via store.db (DatabaseSync) for its own prefixed tables. Root integrates interfaces if needed.

- GET/POST /api/chat/threads; GET/DELETE /api/chat/threads/:id; POST /api/chat/threads/:id/messages `{message,attachmentIds?}` -> `{message,sources,results,proposals}` with actual provider answer, stored thread/history.
- POST /api/chat/proposals/:id/apply through core validated mutation; no direct AI credential/SQL/shell exposure. Provider keys and config are private server files, not API responses.
- GET /api/reports?kind=&module=&field=&groupBy= -> `{title,columns,rows,series,method,sources,generatedAt}`; meaningful counts, stage/cross/sample quality, daily/weekly summary.
- GET /api/knowledge?q= -> `{documents}`; GET /api/knowledge/:id -> permitted document text/source.
- POST /api/ai/transcribe and /api/ai/extract for supported provider audio/image draft parsing; user reviews output. If a provider lacks modality, return explicit supported alternative, no fake success.
