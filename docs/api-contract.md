# Implementation API contract

Native Node 24 ESM server, no third-party runtime dependencies. Frontend native ES modules under web/. Same origin, configurable APP_BASE_PATH=/ithomiini, upstream server may receive path with or without base due reverse proxy. JSON APIs below are relative to the base. GET /health is public; other data requires session. Cookie session + CSRF token for mutations, role checks. Bodies use JSON, errors `{error:{code,message,details?}}`; success objects below. Request IDs for all mutations.

## Shared record shape

`{id, sheet, row, values:{[exactHeader]:typedValue}, formulas:{[exactHeader]:expression}, label, kind, updatedAt, version}`. Stable app ID is distinct from biological mark. Source links available as `sourceUrl`. A module `{id,label,labelEn,group,sheet,fields:[{key,label,type,required?,options?,readonly?,suggestions?}],identityFields,recordCount}`. Schema derives from docs/workbook-schema.json and operation-specific overrides.

## Core endpoints

- POST /api/auth/login `{username,password}` -> `{user,csrf}`; POST /api/auth/logout; GET /api/auth/session -> `{user,csrf,setupRequired?}`; POST /api/auth/setup `{token,username,password,displayName}` one-time admin bootstrap, never public open registration.
- GET /api/bootstrap -> `{user,csrf,modules,stats,options,sync,settings}`. Include role, language, sandbox label, sheet URL.
- GET /api/records?module=&q=&limit=&offset=&filters=<JSON> -> `{records,total,offset,limit}`. Also `sheet=` accepted. GET /api/records/:id -> `{record,related,history}`.
- POST /api/records `{module,values,requestId}` -> `{record,action,status}`. PATCH /api/records/:id `{values,expectedVersion,requestId,reason}` same result. DELETE /api/records/:id performs reviewed withdrawal, not blind source row removal.
- POST /api/actions `{type,recordId?,values,records?,requestId,reason?}` domain actions death, preservation, emergence, collection, observation, pairing, movement, sample, withdrawal etc. Returns `{records,action,status}`. Same validation as record routes.
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

Backend agent owns server/* except assistant.mjs and reports.mjs, tests/backend*.test.mjs. AI agent owns server/assistant.mjs, server/reports.mjs, tests/assistant*.test.mjs. Frontend agent owns web/. Root owns scripts/, deploy/, docs/, package.json and integration/E2E. Coordinate before changing other owners' files. All agents are working concurrently; preserve others' work.
