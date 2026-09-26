import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets, GoogleSheets } from '../server/sheets.mjs';
import { nextInsectaryId } from '../server/schema.mjs';
import { validateValues } from '../server/schema.mjs';
import { createApp } from '../server/index.mjs';

const user={id:'editor-1',username:'editor',role:'editor'};
const seed={
  Insectary_data:[
    {row:2,values:{Insectary_ID:'A0A',SPECIES:'Melinaea menophilus',Sex:'F','CLUTCH NUMBER':'944'}},
    {row:3,cells:[
      {userEnteredValue:{formulaValue:'="A1A"'},effectiveValue:{stringValue:'A1A'}},
      null,null,null,
      {userEnteredValue:{formulaValue:'="Melinaea"'},effectiveValue:{stringValue:'Melinaea'}}
    ]}
  ],
  Collection_data:[{row:2,values:{CAM_ID:'CAM-1',SPECIES:'Species',Death_date:{formula:'=IF(A2="", "", A2)'}}}]
};
async function fixture(){const sheets=new LocalSheets(seed),store=new Store({localMode:true},{sheets});await store.sync({sheets:['Insectary_data','Collection_data']});return {store,sheets};}

test('ID allocation crosses the historical boundary and uses observed records, not preallocation',async()=>{
  assert.equal(nextInsectaryId('A0N'),'A1N');
  assert.equal(nextInsectaryId('A9N'),'B0N');
  assert.equal(nextInsectaryId('N4D'),'N5D');
  assert.equal(nextInsectaryId('Z9N'),'A0Ñ');
  assert.equal(nextInsectaryId('9ZZ'),'A0A');
  const {store}=await fixture();
  assert.equal(store.suggestInsectaryId(),'A1A');
  const placeholder=store.getRecordBySheetRow('Insectary_data',3);
  assert.equal(placeholder.observed,false);
  const result=await store.createRecord({module:'Insectary_data',values:{Sex:'M','CLUTCH NUMBER':'944',Intro2Insectary_date:'2026-09-25'},requestId:'create-0001'},user);
  assert.equal(result.record.id,placeholder.id);
  assert.equal(result.record.row,3);
  assert.equal(result.record.values.Insectary_ID,'A1A');
  assert.equal(result.record.formulas.Insectary_ID,'="A1A"');
  assert.equal(result.record.formulas.SPECIES,'="Melinaea"');
  assert.equal(result.record.values['CLUTCH NUMBER'],944);
  assert.equal(result.record.values.Intro2Insectary_date,46290);
  assert.equal(store.getStats().byModule.Insectary_data,2);
  assert.equal((await store.createRecord({module:'Insectary_data',values:{Sex:'M'},requestId:'create-0001'},user)).record.id,placeholder.id);
  store.close();
});

test('sheet field types normalize new writes but undo keeps exact prior text',()=>{
  const normalized=validateValues('Insectary_data',{'CLUTCH NUMBER':'944',Death_date:'2026-09-25'});
  assert.equal(normalized['CLUTCH NUMBER'],944);
  assert.equal(normalized.Death_date,46290);
  assert.equal(validateValues('Insectary_data',{Death_date:'2026-09-25'},{allowFormula:true,normalize:false}).Death_date,'2026-09-25');
  assert.throws(()=>validateValues('Insectary_data',{'CLUTCH NUMBER':'not-a-number'}),e=>e.code==='INVALID_NUMBER');
  assert.throws(()=>validateValues('Insectary_data',{Death_date:'2026-02-30'}),e=>e.code==='INVALID_DATE');
});

test('saving an unchanged legacy text number leaves its entered type intact',async()=>{
  const {store,sheets}=await fixture();
  const record=store.getRecordBySheetRow('Insectary_data',2);
  const result=await store.updateRecord(record.id,{values:{'CLUTCH NUMBER':'944'},requestId:'unchanged-0001'},user);
  assert.equal(result.status,'unchanged');
  assert.equal((await sheets.readRow('Insectary_data',2)).cells[2].userEnteredValue.stringValue,'944');
  store.close();
});

test('new-record suggestions expose preallocated formula fields before submission',async()=>{
  const {store}=await fixture();
  const app=await createApp({localMode:true,secureCookies:false,setupToken:'test-setup-secret',syncIntervalMs:0},{store,skipInitialSync:true});
  const address=await app.listen(0,'127.0.0.1');
  try{
    const origin=`http://127.0.0.1:${address.port}`;
    const setup=await fetch(`${origin}/ithomiini/api/auth/setup`,{method:'POST',headers:{'content-type':'application/json'},body:JSON.stringify({token:'test-setup-secret',username:'testadmin',password:'long-test-password',displayName:'Test'})});
    assert.equal(setup.status,201);
    const cookie=setup.headers.get('set-cookie').split(';')[0];
    const response=await fetch(`${origin}/ithomiini/api/suggestions?module=Insectary_data`,{headers:{cookie}});
    assert.equal(response.status,200);
    const suggestion=await response.json();
    assert.equal(suggestion.values.Insectary_ID,'A1A');
    assert.equal(suggestion.targetRow,3);
    assert.ok(suggestion.readonlyFields.includes('SPECIES'));
    assert.equal(suggestion.formulas.SPECIES,'="Melinaea"');
  }finally{await app.close();}
});

test('a formula cell rejects direct edits and a changed source cell rejects stale writes',async()=>{
  const {store,sheets}=await fixture();
  const collection=store.getRecordBySheetRow('Collection_data',2);
  await assert.rejects(store.updateRecord(collection.id,{values:{Death_date:'2026-09-25'},requestId:'formula-0001'},user),e=>e.code==='FORMULA_CELL');
  const insect=store.getRecordBySheetRow('Insectary_data',2);
  await sheets.externalEdit('Insectary_data',2,{Sex:'X'});
  await assert.rejects(store.updateRecord(insect.id,{values:{Sex:'M'},requestId:'external-0001'},user),e=>e.code==='EXTERNAL_CONFLICT');
  store.close();
});

test('selected undo preserves another field and rejects a later same-field edit even after value returns',async()=>{
  const {store}=await fixture();
  const record=store.getRecordBySheetRow('Insectary_data',2);
  const first=await store.updateRecord(record.id,{values:{SPECIES:'A'},expectedVersion:1,requestId:'update-0001'},user);
  await store.updateRecord(record.id,{values:{Sex:'M'},requestId:'update-0002'},user);
  const preview=store.previewUndo({actionIds:[first.action.id]});
  assert.equal(preview.eligible,true);
  const undo=await store.undo({actionIds:[first.action.id],requestId:'undo-0001'},user);
  assert.equal((await store.undo({actionIds:[first.action.id],requestId:'undo-0001'},user)).actions[0].id,undo.actions[0].id);
  assert.equal(store.getRecord(record.id).values.Sex,'M');
  const second=await store.updateRecord(record.id,{values:{SPECIES:'B'},requestId:'update-0003'},user);
  await store.updateRecord(record.id,{values:{SPECIES:'A'},requestId:'update-0004'},user);
  assert.equal(store.previewUndo({actionIds:[second.action.id]}).eligible,false);
  store.close();
});

test('Google write request touches only named cells and formats a numeric date',async()=>{
  const google=Object.create(GoogleSheets.prototype),calls=[];
  google.gridRows=new Map([['Insectary_data',10]]);google.metadataAt=Date.now();
  google.request=async(path,options)=>{calls.push({path,body:JSON.parse(options.body)});return {replies:[{},{}]};};
  await google.writeCells('Insectary_data',11,{Death_date:46290,Sex:'F'});
  const requests=calls[0].body.requests;
  assert.equal(requests[0].appendDimension.length,1);
  assert.equal(requests.length,3);
  assert.deepEqual(requests[1].updateCells.rows[0].values[0].userEnteredValue,{numberValue:46290});
  assert.equal(requests[1].updateCells.fields,'userEnteredValue,userEnteredFormat.numberFormat');
  assert.equal(requests[2].updateCells.fields,'userEnteredValue');
  assert.equal(requests[1].updateCells.range.startColumnIndex,8);
  assert.equal(requests[2].updateCells.range.startColumnIndex,5);
});

test('a slow sync read does not block an edit or overwrite its newer value',async()=>{
  const {store,sheets}=await fixture();
  const record=store.getRecordBySheetRow('Insectary_data',2);
  const original=sheets.readSheet.bind(sheets);
  let release;const gate=new Promise(resolve=>{release=resolve;});
  sheets.readSheet=async sheet=>{if(sheet==='Insectary_data')await gate;return original(sheet);};
  const syncing=store.sync({sheets:['Insectary_data']});
  const saved=await Promise.race([
    store.updateRecord(record.id,{values:{Sex:'M'},requestId:'during-sync-0001'},user),
    new Promise((_,reject)=>setTimeout(()=>reject(new Error('edit blocked behind sync read')),500))
  ]);
  assert.equal(saved.status,'verified');
  release();
  const status=await syncing;
  assert.equal(status.skipped,1);
  assert.equal(store.getRecord(record.id).values.Sex,'M');
  store.close();
});

test('a moved source row is resolved by a unique identity before writing',async()=>{
  const {store,sheets}=await fixture();
  const record=store.getRecordBySheetRow('Insectary_data',2);
  sheets.rows.get('Insectary_data').find(r=>r.row===2).row=5;
  const result=await store.updateRecord(record.id,{values:{Sex:'M'},requestId:'moved-row-0001'},user);
  assert.equal(result.record.row,5);
  assert.equal(store.getRecord(record.id).row,5);
  assert.equal(store.getRecordBySheetRow('Insectary_data',2),null);
  store.close();
});

async function partialUndoFixture(){
  const {store}=await fixture();
  const insect=store.getRecordBySheetRow('Insectary_data',2),collection=store.getRecordBySheetRow('Collection_data',2);
  const first=await store.updateRecord(insect.id,{values:{Sex:'M'},requestId:'source-edit-0001'},user);
  const second=await store.updateRecord(collection.id,{values:{SPECIES:'Changed'},requestId:'source-edit-0002'},user);
  const original=store.resolveLiveRow.bind(store);let fail=true;
  store.resolveLiveRow=async record=>{
    if(fail&&record.sheet==='Collection_data'){fail=false;throw Object.assign(new Error('temporary source read failure'),{code:'SOURCE_UNAVAILABLE',status:503});}
    return original(record);
  };
  return {store,insect,collection,actionIds:[first.action.id,second.action.id]};
}

test('partial selected undo resumes only its unfinished record on the same requestId',async()=>{
  const {store,insect,collection,actionIds}=await partialUndoFixture();
  let failed;
  try{await store.undo({actionIds,requestId:'multi-undo-0001'},user);}catch(e){failed=e;}
  assert.equal(failed.code,'PARTIAL_UNDO');
  assert.equal(failed.details.applied.length,1);
  assert.equal(failed.details.failedIndex,1);
  const firstReversalId=failed.details.applied[0].actionId;
  assert.equal(store.getRecord(insect.id).values.Sex,'F');
  assert.equal(store.getRecord(collection.id).values.SPECIES,'Changed');
  const result=await store.undo({actionIds,requestId:'multi-undo-0001'},user);
  assert.equal(result.status,'verified');
  assert.equal(result.actions[0].id,firstReversalId);
  assert.equal(store.getRecord(collection.id).values.SPECIES,'Species');
  assert.equal((await store.undo({actionIds,requestId:'multi-undo-0001'},user)).actions[0].id,firstReversalId);
  store.close();
});

test('planned undo rejects a newer same-field edit even when its value returns',async()=>{
  const {store,collection,actionIds}=await partialUndoFixture();
  await assert.rejects(store.undo({actionIds,requestId:'multi-undo-0002'},user),e=>e.code==='PARTIAL_UNDO');
  await store.updateRecord(collection.id,{values:{SPECIES:'Other'},requestId:'later-edit-0001'},user);
  await store.updateRecord(collection.id,{values:{SPECIES:'Changed'},requestId:'later-edit-0002'},user);
  await assert.rejects(store.undo({actionIds,requestId:'multi-undo-0002'},user),e=>e.code==='PARTIAL_UNDO'&&e.details.conflict?.field==='SPECIES');
  assert.equal(store.getRecord(collection.id).values.SPECIES,'Changed');
  store.close();
});
