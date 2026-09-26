import {icon} from './icons.js';

export const esc = value => String(value ?? '').replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
export const plain = value => value===null||value===undefined||value===''?'—':String(value);
function asDate(value){if(typeof value==='number'&&value>1000&&value<100000)return new Date(Date.UTC(1899,11,30)+Math.round(value*86400000));return new Date(value)}
export function dateSerial(value){if(!value)return null;const [year,month,day]=String(value).slice(0,10).split('-').map(Number);if(!year||!month||!day)return null;return Math.round((Date.UTC(year,month-1,day)-Date.UTC(1899,11,30))/86400000)}
export const fmtDate = value => {if(value===null||value===undefined||value==='')return '—';const d=asDate(value);return Number.isNaN(d.getTime())?String(value):new Intl.DateTimeFormat(document.documentElement.lang==='en'?'en-GB':'es-EC',{dateStyle:'medium',timeZone:typeof value==='number'||/^\d{4}-\d{2}-\d{2}$/.test(String(value))?'UTC':'America/Guayaquil'}).format(d)};
export const fmtTime = value => {if(value===null||value===undefined||value==='')return '—';const d=asDate(value);return Number.isNaN(d.getTime())?String(value):new Intl.DateTimeFormat(document.documentElement.lang==='en'?'en-GB':'es-EC',{dateStyle:'medium',timeStyle:'short',timeZone:'America/Guayaquil'}).format(d)};
export const titleCase = text => String(text||'').replaceAll('_',' ').replace(/([a-z])([A-Z])/g,'$1 $2');
export const hasMeaningfulValue = value => value!==null&&value!==undefined&&!['','NA','N/A','NULL','—','-'].includes(String(value).trim().toUpperCase());

function fieldType(field){const type=String(field.type||'').toLowerCase();if(['date','datetime-local','time','number','email','url'].includes(type))return type;if(type==='datetime')return 'datetime-local';if(type==='boolean'||type==='checkbox')return 'checkbox';return 'text'}
function inputValue(value,type){if(value==null)return '';if(type==='date'){if(typeof value==='number'&&value>1000&&value<100000)return asDate(value).toISOString().slice(0,10);if(typeof value==='string')return value.slice(0,10)}if(type==='datetime-local'){if(typeof value==='number'&&value>1000&&value<100000)return asDate(value).toISOString().slice(0,16);if(typeof value==='string')return value.slice(0,16)}return String(value)}
export function formFields(module, values={}, formulas={}){
  const fields=module?.fields||[];
  if(!fields.length)return '<p class="empty-inline">No hay campos editables definidos para este registro.</p>';
  return fields.map((field,index)=>{
    const key=field.key||field.name;const type=fieldType(field);const id=`field-${index}`;const readonly=field.readonly||Object.hasOwn(formulas,key);const label=field.label||titleCase(key);const val=values[key];
    const help=readonly?`<small class="field-note">${icon('link',14)} ${Object.hasOwn(formulas,key)?'Valor calculado en la hoja; se conserva la fórmula.':'Campo de solo lectura.'}</small>`:'';
    const choices=field.options||field.suggestions||[];
    const hasList=choices.length>0||key==='Subspecies_Form';
    const list=hasList?`<datalist id="list-${id}">${choices.map(opt=>`<option value="${esc(typeof opt==='object'?(opt.value??opt.label):opt)}"></option>`).join('')}</datalist>`:'';
    let control;
    if(type==='checkbox') control=`<input type="checkbox" id="${id}" name="${esc(key)}" ${val?'checked':''} ${readonly?'disabled':''}>`;
    else if(field.multiline||String(key).toLowerCase().includes('note')||String(key).toLowerCase().includes('comment')) control=`<textarea id="${id}" name="${esc(key)}" rows="3" ${field.required?'required':''} ${readonly?'readonly':''}>${esc(inputValue(val,type))}</textarea>`;
    else control=`<input id="${id}" name="${esc(key)}" type="${type}" value="${esc(inputValue(val,type))}" ${field.required?'required':''} ${readonly?'readonly':''} ${hasList?`list="list-${id}"`:''} ${type==='number'?'step="any"':''} autocomplete="off">${list}`;
    return `<div class="field ${readonly?'is-readonly':''} ${type==='checkbox'?'field-check':''}"><label for="${id}">${esc(label)}${field.required?'<span class="required" aria-label="obligatorio"> *</span>':''}</label>${control}${help}</div>`;
  }).join('');
}
export function formValues(form,module,{onlyChanged=false,original={}}={}){
  const values={};for(const field of module?.fields||[]){const key=field.key||field.name;const input=Array.from(form.elements).find(el=>el.name===key);if(!input||input.readOnly||input.disabled)continue;let value=input.type==='checkbox'?input.checked:input.value.trim();if(input.type==='number'&&value!=='')value=Number(value);if(value===''&&original[key]==null)continue;if(onlyChanged&&String(value??'')===inputValue(original[key],fieldType(field)))continue;if(input.type==='date'&&value)value=dateSerial(value);values[key]=value}return values;
}
export function primaryValues(record,module,count=3){
  const values=record.values||{};const preferred=(module?.identityFields||[]).filter(key=>hasMeaningfulValue(values[key]));const fallback=Object.keys(values).filter(key=>hasMeaningfulValue(values[key])&&!preferred.includes(key));return [...preferred,...fallback].slice(0,count).map(key=>({key,label:(module?.fields||[]).find(f=>f.key===key)?.label||titleCase(key),value:values[key]}));
}
export function recordTitle(record,module){const preferred=primaryValues(record,module,1)[0]?.value;return hasMeaningfulValue(record.label)?record.label:preferred||`${record.sheet||module?.label||'Registro'} · ${record.row||record.id}`}
export function statusLabel(status){const es={verified:'Guardado y verificado',saved:'Guardado',pending:'Pendiente de verificar',queued:'Pendiente sin conexión',recorded_in_app:'Registrado en la aplicación',partial:'Guardado parcialmente; revisar',needs_review:'Revisión necesaria',conflict:'Conflicto',uncertain:'Verificación pendiente',rejected:'Rechazado'};const en={verified:'Saved and verified',saved:'Saved',pending:'Pending verification',queued:'Queued offline',recorded_in_app:'Recorded in app',partial:'Partially saved; review',needs_review:'Review needed',conflict:'Conflict',uncertain:'Verification pending',rejected:'Rejected'};return (document.documentElement.lang==='en'?en:es)[status]||status||(document.documentElement.lang==='en'?'Recorded':'Registrado')}
