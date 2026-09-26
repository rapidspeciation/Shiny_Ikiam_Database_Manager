import { randomBytes, scryptSync, createHash, timingSafeEqual } from 'node:crypto';

const digest = text => createHash('sha256').update(text).digest('hex');
const safeEqual = (a,b) => {
  const aa=Buffer.from(String(a)),bb=Buffer.from(String(b));
  return aa.length===bb.length && timingSafeEqual(aa,bb);
};
const bad = (code,message,status=400) => Object.assign(new Error(message),{code,status});

export function publicUser(row) {
  return row && { id:row.id, username:row.username, displayName:row.display_name, role:row.role, active:Boolean(row.active) };
}
export function validatePassword(password) {
  if(typeof password!=='string'||password.length<6||password.length>16) throw bad('WEAK_PASSWORD','Password must have 6 to 16 characters');
}
export function passwordFields(password) {
  validatePassword(password);
  const salt=randomBytes(24).toString('hex');
  return {salt,hash:scryptSync(password,salt,64).toString('hex')};
}
export function verifyPassword(password,user) {
  if(!user||typeof password!=='string')return false;
  const given=scryptSync(password,user.salt,64);
  const expected=Buffer.from(user.password_hash,'hex');
  return given.length===expected.length&&timingSafeEqual(given,expected);
}
export function setup(store,body,config) {
  const count=store.db.prepare("SELECT count(*) n FROM users WHERE role='admin' AND active=1").get().n;
  if(count)throw bad('SETUP_CLOSED','Initial setup is already complete',403);
  if(!config.setupToken||!safeEqual(body.token,config.setupToken))throw bad('INVALID_SETUP_TOKEN','Setup token is invalid',403);
  const username=validateUsername(body.username);
  const {salt,hash}=passwordFields(body.password);
  const user={id:cryptoId(),username,displayName:String(body.displayName||username).trim().slice(0,100),role:'admin'};
  store.db.prepare('INSERT INTO users(id,username,display_name,role,salt,password_hash,created_at) VALUES(?,?,?,?,?,?,?)').run(user.id,user.username,user.displayName,user.role,salt,hash,new Date().toISOString());
  return user;
}
function cryptoId(){return randomBytes(16).toString('hex');}
export function validateUsername(value) {
  if(typeof value!=='string'||!/^[a-zA-Z0-9._-]{3,64}$/.test(value))throw bad('INVALID_USERNAME','Username must be 3 to 64 letters, numbers, dots, underscores, or hyphens');
  return value.toLowerCase();
}
export function login(store,body) {
  const username=typeof body.username==='string'?body.username.toLowerCase():'';
  const row=store.db.prepare('SELECT * FROM users WHERE username=? AND active=1').get(username);
  if(!verifyPassword(body.password,row))throw bad('BAD_CREDENTIALS','Invalid username or password',401);
  const token=randomBytes(32).toString('base64url'),csrf=digest(`csrf:${token}`);
  const expires=new Date(Date.now()+14*24*60*60*1000).toISOString();
  store.db.prepare('INSERT INTO sessions(id_hash,user_id,csrf_hash,expires_at,created_at) VALUES(?,?,?,?,?)').run(digest(token),row.id,digest(csrf),expires,new Date().toISOString());
  return {user:publicUser(row),token,csrf,expires};
}
export function getSession(store,cookieHeader) {
  const raw=String(cookieHeader||'').split(';').map(s=>s.trim()).find(s=>s.startsWith('ithomiini_session='))?.split('=')[1];
  if(!raw)return null;
  const row=store.db.prepare('SELECT s.*,u.* FROM sessions s JOIN users u ON u.id=s.user_id WHERE s.id_hash=? AND s.expires_at>? AND u.active=1').get(digest(raw),new Date().toISOString());
  return row?{user:publicUser(row),token:raw,idHash:row.id_hash,csrfHash:row.csrf_hash}:null;
}
export function checkCsrf(session,header) {
  if(!session||typeof header!=='string'||!safeEqual(digest(header),session.csrfHash))throw bad('CSRF_REQUIRED','Valid CSRF token required',403);
}
export function revoke(store,session) { if(session)store.db.prepare('DELETE FROM sessions WHERE id_hash=?').run(session.idHash); }
export function cookie(token,{path='/',secure=true,maxAge=14*24*60*60}={}) {
  return `ithomiini_session=${token||''}; Path=${path}; HttpOnly; SameSite=Lax; Max-Age=${token?maxAge:0}${secure?'; Secure':''}`;
}
export function requireAdmin(user) {if(user?.role!=='admin')throw bad('FORBIDDEN','Administrator role required',403);}
export function createUser(store,body) {
  const username=validateUsername(body.username);
  const role=body.role||'observer';
  if(!['observer','editor','reviewer','admin'].includes(role))throw bad('INVALID_ROLE','Unknown role');
  const {salt,hash}=passwordFields(body.password);
  const user={id:cryptoId(),username,displayName:String(body.displayName||username).trim().slice(0,100),role};
  store.db.prepare('INSERT INTO users(id,username,display_name,role,salt,password_hash,created_at) VALUES(?,?,?,?,?,?,?)').run(user.id,user.username,user.displayName,user.role,salt,hash,new Date().toISOString());
  return user;
}
export function updateUser(store,id,body,currentUser) {
  const row=store.db.prepare('SELECT * FROM users WHERE id=?').get(id);
  if(!row)throw bad('USER_NOT_FOUND','User not found',404);
  if(body.role!==undefined) {
    if(!['observer','editor','reviewer','admin'].includes(body.role))throw bad('INVALID_ROLE','Unknown role');
    if(id===currentUser.id&&body.role!=='admin')throw bad('SELF_DEMOTION','You cannot remove your own administrator role',409);
    store.db.prepare('UPDATE users SET role=? WHERE id=?').run(body.role,id);
  }
  if(body.active!==undefined) {
    if(id===currentUser.id&&!body.active)throw bad('SELF_DEACTIVATION','You cannot deactivate yourself',409);
    store.db.prepare('UPDATE users SET active=? WHERE id=?').run(body.active?1:0,id);
    if(!body.active)store.db.prepare('DELETE FROM sessions WHERE user_id=?').run(id);
  }
  if(body.displayName!==undefined)store.db.prepare('UPDATE users SET display_name=? WHERE id=?').run(String(body.displayName).trim().slice(0,100),id);
  if(body.password!==undefined) {
    const {salt,hash}=passwordFields(body.password);
    store.db.prepare('UPDATE users SET salt=?,password_hash=? WHERE id=?').run(salt,hash,id);
    store.db.prepare('DELETE FROM sessions WHERE user_id=?').run(id);
  }
  return publicUser(store.db.prepare('SELECT * FROM users WHERE id=?').get(id));
}
export function csrfForSession(store,session) {
  return digest(`csrf:${session.token}`);
}
