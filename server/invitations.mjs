// Accounts by invitation: an administrator enters a name, email and role; the
// person receives an email from the project Gmail account (sent with gog) with a
// link to choose their own username and password. Links last 7 days and work once.

import { createHash, randomBytes } from 'node:crypto';
import { spawn } from 'node:child_process';
import { passwordFields, publicUser, validateUsername } from './auth.mjs';

const DAY = 24 * 60 * 60 * 1000;
const ROLES = ['observer', 'editor', 'reviewer', 'admin'];
const ROLE_NAMES = { observer: 'lectura', editor: 'edición', reviewer: 'revisión', admin: 'administración' };
const EMAIL = /^[^\s@<>"]+@[^\s@<>"]+\.[a-z]{2,}$/i;
const digest = text => createHash('sha256').update(text).digest('hex');
const now = () => new Date().toISOString();
const bad = (code, message, status = 400) => Object.assign(new Error(message), { code, status });
const escape = text =>
  String(text).replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c]);

export function initInvitations(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS invitations(
    id TEXT PRIMARY KEY, email TEXT NOT NULL, display_name TEXT NOT NULL, role TEXT NOT NULL,
    token_hash TEXT NOT NULL UNIQUE, created_by TEXT NOT NULL, created_at TEXT NOT NULL, expires_at TEXT NOT NULL,
    sent_at TEXT, send_error TEXT, used_at TEXT, user_id TEXT)`);
  if (
    !db
      .prepare('PRAGMA table_info(users)')
      .all()
      .some(c => c.name === 'email')
  )
    db.exec('ALTER TABLE users ADD COLUMN email TEXT');
}

export function mailerFromEnv(env = process.env) {
  return {
    bin: env.ITHOMIINI_GOG_BIN || '',
    account: env.GOG_ACCOUNT || '',
    client: env.GOG_CLIENT || '',
    // Where invitation links point, e.g. https://tbs-insect-gallery.duckdns.org/ithomiini
    publicUrl: (env.APP_PUBLIC_URL || '').replace(/\/+$/, ''),
  };
}

/** Sends one email as the project account; resolves when Gmail accepted it. */
export function sendMail(mailer, { to, subject, text, html }) {
  if (!mailer.bin || !mailer.account) return Promise.reject(new Error('Email is not configured on the server'));
  const args = ['--account', mailer.account, ...(mailer.client ? ['--client', mailer.client] : []), '--no-input'];
  args.push('gmail', 'send', '--to', to, '--subject', subject, '--body-file', '-', '--body-html', html);
  return new Promise((resolve, reject) => {
    const child = spawn(mailer.bin, args, {
      env: {
        HOME: process.env.HOME,
        PATH: '/usr/bin:/bin',
        GOG_KEYRING_PASSWORD: process.env.GOG_KEYRING_PASSWORD ?? '',
      },
      stdio: ['pipe', 'ignore', 'pipe'],
    });
    let stderr = '';
    const timer = setTimeout(() => child.kill('SIGTERM'), 60000);
    child.stderr.on('data', chunk => (stderr = (stderr + chunk).slice(-2000)));
    child.on('error', e => {
      clearTimeout(timer);
      reject(e);
    });
    child.on('close', code => {
      clearTimeout(timer);
      code === 0 ? resolve() : reject(new Error(stderr.trim().split('\n').at(-1) || `gog exited ${code}`));
    });
    child.stdin.end(text);
  });
}

function view(row) {
  return {
    id: row.id,
    email: row.email,
    displayName: row.display_name,
    role: row.role,
    createdAt: row.created_at,
    expiresAt: row.expires_at,
    sentAt: row.sent_at,
    sendError: row.send_error,
    usedAt: row.used_at,
    status: row.used_at ? 'used' : row.expires_at < now() ? 'expired' : 'pending',
  };
}

function message(invitation, link, inviter) {
  const role = ROLE_NAMES[invitation.role] ?? invitation.role;
  const intro = `El proyecto de ithómidos te invitó a Ithomiini database, la app del equipo para los datos de colecta, monitoreo e insectario (acceso de ${role}).`;
  // Who sent it, unless that is only the app's own name.
  const by = inviter && !/^ithomiini database/i.test(inviter) ? `Invitación enviada por ${inviter}.` : '';
  const text = [
    `Hola ${invitation.display_name}:`,
    '',
    intro,
    '',
    'Crea tu usuario y contraseña con este enlace (vale 7 días y se usa una sola vez):',
    link,
    '',
    [by, 'Si no esperabas este correo, ignóralo.'].filter(Boolean).join(' '),
  ].join('\n');
  const html = `<div style="font-family:system-ui,sans-serif;max-width:32rem;line-height:1.5;color:#292524">
<p>Hola ${escape(invitation.display_name)}:</p>
<p>${escape(intro).replace('Ithomiini database', '<strong>Ithomiini database</strong>')}</p>
<p><a href="${escape(link)}" style="display:inline-block;background:#1f513a;color:#fff;padding:.6rem 1rem;border-radius:.4rem;text-decoration:none">Crear mi cuenta</a></p>
<p style="font-size:.85rem;color:#57534e">El enlace vale 7 días y se usa una sola vez. ${escape(by)} Si no esperabas este correo, ignóralo.</p>
</div>`;
  return { subject: 'Tu cuenta en Ithomiini database', text, html };
}

export function createInvitations(store, mailer, { send = sendMail } = {}) {
  initInvitations(store.db);
  const db = store.db;

  async function deliver(row, token, inviter) {
    const link = `${mailer.publicUrl}/#/activar?t=${token}`;
    try {
      await send(mailer, { to: row.email, ...message(row, link, inviter) });
      db.prepare('UPDATE invitations SET sent_at=?, send_error=NULL WHERE id=?').run(now(), row.id);
    } catch (e) {
      db.prepare('UPDATE invitations SET send_error=? WHERE id=?').run(String(e.message).slice(0, 300), row.id);
    }
    // The link is returned to the administrator too, so it can be shared by hand if email fails.
    return { invitation: view(db.prepare('SELECT * FROM invitations WHERE id=?').get(row.id)), link };
  }

  return {
    list() {
      return db.prepare('SELECT * FROM invitations ORDER BY created_at DESC LIMIT 200').all().map(view);
    },
    async create(body, admin) {
      const email = String(body.email ?? '')
        .trim()
        .toLowerCase();
      if (!EMAIL.test(email)) throw bad('INVALID_EMAIL', 'Enter a valid email address');
      const displayName = String(body.displayName ?? '')
        .trim()
        .slice(0, 100);
      if (!displayName) throw bad('INVALID_NAME', 'Enter the person’s name');
      const role = body.role || 'editor';
      if (!ROLES.includes(role)) throw bad('INVALID_ROLE', 'Unknown role');
      if (db.prepare('SELECT 1 FROM users WHERE lower(email)=? AND active=1').get(email))
        throw bad('EMAIL_IN_USE', 'An active account already uses this email', 409);
      // A new invitation replaces any unused one for the same address.
      db.prepare('UPDATE invitations SET expires_at=? WHERE lower(email)=? AND used_at IS NULL').run(now(), email);
      const token = randomBytes(24).toString('base64url');
      const row = {
        id: randomBytes(12).toString('hex'),
        email,
        display_name: displayName,
        role,
        token_hash: digest(token),
        created_by: admin.username,
        created_at: now(),
        expires_at: new Date(Date.now() + 7 * DAY).toISOString(),
      };
      db.prepare(
        'INSERT INTO invitations(id,email,display_name,role,token_hash,created_by,created_at,expires_at) VALUES(?,?,?,?,?,?,?,?)',
      ).run(...Object.values(row));
      return deliver(row, token, admin.displayName || admin.username);
    },
    /** A fresh link (the old one stops working) sent again. */
    async resend(id, admin) {
      const row = db.prepare('SELECT * FROM invitations WHERE id=?').get(id);
      if (!row) throw bad('INVITATION_NOT_FOUND', 'Invitation not found', 404);
      if (row.used_at) throw bad('INVITATION_USED', 'This invitation was already used', 409);
      const token = randomBytes(24).toString('base64url');
      const expires = new Date(Date.now() + 7 * DAY).toISOString();
      db.prepare('UPDATE invitations SET token_hash=?, expires_at=? WHERE id=?').run(digest(token), expires, id);
      return deliver({ ...row, expires_at: expires }, token, admin.displayName || admin.username);
    },
    revoke(id) {
      db.prepare('UPDATE invitations SET expires_at=? WHERE id=? AND used_at IS NULL').run(now(), id);
      return view(
        db.prepare('SELECT * FROM invitations WHERE id=?').get(id) ?? bad('INVITATION_NOT_FOUND', 'Not found', 404),
      );
    },
    /** What the activation page shows before the person chooses a username. */
    lookup(token) {
      const row = db.prepare('SELECT * FROM invitations WHERE token_hash=?').get(digest(String(token ?? '')));
      if (!row) throw bad('INVITATION_INVALID', 'This link is not valid', 404);
      const v = view(row);
      return { email: v.email, displayName: v.displayName, role: v.role, status: v.status };
    },
    accept(token, body) {
      const username = validateUsername(body.username);
      const { salt, hash } = passwordFields(body.password);
      db.exec('BEGIN IMMEDIATE');
      try {
        const row = db.prepare('SELECT * FROM invitations WHERE token_hash=?').get(digest(String(token ?? '')));
        if (!row) throw bad('INVITATION_INVALID', 'This link is not valid', 404);
        if (row.used_at) throw bad('INVITATION_USED', 'This link was already used', 409);
        if (row.expires_at < now()) throw bad('INVITATION_EXPIRED', 'This link expired; ask for a new one', 410);
        if (db.prepare('SELECT 1 FROM users WHERE username=?').get(username))
          throw bad('USERNAME_TAKEN', 'That username is taken; choose another', 409);
        const id = randomBytes(16).toString('hex');
        const displayName = String(body.displayName || row.display_name)
          .trim()
          .slice(0, 100);
        db.prepare(
          'INSERT INTO users(id,username,display_name,role,salt,password_hash,created_at,email) VALUES(?,?,?,?,?,?,?,?)',
        ).run(id, username, displayName, row.role, salt, hash, now(), row.email);
        db.prepare('UPDATE invitations SET used_at=?, user_id=? WHERE id=?').run(now(), id, row.id);
        db.exec('COMMIT');
        return publicUser(db.prepare('SELECT * FROM users WHERE id=?').get(id));
      } catch (e) {
        db.exec('ROLLBACK');
        throw e;
      }
    },
  };
}
