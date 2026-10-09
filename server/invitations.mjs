// Accounts by invitation: an administrator enters a name, email and role; the
// person receives an email from the project Gmail account (sent with gog) with a
// link to choose their own username and password. The link does not expire: in
// its first 7 days it opens straight away; after that the page emails a 6-digit
// code to the invitation's address (valid 15 minutes, a new one replaces the
// last) and the account is created with that code. An invitation works once; a
// revoked one never works.

import { createHash, randomBytes, randomInt } from 'node:crypto';
import { spawn } from 'node:child_process';
import { passwordFields, publicUser, validateUsername } from './auth.mjs';

const DAY = 24 * 60 * 60 * 1000;
const CODE_MINUTES = 15;
/** Wrong tries of one code; after them a new code is needed. */
export const CODE_TRIES = 5;
export const ROLES = ['observer', 'editor', 'reviewer', 'admin'];
const ROLE_NAMES = { observer: 'lectura', editor: 'edición', reviewer: 'revisión', admin: 'administración' };
const ROLE_NAMES_EN = { observer: 'read-only', editor: 'editor', reviewer: 'reviewer', admin: 'administrator' };
const EMAIL = /^[^\s@<>"]+@[^\s@<>"]+\.[a-z]{2,}$/i;
export const isEmail = text => EMAIL.test(text);
const digest = text => createHash('sha256').update(text).digest('hex');
const now = () => new Date().toISOString();
const bad = (code, message, status = 400) => Object.assign(new Error(message), { code, status });
const escape = text =>
  String(text).replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c]);
export const cleanEmail = value =>
  String(value ?? '')
    .trim()
    .toLowerCase();
/** p•••n@gmail.com: enough for the person to recognise their address, not for others to read it. */
export function maskEmail(email) {
  const [local = '', domain] = String(email).split('@');
  if (!domain) return '•••';
  return `${local[0] ?? ''}•••${local.length > 2 ? local.at(-1) : ''}@${domain}`;
}
const codeDigest = (row, code) => digest(`${row.id}:${String(code ?? '').replace(/\s+/g, '')}`);

export function initInvitations(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS invitations(
    id TEXT PRIMARY KEY, email TEXT NOT NULL, display_name TEXT NOT NULL, role TEXT NOT NULL,
    token_hash TEXT NOT NULL UNIQUE, created_by TEXT NOT NULL, created_at TEXT NOT NULL, expires_at TEXT NOT NULL,
    sent_at TEXT, send_error TEXT, used_at TEXT, user_id TEXT)`);
  const has = (table, column) =>
    db
      .prepare(`PRAGMA table_info(${table})`)
      .all()
      .some(c => c.name === column);
  if (!has('users', 'email')) db.exec('ALTER TABLE users ADD COLUMN email TEXT');
  if (!has('invitations', 'revoked_at')) {
    db.exec('ALTER TABLE invitations ADD COLUMN revoked_at TEXT');
    // Revoking (and inviting the same address again) used to end the 7 days early: an unused
    // invitation that ended before its 7 days was revoked.
    db.exec(
      'UPDATE invitations SET revoked_at = expires_at WHERE used_at IS NULL AND julianday(expires_at) < julianday(created_at) + 6.99',
    );
  }
  // Openings of the link after its 7 days: the last one and how many.
  if (!has('invitations', 'expired_opened_at')) db.exec('ALTER TABLE invitations ADD COLUMN expired_opened_at TEXT');
  if (!has('invitations', 'expired_opens'))
    db.exec('ALTER TABLE invitations ADD COLUMN expired_opens INTEGER NOT NULL DEFAULT 0');
  // The code emailed after the 7 days: its hash, until when it works, its wrong tries, when it was sent.
  if (!has('invitations', 'code_hash')) db.exec('ALTER TABLE invitations ADD COLUMN code_hash TEXT');
  if (!has('invitations', 'code_expires_at')) db.exec('ALTER TABLE invitations ADD COLUMN code_expires_at TEXT');
  if (!has('invitations', 'code_tries'))
    db.exec('ALTER TABLE invitations ADD COLUMN code_tries INTEGER NOT NULL DEFAULT 0');
  if (!has('invitations', 'code_sent_at')) db.exec('ALTER TABLE invitations ADD COLUMN code_sent_at TEXT');
}

export function mailerFromEnv(env = process.env) {
  return {
    bin: env.ITHOMIINI_GOG_BIN || '',
    account: env.GOG_ACCOUNT || '',
    client: env.GOG_CLIENT || '',
    // Where invitation links point, e.g. https://ithomiini-ikiam.com
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

/** pending: the first 7 days; expired: a code is needed; used; revoked. */
export function statusOf(row) {
  if (row.used_at) return 'used';
  if (row.revoked_at) return 'revoked';
  return row.expires_at < now() ? 'expired' : 'pending';
}

function view(row) {
  return {
    id: row.id,
    email: row.email,
    displayName: row.display_name,
    role: row.role,
    createdBy: row.created_by,
    createdAt: row.created_at,
    expiresAt: row.expires_at,
    sentAt: row.sent_at,
    sendError: row.send_error,
    usedAt: row.used_at,
    revokedAt: row.revoked_at ?? null,
    expiredOpens: row.expired_opens ?? 0,
    expiredOpenedAt: row.expired_opened_at ?? null,
    codeSentAt: row.code_sent_at ?? null,
    status: statusOf(row),
  };
}

const htmlPage = blocks => `<div style="font-family:system-ui,sans-serif;max-width:32rem;line-height:1.5;color:#292524">
${blocks.join('\n<hr style="border:none;border-top:1px solid #d6d3d1;margin:1.5rem 0">\n')}
</div>`;
const bold = text => escape(text).replace('Ithomiini database', '<strong>Ithomiini database</strong>');
const small = text => `<p style="font-size:.85rem;color:#57534e">${escape(text)}</p>`;

/** The invitation email, in English then Spanish (the team is in Ecuador and the UK). */
function message(invitation, link, inviter) {
  const name = invitation.display_name;
  const email = invitation.email;
  const roleEs = ROLE_NAMES[invitation.role] ?? invitation.role;
  const roleEn = ROLE_NAMES_EN[invitation.role] ?? invitation.role;
  // Who sent it, unless that is only the app's own name.
  const named = inviter && !/^ithomiini database/i.test(inviter);
  const en = {
    hello: `Hi ${name},`,
    intro: `The Ithomiini project invited you to Ithomiini database, the team's app for collecting, monitoring and insectary data (${roleEn} access).`,
    button: 'Create my account',
    how: `Choose your username and password with the button above. The link doesn't expire: in the first 7 days it opens straight away; after that it sends a 6-digit code to this email (${email}) to check it's you. Then sign in with your username or with this email.`,
    wifi: "If the page doesn't open on Ikiam's Wi-Fi, try with mobile data.",
    end: [named ? `Invitation sent by ${inviter}.` : '', "If you weren't expecting this email, ignore it."].filter(Boolean).join(' '),
  };
  const es = {
    hello: `Hola ${name}:`,
    intro: `El proyecto de ithómidos te invitó a Ithomiini database, la app del equipo para los datos de colecta, monitoreo e insectario (acceso de ${roleEs}).`,
    button: 'Crear mi cuenta',
    how: `Elige tu usuario y contraseña con el botón de arriba. El enlace no caduca: los primeros 7 días se abre directamente; después envía un código de 6 cifras a este correo (${email}) para comprobar que eres tú. Luego entra con tu usuario o con este correo.`,
    wifi: 'Si la página no abre con el Wi-Fi de Ikiam, prueba con datos móviles.',
    end: [named ? `Invitación enviada por ${inviter}.` : '', 'Si no esperabas este correo, ignóralo.'].filter(Boolean).join(' '),
  };
  const lines = m => [m.hello, '', m.intro, '', `${m.button}: ${link}`, '', m.how, '', m.wifi, '', m.end];
  const text = [...lines(en), '', '— Español —', '', ...lines(es)].join('\n');
  const block = m => `<p>${escape(m.hello)}</p>
<p>${bold(m.intro)}</p>
<p><a href="${escape(link)}" style="display:inline-block;background:#1f513a;color:#fff;padding:.6rem 1rem;border-radius:.4rem;text-decoration:none">${escape(m.button)}</a></p>
<p>${escape(m.how)}</p>
${small(m.wifi)}
${small(m.end)}`;
  return {
    subject: 'Your Ithomiini database account · Tu cuenta en Ithomiini database',
    text,
    html: htmlPage([block(en), block(es)]),
  };
}

/** The email with the 6-digit code asked for from an invitation's page after its 7 days. */
function codeMessage(code) {
  const en = `Your code is ${code}. It's valid for ${CODE_MINUTES} minutes. If you didn't ask for it, ignore this email.`;
  const es = `Tu código es ${code}. Vale ${CODE_MINUTES} minutos. Si no lo pediste, ignora este correo.`;
  const block = m =>
    `<p>${escape(m).replace(code, `<strong style="font-size:1.25rem;letter-spacing:.15em">${code}</strong>`)}</p>`;
  return {
    subject: `${code} · Code for your Ithomiini database account / Código para tu cuenta`,
    text: [en, '', '— Español —', '', es].join('\n'),
    html: htmlPage([block(en), block(es)]),
  };
}

export function createInvitations(store, mailer, { send = sendMail } = {}) {
  initInvitations(store.db);
  const db = store.db;
  const byId = id => db.prepare('SELECT * FROM invitations WHERE id=?').get(id);
  const byToken = token => db.prepare('SELECT * FROM invitations WHERE token_hash=?').get(digest(String(token ?? '')));

  async function deliver(row, token, inviter) {
    const link = `${mailer.publicUrl}/#/activar?t=${token}`;
    try {
      await send(mailer, { to: row.email, ...message(row, link, inviter) });
      db.prepare('UPDATE invitations SET sent_at=?, send_error=NULL WHERE id=?').run(now(), row.id);
    } catch (e) {
      db.prepare('UPDATE invitations SET send_error=? WHERE id=?').run(String(e.message).slice(0, 300), row.id);
    }
    // The link is returned to the administrator too, so it can be shared by hand if email fails.
    return { invitation: view(byId(row.id)), link };
  }

  /** The invitation of a link, or the reason it cannot be used (a fresh one needs no code). */
  function usable(row) {
    if (!row) throw bad('INVITATION_INVALID', 'This link is not valid', 404);
    if (row.used_at) throw bad('INVITATION_USED', 'This invitation was already used', 409);
    if (row.revoked_at) throw bad('INVITATION_REVOKED', 'This invitation was cancelled; ask a team administrator', 410);
    return row;
  }

  /**
   * Checks the code of an invitation past its 7 days. A wrong one counts (it is saved at once);
   * after CODE_TRIES wrong ones the code stops working.
   */
  function checkCode(row, code) {
    if (!row.code_hash) throw bad('CODE_NEEDED', 'Ask for a code first', 403);
    if (row.code_expires_at < now()) throw bad('CODE_EXPIRED', 'This code expired; ask for a new one', 410);
    if (codeDigest(row, code) === row.code_hash) return;
    const tries = (row.code_tries ?? 0) + 1;
    if (tries >= CODE_TRIES) {
      db.prepare('UPDATE invitations SET code_hash=NULL, code_tries=? WHERE id=?').run(tries, row.id);
      throw bad('CODE_EXPIRED', 'Too many wrong codes; ask for a new one', 410);
    }
    db.prepare('UPDATE invitations SET code_tries=? WHERE id=?').run(tries, row.id);
    throw bad('CODE_WRONG', 'That code is not right', 400);
  }

  return {
    list() {
      return db.prepare('SELECT * FROM invitations ORDER BY created_at DESC LIMIT 200').all().map(view);
    },
    /** For an address: its active account, and its latest invitation still open (not used nor revoked). */
    forEmail(value) {
      const email = cleanEmail(value);
      const account = db
        .prepare('SELECT username, display_name, role FROM users WHERE lower(email)=? AND active=1')
        .get(email);
      const open = db
        .prepare(
          'SELECT * FROM invitations WHERE lower(email)=? AND used_at IS NULL AND revoked_at IS NULL ORDER BY created_at DESC LIMIT 1',
        )
        .get(email);
      return {
        account: account ? { username: account.username, displayName: account.display_name, role: account.role } : null,
        invitation: open ? view(open) : null,
      };
    },
    async create(body, admin) {
      const email = cleanEmail(body.email);
      if (!EMAIL.test(email)) throw bad('INVALID_EMAIL', 'Enter a valid email address');
      const displayName = String(body.displayName ?? '')
        .trim()
        .slice(0, 100);
      if (!displayName) throw bad('INVALID_NAME', 'Enter the person’s name');
      const role = body.role || 'editor';
      if (!ROLES.includes(role)) throw bad('INVALID_ROLE', 'Unknown role');
      if (db.prepare('SELECT 1 FROM users WHERE lower(email)=? AND active=1').get(email))
        throw bad('EMAIL_IN_USE', 'An active account already uses this email', 409);
      // A new invitation replaces any open one for the same address.
      db.prepare(
        'UPDATE invitations SET revoked_at=?, code_hash=NULL WHERE lower(email)=? AND used_at IS NULL AND revoked_at IS NULL',
      ).run(now(), email);
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
    /** A fresh link sent again (the old one stops working), with 7 new days; a revoked invitation works again. */
    async resend(id, admin) {
      const row = byId(id);
      if (!row) throw bad('INVITATION_NOT_FOUND', 'Invitation not found', 404);
      if (row.used_at) throw bad('INVITATION_USED', 'This invitation was already used', 409);
      const token = randomBytes(24).toString('base64url');
      const expires = new Date(Date.now() + 7 * DAY).toISOString();
      db.prepare(
        'UPDATE invitations SET token_hash=?, expires_at=?, revoked_at=NULL, code_hash=NULL, code_tries=0 WHERE id=?',
      ).run(digest(token), expires, id);
      return deliver({ ...row, expires_at: expires }, token, admin.displayName || admin.username);
    },
    revoke(id) {
      db.prepare('UPDATE invitations SET revoked_at=?, code_hash=NULL WHERE id=? AND used_at IS NULL AND revoked_at IS NULL').run(
        now(),
        id,
      );
      const row = byId(id);
      if (!row) throw bad('INVITATION_NOT_FOUND', 'Invitation not found', 404);
      return view(row);
    },
    /**
     * What the activation page shows before the person chooses a username. Past its 7 days the
     * address is shown masked (whoever has the link only learns whose it is), and the opening is
     * counted for the administrators.
     */
    lookup(token) {
      const row = byToken(token);
      if (!row) throw bad('INVITATION_INVALID', 'This link is not valid', 404);
      const status = statusOf(row);
      if (status === 'expired')
        db.prepare('UPDATE invitations SET expired_opened_at=?, expired_opens=expired_opens+1 WHERE id=?').run(now(), row.id);
      return {
        email: status === 'pending' ? row.email : maskEmail(row.email),
        displayName: row.display_name,
        role: row.role,
        status,
        // A code sent and still good: the page asks for it straight away.
        codeSent: status === 'expired' && !!row.code_hash && row.code_expires_at > now(),
      };
    },
    /** Past the 7 days: a new 6-digit code to the invitation's address (the last one stops working). */
    async sendCode(token) {
      const row = usable(byToken(token));
      if (statusOf(row) === 'pending') throw bad('CODE_NOT_NEEDED', 'This link works without a code', 409);
      const code = String(randomInt(0, 1_000_000)).padStart(6, '0');
      db.prepare('UPDATE invitations SET code_hash=?, code_expires_at=?, code_tries=0, code_sent_at=? WHERE id=?').run(
        codeDigest(row, code),
        new Date(Date.now() + CODE_MINUTES * 60_000).toISOString(),
        now(),
        row.id,
      );
      try {
        await send(mailer, { to: row.email, ...codeMessage(code) });
      } catch {
        db.prepare('UPDATE invitations SET code_hash=NULL WHERE id=?').run(row.id);
        throw bad('CODE_NOT_SENT', 'The code could not be sent; try again in a few minutes', 502);
      }
      return { email: maskEmail(row.email), minutes: CODE_MINUTES };
    },
    /** Whether the code is right, before the person chooses a username (a wrong one counts); then their whole address. */
    checkCode(token, code) {
      const row = usable(byToken(token));
      if (statusOf(row) !== 'pending') checkCode(row, code);
      return { ok: true, email: row.email };
    },
    /** Creates the account: with the link alone in its 7 days, with the emailed code after them. */
    accept(token, body) {
      const username = validateUsername(body.username);
      const { salt, hash } = passwordFields(body.password);
      const first = usable(byToken(token));
      // Checked (and a wrong code counted) before the account is written.
      if (statusOf(first) === 'expired') checkCode(first, body.code);
      db.exec('BEGIN IMMEDIATE');
      try {
        const row = usable(byToken(token));
        // The same code still (not replaced, used up or expired in between).
        if (statusOf(row) === 'expired' && (row.code_hash !== codeDigest(row, body.code) || row.code_expires_at < now()))
          throw bad('CODE_EXPIRED', 'This code expired; ask for a new one', 410);
        if (db.prepare('SELECT 1 FROM users WHERE username=?').get(username))
          throw bad('USERNAME_TAKEN', 'That username is taken; choose another', 409);
        const id = randomBytes(16).toString('hex');
        const displayName = String(body.displayName || row.display_name)
          .trim()
          .slice(0, 100);
        db.prepare(
          'INSERT INTO users(id,username,display_name,role,salt,password_hash,created_at,email) VALUES(?,?,?,?,?,?,?,?)',
        ).run(id, username, displayName, row.role, salt, hash, now(), row.email);
        db.prepare('UPDATE invitations SET used_at=?, user_id=?, code_hash=NULL WHERE id=?').run(now(), id, row.id);
        db.exec('COMMIT');
        return publicUser(db.prepare('SELECT * FROM users WHERE id=?').get(id));
      } catch (e) {
        db.exec('ROLLBACK');
        throw e;
      }
    },
  };
}
