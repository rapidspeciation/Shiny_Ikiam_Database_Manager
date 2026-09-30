// Password recovery: a person asks for a link from the sign-in page (by username
// or email), or an administrator makes one in Usuarios (sent by email and also
// shown, to share by WhatsApp when email does not arrive). Links last 24 hours,
// work once, and a new one replaces the person's older unused ones. Only the
// hash of the token is stored.

import { createHash, randomBytes } from 'node:crypto';
import { passwordFields, publicUser, resolveAccount } from './auth.mjs';
import { sendMail } from './invitations.mjs';

const HOURS = 24;
const digest = text => createHash('sha256').update(text).digest('hex');
const now = () => new Date().toISOString();
const bad = (code, message, status = 400) => Object.assign(new Error(message), { code, status });
const escape = text =>
  String(text).replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c]);

export function initPasswordResets(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS password_resets(
    id TEXT PRIMARY KEY, user_id TEXT NOT NULL, token_hash TEXT NOT NULL UNIQUE, created_by TEXT,
    created_at TEXT NOT NULL, expires_at TEXT NOT NULL, sent_at TEXT, send_error TEXT, used_at TEXT)`);
}

/** The reset email, in English then Spanish like the invitations. `admin`: who made the link, if not the person. */
function message(user, link, admin) {
  const name = user.display_name || user.username;
  const withEmail = user.email ? ` (${user.email})` : '';
  const en = {
    hello: `Hi ${name},`,
    intro: admin
      ? `${admin} made you a link to choose a new password for Ithomiini database.`
      : 'Someone (hopefully you) asked to reset your Ithomiini database password.',
    user: `Your username is ${user.username}; you can also sign in with your email${withEmail}.`,
    how: `Choose a new password with this link (valid for ${HOURS} hours, single use):`,
    button: 'Choose a new password',
    end: 'If you did not ask for this, ignore this email; your password does not change.',
  };
  const es = {
    hello: `Hola ${name}:`,
    intro: admin
      ? `${admin} te creó un enlace para elegir una contraseña nueva en Ithomiini database.`
      : 'Alguien (esperamos que tú) pidió restablecer tu contraseña de Ithomiini database.',
    user: `Tu usuario es ${user.username}; también puedes entrar con tu correo${withEmail}.`,
    how: `Elige una contraseña nueva con este enlace (vale ${HOURS} horas y se usa una sola vez):`,
    button: 'Elegir una contraseña nueva',
    end: 'Si no lo pediste, ignora este correo; tu contraseña no cambia.',
  };
  const lines = m => [m.hello, '', m.intro, m.user, '', m.how, link, '', m.end];
  const text = [...lines(en), '', '— Español —', '', ...lines(es)].join('\n');
  const block = m => `<p>${escape(m.hello)}</p>
<p>${escape(m.intro).replace('Ithomiini database', '<strong>Ithomiini database</strong>')}</p>
<p>${escape(m.user)}</p>
<p><a href="${escape(link)}" style="display:inline-block;background:#1f513a;color:#fff;padding:.6rem 1rem;border-radius:.4rem;text-decoration:none">${escape(m.button)}</a></p>
<p style="font-size:.85rem;color:#57534e">${escape(m.how.replace(/:$/, '.'))} ${escape(m.end)}</p>`;
  const html = `<div style="font-family:system-ui,sans-serif;max-width:32rem;line-height:1.5;color:#292524">
${block(en)}
<hr style="border:none;border-top:1px solid #d6d3d1;margin:1.5rem 0">
${block(es)}
</div>`;
  return { subject: 'Reset your Ithomiini database password · Restablece tu contraseña', text, html };
}

export function createPasswordResets(store, mailer, { send = sendMail } = {}) {
  initPasswordResets(store.db);
  const db = store.db;
  const status = row => (row.used_at ? 'used' : row.expires_at < now() ? 'expired' : 'valid');

  /** A new link for the user; older unused ones stop working. */
  function issue(user, createdBy) {
    const token = randomBytes(24).toString('base64url');
    const id = randomBytes(12).toString('hex');
    db.exec('BEGIN IMMEDIATE');
    try {
      db.prepare('UPDATE password_resets SET expires_at=? WHERE user_id=? AND used_at IS NULL AND expires_at>?').run(
        now(),
        user.id,
        now(),
      );
      db.prepare(
        'INSERT INTO password_resets(id,user_id,token_hash,created_by,created_at,expires_at) VALUES(?,?,?,?,?,?)',
      ).run(id, user.id, digest(token), createdBy, now(), new Date(Date.now() + HOURS * 3600_000).toISOString());
      db.exec('COMMIT');
    } catch (e) {
      db.exec('ROLLBACK');
      throw e;
    }
    return { id, link: `${mailer.publicUrl}/#/restablecer?t=${token}` };
  }

  /** Emails the link; records (and returns) the error instead of throwing. */
  async function deliver(user, id, link, admin) {
    try {
      await send(mailer, { to: user.email, ...message(user, link, admin) });
      db.prepare('UPDATE password_resets SET sent_at=?, send_error=NULL WHERE id=?').run(now(), id);
      return null;
    } catch (e) {
      const error = String(e.message).slice(0, 300);
      db.prepare('UPDATE password_resets SET send_error=? WHERE id=?').run(error, id);
      return error;
    }
  }

  function row(token) {
    return db.prepare('SELECT * FROM password_resets WHERE token_hash=?').get(digest(String(token ?? '')));
  }

  return {
    /**
     * From the sign-in page. Returns the delivery (or null when there is nothing to
     * send); the caller answers the same thing either way and does not wait for it.
     */
    request(identifier) {
      const user = resolveAccount(store, identifier);
      if (!user?.email) return null;
      const { id, link } = issue(user, null);
      return deliver(user, id, link, null);
    },
    /** From Usuarios: the link is emailed when the person has an email, and returned to the administrator. */
    async adminLink(userId, admin) {
      const user = db.prepare('SELECT * FROM users WHERE id=?').get(userId);
      if (!user) throw bad('USER_NOT_FOUND', 'User not found', 404);
      if (!user.active) throw bad('USER_INACTIVE', 'This account is deactivated', 409);
      const { id, link } = issue(user, admin.username);
      const sendError = user.email ? await deliver(user, id, link, admin.displayName || admin.username) : null;
      return { link, email: user.email ?? null, sent: Boolean(user.email) && !sendError, sendError, username: user.username };
    },
    /** What the reset page shows: whose password it changes and whether the link still works. */
    lookup(token) {
      const reset = row(token);
      const user = reset && db.prepare('SELECT * FROM users WHERE id=? AND active=1').get(reset.user_id);
      if (!user) throw bad('RESET_INVALID', 'This link is not valid', 404);
      return { username: user.username, displayName: user.display_name, status: status(reset) };
    },
    /** Sets the new password, signs the person out everywhere and uses up the link; returns the user. */
    use(token, password) {
      const { salt, hash } = passwordFields(password);
      db.exec('BEGIN IMMEDIATE');
      try {
        const reset = row(token);
        const user = reset && db.prepare('SELECT * FROM users WHERE id=? AND active=1').get(reset.user_id);
        if (!user) throw bad('RESET_INVALID', 'This link is not valid', 404);
        if (reset.used_at) throw bad('RESET_USED', 'This link was already used; ask for a new one', 409);
        if (reset.expires_at < now()) throw bad('RESET_EXPIRED', 'This link expired; ask for a new one', 410);
        db.prepare('UPDATE users SET salt=?, password_hash=? WHERE id=?').run(salt, hash, user.id);
        db.prepare('DELETE FROM sessions WHERE user_id=?').run(user.id);
        db.prepare('UPDATE password_resets SET used_at=? WHERE id=?').run(now(), reset.id);
        // Any other link for the same person stops working too.
        db.prepare('UPDATE password_resets SET expires_at=? WHERE user_id=? AND used_at IS NULL').run(now(), user.id);
        db.exec('COMMIT');
        return publicUser(user);
      } catch (e) {
        db.exec('ROLLBACK');
        throw e;
      }
    },
  };
}
