// The assistant's tools for inviting people from a chat (administrators only): invite_person
// previews, then sends through the same invitations as Usuarios (server/invitations.mjs);
// list_invitations says how each invitation stands.

import { ROLES, cleanEmail, isEmail } from './invitations.mjs';

/** What each role may do in the app (server/index.mjs requireEditor, requireReviewer, requireAdmin). */
export const ROLE_ACCESS = {
  observer: 'read-only: sees the tabs (not Revisión) but cannot enter, edit or undo data, nor use the AI assistant',
  editor: 'enters and edits data in every tab, undoes saves in Historial, and uses the AI assistant (proposes and applies changes)',
  reviewer:
    "as an editor, plus adding pre-made rows at the end of a sheet, changing or removing anyone's clutch events and walks, and applying the re-matching of walks",
  admin: 'everything: as a reviewer, plus users and invitations, and updating the AI assistant',
};
const STATUS = {
  pending: 'link works straight away (first 7 days)',
  expired: 'past 7 days: the link emails a 6-digit code first',
  used: 'account created',
  revoked: 'cancelled; resending makes it work again',
};
const RECEIVES =
  'An email from the project account with a «Create my account» button: for 7 days the link opens straight away, after that it first emails them a 6-digit code. They choose a username and password, then sign in with that username or this email.';

export const INVITE_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'invite_person',
      description: [
        'Invite someone to the app by email (only for administrators). The role is required: ask it when the person did not say.',
        "- Without `confirmed`: a preview, nothing sent (who, the role in plain words, and an account or open invitation the email already has: an open one is sent again). Show it.",
        "- `confirmed: true`, only after the person's latest message approves that preview: sends it; answers whether the email went out, or the error and the link to share by hand.",
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          email: { type: 'string' },
          name: { type: 'string', description: 'Name shown in the app and the email' },
          role: { type: 'string', enum: ROLES },
          confirmed: { type: 'boolean' },
        },
        required: ['email', 'name', 'role'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'list_invitations',
      description:
        'The invitations, newest first (only for administrators): status (pending, expired = needs an emailed code, used, revoked), sent, send error, and how often the link was opened after its 7 days.',
      parameters: {
        type: 'object',
        properties: {
          email: { type: 'string', description: 'Only this address' },
          status: { type: 'string', enum: Object.keys(STATUS) },
        },
      },
    },
  },
];
export const INVITE_TOOL_NAMES = new Set(INVITE_TOOLS.map(t => t.function.name));

const day = iso => (iso ? String(iso).slice(0, 10) : null);
const brief = i => ({
  name: i.displayName,
  email: i.email,
  role: i.role,
  status: i.status,
  invitedBy: i.createdBy,
  created: day(i.createdAt),
  ...(i.sentAt ? { sent: day(i.sentAt) } : {}),
  ...(i.sendError ? { sendError: i.sendError } : {}),
  ...(i.usedAt ? { used: day(i.usedAt) } : {}),
  ...(i.revokedAt ? { revoked: day(i.revokedAt) } : {}),
  ...(i.expiredOpens ? { openedAfterExpiry: i.expiredOpens, lastOpenedAfterExpiry: day(i.expiredOpenedAt) } : {}),
});

/** Runs one of INVITE_TOOLS as `user` (read from the database at the call: their role now). */
export async function runInviteTool(invitations, name, args = {}, user) {
  if (user?.role !== 'admin')
    return { error: 'Only an administrator can invite people or see the invitations: ask a team administrator.' };
  if (name === 'list_invitations') {
    const email = args.email ? cleanEmail(args.email) : null;
    const list = invitations
      .list()
      .filter(i => (!email || i.email === email) && (!args.status || i.status === args.status))
      .slice(0, 50);
    return { invitations: list.map(brief), statuses: STATUS };
  }
  const email = cleanEmail(args.email);
  if (!isEmail(email)) return { error: `"${String(args.email ?? '')}" is not an email address. Nothing was sent.` };
  const displayName = String(args.name ?? '')
    .trim()
    .slice(0, 100);
  if (!displayName) return { error: "Give the person's name. Nothing was sent." };
  if (!ROLES.includes(args.role))
    return { error: `Give a role (${ROLES.join(', ')}); ask the person if they did not say. Nothing was sent.` };
  const role = args.role;
  const { account, invitation } = invitations.forEmail(email);
  const action = account ? 'exists' : invitation ? (invitation.role === role ? 'resend' : 'replace') : 'invite';
  const about = {
    name: displayName,
    email,
    role,
    access: ROLE_ACCESS[role],
    ...(account ? { account } : {}),
    ...(invitation ? { openInvitation: brief(invitation) } : {}),
  };
  if (action === 'exists')
    return {
      ...about,
      action,
      sent: false,
      note: `This email already has an account (username ${account.username}): nothing to send. They sign in with that username or this email; a forgotten password is reset from the sign-in page.`,
    };
  const what = {
    invite: 'A new invitation.',
    resend: 'This email has an open invitation with that role: it is sent again with a fresh link (the old link stops working) and 7 new days.',
    replace: `This email has an open invitation as ${invitation?.role}: a new one as ${role} replaces it (the old link stops working).`,
  }[action];
  if (!args.confirmed)
    return { ...about, action, preview: true, sent: false, note: `${what} Nothing was sent: show this and call again with confirmed: true once the person approves.` };
  const admin = { username: user.username, displayName: user.displayName };
  const out =
    action === 'resend'
      ? await invitations.resend(invitation.id, admin)
      : await invitations.create({ email, displayName, role }, admin);
  const sent = Boolean(out.invitation.sentAt) && !out.invitation.sendError;
  return {
    action,
    sent,
    name: out.invitation.displayName,
    email,
    role: out.invitation.role,
    status: out.invitation.status,
    ...(sent
      ? { theyReceive: RECEIVES }
      : {
          sendError: out.invitation.sendError,
          link: out.link,
          note: 'The email did not go out: share this link with the person by hand (it works like the email).',
        }),
  };
}
