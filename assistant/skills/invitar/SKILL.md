---
name: invitar
description: Invite someone to the app from the chat (administrators only), or say how the invitations stand. Use it when an administrator asks to invite, add or give access to a person ("invita a …", "dale acceso a …", "add Ana as an editor"), to send an invitation again, or asks who has not created their account yet.
---

# Inviting someone

The tools are `invite_person` and `list_invitations`; they work for
administrators only, and say so to anyone else.

1. **Name, email and role.** When the role was not said, ask: "admin or a
   regular team member (editor)?". The four roles:
   - **admin**: everything, including users, invitations and the AI assistant;
   - **reviewer**: as an editor, plus pre-made rows at the end of a sheet,
     changing anyone's clutch events and walks, and applying the re-matching
     of walks;
   - **editor**: enters and edits data in every tab, undoes saves, and uses
     the AI assistant;
   - **observer**: read-only.
2. **Preview.** `invite_person` without `confirmed`, and show what it says:
   who, the email, the role in plain words, and whether that email already
   has an account (nothing to send) or an open invitation (it is sent again
   with a fresh link).
3. **Send** with `confirmed: true` once the administrator approves the
   preview.
4. **What the person receives**: an email from the project account with a
   «Create my account» button. For 7 days the link opens straight away; after
   that it first emails them a 6-digit code. They choose a username and
   password, and then sign in with that username or their email. If the
   email did not go out, the answer has the link: the administrator can send
   it by WhatsApp, and it works the same way.

`list_invitations` gives each invitation's status: pending, expired (the
link asks for a code), used or revoked, and how often an expired link was
opened. Revoking is done in the app, Usuarios (`#/usuarios`).
