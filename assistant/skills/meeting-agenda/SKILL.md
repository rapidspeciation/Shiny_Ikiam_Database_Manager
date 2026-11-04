---
name: meeting-agenda
description: The weekly 🇪🇨🇬🇧 meeting (Thursdays 9:00 Ecuador): the topics to discuss and the proposed Agenda in the team's minutes format, and the proposal that creates the meeting's Google Doc from the template. Use it for «temas para discutir mañana en la reunión», «prepara la agenda propuesta para la reunión de mañana», «crea el documento de la reunión», «¿qué quedó pendiente de la última reunión?».
---

# The meeting agenda

How the team does it: on Wednesday the organiser (Patricio; Alex, Kim or Ale when he is away)
asks the underground group for points («¿me pueden enviar sus puntos para la reunión de mañana?»),
people answer there (in Spanish), and the organiser copies the minutes template in Drive
(«## Meeting-##/##/2026 - TEMPLATE», folder «Meetings»), names it «NNN Meeting-DD/MM/YYYY» and
writes the Agenda in English as a checklist. Each point is «Topic (Owner)»; then **Follow-ups:**
and **Other/various:**. The agenda is shared in «Ithomiini Ikiam» by a person, never by you.

## Two kinds of request

| Asked | You give |
|---|---|
| «temas para la reunión de mañana», «¿qué hay pendiente?» | Text in the chat (Spanish), grouped as below, each topic with what it is about and its owner. No proposal unless asked. |
| «prepara la agenda propuesta», «crea el documento de la reunión» | The agenda (English, the minutes' format) as a proposal with `meeting_doc_propose`, and in the chat: the `whatsapp` text it returns and the preview `link`. |

## Sources, in this order

1. `meeting_doc_context`: the meeting's date, number and title, and the last two minutes (`last`,
   `previous`: their file in the documents' copy). Read both files: the Agenda, the summary, and
   «Next week(s)» / «Follow-ups». A point of the last agenda with an empty summary was not discussed:
   it goes on again.
2. The WhatsApp groups since the day before the last meeting:
   - «Ithomiini (underground)»: its history file (wa-history, named in your message). The answers to
     «puntos para la reunión» are the agenda's core: every point someone sent goes in, in their words,
     with their name. Also questions left unanswered, decisions waiting for someone, announcements.
   - «Ithomiini Ikiam» (read-only history file, when your message names one): the UK side's messages,
     announcements (visits, shipments, grants), questions to the meeting. You never write there.
   If you have no history file (the app chat), say that the WhatsApp groups were not read.
3. The base: `review_issues` with show «alerts», and what the minutes' topics need from it (a count,
   a date). Only facts that change a decision; at most three.
4. Open proposals (`list_proposals`) that wait for a meeting decision, if any.

## Writing the points

- One point per topic, as the minutes write it: «Water for the new insectary (Ángel and Kim)».
  The owner is who sent the point or who the minutes name for it; never guess one (leave it out).
- Every point says what it is about. A topic you can only name («Infrastructure») gets the concrete
  question from its source («hose from the bus-washing tap instead of a pump: check the path (Ángel)»).
- Follow-ups: what the last minutes left for this week (Next week(s), unanswered agenda points).
- Other/various: announcements and small items.
- Species in italics with `_Methona_`. English, short, no adjectives, no "AI" commentary.
- `sources`: one line per point, «Ángel, underground 29-oct: points for the meeting» or
  «Minutes 143, Next week(s)». The person who applies it sees them.
- Nothing you cannot point to: if a source is thin, the point says less, it does not fill in.

## After proposing

Tell the person, in one or two lines: nothing is created yet; an administrator creates the document
by confirming here («sí, créalo») or with the button on the preview. Then the `whatsapp` text.

When an administrator's latest message explicitly confirms it, call `meeting_doc_apply` with the id.
It answers `docLink` and `picture`: send the picture to this chat with
`[images]` `<picture> | <title>` `[/images]` and the link in the text. For anyone else, or without an
explicit confirmation, do not call it. If it fails, say why (the document exists already, Drive).

Never post or offer to post the agenda in «Ithomiini Ikiam»: the person shares it there themselves.
