# WhatsApp relay (lab prototype)

The app server relays WhatsApp messages to one shared «WhatsApp» T3 chat and takes its answers
back. The design is in `.work/whatsapp-assistant-20261030/bridge-design.md`. This prototype runs
in the lab only, and its answers go to an outbox file. **Nothing is sent to WhatsApp.**

## Parts

| Part | Where |
|---|---|
| Relay: signature, duplicates, whitelist, group rules, rate limits, batching, one turn at a time | `server/whatsapp.mjs` |
| Routes `/api/wa/*`, the T3 glue, the CAM lookup | `server/index.mjs` (`createWhatsApp`, `whatsappRoute`) |
| `whatsapp_reply` MCP tool (listed only for the service user's token, which also loses `apply_proposal`) | `server/whatsapp.mjs` (`REPLY_TOOL`, `mcp`) |
| The assistant's guide for WhatsApp | `assistant/skills/whatsapp/SKILL.md` (every workspace gets it through `scripts/t3-provision.mjs`) |
| Tests | `tests/whatsapp.test.mjs` (`node --test tests/whatsapp.test.mjs`) |
| Simulator | `tools/whatsapp/simulate.mjs`, examples in `tools/whatsapp/examples/` |

## How a message goes

1. wacli (later) or the simulator POSTs `/api/wa/inbound`. The body is signed with
   `X-Wacli-Signature: sha256=<HMAC-SHA256 of the raw body>`.
2. The relay drops duplicates (by message id) and checks the whitelist (`wa_people`, `wa_groups`).
   Strangers and unknown groups are logged without their text and get no answer. An unknown group
   is added as *pending* (disabled).
3. In groups, these wake the chat: a mention of a bot JID (`WA_BOT_JIDS`), a quote of a bot answer,
   the bot's name (`WA_BOT_NAME`, default `\b(ithobot|asistente|assistant)\b`) used to call it, or a
   `/command`. The name counts as «@asistente …» anywhere, or as a vocative at the start
   («Asistente: …», «oye asistente, …») or after a comma at the end («…, asistente?»). A name that goes on
   («@Asistente de campo A») or the word inside a sentence («la asistente llega mañana») does not wake it.
   In `helpful` groups two more things count as candidates:
   - a CAM whose genus or sex disagrees with the database, or a CAM it does not have;
   - a protocol question that tags nobody and that no teammate answered within 20 min.

   Candidates are limited to 3 a day per group and never sent between 21:00 and 06:30 Ecuador time.
   Every other group message is kept as context for the next envelope.
4. Rate limits: 20 handled messages an hour and 80 a day per number (one notice, then silence),
   150 wakes a day in total (above that only admins' DMs go through), and 40 answers an hour.
5. In the envelope, a person's line that follows their own line in the same chat within 5 minutes is
   marked `(goes on from #m33)`, so the chat reads the two as one message.
6. A chat's messages are sent 20 s after its last message, and no later than 60 s after its first,
   as one envelope (`[WA] N new`, `#m12 DM …`). The envelope goes to the service user's T3 chat
   «WhatsApp» (Sonnet 5.5). The relay makes that chat on the first wake and provisions the
   `whatsapp` user's T3 project if it is missing. Only one turn runs at a time; messages wait while
   the chat is answering (`server/t3tell.mjs`).
7. The chat answers with `whatsapp_reply({ replyTo: "#m12", text, file?, confirm? })`. A reply to a
   chat with no message in the current turn returns a warning unless `confirm: true`.
8. Admins can DM `/add +593… Name` or `/remove +593…`. The relay handles these itself, without a
   model call.

Everything is logged beside the app database:
- `wa_inbox`: every message, its decision and reason. Text is kept only for whitelisted chats.
- `wa_outbox`: every answer.
- `wa_turns`: the turns.
- `wa-outbox.jsonl`: the answers, which is what a test harness reads.
- `wa-relay.log.jsonl`: everything in and out, including the envelopes.

In the lab these files are in `~/.cache/ithomiini-lab/app-workbook/` (or `app/` in offline mode).
`/health` shows the relay's counters.

## Settings (environment)

| Variable | Meaning |
|---|---|
| `WA_WEBHOOK_SECRET_FILE` | Turns the relay on. The HMAC secret (≥ 16 chars, file mode 600). The lab makes `~/.cache/ithomiini-lab/wa-webhook-secret` |
| `WA_SENDER` | `outbox` (default): answers are only written down. `wacli`: also `wacli send text/file` (**future; off**) |
| `WA_WACLI_BIN`, `WA_WACLI_ACCOUNT` | For the wacli sender (e.g. `--account ithomiini`) |
| `WA_SERVICE_USER` | The app user the chat's tools run as (`whatsapp`, made once, cannot sign in) |
| `WA_BOT_JIDS`, `WA_BOT_NAME` | The bot's JIDs/LIDs (comma-separated) and the name regex |
| `WA_LAB=1` | Lets the simulator set the whitelist (`POST /api/wa/setup`), and only when the workbook is not the team's |
| `WA_DEBOUNCE_MS` | The 20 s quiet time, for experiments |
| `WA_QUESTION_WAIT_MS`, `WA_QUIET_HOURS=0`, `WA_SENDS_HOUR` | Lab only (`WA_LAB=1`): a shorter wait for unanswered questions, no quiet hours and a higher hourly answer limit, so the simulator can run the unprompted cases at any time |

`tools/lab/app.sh` sets the secret file and `WA_LAB=1`, and unsets `WA_SENDER`. It also adds the
secret to the paths the lab chats may not read.

## Simulator

```sh
# The lab app from this checkout (tools/lab/README.md), then:
node tools/whatsapp/simulate.mjs tools/whatsapp/examples/cases.jsonl /tmp/wa-sim-$(date +%Y%m%dT%H%M%S) \
  --setup tools/whatsapp/examples/setup.json --fresh-thread --timeout 420
```

- A case looks like this:
  `{ id, chat: "dm" | "<group>@g.us", chatName?, sender: "+593…", senderName?, preceding?: [{ sender, senderName?, text }], message, image?: "/abs/path.jpg", quoted?: "out:<n>", mentions?: [jid] }`.
  `preceding` messages are posted first, one minute apart.
- `--setup` loads the whitelist, and `--fresh-thread` starts a new «WhatsApp» chat on the next wake.
- Every message of a case is followed, the preceding ones too (one of them may wake the chat).
- Output, in a new, empty folder: `results.jsonl`, one line per case with `{ id, decision, reason, outcome, reply | "no reply", replies, proposalLinks, seconds }`, and `run.json`.
- The simulator waits for each case to finish: a final decision (dropped, context, limited) or the
  end of the chat's turn.
- It runs only against `http://127.0.0.1:8795`, and only when the relay there says it is the lab
  and not on the team's workbook. It signs with the lab's secret and never prints it.

## What is stubbed or simple

- **No WhatsApp transport.** wacli is not run, and nothing is sent. The wacli sender exists behind
  `WA_SENDER=wacli`, untested. wacli's exact webhook field names must be checked against a recorded
  body; `normalizeEvent` accepts the likely snake_case and Go names.
- **LIDs.** LID→phone mapping is only a `lid` column in `wa_people`. Nothing reads whatsmeow's
  mapping, and there is no "unknown member" admin view.
- **No admin page «WhatsApp».** The whitelist is set through `/api/wa/setup` (lab) or `/add` /
  `/remove` DMs.
- **Candidates.** The CAM check compares genus and sex only (one regex on capitalised words against
  the database's genera). The protocol question is a word list and question regex. It waits 20 min
  per message rather than going out in a 20-min digest.
- **Proposals belong to the `whatsapp` user.** `propose_changes` has no `forMessage` attribution
  yet. The skill asks the chat to start the title with "WhatsApp, <name>:".
- **No proposal screenshot.** The relay does not render one. The chat sends the link; `file` can
  carry any picture the chat already has.
- **No catch-up after a restart.** Nothing is read from wacli's store after a restart. Rate-limit
  counts survive (they are in SQLite), and a turn left open is closed by the next pump.
- **None of these yet:** the weekly chat rotation, the media retention clean-up, the bridge
  disconnect alerts or the Haiku classifier.
