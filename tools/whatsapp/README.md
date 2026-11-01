# WhatsApp: the live bridge and the relay prototype

Two pieces live here. **The live bridge** (31 Oct 2026, first section) runs on Franz's own WhatsApp
and answers in two chats. **The relay** (the rest of this file) is the earlier lab prototype inside
the app server, for a future project number; it sends nothing.

## The live bridge (`bridge.mjs`)

| Part | Where |
|---|---|
| Receiver: signature, chat filter, loop guard, commands, @claude trigger, hourly limit, one turn at a time per chat, media, voice, sends | `tools/whatsapp/bridge.mjs` (`serve`; `install` copies it to `~/.local/share/ithomiini-wa-bridge` and writes `ithomiini-wa-bridge.service`) |
| Pure parts (trigger, commands, reply blocks, step lines, media decryption) | `tools/whatsapp/bridge-lib.mjs` |
| One T3 turn: send, wait, last message (and tool steps as they start) | `tools/whatsapp/t3-turn.mjs` (copied to claudeclaw's `~/ithomiini/work/wa-bridge/`) |
| The chats' guide | `tools/whatsapp/bridge-skill/SKILL.md` (copied into both workspaces' `.claude/skills/whatsapp-bridge`) |
| Simulator for dry-run chats | `tools/whatsapp/bridge-sim.mjs` |
| Config shape | `tools/whatsapp/bridge-config.example.json` (the real one: `~/.config/ithomiini-wa-bridge/config.json`, 600) |
| Tests | `tests/whatsapp-bridge.test.mjs` |

**Path.** `wacli-sync.service` runs `sync --webhook http://127.0.0.1:8789/wacli --webhook-secret … --webhook-allow-private`
(secret from `~/.config/ithomiini-wa-bridge/wacli-webhook.env`). Every live message (Franz's own
too: wacli does not filter `FromMe`) is posted, signed with HMAC-SHA256 in `X-Wacli-Signature`.
A message wacli itself sends is not posted back.

**Order, per message.** Bad signature → 403. A chat not in the config (or disabled) → dropped, and the
log keeps only that a message was dropped. Then: duplicates, reactions, edits, deletions and messages
older than 15 min are skipped; text starting with «Claude:» is the bridge's own (loop guard); in the
self-chat only Franz's messages count. `/help`, `/status`, `/whoami`, `/usage` are answered without a
model; `/stop` and `/start` (Franz only) switch everything off and on (the `stopped` flag in
`~/.local/state/ithomiini-wa-bridge/state.json`); `/ack reaction|edit` (Franz only) chooses how a chat
shows it is working. Anything else needs `@claude` as a whole word, and the chat's hourly limit
(group 20, self-chat 60 answers; one notice when reached).

**A turn.**
- How it shows it is working (`ack`, per chat):
  - `reaction`: 👀 on the message while working, ✅ when Claude replied, ☑️ when it woke but stayed silent (⚠️ on failure). The emojis are the config's `reactions`: { working, replied, silent, failed }.
  - `edit`: one «Claude: pensando…» message that lists the chat's tool steps (tool names, never its reasoning) and is edited into the answer.
- What the chat gets: the group's last 15 messages from wacli's store, the message, and its media:
  - the message's own, the quoted message's, and the sender's media of the last 3 minutes;
  - media are downloaded from WhatsApp's CDN with the store's media key and decrypted, so the running sync is never interrupted;
  - photos are copied into the route's T3 attachments folder, where `identify_species` finds them by name;
  - voice notes are transcribed: ElevenLabs Scribe v2 with the genera as keyterms, faster-whisper `small` as fallback.
- Where it goes:
  - the group: the chat «Ithomiini 🦋 (underground) · WhatsApp» in the claudeclaw T3 project of the same name, over ssh;
  - the self-chat: «Orchestrator wpp» in this PC's T3, project «Ithomiini lab · WhatsApp» (the lab workspace, the lab app's tools).
- **Reply path.** The bridge waits for the turn and sends the chat's last message, starting with «Claude: ».
  - `[files]` … `[/files]` lines (`<path or app rack-report link> | caption`, up to 3) become documents (`wacli send file --as document`), one after another, through send()'s group guard. Only files under the route's `rackReportsDir` (and `fileDirs`), by real path (symlinks and `..` resolved; over ssh with `realpath` on the route's host), of type xlsx/csv/pdf/docx/txt/png/jpg/gpx/kml, at most 25 MB, written in the last 3 hours; anything else is refused and logged (`refused`).
  - `[images]` … `[/images]` lines (`<path or https URL> | caption`) become up to 5 pictures, each at most 2048 px on its long side as JPEG. wacli 0.22 has no HD flag; it uploads the file as given. Every picture's first bytes are checked to be an image.
  - `[voice]`, or a voice note in the question, adds an Edge TTS voice note (es-EC / en-US).
  - `[silent]` sends nothing.
- **Hard block.** `send()` sends with wacli only to the config's group and self-chat JIDs, whatever else the config says. Dry-run chats write to `outbox.jsonl` instead.

**The group's workspace on claudeclaw** (`shared/t3-workspaces/wa-underground`, app user `wa-underground`,
editor, no password; made by `t3-provision.mjs`, kept by `--refresh-all`).
- Its `.claude/settings.json` denies `Bash`, `Edit`, `Write`, `NotebookEdit` and the app tools that write without review: `apply_proposal`, `undo_edits`, `invite_person`, `list_invitations`, `organize_photos`, `sync_photo_links`, `update_websites`, `queue_walk`, `rename_photos_edit`, `drive_photos_edit`.
- Proposals are drafted with the link and applied in the app by a person.

**Voice and `/tono`.** `assistant/voice/whatsapp.md` (the WhatsApp header and `/tono`) and `assistant/voice/core.md`
(shared with the app's assistant) are the chats' voice: tone only, the skill keeps the rules on facts, evidence and
the group. With `soulFile` in the config (a file or a list, read in order), the bridge puts it in a thread's first envelope, again when
the file changes and every 20 turns. Every envelope names the chat's humour level, `Tone in this chat (/tono): …`.
`/tono normal | gracioso | picante` (anyone in the chat, with `@claude` in the group) sets it per chat, without a model;
`/tono` alone shows it. Normal is the default (`chats.<key>.tono` changes it).

**Logs.** `~/.local/state/ithomiini-wa-bridge/bridge.log.jsonl`: each message's decision, each send
(kind, length, ok), each turn (time, steps). No message text; dropped chats appear without id or sender.


The app server relays WhatsApp messages to one shared «WhatsApp» T3 chat and takes its answers
back. The design is in `.work/whatsapp-assistant-20261030/bridge-design.md`. This prototype runs
in the lab only, and its answers go to an outbox file. **Nothing is sent to WhatsApp.**

## Parts

| Part | Where |
|---|---|
| Relay: signature, duplicates, whitelist, commands, group rules, rate limits, batching, one turn at a time | `server/whatsapp.mjs` |
| The Haiku gate (off by default, see below) | `server/whatsapp-gate.mjs` |
| `/usage`: the Claude account's use | `server/claude-usage.mjs` |
| Routes `/api/wa/*`, the T3 glue, the CAM lookup | `server/index.mjs` (`createWhatsApp`, `whatsappRoute`) |
| `whatsapp_reply` MCP tool (listed only for the service user's token, which also loses `apply_proposal`) | `server/whatsapp.mjs` (`REPLY_TOOL`, `mcp`) |
| The assistant's guide for WhatsApp | `assistant/skills/whatsapp/SKILL.md` (every workspace gets it through `scripts/t3-provision.mjs`) |
| Tests | `tests/whatsapp.test.mjs` (`node --test tests/whatsapp.test.mjs`) |
| Simulator | `tools/whatsapp/simulate.mjs`, examples in `tools/whatsapp/examples/` |
| Gate calibration | `tools/whatsapp/calibrate-gate.mjs` |

## How a message goes

1. wacli (later) or the simulator POSTs `/api/wa/inbound`. The body is signed with
   `X-Wacli-Signature: sha256=<HMAC-SHA256 of the raw body>`.
2. The relay drops duplicates (by message id) and checks the whitelist (`wa_people`, `wa_groups`)
   before anything else. A message is kept only from an enabled person, in a DM or in an enabled
   group. Everyone else (strangers, people not on the list writing in an enabled group, any message
   in a disabled or unknown group) is logged without text, name, quote or media, and gets no answer:
   no gate, no command and no model call. An unknown group is added as *pending* (disabled).
3. DMs starting with `/` are answered by the relay, without a model:
   - `/usage`: the 5-hour and weekly use of the Claude account the server runs on, with the reset
     times in Ecuador time. It reads the account's `~/.claude/.credentials.json` on the server, as
     the orchestrator's `claude_usage.py` does, and shares its 3-minute cache
     (`/tmp/claude_usage_cache.json`). The token never leaves that one request.
   - `/status`: the connection (outbox only, or wacli and the last message's age), the queue, the
     last turn, the answers sent today, the gate and the chat's model.
   - `/help`: the commands. Any other `/word` gets one line naming them.
   - Admins also have `/add +593… Name` and `/remove +593…`. For anyone else these get a line saying
     they are for admins.
   In English, or in Spanish for people whose `language` is `es`.
4. In groups, these wake the chat: a mention of a bot JID (`WA_BOT_JIDS`), a quote of a bot answer,
   the bot's name (`WA_BOT_NAME`, default `\b(ithobot|asistente|assistant)\b`) used to call it, or a
   `/command`. The name counts as «@asistente …» anywhere, or as a vocative at the start
   («Asistente: …», «oye asistente, …») or after a comma at the end («…, asistente?»). A name that goes on
   («@Asistente de campo A») or the word inside a sentence («la asistente llega mañana») does not wake it.
   In `helpful` groups, the other lines go to one of two filters:
   - **Rules** (the default): a CAM whose genus or sex disagrees with the database, or a CAM it does
     not have; a protocol question that tags nobody and that no teammate answered within 20 min.
   - **The Haiku gate** (`WA_GATE=haiku`): one `claude -p` call on Haiku 5.5 per line, with no tools,
     MCP, settings or CLAUDE.md, in an empty folder. It sees a short prompt, the group's last 8 lines
     and the line, and answers `{wake, reason}`. A protocol question it passes still waits 20 min for
     a teammate. When the call fails, or past 600 calls a day, the rules decide. It costs about
     $0.0003 and 3 s a line.
     The calibration on 60 labelled group cases (30 Oct 2026) did not pass the bar set before the
     run (wake recall ≥ 95 % on should-reply, false wakes ≤ 15 %): recall 95–97 %, false wakes
     23 %. The rules got recall 55 % and false wakes 9 %. So the gate stays off by default; turn it
     on to try it (`WA_GATE_DEFAULT` in `server/index.mjs` switches the default).

   Candidates are limited to 3 a day per group and never sent between 21:00 and 06:30 Ecuador time.
   Every other group message is kept as context for the next envelope.
5. Rate limits: 20 handled messages an hour and 80 a day per number (one notice, then silence),
   150 wakes a day in total (above that only admins' DMs go through), and 40 answers an hour.
6. In the envelope, a person's line that follows their own line in the same chat within 5 minutes is
   marked `(goes on from #m33)`, so the chat reads the two as one message. When a line wakes the chat,
   the same chat's earlier lines that are still waiting (an unprompted question's 20 min) go in the
   same envelope. `whatsapp_reply` warns before a second answer to one person's message.
7. A chat's messages are sent 20 s after its last message, and no later than 60 s after its first,
   as one envelope (`[WA] N new`, `#m12 DM …`). The envelope goes to the service user's T3 chat
   «WhatsApp» (Opus 5.5 at medium effort; `WA_MODEL`, `WA_EFFORT`). The relay makes that chat on the first wake and provisions the
   `whatsapp` user's T3 project if it is missing. Only one turn runs at a time; messages wait while
   the chat is answering (`server/t3tell.mjs`).
8. The chat answers with `whatsapp_reply({ replyTo: "#m12", text, file?, confirm? })`. A reply to a
   chat with no message in the current turn returns a warning unless `confirm: true`.
9. Photos from people on the list are copied into `wa-media/<date>/m<n>.<ext>` beside the database
   (mode 600) and the envelope gives that path. `identify_species` takes the path (only inside that
   folder), and the skill has the chat answer with the first species when the identifier is sure
   (60 % or more) and the first two or three with their % when it is not.

Everything is logged beside the app database:
- `wa_inbox`: every message, its decision and reason. Text is kept only for whitelisted chats.
- `wa_outbox`: every answer.
- `wa_turns`: the turns.
- `wa-outbox.jsonl`: the answers, which is what a test harness reads.
- `wa-relay.log.jsonl`: everything in and out, including the envelopes, the gate's decisions and costs.
- `wa-media/`: the photos.

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
| `WA_MODEL`, `WA_EFFORT` | The chat's model (`claude-opus-5-5` by default, or `claude-sonnet-5-5`) and effort (`medium`; `low`, `high`). Read when the chat is made |
| `WA_GATE` | `rules` (default) or `haiku` |
| `WA_CLAUDE_BIN` | The `claude` the gate runs (`claude`) |
| `WA_USAGE=0` | Turns `/usage` off |
| `WA_MEDIA_DIR` | Where photos are kept (`wa-media` beside the database) |
| `WA_DEBOUNCE_MS` | The 20 s quiet time, for experiments |
| `WA_QUESTION_WAIT_MS`, `WA_QUIET_HOURS=0`, `WA_SENDS_HOUR` | Lab only (`WA_LAB=1`): a shorter wait for unanswered questions, no quiet hours and a higher hourly answer limit, so the simulator can run the unprompted cases at any time |

`tools/lab/app.sh` sets the secret file and `WA_LAB=1`, and unsets `WA_SENDER`. It also adds the
secret to the paths the lab chats may not read. It points `IDENTIFIER_DIR` at the lab's copy of the
identifier's files (`~/.cache/ithomiini-lab/identifier`, made as `tools/identifier/setup.sh` does)
and keeps the background photo readings off (`READINGS=0`, unless set).

## Gate calibration

```sh
node tools/whatsapp/calibrate-gate.mjs <new private folder> <cases.jsonl>... --repeats 2 --db ~/.cache/ithomiini-lab/app-workbook/app.sqlite
```

It runs the rules and the gate on each group case (DM cases are left out; `stranger` and
`new_unknown_member` cases count as dropped) and writes `items.jsonl` and `summary.json`: recall and
false wakes for the rules, the rule filter and each gate repeat, with the cost per call. Keep the
cases and the output out of the repo.

## Simulator

```sh
# The lab app from this checkout (tools/lab/README.md), then:
node tools/whatsapp/simulate.mjs tools/whatsapp/examples/cases.jsonl /tmp/wa-sim-$(date +%Y%m%dT%H%M%S) \
  --setup tools/whatsapp/examples/setup.json --fresh-thread --timeout 420
```

- A case looks like this:
  `{ id, chat: "dm" | "<group>@g.us", chatName?, sender: "+593…", senderName?, preceding?: [{ sender, senderName?, text }], message, image?: "/abs/path.jpg", quoted?: "out:<n>", mentions?: [jid] }`.
  `preceding` messages are posted first, one minute apart. Name a photo without its species: the
  chat sees its path.
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
- **None of these yet:** the weekly chat rotation, the media retention clean-up and the bridge
  disconnect alerts. `/status` reports the last message's age, not wacli's own connection state.
- **Group `/commands`** still wake the chat; the relay answers commands in DMs only.
