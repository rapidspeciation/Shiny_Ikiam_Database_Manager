# Proposing instruction changes from outside T3 (lab)

An outside proposer, such as the assistant-eval chat, can load changes to the assistant's instructions into a lab
instance. They appear as «cambio de documento» cards on `#/instrucciones`, where Franz applies, discards or comments
on each, as he would with Claude's own. The proposer reads his comments and answers with a new version. Nothing here
reaches the live app.

- **Address:** `http://127.0.0.1:8841/api/ai/mcp` (MCP over HTTP, JSON-RPC `tools/call`). The page is
  `http://127.0.0.1:8841/#/instrucciones`, or https://franz-gpdwin4.tailf5d2f1.ts.net:9921/#/instrucciones on the
  tailnet.
- **Token:** `~/.cache/ithomiini-lab/doc-comments-20261108/auditor-token.txt` (mode 600), sent as
  `Authorization: Bearer <token>`.
  - It belongs to the lab admin `auditor` («Auditoría»).
  - Its label is `external`. That makes the app treat it as a proposer with no T3 chat: its cards are never linked
    to a chat and no lab chat is ever started for them.
  - Its changes and replies carry the Claude tag, like Claude's. The name «Auditoría» is never shown on a card.

## Loading changes

```sh
node tools/lab/doc-proposals-import.mjs proposals.jsonl --dry-run   # checks only, proposes nothing
node tools/lab/doc-proposals-import.mjs proposals.jsonl             # one card per line
```

Each line is a JSON object:

```json
{"path": "assistant/skills/weather/SKILL.md", "section": "The site", "before": "stands in for", "after": "gives the forecast of", "reason": "Estilo: una frase más clara."}
```

| Field | Meaning |
|---|---|
| `path` | A Markdown file under `assistant/`. |
| `section` | The heading's text, at any level: `The site` and `### The site` mean the same. Use `''` for the text before the first heading, or `description` for the front matter's description (below). |
| `before` | Exact text from that section's current source: a part of it, or the whole section. |
| `after` | Its replacement. |
| `reason` | Optional. One short line, shown as the card's first Claude comment. |
| `thread_ids` | Optional. The comment threads the change answers. |

The script prints one line per change: `line N: ok <change id> v1`, or `line N: refused: <the tool's message>`. The
exit code is 1 if any line was refused. `--url` and `--token-file` override the defaults.

### Sections

- **A heading at any level.** A section runs from its heading to the next heading of the same or a higher level, so
  a `##` section includes its `###` subsections. Headings inside code fences don't count.
- **The same heading twice in one file.** The change goes to the one section whose text contains `before`. If
  `before` is in none of them, or in more than one (nested sections both contain it), the change is refused:
  «N sections are named "X" in <file>: make before unique to one of them».
- **`description`.** The section name `description` (any case) stands for the value of the front matter's
  `description:` line in a skill's `SKILL.md` or a subagent's file. That line is the skills listing every chat
  carries.
  - `before` and `after` match inside that value only, which stays one line and not empty. `name:` and the rest of
    the front matter can't be reached.
  - The card is titled «Descripción» and shows the same word diff.
  - The size check measures the skill's listing (`skill:<name>`), so a longer description needs «Aplicar
    igualmente».
  - A file that also has a heading named «description» refuses it, because the two can't be told apart.
  - Example:

    ```json
    {"path": "assistant/skills/weather/SKILL.md", "section": "description", "before": "for today and up to a week ahead, ", "after": "", "reason": "Recorte: el listado de habilidades."}
    ```

### When a change is refused

- **`before` doesn't match:** it must appear exactly once in the section's current source, which is the text in
  effect, including changes already applied in the app. Copy it from `doc_threads` (export) or from the file, not
  from memory.
- **Wrong file:** only `assistant/**/*.md` takes changes, and the file must exist.
- **Front matter:** only the `description:` value can change (section `description`). `name:` and the front
  matter's shape are locked, and the checks below verify them.
- **The checks of `assistant/`:**
  - a skill's front matter must stay valid
  - AGENTS.md may contain no unknown `{{placeholder}}`
  - app-guide must still link its reference files, and every `#/route` it names must exist
- **Size growth (accepted, not refused):** a change that grows a part of the chats' fixed context past its recorded
  size (the brief, a skill's listing) shows «Crece N bytes: …» on the card. Its main button becomes «Aplicar
  igualmente». The import prints `(grows: …)` for it.

## Reading Franz's decisions and comments

```sh
node tools/lab/doc-proposals-export.mjs                       # doc_threads as JSON: every outside change and open thread
node tools/lab/doc-proposals-export.mjs --pending-revisions   # only the changes waiting for a new version
node tools/lab/doc-proposals-export.mjs --status applied      # pending, applied, undone or discarded (--path <file> too)
```

`doc_threads` answers in pages within the tool answer's size limit: `total` (after the filters), `offset`, and while
there is more, `nextOffset`. Its filters (`status`, `revisionRequested`, `path`) run in the app before the page is
cut, and the threads come with the first page. The export asks for every page and checks it read `total` changes.

Each change has its `status`, its `comments`, and `revisionRequested`:
- `status` is `pending`, `applied`, `undone` or `discarded`.
- `comments` lists who wrote each one and when; `by: "Claude"` marks the proposer's own lines.
- `revisionRequested` is true while a new version is wanted. `commentsSinceVersion` then holds Franz's comments made
  since the current version.

## Revisions

1. **Franz asks.** On a card he writes a comment and presses «Pedir versión nueva», or on an out-of-date card
   «Pedir a Claude que la rehaga».
   - The request is recorded and no chat is started.
   - The card shows «Esperando nueva versión», and Aplicar is disabled until the answer comes.
   - The page's «Pedir a Claude» never sends these cards or their comments to a lab chat.
2. **The proposer answers**, after reading the export:
   - with a new version: `revise_doc_change` with `{ change_id, after }`. Add `before` when the section changed; it
     defaults to the last version's. v2 replaces v1 on the card, and v1 stays one click away.
   - or with a reply: `reply_doc_thread` with `{ id: change_id, text }`, for example why nothing changes.
   - Either one clears `revisionRequested`.
3. **Writing rules** for the text, the same as skill edit-instructions step 3: state the rule or fact as it is now,
   with the smallest change. No history («we decided», «new rule»), nothing about past mistakes, no capitals or
   never/always for emphasis.
