---
name: edit-instructions
description: Edit the assistant's own instructions (AGENTS.md and the skills, shown in the app as «AI instructions») when the team changes how it records something or settles something new. Use when the person asks for a value or a way of recording that differs from what the rules say (e.g. the rules say Sex `NA` and they ask for `NOT_COLLECTED`), says "from now on…", "we decided…", "that's not how we do it", or corrects the same thing twice. Covers asking whether it is a lasting rule, where the rule lives, and how to write it so a new chat applies it.
---

# Changing the team's rules

The rules in `AGENTS.md` and the skills are what every new chat knows about
how the team works. When the team changes its mind, the rule has to change
too, or the next chat repeats the old way.

## 1. Ask before changing a rule

When a request contradicts a rule, or brings one that is not written
anywhere, say what the rule says now and ask which case it is:

- **A change from now on**: the rule is rewritten.
- **A new rule**: it is added where it belongs.
- **This time only**: an exception; the rule stays and nothing is saved.

Also ask what happens to the rows already in the sheet: they stay as they are,
or they are corrected (that is a separate proposal, made with the usual
tools). Do the data change the person asked for either way.

## 2. Find where the rule lives

Search `assistant/` in the source checkout (skill **app-dev**: where it is,
how to commit and deploy), e.g. `grep -rn "NOT_COLLECTED" assistant/`. A rule
can also be built into the app: a tool that fills a value (`match_notebook`,
the tabs' forms) or a check in Revisión. Search `server/` and `frontend/src`
too; changing those is an app change (skill **app-dev**).

## 3. Write it for a chat that has not seen this one

The reader is a new chat, and the team, who read these files in the app
(«Instrucciones de la IA»). Neither has this conversation.

- State the rule as it is now: "Preserved larvae and eggs: Sex
  `NOT_COLLECTED`." Add the reason when it helps decide a case the rule does
  not name ("the sex cannot be seen at that stage").
- Replace the old rule in its place. Two versions of a rule in two files
  confuse the next chat more than none.
- Leave out how it came about: who decided, when, "as discussed", "the new
  rule", "unlike before". Old rows that differ get one plain sentence if they
  matter ("older rows with `NA` stay as they are").
- Plain words and the sheet's own names (columns, list values). An example
  from real rows when the rule is easy to misread.
- No capitals or "never"/"always" for emphasis, and nothing about mistakes
  people made: just the rule.
- Short: the smallest change that makes the rule right.

Then read the edited part as a new chat would: could it apply the rule to a
case that is not in this conversation, without guessing?

## 4. Show it, then save it

Show the person the old and the new text side by side and change it until
they agree. Then commit and deploy as skill **app-dev** says, with a commit
message that names the rule ("data-rules: preserved larvae Sex
NOT_COLLECTED"). New chats read the new text after the deploy; this chat keeps
the version it started with, so follow the new rule here from now on.
