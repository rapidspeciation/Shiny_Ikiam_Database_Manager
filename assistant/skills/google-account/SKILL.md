---
name: google-account
description: The Ithomiini project's Google account (Gmail, Drive, Docs, Sheets, Slides, Calendar, Forms) through the gog command, read-only unless the person explicitly asks for a write. Use it when the person asks about an email or calendar event of the project, a Drive file that is not among the mirrored project documents, or asks to send an email, create an event, or edit or share a file.
---

# The project's Google account (gog)

```sh
set -a; . ~/.config/ithomiini/gog.env; set +a
gog --readonly --account jmithominii@gmail.com --client ithomiini <service> <command>
```

Services: gmail (search/get), drive, docs, sheets, slides, calendar, forms,
appscript (`gog <service> --help`). Always `--readonly`, which blocks every
change. Drop it only for a write the person explicitly asked for (send an
email, create an event, edit or share a file), for that command only, after
showing them the text. Meeting notes, protocols, reports and presentations
are quicker through the document tools (`search_knowledge`); the workbook only
through the `ithomiini` tools.
