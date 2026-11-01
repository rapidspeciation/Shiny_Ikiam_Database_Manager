# Header at medium widths, «Más», theme button, menus that close (1 Nov 2026)

Branch `ui-header-20261101` from main d265a2c. Not pushed.

## What changed
- **Tab row (AppHeader.vue).** Below 1024 px the tabs have their own full-width row under the title and buttons. From
  1024 px everything is on one row. The row no longer scrolls sideways: it shows as many tabs as fit, measured from an
  invisible copy of the row, and the rest move into «Más ▾» (`fitTabs`). The open tab is never moved. On phones the
  title text hides (the butterfly stays) and the user name truncates past 10rem, with the full name on hover.
- **Hidden tabs.** `HIDDEN_TABS` in `frontend/src/lib/headerTabs.ts` is one list to edit:
  Colecta, Tubos, Emergidos, Clutches, Censo, Revisión. These tabs sit in «Más» below a line, after any tabs that did
  not fit. While a hidden tab is open it shows in the row in its usual place. The routes are unchanged, so deep
  links such as #/colecta still work. The amber «waiting» counts for Emergidos, Clutches and Censo appear in the menu,
  and their total appears on «Más», so changes waiting in those tabs are not hidden.
- **«Más» menu.** It is a disclosure button (`aria-expanded`, `aria-controls`) over a list of links. ↓ on the button
  opens it and focuses the first link. ↑/↓/Home/End move through the links, and Tab works too. Escape closes it and
  returns focus to «Más». The menu keeps 8 px from both screen edges. Focus rings use the normal `--focus` colour
  inside the menu and white on the green bar.
- **Theme.** One icon button in the bar, between the database badge and EN/ES. Each press goes to the next choice:
  System → Light → Dark (`nextPref` in lib/theme.ts). Its title and aria-label read «Tema: Sistema (pulsa para
  Claro)», or «Theme: System (press for Light)» in English. On touch screens its hit area is 44 px (`hit-target`). The
  theme choice was removed from the user menu.

## Dropdowns checked (item 4)
The shared helper is `frontend/src/composables/useDismiss.ts`: `useDismiss(open, insideEl, { trigger?, closeOn? })`.
While a menu is open, it closes the menu on:
- a pointerdown outside it (capture phase, so a grid that stops the event cannot block it);
- focus moving outside it;
- a click into an iframe (the window loses focus to the iframe, which is the Asistente's T3 frame);
- Escape, which also returns focus to `trigger`, or to whatever had focus when the menu opened;
- `closeOn` changing (the route).

The old user menu already closed on outside clicks inside the page; a browser check showed this before any change.
The likely failing case was a click into the embedded T3 frame, because iframe clicks never reach the page. That case
now closes the menu (checked in the browser with an injected iframe).

| Dropdown | Before | Now |
|---|---|---|
| Header user menu (name) | outside click and Escape; not iframe clicks, focus leaving or route change | useDismiss + route change |
| Header «Más» (new) | — | useDismiss + route change |
| Buscador ⋯ tools menu (phones, TablesView) | outside click only | useDismiss (Escape returns focus to ⋯) |
| Photo_links «Tarjetas recientes» (PhotoLinksPanel) | closed only by picking a card | useDismiss |
| ChoiceField, DateField popups | capture-phase outside click + Escape already | unchanged |
| monitoring/FilterSelect | focusout + Escape already | unchanged |
| IdPicker, Tubos and Muertes suggestion lists | tied to the input's focus (close on blur) | unchanged |
| `<details>` (17, e.g. WorkbookWarnings, UsersView) | in-page disclosures, not floating menus | unchanged |

Merge note: TablesView.vue is also edited on `ui-rowform-proto`. My change touches line 3 (the import) and the
⋯-menu listener lines (around 188–196), plus one added import after `usePhoneWidth`. None of those overlap their
hunks.

## Proof
- Tests: `useDismiss.test.ts` covers outside click, a stopped click, Escape with focus back, focus leaving, route
  change, listeners off once closed, and focus going back to the opener. `headerTabs.test.ts` covers the hidden
  config, an open hidden tab, the fit, and the theme cycle. 81 files and 731 tests pass; typecheck and build pass.
- Shots are in `~/.local/share/ui-overhaul-20261031/header-shots/`:
  - `before-20261101T1`: the current build.
  - `after-20261101T3`: the final version.

  Each covers #/inicio, #/muertes and #/colecta, plus «Más» open on #/muertes, at 390/640/768/1024/1280/1440, ES and
  EN, light and dark. That is 72 views and 24 open-menu shots.
- Script check (`shoot-header.mjs`): in the header, no element overflows or scrolls sideways, the page has no
  horizontal overflow, and no tab is cut off.
  - Before: 60 of 72 views failed, including 1280 (Revisión cut off) and every width below that.
  - After: 0 of 72 fail, and Escape on «Más» closes the menu and returns focus in all 24 cases.
- `dismiss-check.mjs` (1280 and 390): the user menu and «Más» close on an outside click, on Escape (with focus
  back) and on a route change. The user menu closes on a click inside an iframe. Buscador ⋯ closes on Escape.
- Tabs visible in the row:

  | Width | Tabs in the row |
  |---|---|
  | 390 | Inicio, Buscador, Monitoreo, Muertes |
  | 640–768 | all 7 main tabs |
  | 1024 | 6 (Asistente moves into «Más») |
  | 1280 and wider | all 7 |
