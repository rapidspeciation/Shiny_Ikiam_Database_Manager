# UI principles for the ithomiini app, learnt from T3 Code

31 October 2026. The checklist the UI audit scores against. Raw notes, the PR list and the full output of the contrast script are in `/home/franz/.local/share/ui-overhaul-20261031/t3-research-notes.md`. Every ratio below was computed by `/home/franz/.local/share/ui-overhaul-20261031/contrast.py` from the Tailwind v4 colour file the app ships with (oklch converted to 8-bit sRGB, translucent colours composited on the surface below them).

## 1. What T3 Code changed

T3 Code's "New design" landed between 25 and 27 October 2026 in five PRs, and a run of smaller fixes went in around them. The commits are in pingdotgg/t3code main (checked at `8b74a69`, 31 Oct).

| PR | Merge | What changed | Why it is better |
|---|---|---|---|
| [#24865](https://github.com/pingdotgg/t3code/pull/24865) UI design overhaul | `381629602` | A designer's verdict was "the fonts, the border radius, the icons, the font hierarchy". Four model reviewers scored fresh screenshots of real data against Claude Code and Codex for 18 rounds (7.1 → 9.6 of 10). Inter on Windows and Linux so 500 and 600 weights render. Radius base 10 → 8 px, scale 4/6/8/12/16. Icon strokes 2 → 1.75. Light text zinc-800 → zinc-900. Dark muted text and borders one step stronger. Read titles at 75 %, metadata one step below. "Snoozed 1 ›" instead of "Snoozed (1)". The composer rests at two lines, with one neutral control style and no divider lines. Dividers show on hover only. Empty drafts hide Push and "+0 −0". Settings titles 16 px semibold. One placeholder text everywhere. | Hierarchy comes from three text steps and two weights, not from boxes and lines. Less chrome, and nothing shown that cannot be used. |
| [#25251](https://github.com/pingdotgg/t3code/pull/25251) colour where it means something | `c3888da8b` | The grey pass was "quiet but flat, and some states were hard to read". New rule: structure neutral, identity in colour (project and agent icons), action and state in the one accent (switches, checkboxes, focus rings, Merge, NEW), meaning in semantic colour (added green, deleted red, modified a muted blue). True black dark canvas, with cards and popovers one tone lighter. Empty third lines dropped. Shelf headers look the same open or closed. A snoozed time gets a clock icon so "16h" is not read as an age. Tables 12 → 14 px. Model names 14 px, a check in a fixed column, the repeated provider line dropped, highlight stronger than selection. Keyboard focus on a picked tile is a neutral ring inside it with a gap, so focus never looks like a second pick. Loading skeletons get a caption. Light syntax colours darkened per hue until 4.5:1 on every code surface (some were about 2:1). Settings sections flat instead of cards inside cards. | Colour carries one meaning each, so it can be read at a glance. Focus and selection can no longer be confused. |
| [#25323](https://github.com/pingdotgg/t3code/pull/25323) sent messages on black | `7df7cb999` | On the new #000 canvas the 4 % white bubble came out #0a0a0a (1.06:1 against the canvas, darker than the composer). Now a solid #1e1e1e (1.26:1, the same tone as the selected row). | Translucent fills change when the canvas changes, so they must be measured again. |
| [#25324](https://github.com/pingdotgg/t3code/pull/25324), [#25778](https://github.com/pingdotgg/t3code/pull/25778) default on, then the only design | `f40c7c643`, `96fc7b955` | The toggle and the old look were deleted because "every redesigned element carried two styles". The launch splash, `theme-color` and window background still used #0a0a0a, so dark mode flashed grey before the true black. Proof was a pixel diff of main against the branch on the same seeded data. | One look is cheaper to keep right than two. The boot colour must match the app. |
| [#25066](https://github.com/pingdotgg/t3code/pull/25066) chips readable on every theme | `4914cdd98` | Chips lost their pill and border and now sit on the text baseline at body size. Each accent keeps its OKLCH hue, and its lightness is solved until it reaches 5:1 on every surface it can sit on. All 1,656 pairs pass (78 were under 4.5:1 before). Hover underlines the label instead of changing its colour. Focus is a 2 px inset outline in the chip's own colour. | Contrast is solved per surface, not eyeballed, and hover never lowers contrast. |
| [#25401](https://github.com/pingdotgg/t3code/pull/25401) state at the end of the timeline | `bd3287b64` | Cards stacked over the composer, and only one showed. Now each state lives where it belongs: the timeline ends past → now → next, and the composer is only for input. "Scroll to end" names what waits ("Needs input · 2 queued"). The pulsing dot was removed. A queued item is announced as "Queued message 1 of 2, not sent". | Key information is placed where the user acts on it, and announced to screen readers. |
| [#21039](https://github.com/pingdotgg/t3code/pull/21039) project folders | `4e4d4ba39` | Folders are part of the keyboard tree (Left and Right close and open, Tab reaches the row's button). A closed folder shows its count, plus a dot when a hidden thread needs you. | Hidden content still signals when it needs attention. |
| [#14984](https://github.com/pingdotgg/t3code/pull/14984) switched-off controls in dark | `8a948076b` | The off thumb was the page colour on an 8 % track (1.19:1). It now uses the muted text grey. A switched-off row dims only its icon and text, not the switch, so "off" and "unavailable" look different. | Off, disabled and unavailable are three different states. |
| [#24036](https://github.com/pingdotgg/t3code/pull/24036), [#22367](https://github.com/pingdotgg/t3code/pull/22367) | `1df9c3294`, `5ad7da060` | Blockquotes inherit the text colour, with a bar at 35 %, so composer quotes no longer look disabled. Dark-mode separators use the contrast-aware muted token. | Muted colours on muted things become invisible in dark mode. |
| [#21924](https://github.com/pingdotgg/t3code/pull/21924), [#6000](https://github.com/pingdotgg/t3code/pull/6000) | `7845898c8`, `0a7c662d3` | Status colours keep their hue and shift only lightness until 4.5:1. Built-in palettes went from 275 to 300 of 300 AA pairs. Tests now hold text ≥ 7:1 on the canvas and muted text between 4.5 and 5.5:1. | A band for muted text keeps it readable and still muted. Tests stop regressions. |
| [#7906](https://github.com/pingdotgg/t3code/pull/7906) contrast control | `6e9c57f7b` | A 50–200 % contrast slider derives `--contrast-*` roles from the semantic tokens, with no CSS filter on the app root, so photos and terminals are untouched. | Semantic tokens make global adjustments possible. |

T3's shared components (`components/ui/button.tsx` on `8b74a69`) also show the rules they enforce everywhere:

- `focus-visible` draws a 2 px ring.
- Disabled is opacity 0.64 with no pointer events.
- A press scales to 0.97.
- On coarse pointers an `::after` hit area of at least 44 × 44 px wraps every button, badge-button and clear button.
- Controls are one step taller on touch (36 px, 32 px on desktop).
- The composer font is at least 16 px on touch so iOS does not zoom.

Two things T3 still fails, which we do not copy: input borders at 1.48:1 (light) and 1.23:1 (dark), and a white label on red-500 at 3.81:1.

## 2. Principles and pass bars

Each principle has a bar the audit can measure. "Surface" means the colour actually painted behind the element.

### P1. Text contrast
- Body text ≥ 7:1 on its surface (T3's own test bar). AA minimum is 4.5:1.
- Secondary and hint text ≥ 4.5:1, ideally below 5.5:1 so it still reads as secondary.
- Large text (≥ 24 px, or ≥ 18.66 px bold) ≥ 3:1.
- Text on a state fill (pending, error, selected, formula) is measured against that fill, not against white.
- Disabled text is exempt from WCAG, but the app's bar is ≥ 3:1 so a disabled label stays readable. Say why it is disabled (a tooltip or a hint).

### P2. Non-text contrast (WCAG 1.4.11)
- ≥ 3:1 for anything that must be seen to use the control: text-field and checkbox boundaries, toggle thumbs, the fill handle, icon-only buttons, the selection outline, chart series, map markers and transect lines.
- A button with a text label needs no 3:1 border, because its label identifies it. Decorative dividers have no bar.

### P3. Focus visible and keyboard
- Every interactive element shows `:focus-visible`: a 2 px ring ≥ 3:1 against both the control and its surface, with a 2 px offset, or inset where space is tight. `outline: none` is allowed only together with a replacement ring.
- On the green header the ring is white.
- Focus never looks like selection. The selected grid cell is outlined in brand green, while keyboard focus on a picked tile or option is a separate ring, in a neutral colour with a gap (T3 #25251).
- The whole flow can be done by keyboard:
  - tabs, toolbar, grid (the existing Enter, F2, Ctrl+C/V/D and Supr), dialogs, dropdowns
  - Esc closes and returns focus to the control that opened it
  - Tab order follows the visual order.

### P4. Hierarchy
- Three text steps only: `fg`, `fg-muted`, `fg-subtle`. Three weights only: 400, 500, 600.
- One type scale: 12 (minimum, labels only), 13 (grid cells), 14 (body, controls), 16 (section titles), 20 or 24 (page titles and headline numbers).
- One primary (filled green) action per toolbar or dialog. The rest are secondary or ghost.
- Hierarchy comes from size, weight and space before borders and boxes. No cards inside cards.
- One radius scale (4, 6, 8, 12) chosen by role: cells and chips 4, controls 6, panels and dialogs 8, sheets on phones 12. One icon stroke width.

### P5. No redundancy
- No fact is shown twice in the same view: a count in the badge and again in the title, a column name in the cell bar and again in a label beside it, a percentage beside a bar that already shows it.
- Controls that cannot do anything in the current state are hidden when they would show zeros or nothing, and disabled with a reason when the user is expected to make them usable.
- No empty lines or placeholder rows for missing data. No divider where spacing already separates.
- One name per thing across the app. A button and its confirmation use the same verb ("Guardar" → "Guardado").

### P6. Key information visible
- Anything that changes what the user should do next is visible without hovering, scrolling or opening a menu:
  - unsaved count and its save button
  - queued saves waiting for Google
  - offline
  - conflicts and cells needing review
  - the selected specimen's ID
  - which database is open (real database or the lab copy).
- When it sits off-screen, the edge of the view names it (T3's "Needs input · 2 queued").
- A collapsed or filtered section shows its count and a marker when something hidden inside needs attention (T3 #21039).

### P7. Density
- Desktop grid: 13 px cell text, rows 28–32 px, header row sticky. Text outside the grid is 14 px.
- Phones use the row form, never a shrunk grid. Form fields are full width, 44 px tall, and use 16 px text so iOS does not zoom.
- Line length of running text ≤ 80 characters (help text, instructions, dialogs).
- Whitespace is consistent: one spacing scale (4, 8, 12, 16, 24).

### P8. States
Every control and view has its states designed, and each state differs by more than colour (WCAG 1.4.1: an icon, an edge, text or a pattern as well as the fill).

| State | Bar |
|---|---|
| Hover | Visible on pointer devices only, and never lowers contrast (underline or fill, not a fainter colour). |
| Active or pressed | Visible for at least the press (T3: scale 0.97). Not on menu triggers. |
| Selected | Fill plus a ≥ 3:1 edge or check mark. Distinct from focus. |
| Disabled | ≥ 3:1 label (app bar), cursor `not-allowed`, a reason on hover or tap. |
| Loading | A skeleton or spinner with a caption saying what loads. No layout jump when it finishes. |
| Empty | Says what is missing and offers the next action. |
| Error | Says what happened and how to fix it, next to the field. Red only for errors. |
| Pending, queued, conflict (this app) | Amber fill plus a ≥ 3:1 amber edge for unsaved. Stripes for queued. Red edge and icon for conflict. All three readable in dark. |

### P9. Colour carries one meaning each
- Neutral greys for structure.
- Brand green for the brand bar and the primary action, focus and selection.
- Amber only for pending or unsaved work.
- Red only for errors and destructive actions.
- Grey fill for formula and locked cells.
- Any extra hue (the violet at #7c3aed in today's CSS) needs a stated meaning, or it goes.
- Identity colours (species, transects, chart series) are solved per surface, keeping their hue and changing only lightness (T3 #25066, #21924).

### P10. Motion
- No continuous animation (T3 removed its pulsing dot).
- Motion only answers an action (open, expand, confirm), lasts at most 200 ms, and respects `prefers-reduced-motion`.

### P11. Mobile ergonomics
- Tap targets on coarse pointers ≥ 44 × 44 CSS px. A smaller visual can keep a 44 px `::after` hit area, as in T3.
- The absolute floor is 24 × 24 px with no other target inside a 24 px circle (WCAG 2.5.8). Today's touch fill handle is 22 × 22 px, which needs checking against that floor.
- Primary actions sit in the bottom half and respect `env(safe-area-inset-bottom)`.
- Nothing needs hover. Touch gets visible equivalents (T3 shows stars always on touch).
- Toasts and floating bars do not cover the field being edited or the save bar.
- The page works sideways (the existing `short` variant) and at 320 px width without horizontal page scroll, apart from the grid itself.

### P12. Dark theme
- Built from the same role tokens, never by inverting.
- The canvas is darkest, and each raised surface is one step lighter. Shadows carry little on dark, so separation comes from surface steps and 10 % white borders.
- Every translucent fill and state colour is measured again on the dark canvas (T3 #25323).
- Status colours keep their hue and get lighter.
- The `theme-color` meta, the boot background and the `html` background use the dark canvas, so there is no light flash (T3 #25778).
- Follows `prefers-color-scheme` by default, with a manual choice in the user menu next to EN/ES.
- **Maps (Leaflet):**
  - Satellite imagery stays as it is.
  - Street tiles (OSM) are never inverted. Use a dark tile set, or dim at most to `brightness(0.85)`.
  - Markers and transect lines keep ≥ 3:1 against the tiles, with a light or dark halo.
  - Leaflet's zoom and attribution controls use the surface tokens.
- **Photos:**
  - No filter on photos, ever.
  - Shown on a neutral surface (`surface-sunken`), with no white mat.
  - Text over a photo sits on a scrim at ≥ 4.5:1.
- **Charts (ECharts):**
  - The ECharts theme is built from the tokens.
  - Axis labels and legends ≥ 4.5:1, series ≥ 3:1 against the plot background.
  - Gridlines are decorative.
  - Hues are solved again for dark rather than reused.
- **Tabulator:**
  - Its stylesheet hard-codes light colours, so every grid rule reads tokens.
  - Formula, pending, error, staged, queued, match and focus-row states each get a dark value.

### P13. Proof before shipping
- Before and after screenshots of real data, in light and dark, on desktop and on a phone (the Android emulator with real Chrome).
- Two independent critics score them against this checklist.
- A small test holds the token ratios, as T3's `themePalette.test.ts` does.

## 3. Scoring rubric

Score each principle 0–3 per screen (Inicio, Tablas, Colecta, Muertes, Tubos, Emergidos, Historial, Asistente, Fotos, Monitoreo, review dialog, row form on phone).

| Score | Meaning |
|---|---|
| 0 | Fails the bar on most elements, or a task cannot be done. |
| 1 | Fails the bar on several elements, or once on a frequent element. |
| 2 | Meets the bar, with small inconsistencies (one-off hex colours, one missing state). |
| 3 | Meets the bar everywhere on the screen, in light and dark, on desktop and phone. |

Each finding gets a severity:

| Severity | Definition | Example |
|---|---|---|
| P0 | Blocks a task, or can lose or misrecord data. | The unsaved state cannot be seen, or Save is unreachable on a phone. |
| P1 | Fails a WCAG 2.2 AA bar on a frequent element, or key information is hidden. | 2.59:1 hint text used 78 times. No focus ring on grid controls. |
| P2 | Fails on a rare element, or is inconsistent in a way users notice. | An undefined colour class. Two radii for the same kind of control. |
| P3 | Polish. | Uneven icon strokes, an extra divider. |

## 4. Where the app stands on contrast today

Measured on the current CSS (light only, since there is no dark mode yet).

| Pair | Ratio | Bar |
|---|---|---|
| Body stone-900 on stone-50 | 16.74 | 7 ✓ |
| `text-stone-500` (490 uses) on white / stone-50 / stone-100 | 4.79 / 4.58 / 4.39 | 4.5 (fails on stone-100) |
| `text-stone-400` (78 uses) on white | 2.59 | 4.5 ✗ |
| Grid row number #78716c on #f5f5f4 | 4.40 | 4.5 ✗ |
| Row remove ✕ #a8a29e on white | 2.52 | 3 ✗ |
| Button and input border stone-300 on white | 1.49 | 3 ✗ for text fields (buttons are labelled) |
| Input focus ring brand-100 on white | 1.28 | 3 ✗ (the brand-600 border beside it is 6.39 ✓) |
| Header tabs brand-100 on brand-700 | 7.53 | 4.5 ✓ |
| Inactive language label (60 % white) on brand-700 | 4.62 | 4.5 ✓, barely |
| Primary button white on brand-700 | 9.63 | 4.5 ✓ |
| Pending fill #fef3c7 against a white cell | 1.11 | It needs its edge or marker, since the fill alone is colour-only. |

Other findings:

- Only 2 `.vue` files use `focus-visible`, and none use `dark:`.
- AppHeader uses `ring-brand-500`, which the theme does not define.
- `style.css` hard-codes 74 distinct hex colours.

## 5. Proposed token set

Role tokens on `:root` and `[data-theme="dark"]`, mapped into Tailwind with `@theme inline { --color-canvas: var(--canvas); … }`, so the utilities read as roles: `bg-canvas`, `bg-surface`, `text-fg-muted`, `border-control`, `ring-focus`, `bg-cell-pending`. Dark mode uses `@custom-variant dark (&:where([data-theme=dark], [data-theme=dark] *))`. Fira Sans stays.

The light theme keeps today's warm stone neutrals and deep green. The dark theme uses a faintly green near-black instead of T3's pure black, so the brand carries into it and photos and satellite maps are not framed in harsh black.

| Token | Light | Dark | Use |
|---|---|---|---|
| `canvas` | #fafaf9 | #0e1210 | page background |
| `surface` | #ffffff | #151a18 | grid, toolbars, dialogs |
| `surface-sunken` | #f5f5f4 | #1b211e | grid header, row numbers, photo mat |
| `surface-raised` | #ffffff | #202723 | menus, popovers (dark lifts by tone) |
| `fg` | #1c1917 | #e9ece9 | body text |
| `fg-muted` | #57534e | #b2b9b4 | labels, formula text |
| `fg-subtle` | #6f6964 | #959d98 | hints, row numbers (replaces stone-400 and stone-500) |
| `border` | #e7e5e4 | white 10 % | decorative dividers |
| `border-control` | #8a837d | #6e7772 | text-field, checkbox and select boundaries |
| `brand` | #1f4d3a | #2d6a4f | header bar, primary button |
| `brand-hover` | #173a2c | #357a5b | primary hover |
| `on-brand` | #ffffff | #ffffff | text on brand |
| `on-brand-muted` | #cfe3d7 | #d5e8dd | inactive header tabs |
| `brand-text` | #1f4d3a | #7cc6a0 | IDs, links |
| `brand-soft` | #e6f1eb | #1a2e25 | brand-tinted chips, fill target |
| `focus` | #2d6a4f | #7cc6a0 | focus ring |
| `focus-on-brand` | #ffffff | #ffffff | focus ring on the header |
| `select` | #e3f0e8 | #1d3328 | selected row (plus an edge) |
| `cell-formula` | #f1f2ee | #1a1f1c | formula and locked cells |
| `cell-pending` | #fdf0c4 | #3a2c0c | unsaved cell |
| `pending-text` | #7a3d0b | #fcd34d | "unsaved" labels |
| `pending-edge` | #b45309 | #f59e0b | unsaved cell edge, staged row marker |
| `cell-error` | #fde0e0 | #3d1616 | invalid or conflicting cell |
| `error-text` | #991b1b | #fca5a5 | error messages |
| `error-edge` | #c62828 | #f87171 | error cell edge, invalid corner |
| `success-text` | #065f46 | #6ee7b7 | saved confirmations |
| `danger` | #b91c1c | #c62828 | destructive button fill |
| `on-danger` | #ffffff | #ffffff | destructive button label |

Measured ratios (all pass):

| Pair | Light | Dark | Bar |
|---|---|---|---|
| `fg` on `canvas` / `surface` / `surface-sunken` | 16.74 / 17.49 / 16.03 | 15.85 / 14.79 / 13.75 | 7 |
| `fg-muted` on `surface` / `surface-sunken` / `cell-formula` | 7.63 / 6.99 / 6.78 | 8.80 / 8.18 / 8.35 | 4.5 |
| `fg-subtle` on `surface` / `canvas` / `surface-sunken` | 5.41 / 5.18 / 4.96 | 6.33 / 6.79 / 5.89 | 4.5 |
| `border-control` on `surface` / `canvas` | 3.73 / 3.58 | 3.81 / 4.08 | 3 |
| `on-brand` on `brand` / `brand-hover` | 9.63 / 12.51 | 6.39 / 5.14 | 4.5 |
| `on-brand-muted` on `brand` | 7.16 | 5.00 | 4.5 |
| `brand-text` on `surface` / `brand-soft` | 9.63 / 8.32 | 8.75 / 7.15 | 4.5 |
| `focus` on `surface` / `canvas` | 6.39 / 6.12 | 8.75 / 9.38 | 3 |
| `focus-on-brand` on `brand` | 9.63 | 6.39 | 3 |
| `fg` on `cell-pending` / `cell-error` / `select` | 15.35 / 14.09 / 14.90 | 11.41 / 13.32 / 11.33 | 7 |
| `pending-text` on `cell-pending` | 7.38 | 9.42 | 4.5 |
| `pending-edge` on `cell-pending` / `surface` | 4.41 / 5.02 | 6.33 / 8.20 | 3 |
| `error-text` on `cell-error` / `surface` | 6.69 / 8.31 | 8.35 / 9.28 | 4.5 |
| `error-edge` on `surface` | 5.62 | 6.36 | 3 |
| `success-text` on `surface` | 7.68 | 11.55 | 4.5 |
| `on-danger` on `danger` | 6.47 | 5.62 | 4.5 |

State fills against a plain cell are deliberately faint and so cannot carry the state alone:

| Fill against `surface` | Light | Dark |
|---|---|---|
| `cell-pending` | 1.14 | 1.30 |
| `cell-error` | 1.24 | 1.11 |
| `select` | 1.17 | 1.30 |

The edge tokens above carry the ≥ 3:1 signal.

To change a value, edit `tokens.json` beside the script and run `python3 -I contrast.py <frontend/node_modules/tailwindcss/theme.css> tokens.json`.
