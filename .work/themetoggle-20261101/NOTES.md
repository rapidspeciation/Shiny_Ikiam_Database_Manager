# Theme toggle (1 Nov 2026)
- Header button: two-state sun/moon, always the theme in effect (never a monitor); a press stores the opposite (`oppositeTheme`, lib/theme.ts). aria-label «Tema oscuro» + aria-pressed (pressed = dark); tooltip «Tema: claro|oscuro (sistema|elegido)» (EN strings in locales/en/common.ts).
- System stays the default and follows the OS live (unchanged). Way back: user-menu item «Usar el tema del sistema», shown only while a choice is stored. The user menu exists only when signed in; signed out, the stored choice can only be changed with the toggle.
- Animation: Caligo's ThemeToggle.astro (rotate -90° + scale .6 → 0, 0.4 s) on each press only; none under prefers-reduced-motion.
- Licence: Caligo's code is its own (no theme-toggles snippet, no third-party attribution); the repo has no licence file but is the same organisation. Icons are lucide (ISC), already used here. A comment names Caligo as the origin.
- Tests: AppHeader.test.ts (toggle, aria, tooltip, System item), theme.test.ts (opposite, live OS change). Native title tooltips can't be photographed headless; text asserted instead.
- Shots: /home/franz/.local/share/ui-overhaul-20261031/theme-shots/lab-20261101T1605 (script ../scripts/themetoggle-lab.mjs).
