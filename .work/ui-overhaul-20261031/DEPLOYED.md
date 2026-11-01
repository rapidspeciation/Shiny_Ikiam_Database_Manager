# Deployed: dark theme + UI fix wave 1 (1 Nov 2026, 04:51 UTC)

- Release **20261101T045130Z** (commit 71cf37c, branch ui-overhaul-20261031 = main 1973079 + the overhaul; main
  fast-forwarded and pushed to origin and team, no force push). Before: 20261101T044506Z (perf E1b). Health ok,
  T3 skills refreshed by deploy.sh. Deploy lock taken and released at claudeclaw:/home/ubuntu/ithomiini/deploy.lock.
- Rollback:
  `ssh claudeclaw 'cd /home/ubuntu/ithomiini && ln -sfn releases/20261101T044506Z current && systemctl --user restart ithomiini.service'`
  then `ssh claudeclaw 'cd /home/ubuntu/ithomiini/current && node=$(sed -n "s/^ExecStart=\([^ ]*\/node\) .*/\1/p" deploy/ithomiini.service) && "$node" scripts/t3-provision.mjs --refresh-all'`
- What: role tokens for light and dark (frontend/src/style.css, lib/theme.ts, public/theme-boot.js sets the theme
  before the first paint); System / Light / Dark in the user menu, remembered per device; grid states with ≥ 3:1
  edges; maps, charts and photos dark-safe. Wave 1 (audit P0/P1): no page wider than a 360/390 phone, header and
  tab rows scroll with a cue, the open database shown at every width, 2 px focus rings everywhere, names on date and
  filter boxes, control edges ≥ 3:1, save bar names what is unsaved, rack Aplicar/Descartar always reachable,
  Monitoreo map on phones starts with the map (filters open when the link carries filters).
  Known: the T3 frame keeps its own theme; visitors (not signed in) follow the system setting (no menu).
- Tests: deploy.sh server 820/820, frontend 714/714, type check, build. Screenshots of 74 views × 1440/390/360 ×
  es/en × light/dark: /home/franz/.local/share/ui-overhaul-20261031/{audit-before,c1-after-light,c1-after-dark,c1-r2-*}.
  Contrast after: dark no text < 4.5:1; light only disabled labels at 3.35:1 (app bar 3:1).
- Reviewed with dual-critic-loop: dark theme 3 rounds (Sol B → A → A, Opus B+ → A → A), wave 1 2 rounds
  (Sol B → A, Opus B+ → A-). Ledger: /home/franz/.local/share/ui-overhaul-20261031/review-c1/LEDGER-w0.md.
