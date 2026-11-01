# Deployed: header release (1 Nov 2026, 13:35 UTC)

- Release **20261101T133517Z** (branch ui-header-20261101 merged with team main; main pushed to origin and team).
  Before: 20261101T132910Z (assistant voice fixes). Health ok. Deploy lock taken after that chat released it, then released.
- Rollback:
  `ssh claudeclaw 'cd /home/ubuntu/ithomiini && ln -sfn releases/20261101T132910Z current && systemctl --user restart ithomiini.service'`
  then `ssh claudeclaw 'cd /home/ubuntu/ithomiini/current && node=$(sed -n "s/^ExecStart=\([^ ]*\/node\) .*/\1/p" deploy/ithomiini.service) && "$node" scripts/t3-provision.mjs --refresh-all'`
- What: header never scrolls or clips at 390–1440 px (tabs that don't fit go into «Más ▾»; below 1024 px the tabs
  get their own row, header ~90 px instead of 46); Colecta, Tubos, Emergidos, Clutches, Censo, Revisión in «Más ▾»
  (config: HIDDEN_TABS in frontend/src/lib/headerTabs.ts; an open hidden tab shows in the row; deep links work);
  theme button in the header cycling System/Light/Dark (also for visitors); menus close on outside click, Escape
  (focus back), focus leaving and route change via a shared useDismiss (user menu, «Más», Buscador ⋯, Photo_links recent cards).
- Tests: server 881/881, frontend 743/743, type check, build. Overflow check 0/72 header views (was 60/72).
- Reviewed with dual-critic-loop: 2 rounds, Sol B → A, Opus A- → A. Ledger: ~/.local/share/ui-overhaul-20261031/review-hd/LEDGER.md.
