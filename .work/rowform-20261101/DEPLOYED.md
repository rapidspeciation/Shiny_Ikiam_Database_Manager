# Deployed: phone row form option (1 Nov 2026, 13:14 UTC)

- Release **20261101T131426Z** (branch ui-rowform-proto merged with team main; main pushed to origin and team).
  Before: 20261101T124605Z. Health ok. Deploy lock taken and released. First deploy run stopped on the flaky
  readings timing test under PC load (passed 3/3 alone); the rerun passed 879/879.
- Rollback:
  `ssh claudeclaw 'cd /home/ubuntu/ithomiini && ln -sfn releases/20261101T124605Z current && systemctl --user restart ithomiini.service'`
  then `ssh claudeclaw 'cd /home/ubuntu/ithomiini/current && node=$(sed -n "s/^ExecStart=\([^ ]*\/node\) .*/\1/p" deploy/ithomiini.service) && "$node" scripts/t3-provision.mjs --refresh-all'`
- What: Buscador on phones opens the grid as before; a button beside ⋯ switches to a row form (remembered per device):
  fields in collapsible sections (most-used first, racks/manifest folded), one-line header, Anterior / «Ir a…» /
  Siguiente following the grid's filters and sort, «Ir a…» usable with the keyboard open, edits through the same save bar.
- Tests: server 879/879, frontend 724/724, type check, build. Android emulator (Gboard): «Ir a…» and a field edit OK.
- Reviewed with dual-critic-loop: 2 rounds, Sol A → A, Opus A- → A. Ledger: ~/.local/share/ui-overhaul-20261031/review-rf/LEDGER.md.
