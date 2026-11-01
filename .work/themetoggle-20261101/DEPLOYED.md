# Deployed: sun/moon theme toggle (1 Nov 2026, 16:19 UTC)

## Lab app
- Release **20261101T161913Z** (branch ui-themetoggle-20261101 = team main + toggle; main pushed to origin and team).
  Before: 20261101T133517Z. Health ok. Deploy lock taken and released.
- Rollback:
  `ssh claudeclaw 'cd /home/ubuntu/ithomiini && ln -sfn releases/20261101T133517Z current && systemctl --user restart ithomiini.service'`
  then the t3-provision --refresh-all line (see .work/header-20261101/DEPLOYED.md).
- Live check (Chromium, signed out): light system → «Theme: light (system)», aria-pressed false; dark → «Theme: dark (system)», true.

## Wings Atlas
- rapidspeciation/ithomiini_maps main e30f56c → b54ea7a (fast-forward); Pages deploy run 41325262853 succeeded; same live check passes.
- Rollback: `git revert --no-edit e30f56c..b54ea7a` on a branch from rapidspeciation/main, push to main.

## What
Two-state sun/moon button with Caligo's turn (own code of rapidspeciation/caligo, no third-party licence; comment names
the origin); the icon shows the theme in effect; a click switches and is remembered per device; System stays the
default and follows the OS live; «Usar el tema del sistema» in the user menu (Atlas: palette menu) only while a choice
is stored; tooltip «Tema: claro (sistema)» / «oscuro (elegido)»; no turn with reduced motion; aria-label + aria-pressed.
Known: signed-out lab visitors have no user menu, so no way back to System after a click.
Tests: lab server 881/881, frontend 746/746, build; Atlas 299, typecheck, build.
Reviewed with dual-critic-loop: 1 round, Sol A/A, Opus A-/A- (nits fixed c98f61c, b54ea7a). Ledger ~/.local/share/ui-overhaul-20261031/review-tt/LEDGER.md.
