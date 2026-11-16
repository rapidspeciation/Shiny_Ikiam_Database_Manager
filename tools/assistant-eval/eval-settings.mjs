// The eval workspace's Google rules, on top of the lab workspace's settings (eval-app.mjs `workspace`).
// gcloud stays denied (this PC's holds Franz's own Google login). The project's Gmail is read-only, as in the lab
// (16 Nov 2026): the checkout's lab guard keeps gog's binary and token out of the sandbox and puts its read hook
// (scripts/lab-gmail-read.mjs) in; every other gog command is refused there. Until then the eval denied gog
// outright, so S07 could only load the skill.
import { existsSync } from 'node:fs';
import { join } from 'node:path';

export const GOG_DENY = ['Bash(gog*)', 'Bash(*gog *)', 'Bash(*/gog*)'];

/** `settings` (the lab workspace's, parsed) for the eval of `checkout`; changed in place and returned. */
export async function evalSettings(settings, { checkout, home = '/home/franz', hookDir }) {
  const hook = join(checkout, 'scripts', 'lab-gmail-read.mjs');
  const readHook = existsSync(hook) && existsSync(join(checkout, 'scripts', 'lab-guard.mjs'));
  const extra = ['Bash(gcloud*)', 'Bash(*gcloud*)', 'Bash(*gsutil*)', `Read(/${home}/.config/gogcli/**)`, `Read(/${home}/.config/gcloud/**)`];
  settings.permissions ??= {};
  settings.permissions.deny = [...new Set([...(settings.permissions.deny ?? []).filter(r => !GOG_DENY.includes(r)), ...extra, ...(readHook ? [] : GOG_DENY)])];
  settings.sandbox ??= {};
  settings.sandbox.filesystem ??= {};
  settings.sandbox.filesystem.denyRead = [...new Set([...(settings.sandbox.filesystem.denyRead ?? []), ...['.config/gogcli', '.local/share/gogcli', '.config/gcloud', '.local/bin/gog'].map(p => join(home, p))])];
  // An older checkout under test has no read hook: gog stays denied there (above).
  if (readHook) (await import(join(checkout, 'scripts', 'lab-guard.mjs'))).applyLabGuard(settings, { home, source: null, hook, hookDir });
  return settings;
}
