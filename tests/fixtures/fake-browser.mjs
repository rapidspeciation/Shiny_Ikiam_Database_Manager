// A browser-like process for tests/fixtures/fake-playwright.mjs. As Playwright starts Chromium: a process group of its
// own, the profile in TMPDIR (in its command line), and a helper child, here in yet another group of its own, so only
// the process tree (not a group kill) finds it. Each logs its pid to the scenario's log and waits.
// argv: <log file> <role: browser | renderer> [--user-data-dir=…]
import { appendFileSync } from 'node:fs';
import { spawn } from 'node:child_process';

const [log, role, ...rest] = process.argv.slice(2);
appendFileSync(log, `${JSON.stringify({ [role]: process.pid })}\n`);
if (role === 'browser') spawn(process.execPath, [process.argv[1], log, 'renderer', ...rest], { detached: true, stdio: 'ignore' });
setInterval(() => {}, 1000);
