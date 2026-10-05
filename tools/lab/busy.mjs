#!/usr/bin/env node
// The lab's sheets answer as Google does while the team's workbook recalculates
// (5 Oct 2026): to see the banner, saves waiting in the app and the Emergidos /
// Clutches entries going to Google Sheets once it answers. The lab app runs in
// LOCAL_MODE: nothing reaches Google. Signs in as the lab admin ($LAB/credentials.json).
//   node tools/lab/busy.mjs 5                 every request gets a 503 for 5 minutes
//   node tools/lab/busy.mjs 5 hang            no answer (given up after 3 s each) for 5 minutes
//   node tools/lab/busy.mjs 5 slow --delay 25 answers after 25 s (over the 20 s that makes it "slow")
//   node tools/lab/busy.mjs 0                 answers again now (the waiting saves are written)
//   … --probe 10   asks the busy workbook every 10 s instead of every minute
//   … --app http://127.0.0.1:8797 --user lab --password …   another local app
import { labPath, readJson } from './lib.mjs';

const argv = process.argv.slice(2);
const flag = name => {
  const i = argv.indexOf(name);
  if (i < 0) return null;
  const value = argv[i + 1];
  argv.splice(i, 2);
  return value;
};
const saved = (() => {
  try {
    return readJson(labPath('credentials.json'));
  } catch {
    return {};
  }
})();
const app = (flag('--app') ?? saved.url ?? 'http://127.0.0.1:8795').replace(/\/+$/, '');
const username = flag('--user') ?? saved.username ?? 'lab';
const password = flag('--password') ?? saved.password;
const delay = flag('--delay');
const probe = flag('--probe');
const [minutes, mode = 'unavailable'] = argv;
if (minutes === undefined || !Number.isFinite(Number(minutes)) || !password) {
  console.error('Usage: busy.mjs <minutes> [unavailable|hang|slow] [--delay <seconds>] [--probe <seconds>] [--app <url> --user <name> --password <pw>]');
  process.exit(2);
}

const headers = { 'content-type': 'application/json', origin: app };
const login = await fetch(`${app}/api/auth/login`, { method: 'POST', headers, body: JSON.stringify({ username, password }) });
if (!login.ok) throw new Error(`Sign-in: ${login.status} ${(await login.text()).slice(0, 200)}`);
const { csrf } = await login.json();
const cookie = login.headers.getSetCookie().map(c => c.split(';')[0]).join('; ');
const out = await fetch(`${app}/api/local/busy`, {
  method: 'POST',
  headers: { ...headers, cookie, 'x-csrf-token': csrf },
  body: JSON.stringify({
    minutes: Number(minutes),
    mode,
    ...(delay ? { delayMs: Number(delay) * 1000 } : {}),
    ...(probe ? { probeSeconds: Number(probe) } : {}),
  }),
});
console.log(out.status, await out.text());
