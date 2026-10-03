#!/usr/bin/env node
// Someone types in the lab's sheet, as in Google Sheets: to see what a proposal
// does with a cell edited after it was drafted (Cambios propuestos marks it).
// The lab app runs in LOCAL_MODE: the edit goes to its in-memory sheets, never
// to Google. Signs in as the lab admin ($LAB/credentials.json).
//   node tools/lab/sheet-edit.mjs Insectary_data 5012 Sex=female "Notes_Insectary_data=ala rota"
//   node tools/lab/sheet-edit.mjs Insectary_stocks 950 "NUMBER OF EGGS==12+16"   (a sum: the value after = is a formula)
//   … --no-hook   the app is not told (as before its next sync): applying finds it then
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
const noHook = argv.includes('--no-hook');
if (noHook) argv.splice(argv.indexOf('--no-hook'), 1);
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
const [sheet, row, ...cells] = argv;
if (!sheet || !Number(row) || !cells.length || !password) {
  console.error('Usage: sheet-edit.mjs <sheet> <row> <field>=<value>… [--no-hook] [--app <url> --user <name> --password <pw>]');
  process.exit(2);
}
// field=value; a number stays a number, "=…" is a formula, an empty value empties the cell.
const values = Object.fromEntries(
  cells.map(cell => {
    const at = cell.indexOf('=');
    if (at < 1) throw new Error(`Not field=value: ${cell}`);
    const text = cell.slice(at + 1);
    const value = text.startsWith('=') ? { formula: text } : text === '' ? null : /^-?\d+(\.\d+)?$/.test(text) ? Number(text) : text;
    return [cell.slice(0, at), value];
  }),
);

const headers = { 'content-type': 'application/json', origin: app };
const login = await fetch(`${app}/api/auth/login`, { method: 'POST', headers, body: JSON.stringify({ username, password }) });
if (!login.ok) throw new Error(`Sign-in: ${login.status} ${(await login.text()).slice(0, 200)}`);
const { csrf } = await login.json();
const cookie = login.headers.getSetCookie().map(c => c.split(';')[0]).join('; ');
const out = await fetch(`${app}/api/local/sheet-edit`, {
  method: 'POST',
  headers: { ...headers, cookie, 'x-csrf-token': csrf },
  body: JSON.stringify({ sheet, row: Number(row), values, hook: !noHook }),
});
console.log(out.status, await out.text());
