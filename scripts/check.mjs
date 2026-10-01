#!/usr/bin/env node
import { readdirSync } from 'node:fs';
import { resolve } from 'node:path';
import { spawnSync } from 'node:child_process';

function files(path) {
  return readdirSync(path, { withFileTypes: true }).flatMap(entry =>
    entry.isDirectory() ? files(`${path}/${entry.name}`) : [`${path}/${entry.name}`],
  );
}
const targets = [
  ...['server', 'scripts', 'tests'].flatMap(files),
  ...readdirSync('tools/wikiloc').map(name => `tools/wikiloc/${name}`),
].filter(path => /\.(m?js)$/.test(path));
let failed = false;
for (const path of targets) {
  const result = spawnSync(process.execPath, ['--check', resolve(path)], { encoding: 'utf8' });
  if (result.status) {
    failed = true;
    process.stderr.write(result.stderr);
  }
}
if (failed) process.exit(1);
console.log(`Syntax checked ${targets.length} JavaScript files`);

// The frontend is TypeScript + Vue; its type check needs `npm --prefix frontend install` once.
const typecheck = spawnSync('npm', ['--prefix', 'frontend', 'run', 'typecheck'], {
  encoding: 'utf8',
  stdio: 'inherit',
});
if (typecheck.status) process.exit(1);
console.log('Type checked the frontend');
