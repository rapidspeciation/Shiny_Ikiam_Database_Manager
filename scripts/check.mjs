#!/usr/bin/env node
import { readdirSync } from 'node:fs';
import { resolve } from 'node:path';
import { spawnSync } from 'node:child_process';

function files(path) {
  return readdirSync(path, { withFileTypes: true }).flatMap(entry => entry.isDirectory()
    ? files(`${path}/${entry.name}`) : [`${path}/${entry.name}`]);
}
const targets = ['server', 'web', 'scripts', 'tests'].flatMap(files).filter(path => /\.(m?js)$/.test(path));
let failed = false;
for (const path of targets) {
  const result = spawnSync(process.execPath, ['--check', resolve(path)], { encoding: 'utf8' });
  if (result.status) { failed = true; process.stderr.write(result.stderr); }
}
if (failed) process.exit(1);
console.log(`Syntax checked ${targets.length} JavaScript files`);
