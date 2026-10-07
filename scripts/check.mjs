#!/usr/bin/env node
import { readdirSync } from 'node:fs';
import { resolve } from 'node:path';
import { spawn, spawnSync } from 'node:child_process';
import { availableParallelism } from 'node:os';

function files(path) {
  return readdirSync(path, { withFileTypes: true }).flatMap(entry =>
    entry.isDirectory() ? files(`${path}/${entry.name}`) : [`${path}/${entry.name}`],
  );
}
const targets = [
  ...['server', 'scripts', 'tests'].flatMap(files),
  ...readdirSync('tools/wikiloc').map(name => `tools/wikiloc/${name}`),
].filter(path => /\.(m?js)$/.test(path));
// One `node --check` per file, a few at a time.
const check = path =>
  new Promise(done => {
    const child = spawn(process.execPath, ['--check', resolve(path)], { stdio: ['ignore', 'ignore', 'pipe'] });
    let stderr = '';
    child.stderr.on('data', chunk => (stderr += chunk));
    child.on('close', status => done(status ? stderr : ''));
  });
const queue = [...targets];
const errors = [];
await Promise.all(
  Array.from({ length: Math.max(2, availableParallelism()) }, async () => {
    while (queue.length) errors.push(await check(queue.shift()));
  }),
);
if (errors.some(Boolean)) {
  process.stderr.write(errors.join(''));
  process.exit(1);
}
console.log(`Syntax checked ${targets.length} JavaScript files`);

// The frontend is TypeScript + Vue; its type check needs `npm --prefix frontend install` once.
const typecheck = spawnSync('npm', ['--prefix', 'frontend', 'run', 'typecheck'], {
  encoding: 'utf8',
  stdio: 'inherit',
});
if (typecheck.status) process.exit(1);
console.log('Type checked the frontend');
