#!/usr/bin/env node
import { readdirSync } from 'node:fs';
import { spawn, spawnSync } from 'node:child_process';

function files(path) {
  return readdirSync(path, { withFileTypes: true }).flatMap(entry =>
    entry.isDirectory() ? files(`${path}/${entry.name}`) : [`${path}/${entry.name}`],
  );
}
const targets = [
  ...['server', 'scripts', 'tests'].flatMap(files),
  ...readdirSync('tools/wikiloc').map(name => `tools/wikiloc/${name}`),
].filter(path => /\.(m?js)$/.test(path));

const run = (command, args, options = {}) =>
  new Promise(done => {
    const child = spawn(command, args, { stdio: ['ignore', 'inherit', 'pipe'], ...options });
    let stderr = '';
    child.stderr?.on('data', chunk => (stderr += chunk));
    child.on('close', status => done({ status, stderr }));
  });

// The frontend is TypeScript + Vue; its type check needs `npm --prefix frontend install` once.
// It runs beside the syntax check, and is incremental (frontend/package.json).
const typecheck = run('npm', ['--prefix', 'frontend', 'run', 'typecheck'], { stdio: ['ignore', 'inherit', 'inherit'] });

// Syntax: every file compiled as an ES module (both packages are "type": "module") in one process,
// instead of a node start per file; a file that fails is checked again with `node --check`, which
// says where.
const SYNTAX = `
import { readFileSync } from 'node:fs';
import { SourceTextModule } from 'node:vm';
for (const path of process.argv.slice(1)) {
  try { new SourceTextModule(readFileSync(path, 'utf8'), { identifier: path }); }
  catch { console.error(path); }
}`;
const syntax = await run(process.execPath, ['--experimental-vm-modules', '--no-warnings', '--input-type=module', '-e', SYNTAX, ...targets]);
const failing = syntax.stderr.split('\n').filter(Boolean);
if (syntax.status || failing.length) {
  for (const path of failing) {
    const check = spawnSync(process.execPath, ['--check', path], { encoding: 'utf8' });
    process.stderr.write(check.status ? check.stderr : `${path}\n`);
  }
  if (!failing.length) process.stderr.write(syntax.stderr || 'The syntax check failed\n');
  await typecheck;
  process.exit(1);
}
console.log(`Syntax checked ${targets.length} JavaScript files`);

if ((await typecheck).status) process.exit(1);
console.log('Type checked the frontend');
