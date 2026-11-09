#!/usr/bin/env node
import { mkdirSync, readdirSync } from 'node:fs';
import { spawn, spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { homedir } from 'node:os';
import { dirname, join, resolve } from 'node:path';

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
// It runs beside the syntax check, and is incremental (as frontend/package.json's typecheck). Its incremental file is
// kept in the deploy cache (ITHOMIINI_DEPLOY_CACHE, as scripts/deploy.sh), one per checkout: in frontend/node_modules
// it went with every `npm ci` (a new lockfile or node) and every new checkout, and the check started cold (+10 to 30 s).
const cache = process.env.ITHOMIINI_DEPLOY_CACHE || join(homedir(), '.cache', 'ithomiini-deploy');
const buildInfo = join(cache, 'typecheck', `${createHash('sha256').update(resolve('frontend')).digest('hex').slice(0, 16)}.tsbuildinfo`);
mkdirSync(dirname(buildInfo), { recursive: true });
const typecheck = run(process.execPath, ['node_modules/vue-tsc/bin/vue-tsc.js', '--noEmit', '--incremental', '--tsBuildInfoFile', buildInfo], {
  cwd: 'frontend',
  stdio: ['ignore', 'inherit', 'inherit'],
});

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
