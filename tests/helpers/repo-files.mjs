import { execFileSync } from 'node:child_process';
import { cpSync, readdirSync, realpathSync } from 'node:fs';
import { join, relative, sep } from 'node:path';

// The tests that need the repo's files as git sees them, in a git checkout (a PC, deploy.sh) and in a plain copy of a
// commit without .git (the nightly on claudeclaw: `git archive` into a scratch folder).

/** True when `root` is the top of a git work tree (not only a folder inside another repo). */
export function isGitCheckout(root) {
  try {
    const top = execFileSync('git', ['-C', root, 'rev-parse', '--show-toplevel'], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'ignore'] }).trim();
    return realpathSync(top) === realpathSync(root);
  } catch {
    return false;
  }
}

/**
 * The repo's files, relative with '/': `git ls-files` in a checkout; in a plain copy every file under it but
 * node_modules (the copy holds only the commit's files and what npm ci installs).
 */
export function repoFiles(root) {
  if (isGitCheckout(root)) return execFileSync('git', ['ls-files'], { cwd: root, encoding: 'utf8' }).split('\n').filter(Boolean);
  const files = [];
  const walk = rel => {
    for (const e of readdirSync(join(root, rel), { withFileTypes: true })) {
      const path = rel ? `${rel}/${e.name}` : e.name;
      if (e.isDirectory()) { if (e.name !== 'node_modules') walk(path); }
      else files.push(path);
    }
  };
  walk('');
  return files.sort();
}

/**
 * A git repo at `dest` with the repo's last commit: a clone of a checkout, or in a plain copy a new repo of its files
 * (node_modules and the .gitignore'd left out), one commit.
 */
export function cloneRepo(root, dest) {
  if (isGitCheckout(root)) {
    execFileSync('git', ['clone', '-q', '--no-hardlinks', root, dest]);
    return;
  }
  cpSync(root, dest, { recursive: true, filter: src => !relative(root, src).split(sep).includes('node_modules') });
  const g = (...args) => execFileSync('git', ['-C', dest, ...args], { stdio: ['ignore', 'pipe', 'pipe'] });
  g('init', '-q');
  g('add', '-A');
  g('-c', 'user.name=T', '-c', 'user.email=t@example.invalid', 'commit', '-q', '--no-verify', '-m', 'the release copy');
}
