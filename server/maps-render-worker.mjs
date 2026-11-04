#!/usr/bin/env node
// The thin browser worker of server/maps-render.mjs (run as its own process group, TMPDIR a fresh folder that the
// parent removes). stdin: one line { url, viewport, deadline_ms }, then kept open by the parent as a lease; stdout: one
// JSON line, { ok: true, result } (what window.ithomiiniMapsExport() returned) or { ok: false, reason, stage, message }.
// It opens only the official maps app with the URL the parent built (checked again here) and evaluates nothing but the
// call to window.ithomiiniMapsExport(): no other script, no input in any evaluated code.
// `--check`: import PLAYWRIGHT_MODULE, find chromium.launch and its executable, print { ok }, launch nothing.
//
// Cleanup that does not depend on the parent: before it exits, for any reason, the worker kills every process started
// under it that is still the same process (processTree in server/maps-render.mjs: the browser has a process group of
// its own, so a group kill would miss it). It does so, removes its TMPDIR and exits when
//   - stdin ends: the lease is gone (the tools thread was terminated, the server process died, or the parent let go);
//   - its parent process changes (the server process died);
//   - its own deadline passes (deadline_ms + 1.5 s), however the browser hangs (launch included).
import { existsSync, realpathSync, rmSync } from 'node:fs';
import { basename } from 'node:path';
import { pathToFileURL } from 'node:url';
import { executableProblem, processTree, renderUrlProblem } from './maps-render.mjs';

process.stdout.on('error', () => {}); // the parent gone: no one to tell
const say = v => process.stdout.write(`${JSON.stringify(v)}\n`);
const fail = (reason, stage, e) => say({ ok: false, reason, stage, message: String(e?.message ?? e ?? '').slice(0, 300) });
const parent = process.ppid;
const check = process.argv.includes('--check');

const tree = processTree({ pid: process.pid, alive: () => true });
let ending = null;
/** Kill what was started under this worker, remove TMPDIR when the parent may not be there to, exit with `code`. */
function end(code, abandoned = false) {
  ending ??= (async () => {
    clearInterval(watch);
    await tree.reap({ waitMs: 3000 }).catch(() => {});
    if (abandoned) removeOwnTmp();
    process.exit(code);
  })();
  return ending;
}
/** TMPDIR, only when it is this run's folder (maps-render-XXXXXX, the working directory the parent gave). */
function removeOwnTmp() {
  const tmp = process.env.TMPDIR;
  try {
    if (tmp && /^maps-render-\w{6}$/.test(basename(tmp)) && realpathSync(tmp) === realpathSync(process.cwd())) rmSync(tmp, { recursive: true, force: true });
  } catch {
    // gone already
  }
}
// What runs under this worker, seen often, so each is still known after its parent dies; and the server's death.
const watch = setInterval(() => {
  tree.observe();
  if (process.ppid !== parent) end(70, true);
}, 250);

/** The job: the first line of stdin. After it, the end of stdin is the lease ending. */
function readJob() {
  return new Promise((resolve, reject) => {
    let buf = '';
    let got = false;
    process.stdin.setEncoding('utf8');
    process.stdin.on('data', c => {
      if (got) return;
      buf += c;
      const at = buf.indexOf('\n');
      if (at >= 0) {
        got = true;
        try {
          resolve(JSON.parse(buf.slice(0, at)));
        } catch (e) {
          reject(e);
        }
      } else if (buf.length > 64 * 1024) reject(new Error('job line too long'));
    });
    const lost = () => {
      if (!got) reject(new Error('stdin ended before the job'));
      end(71, true);
    };
    process.stdin.on('end', lost);
    process.stdin.on('error', lost);
  });
}

async function chromiumOf() {
  const mod = await import(pathToFileURL(process.env.PLAYWRIGHT_MODULE).href);
  const chromium = mod.chromium?.launch ? mod.chromium : mod.default?.chromium;
  if (!chromium?.launch) throw new Error('PLAYWRIGHT_MODULE has no chromium.launch');
  // MAPS_RENDER_CHROMIUM, when set, is the binary used (on the server: the headless shell; docs/operations.md); else
  // the module's own, which is full Chromium.
  const executablePath = process.env.MAPS_RENDER_CHROMIUM || chromium.executablePath?.();
  if (!executablePath || !existsSync(executablePath)) throw new Error('no Chromium executable');
  const problem = executableProblem(executablePath);
  if (problem) throw new Error(`Chromium executable ${problem}`);
  return { chromium, executablePath };
}

async function main() {
  let browserKit;
  try {
    browserKit = await chromiumOf();
  } catch (e) {
    return fail('renderer_unavailable', 'module', e);
  }
  if (check) return say({ ok: true });
  let job;
  try {
    job = await readJob();
  } catch (e) {
    return fail('worker_failed', 'job', e);
  }
  const problem = renderUrlProblem(job.url);
  if (problem) return fail('url_rejected', 'url', problem);
  const deadline = Date.now() + Math.max(1000, Number(job.deadline_ms) || 45_000);
  const left = () => Math.max(500, deadline - Date.now());
  // The worker's own stop, after the parent's (which kills the run first when it is there).
  setTimeout(() => end(124, true), left() + 1500);
  let browser;
  try {
    browser = await browserKit.chromium.launch({
      executablePath: browserKit.executablePath,
      headless: true,
      // WebGL without a GPU (the maps are MapLibre); the pilots ran with these.
      args: ['--use-angle=swiftshader', '--enable-unsafe-swiftshader', '--ignore-gpu-blocklist', '--disable-extensions', '--no-first-run'],
      timeout: left(),
    });
  } catch (e) {
    return fail('launch_failed', 'launch', e);
  }
  try {
    const context = await browser.newContext({ viewport: job.viewport ?? { width: 1280, height: 800 }, deviceScaleFactor: 1, colorScheme: 'light', locale: 'en-US', acceptDownloads: false, serviceWorkers: 'block' });
    const page = await context.newPage();
    page.on('dialog', d => d.dismiss().catch(() => {}));
    let stage = 'goto';
    try {
      await page.goto(job.url, { waitUntil: 'domcontentloaded', timeout: left() });
      stage = 'interface';
      // The app defines it when it mounts: an app without it (not yet deployed) is told apart in at most 25 s.
      await page.waitForFunction(() => typeof window.ithomiiniMapsExport === 'function', null, { timeout: Math.min(25_000, left()) });
      stage = 'export';
      const result = await Promise.race([
        page.evaluate(() => window.ithomiiniMapsExport()),
        new Promise((_, reject) => setTimeout(() => reject(Object.assign(new Error('export did not answer in time'), { deadline: true })), left())),
      ]);
      say({ ok: true, result });
    } catch (e) {
      fail(e?.deadline || Date.now() >= deadline ? 'deadline' : stage === 'interface' ? 'no_export_interface' : 'export_failed', stage, e);
    }
  } finally {
    await browser.close().catch(() => {});
  }
}

main()
  .catch(e => fail('worker_failed', 'main', e))
  .finally(() => end(0));
