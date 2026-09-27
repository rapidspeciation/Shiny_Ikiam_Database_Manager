#!/usr/bin/env node
/**
 * Background processor for Wikiloc links pasted or shared in the app, and for
 * "Buscar nuevos en Wikiloc" on followed profiles. Runs on a computer with a
 * home connection (Wikiloc's Cloudflare check blocks the app server).
 *
 * Every 30 s it asks the app for a waiting job. A link job reads that trail;
 * a profile job lists the profile's public trails and reads those whose title
 * matches the profile's pattern (default "monitoreo") and are not in the app
 * yet. Walks are sent to the app for review. The browser only starts when
 * there is work.
 *
 * Credentials: ~/.config/ithomiini-wikiloc/worker.json (mode 600) with
 * {"app": "...", "username": "...", "password": "..."}; see README.md for the
 * systemd user service.
 */
import { readFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { join } from 'node:path';
import { DEFAULT_APP, appClient, openBrowser, photoCount, readProfile, readTrail, signIn } from './lib.mjs';

const config = JSON.parse(
  readFileSync(process.env.ITHOMIINI_WORKER_CONFIG || join(homedir(), '.config', 'ithomiini-wikiloc', 'worker.json'), 'utf8'),
);
const app = (config.app || DEFAULT_APP).replace(/\/?$/, '/');
const client = appClient(app, () => signIn(app, config.username, config.password));
const sleep = ms => new Promise(r => setTimeout(r, ms));
const log = (...a) => console.log(new Date().toISOString(), ...a);

async function sendWalk(page, url, jobId, collector) {
  const walk = await readTrail(page, url);
  const saved = await client.request('monitoring/wikiloc', { method: 'POST', body: { ...walk, jobId, collector } });
  const failed = saved.failed?.length ? `, ${saved.failed.length} fotos fallaron` : '';
  return `${walk.name}: ${walk.waypoints.length} puntos, ${photoCount(walk)} fotos${failed}`;
}

async function run(job) {
  const browser = await openBrowser();
  try {
    if (job.kind === 'trail') return await sendWalk(browser.page, job.target, job.id);
    const pattern = new RegExp(job.profile?.pattern || 'monitoreo', 'i');
    const { name, trails } = await readProfile(browser.page, job.target);
    const known = new Set(job.knownIds || []);
    const fresh = trails.filter(t => pattern.test(t.title) && !known.has(t.wikilocId));
    const done = [];
    for (const t of fresh) {
      await browser.page.waitForTimeout(4000);
      done.push(await sendWalk(browser.page, t.url, job.id, job.profile?.collector));
    }
    return {
      profileName: name,
      message: fresh.length
        ? `${fresh.length} rutas nuevas de ${trails.length}: ${done.join('; ')}`
        : `Sin rutas nuevas (${trails.length} rutas revisadas)`,
    };
  } finally {
    await browser.close();
  }
}

log(`Wikiloc worker for ${app}`);
for (;;) {
  let job = null;
  try {
    ({ job } = await client.request('monitoring/wikiloc/jobs/claim', { method: 'POST', body: {} }));
    if (!job) {
      await sleep(30_000);
      continue;
    }
    log(`job ${job.kind} ${job.target}`);
    const result = await run(job);
    const message = typeof result === 'string' ? result : result.message;
    await client.request(`monitoring/wikiloc/jobs/${job.id}`, {
      method: 'POST',
      body: { status: 'done', message, profileName: result.profileName },
    });
    log(`done: ${message}`);
  } catch (e) {
    log(`error: ${e.message}`);
    if (job)
      await client
        .request(`monitoring/wikiloc/jobs/${job.id}`, { method: 'POST', body: { status: 'failed', message: e.message.slice(0, 500) } })
        .catch(() => {});
    await sleep(60_000);
  }
}
