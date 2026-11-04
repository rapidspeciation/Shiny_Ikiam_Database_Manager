// A stand-in for playwright-core's module, for tests/maps-render.test.mjs: the real server/maps-render-worker.mjs
// imports it as PLAYWRIGHT_MODULE. The scenario is a JSON file named in the import URL (`?s=<path>`):
// { result | app: {records, png, geometry} (an app that answers from the URL, with picture geometry unless geometry: false), launchFail, hang: 'launch'|'export',
//   noInterface, grandchild (a process left in the worker's group), browserGroup (tests/fixtures/fake-browser.mjs: a
//   browser-like process in a group of its own, its profile in TMPDIR, with a helper child in another group), log (a
//   file: the URL opened, the code evaluated, TMPDIR, the pids started) }.
import { appendFileSync, readFileSync, writeFileSync } from 'node:fs';
import { spawn } from 'node:child_process';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { EXPORT, fitBoundsView } from '../../server/maps-render.mjs';
import { withGeometry } from './export-geometry.mjs';

const scenario = JSON.parse(readFileSync(new URL(import.meta.url).searchParams.get('s'), 'utf8'));
const log = (k, v) => scenario.log && appendFileSync(scenario.log, `${JSON.stringify({ [k]: v })}\n`);
const forever = () => new Promise(() => setInterval(() => {}, 1000));

/** What the supported interface would return for this URL: the app's filters on `app.records`, fitBounds of bbox=. */
function appAnswer(url) {
  const p = new URL(url).searchParams;
  const sp = new Set((p.get('sp') ?? '').split(',').filter(Boolean));
  const sources = new Set((p.get('source') ?? '').split(','));
  const countries = new Set((p.get('country') ?? '').split(',').filter(Boolean));
  const sex = p.get('sex');
  const records = scenario.app.records.filter(r => (!sp.size || sp.has(r.name)) && sources.has(r.source) && (!countries.size || countries.has(r.country)) && (!sex || r.sex === sex));
  const bbox = p.get('bbox').split(',').map(Number);
  const frame = scenario.app.frame ?? EXPORT.frame;
  const fitted = fitBoundsView(scenario.app.ignoreBbox ? [-80, -4, -75, 2] : bbox, frame);
  const bySsp = new Map();
  for (const r of records) bySsp.set(r.ssp ?? null, (bySsp.get(r.ssp ?? null) ?? 0) + 1);
  const places = new Map();
  for (const r of records) (places.get(r.place) ?? places.set(r.place, []).get(r.place)).push(`${r.source}|${r.id}`);
  const answer = {
    version: 1,
    png_base64: readFileSync(scenario.app.png).toString('base64'),
    width: 1600,
    height: 1200,
    params: { applied: Object.fromEntries(p), ignored: [] },
    basemap: p.get('style'),
    color_by: p.get('color_by'),
    title: `${[...sp].join(', ')}${sex ? ` · ${sex}s` : ''}`,
    view: { center: fitted.center, zoom: fitted.zoom, bbox: fitted.bbox, frame, fit: { padding: EXPORT.padding, max_zoom: EXPORT.maxZoom } },
    records: records.map(({ source, id, sex: s, country, year, lng, lat, ssp }) => ({ source, id, sex: s ?? null, country, year, subspecies: ssp ?? null, lng: lng + (scenario.app.shift ?? 0), lat })),
    legend: { unit: 'records', rows: [...bySsp].map(([label, n]) => ({ label: label ?? 'Unknown subspecies', records: n, unknown: label === null })) },
    sites: [...places].map(([label, ids]) => ({ label, merged_sites: 0, members: [{ key: label, locality: label, record_ids: ids }] })),
  };
  // `app.geometry`: false for an app without the picture geometry; { overlays } to place them.
  return scenario.app.geometry === false ? answer : withGeometry(answer, scenario.app.geometry ?? {});
}

const page = {
  on() {},
  async goto(url) {
    log('url', url);
    this.url = url;
  },
  async waitForFunction(fn) {
    log('wait', String(fn));
    if (scenario.noInterface) throw new Error('Timeout waiting for window.ithomiiniMapsExport');
  },
  async evaluate(fn) {
    log('evaluate', String(fn));
    if (scenario.hang === 'export') await forever();
    return scenario.result ?? appAnswer(this.url);
  },
};

export const chromium = {
  executablePath: () => process.execPath,
  async launch(options) {
    log('tmpdir', process.env.TMPDIR);
    log('worker', process.pid);
    writeFileSync(join(process.env.TMPDIR, 'profile-file'), 'x');
    if (scenario.grandchild) {
      const g = spawn(process.execPath, ['-e', 'setInterval(() => {}, 1000)'], { stdio: 'ignore' });
      log('grandchild', g.pid);
    }
    if (scenario.browserGroup) {
      // As Playwright starts Chromium: a process group of its own, the profile in TMPDIR, a child of its own.
      // (It and its helper log their own pids.)
      spawn(process.execPath, [fileURLToPath(new URL('./fake-browser.mjs', import.meta.url)), scenario.log, 'browser', `--user-data-dir=${join(process.env.TMPDIR, 'profile')}`], { detached: true, stdio: 'ignore' });
    }
    if (scenario.launchFail) throw new Error(scenario.launchFail);
    if (scenario.hang === 'launch') await forever();
    log('launch', { headless: options.headless, args: options.args });
    return { newContext: async o => (log('context', o), { newPage: async () => page }), close: async () => {} };
  },
};
