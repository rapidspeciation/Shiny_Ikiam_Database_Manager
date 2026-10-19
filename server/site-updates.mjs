// The two public sites that show the workbook's data, the Wings Gallery and the maps, take it when
// their repository's «Update Database» workflow runs on GitHub: it reads the Google Sheets, commits
// the new data files and starts the site's «Deploy to GitHub Pages». Each site's own update button
// starts that workflow through a Cloudflare Worker that holds a GitHub token and asks for a password;
// update_websites asks the same workers, so this app needs no GitHub token of its own.
//
// The workers' password is in the file SITE_UPDATE_PASSWORD_FILE (mode 600). Without it the tool
// still says when each site was last updated and how its publishing went (the repositories are public).

import { existsSync, readFileSync } from 'node:fs';

export const SITES = {
  gallery: {
    name: 'Wings Gallery',
    url: 'https://rapidspeciation.github.io/Shiny_Ikiam_Wings_Gallery/',
    repo: 'rapidspeciation/Shiny_Ikiam_Wings_Gallery',
    worker: 'https://ikiam-db-updater.franz-chandi.workers.dev/',
    update: 'update_data.yml',
    deploy: 'deploy.yml',
  },
  maps: {
    name: 'Ithomiini maps',
    url: 'https://rapidspeciation.github.io/ithomiini_maps/',
    repo: 'rapidspeciation/ithomiini_maps',
    worker: 'https://ithomiini-maps-db-updater.franz-chandi.workers.dev/',
    update: 'update_data.yml',
    deploy: 'deploy.yml',
    // The workbook's data always; the museum and citizen-science records only when asked (a long download).
    body: gbif => ({ owner: 'rapidspeciation', repo: 'ithomiini_maps', update_sanger: true, update_gbif: gbif }),
  },
};

export const SITE_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'update_websites',
      description: [
        "Brings the project's public sites up to date with the workbook: the Wings Gallery (`gallery`: the wing photos listed in Photo_links and their butterflies' data) and the maps (`maps`: the collecting localities). Each takes the data when its «Update Database» run on GitHub reads the sheets; that run then publishes the site again. The «Fotos» tab has the same buttons.",
        '- `sites`: which to update. The call waits about a minute and a half: the gallery ends within it; the maps take 5 to 10 minutes (`running`), so call again later without `sites` to see how they ended.',
        '- Without `sites`: when each site was last updated, how that run went, and its publishing.',
        '- `gbif: true`: the maps also download the museum and citizen-science records again (GBIF, iNaturalist): about a quarter of an hour more, wanted a few times a year.',
        'The gallery shows a photo once Photo_links lists it (sync_photo_links) and the workbook has recalculated.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          sites: { type: 'array', items: { type: 'string', enum: Object.keys(SITES) } },
          gbif: { type: 'boolean' },
        },
      },
    },
  },
];
export const SITE_TOOL_NAMES = new Set(SITE_TOOLS.map(t => t.function.name));

const EDITORS = ['editor', 'reviewer', 'admin'];
const API = 'https://api.github.com';
/** How long a call follows the runs it started. */
const WAIT_MS = 90_000;
const STEP_MS = 15_000;
/**
 * GitHub answers 60 unsigned requests an hour from the server: a workflow's runs are asked again after
 * this long, or (`fresh`, a run going on) after FRESH_MS; refused for that limit, the last ones are told.
 */
const HELD_MS = 5 * 60_000;
const FRESH_MS = 20_000;

/**
 * update_websites for one app. `passwordFile`: the workers' password (config.siteUpdatePasswordFile);
 * `fetchImpl`, `waitMs`, `stepMs`, `freshMs`, `now`: for tests.
 */
export function createSiteUpdates({ passwordFile = null, fetchImpl = fetch, waitMs = WAIT_MS, stepMs = STEP_MS, freshMs = FRESH_MS, now = Date.now } = {}) {
  const password = () => (passwordFile && existsSync(passwordFile) ? readFileSync(passwordFile, 'utf8').trim() : '');
  const held = new Map();

  async function github(path) {
    const response = await fetchImpl(`${API}${path}`, {
      headers: { accept: 'application/vnd.github+json', 'x-github-api-version': '2022-11-28', 'user-agent': 'ithomiini-database' },
      signal: AbortSignal.timeout(20_000),
    });
    if (!response.ok) {
      const said = await response.text().catch(() => '');
      throw Object.assign(new Error(`GitHub answered ${response.status}${said ? `: ${said.slice(0, 160)}` : ''}`), { status: response.status });
    }
    return response.json();
  }
  /** A workflow's latest runs; `fresh`: asked again unless they were a moment ago; `force`: asked again. */
  async function runsOf(site, workflow, { fresh = false, force = false } = {}) {
    const key = `${site.repo} ${workflow}`;
    const kept = held.get(key);
    if (!force && kept && now() - kept.at < (fresh ? freshMs : HELD_MS)) return kept.runs;
    try {
      const runs = (await github(`/repos/${site.repo}/actions/workflows/${workflow}/runs?per_page=3`)).workflow_runs ?? [];
      held.set(key, { at: now(), runs });
      return runs;
    } catch (e) {
      if (kept && (e.status === 403 || e.status === 429)) return kept.runs;
      throw e;
    }
  }
  /** Whether the runs last read of a site show one going on (only those are asked again often). */
  const going = site =>
    [site.update, site.deploy].some(w => held.get(`${site.repo} ${w}`)?.runs?.some(r => r.status !== 'completed'));
  /** Asks the site's worker to start its update: null, or what went wrong. */
  async function start(site, gbif) {
    const response = await fetchImpl(site.worker, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ password: password(), ...(site.body?.(gbif) ?? {}) }),
      signal: AbortSignal.timeout(30_000),
    });
    if (response.ok) return null;
    const said = (await response.text().catch(() => '')).slice(0, 200);
    if (response.status === 401) return 'the update server refused the password (SITE_UPDATE_PASSWORD_FILE)';
    return `the update server answered ${response.status}${said ? `: ${said}` : ''}`;
  }
  const shown = run =>
    run && {
      started: run.created_at,
      state: run.status === 'completed' ? run.conclusion : 'running',
      ...(run.status === 'completed' ? { ended: run.updated_at } : {}),
      link: run.html_url,
    };

  /** The publishing that followed an update (the update starts it once it has committed new data). */
  async function publishing(site, update, fresh = false) {
    const deploys = await runsOf(site, site.deploy, { fresh });
    if (update?.status !== 'completed') return deploys[0] ? { publishing: shown(deploys[0]) } : {};
    const after = deploys.find(d => d.created_at >= update.created_at);
    if (after) return { publishing: shown(after) };
    // Ended well a moment ago: its publishing may not be listed yet.
    if (update.conclusion === 'success' && now() - Date.parse(update.updated_at) < 60_000)
      return { publishing: { state: 'running', note: 'starting: the site shows the new data in about two minutes' } };
    return deploys[0] ? { publishing: shown(deploys[0]), ...(update.conclusion === 'success' ? { note: 'the update found no new data to publish' } : {}) } : {};
  }

  async function status(key, fresh = false) {
    const site = SITES[key];
    const again = fresh && going(site);
    const [update] = await runsOf(site, site.update, { fresh: again });
    return { site: site.name, url: site.url, lastUpdate: shown(update) ?? 'never run', ...(await publishing(site, update, again)) };
  }

  /** Starts a site's update and (`wait`) follows it until it ends or the wait is over. */
  async function update(key, { gbif = false, wait = true } = {}) {
    const site = SITES[key];
    const before = await runsOf(site, site.update, { force: true });
    if (before.some(r => r.status !== 'completed'))
      return { site: site.name, url: site.url, update: { ...shown(before.find(r => r.status !== 'completed')), note: 'an update was already running: this one was not started' } };
    const last = before[0]?.id ?? null;
    const refused = await start(site, gbif);
    if (refused) return { site: site.name, url: site.url, error: refused };
    const until = now() + (wait ? waitMs : Math.min(waitMs, 15_000));
    let run = null;
    while (now() < until) {
      await new Promise(resolve => setTimeout(resolve, stepMs));
      run = (await runsOf(site, site.update, { force: true }))[0] ?? null;
      if (run?.id === last) run = null;
      if (run && (run.status === 'completed' || !wait)) break;
    }
    if (!run) return { site: site.name, url: site.url, update: { state: 'running', note: 'started; GitHub has not listed the run yet' } };
    return { site: site.name, url: site.url, update: shown(run), ...(run.status === 'completed' ? await publishing(site, run, true) : {}) };
  }

  /** `wait: false` (the «Fotos» tab's buttons): answers once GitHub lists the run, without following it. */
  async function run(args = {}, user, { wait = true } = {}) {
    if (!EDITORS.includes(user?.role)) return { error: 'Your role cannot update the sites' };
    const asked = [...new Set((Array.isArray(args.sites) ? args.sites : args.sites ? [args.sites] : []).map(s => String(s).toLowerCase()))];
    const wrong = asked.filter(s => !SITES[s]);
    if (wrong.length) return { error: `sites: ${Object.keys(SITES).join(' or ')} (${wrong.join(', ')} given)` };
    if (asked.length && !password())
      return { error: "This app has no password set up for the sites' update server (SITE_UPDATE_PASSWORD_FILE). Without `sites` the tool says when each was last updated." };
    try {
      const sites = await Promise.all(
        asked.length ? asked.map(key => update(key, { gbif: args.gbif === true, wait })) : Object.keys(SITES).map(key => status(key, args.fresh === true)),
      );
      return {
        sites,
        ...(sites.some(s => s.update?.state === 'running' || s.publishing?.state === 'running' || s.lastUpdate?.state === 'running')
          ? { next: 'Still running: call update_websites without `sites` in a few minutes to see how it ended.' }
          : {}),
      };
    } catch (e) {
      return { error: `GitHub could not be reached: ${e?.message || e}` };
    }
  }
  return {
    run,
    ready: () => !!password(),
    /** Starts a site's update without a person (the photo readings, server/readings.mjs): answers once GitHub lists it. */
    start: key => (password() ? update(key, { wait: false }) : Promise.resolve({ error: 'no password for the update server (SITE_UPDATE_PASSWORD_FILE)' })),
  };
}
