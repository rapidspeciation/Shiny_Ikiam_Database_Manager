// The two public sites that show the workbook's data, the Wings Gallery and the maps, take it when
// their repository's «Update Database» workflow runs on GitHub: it reads the Google Sheets and
// commits the new data files. That commit does not publish the site by itself (a workflow's own
// commit starts no other workflow), so the site's «Deploy to GitHub Pages» workflow is started
// after it. update_websites does both and says how each run went.
//
// It needs a GitHub token that may run the workflows of those two repositories and nothing else
// (a fine-grained token, Actions: read and write), in the file GITHUB_ACTIONS_TOKEN_FILE. Without
// it the tool still says when each site was last updated (the repositories are public).

import { existsSync, readFileSync } from 'node:fs';

export const SITES = {
  gallery: {
    name: 'Wings Gallery',
    url: 'https://rapidspeciation.github.io/Shiny_Ikiam_Wings_Gallery/',
    repo: 'rapidspeciation/Shiny_Ikiam_Wings_Gallery',
    update: 'update_data.yml',
    deploy: 'deploy.yml',
  },
  maps: {
    name: 'Ithomiini maps',
    url: 'https://rapidspeciation.github.io/ithomiini_maps/',
    repo: 'rapidspeciation/ithomiini_maps',
    update: 'update_data.yml',
    deploy: 'deploy.yml',
    // The workbook's data always; the museum and citizen-science records only when asked (a long download).
    inputs: gbif => ({ update_sanger: 'true', update_gbif: gbif ? 'true' : 'false' }),
  },
};

export const SITE_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'update_websites',
      description: [
        "Brings the project's public sites up to date with the workbook: the Wings Gallery (`gallery`: the wing photos listed in Photo_links and their butterflies' data) and the maps (`maps`: the collecting localities). Each takes the data when its «Update Database» run on GitHub reads the sheets; the site is then published again.",
        '- `sites`: which to update. The call waits about a minute and a half: the gallery ends within it; the maps take 5 to 10 minutes (`running`), so call again later without `sites` to see how they ended.',
        '- Without `sites`: when each site was last updated and how that run went, and the publishing of a finished update is started if it is still to do.',
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
const STEP_MS = 5_000;

/**
 * update_websites for one app. `tokenFile`: the GitHub token's file (config.githubTokenFile);
 * `fetchImpl`, `waitMs`, `stepMs`, `now`: for tests.
 */
export function createSiteUpdates({ tokenFile = null, fetchImpl = fetch, waitMs = WAIT_MS, stepMs = STEP_MS, now = Date.now } = {}) {
  const token = () => (tokenFile && existsSync(tokenFile) ? readFileSync(tokenFile, 'utf8').trim() : '');
  /** Data commits whose publishing this app started (a run takes a moment to show in GitHub's list). */
  const published = new Set();

  async function github(path, { method = 'GET', body = null } = {}) {
    const key = token();
    const response = await fetchImpl(`${API}${path}`, {
      method,
      headers: {
        accept: 'application/vnd.github+json',
        'x-github-api-version': '2022-11-28',
        'user-agent': 'ithomiini-database',
        ...(key ? { authorization: `Bearer ${key}` } : {}),
        ...(body ? { 'content-type': 'application/json' } : {}),
      },
      ...(body ? { body: JSON.stringify(body) } : {}),
      signal: AbortSignal.timeout(20_000),
    });
    if (!response.ok) {
      const said = await response.text().catch(() => '');
      throw Object.assign(new Error(`GitHub answered ${response.status}${said ? `: ${said.slice(0, 160)}` : ''}`), { status: response.status });
    }
    return response.status === 204 ? null : response.json();
  }
  const runsOf = async (site, workflow, n = 5) =>
    (await github(`/repos/${site.repo}/actions/workflows/${workflow}/runs?per_page=${n}`)).workflow_runs ?? [];
  const start = (site, workflow, inputs = undefined) =>
    github(`/repos/${site.repo}/actions/workflows/${workflow}/dispatches`, { method: 'POST', body: { ref: 'main', ...(inputs ? { inputs } : {}) } });
  const shown = run =>
    run && {
      started: run.created_at,
      state: run.status === 'completed' ? run.conclusion : 'running',
      ...(run.status === 'completed' ? { ended: run.updated_at } : {}),
      link: run.html_url,
    };

  /**
   * After an update that ended well: whether it left new data (the branch's last commit is the
   * workflow's own, newer than the last publishing) and, with a token, the publishing started.
   */
  async function publish(site, update) {
    if (update?.status !== 'completed' || update.conclusion !== 'success') return null;
    const [head, deploys] = await Promise.all([github(`/repos/${site.repo}/commits/main`), runsOf(site, site.deploy, 5)]);
    const last = deploys[0];
    const byWorkflow = /^(🔄 )?Auto-update database/.test(head?.commit?.message ?? '');
    if (!byWorkflow || deploys.some(d => d.head_sha === head.sha)) return last ? { publishing: shown(last) } : null;
    if (published.has(head.sha)) return { publishing: { state: 'running', note: 'started a moment ago' } };
    if (!token()) return { publishing: { state: 'waiting', note: 'the new data is in the repository, not on the site yet: publishing it needs the GitHub token' } };
    await start(site, site.deploy);
    published.add(head.sha);
    return { publishing: { state: 'running', note: 'started now: the site shows the new data in about two minutes' } };
  }

  async function status(key) {
    const site = SITES[key];
    const [update] = await runsOf(site, site.update, 1);
    return { site: site.name, url: site.url, lastUpdate: shown(update) ?? 'never run', ...((await publish(site, update)) ?? {}) };
  }

  async function update(key, { gbif = false } = {}) {
    const site = SITES[key];
    const before = (await runsOf(site, site.update, 1))[0]?.id ?? null;
    if ((await runsOf(site, site.update, 3)).some(r => r.status !== 'completed'))
      return { site: site.name, url: site.url, update: { state: 'running', note: 'an update was already running: this one was not started' } };
    await start(site, site.update, site.inputs?.(gbif));
    const until = now() + waitMs;
    let run = null;
    while (now() < until) {
      await new Promise(resolve => setTimeout(resolve, stepMs));
      run = (await runsOf(site, site.update, 1))[0] ?? null;
      if (run && run.id !== before && run.status === 'completed') break;
      if (run?.id === before) run = null;
    }
    if (!run) return { site: site.name, url: site.url, update: { state: 'running', note: 'started; GitHub has not listed the run yet' } };
    return { site: site.name, url: site.url, update: shown(run), ...((await publish(site, run)) ?? {}) };
  }

  return {
    async run(args = {}, user) {
      if (!EDITORS.includes(user?.role)) return { error: 'Your role cannot update the sites' };
      const asked = [...new Set((Array.isArray(args.sites) ? args.sites : args.sites ? [args.sites] : []).map(s => String(s).toLowerCase()))];
      const wrong = asked.filter(s => !SITES[s]);
      if (wrong.length) return { error: `sites: ${Object.keys(SITES).join(' or ')} (${wrong.join(', ')} given)` };
      if (asked.length && !token())
        return { error: 'This app has no GitHub token set up to start the sites\' updates (GITHUB_ACTIONS_TOKEN_FILE). Without `sites` the tool says when each was last updated.' };
      try {
        const sites = await Promise.all(asked.length ? asked.map(key => update(key, { gbif: args.gbif === true })) : Object.keys(SITES).map(status));
        return {
          sites,
          ...(sites.some(s => s.update?.state === 'running' || s.publishing?.state === 'running')
            ? { next: 'Still running: call update_websites without `sites` in a few minutes to see how it ended (and have a finished update published).' }
            : {}),
        };
      } catch (e) {
        return {
          error:
            e?.status === 401 || e?.status === 403
              ? `GitHub refused (${e.status}): the token may have run out, or lacks the Actions permission on the site's repository.`
              : `GitHub could not be reached: ${e?.message || e}`,
        };
      }
    },
  };
}
