#!/usr/bin/env node
// The eval's own app and workspace (lab only), so every task starts from the same state:
//   node tools/assistant-eval/eval-app.mjs start [--port 8805] [--checkout <repo>]   app (LOCAL_MODE, dev-app.sh) + workspace
//   node tools/assistant-eval/eval-app.mjs stop  [--port 8805]
//   node tools/assistant-eval/eval-app.mjs workspace [--checkout <repo>]               rewrite the workspace (brief, skills) only
//   node tools/assistant-eval/eval-app.mjs reset [--since <iso>]                      drop proposals the eval drafted
//
// - App: tools/lab/dev-app.sh of the checkout under test (its own systemd user unit, memory ceiling, LOCAL_MODE on the
//   lab snapshot: in-memory sheets, LocalDrive sandbox, no Google writes, no WhatsApp sender), its DB a fresh copy of
//   a frozen base (VACUUM INTO of the lab's offline DB) with the eval's own MCP token for the admin `lab`.
// - Workspace: ~/.t3-ithomiini-lab/workspaces-eval/eval, a copy of the lab workspace's layout written from the
//   checkout's assistant/ (brief, skills, subagents), MCP pointing at this app, Google CLIs and wacli denied.
//   Its T3 project in the lab T3 is made once (`t3 project add`).
import { spawnSync } from 'node:child_process';
import { createHash, randomBytes } from 'node:crypto';
import { DatabaseSync } from 'node:sqlite';
import { cpSync, existsSync, mkdirSync, readFileSync, readdirSync, rmSync, symlinkSync, writeFileSync, lstatSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const HERE = dirname(fileURLToPath(import.meta.url));
const LAB = '/home/franz/.cache/ithomiini-lab';
const T3HOME = '/home/franz/.t3-ithomiini-lab';
const OUT = join(LAB, 'assistant-eval', 'evalapp');
const WS = join(T3HOME, 'workspaces-eval', 'eval');
const LAB_WS = join(T3HOME, 'workspaces', 'lab');

const argv = process.argv.slice(2);
const opt = (n, d) => (argv.includes(`--${n}`) ? argv[argv.indexOf(`--${n}`) + 1] : d);
const cmd = argv[0];
const PORT = Number(opt('port', 8805));
const CHECKOUT = opt('checkout', join(HERE, '..', '..'));
const RUN = join(OUT, `run-${PORT}`);

function base() {
  const dir = join(OUT, 'base');
  if (existsSync(join(dir, 'app.sqlite'))) return dir;
  mkdirSync(dir, { recursive: true });
  for (const f of ['app.sqlite', 'sheets.sqlite']) {
    const src = join(LAB, 'app', f);
    if (!existsSync(src)) continue;
    const db = new DatabaseSync(src, { readOnly: true });
    db.exec(`VACUUM INTO '${join(dir, f).replaceAll("'", "''")}'`);
    db.close();
  }
  const token = randomBytes(24).toString('base64url');
  const db = new DatabaseSync(join(dir, 'app.sqlite'));
  const user = db.prepare("SELECT id FROM users WHERE username = 'lab' AND active = 1").get();
  if (!user) throw new Error('No active user lab in the lab database');
  db.prepare('INSERT INTO ai_tokens (token_hash,user_id,label,created_at) VALUES (?,?,?,?)').run(createHash('sha256').update(token).digest('hex'), user.id, 'assistant eval', new Date().toISOString());
  // Start every run without pending proposals.
  db.exec("UPDATE ai_proposals SET status = 'discarded' WHERE status NOT IN ('applied','discarded')");
  db.close();
  writeFileSync(join(dir, 'token'), token, { mode: 0o600 });
  cpSync(join(LAB, 'credentials.json'), join(dir, 'credentials.json'));
  return dir;
}

async function workspace() {
  const b = base();
  mkdirSync(join(WS, '.claude'), { recursive: true });
  // Brief: the lab's composed brief (same person, same paths), with the checkout's assistant/AGENTS.md body when it differs.
  const brief = composeBrief();
  writeFileSync(join(WS, 'AGENTS.md'), brief);
  if (!existsSync(join(WS, 'CLAUDE.md'))) symlinkSync('AGENTS.md', join(WS, 'CLAUDE.md'));
  for (const sub of ['skills', 'agents']) {
    rmSync(join(WS, '.claude', sub), { recursive: true, force: true });
    cpSync(join(CHECKOUT, 'assistant', sub), join(WS, '.claude', sub), { recursive: true });
  }
  // As in production's WhatsApp workspaces: the bridge's skill (the chats' guide to the WhatsApp answer), which the
  // tasks' WhatsApp envelope names. Without it the chat loaded the older `whatsapp` relay skill instead (4 Nov eval).
  const bridgeSkill = join(CHECKOUT, 'tools', 'whatsapp', 'bridge-skill', 'SKILL.md');
  if (existsSync(bridgeSkill)) {
    mkdirSync(join(WS, '.claude', 'skills', 'whatsapp-bridge'), { recursive: true });
    cpSync(bridgeSkill, join(WS, '.claude', 'skills', 'whatsapp-bridge', 'SKILL.md'));
  }
  // The lab's settings, plus the eval's denies (Google CLIs: this PC's gog holds the project account's token).
  const settings = JSON.parse(readFileSync(join(LAB_WS, '.claude', 'settings.json'), 'utf8'));
  const extra = ['Bash(gog*)', 'Bash(*gog *)', 'Bash(*/gog*)', 'Bash(gcloud*)', 'Bash(*gcloud*)', 'Bash(*gsutil*)', 'Read(//home/franz/.config/gogcli/**)', 'Read(//home/franz/.config/gcloud/**)'];
  settings.permissions.deny = [...new Set([...(settings.permissions.deny ?? []), ...extra])];
  settings.sandbox ??= {};
  settings.sandbox.filesystem ??= {};
  settings.sandbox.filesystem.denyRead = [...new Set([...(settings.sandbox.filesystem.denyRead ?? []), '/home/franz/.config/gogcli', '/home/franz/.config/gcloud', '/home/franz/.local/bin/gog'])];
  settings.disableClaudeAiConnectors = true;
  // The eval folder is under /home/franz, so Claude Code also loads /home/franz/.claude/CLAUDE.md (Franz's own notes,
  // about 1.0k tokens) as a parent folder's CLAUDE.md. Production's /home/ubuntu has none (checked 6 Nov 2026).
  settings.claudeMdExcludes = ['/home/franz/.claude/CLAUDE.md', '/home/franz/CLAUDE.md'];
  // The checkout's own Claude Code trims, when it has them, as scripts/t3-provision.mjs applies them in production.
  const lean = join(CHECKOUT, 'scripts', 'claude-lean.mjs');
  if (existsSync(lean)) (await import(lean)).leanClaude(settings);
  writeFileSync(join(WS, '.claude', 'settings.json'), JSON.stringify(settings, null, 2));
  for (const f of ['t3-skills.json']) if (existsSync(join(LAB_WS, '.claude', f))) cpSync(join(LAB_WS, '.claude', f), join(WS, '.claude', f));
  const token = readFileSync(join(b, 'token'), 'utf8').trim();
  writeFileSync(join(WS, '.mcp.json'), JSON.stringify({ mcpServers: { db: { type: 'http', url: `http://127.0.0.1:${PORT}/api/ai/mcp`, headers: { Authorization: `Bearer ${token}` } } } }, null, 2), { mode: 0o600 });
  return WS;
}

/** The brief as production composes it (server/brief.mjs of the checkout under test, no lab section), for the admin `lab`. */
function composeBrief() {
  const code = `import(${JSON.stringify(join(CHECKOUT, 'server', 'brief.mjs'))}).then(m => process.stdout.write(m.composeBrief({ display_name: 'Lab', username: 'lab' }, { docs: ${JSON.stringify(join(CHECKOUT, 'docs'))}, knowledge: ${JSON.stringify(join(RUN, 'knowledge'))}, sheets: ${JSON.stringify(join(RUN, 'sheets.sqlite'))}, root: ${JSON.stringify(CHECKOUT)} })))`;
  const r = spawnSync(process.execPath, ['--input-type=module', '-e', code], { encoding: 'utf8' });
  if (r.status !== 0) throw new Error(`composeBrief: ${r.stderr}`);
  return r.stdout;
}

function project() {
  const marker = join(WS, '.claude', 't3-project');
  if (existsSync(marker)) return readFileSync(marker, 'utf8').trim();
  const t3 = existsSync(join(LAB, 'bin', 't3')) ? join(LAB, 'bin', 't3') : '/home/franz/.local/bin/t3';
  const r = spawnSync(t3, ['project', 'add', WS, '--title', 'Assistant eval'], { env: { ...process.env, T3CODE_HOME: T3HOME }, encoding: 'utf8' });
  if (r.status !== 0) throw new Error(`t3 project add: ${r.stderr || r.stdout}`);
  const db = new DatabaseSync(join(T3HOME, 'userdata', 'statev2.sqlite'), { readOnly: true });
  const row = db.prepare('SELECT project_id FROM projection_projects WHERE workspace_root = ? AND deleted_at IS NULL').get(WS);
  db.close();
  if (!row) throw new Error('project not found after t3 project add');
  writeFileSync(marker, row.project_id);
  return row.project_id;
}

async function start() {
  const b = base();
  rmSync(RUN, { recursive: true, force: true });
  mkdirSync(RUN, { recursive: true });
  for (const f of ['app.sqlite', 'sheets.sqlite', 'credentials.json']) if (existsSync(join(b, f))) cpSync(join(b, f), join(RUN, f));
  // The host-plant photo tags (which photo shows leaves, a flower…), as the personal lab app has them.
  const tags = '/home/franz/.cache/ithomiini-lab/app-workbook/photos/maps-cache/plant-photo-tags-v1.json';
  if (existsSync(tags)) {
    mkdirSync(join(RUN, 'photos/maps-cache'), { recursive: true });
    cpSync(tags, join(RUN, 'photos/maps-cache/plant-photo-tags-v1.json'));
  }
  const env = [
    `IDENTIFIER_DIR=${join(LAB, 'identifier')}`,
    `ITHOMIINI_T3_HOME=${T3HOME}`,
    // config.t3 (chat attachments for identify_species, photos beside proposals) exists only with a T3 URL; provisioning off.
    'ITHOMIINI_T3_URL=http://127.0.0.1:3775',
    'ITHOMIINI_T3_LOCAL=http://127.0.0.1:3775',
    'ITHOMIINI_T3_PROVISION=off',
    `ITHOMIINI_LAB_DRIVE_SANDBOX=1`,
    `DRIVE_LISTING_FILE=${join(LAB, 'drive-listing.json')}`,
    `DRIVE_ACCOUNT=jmithominii@gmail.com`,
    // maps_snapshot's app picture, as in the personal WhatsApp lab's drop-in (ithomiini-wa-lab.service.d/maps-render.conf);
    // without these files it draws with Pillow and says so (`degraded`).
    ...[['PLAYWRIGHT_MODULE', process.env.PLAYWRIGHT_MODULE ?? '/home/franz/.local/share/agent-tools/node_modules/playwright/index.mjs'],
      ['MAPS_RENDER_CHROMIUM', process.env.MAPS_RENDER_CHROMIUM ?? '/home/franz/.cache/ms-playwright/chromium_headless_shell-1228/chrome-headless-shell-linux64/chrome-headless-shell']]
      .filter(([, f]) => existsSync(f)).map(([k, f]) => `${k}=${f}`),
  ].flatMap(e => ['--env', e]);
  const r = spawnSync(join(CHECKOUT, 'tools/lab/dev-app.sh'), ['--port', String(PORT), '--dir', RUN, '--ttl', opt('ttl', '360m'), '--max', opt('max', '4G'), ...env], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] });
  if (r.status !== 0) throw new Error(`dev-app.sh: ${r.stderr || r.stdout}`);
  await workspace();
  const pid = project();
  console.log(JSON.stringify({ app: `http://127.0.0.1:${PORT}`, workspace: WS, projectId: pid, db: join(RUN, 'app.sqlite') }));
}

/** Drafts of the eval since `since` are removed, so the next task sees no pending proposal. */
// --before <iso>: only drafts made before it (tasks running in parallel keep theirs); --ids a,b: exactly these.
function reset(since = '1970') {
  const db = new DatabaseSync(join(RUN, 'app.sqlite'), { timeout: 5000 });
  const before = opt('before', '9999');
  const ids = (opt('ids', '') || '').split(',').filter(Boolean);
  let n = db.prepare("UPDATE ai_proposals SET status = 'discarded' WHERE status NOT IN ('applied','discarded') AND created_at >= ? AND created_at < ?").run(since, before).changes;
  for (const id of ids) n += db.prepare("UPDATE ai_proposals SET status = 'discarded' WHERE id = ? AND status NOT IN ('applied','discarded')").run(id).changes;
  const applied = db.prepare("SELECT count(*) n FROM ai_proposals WHERE status = 'applied' AND applied_at >= ?").get(since).n;
  db.close();
  console.log(JSON.stringify({ discarded: n, appliedSince: applied }));
}

if (cmd === 'start') await start();
else if (cmd === 'stop') spawnSync(join(CHECKOUT, 'tools/lab/dev-app.sh'), ['--stop', '--port', String(PORT)], { stdio: 'inherit' });
else if (cmd === 'workspace') console.log(await workspace());
else if (cmd === 'reset') reset(opt('since'));
else {
  console.error('usage: eval-app.mjs start|stop|workspace|reset');
  process.exit(2);
}
