// Shared helper for registering the standalone Antigravity stdio MCP proxy across
// `<t3Home>/userdata/providers/antigravity/*/config/mcp_config.json`. Used by both
// `scripts/t3-provision.mjs` and `tools/assistant-eval/eval-app.mjs`.
import { randomBytes } from 'node:crypto';
import { chmodSync, existsSync, mkdirSync, readFileSync, readdirSync, renameSync, rmSync, statSync, writeFileSync } from 'node:fs';
import { basename, dirname, join } from 'node:path';

/**
 * Writes `content` to `filePath` atomically with exact `mode`, skipping the write
 * when the existing file already matches both bytes and permissions.
 */
function writeAtomicIfChanged(filePath, content, mode) {
  const buf = Buffer.isBuffer(content) ? content : Buffer.from(content, 'utf8');
  try {
    const st = statSync(filePath);
    if (st.isFile() && (st.mode & 0o777) === mode && readFileSync(filePath).equals(buf)) return false;
  } catch {
    /* missing or unreadable */
  }
  const tmp = join(dirname(filePath), `.${basename(filePath)}.${process.pid}.${randomBytes(4).toString('hex')}.tmp`);
  try {
    writeFileSync(tmp, buf, { mode });
    chmodSync(tmp, mode);
    renameSync(tmp, filePath);
  } catch (err) {
    rmSync(tmp, { force: true });
    throw err;
  }
  return true;
}

/**
 * Copies `sourceProxy` to `proxyPath` (mode `0o555`, atomic, skip if unchanged) and updates every
 * Antigravity profile's `config/mcp_config.json` (mode `0o600`, atomic, skip if unchanged) to
 * point `mcpServers[serverName]` at `{ command: nodeBin, args: [proxyPath] }`.
 * Clean no-op when no Antigravity profiles with a `config` directory exist.
 * Malformed `mcp_config.json` files are warned and skipped without overwriting or aborting.
 */
export function syncAntigravityMcp({
  sourceProxy,
  proxyPath,
  proxyDirMode = null,
  t3Home,
  serverName = 'db',
  nodeBin = process.execPath,
  warn = msg => console.warn(msg),
}) {
  if (!t3Home || !sourceProxy || !proxyPath || !existsSync(sourceProxy)) {
    return { profiles: 0, proxyUpdated: false, updated: 0, skipped: 0 };
  }
  const rootDir = join(t3Home, 'userdata', 'providers', 'antigravity');
  if (!existsSync(rootDir)) {
    return { profiles: 0, proxyUpdated: false, updated: 0, skipped: 0 };
  }
  const profiles = readdirSync(rootDir, { withFileTypes: true }).filter(
    d => d.isDirectory() && existsSync(join(rootDir, d.name, 'config')) && statSync(join(rootDir, d.name, 'config')).isDirectory(),
  );
  if (!profiles.length) {
    return { profiles: 0, proxyUpdated: false, updated: 0, skipped: 0 };
  }

  const proxyDir = dirname(proxyPath);
  mkdirSync(proxyDir, { recursive: true, mode: proxyDirMode ?? 0o755 });
  if (proxyDirMode != null) chmodSync(proxyDir, proxyDirMode);
  const proxyUpdated = writeAtomicIfChanged(proxyPath, readFileSync(sourceProxy), 0o555);

  let updated = 0;
  let skipped = 0;
  for (const d of profiles) {
    const file = join(rootDir, d.name, 'config', 'mcp_config.json');
    let data = {};
    if (existsSync(file)) {
      const raw = readFileSync(file, 'utf8').trim();
      if (raw) {
        let parsed;
        try {
          parsed = JSON.parse(raw);
        } catch (err) {
          warn(`Skipped malformed Antigravity MCP config ${file}: ${err.message}`);
          skipped++;
          continue;
        }
        if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) {
          warn(`Skipped invalid Antigravity MCP config ${file}: expected a JSON object`);
          skipped++;
          continue;
        }
        data = parsed;
      }
    }
    const mcpServers =
      data.mcpServers && typeof data.mcpServers === 'object' && !Array.isArray(data.mcpServers)
        ? { ...data.mcpServers }
        : {};
    mcpServers[serverName] = { command: nodeBin, args: [proxyPath] };
    const text = JSON.stringify({ ...data, mcpServers }, null, 2) + '\n';
    if (writeAtomicIfChanged(file, text, 0o600)) updated++;
  }
  return { profiles: profiles.length, proxyUpdated, updated, skipped };
}
