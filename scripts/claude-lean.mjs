// Claude Code features the app's chats never use, taken out of their workspace settings: each is sent with every
// model call. Measured on the eval workspace on 6 Nov 2026 (Claude Code 2.1.317, token counts of the captured request):
//   Artifact tool (claude.ai pages)             12.5k tokens   enableArtifact: false
//   Workflow 2.0k, ScheduleWakeup 1.7k, ReportFindings 0.8k, ListAgents 0.4k       blanket deny (the tool is dropped)
//   built-in subagent types (Explore, Plan, general-purpose, claude-code-guide, web-fetch, statusline-setup, claude)
//                                               about 1.6k of the Agent tool's listing; notebook-reader/-reviewer stay
// None of them was called in the 279 production chat transcripts on claudeclaw (checked 6 Nov 2026); Agent was
// called only with notebook-reader and notebook-reviewer. Applied by scripts/t3-provision.mjs and by the eval's
// workspace (tools/assistant-eval/eval-app.mjs).
//   bundled skills listing (dataviz, code-review, claude-api, loop, schedule...)   2.3k   disableBundledSkills: true
// With these trims the photo tasks (G01, M02, N02) often skipped the maps-gallery skill and took gallery_specimens
// instead of taxon_photos (3 of 12 runs right, against 3 of 3 on the full workspace). The maps-gallery row in the
// brief's skills table and the bridge skill's "load maps-gallery first" brought it to 12 of 12 (R15, 6-7 Nov 2026).
export const UNUSED_TOOLS = ['Workflow', 'ScheduleWakeup', 'ReportFindings', 'ListAgents'];
export const UNUSED_AGENTS = ['Explore', 'Plan', 'general-purpose', 'claude-code-guide', 'web-fetch', 'statusline-setup', 'claude'];

export function leanClaude(settings) {
  settings.enableArtifact = false;
  settings.disableBundledSkills = true;
  settings.permissions ??= {};
  settings.permissions.deny = [...new Set([...(settings.permissions.deny ?? []), ...UNUSED_TOOLS, ...UNUSED_AGENTS.map(a => `Agent(${a})`)])];
  return settings;
}

/** The rollback: takes out exactly what leanClaude adds (none of these keys or deny rules was set before R15). */
export function unleanClaude(settings) {
  delete settings.enableArtifact;
  delete settings.disableBundledSkills;
  const ours = new Set([...UNUSED_TOOLS, ...UNUSED_AGENTS.map(a => `Agent(${a})`)]);
  if (settings.permissions?.deny) settings.permissions.deny = settings.permissions.deny.filter(rule => !ours.has(rule));
  return settings;
}
