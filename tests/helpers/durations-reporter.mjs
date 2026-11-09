// node --test reporter for scripts/test-server.mjs: what each test file took, so the next run starts the longest first
// and fills its groups evenly. One JSON object at the end: { file: { tests_ms, wall_ms } }. tests_ms: its top-level
// tests' time (also when the file ran in a group); wall_ms: its own process from start to end (files run alone only).
import { basename } from 'node:path';

export default async function* durations(source) {
  const files = {};
  const of = file => (files[file] ??= { tests_ms: 0, wall_ms: null });
  for await (const ev of source) {
    if (ev.type !== 'test:complete' || ev.data.nesting !== 0 || !ev.data.file) continue;
    const { file, name, details } = ev.data;
    const ms = Math.round(details?.duration_ms ?? 0);
    // The file's own entry (its process) is named after the file.
    if (name === file || name === basename(file) || file.endsWith(`/${name}`)) of(file).wall_ms = ms;
    else of(file).tests_ms += ms;
  }
  yield JSON.stringify(files) + '\n';
}
