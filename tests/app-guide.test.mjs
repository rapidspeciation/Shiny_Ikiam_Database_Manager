import test from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync, readdirSync } from 'node:fs';

const dir = new URL('../assistant/skills/app-guide/', import.meta.url);
const files = ['SKILL.md', ...readdirSync(new URL('reference/', dir)).map(f => `reference/${f}`)];
const text = files.map(f => readFileSync(new URL(f, dir), 'utf8')).join('\n');
const router = readFileSync(new URL('../frontend/src/router.ts', import.meta.url), 'utf8');

test('the app guide has the skill frontmatter and links its reference files', () => {
  const skill = readFileSync(new URL('SKILL.md', dir), 'utf8');
  assert.match(skill, /^---\nname: app-guide\ndescription: .+\n---\n/);
  for (const f of files.slice(1)) assert.ok(skill.includes(`(${f})`), `SKILL.md links ${f}`);
});

test('every route the app guide links to exists in the router', () => {
  const routes = new Set([...router.matchAll(/path: '\/([a-z]*)'/g)].map(m => m[1]));
  // Being added in parallel (the proposals panel in its own tab).
  routes.add('propuestas');
  const linked = new Set([...text.matchAll(/#\/([a-z]+)/g)].map(m => m[1]));
  assert.ok(linked.size >= 12);
  for (const route of linked) assert.ok(routes.has(route), `#/${route} is a route`);
});
