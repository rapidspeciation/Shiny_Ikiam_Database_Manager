#!/usr/bin/env node
// The photo and voice endpoints of the deployed app, without running a model (Franz's rule, 9 Nov 2026: smoke checks
// are /health and direct API calls, never an AI call): /health answers, the login works, and /api/ai/extract and
// /api/ai/transcribe are there behind it and refuse a request that is not an image or audio (400), or say they are not
// configured (501), before any model is called. LIVE_SMOKE_URL, TEST_ACCOUNT_FILE (JSON {username,password}).
import { readFileSync } from 'node:fs';
import assert from 'node:assert/strict';
const base = process.env.LIVE_SMOKE_URL?.replace(/\/$/, '');
if (!base || !process.env.TEST_ACCOUNT_FILE) throw new Error('LIVE_SMOKE_URL and TEST_ACCOUNT_FILE are required');
const account = JSON.parse(readFileSync(process.env.TEST_ACCOUNT_FILE));
let cookie = '',
  csrf = '';
async function call(path, body) {
  const response = await fetch(base + path, {
    method: 'POST',
    headers: { 'content-type': 'application/json', ...(cookie ? { cookie, 'x-csrf-token': csrf } : {}) },
    body: JSON.stringify(body),
  });
  const data = await response.json().catch(() => ({}));
  if (response.headers.get('set-cookie')) cookie = response.headers.get('set-cookie').split(';')[0];
  if (data.csrf) csrf = data.csrf;
  return { status: response.status, data };
}
const health = await (await fetch(`${base}/health`)).json();
assert.equal(health.status, 'ok');
const login = await call('/api/auth/login', account);
assert.equal(login.status, 200, `/api/auth/login: ${login.status} ${login.data.error?.message || ''}`);
// Not an image / not audio: refused by the route's own checks, no model call.
for (const [path, kind] of [['/api/ai/extract', 'image'], ['/api/ai/transcribe', 'audio']]) {
  const r = await call(path, { mimeType: 'text/plain', dataBase64: 'eA==' });
  assert.ok([400, 501].includes(r.status), `${path}: ${r.status} ${r.data.error?.message || ''}`);
  console.log(`${path}: answers (${r.status}, ${r.status === 501 ? 'not configured' : `refuses a non-${kind} request`}), no model called`);
}
