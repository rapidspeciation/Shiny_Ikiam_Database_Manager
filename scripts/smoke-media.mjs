#!/usr/bin/env node
import { readFileSync } from 'node:fs';
import assert from 'node:assert/strict';
const base = process.env.LIVE_SMOKE_URL?.replace(/\/$/, '');
const account = JSON.parse(readFileSync(process.env.TEST_ACCOUNT_FILE));
let cookie = '', csrf = '';
async function call(path, body) {
  const response = await fetch(base + path, { method: 'POST', headers: { 'content-type': 'application/json', ...(cookie ? { cookie, 'x-csrf-token': csrf } : {}) }, body: JSON.stringify(body) });
  const data = await response.json();
  assert.equal(response.ok, true, `${path}: ${response.status} ${data.error?.message || ''}`);
  if (response.headers.get('set-cookie')) cookie = response.headers.get('set-cookie').split(';')[0];
  if (data.csrf) csrf = data.csrf;
  return data;
}
await call('/api/auth/login', account);
const image = await call('/api/ai/extract', { mimeType: 'image/png', dataBase64: readFileSync(process.env.TEST_IMAGE_FILE).toString('base64') });
assert.equal(image.requiresReview, true);
assert.match(image.text, /CAM\s*12345/i);
console.log('Live image extraction recognized the synthetic CAM label and requires review');
const audio = await call('/api/ai/transcribe', { mimeType: 'audio/wav', dataBase64: readFileSync(process.env.TEST_AUDIO_FILE).toString('base64') });
assert.equal(audio.requiresReview, true);
assert.match(audio.text, /field note|butterfly/i);
console.log('Live audio transcription recognized the synthetic field note and requires review');
