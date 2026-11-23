import test from 'node:test';
import assert from 'node:assert/strict';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { composeBrief } from '../server/brief.mjs';
import { resolveEvalWorkspaceName, resolveVoiceFiles } from '../tools/assistant-eval/eval-app.mjs';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');

test('resolveVoiceFiles returns null for default app voice and ordered WhatsApp files for whatsapp-d3l', () => {
  assert.equal(resolveVoiceFiles(ROOT, 'app'), null);
  const wa = resolveVoiceFiles(ROOT, 'whatsapp-d3l');
  assert.deepEqual(
    wa.map(f => f.slice(ROOT.length + 1)),
    [
      'assistant/voice/whatsapp-d3l/whatsapp.md',
      'assistant/voice/whatsapp-d3l/core.md',
      'assistant/voice/whatsapp-d3l/humor.md',
    ],
  );
  assert.throws(() => resolveVoiceFiles(ROOT, 'no-such-voice'), /No voice files found/);
});

test('resolveEvalWorkspaceName isolates non-app voice into eval-wa by default', () => {
  assert.equal(resolveEvalWorkspaceName('app'), 'eval');
  assert.equal(resolveEvalWorkspaceName('whatsapp-d3l'), 'eval-wa');
  assert.equal(resolveEvalWorkspaceName('whatsapp-d3l', 'custom-ws'), 'custom-ws');
});

test('composeBrief with whatsapp-d3l voice includes WhatsApp voice and excludes app voice', () => {
  const appBrief = composeBrief({ display_name: 'Lab', username: 'lab' }, { root: ROOT, docs: '/d', knowledge: '/k', sheets: '/s', voice: resolveVoiceFiles(ROOT, 'app') });
  const waBrief = composeBrief({ display_name: 'Lab', username: 'lab' }, { root: ROOT, docs: '/d', knowledge: '/k', sheets: '/s', voice: resolveVoiceFiles(ROOT, 'whatsapp-d3l') });
  assert.match(waBrief, /WhatsApp/i);
  assert.ok(waBrief.length > appBrief.length, 'whatsapp-d3l baseline voice should be larger than app voice');
});
