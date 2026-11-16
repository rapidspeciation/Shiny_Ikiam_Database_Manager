// The arithmetic rule every chat reads (16 Nov 2026: a reply added 10+13+9+12+4 in its head and wrote 41).
import test from 'node:test';
import assert from 'node:assert/strict';
import { readdirSync } from 'node:fs';
import { join } from 'node:path';
import { composeBrief, GENERIC_PERSON, SERVER_PATHS } from '../server/brief.mjs';

test('the app brief and the WhatsApp brief both say to do arithmetic with bash or python, a worked-out total after its list', () => {
  const wa = join(import.meta.dirname, '..', 'assistant/voice/whatsapp-d3l');
  for (const voice of [null, readdirSync(wa).filter(f => f.endsWith('.md')).map(f => join(wa, f))]) {
    const brief = composeBrief(GENERIC_PERSON, { ...SERVER_PATHS, voice }).replace(/\s+/g, ' ');
    const where = voice ? 'WhatsApp' : 'app';
    assert.match(brief, /Do any arithmetic, even a sum of two numbers, with bash or python \(`python3 -c 'print\(10\+13\+9\+12\+4\)'`\), never in your head/, where);
    assert.match(brief, /A total you work out goes after the list it sums/, where);
  }
});

