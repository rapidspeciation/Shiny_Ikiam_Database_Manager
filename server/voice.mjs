// Live voice calls with the assistant. The browser talks to the voice model
// directly (audio never passes through this server) with a short-lived token
// minted here; the model's tool calls come back to the server through
// /api/ai/voice/tool and run with the person's own permissions.
//
// Provider: Gemini Live (gemini-3.8-live, about $0.005/min heard + $0.018/min
// spoken in September 2026, against $0.02 + $0.08 for OpenAI gpt-realtime-2.1).
// Another provider plugs in as one more entry in PROVIDERS plus a browser
// transport in frontend/src/lib/liveCall.ts.

import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

const here = fileURLToPath(new URL('.', import.meta.url));

export function voiceConfig(env = process.env) {
  return {
    provider: env.ITHOMIINI_VOICE_PROVIDER || 'gemini',
    model: env.ITHOMIINI_VOICE_MODEL || 'gemini-3.8-live',
    // Prebuilt Gemini voice, e.g. Kore or Puck; empty uses the model's default.
    voiceName: env.ITHOMIINI_VOICE_NAME || '',
    apiKeyFile: env.ITHOMIINI_GEMINI_API_KEY_FILE || '',
    apiKey: env.ITHOMIINI_GEMINI_API_KEY || '',
  };
}

export async function voiceKey(voice) {
  if (voice.apiKeyFile) return (await readFile(voice.apiKeyFile, 'utf8')).trim();
  return voice.apiKey ?? '';
}

let brief = null;
/** assistant/VOICE.md: the chat brief (CLAUDE.md) condensed for speaking. */
export async function voiceBrief() {
  brief ??= await readFile(join(here, '..', 'assistant', 'VOICE.md'), 'utf8');
  return brief;
}

/** "sí", "dale", "guárdalo"… The person's own words, not the model's, unlock apply_proposal. */
export function affirmative(text) {
  const said = String(text ?? '')
    .toLowerCase()
    .normalize('NFD')
    .replace(/\p{M}/gu, '')
    .trim();
  if (!said || /^(no|nop|todavia|espera|aun no)\b/.test(said)) return false;
  return /\b(si|dale|ok|okay|okey|vale|listo|claro|correcto|exacto|perfecto|confirmo|confirmado|de acuerdo|adelante|hazlo|aplica\w*|guarda\w*|graba\w*|yes)\b/.test(
    said,
  );
}

// In a call the model may only save after the person says yes out loud.
const VOICE_NOTES = {
  apply_proposal:
    'Write a pending proposal to Google Sheets. Only call this right after the person said yes out loud to that proposal (e.g. "sí, guárdalo", "está correcto"); the server checks their words. Optionally only some rows, by their index.',
};

/** The setup message for a Gemini Live session: prompt, tools and transcripts of both sides. */
export function liveSetup({ model, voiceName, prompt, tools }) {
  return {
    model: `models/${model}`,
    generationConfig: {
      responseModalities: ['AUDIO'],
      ...(voiceName ? { speechConfig: { voiceConfig: { prebuiltVoiceConfig: { voiceName } } } } : {}),
    },
    systemInstruction: { parts: [{ text: prompt }] },
    tools: [
      {
        functionDeclarations: tools.map(({ function: f }) => ({
          name: f.name,
          description: VOICE_NOTES[f.name] ?? f.description,
          // Plain JSON Schema, as the SDK sends MCP tools (free-form objects like `values` stay allowed).
          parametersJsonSchema: f.parameters,
        })),
      },
    ],
    // People dictate while handling butterflies: a pause to read a label is not the end of the turn.
    realtimeInputConfig: {
      automaticActivityDetection: { endOfSpeechSensitivity: 'END_SENSITIVITY_LOW', silenceDurationMs: 900 },
    },
    inputAudioTranscription: {},
    outputAudioTranscription: {},
    // Without compression an audio session ends after 15 minutes.
    contextWindowCompression: { slidingWindow: {} },
  };
}

/**
 * Fields the token locks, so the browser cannot swap the prompt, tools or model.
 * sessionResumption stays open: the browser sends the handle when it reconnects.
 */
export function lockedFields(setup) {
  return Object.entries(setup)
    .flatMap(([key, value]) =>
      value && typeof value === 'object' && !Array.isArray(value) && Object.keys(value).length
        ? Object.keys(value).map(inner => `${key}.${inner}`)
        : [key],
    )
    .join(',');
}

const GEMINI = 'https://generativelanguage.googleapis.com';

/** One-use Gemini token: a new call or reconnect must start within 2 minutes; a connection lasts at most 30. */
async function mintGemini({ key, setup, fetchImpl = fetch, now = Date.now() }) {
  const expiresAt = new Date(now + 30 * 60_000).toISOString();
  const response = await fetchImpl(`${GEMINI}/v1alpha/auth_tokens`, {
    method: 'POST',
    headers: { 'content-type': 'application/json', 'x-goog-api-key': key },
    body: JSON.stringify({
      uses: 1,
      expireTime: expiresAt,
      newSessionExpireTime: new Date(now + 2 * 60_000).toISOString(),
      bidiGenerateContentSetup: setup,
      fieldMask: lockedFields(setup),
    }),
    signal: AbortSignal.timeout(15000),
  });
  if (!response.ok) {
    // Google's message says what is wrong (bad key, unknown model) without echoing the key.
    const detail = await response.text().catch(() => '');
    throw new Error(`Gemini token: HTTP ${response.status} ${detail.slice(0, 300)}`);
  }
  const token = (await response.json())?.name;
  if (typeof token !== 'string' || !token.startsWith('auth_tokens/')) throw new Error('Gemini token: no token returned');
  return {
    token,
    expiresAt,
    url: `wss://generativelanguage.googleapis.com/ws/google.ai.generativelanguage.v1alpha.GenerativeService.BidiGenerateContentConstrained`,
  };
}

const PROVIDERS = { gemini: { setup: liveSetup, mint: mintGemini } };

/** What the browser needs to open one connection: provider, model, token, socket URL and setup. */
export async function startVoiceSession(voice, { prompt, tools, fetchImpl, now }) {
  const provider = PROVIDERS[voice.provider];
  if (!provider) throw Object.assign(new Error(`Unknown voice provider ${voice.provider}`), { unconfigured: true });
  const key = await voiceKey(voice).catch(() => '');
  if (!key) throw Object.assign(new Error('Voice key missing'), { unconfigured: true });
  const setup = provider.setup({ model: voice.model, voiceName: voice.voiceName, prompt, tools });
  const minted = await provider.mint({ key, setup, fetchImpl, now });
  return { provider: voice.provider, model: voice.model, setup, ...minted };
}

export const voiceProviders = () => Object.keys(PROVIDERS);
