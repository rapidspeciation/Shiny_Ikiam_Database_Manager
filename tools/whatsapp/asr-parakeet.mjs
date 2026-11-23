#!/usr/bin/env node
// One voice note through Parakeet (transcribe.cpp, the engine T3 uses for its own dictation), on this PC's eGPU or CPU.
//   node asr-parakeet.mjs <audio file>        → one JSON line: { text, language, audioMs, ms, loadMs, device, vramMB, rssMB }
// The bridge runs it as a child process, one note at a time, under systemd-run --scope -p MemoryMax=… (bridge.mjs
// parakeetRun()), so the model's memory is given back after each note. Settings from the environment (the bridge sets them
// from config.voice.parakeet):
//   PARAKEET_MODULE  transcribe-cpp's dist/index.js (a copy of T3's, so a T3 update does not move it)
//   PARAKEET_MODEL   the GGUF (parakeet-ultra-Q8_0.gguf)
//   PARAKEET_DEVICE  the eGPU's PCI id (0000:03:00.0, the RX 7600M XT) or "cpu" for the explicit CPU fallback (called by
//                    the bridge with Vulkan ICDs disabled so a wedged eGPU cannot hang probing and the iGPU is never touched).
//                    The helper uses that exact target and nothing else: never the iGPU, and no silent fallback from a PCI id.
import { execFile } from 'node:child_process';
import { realpathSync } from 'node:fs';
import { pathToFileURL } from 'node:url';

const fail = (msg, code = 1) => {
  process.stdout.write(JSON.stringify({ error: msg }) + '\n');
  process.exit(code);
};

/** 16 kHz mono float32 PCM of any audio file ffmpeg reads (ogg/opus, m4a, mp3, wav). */
export function decodePcm(file, { ffmpeg = 'ffmpeg', maxSeconds = 1800 } = {}) {
  return new Promise((resolve, reject) => {
    execFile(ffmpeg, ['-v', 'error', '-nostdin', '-i', file, '-t', String(maxSeconds), '-ac', '1', '-ar', '16000', '-f', 'f32le', 'pipe:1'], { encoding: 'buffer', maxBuffer: 16000 * 4 * (maxSeconds + 5), timeout: 120_000 }, (e, out, err) => {
      if (e) return reject(new Error(String(err || e.message).trim().slice(0, 200)));
      const buf = Buffer.from(out);
      resolve(new Float32Array(buf.buffer, buf.byteOffset, Math.floor(buf.length / 4)));
    });
  });
}

/** The exact target: "cpu" for the CPU backend, or a configured discrete GPU PCI id; never the iGPU, and no silent fallback. */
export function pickDevice(devices, target) {
  if (!target) throw new Error('no device configured');
  if (target === 'cpu') {
    const d = devices.find(x => x.deviceType === 'cpu');
    if (!d) throw new Error('CPU device not found');
    return d;
  }
  const d = devices.find(x => x.deviceId === target);
  if (!d) throw new Error(`device ${target} not found`);
  if (d.deviceType !== 'gpu') throw new Error(`device ${target} is ${d.deviceType}, not the eGPU`);
  return d;
}

async function main() {
  const file = process.argv[2];
  const { PARAKEET_MODULE: mod, PARAKEET_MODEL: model, PARAKEET_DEVICE: pci } = process.env;
  if (!file || !mod || !model || !pci) fail('usage: PARAKEET_MODULE=… PARAKEET_MODEL=… PARAKEET_DEVICE=<pci id|cpu> node asr-parakeet.mjs <audio>', 2);
  const t0 = Date.now();
  const pcm = await decodePcm(file);
  if (pcm.length < 16000 * 0.3) fail('audio too short');
  const tc = await import(mod);
  const device = pickDevice(await tc.getAvailableBackendsAsync(), pci);
  const freeBefore = device.memoryFree;
  const t1 = Date.now();
  const m = await tc.TranscribeModel.load(model, { device });
  const loadMs = Date.now() - t1;
  try {
    const r = await m.transcribe(pcm, { timestamps: 'none' });
    const freeAfter = m.device.memoryFree;
    process.stdout.write(
      JSON.stringify({
        text: String(r.text ?? '').trim(),
        language: r.language || null,
        audioMs: Math.round((pcm.length / 16000) * 1000),
        ms: Date.now() - t0,
        loadMs,
        device: m.device.description,
        vramMB: freeBefore && freeAfter ? Math.round((freeBefore - freeAfter) / 1048576) : null,
        rssMB: Math.round(process.resourceUsage().maxRSS / 1024),
      }) + '\n',
    );
  } finally {
    m.dispose();
  }
}

if (process.argv[1] && import.meta.url === pathToFileURL(realpathSync(process.argv[1])).href) main().catch(e => fail(String(e?.message ?? e).slice(0, 300)));
