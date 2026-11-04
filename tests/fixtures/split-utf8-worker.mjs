// A render worker for tests/maps-render.test.mjs whose answer reaches the parent in pieces cut inside multibyte UTF-8
// characters (a 2-byte 'ú', a 3-byte '♀'), written 80 ms apart. `--check`: ready. `big`: 600 'é' (1200 bytes) instead.
const say = s => process.stdout.write(s);
if (process.argv.includes('--check')) say('{"ok":true}\n');
else if (process.argv.includes('big')) say(`${JSON.stringify({ ok: true, result: { title: 'é'.repeat(600) } })}\n`);
else {
  const line = Buffer.from(`${JSON.stringify({ ok: true, result: { title: 'Tithorea harmonia · hembras ♀ · Perú', records: [{ id: 'CAM1', country: 'Perú' }] } })}\n`);
  const cuts = [line.indexOf(Buffer.from('♀')) + 1, line.indexOf(Buffer.from('ú')) + 1, line.lastIndexOf(Buffer.from('ú')) + 1];
  const pieces = [0, ...cuts, line.length].map((at, i, all) => line.subarray(at, all[i + 1])).filter(p => p.length);
  pieces.forEach((p, i) => setTimeout(() => say(p), i * 80));
}
