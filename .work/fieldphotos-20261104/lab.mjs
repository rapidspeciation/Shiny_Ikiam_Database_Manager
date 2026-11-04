// Lab: the app in memory (no identifier, no team data). node lab.mjs <port>
import { createApp } from '../../server/index.mjs';
const port = Number(process.argv[2] || 0);
const app = await createApp({ databasePath: ':memory:', localMode: true, secureCookies: false, syncIntervalMs: 0, port, host: '127.0.0.1' }, { seed: {} });
await app.ready;
const a = await app.listen(port, '127.0.0.1');
console.log(`lab on http://127.0.0.1:${a.port}/`);
