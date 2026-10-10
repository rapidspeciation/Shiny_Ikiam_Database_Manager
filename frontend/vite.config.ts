/// <reference types="vitest/config" />
import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
import tailwindcss from '@tailwindcss/vite'
import { readdir, readFile, writeFile } from 'node:fs/promises'
import { join, resolve } from 'node:path'
import { promisify } from 'node:util'
import { brotliCompress, constants } from 'node:zlib'

// The app is served under a configurable base path (default /ithomiini/), so
// production assets use relative URLs. The dev server proxies the API to a
// locally running backend (see README).
// Each build has an ID, written to version.json: an open page compares it with
// its own to tell the person a new version was deployed (lib/updates.ts).
const buildId = new Date().toISOString()
const version = {
  name: 'version-file',
  generateBundle(this: { emitFile: (file: { type: 'asset'; fileName: string; source: string }) => void }) {
    this.emitFile({ type: 'asset', fileName: 'version.json', source: JSON.stringify({ build: buildId }) })
  },
}

// Every built script, style, font and page also as <file>.br, at brotli's highest level: the server
// sends that copy to browsers that take brotli (server/index.mjs serveStatic). It is about a fifth
// smaller than what the proxy compresses on the fly (zstd or gzip), and the fonts, which the proxy
// leaves alone, go from 1.5 MB to 0.5 MB.
let outDir = ''
const precompress = {
  name: 'precompress',
  apply: 'build' as const,
  configResolved(config: { root: string; build: { outDir: string } }) {
    outDir = resolve(config.root, config.build.outDir)
  },
  async closeBundle() {
    const brotli = promisify(brotliCompress)
    const files = (await readdir(outDir, { recursive: true })).filter(f => /\.(js|css|html|svg|ttf|json|webmanifest)$/.test(f))
    await Promise.all(
      files.map(async name => {
        const data = await readFile(join(outDir, name))
        if (data.length < 1024) return
        const params = { [constants.BROTLI_PARAM_QUALITY]: 11, [constants.BROTLI_PARAM_SIZE_HINT]: data.length }
        const packed = await brotli(data, { params })
        if (packed.length < data.length * 0.9) await writeFile(join(outDir, `${name}.br`), packed)
      }),
    )
  },
}

// Lib tests that need a page (document, window or localStorage).
const libWithPage = ['camera', 'cellBar', 'clipboard', 'gridKeys', 'proposalColumns'].map(
  name => `src/lib/__tests__/${name}.test.ts`,
)

export default defineConfig(({ command }) => ({
  base: command === 'serve' ? '/ithomiini/' : './',
  plugins: [vue(), tailwindcss(), version, precompress],
  define: { __BUILD_ID__: JSON.stringify(command === 'serve' ? 'dev' : buildId) },
  build: { outDir: '../web', emptyOutDir: true, chunkSizeWarningLimit: 900 },
  server: {
    port: 5173,
    proxy: { '/ithomiini/api': { target: process.env.API_TARGET || 'http://127.0.0.1:8794', changeOrigin: false } },
  },
  // Tests check the Spanish texts (the keys); i18n.test.ts switches language itself.
  // Lib tests run in Node, where Vue loads natively and no page is built per file;
  // they also share loaded modules within a worker (isolate: false), so the
  // translations load once per worker — the setup file still runs per test file.
  // Components, views, composables, stores and the lib tests that use the page or
  // localStorage run in happy-dom, each file on its own.
  test: {
    setupFiles: ['./src/test-setup.ts'],
    projects: [
      {
        extends: true,
        test: { name: 'node', environment: 'node', isolate: false, include: ['src/lib/**/*.test.ts'], exclude: libWithPage },
      },
      {
        extends: true,
        test: { name: 'dom', environment: 'happy-dom', include: ['src/{components,composables,stores,views}/**/*.test.ts', ...libWithPage] },
      },
    ],
  },
}))
