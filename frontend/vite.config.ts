/// <reference types="vitest/config" />
import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
import tailwindcss from '@tailwindcss/vite'

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

// Lib tests that need a page (document, window or localStorage).
const libWithPage = ['camera', 'cellBar', 'clipboard', 'gridKeys', 'gridKit', 'pendingStaged', 'proposalColumns', 'saveChecks'].map(
  name => `src/lib/__tests__/${name}.test.ts`,
)

export default defineConfig(({ command }) => ({
  base: command === 'serve' ? '/ithomiini/' : './',
  plugins: [vue(), tailwindcss(), version],
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
  // Components, composables, stores and the lib tests that use the page or
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
        test: { name: 'dom', environment: 'happy-dom', include: ['src/{components,composables,stores}/**/*.test.ts', ...libWithPage] },
      },
    ],
  },
}))
