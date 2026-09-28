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

export default defineConfig(({ command }) => ({
  base: command === 'serve' ? '/ithomiini/' : './',
  plugins: [vue(), tailwindcss(), version],
  define: { __BUILD_ID__: JSON.stringify(command === 'serve' ? 'dev' : buildId) },
  build: { outDir: '../web', emptyOutDir: true, chunkSizeWarningLimit: 900 },
  server: {
    port: 5173,
    proxy: { '/ithomiini/api': { target: process.env.API_TARGET || 'http://127.0.0.1:8794', changeOrigin: false } },
  },
  test: { environment: 'happy-dom' },
}))
