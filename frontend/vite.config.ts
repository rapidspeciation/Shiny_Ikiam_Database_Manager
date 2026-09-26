/// <reference types="vitest/config" />
import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
import tailwindcss from '@tailwindcss/vite'

// The app is served under a configurable base path (default /ithomiini/), so
// production assets use relative URLs. The dev server proxies the API to a
// locally running backend (see README).
export default defineConfig(({ command }) => ({
  base: command === 'serve' ? '/ithomiini/' : './',
  plugins: [vue(), tailwindcss()],
  build: { outDir: '../web', emptyOutDir: true, chunkSizeWarningLimit: 900 },
  server: {
    port: 5173,
    proxy: { '/ithomiini/api': { target: process.env.API_TARGET || 'http://127.0.0.1:8794', changeOrigin: false } },
  },
  test: { environment: 'happy-dom' },
}))
