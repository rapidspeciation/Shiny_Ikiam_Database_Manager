<script setup lang="ts">
import { defineAsyncComponent, onMounted, watch } from 'vue'
import { RouterView, useRoute } from 'vue-router'
import AppHeader from './components/AppHeader.vue'
import GoogleBanner from './components/GoogleBanner.vue'
import SaveBar from './components/SaveBar.vue'
import LoginView from './views/LoginView.vue'
import { accountPaths, openPaths } from './router'
import { notice } from './lib/notice'
import { t3Host } from './lib/t3Host'
import { updateAvailable } from './lib/updates'
import { useLive } from './stores/live'
import { usePending } from './stores/pending'
import { useSession } from './stores/session'
import { useTables } from './stores/tables'

// Loaded with the Asistente tab, not before.
const T3Host = defineAsyncComponent(() => import('./components/T3Host.vue'))
const session = useSession()
const route = useRoute()
const pending = usePending()
const tables = useTables()
const live = useLive()

onMounted(() => session.init())
const reload = () => window.location.reload()
watch(
  () => session.user?.username,
  (name, before) => {
    // Visitors get reduced copies of a few sheets: signing in or out starts afresh.
    if (name !== before) tables.$reset()
    if (!name) return live.stop()
    pending.restore()
    tables.follow()
    // The workbook's state, saves waiting for Google, everyone's Emergidos and Clutches entries (stores/live.ts).
    live.start()
  },
)
// A change there may be one of this device's saves that waited for Google, now written (or refused).
watch(
  () => live.ticks,
  () => {
    if (Object.keys(pending.queued).length) void pending.resolveQueued()
  },
)
</script>

<template>
  <!-- The invitation and password pages and the home page work without an account (the rest needs a login). -->
  <RouterView v-if="accountPaths.has(route.path)" />
  <div v-else-if="!session.ready" class="grid h-full place-items-center text-stone-500">{{ $t('Cargando…') }}</div>
  <LoginView v-else-if="!session.user && !openPaths.has(route.path)" />
  <div v-else class="flex h-full flex-col">
    <!-- T3 Code, loaded once the Asistente tab was opened and kept for the session (it would reload in the tab). -->
    <T3Host v-if="session.user && t3Host.url" :key="session.user.username" />
    <AppHeader v-if="!route.meta.bare" />
    <GoogleBanner v-if="session.user" />
    <!-- Tabs stay alive while another one is open: going back to Tablas does not rebuild a 13k-row grid. -->
    <main class="min-h-0 flex-1">
      <RouterView v-slot="{ Component }">
        <KeepAlive :exclude="['HomeView', 'UsersView', 'LoginView']">
          <component :is="Component" />
        </KeepAlive>
      </RouterView>
    </main>
    <SaveBar v-if="session.user && !route.meta.bare" />
  </div>
  <!-- A new version was deployed while the page was open. -->
  <div
    v-if="updateAvailable"
    role="alert"
    class="fixed top-2 left-1/2 z-50 flex max-w-[95vw] -translate-x-1/2 items-center gap-3 rounded-md bg-amber-100 px-4 py-2 text-sm text-amber-950 shadow-lg ring-1 ring-amber-300"
  >
    <span>{{ $t('Hay una versión nueva de la app. Recarga la página (los cambios sin guardar se conservan).') }}</span>
    <button class="btn-primary px-3 py-1" @click="reload">{{ $t('Recargar') }}</button>
  </div>
  <!-- Messages let touches through: on a phone they sit over the grid (and its handle). -->
  <div
    v-if="notice.text"
    role="status"
    class="pointer-events-none fixed bottom-20 left-1/2 z-50 max-w-[90vw] -translate-x-1/2 rounded-md px-4 py-2 text-sm shadow-lg"
    :class="{
      'bg-stone-800 text-white': notice.kind === 'info',
      'bg-red-700 text-white': notice.kind === 'error',
      'bg-brand-700 text-white': notice.kind === 'success',
    }"
  >
    {{ notice.text }}
  </div>
</template>
