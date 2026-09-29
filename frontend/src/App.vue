<script setup lang="ts">
import { onMounted, watch } from 'vue'
import { RouterView, useRoute } from 'vue-router'
import AppHeader from './components/AppHeader.vue'
import SaveBar from './components/SaveBar.vue'
import LoginView from './views/LoginView.vue'
import { openPaths } from './router'
import { notice } from './lib/notice'
import { updateAvailable } from './lib/updates'
import { usePending } from './stores/pending'
import { useSession } from './stores/session'
import { useTables } from './stores/tables'

const session = useSession()
const route = useRoute()
const pending = usePending()
const tables = useTables()

onMounted(() => session.init())
const reload = () => window.location.reload()
watch(
  () => session.user?.username,
  (name, before) => {
    // Visitors get reduced copies of a few sheets: signing in or out starts afresh.
    if (name !== before) tables.$reset()
    if (!name) return
    pending.restore()
    tables.follow()
  },
)
</script>

<template>
  <!-- The invitation page, the home page and the monitoring report work without an account. -->
  <RouterView v-if="route.path === '/activar'" />
  <div v-else-if="!session.ready" class="grid h-full place-items-center text-stone-500">Cargando…</div>
  <LoginView v-else-if="!session.user && !openPaths.has(route.path)" />
  <div v-else class="flex h-full flex-col">
    <AppHeader v-if="!route.meta.bare" />
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
    <span>Hay una versión nueva de la app. Recarga la página (los cambios sin guardar se conservan).</span>
    <button class="btn-primary px-3 py-1" @click="reload">Recargar</button>
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
