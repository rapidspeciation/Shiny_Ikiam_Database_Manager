<script setup lang="ts">
import { onMounted, watch } from 'vue'
import { RouterView, useRoute } from 'vue-router'
import AppHeader from './components/AppHeader.vue'
import SaveBar from './components/SaveBar.vue'
import LoginView from './views/LoginView.vue'
import { notice } from './lib/notice'
import { usePending } from './stores/pending'
import { useSession } from './stores/session'
import { useTables } from './stores/tables'

const session = useSession()
const route = useRoute()
const pending = usePending()
const tables = useTables()

onMounted(() => session.init())
watch(
  () => session.user?.username,
  name => {
    if (!name) return
    pending.restore()
    tables.follow()
  },
)
</script>

<template>
  <!-- The invitation page works without an account. -->
  <RouterView v-if="route.path === '/activar'" />
  <div v-else-if="!session.ready" class="grid h-full place-items-center text-stone-500">Cargando…</div>
  <LoginView v-else-if="!session.user" />
  <div v-else class="flex h-full flex-col">
    <AppHeader />
    <main class="min-h-0 flex-1"><RouterView /></main>
    <SaveBar />
  </div>
  <div
    v-if="notice.text"
    role="status"
    class="fixed bottom-20 left-1/2 z-50 max-w-[90vw] -translate-x-1/2 rounded-md px-4 py-2 text-sm shadow-lg"
    :class="{
      'bg-stone-800 text-white': notice.kind === 'info',
      'bg-red-700 text-white': notice.kind === 'error',
      'bg-brand-700 text-white': notice.kind === 'success',
    }"
  >
    {{ notice.text }}
  </div>
</template>
