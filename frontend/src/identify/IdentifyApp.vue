<script setup lang="ts">
// The Identificar page: the app's header (its session, tabs and menus) above the identifier.
import { onMounted } from 'vue'
import { RouterView } from 'vue-router'
import AppHeader from '../components/AppHeader.vue'
import { useSession } from '../stores/session'

const session = useSession()
// Offline the service worker answers with the last session it saw; without one the page still works with the models.
onMounted(() => session.init().catch(() => (session.ready = true)))
</script>

<template>
  <div class="flex min-h-full flex-col">
    <AppHeader />
    <main class="flex-1">
      <RouterView />
    </main>
  </div>
</template>
