<script setup lang="ts">
import { computed, ref } from 'vue'
import { RouterLink, useRoute } from 'vue-router'
import { ExternalLink, LogIn, LogOut, Users, ChevronDown } from 'lucide-vue-next'
import { tabs } from '../router'
import { useSession } from '../stores/session'
import { usePending } from '../stores/pending'
import { notify } from '../lib/notice'
import { locale, t } from '../lib/i18n'

const session = useSession()
const pending = usePending()
const menu = ref(false)
const route = useRoute()
/** Without an account only the open pages are listed; Revisión only for people who edit. */
const shown = computed(() =>
  session.user ? tabs.filter(t => !('editors' in t && t.editors) || session.canEdit) : tabs.filter(t => 'open' in t && t.open),
)

async function logout() {
  if (
    pending.changeCount &&
    !confirm(t('Hay cambios sin guardar en este dispositivo. Se conservarán para cuando vuelvas. ¿Cerrar sesión?'))
  )
    return
  await session.logout()
  pending.clear()
  notify(t('Sesión cerrada'))
}
</script>

<template>
  <header class="app-header border-b border-brand-800 bg-brand-700 text-white">
    <!-- One row on wide screens; on phones the tabs get their own full-width row. -->
    <div class="flex flex-wrap items-center gap-x-3 px-3 sm:flex-nowrap sm:px-4">
      <RouterLink to="/inicio" class="flex shrink-0 items-center gap-2 py-2 font-semibold">
        <img src="/mark.svg" alt="" class="h-6 w-6" />
        <span>Ikiam Insectary DB</span>
      </RouterLink>
      <nav
        class="order-last -mx-3 -mb-px flex w-full min-w-0 overflow-x-auto px-1 sm:order-none sm:mx-0 sm:w-auto sm:flex-1 sm:px-0"
        :aria-label="$t('Secciones')"
      >
        <RouterLink
          v-for="tab in shown"
          :key="tab.path"
          :to="tab.path"
          class="shrink-0 border-b-2 border-transparent px-3 py-3 text-sm font-medium whitespace-nowrap text-brand-100 hover:text-white"
          :class="{ '!border-white !text-white': route.meta.tab === tab.path }"
          active-class="!border-white !text-white"
        >
          {{ $t(tab.label) }}
        </RouterLink>
      </nav>
      <span
        v-if="session.settings?.localMode"
        class="hidden shrink-0 items-center gap-1 rounded bg-amber-300 px-2 py-0.5 text-xs font-semibold text-amber-950 md:inline-flex"
        :title="$t('Copia sin conexión: los cambios se guardan solo aquí, nunca en la hoja del equipo')"
      >
        {{ $t('Copia de prueba') }}
      </span>
      <a
        v-else-if="session.settings?.sheetUrl"
        :href="session.settings.sheetUrl"
        target="_blank"
        rel="noopener"
        class="hidden shrink-0 items-center gap-1 rounded px-2 py-0.5 text-xs font-semibold text-brand-100 ring-1 ring-brand-500 hover:bg-brand-800 hover:text-white md:inline-flex"
        :title="$t('Abrir Google Sheet')"
      >
        Google Sheet <ExternalLink :size="12" />
      </a>
      <!-- Interface language: English by default (lib/i18n.ts). -->
      <button
        class="ml-auto shrink-0 rounded px-1.5 py-0.5 text-xs font-semibold text-brand-100 ring-1 ring-brand-500 hover:bg-brand-800 hover:text-white sm:ml-0"
        :title="$t('Cambiar idioma')"
        :aria-label="$t('Cambiar idioma')"
        @click="locale = locale === 'en' ? 'es' : 'en'"
      >
        <span :class="locale === 'en' ? 'text-white' : 'opacity-60'">EN</span>
        <span class="opacity-60">/</span>
        <span :class="locale === 'es' ? 'text-white' : 'opacity-60'">ES</span>
      </button>
      <RouterLink
        v-if="!session.user"
        :to="{ path: '/entrar', query: { volver: route.fullPath } }"
        class="flex shrink-0 items-center gap-1 rounded px-2 py-1 text-sm hover:bg-brand-800"
      >
        <LogIn :size="15" /> {{ $t('Iniciar sesión') }}
      </RouterLink>
      <div v-else class="relative shrink-0">
        <button class="flex items-center gap-1 rounded px-2 py-1 text-sm hover:bg-brand-800" @click="menu = !menu">
          {{ session.user?.displayName }} <ChevronDown :size="14" />
        </button>
        <div
          v-if="menu"
          class="absolute right-0 z-50 mt-1 w-52 rounded-md border border-stone-200 bg-white py-1 text-sm text-stone-800 shadow-lg"
          @click="menu = false"
        >
          <p class="px-3 py-1 text-xs text-stone-500">{{ session.user?.username }} · {{ session.user?.role }}</p>
          <a
            v-if="session.settings?.sheetUrl"
            :href="session.settings.sheetUrl"
            target="_blank"
            rel="noopener"
            class="flex items-center gap-2 px-3 py-2 hover:bg-stone-100 md:hidden"
          >
            <ExternalLink :size="15" /> {{ $t('Abrir Google Sheet') }}
          </a>
          <RouterLink v-if="session.isAdmin" to="/usuarios" class="flex items-center gap-2 px-3 py-2 hover:bg-stone-100">
            <Users :size="15" /> {{ $t('Usuarios') }}
          </RouterLink>
          <button class="flex w-full items-center gap-2 px-3 py-2 hover:bg-stone-100" @click="logout">
            <LogOut :size="15" /> {{ $t('Cerrar sesión') }}
          </button>
        </div>
      </div>
    </div>
  </header>
</template>
