<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref } from 'vue'
import { RouterLink, useRoute } from 'vue-router'
import { ExternalLink, FlaskConical, LogIn, LogOut, Users, ChevronDown } from 'lucide-vue-next'
import { tabs } from '../router'
import { useSession } from '../stores/session'
import { usePending } from '../stores/pending'
import { useLive } from '../stores/live'
import { changeCount } from '../lib/staged'
import { notify } from '../lib/notice'
import { locale, t } from '../lib/i18n'
import { menuLeft } from '../lib/menuPlace'

const session = useSession()
const pending = usePending()
const live = useLive()
const menu = ref(false)
const holder = ref<HTMLElement>()
const trigger = ref<HTMLElement>()
const menuBox = ref<HTMLElement>()
/** The menu's left edge from the name's (lib/menuPlace.ts): it never opens past the screen's sides. */
const menuX = ref<number | null>(null)
async function toggleMenu() {
  menu.value = !menu.value
  if (!menu.value) return
  menuX.value = null
  await nextTick()
  if (trigger.value && menuBox.value)
    menuX.value = menuLeft(trigger.value.getBoundingClientRect(), menuBox.value.offsetWidth, document.documentElement.clientWidth)
}
// Closed by a tap outside it, by Escape, and when the screen turns (the name moves with the header; only a
// change of width: Android's address bar changes the height while scrolling).
const outside = (event: PointerEvent) => {
  if (menu.value && !holder.value?.contains(event.target as Node)) menu.value = false
}
const escape = (event: KeyboardEvent) => {
  if (!menu.value || event.key !== 'Escape') return
  menu.value = false
  trigger.value?.focus()
}
let width = 0
const resized = () => {
  if (window.innerWidth !== width) menu.value = false
  width = window.innerWidth
}
onMounted(() => {
  width = window.innerWidth
  document.addEventListener('pointerdown', outside)
  document.addEventListener('keydown', escape)
  window.addEventListener('resize', resized)
})
onBeforeUnmount(() => {
  document.removeEventListener('pointerdown', outside)
  document.removeEventListener('keydown', escape)
  window.removeEventListener('resize', resized)
})
/** Emergidos, Clutches and Censo: their changes kept in the app, waiting for «Guardar en Google Sheets». */
const PURPOSE: Record<string, string> = { '/emergidos': 'emergidos', '/clutches': 'clutches', '/censo': 'censo' }
const waiting = computed(() => {
  const out: Record<string, number> = {}
  for (const [path, purpose] of Object.entries(PURPOSE)) {
    const items = live.items.filter(i => i.purpose === purpose && i.status === 'staged')
    // A census: its disappearances (one row each), not their cells.
    out[path] = changeCount(items)
  }
  return out
})
const route = useRoute()
/** Without an account only the open pages are listed; Revisión only for people who edit. */
const shown = computed(() =>
  session.user
    ? tabs
        .filter(t => !('editors' in t && t.editors) || session.canEdit)
        // Censo keeps its disappearances in the app: shown only where those tabs save there.
        .filter(t => !('staged' in t && t.staged) || session.settings?.stagedSaving !== false)
    : tabs.filter(t => 'open' in t && t.open),
)

/** The offline identifier test page (a page of its own, server/offline-id.mjs), opened in this window: inside the installed app it stays in the app. */
const offlineIdPage = new URL('prueba-offline/', location.href.split('#')[0]).href

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
      <RouterLink to="/inicio" class="flex shrink-0 items-center gap-2 py-1 font-semibold">
        <!-- The butterfly alone (no tile), with a pale edge for the dark bar. -->
        <img src="/logo.svg?v=1" alt="" class="h-9 w-auto" />
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
          <span
            v-if="waiting[tab.path]"
            class="ml-1 rounded-full bg-amber-300 px-1.5 text-xs font-semibold text-amber-950"
            :title="
              $tn(
                waiting[tab.path],
                '{n} fila cambiada en la app, aún no en Google Sheets',
                '{n} filas cambiadas en la app, aún no en Google Sheets',
              )
            "
            >{{ waiting[tab.path] }}</span
          >
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
      <div v-else ref="holder" class="relative shrink-0">
        <button
          ref="trigger"
          class="flex items-center gap-1 rounded px-2 py-1 text-sm hover:bg-brand-800"
          :aria-expanded="menu"
          @click="toggleMenu"
        >
          {{ session.user?.displayName }} <ChevronDown :size="14" />
        </button>
        <div
          v-if="menu"
          ref="menuBox"
          data-user-menu
          class="absolute z-50 mt-1 w-max max-w-[calc(100vw-1rem)] min-w-52 rounded-md border border-stone-200 bg-white py-1 text-sm text-stone-800 shadow-lg"
          :class="menuX === null ? 'right-0' : ''"
          :style="menuX === null ? undefined : { left: `${menuX}px` }"
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
          <a v-if="session.isOfflineIdTester" :href="offlineIdPage" class="flex items-center gap-2 px-3 py-2 hover:bg-stone-100">
            <FlaskConical :size="15" /> {{ $t('Prueba: identificador sin conexión') }}
          </a>
          <button class="flex w-full items-center gap-2 px-3 py-2 hover:bg-stone-100" @click="logout">
            <LogOut :size="15" /> {{ $t('Cerrar sesión') }}
          </button>
        </div>
      </div>
    </div>
  </header>
</template>
