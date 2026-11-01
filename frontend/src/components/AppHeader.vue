<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref } from 'vue'
import { RouterLink, useRoute } from 'vue-router'
import { ExternalLink, FlaskConical, LogIn, LogOut, Users, ChevronDown, Monitor, Sun, Moon } from 'lucide-vue-next'
import { tabs } from '../router'
import DatabaseBadge from './DatabaseBadge.vue'
import ScrollRow from './ScrollRow.vue'
import { useSession } from '../stores/session'
import { usePending } from '../stores/pending'
import { useLive } from '../stores/live'
import { changeCount } from '../lib/staged'
import { notify } from '../lib/notice'
import { locale, t } from '../lib/i18n'
import { menuLeft } from '../lib/menuPlace'
import { themePref, type ThemePref } from '../lib/theme'

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

/** Light or dark (lib/theme.ts): the system's choice unless one is picked here. Arrow keys move the choice, as in any radio group. */
const themeOptions: { value: ThemePref; label: string; icon: typeof Sun }[] = [
  { value: 'system', label: 'Sistema', icon: Monitor },
  { value: 'light', label: 'Claro', icon: Sun },
  { value: 'dark', label: 'Oscuro', icon: Moon },
]
function themeKeys(event: KeyboardEvent) {
  const step = { ArrowRight: 1, ArrowDown: 1, ArrowLeft: -1, ArrowUp: -1 }[event.key]
  if (!step) return
  event.preventDefault()
  const at = themeOptions.findIndex(o => o.value === themePref.value)
  const next = (at + step + themeOptions.length) % themeOptions.length
  themePref.value = themeOptions[next]!.value
  ;(event.currentTarget as HTMLElement).querySelectorAll<HTMLElement>('[role="radio"]')[next]?.focus()
}

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
  <header class="app-header border-b border-brand-hover bg-brand text-white">
    <!-- One row on wide screens; on phones the tabs get their own full-width row. -->
    <div class="flex flex-wrap items-center gap-x-3 px-3 sm:flex-nowrap sm:px-4">
      <RouterLink to="/inicio" class="flex shrink-0 items-center gap-2 py-1 font-semibold">
        <!-- The butterfly alone (no tile), with a pale edge for the dark bar. -->
        <img src="/logo.svg?v=1" alt="" class="h-9 w-auto" />
        <span>Ikiam Insectary DB</span>
      </RouterLink>
      <!-- The tabs and which database is open: on phones a full-width row of their own, the tabs scrolling sideways. -->
      <div class="order-last -mx-3 flex w-[calc(100%+1.5rem)] min-w-0 items-center sm:order-none sm:mx-0 sm:w-auto sm:flex-1">
        <ScrollRow
          as="nav"
          :active="route.meta.tab ?? route.path"
          class="-mb-px flex min-w-0 flex-1 px-1 sm:px-0"
          :aria-label="$t('Secciones')"
        >
          <RouterLink
            v-for="tab in shown"
            :key="tab.path"
            :to="tab.path"
            class="shrink-0 border-b-2 border-transparent px-3 py-3 text-sm font-medium whitespace-nowrap text-on-brand-muted hover:text-white focus-visible:-outline-offset-2 lg:px-2 2xl:px-3"
            :class="{ '!border-white !text-white': route.meta.tab === tab.path }"
            :data-active="route.meta.tab === tab.path || route.path === tab.path ? '' : undefined"
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
        </ScrollRow>
        <DatabaseBadge class="mr-3 ml-2 sm:mr-0 sm:ml-3" />
      </div>
      <!-- Interface language: English by default (lib/i18n.ts). -->
      <button
        class="hit-target ml-auto inline-flex min-h-6 shrink-0 items-center rounded px-1.5 text-xs font-semibold text-on-brand-muted ring-1 ring-on-brand-muted/50 hover:bg-brand-hover hover:text-white sm:ml-0"
        :title="$t('Cambiar idioma')"
        :aria-label="$t('Cambiar idioma')"
        @click="locale = locale === 'en' ? 'es' : 'en'"
      >
        <span :class="locale === 'en' ? 'text-white underline underline-offset-2' : ''">EN</span>
        <span aria-hidden="true">/</span>
        <span :class="locale === 'es' ? 'text-white underline underline-offset-2' : ''">ES</span>
      </button>
      <RouterLink
        v-if="!session.user"
        :to="{ path: '/entrar', query: { volver: route.fullPath } }"
        class="flex shrink-0 items-center gap-1 rounded px-2 py-1 text-sm hover:bg-brand-hover pointer-coarse:min-h-11"
      >
        <LogIn :size="15" /> {{ $t('Iniciar sesión') }}
      </RouterLink>
      <div v-else ref="holder" class="relative shrink-0">
        <button
          ref="trigger"
          class="flex items-center gap-1 rounded px-2 py-1 text-sm hover:bg-brand-hover pointer-coarse:min-h-11"
          :aria-expanded="menu"
          @click="toggleMenu"
        >
          {{ session.user?.displayName }} <ChevronDown :size="14" />
        </button>
        <div
          v-if="menu"
          ref="menuBox"
          data-user-menu
          class="absolute z-50 mt-1 w-max max-w-[calc(100vw-1rem)] min-w-52 rounded-md border border-border-strong bg-surface-raised py-1 text-sm text-fg shadow-lg"
          :class="menuX === null ? 'right-0' : ''"
          :style="menuX === null ? undefined : { left: `${menuX}px` }"
          @click="menu = false"
        >
          <p class="px-3 py-1 text-xs text-fg-subtle">{{ session.user?.username }} · {{ session.user?.role }}</p>
          <!-- Picking a theme keeps the menu open, so the change can be seen and undone. -->
          <div class="flex items-center justify-between gap-3 px-3 py-1.5" @click.stop>
            <span id="theme-label" class="text-xs text-fg-subtle">{{ $t('Tema') }}</span>
            <div role="radiogroup" aria-labelledby="theme-label" class="inline-flex rounded-md border border-control p-0.5" @keydown="themeKeys">
              <button
                v-for="o in themeOptions"
                :key="o.value"
                type="button"
                role="radio"
                :aria-checked="themePref === o.value"
                :tabindex="themePref === o.value ? 0 : -1"
                class="flex min-h-8 items-center gap-1 rounded px-2 text-xs font-medium focus-visible:outline-2 focus-visible:outline-offset-1 focus-visible:outline-focus"
                :class="themePref === o.value ? 'bg-brand text-on-brand' : 'text-fg-muted hover:bg-surface-sunken'"
                @click="themePref = o.value"
              >
                <component :is="o.icon" :size="14" aria-hidden="true" /> {{ $t(o.label) }}
              </button>
            </div>
          </div>
          <a
            v-if="session.settings?.sheetUrl"
            :href="session.settings.sheetUrl"
            target="_blank"
            rel="noopener"
            class="flex items-center gap-2 px-3 py-2 hover:bg-surface-sunken md:hidden"
          >
            <ExternalLink :size="15" /> {{ $t('Abrir Google Sheet') }}
          </a>
          <RouterLink v-if="session.isAdmin" to="/usuarios" class="flex items-center gap-2 px-3 py-2 hover:bg-surface-sunken">
            <Users :size="15" /> {{ $t('Usuarios') }}
          </RouterLink>
          <a v-if="session.isOfflineIdTester" :href="offlineIdPage" class="flex items-center gap-2 px-3 py-2 hover:bg-surface-sunken">
            <FlaskConical :size="15" /> {{ $t('Prueba: identificador sin conexión') }}
          </a>
          <button class="flex w-full items-center gap-2 px-3 py-2 hover:bg-surface-sunken" @click="logout">
            <LogOut :size="15" /> {{ $t('Cerrar sesión') }}
          </button>
        </div>
      </div>
    </div>
  </header>
</template>
