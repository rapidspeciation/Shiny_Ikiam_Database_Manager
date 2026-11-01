<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { RouterLink, useRoute } from 'vue-router'
import { ExternalLink, FlaskConical, LogIn, LogOut, Users, ChevronDown, Monitor, Sun, Moon } from 'lucide-vue-next'
import { tabs } from '../router'
import DatabaseBadge from './DatabaseBadge.vue'
import { useSession } from '../stores/session'
import { usePending } from '../stores/pending'
import { useLive } from '../stores/live'
import { changeCount } from '../lib/staged'
import { notify } from '../lib/notice'
import { locale, t } from '../lib/i18n'
import { menuLeft } from '../lib/menuPlace'
import { nextPref, themePref, type ThemePref } from '../lib/theme'
import { fitTabs, splitTabs } from '../lib/headerTabs'
import { useDismiss } from '../composables/useDismiss'

const session = useSession()
const pending = usePending()
const live = useLive()
const route = useRoute()
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
// Both menus close on a click outside, Escape, focus moving away and a change of page (composables/useDismiss.ts).
useDismiss(menu, holder, { trigger, closeOn: () => route.fullPath })
// …and when the screen turns (the buttons move with the header; only a change of width: Android's address bar
// changes the height while scrolling).
let width = 0
const resized = () => {
  if (window.innerWidth !== width) menu.value = more.value = false
  width = window.innerWidth
}
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
/** Without an account only the open pages are listed; Revisión only for people who edit. */
const shown = computed(() =>
  session.user
    ? tabs
        .filter(t => !('editors' in t && t.editors) || session.canEdit)
        // Censo keeps its disappearances in the app: shown only where those tabs save there.
        .filter(t => !('staged' in t && t.staged) || session.settings?.stagedSaving !== false)
    : tabs.filter(t => 'open' in t && t.open),
)
/** A tab of the row (and «Más», and their measured copies). */
const TAB =
  'shrink-0 border-b-2 border-transparent px-3 py-3 text-sm font-medium whitespace-nowrap text-on-brand-muted hover:text-white focus-visible:-outline-offset-2 max-sm:px-2.5 lg:px-2.5'
const activePath = computed(() => (route.meta.tab as string | undefined) ?? route.path)

/**
 * The row: the tabs used day to day, plus a hidden one while it is open (lib/headerTabs.ts); those that do not fit
 * the row's width move into «Más» too. The widths come from an invisible copy of the row (`measurer`).
 */
const split = computed(() => splitTabs(shown.value, activePath.value))
const rowBox = ref<HTMLElement>()
const measurer = ref<HTMLElement>()
const kept = ref<number[] | null>(null)
function fit() {
  const box = rowBox.value
  const m = measurer.value
  if (!box || !m) return
  const items = Array.from(m.querySelectorAll<HTMLElement>('[data-measure-tab]'))
  const moreWidth = m.querySelector<HTMLElement>('[data-measure-more]')?.offsetWidth ?? 80
  const active = split.value.row.findIndex(t => t.path === activePath.value)
  kept.value = fitTabs(
    items.map(el => el.getBoundingClientRect().width),
    box.clientWidth,
    moreWidth,
    split.value.menu.length > 0,
    active,
  )
}
const rowTabs = computed(() => (kept.value ? split.value.row.filter((_, i) => kept.value!.includes(i)) : split.value.row))
const menuTabs = computed(() => [...split.value.row.filter(t => !rowTabs.value.includes(t)), ...split.value.menu])
/** Where the hidden ones begin in the menu (a line above them). */
const firstHidden = computed(() => menuTabs.value.length - split.value.menu.length)
/** Changes waiting in the tabs behind «Más»: shown on the button, since those tabs are out of sight. */
const waitingInMenu = computed(() => menuTabs.value.reduce((n, tab) => n + (waiting.value[tab.path] ?? 0), 0))
let observer: ResizeObserver | null = null
watch([split, locale], () => nextTick(fit))

const more = ref(false)
const moreHolder = ref<HTMLElement>()
const moreTrigger = ref<HTMLElement>()
const moreBox = ref<HTMLElement>()
const moreX = ref<number | null>(null)
useDismiss(more, moreHolder, { trigger: moreTrigger, closeOn: () => route.fullPath })
async function toggleMore() {
  more.value = !more.value
  if (!more.value) return
  moreX.value = null
  await nextTick()
  if (moreTrigger.value && moreBox.value) {
    const r = moreTrigger.value.getBoundingClientRect()
    // Under the button, starting at its left edge, moved left as little as needed to stay on the screen.
    const pageWidth = document.documentElement.clientWidth
    const left = Math.max(8, Math.min(r.left, pageWidth - 8 - moreBox.value.offsetWidth))
    moreX.value = Math.round(left - r.left)
  }
}
/** Up and down move through the menu's links (Tab works too); Home and End go to the first and last. */
function moreKeys(event: KeyboardEvent) {
  const links = Array.from(moreBox.value?.querySelectorAll<HTMLElement>('a') ?? [])
  if (!links.length) return
  const at = links.indexOf(document.activeElement as HTMLElement)
  const to = { ArrowDown: at + 1, ArrowUp: at - 1, Home: 0, End: links.length - 1 }[event.key]
  if (to === undefined) return
  event.preventDefault()
  links[(to + links.length) % links.length]?.focus()
}
function openMoreWithKeys(event: KeyboardEvent) {
  if (event.key !== 'ArrowDown') return
  event.preventDefault()
  if (!more.value) toggleMore().then(() => moreBox.value?.querySelector<HTMLElement>('a')?.focus())
}

onMounted(() => {
  width = window.innerWidth
  window.addEventListener('resize', resized)
  observer = new ResizeObserver(fit)
  if (rowBox.value) observer.observe(rowBox.value)
  if (measurer.value) observer.observe(measurer.value)
  fit()
  // The web font changes the labels' widths once it is in.
  document.fonts?.ready.then(fit)
})
onBeforeUnmount(() => {
  window.removeEventListener('resize', resized)
  observer?.disconnect()
})

/** The offline identifier test page (a page of its own, server/offline-id.mjs), opened in this window: inside the installed app it stays in the app. */
const offlineIdPage = new URL('prueba-offline/', location.href.split('#')[0]).href

/** Light or dark (lib/theme.ts): the system's choice unless one is picked with this button, which goes to the next each press. */
const THEMES: Record<ThemePref, { label: string; icon: typeof Sun }> = {
  system: { label: 'Sistema', icon: Monitor },
  light: { label: 'Claro', icon: Sun },
  dark: { label: 'Oscuro', icon: Moon },
}
const themeTitle = computed(() =>
  t('Tema: {current} (pulsa para {next})', {
    current: t(THEMES[themePref.value].label),
    next: t(THEMES[nextPref(themePref.value)].label),
  }),
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
  <header class="app-header border-b border-brand-hover bg-brand text-white">
    <!-- One row from 1024 px; narrower, the tabs get a full-width row of their own under the title and the buttons. -->
    <div class="flex flex-wrap items-center gap-x-2 px-3 lg:flex-nowrap lg:px-4">
      <RouterLink to="/inicio" class="flex shrink-0 items-center gap-2 py-1 font-semibold" :aria-label="$t('Inicio')">
        <!-- The butterfly alone (no tile), with a pale edge for the dark bar. -->
        <img src="/logo.svg?v=1" alt="" class="h-9 w-auto" />
        <span class="max-sm:hidden">Ikiam Insectary DB</span>
      </RouterLink>
      <!-- The tabs: as many as fit, the rest in «Más» (lib/headerTabs.ts); the row never scrolls nor cuts a label. -->
      <div ref="rowBox" class="relative order-last flex w-full min-w-0 lg:order-none lg:w-auto lg:flex-1">
        <nav class="-mb-px flex min-w-0 items-stretch" :aria-label="$t('Secciones')">
          <RouterLink
            v-for="tab in rowTabs"
            :key="tab.path"
            :to="tab.path"
            :class="[TAB, { '!border-white !text-white': activePath === tab.path }]"
            :aria-current="activePath === tab.path ? 'page' : undefined"
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
          <div v-if="menuTabs.length" ref="moreHolder" class="relative flex shrink-0">
            <button
              ref="moreTrigger"
              type="button"
              :class="[TAB, 'inline-flex items-center gap-0.5']"
              :aria-expanded="more"
              aria-controls="more-tabs"
              data-more-trigger
              @click="toggleMore"
              @keydown="openMoreWithKeys"
            >
              {{ $t('Más') }}
              <span
                v-if="waitingInMenu"
                class="ml-1 rounded-full bg-amber-300 px-1.5 text-xs font-semibold text-amber-950"
                :title="
                  $tn(
                    waitingInMenu,
                    '{n} fila cambiada en la app, aún no en Google Sheets',
                    '{n} filas cambiadas en la app, aún no en Google Sheets',
                  )
                "
                >{{ waitingInMenu }}</span
              >
              <ChevronDown :size="14" aria-hidden="true" />
            </button>
            <ul
              v-if="more"
              id="more-tabs"
              ref="moreBox"
              data-more-menu
              class="absolute top-full z-50 mt-1 w-max max-w-[calc(100vw-1rem)] min-w-44 rounded-md border border-border-strong bg-surface-raised py-1 text-sm text-fg shadow-lg"
              :class="moreX === null ? 'left-0' : ''"
              :style="moreX === null ? undefined : { left: `${moreX}px` }"
              @keydown="moreKeys"
              @click="more = false"
            >
              <li
                v-for="(tab, i) in menuTabs"
                :key="tab.path"
                :class="{ 'mt-1 border-t border-border pt-1': i === firstHidden && i > 0 }"
              >
                <RouterLink
                  :to="tab.path"
                  class="flex min-h-10 items-center justify-between gap-3 px-3 py-2 hover:bg-surface-sunken pointer-coarse:min-h-11"
                >
                  {{ $t(tab.label) }}
                  <span v-if="waiting[tab.path]" class="rounded-full bg-amber-300 px-1.5 text-xs font-semibold text-amber-950">{{
                    waiting[tab.path]
                  }}</span>
                </RouterLink>
              </li>
            </ul>
          </div>
        </nav>
        <!-- Every tab of the row and «Más» at their real widths, out of sight: what fit() measures. -->
        <!-- In a box of no height, so the copy never widens the page. -->
        <div class="pointer-events-none invisible absolute top-0 left-0 h-0 w-full overflow-hidden" aria-hidden="true">
          <div ref="measurer" class="flex w-max">
            <span v-for="tab in split.row" :key="tab.path" :class="TAB" data-measure-tab
              >{{ $t(tab.label) }}<span v-if="waiting[tab.path]" class="ml-1 px-1.5 text-xs">{{ waiting[tab.path] }}</span></span
            >
            <span :class="[TAB, 'inline-flex items-center gap-0.5']" data-measure-more
              >{{ $t('Más')
              }}<span v-if="waitingInMenu || split.menu.some(t => waiting[t.path])" class="ml-1 px-1.5 text-xs">99</span
              ><ChevronDown :size="14"
            /></span>
          </div>
        </div>
      </div>
      <div class="ml-auto flex shrink-0 items-center gap-2">
        <DatabaseBadge />
        <!-- Light or dark: one button, the next choice each press. -->
        <button
          type="button"
          class="hit-target inline-flex size-8 shrink-0 items-center justify-center rounded text-on-brand-muted hover:bg-brand-hover hover:text-white"
          :title="themeTitle"
          :aria-label="themeTitle"
          data-theme-button
          @click="themePref = nextPref(themePref)"
        >
          <component :is="THEMES[themePref].icon" :size="17" aria-hidden="true" />
        </button>
        <!-- Interface language: English by default (lib/i18n.ts). -->
        <button
          class="hit-target inline-flex min-h-6 shrink-0 items-center rounded px-1.5 text-xs font-semibold text-on-brand-muted ring-1 ring-on-brand-muted/50 hover:bg-brand-hover hover:text-white"
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
            class="flex max-w-40 items-center gap-1 rounded px-2 py-1 text-sm hover:bg-brand-hover pointer-coarse:min-h-11"
            :aria-expanded="menu"
            :title="session.user?.displayName"
            @click="toggleMenu"
          >
            <span class="truncate">{{ session.user?.displayName }}</span> <ChevronDown :size="14" class="shrink-0" />
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
            <a
              v-if="session.isOfflineIdTester"
              :href="offlineIdPage"
              class="flex items-center gap-2 px-3 py-2 hover:bg-surface-sunken"
            >
              <FlaskConical :size="15" /> {{ $t('Prueba: identificador sin conexión') }}
            </a>
            <button class="flex w-full items-center gap-2 px-3 py-2 hover:bg-surface-sunken" @click="logout">
              <LogOut :size="15" /> {{ $t('Cerrar sesión') }}
            </button>
          </div>
        </div>
      </div>
    </div>
  </header>
</template>
