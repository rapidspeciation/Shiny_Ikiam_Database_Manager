<script setup lang="ts">
import { computed, onMounted, ref } from 'vue'
import { ArrowUpCircle, ExternalLink, ListChecks, RefreshCw } from 'lucide-vue-next'
import { api } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import { useSession } from '../stores/session'
import T3Frame from '../components/T3Frame.vue'
import ProposalsLive from '../components/assistant/ProposalsLive.vue'
import { persistentRef } from '../lib/persist'
import { panelShare } from '../lib/proposals'
import type { T3Seen } from '../lib/t3Bridge'
import { t, tn } from '../lib/i18n'

/**
 * The assistant: T3 Code (the team's chats with Claude or Codex, on its own
 * address) with the changes it proposes beside it (Cambios propuestos), to
 * review, correct and apply in the sheet.
 */
/** T3's address (null: T3 is not configured on this server), once asked for. */
const t3Url = ref<string | null>(null)
const loaded = ref(false)
// Proposals (from T3 Code or any conversation) beside T3, or under it on phones; see ProposalsLive.
// Closed at first (T3 gets the whole width); a new proposal opens it, and the button toggles it.
const panel = persistentRef('assistant:proposals-open', false)
// At the right of T3 or below it (computers), with the share of the space dragged on the divider;
// on phones below T3. "Pantalla completa" puts it over T3; another browser tab: #/propuestas.
const layout = persistentRef<'right' | 'bottom'>('assistant:proposals-layout', 'right', { lasting: true })
const shares = persistentRef('assistant:proposals-share', { right: 42, bottom: 40 }, { lasting: true })
const full = ref(false)
const split = ref<HTMLElement>()
const dragging = ref<number | null>(null)
const share = computed(() => dragging.value ?? shares.value[layout.value])
/** Dragging the divider resizes the panel (T3's frame lets the pointer through meanwhile). */
function resize(down: PointerEvent) {
  const box = split.value?.getBoundingClientRect()
  const bar = down.currentTarget as HTMLElement
  if (!box) return
  down.preventDefault()
  bar.setPointerCapture(down.pointerId)
  const side = layout.value
  const at = (e: PointerEvent) =>
    side === 'right' ? panelShare(e.clientX, box.left, box.width) : panelShare(e.clientY, box.top, box.height)
  dragging.value = share.value
  const move = (e: PointerEvent) => (dragging.value = at(e))
  const up = () => {
    if (dragging.value !== null) shares.value = { ...shares.value, [side]: dragging.value }
    dragging.value = null
    bar.removeEventListener('pointermove', move)
    bar.removeEventListener('pointerup', up)
    bar.removeEventListener('pointercancel', up)
  }
  bar.addEventListener('pointermove', move)
  bar.addEventListener('pointerup', up)
  bar.addEventListener('pointercancel', up)
}
/** The divider with the keyboard: arrows give the panel more or less room. */
function nudge(event: KeyboardEvent) {
  const step = ({ ArrowLeft: 5, ArrowUp: 5, ArrowRight: -5, ArrowDown: -5 } as Record<string, number>)[event.key]
  if (!step) return
  event.preventDefault()
  shares.value = { ...shares.value, [layout.value]: Math.min(80, Math.max(20, share.value + step)) }
}
function place(side: 'right' | 'bottom') {
  layout.value = side
  full.value = false
}
const waiting = ref(0)
const fresh = ref(false)
function arrived() {
  // A new proposal shows the panel, so the person sees the edit as the assistant makes it.
  fresh.value = !panel.value
  panel.value = true
  setTimeout(() => (fresh.value = false), 4000)
}
const t3Frame = ref<InstanceType<typeof T3Frame>>()
/** The chat T3's frame shows (its bridge), followed by Cambios propuestos. */
const t3Seen = ref<T3Seen | null>(null)

// ------------------------------------------------------------ updating T3 (admins)
interface T3Version {
  current: string | null
  latest: string | null
  updateAvailable: boolean
  sessions: number
  updating: boolean
  log: string
}
const session = useSession()
const t3Version = ref<T3Version | null>(null)
const t3Updating = ref(false)
async function loadT3Version() {
  if (!session.isAdmin) return
  try {
    t3Version.value = await api<T3Version>('admin/t3')
  } catch {
    t3Version.value = null
  }
}
onMounted(loadT3Version)
/** Starts `t3 update` on the server, then waits for T3 to come back on the new version and reconnects. */
async function updateT3() {
  const v = t3Version.value
  if (!v?.updateAvailable) return
  const cut = v.sessions
    ? '\n\n' +
      tn(
        v.sessions,
        'T3 tiene {n} chat abierto: se cortarán las respuestas en curso (los chats guardados no se pierden).',
        'T3 tiene {n} chats abiertos: se cortarán las respuestas en curso (los chats guardados no se pierden).',
      )
    : ''
  const question = t('¿Actualizar T3 Code de {from} a {to}? T3 se reinicia (tarda un minuto).', { from: v.current, to: v.latest })
  if (!confirm(question + cut)) return
  t3Updating.value = true
  try {
    await api('admin/t3', { method: 'POST', body: {} })
    const from = v.current
    for (let i = 0; i < 60; i++) {
      await new Promise(r => setTimeout(r, 5000))
      await loadT3Version()
      const now = t3Version.value
      if (now && !now.updating && now.current !== from) {
        notify(t('T3 actualizado a {version}', { version: now.current }), 'success')
        t3Frame.value?.connect(true)
        return
      }
      if (now && !now.updating && i > 2) break
    }
    notify(`${t('T3 no cambió de versión. Últimas líneas del registro:')}\n${t3Version.value?.log || '—'}`, 'error')
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    t3Updating.value = false
  }
}

onMounted(async () => {
  try {
    t3Url.value = (await api<{ url: string | null }>('t3/status')).url
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    loaded.value = true
  }
})
</script>

<template>
  <div class="flex h-full flex-col">
    <!-- One slim bar, so T3 keeps nearly the whole screen. -->
    <div v-if="t3Url" class="flex items-center gap-1 border-b border-stone-200 bg-stone-100 px-2 py-0.5 text-sm">
      <button
        class="ml-auto flex items-center gap-1 rounded px-2 py-0.5"
        :class="
          fresh ? 'animate-pulse bg-emerald-600 text-white' : waiting ? 'bg-emerald-100 text-emerald-900' : 'text-stone-500'
        "
        :title="panel ? $t('Ocultar los cambios propuestos') : $t('Mostrar los cambios propuestos')"
        @click="panel = !panel"
      >
        <ListChecks :size="14" /> {{ $t('Cambios propuestos ({n})', { n: waiting }) }}
      </button>
      <button
        v-if="t3Version?.updateAvailable || t3Updating"
        class="flex items-center gap-1 rounded bg-amber-100 px-2 py-0.5 text-amber-900 hover:bg-amber-200 disabled:opacity-60"
        :disabled="t3Updating"
        :title="
          $t('Hay una versión nueva de T3 Code ({latest}); tienes la {current}', {
            latest: t3Version?.latest,
            current: t3Version?.current,
          })
        "
        @click="updateT3"
      >
        <ArrowUpCircle :size="14" :class="{ 'animate-spin': t3Updating }" />
        {{
          t3Updating
            ? $t('Actualizando T3…')
            : $t('Actualizar T3 ({from} → {to})', { from: t3Version?.current, to: t3Version?.latest })
        }}
      </button>
      <span
        v-else-if="t3Version"
        class="px-1 text-xs text-stone-500"
        :title="t3Version.latest ? $t('Es la última versión estable de T3 Code') : $t('No se pudo consultar la última versión')"
        >T3 {{ t3Version.current }}<template v-if="t3Version.latest"> · {{ $t('al día') }}</template></span
      >
      <button class="btn-ghost" :title="$t('Volver a conectar T3')" @click="t3Frame?.connect(true)">
        <RefreshCw :size="13" />
      </button>
      <a class="btn-ghost" :href="t3Url" target="_blank" rel="noopener" :title="$t('Abrir T3 en otra pestaña')"
        ><ExternalLink :size="13"
      /></a>
    </div>
    <div
      v-if="t3Url"
      ref="split"
      class="flex min-h-0 flex-1 flex-col"
      :class="{ 'md:flex-row': layout === 'right', 'select-none': dragging !== null }"
      :style="{ '--share': `${share}%` }"
    >
      <T3Frame
        ref="t3Frame"
        :url="t3Url"
        class="min-h-0 min-w-0 flex-1"
        :class="{ 'pointer-events-none': dragging !== null, hidden: panel && full }"
        @seen="value => (t3Seen = value)"
      />
      <!-- The divider: drag it (or use the arrow keys) to give the panel more or less room. -->
      <div
        v-show="panel && !full"
        role="separator"
        tabindex="0"
        :aria-orientation="layout === 'right' ? 'vertical' : 'horizontal'"
        :aria-valuenow="share"
        :aria-label="$t('Tamaño de los cambios propuestos')"
        :title="$t('Arrastra para cambiar el tamaño')"
        class="hidden shrink-0 touch-none bg-stone-200 hover:bg-emerald-400 focus:bg-emerald-400 focus:outline-none md:block"
        :class="[
          layout === 'right' ? 'w-1.5 cursor-col-resize' : 'h-1.5 cursor-row-resize',
          { 'bg-emerald-500': dragging !== null },
        ]"
        @pointerdown="resize"
        @keydown="nudge"
      />
      <!-- Proposed edits beside T3 (under it on phones), updated live as the assistant and the person edit them. -->
      <ProposalsLive
        v-show="panel"
        :layout="layout"
        :full="full"
        :t3="t3Seen"
        class="border-stone-300"
        :class="
          full
            ? 'min-h-0 flex-1'
            : [
                'max-h-[45%] border-t md:max-h-none md:shrink-0 md:grow-0 md:basis-(--share)',
                layout === 'right' ? 'md:min-w-0 md:border-t-0 md:border-l' : 'md:min-h-0',
              ]
        "
        @count="n => (waiting = n)"
        @fresh="arrived"
        @close="panel = false"
        @layout="place"
        @full="full = !full"
      />
    </div>
    <p v-else-if="loaded" class="p-4 text-sm text-stone-600">
      {{ $t('T3 Code no está configurado en este servidor.') }}
    </p>
  </div>
</template>
