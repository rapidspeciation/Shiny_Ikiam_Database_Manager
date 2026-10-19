<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { ArrowUpCircle, Check, ExternalLink, History, Loader2, RotateCcw, X, XCircle } from 'lucide-vue-next'
import { api } from '../../lib/api'
import { errorText, notify } from '../../lib/notice'
import { t, tn } from '../../lib/i18n'
import { dayFirst } from '../../lib/dates'

/**
 * Updating T3 Code (admins): what the update will do before it starts (the version jump, its
 * release notes and their "Before you update", a warning for a major version, the Claude
 * sessions it will cut), its steps and log while it runs (server/t3admin.mjs reads them from
 * scripts/t3-update.mjs), the checks of what the app needs from T3 afterwards, and going back to
 * a version T3 kept.
 */
export interface T3Status {
  current: string | null
  protocol: number | null
  answering: boolean
  latest: string | null
  updateAvailable: boolean
  release: { version: string; name: string; url: string; publishedAt: string | null; before: string | null } | null
  jump: { major: boolean; orchestratorV2: boolean } | null
  sessions: number
  terminals: number
  updating: boolean
  run: {
    step: string
    from: string | null
    to: string | null
    back?: boolean
    backup: string | null
    startedAt: string
    endedAt?: string
    error?: string
    tokenError?: string
    steps: { step: string; seconds: number; detail: string }[]
    running: boolean
  } | null
  log: string
  previous: string[]
  notesUrl: string | null
}
interface Check {
  id: string
  ok: boolean
  detail: string
}

const props = defineProps<{ status: T3Status | null }>()
const emit = defineEmits<{ close: []; changed: [status: T3Status]; reconnect: [] }>()
const status = ref<T3Status | null>(props.status)
watch(
  () => props.status,
  s => (status.value = s),
)
/** When this dialog started an update: a run that began before is not this one. */
const asked = ref<{ at: number; version: string } | null>(null)
const checks = ref<Check[] | null>(null)
const checking = ref(false)
const starting = ref(false)
const logBox = ref<HTMLElement>()

/** The run on screen: the one under way, or the last one (this dialog's once it began). */
const run = computed(() => {
  const r = status.value?.run
  if (!r) return null
  if (asked.value && Date.parse(r.startedAt) < asked.value.at - 5000) return null
  return r
})
const waitingToStart = computed(() => !!asked.value && !run.value)
const running = computed(() => waitingToStart.value || !!run.value?.running || !!status.value?.updating)
const finished = computed(() => !!asked.value && !!run.value && !run.value.running && ['done', 'failed'].includes(run.value.step))

/** The update's steps as the script names them, in order (restarting: the lab only). */
const STEPS = ['checking', 'backup', 'installing', 'restarting', 'waiting', 'token', 'done'] as const
const stepLabel = (step: string) =>
  ({
    checking: t('Comprobar las versiones'),
    backup: t('Copiar la base de datos de T3'),
    installing: t('Descargar e instalar (t3 update)'),
    restarting: t('Reiniciar T3'),
    waiting: t('Esperar a que T3 responda'),
    token: t('Renovar el acceso de la app a T3'),
    done: t('Listo'),
    failed: t('Falló'),
  })[step] ?? step
/** A step's record, as scripts/t3-update.mjs names it, and how long it took. */
const RECORD: Record<string, string> = { checking: 'check', backup: 'backup', installing: 't3 update', restarting: 'restart', waiting: 'T3 answers', token: 'token' }
const took = (step: string) => run.value?.steps.find(x => x.step === RECORD[step])?.seconds
const shownSteps = computed(() => {
  const r = run.value
  const at = r ? STEPS.indexOf(r.step as (typeof STEPS)[number]) : 0
  return STEPS.filter(s => s !== 'restarting' || took('restarting') !== undefined || r?.step === 'restarting').map(s => ({
    step: s,
    state: r?.step === 'failed' ? 'idle' : r?.step === 'done' || STEPS.indexOf(s) < at ? 'done' : STEPS.indexOf(s) === at ? 'now' : 'idle',
  }))
})

const checkLabel = (id: string) =>
  ({
    answers: t('T3 responde'),
    token: t('Acceso de la app a T3'),
    pairing: t('Enlace de inicio de sesión'),
    commands: t('Crear chats y enviar mensajes'),
    chats: t('Leer los chats (propuestas, dueño del chat)'),
    page: t('Puente de la página de T3'),
  })[id] ?? id

let timer: ReturnType<typeof setTimeout> | undefined
async function refresh() {
  try {
    status.value = await api<T3Status>('admin/t3')
    emit('changed', status.value)
  } catch {
    /* tried again */
  }
}
async function poll() {
  clearTimeout(timer)
  await refresh()
  await nextTick()
  if (logBox.value) logBox.value.scrollTop = logBox.value.scrollHeight
  if (finished.value) {
    if (run.value?.step === 'done') {
      notify(run.value.back ? t('T3 volvió a {version}', { version: run.value.to }) : t('T3 actualizado a {version}', { version: run.value.to }), 'success')
      emit('reconnect')
      void runChecks()
    }
    return
  }
  if (running.value) timer = setTimeout(poll, 2000)
}
async function runChecks() {
  checking.value = true
  try {
    checks.value = (await api<{ checks: Check[] }>('admin/t3/checks')).checks
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    checking.value = false
  }
}
async function start(version: string, back = false) {
  const from = status.value?.current
  if (back && !confirm(t('¿Volver a T3 {to}? T3 se reinicia en la versión {to}. Lo que se hizo en {from} (chats nuevos, ajustes) no se ve en {to}; sigue guardado para cuando se actualice otra vez.', { to: version, from }))) return
  starting.value = true
  checks.value = null
  try {
    await api('admin/t3', { method: 'POST', body: { version } })
    asked.value = { at: Date.now(), version }
    timer = setTimeout(poll, 1500)
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    starting.value = false
  }
}
const onKey = (e: KeyboardEvent) => e.key === 'Escape' && emit('close')
onMounted(() => {
  window.addEventListener('keydown', onKey)
  // Opened while an update runs (or right after): its progress.
  if (status.value?.updating) {
    asked.value = { at: Date.parse(status.value.run?.startedAt ?? '') || Date.now(), version: status.value.run?.to ?? '' }
    void poll()
  }
})
onBeforeUnmount(() => {
  clearTimeout(timer)
  window.removeEventListener('keydown', onKey)
})
/** A moment, day first, in this computer's time. */
function when(iso: string) {
  const d = new Date(iso)
  const two = (n: number) => String(n).padStart(2, '0')
  return `${dayFirst(`${d.getFullYear()}-${two(d.getMonth() + 1)}-${two(d.getDate())}`)} ${two(d.getHours())}:${two(d.getMinutes())}`
}
const seconds = (n: number) => (n < 1 ? '<1 s' : `${Math.round(n)} s`)
</script>

<template>
  <div class="fixed inset-0 z-50 flex items-end justify-center bg-black/30 sm:items-center" @click.self="emit('close')">
    <section
      class="flex max-h-full w-full flex-col overflow-y-auto rounded-t-2xl bg-white p-4 text-sm shadow-xl sm:max-w-2xl sm:rounded-2xl"
      role="dialog"
      :aria-label="$t('Actualizar T3 Code')"
    >
      <header class="flex items-center gap-2">
        <ArrowUpCircle :size="20" class="text-stone-600" />
        <h2 class="flex-1 text-lg font-semibold">{{ $t('Actualizar T3 Code') }}</h2>
        <button class="btn-ghost h-11 w-11 justify-center" :aria-label="$t('Cerrar')" @click="emit('close')"><X :size="22" /></button>
      </header>

      <!-- Before: what the update will do. -->
      <template v-if="status && !asked">
        <p class="mt-2">
          {{ $t('T3 Code en este servidor: {current}', { current: status.current ?? '—' }) }}
          <span v-if="!status.answering" class="ml-1 text-red-700">· {{ $t('no responde') }}</span>
        </p>
        <!-- The last update, as it ended (its copy of the database is the way back to it). -->
        <p v-if="status.run && !status.run.running" class="mt-1 text-xs text-stone-600">
          {{ $t('Última actualización: {from} → {to}, el {date}', { from: status.run.from, to: status.run.to, date: when(status.run.startedAt) }) }}
          · <span :class="status.run.step === 'done' ? 'text-emerald-800' : 'text-red-700'">{{ stepLabel(status.run.step) }}</span>
          <template v-if="status.run.step === 'failed'"> ({{ status.run.error }})</template>
        </p>
        <template v-if="status.updateAvailable && status.release">
          <p class="mt-1 text-base">
            <strong>{{ status.current }} → {{ status.release.version }}</strong>
            <a :href="status.release.url" target="_blank" rel="noopener" class="ml-2 inline-flex items-center gap-1 text-emerald-800 underline"
              >{{ $t('Notas de la versión') }} <ExternalLink :size="12"
            /></a>
          </p>
          <div v-if="status.jump?.major" class="mt-2 rounded border border-amber-300 bg-amber-50 p-2 text-amber-950" role="alert">
            <strong>{{ $t('Versión mayor.') }}</strong>
            {{ $t('Cambia cómo funciona T3: lee las notas antes de actualizar.') }}
            <template v-if="status.jump.orchestratorV2">
              {{
                $t(
                  'Al arrancar por primera vez, {to} copia los chats a una base de datos nueva (statev2.sqlite), en un solo sentido: los chats conservan sus mensajes, pero las llamadas a herramientas y razonamientos de los chats viejos quedan solo en el archivo anterior. Si luego se vuelve a {from}, se ven los chats como estaban antes de actualizar.',
                  { to: status.release.version, from: status.current },
                )
              }}
            </template>
          </div>
          <details v-if="status.release.before" class="mt-2 rounded border border-stone-200 p-2" :open="status.jump?.major">
            <summary class="cursor-pointer font-medium">{{ $t('«Antes de actualizar», de las notas (en inglés)') }}</summary>
            <p class="mt-1 max-h-48 overflow-y-auto whitespace-pre-wrap text-xs text-stone-700">{{ status.release.before }}</p>
          </details>
          <p class="mt-2" :class="status.sessions ? 'text-amber-900' : 'text-emerald-800'">
            {{
              status.sessions
                ? tn(
                    status.sessions,
                    'T3 tiene {n} sesión de Claude abierta: se cortarán las respuestas en curso (los chats guardados no se pierden).',
                    'T3 tiene {n} sesiones de Claude abiertas: se cortarán las respuestas en curso (los chats guardados no se pierden).',
                  )
                : $t('Ninguna sesión de Claude abierta en T3: buen momento para actualizar.')
            }}
            <template v-if="status.terminals">
              {{ tn(status.terminals, 'También se cierra {n} terminal de T3.', 'También se cierran {n} terminales de T3.') }}</template
            >
          </p>
          <p class="mt-1 text-stone-600">
            {{ $t('Antes de actualizar se guarda una copia de la base de datos de T3 (en ~/.t3/backups, quedan las 3 últimas). T3 se reinicia: tarda uno o dos minutos.') }}
          </p>
          <div class="mt-3 flex gap-2">
            <button class="btn-primary" :disabled="starting" @click="start(status.release!.version)">
              <ArrowUpCircle :size="16" /> {{ $t('Actualizar a {version}', { version: status.release.version }) }}
            </button>
            <button class="btn" @click="emit('close')">{{ $t('Cancelar') }}</button>
          </div>
        </template>
        <p v-else class="mt-1 text-stone-600">
          {{ status.latest ? $t('Es la última versión estable de T3 Code') : $t('No se pudo consultar la última versión') }}
        </p>
      </template>

      <!-- While it runs, and after. -->
      <template v-if="asked">
        <p class="mt-2 text-base">
          <strong>{{ run?.from ?? status?.current }} → {{ run?.to ?? asked.version }}</strong>
          <span v-if="run?.back" class="ml-1 text-stone-600">({{ $t('volver a una versión anterior') }})</span>
        </p>
        <ol class="mt-2 space-y-1">
          <li v-if="waitingToStart" class="flex items-center gap-2"><Loader2 :size="14" class="animate-spin" /> {{ $t('Iniciando…') }}</li>
          <li v-for="s in shownSteps" v-else :key="s.step" class="flex items-center gap-2" :class="s.state === 'idle' ? 'text-stone-400' : ''">
            <Check v-if="s.state === 'done'" :size="14" class="text-emerald-700" />
            <Loader2 v-else-if="s.state === 'now'" :size="14" class="animate-spin" />
            <span v-else class="inline-block w-3.5" />
            {{ stepLabel(s.step) }}
            <span v-if="took(s.step) !== undefined" class="text-xs text-stone-500">· {{ seconds(took(s.step)!) }}</span>
          </li>
        </ol>
        <p v-if="run?.step === 'waiting' && status?.jump?.orchestratorV2 !== false" class="mt-1 text-xs text-stone-500">
          {{ $t('El primer arranque de una versión mayor puede tardar más: copia los chats a su base de datos nueva.') }}
        </p>
        <p v-if="run?.step === 'failed'" class="mt-2 rounded bg-red-50 p-2 text-red-900" role="alert">
          <XCircle :size="14" class="inline" /> {{ run.error }}
        </p>
        <p v-if="run?.tokenError" class="mt-2 rounded bg-amber-50 p-2 text-amber-900">{{ $t('El acceso de la app no se renovó:') }} {{ run.tokenError }}</p>
        <p v-if="run?.backup" class="mt-2 text-xs text-stone-600">{{ $t('Copia de seguridad:') }} <code>{{ run.backup }}</code></p>
        <pre ref="logBox" class="mt-2 max-h-48 overflow-y-auto rounded bg-stone-900 p-2 text-xs leading-snug break-all whitespace-pre-wrap text-stone-100">{{ status?.log || '…' }}</pre>
      </template>

      <!-- Afterwards (or on demand): what the app needs from T3. -->
      <section v-if="checks || checking" class="mt-3">
        <h3 class="font-medium">{{ $t('Comprobaciones de la app con T3') }}</h3>
        <p v-if="checking" class="flex items-center gap-2 text-stone-600"><Loader2 :size="14" class="animate-spin" /> {{ $t('Comprobando…') }}</p>
        <ul v-else class="mt-1 space-y-1">
          <li v-for="c in checks" :key="c.id" class="flex items-start gap-2">
            <Check v-if="c.ok" :size="14" class="mt-0.5 shrink-0 text-emerald-700" />
            <XCircle v-else :size="14" class="mt-0.5 shrink-0 text-red-700" />
            <span
              ><strong class="font-medium">{{ checkLabel(c.id) }}</strong> <span class="text-stone-600">{{ c.detail }}</span></span
            >
          </li>
        </ul>
      </section>
      <div v-if="finished || (!asked && status)" class="mt-3 flex flex-wrap gap-2">
        <button class="btn" :disabled="checking" @click="runChecks">{{ $t('Comprobar la app con T3') }}</button>
        <button v-if="finished" class="btn" @click="emit('reconnect')"><RotateCcw :size="14" /> {{ $t('Volver a conectar T3') }}</button>
        <button v-if="finished" class="btn-primary ml-auto" @click="emit('close')">{{ $t('Cerrar') }}</button>
      </div>

      <!-- Going back: T3 keeps the versions it had. -->
      <details v-if="status?.previous.length && !running" class="mt-3 rounded border border-stone-200 p-2">
        <summary class="cursor-pointer font-medium"><History :size="14" class="inline" /> {{ $t('Volver a una versión anterior') }}</summary>
        <p class="mt-1 text-stone-600">
          {{ $t('T3 guarda las versiones que tuvo. Volver a una es lo mismo que actualizar a ella (con su copia de seguridad antes).') }}
        </p>
        <div class="mt-2 flex flex-wrap gap-2">
          <button v-for="v in status.previous.slice(0, 3)" :key="v" class="btn" @click="start(v, true)">
            {{ $t('Volver a {version}', { version: v }) }}
          </button>
        </div>
      </details>
    </section>
  </div>
</template>
