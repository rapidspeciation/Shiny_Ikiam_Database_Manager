<script setup lang="ts">
import ChoiceField from '../ChoiceField.vue'
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { Link, RefreshCw, Trash2, UserPlus } from 'lucide-vue-next'
import { useMonitoring } from '../../composables/useMonitoring'
import { api } from '../../lib/api'
import { t } from '../../lib/i18n'
import { errorText, notify } from '../../lib/notice'
import { useSession } from '../../stores/session'

/**
 * Wikiloc links pasted here (or shared from the phone) and "Buscar nuevos"
 * on followed profiles become jobs for the server's Camoufox worker
 * (tools/wikiloc/worker.mjs); finished walks appear for review.
 */
interface Job {
  id: string
  kind: 'trail' | 'profile'
  target: string
  status: 'queued' | 'running' | 'done' | 'failed'
  message: string | null
  createdAt: string
  updatedAt: string
}
interface Profile {
  id: string
  wikilocUser: string
  name: string | null
  pattern: string
  lastChecked: string | null
  collector: string | null
}

const session = useSession()
const { loadWalks, reopenWalk, options } = useMonitoring()
const emit = defineEmits<{ reopened: [walkId: string] }>()
const link = ref('')
const jobs = ref<Job[]>([])
const workerSeen = ref<string | null>(null)
const profiles = ref<Profile[]>([])
const showProfiles = ref(false)
/** Phones show only the jobs still running; the rest on demand. */
const showJobs = ref(false)
const newProfile = ref('')
const newCollector = ref('')
// "NA - Missing data" is no one to follow.
const collectors = computed(() => (options.value.Collector || []).filter(c => / - /.test(c) && !/^NA\b/.test(c)))
let timer: ReturnType<typeof setTimeout> | null = null

const active = computed(() => jobs.value.filter(j => j.status === 'queued' || j.status === 'running'))
const recent = computed(() => jobs.value.slice(0, 4))
/** The server worker asks for work every 30 s; detect a missing heartbeat. */
const workerOnline = computed(() => !!workerSeen.value && Date.now() - Date.parse(workerSeen.value) < 2 * 60_000)

async function refresh() {
  try {
    const before = new Set(active.value.map(j => j.id))
    const data = await api<{ jobs: Job[]; workerSeen: string | null }>('monitoring/wikiloc/jobs')
    jobs.value = data.jobs
    workerSeen.value = data.workerSeen
    const finished = data.jobs.filter(j => before.has(j.id) && (j.status === 'done' || j.status === 'failed'))
    if (finished.length) {
      await loadWalks()
      for (const j of finished) notify(j.message || t('Listo'), j.status === 'done' ? 'success' : 'error')
    }
  } catch {
    /* offline: try again later */
  }
  timer = setTimeout(refresh, active.value.length ? 5000 : 60_000)
}
function soon() {
  if (timer) clearTimeout(timer)
  timer = setTimeout(refresh, 1500)
}

/** Queues a Wikiloc link (any text containing one works, as shared from the Wikiloc app). */
async function queue(text = link.value) {
  if (!text.trim()) return
  try {
    const result = await api<{ known: string | null; walkId: string | null; name: string | null }>('monitoring/wikiloc/links', {
      method: 'POST',
      body: { text },
    })
    link.value = ''
    soon()
    // Already imported: it only comes back for review if the person says so.
    if (result.known === 'imported' && result.walkId) {
      const name = result.name || t('Esa ruta')
      if (
        !confirm(t('«{name}» ya se importó. ¿Revisarla de nuevo? Vuelve a "por revisar" y se lee otra vez de Wikiloc.', { name }))
      )
        return notify(t('Se vuelve a leer de Wikiloc; la ruta sigue importada.'), 'info', 3000)
      await reopenWalk(result.walkId)
      emit('reopened', result.walkId)
      return notify(t('Ruta de nuevo por revisar.'), 'success', 3000)
    }
    notify(
      result.known
        ? t('Esa ruta ya estaba por revisar; se actualizará.')
        : t('Enlace en cola: el recorrido aparecerá aquí en unos minutos.'),
      'success',
      3000,
    )
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
defineExpose({ queue })

async function sync() {
  try {
    await api('monitoring/wikiloc/sync', { method: 'POST', body: {} })
    notify(t('Buscando rutas nuevas de monitoreo en los perfiles seguidos…'), 'success')
    soon()
  } catch (e) {
    notify(errorText(e), 'error')
    if ((e as { code?: string }).code === 'NO_PROFILES') showProfiles.value = true
  }
}

async function loadProfiles() {
  try {
    profiles.value = (await api<{ profiles: Profile[] }>('monitoring/wikiloc/profiles')).profiles
  } catch {
    /* ignore */
  }
}
async function follow() {
  try {
    await api('monitoring/wikiloc/profiles', {
      method: 'POST',
      body: { url: newProfile.value, collector: newCollector.value || null },
    })
    newProfile.value = ''
    newCollector.value = ''
    await loadProfiles()
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
async function unfollow(p: Profile) {
  if (!confirm(t('¿Dejar de seguir el perfil {name}?', { name: p.name || p.wikilocUser }))) return
  await api(`monitoring/wikiloc/profiles/${encodeURIComponent(p.id)}`, { method: 'DELETE', body: {} }).catch(e =>
    notify(errorText(e), 'error'),
  )
  await loadProfiles()
}

// Spanish; t() where shown.
const STATUS = { queued: 'en cola', running: 'procesando…', done: 'listo', failed: 'falló' }
const ago = (iso: string | null) => {
  if (!iso) return t('nunca')
  const minutes = Math.round((Date.now() - Date.parse(iso)) / 60_000)
  return minutes < 1
    ? t('ahora')
    : minutes < 60
      ? t('hace {n} min', { n: minutes })
      : t('hace {n} h', { n: Math.round(minutes / 60) })
}
const label = (j: Job) =>
  j.kind === 'profile'
    ? t('Perfil {name}', { name: profiles.value.find(p => p.wikilocUser === j.target)?.name || j.target })
    : (/\/([^/]+)-\d+$/.exec(j.target)?.[1] || j.target).replace(/-/g, ' ')

onMounted(() => {
  refresh()
  loadProfiles()
})
onBeforeUnmount(() => {
  if (timer) clearTimeout(timer)
})
watch(showProfiles, v => v && loadProfiles())
</script>

<template>
  <div class="border-b border-stone-200 bg-white px-3 py-2 text-sm sm:px-4">
    <form class="flex flex-wrap items-center gap-2" @submit.prevent="queue()">
      <label class="flex min-w-64 flex-1 items-center gap-2">
        <Link :size="15" class="shrink-0 text-stone-500" />
        <input
          v-model="link"
          class="field-input"
          :placeholder="$t('Pega el enlace de una ruta de Wikiloc')"
          :disabled="!session.canEdit"
          :aria-label="$t('Enlace de Wikiloc')"
        />
      </label>
      <button class="btn" :disabled="!session.canEdit || !link.trim()">{{ $t('Traer') }}</button>
      <button type="button" class="btn" :disabled="!session.canEdit" @click="sync">
        <RefreshCw :size="15" :class="{ 'animate-spin': active.some(j => j.kind === 'profile') }" />
        {{ $t('Buscar nuevos') }}
        <span class="-ml-1 hidden sm:inline">{{ $t('en Wikiloc') }}</span>
      </button>
      <button type="button" class="btn-ghost text-xs underline" @click="showProfiles = !showProfiles">
        {{ $t('Perfiles seguidos ({n})', { n: profiles.length }) }}
      </button>
      <span
        class="text-xs"
        :class="workerOnline ? 'text-brand-700' : 'text-amber-800'"
        :title="$t('Última señal: {ago}', { ago: ago(workerSeen) })"
      >
        ● <span class="hidden sm:inline">{{ $t('Importador de Wikiloc') }}</span>
        {{ workerOnline ? $t('activo') : $t('sin señal ({ago})', { ago: ago(workerSeen) }) }}
      </span>
      <button
        v-if="recent.length"
        type="button"
        class="btn-ghost text-xs underline sm:hidden"
        :aria-expanded="showJobs"
        @click="showJobs = !showJobs"
      >
        {{ $t('Trabajos ({n})', { n: recent.length }) }}
      </button>
    </form>

    <div v-if="showProfiles" class="mt-2 rounded-md border border-stone-200 bg-stone-50 p-2 text-xs">
      <p class="mb-1 text-stone-600">
        {{
          $t(
            '“Buscar nuevos” revisa estos perfiles y trae las rutas cuyo título contiene el patrón (p. ej. “monitor”) y que aún no están en la app, asignadas al recolector del perfil.',
          )
        }}
      </p>
      <ul class="mb-2 space-y-1">
        <li v-for="p in profiles" :key="p.id" class="flex items-center gap-2">
          <a
            :href="`https://es.wikiloc.com/wikiloc/user.do?id=${p.wikilocUser}`"
            target="_blank"
            rel="noopener"
            class="underline"
          >
            {{ p.name || $t('Perfil {name}', { name: p.wikilocUser }) }}
          </a>
          <span class="text-stone-500">{{
            $t('{collector} · título con “{pattern}” · revisado {ago}', {
              collector: p.collector || $t('sin recolector'),
              pattern: p.pattern,
              ago: ago(p.lastChecked),
            })
          }}</span>
          <button class="btn-ghost" :title="$t('Dejar de seguir')" @click="unfollow(p)"><Trash2 :size="13" /></button>
        </li>
      </ul>
      <form class="flex gap-2" @submit.prevent="follow">
        <input
          v-model="newProfile"
          class="field-input"
          :placeholder="$t('Enlace del perfil: https://es.wikiloc.com/wikiloc/user.do?id=…')"
          :aria-label="$t('Enlace del perfil de Wikiloc')"
        />
        <ChoiceField
          v-model="newCollector"
          class="field-input max-w-56"
          :aria-label="$t('Recolector de ese perfil')"
          :placeholder="$t('Recolector…')"
          :freetext="false"
          allow-empty
          :options="collectors"
        />
        <button class="btn" :disabled="!newProfile.trim()"><UserPlus :size="14" /> {{ $t('Seguir') }}</button>
      </form>
    </div>

    <ul v-if="recent.length" class="mt-1.5 space-y-0.5 text-xs text-stone-600">
      <li
        v-for="j in recent"
        :key="j.id"
        class="truncate"
        :class="{ 'hidden sm:list-item': !showJobs && j.status !== 'queued' && j.status !== 'running' }"
      >
        <span
          class="mr-1 rounded px-1.5 py-0.5"
          :class="{
            'bg-amber-100 text-amber-900': j.status === 'queued' || j.status === 'running',
            'bg-brand-50 text-brand-700': j.status === 'done',
            'bg-red-100 text-red-800': j.status === 'failed',
          }"
          >{{ $t(STATUS[j.status]) }}</span
        >
        {{ label(j) }} · {{ ago(j.updatedAt) }}<template v-if="j.message"> — {{ j.message }}</template>
      </li>
    </ul>
    <p v-if="active.length && !workerOnline" class="mt-1 text-xs text-amber-800">
      {{ $t('El importador de Wikiloc no responde; los enlaces quedan en cola y se procesarán cuando se restablezca.') }}
    </p>
  </div>
</template>
