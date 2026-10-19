<script setup lang="ts">
import ChoiceField from '../ChoiceField.vue'
import DateField from '../DateField.vue'
import { computed, onActivated, onBeforeUnmount, onDeactivated, onMounted, ref, watch } from 'vue'
import { Bot, Check, Copy, ExternalLink, ListRestart, MessageSquarePlus, ScanSearch, X } from 'lucide-vue-next'
import ProposalsLive from '../assistant/ProposalsLive.vue'
import type { WikilocWalk } from '../../composables/useMonitoring'
import { api } from '../../lib/api'
import { askClaude } from '../../lib/askClaude'
import { formatSerial, isoToSerial } from '../../lib/dates'
import { t } from '../../lib/i18n'
import { errorText, notify } from '../../lib/notice'
import { persistentRef } from '../../lib/persist'
import { t3Host } from '../../lib/t3Host'
import { photoReadings, type PhotoReading } from '../../lib/walkReadings'

/**
 * One Wikiloc walk in Monitoreo → Importar: its captures as a proposal in the
 * review table every proposal uses (Cambios propuestos), each row with its
 * Wikiloc photo beside the table, and the day's SamplingDay_data row. The
 * first CAM_ID and tube fill the preserved butterflies in walk order; the AI
 * Identifier reads each photo in the background and the table shows its
 * species beside the one typed. «Preguntar a Claude» opens a chat with the
 * walk and its proposal, beside the table, for what only people know (the
 * envelopes, a butterfly that escaped).
 */
const props = defineProps<{
  walk: WikilocWalk
  /** Who walked it, as the panel found it (profile, title), and where from. */
  collector: string
  collectorFrom: string
  collectorChoices: { value: string; label: string; group?: string; hint?: string }[]
  /** The next free CAM and tube (Colecta's suggestion), offered for the first ones. */
  suggestCam: string | null
  suggestTube: string | null
}>()
const emit = defineEmits<{ close: []; changed: [] }>()

const date = ref(props.walk.date || '')
const collector = ref(props.collector)
watch(
  () => props.collector,
  c => {
    if (!collector.value) collector.value = c
  },
)
const firstCam = ref('')
const firstTube = ref('')
const proposalId = ref<string | null>(props.walk.proposalId ?? null)
const busy = ref(false)
const problems = ref<string[]>([])
const inSheet = ref<{ point: number; text: string; row: number }[]>([])
const ready = computed(() => /^\d{4}-\d{2}-\d{2}$/.test(date.value) && !!collector.value)
const photos = computed(() => props.walk.waypoints.reduce((n, p) => n + p.photos.length, 0))

/** Makes (or makes again, while nobody worked on it) the walk's proposal. */
async function prepare() {
  if (!ready.value) return
  busy.value = true
  try {
    const out = await api<{ proposalId: string; problems: string[]; inSheet: typeof inSheet.value }>('chat/proposals/from-walk', {
      method: 'POST',
      body: {
        walkId: props.walk.id,
        date: date.value,
        collector: collector.value,
        firstCam: firstCam.value.trim() || undefined,
        firstTube: firstTube.value.trim() || undefined,
      },
    })
    proposalId.value = out.proposalId
    problems.value = out.problems || []
    inSheet.value = out.inSheet || []
    emit('changed')
    follow()
  } catch (e) {
    const err = e as { code?: string; details?: { proposalId?: string } }
    if (err.code === 'walk_proposal_edited' && err.details?.proposalId) proposalId.value = err.details.proposalId
    notify(errorText(e), 'error', 6000)
  } finally {
    busy.value = false
  }
}
onMounted(() => {
  // A walk opened for the first time: its table at once.
  if (!proposalId.value && ready.value) prepare()
  else if (proposalId.value) follow()
})

// ------------------------------------------------------------ the photos' species (AI Identifier)

const reading = ref<{ read: number; total: number; installed: boolean; unavailable?: string | null } | null>(null)
let timer: ReturnType<typeof setTimeout> | null = null
/** Follows the identifier through the walk's photos, every 2 s until all are read. */
async function follow(start = false) {
  if (timer) clearTimeout(timer)
  try {
    const out = await api<{
      read: number
      total: number
      installed: boolean
      unavailable?: string
      photos: ({ photo: string; point: number } & PhotoReading)[]
    }>(`chat/walks/${props.walk.id}/identify`, start ? { method: 'POST', body: {} } : {})
    for (const p of out.photos) photoReadings.set(p.photo, p)
    reading.value = { read: out.read, total: out.total, installed: out.installed, unavailable: out.unavailable ?? null }
    const waiting = out.photos.some(p => p.status === 'queued' || p.status === 'reading')
    if (waiting) timer = setTimeout(() => follow(), 2000)
    // Never started (an older proposal): start it.
    else if (!start && out.installed && out.photos.some(p => p.status === 'not_started')) timer = setTimeout(() => follow(true), 200)
  } catch {
    timer = setTimeout(() => follow(), 10_000)
  }
}

// ------------------------------------------------------------ Claude beside the table

/** The chat each walk was asked about (kept): continued, or a new one. */
const chats = persistentRef<Record<string, { chat: string; at: string }>>('monitoring:walk-chats', {}, { lasting: true })
const earlier = computed(() => chats.value[props.walk.id] ?? null)
const chatOpen = ref(false)
const prompt = ref('')
const copied = ref(false)
const chatSlot = ref<HTMLElement>()
let onScreen = true

function askText() {
  const day = date.value ? formatSerial(isoToSerial(date.value)) : '?'
  return [
    `Monitoring walk from Wikiloc «${props.walk.name}» (${day}, ${collector.value.split(' - ')[0] || '?'}): walkId ${props.walk.id}, ${props.walk.url}.`,
    proposalId.value
      ? `Its rows are in proposal ${proposalId.value} (Monitoreo → Importar recorrido), each with its Wikiloc photo; read it with get_proposal and the walk with get_walk.`
      : 'Read it with get_walk.',
    'Help me finish it: I will tell you the CAM_IDs, tubes and what happened in the field (e.g. "start with CAM078900", "the 3rd one escaped"); change the proposal with update_proposal and tell me what to check.',
  ].join(' ')
}
async function ask(fresh: boolean) {
  chatOpen.value = true
  prompt.value = ''
  if (!fresh && earlier.value) {
    t3Host.open = { thread: earlier.value.chat, made: false }
    return
  }
  try {
    const out = await askClaude(askText(), t('Recorrido {date}', { date: date.value ? formatSerial(isoToSerial(date.value)) : '' }))
    if (out.chat) chats.value = { ...chats.value, [props.walk.id]: { chat: out.chat, at: new Date().toISOString() } }
    else prompt.value = out.prompt
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
function place() {
  if (onScreen) t3Host.slot = chatOpen.value ? (chatSlot.value ?? null) : null
}
watch([chatSlot, chatOpen], place, { flush: 'post' })
// A proposal's link clicked in the chat: this walk's is already on screen.
const onLink = (id: string) => id === proposalId.value
onMounted(() => (t3Host.onLink = onLink))
onActivated(() => {
  onScreen = true
  place()
  t3Host.onLink = onLink
})
onDeactivated(() => {
  onScreen = false
  t3Host.slot = null
})
onBeforeUnmount(() => {
  if (timer) clearTimeout(timer)
  if (t3Host.onLink === onLink) t3Host.onLink = null
  t3Host.slot = null
})
async function copyPrompt() {
  try {
    await navigator.clipboard.writeText(prompt.value)
    copied.value = true
    setTimeout(() => (copied.value = false), 2500)
  } catch {
    notify(t('No se pudo copiar: selecciona el texto y cópialo.'), 'error')
  }
}
const earlierDay = computed(() => (earlier.value ? formatSerial(isoToSerial(earlier.value.at.slice(0, 10))) : ''))
</script>

<template>
  <div class="flex min-h-0 flex-1 flex-col" data-walk-import>
    <div class="toolbar flex-wrap border-b border-stone-200 bg-white">
      <div class="min-w-0">
        <span class="field-label">{{ $t('Recorrido de Wikiloc') }}</span>
        <p class="flex items-center gap-1 pb-1.5 text-sm">
          <span class="max-w-96 truncate font-medium" :title="walk.name">{{ walk.name }}</span>
          <a :href="walk.url" target="_blank" rel="noopener" class="btn-ghost p-0.5" :title="$t('Abrir en Wikiloc')"><ExternalLink :size="13" /></a>
          <span class="text-xs text-stone-500">
            · {{ $t('{n} puntos', { n: walk.waypoints.length }) }} · {{ $t('{n} fotos', { n: photos }) }}
          </span>
        </p>
      </div>
      <label>
        <span class="field-label">{{ $t('Fecha') }}</span>
        <DateField v-model="date" class="field-input" />
      </label>
      <label class="min-w-52">
        <span class="field-label">
          {{ $t('Recolector') }}
          <span v-if="collectorFrom && collector === props.collector" class="font-normal text-stone-500">({{ $t(collectorFrom) }})</span>
        </span>
        <ChoiceField
          v-model="collector"
          class="field-input"
          :class="{ 'border-amber-500': !collector }"
          :placeholder="$t('Elegir…')"
          :freetext="false"
          :options="collectorChoices"
        />
      </label>
      <label class="w-36">
        <span class="field-label">{{ $t('Primer CAM_ID') }}</span>
        <input v-model="firstCam" class="field-input" :placeholder="suggestCam || 'CAM0…'" spellcheck="false" @keydown.enter="prepare" />
      </label>
      <label class="w-36">
        <span class="field-label">{{ $t('Primer tubo') }}</span>
        <input v-model="firstTube" class="field-input" :placeholder="suggestTube || 'FS…'" spellcheck="false" @keydown.enter="prepare" />
      </label>
      <div class="flex flex-wrap items-end gap-1.5 pb-0.5">
        <button
          v-if="!firstCam && !firstTube && (suggestCam || suggestTube)"
          class="btn-ghost py-0.5 text-xs underline"
          :title="$t('El siguiente CAM libre de la lista y el siguiente tubo de la gradilla en uso: compáralos con los sobres y tubos')"
          @click="((firstCam = suggestCam || ''), (firstTube = suggestTube || ''))"
        >
          {{ $t('Usar los siguientes libres') }}
        </button>
        <button class="btn-primary" :disabled="!ready || busy" :title="$t('Rellena la tabla de nuevo con estas fechas, recolector e IDs (mientras nadie la haya editado)')" @click="prepare">
          <ListRestart :size="15" /> {{ proposalId ? $t('Rellenar la tabla de nuevo') : $t('Preparar la tabla') }}
        </button>
        <template v-if="earlier">
          <button class="btn" :title="$t('Vuelve al chat de este recorrido ({date})', { date: earlierDay })" @click="ask(false)">
            <Bot :size="15" /> {{ $t('Seguir el chat con Claude') }}
          </button>
          <button class="btn-ghost" :title="$t('Un chat nuevo con este recorrido')" @click="ask(true)"><MessageSquarePlus :size="15" /></button>
        </template>
        <button v-else class="btn" :disabled="!proposalId" :title="$t('Un chat con el recorrido y su tabla: dile los CAM, tubos o lo que pasó en el campo')" @click="ask(true)">
          <Bot :size="15" /> {{ $t('Preguntar a Claude') }}
        </button>
        <button class="btn-ghost" :title="$t('Cerrar')" @click="emit('close')"><X :size="15" /></button>
      </div>
    </div>
    <div class="flex flex-wrap items-center gap-x-3 gap-y-1 border-b border-stone-200 bg-stone-50 px-3 py-1 text-xs text-stone-600" data-walk-status>
      <span v-if="reading && reading.total" class="inline-flex items-center gap-1" data-identifier>
        <ScanSearch :size="13" />
        <template v-if="reading.unavailable || !reading.installed">{{ $t('El identificador de especies no está instalado en este servidor') }}</template>
        <template v-else-if="reading.read < reading.total">{{ $t('Identificador: {read} de {total} fotos leídas…', { read: reading.read, total: reading.total }) }}</template>
        <template v-else>{{ $t('Identificador: {total} fotos leídas; su especie está junto a la escrita', { total: reading.total }) }}</template>
      </span>
      <span v-if="inSheet.length">· {{ $t('{n} puntos ya están en la hoja (no se proponen)', { n: inSheet.length }) }}</span>
      <span v-if="!ready" class="text-amber-800">{{ $t('Elige la fecha y el recolector para preparar la tabla.') }}</span>
      <span v-for="(p, i) in problems" :key="i" class="text-amber-800">· {{ p }}</span>
    </div>
    <div class="flex min-h-0 flex-1">
      <div class="flex min-h-0 min-w-0 flex-1 flex-col">
        <ProposalsLive v-if="proposalId" :key="proposalId" layout="page" :only="proposalId" class="min-h-0 flex-1" />
        <p v-else class="p-6 text-sm text-stone-500">{{ busy ? $t('Preparando la tabla…') : $t('La tabla del recorrido aparece aquí.') }}</p>
      </div>
      <!-- Claude beside the table (T3's frame sits over its place). -->
      <div v-if="chatOpen" class="flex w-[38%] min-w-80 flex-col border-l border-stone-200" data-walk-chat>
        <div class="flex items-center justify-between border-b border-stone-200 px-2 py-1 text-xs text-stone-600">
          <span>{{ $t('Chat con Claude sobre este recorrido') }}</span>
          <button class="btn-ghost p-0.5" :title="$t('Cerrar el chat (sigue guardado)')" @click="chatOpen = false"><X :size="14" /></button>
        </div>
        <div v-if="prompt" class="space-y-1.5 border-b border-amber-200 bg-amber-50 px-3 py-2 text-xs text-amber-950">
          <p>{{ $t('No se pudo abrir el chat con el pedido. Pégalo en un chat nuevo:') }}</p>
          <p class="rounded border border-amber-200 bg-white px-2 py-1 select-all">{{ prompt }}</p>
          <button class="btn px-2 py-0.5" @click="copyPrompt">
            <Check v-if="copied" :size="13" /><Copy v-else :size="13" /> {{ copied ? $t('Copiado') : $t('Copiar') }}
          </button>
        </div>
        <div v-if="t3Host.url" ref="chatSlot" class="min-h-0 flex-1" />
        <p v-else class="p-4 text-sm text-stone-600">{{ $t('T3 Code no está configurado en este servidor.') }}</p>
      </div>
    </div>
  </div>
</template>
