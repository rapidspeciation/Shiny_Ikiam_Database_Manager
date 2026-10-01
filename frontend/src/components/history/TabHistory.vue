<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, reactive, ref, watch } from 'vue'
import { ChevronLeft, ChevronRight, History, RefreshCw, X } from 'lucide-vue-next'
import DateField from '../DateField.vue'
import HistoryCard from './HistoryCard.vue'
import UndoDialog from './UndoDialog.vue'
import { useUndo, type UndoBody } from '../../composables/useUndo'
import { api } from '../../lib/api'
import { dayLabel, isoToSerial, serialToIso, todayIso } from '../../lib/dates'
import { errorText, notify } from '../../lib/notice'
import { persistentRef } from '../../lib/persist'
import type { HistoryGroup } from '../../lib/types'
import { useSession } from '../../stores/session'

/**
 * A data-entry tab's own history (Muertes, Tubos, Emergidos, Colecta…), to see
 * and undo an accident without leaving the tab: that tab's saves of one day
 * (today by default), mine or everyone's, one card per save with each cell
 * before → after, open. Undoing goes through the Historial's preview and
 * confirmation (useUndo): its conflict checks refuse a cell changed again
 * since. `purpose`: the tab's saves (lib/history PURPOSES); `sheet`: or every
 * change to a sheet. Full screen on a phone, a panel on the right on a wider screen.
 */
const props = defineProps<{ title: string; purpose?: string; sheet?: string }>()
const emit = defineEmits<{ close: [] }>()
const session = useSession()

const PAGE = 20
const day = ref(todayIso())
const mine = persistentRef(`history:${props.purpose || props.sheet}:mine`, true)
const groups = ref<HistoryGroup[]>([])
const details = reactive(new Map<string, HistoryGroup>())
const open = reactive(new Set<string>())
const next = ref<number | null>(null)
const loading = ref(false)
const isToday = computed(() => day.value === todayIso())

function query(offset: number) {
  const p = new URLSearchParams({ limit: String(PAGE), offset: String(offset), from: day.value, to: day.value })
  if (props.purpose) p.set('purpose', props.purpose)
  if (props.sheet) p.set('sheet', props.sheet)
  if (mine.value && session.user?.id) p.set('user', session.user.id)
  return p
}
/** One save's changes, cell by cell (asked once; again after an undo). */
async function detail(id: string, force = false) {
  if (details.has(id) && !force) return
  const { group } = await api<{ group: HistoryGroup }>(`history/groups/${encodeURIComponent(id)}`)
  details.set(id, group)
}
let asked = 0
async function load({ more = false } = {}) {
  const ask = ++asked
  loading.value = true
  try {
    const data = await api<{ groups: HistoryGroup[]; next: number | null }>(`history/groups?${query(more ? (next.value ?? 0) : 0)}`)
    if (ask !== asked) return
    if (!more) {
      details.clear()
      open.clear()
    }
    groups.value = more ? [...groups.value, ...data.groups.filter(g => !groups.value.some(o => o.id === g.id))] : data.groups
    next.value = data.next
    // Open, to see each cell at once (a day of one tab is a few saves).
    for (const g of data.groups) open.add(g.id)
    await Promise.all(data.groups.map(g => detail(g.id).catch(() => open.delete(g.id))))
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    if (ask === asked) loading.value = false
  }
}
async function toggle(group: HistoryGroup) {
  if (open.has(group.id)) return open.delete(group.id)
  open.add(group.id)
  try {
    await detail(group.id)
  } catch (e) {
    open.delete(group.id)
    notify(errorText(e), 'error')
  }
}
watch([day, mine], () => day.value && load())
onMounted(() => load())

const { undoing, reason, busy, review, cancel, confirm } = useUndo(() => load())
const undo = (group: HistoryGroup, body: UndoBody, title: string) => review(body, title, group.id)

const step = (n: number) => (day.value = serialToIso(isoToSerial(day.value || todayIso()) + n))
const onKey = (e: KeyboardEvent) => e.key === 'Escape' && !undoing.value && emit('close')
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <div class="fixed inset-0 z-50 flex justify-end bg-black/30" @click.self="emit('close')">
    <section class="flex h-full w-full flex-col bg-stone-50 shadow-xl sm:max-w-xl" role="dialog" :aria-label="title">
      <header class="flex items-center gap-2 border-b border-stone-200 bg-white px-3 py-1.5">
        <History :size="20" class="shrink-0 text-stone-600" />
        <h2 class="min-w-0 flex-1 truncate text-lg font-semibold">{{ title }}</h2>
        <button class="btn-ghost h-11 w-11 justify-center" :aria-label="$t('Actualizar')" :disabled="loading" @click="load()">
          <RefreshCw :size="18" :class="{ 'animate-spin': loading }" />
        </button>
        <button class="btn-ghost h-11 w-11 justify-center" :aria-label="$t('Cerrar')" @click="emit('close')"><X :size="22" /></button>
      </header>
      <div class="space-y-2 border-b border-stone-200 bg-white px-3 py-2">
        <div class="flex flex-wrap items-center gap-2">
          <div class="inline-flex overflow-hidden rounded-lg border border-stone-300 text-sm" role="group">
            <button class="h-11 px-3" :class="mine ? 'bg-brand-700 text-white' : 'bg-white'" :aria-pressed="mine" @click="mine = true">
              {{ $t('Míos') }}
            </button>
            <button class="h-11 border-l border-stone-300 px-3" :class="!mine ? 'bg-brand-700 text-white' : 'bg-white'" :aria-pressed="!mine" @click="mine = false">
              {{ $t('De todos') }}
            </button>
          </div>
          <div class="flex min-w-0 flex-1 items-center gap-1">
            <button class="btn h-11 w-11 shrink-0 justify-center px-0" :aria-label="$t('Día anterior')" @click="step(-1)"><ChevronLeft :size="18" /></button>
            <DateField v-model="day" class="field-input h-11 min-w-0 text-base" />
            <button class="btn h-11 w-11 shrink-0 justify-center px-0" :aria-label="$t('Día siguiente')" :disabled="isToday" @click="step(1)">
              <ChevronRight :size="18" />
            </button>
          </div>
        </div>
        <p class="text-xs text-stone-600">
          {{ day ? dayLabel(day) : '' }} ·
          {{ $t('cada guardado con sus celdas antes → después; deshacer pide confirmación y no escribe nada si la celda cambió después.') }}
        </p>
      </div>
      <div class="min-h-0 flex-1 overflow-y-auto">
        <div class="space-y-2 p-2 sm:p-3">
          <p v-if="!groups.length && loading" class="p-6 text-center text-sm text-stone-500">{{ $t('Cargando…') }}</p>
          <p v-else-if="!groups.length" class="p-6 text-center text-sm text-stone-500">
            {{ mine ? $t('Ningún guardado tuyo este día.') : $t('Ningún guardado este día.') }}
          </p>
          <HistoryCard
            v-for="group in groups"
            :key="group.id"
            :group="group"
            :detail="details.get(group.id) ?? null"
            :open="open.has(group.id)"
            :highlighted="false"
            :can-edit="session.canEdit"
            touch
            @toggle="toggle(group)"
            @undo="(body, title) => undo(group, body, title)"
          />
          <div v-if="next !== null" class="p-3 text-center">
            <button class="btn h-11" :disabled="loading" @click="load({ more: true })">{{ $t('Cargar más') }}</button>
          </div>
        </div>
      </div>
    </section>
    <UndoDialog v-if="undoing" v-model:reason="reason" :review="undoing" :busy="busy" @cancel="cancel" @confirm="confirm" />
  </div>
</template>
