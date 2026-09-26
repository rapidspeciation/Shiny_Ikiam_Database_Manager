<script setup lang="ts">
import { computed, onMounted, reactive, ref } from 'vue'
import { ChevronDown, ChevronRight, Search, Undo2, RefreshCw, X } from 'lucide-vue-next'
import { api, requestId } from '../lib/api'
import { displayValue } from '../lib/cells'
import { errorText, notify } from '../lib/notice'
import type { Action, Change } from '../lib/types'
import { useSession } from '../stores/session'
import { useTables } from '../stores/tables'

/** "Historial de Cambios" with selective undo, like the original app's history tab. */
const session = useSession()
const tables = useTables()

const SOURCES: Record<string, string> = {
  app: 'Aplicación',
  sheet_reconciliation: 'Google Sheets',
  undo: 'Deshacer',
  ai_approved: 'Asistente',
  import: 'Importación',
}
const STATUS: Record<string, string> = {
  verified: 'Guardado',
  observed: 'Detectado',
  pending: 'En curso',
  uncertain: 'Sin confirmar',
  failed: 'No guardado',
}

const filters = reactive({ q: '', actor: '', source: '', sheet: '', from: '', to: '' })
const actions = ref<Action[]>([])
const total = ref(0)
const loading = ref(false)
const open = reactive(new Set<string>())
const selected = reactive(new Set<string>())
const excluded = reactive(new Set<string>())
const preview = ref<null | { changes: PreviewItem[]; conflicts: PreviewItem[]; eligible: boolean }>(null)
const reason = ref('')
const busy = ref(false)

interface PreviewItem {
  recordId: string
  field: string
  before: Change['before']
  after: Change['after']
  reason?: string
}

async function load(append = false) {
  loading.value = true
  try {
    const params = new URLSearchParams({ limit: '50', offset: String(append ? actions.value.length : 0) })
    for (const [key, value] of Object.entries(filters)) if (value) params.set(key, value)
    const data = await api<{ actions: Action[]; total: number }>(`history?${params}`)
    actions.value = append ? [...actions.value, ...data.actions] : data.actions
    total.value = data.total
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    loading.value = false
  }
}
onMounted(() => load())

const fmtTime = (iso: string) =>
  new Intl.DateTimeFormat('es-EC', { dateStyle: 'medium', timeStyle: 'short', timeZone: 'America/Guayaquil' }).format(
    new Date(iso),
  )
const fieldOf = (sheet: string, key: string) => session.module(sheet)?.fields.find(f => f.key === key)
function show(value: Change['before'], sheet: string, field: string) {
  if (value && typeof value === 'object') return `fórmula ${value.formula}`
  return displayValue(value, fieldOf(sheet, field)) || 'vacío'
}
function summary(action: Action) {
  const labels = [...new Set(action.changes.map(c => c.label || `${c.sheet} ${c.row}`))]
  return labels.length > 4 ? `${labels.slice(0, 4).join(', ')} y ${labels.length - 4} más` : labels.join(', ')
}
const canUndo = (a: Action) => a.status === 'verified'
function toggle(action: Action) {
  if (selected.has(action.id)) selected.delete(action.id)
  else selected.add(action.id)
}
const selectedChangeIds = computed(() =>
  actions.value
    .filter(a => selected.has(a.id))
    .flatMap(a => a.changes.map(c => c.id))
    .filter(id => !excluded.has(id)),
)

function selection() {
  const actionIds = [...selected]
  return excluded.size ? { actionIds, changeIds: selectedChangeIds.value } : { actionIds }
}
function clearSelection() {
  selected.clear()
  excluded.clear()
}
async function review() {
  try {
    preview.value = await api('history/preview', { method: 'POST', body: selection() })
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
async function undo() {
  busy.value = true
  try {
    const result = await api<{ action: Action | null }>('history/undo', {
      method: 'POST',
      body: { ...selection(), requestId: requestId(), reason: reason.value || null },
    })
    const sheets = new Set(result.action?.changes.map(c => c.sheet) || [])
    await Promise.all([...sheets].filter(s => tables.tables[s]).map(s => tables.load(s, true)))
    notify('Cambios deshechos en Google Sheets', 'success')
    preview.value = null
    selected.clear()
    excluded.clear()
    reason.value = ''
    await load()
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}
async function recover() {
  try {
    const result = await api<{ recovered: number; failed: number }>('admin/recover', { method: 'POST', body: {} })
    notify(`${result.recovered} confirmadas, ${result.failed} marcadas como no guardadas`)
    await load()
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
</script>

<template>
  <div class="flex h-full flex-col">
    <form class="toolbar" @submit.prevent="load()">
      <label class="min-w-48 flex-1">
        <span class="field-label">Buscar (ID, campo, valor o nota)</span>
        <input v-model="filters.q" class="field-input" type="search" placeholder="p. ej. N4D, Death_date, FS0001" />
      </label>
      <label>
        <span class="field-label">Persona</span>
        <input v-model="filters.actor" class="field-input w-32" />
      </label>
      <label>
        <span class="field-label">Origen</span>
        <select v-model="filters.source" class="field-input">
          <option value="">Todos</option>
          <option v-for="(label, key) in SOURCES" :key="key" :value="key">{{ label }}</option>
        </select>
      </label>
      <label>
        <span class="field-label">Hoja</span>
        <select v-model="filters.sheet" class="field-input">
          <option value="">Todas</option>
          <option v-for="m in session.modules" :key="m.id" :value="m.id">{{ m.id }}</option>
        </select>
      </label>
      <label>
        <span class="field-label">Desde</span>
        <input v-model="filters.from" type="date" class="field-input" />
      </label>
      <label>
        <span class="field-label">Hasta</span>
        <input v-model="filters.to" type="date" class="field-input" />
      </label>
      <button class="btn-primary" :disabled="loading"><Search :size="15" /> Buscar</button>
      <button
        v-if="session.isAdmin"
        type="button"
        class="btn"
        title="Volver a comprobar escrituras sin confirmar"
        @click="recover"
      >
        <RefreshCw :size="15" />
      </button>
    </form>
    <div v-if="selected.size" class="flex flex-wrap items-center gap-2 border-b border-amber-300 bg-amber-50 px-4 py-2 text-sm">
      <span>{{ selected.size }} acciones seleccionadas · {{ selectedChangeIds.length }} cambios</span>
      <button class="btn-primary ml-auto" :disabled="!session.canEdit || !selectedChangeIds.length" @click="review">
        <Undo2 :size="15" /> Deshacer selección
      </button>
      <button class="btn" @click="clearSelection">Quitar selección</button>
    </div>
    <div class="min-h-0 flex-1 overflow-y-auto">
      <p v-if="!actions.length && !loading" class="p-6 text-stone-500">No hay cambios para estos filtros.</p>
      <table v-else class="w-full text-sm">
        <thead class="sticky top-0 bg-stone-100 text-left text-xs text-stone-600">
          <tr>
            <th class="w-8 px-2 py-2"></th>
            <th class="px-2 py-2">Fecha</th>
            <th class="px-2 py-2">Persona</th>
            <th class="px-2 py-2">Origen</th>
            <th class="px-2 py-2">Filas</th>
            <th class="hidden px-2 py-2 md:table-cell">Nota</th>
            <th class="px-2 py-2">Estado</th>
          </tr>
        </thead>
        <tbody>
          <template v-for="action in actions" :key="action.id">
            <tr
              class="cursor-pointer border-t border-stone-200 hover:bg-stone-50"
              @click="open.has(action.id) ? open.delete(action.id) : open.add(action.id)"
            >
              <td class="px-2 py-2" @click.stop>
                <input
                  type="checkbox"
                  :disabled="!canUndo(action)"
                  :checked="selected.has(action.id)"
                  :aria-label="`Seleccionar acción de ${fmtTime(action.createdAt)}`"
                  @change="toggle(action)"
                />
              </td>
              <td class="px-2 py-2 whitespace-nowrap">
                <component :is="open.has(action.id) ? ChevronDown : ChevronRight" :size="14" class="mr-1 inline" />
                {{ fmtTime(action.createdAt) }}
              </td>
              <td class="px-2 py-2">{{ action.actorName || (action.actor === 'unknown' ? 'desconocido' : action.actor) }}</td>
              <td class="px-2 py-2">{{ SOURCES[action.source] || action.source }}</td>
              <td class="px-2 py-2">
                {{ summary(action) }} <span class="text-stone-500">· {{ action.changes.length }} cambios</span>
              </td>
              <td class="hidden px-2 py-2 text-stone-600 md:table-cell">{{ action.reason }}</td>
              <td class="px-2 py-2">
                <span
                  class="rounded px-1.5 py-0.5 text-xs"
                  :class="{
                    'bg-brand-50 text-brand-800': action.status === 'verified',
                    'bg-sky-50 text-sky-800': action.status === 'observed',
                    'bg-amber-100 text-amber-900': ['pending', 'uncertain'].includes(action.status),
                    'bg-red-50 text-red-800': action.status === 'failed',
                  }"
                >
                  {{ STATUS[action.status] || action.status }}
                </span>
                <span v-if="action.reversedBy" class="ml-1 text-xs text-stone-500">deshecho</span>
              </td>
            </tr>
            <tr v-if="open.has(action.id)" class="bg-stone-50">
              <td></td>
              <td colspan="6" class="px-2 pb-3">
                <table class="w-full text-xs">
                  <tr v-for="c in action.changes" :key="c.id" class="border-t border-stone-200">
                    <td class="w-6 py-1">
                      <input
                        v-if="selected.has(action.id)"
                        type="checkbox"
                        :checked="!excluded.has(c.id)"
                        :aria-label="`Incluir ${c.field}`"
                        @change="excluded.has(c.id) ? excluded.delete(c.id) : excluded.add(c.id)"
                      />
                    </td>
                    <td class="py-1 pr-2 text-stone-500">{{ c.sheet }} fila {{ c.row }}</td>
                    <td class="py-1 pr-2 font-medium">{{ c.label }}</td>
                    <td class="py-1 pr-2">{{ c.field }}</td>
                    <td class="py-1 pr-2 text-stone-500 line-through decoration-stone-300">
                      {{ show(c.before, c.sheet, c.field) }}
                    </td>
                    <td class="py-1">{{ show(c.after, c.sheet, c.field) }}</td>
                  </tr>
                </table>
              </td>
            </tr>
          </template>
        </tbody>
      </table>
      <div v-if="actions.length < total" class="p-3 text-center">
        <button class="btn" :disabled="loading" @click="load(true)">Cargar más ({{ total - actions.length }})</button>
      </div>
    </div>

    <div v-if="preview" class="fixed inset-0 z-40 grid place-items-center bg-black/40 p-2" @click.self="preview = null">
      <section class="flex max-h-[90vh] w-full max-w-2xl flex-col rounded-lg bg-white shadow-xl">
        <header class="flex items-center border-b border-stone-200 px-4 py-3">
          <h2 class="flex-1 text-lg font-semibold">Deshacer {{ preview.changes.length }} cambios</h2>
          <button class="btn-ghost" @click="preview = null"><X :size="20" /></button>
        </header>
        <div class="flex-1 overflow-y-auto px-4 py-3 text-sm">
          <p v-if="preview.conflicts.length" class="mb-2 rounded bg-red-50 px-3 py-2 text-red-800">
            {{ preview.conflicts.length }} cambios no se pueden deshacer porque el valor cambió después ({{
              preview.conflicts.map(c => c.field).join(', ')
            }}). Quítalos de la selección o corrígelos a mano.
          </p>
          <table class="w-full">
            <tr v-for="c in preview.changes" :key="c.recordId + c.field" class="border-t border-stone-100">
              <td class="py-1 pr-2">{{ c.field }}</td>
              <td class="py-1 pr-2 text-stone-500 line-through decoration-stone-300">{{ show(c.before, '', c.field) }}</td>
              <td class="py-1 font-medium">{{ show(c.after, '', c.field) }}</td>
            </tr>
          </table>
        </div>
        <footer class="flex flex-wrap items-end gap-2 border-t border-stone-200 px-4 py-3">
          <label class="min-w-48 flex-1">
            <span class="field-label">Motivo (opcional)</span>
            <input v-model="reason" class="field-input" />
          </label>
          <button class="btn-primary" :disabled="busy || !preview.eligible" @click="undo">
            <Undo2 :size="15" /> {{ busy ? 'Deshaciendo…' : 'Deshacer en la hoja' }}
          </button>
        </footer>
      </section>
    </div>
  </div>
</template>
