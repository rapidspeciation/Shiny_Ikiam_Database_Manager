<script setup lang="ts">
import ChoiceField from '../components/ChoiceField.vue'
import DateField from '../components/DateField.vue'
import HistoryCard from '../components/history/HistoryCard.vue'
import UndoDialog from '../components/history/UndoDialog.vue'
import { computed, nextTick, onBeforeUnmount, reactive, ref, watch } from 'vue'
import { useRoute } from 'vue-router'
import { ListChecks, RefreshCw, Search, SlidersHorizontal, X } from 'lucide-vue-next'
import { api } from '../lib/api'
import { useUndo, type UndoBody } from '../composables/useUndo'
import { PURPOSES, linkedSave } from '../lib/history'
import { errorText, notify } from '../lib/notice'
import type { HistoryGroup } from '../lib/types'
import { useSession } from '../stores/session'
import { t } from '../lib/i18n'

/**
 * Historial: one card per save (a person's saves with one purpose, close in
 * time), newest first. A card opens to show every change by row; a whole save,
 * one of its parts, a row or single cells can be undone after a preview.
 * #/historial?grupo=<id> (or ?accion=<id>) opens that save and scrolls to it.
 */
const session = useSession()
const route = useRoute()

const PAGE = 20
const blank = { text: '', user: '', purpose: '', sheet: '', from: '', to: '' }
const filters = reactive({ ...blank })
const filtered = computed(() => Object.values(filters).some(Boolean))
const showFilters = ref(false)
const purposeChoices = computed(() => Object.entries(PURPOSES).map(([value, p]) => ({ value, label: t(p.label) })))
const sheetChoices = computed(() => session.modules.map(m => ({ value: m.id, label: m.id })))

const groups = ref<HistoryGroup[]>([])
const next = ref<number | null>(null)
const loading = ref(false)
/** A linked save too far down the list to load: shown on top. */
const pinned = ref<HistoryGroup | null>(null)
const details = reactive(new Map<string, HistoryGroup>())
const open = reactive(new Set<string>())
const highlighted = ref('')

function params(extra: Record<string, string | number> = {}) {
  const p = new URLSearchParams()
  for (const [key, value] of Object.entries({ ...filters, ...extra }))
    if (value !== '' && value !== undefined) p.set(key, String(value))
  return p
}
/** Loads the first page again (down to `until`, a linked save), or the next page. */
async function load({ more = false, until = '' } = {}) {
  loading.value = true
  try {
    const offset = more ? (next.value ?? groups.value.length) : 0
    const data = await api<{ groups: HistoryGroup[]; next: number | null }>(
      `history/groups?${params({ limit: PAGE, offset, ...(until ? { until } : {}) })}`,
    )
    if (more) {
      const known = new Set(groups.value.map(g => g.id))
      groups.value = [...groups.value, ...data.groups.filter(g => !known.has(g.id))]
    } else groups.value = data.groups
    next.value = data.next
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    loading.value = false
  }
}

async function loadDetail(id: string, force = false) {
  if (details.has(id) && !force) return details.get(id)!
  const { group } = await api<{ group: HistoryGroup }>(`history/groups/${encodeURIComponent(id)}`)
  details.set(group.id, group)
  return group
}
async function toggle(group: HistoryGroup) {
  if (open.has(group.id)) return open.delete(group.id)
  open.add(group.id)
  try {
    await loadDetail(group.id)
  } catch (e) {
    open.delete(group.id)
    notify(errorText(e), 'error')
  }
}

// Filters apply a moment after the last change.
let timer: ReturnType<typeof setTimeout> | null = null
watch(filters, () => {
  if (timer) clearTimeout(timer)
  timer = setTimeout(() => {
    pinned.value = null
    load()
  }, 350)
})
onBeforeUnmount(() => timer && clearTimeout(timer))
function clearFilters() {
  Object.assign(filters, blank)
}

/** A link to a save: all saves shown, down to that one, which opens, scrolls into view and stays highlighted. */
async function openLinked(id: string) {
  let group: HistoryGroup
  try {
    group = await loadDetail(id, true)
  } catch (e) {
    notify(errorText(e), 'error')
    return load()
  }
  if (timer) clearTimeout(timer)
  Object.assign(filters, blank)
  await nextTick()
  if (timer) clearTimeout(timer)
  await load({ until: group.id })
  pinned.value = groups.value.some(g => g.id === group.id) ? null : group
  open.add(group.id)
  highlighted.value = group.id
  await nextTick()
  document.getElementById(`grupo-${group.id}`)?.scrollIntoView({ block: 'start', behavior: 'smooth' })
}
watch(
  () => linkedSave(route.query),
  id => (id ? openLinked(id) : load()),
  { immediate: true },
)

// --- Undo: always a preview first, then one confirmation (useUndo).
const { undoing, reason, busy, review: reviewUndo, cancel: cancelUndo, confirm: confirmUndo } = useUndo(async u => {
  // The card stays in view with its changes marked as undone; the undo is a new card on top.
  details.clear()
  await load({ until: u.groupId })
  if (u.groupId && pinned.value?.id === u.groupId) pinned.value = await loadDetail(u.groupId, true)
  await Promise.all([...open].map(id => loadDetail(id).catch(() => open.delete(id))))
})
const review = (group: HistoryGroup, body: UndoBody, title: string) => reviewUndo(body, title, group.id)

async function recover() {
  try {
    const result = await api<{ recovered: number; failed: number }>('admin/recover', { method: 'POST', body: {} })
    notify(t('{recovered} confirmadas, {failed} marcadas como no guardadas', result))
    await load()
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
</script>

<template>
  <div class="flex h-full flex-col">
    <form class="toolbar gap-2 py-2 sm:gap-3" @submit.prevent="load()">
      <label class="min-w-40 flex-1">
        <span class="field-label">{{ $t('Buscar (ID, campo, valor o nota)') }}</span>
        <span class="relative block">
          <Search :size="14" class="absolute top-2.5 left-2 text-stone-400" />
          <input
            v-model="filters.text"
            class="field-input pl-7"
            type="search"
            :placeholder="$t('p. ej. A0D, CAM079891, Death_date')"
          />
        </span>
      </label>
      <button
        type="button"
        class="btn sm:hidden"
        :class="{ 'bg-stone-800 text-white': filtered }"
        @click="showFilters = !showFilters"
      >
        <SlidersHorizontal :size="15" /> {{ $t('Filtros') }}
      </button>
      <div
        class="w-full flex-wrap items-end gap-2 sm:flex sm:w-auto sm:gap-3"
        :class="showFilters ? 'grid grid-cols-2' : 'hidden'"
      >
        <label>
          <span class="field-label">{{ $t('Persona') }}</span>
          <input v-model="filters.user" class="field-input sm:w-32" :placeholder="$t('Todas')" />
        </label>
        <label>
          <span class="field-label">{{ $t('Para qué') }}</span>
          <ChoiceField
            v-model="filters.purpose"
            class="field-input sm:w-40"
            :options="purposeChoices"
            :freetext="false"
            allow-empty
            :placeholder="$t('Todo')"
          />
        </label>
        <label>
          <span class="field-label">{{ $t('Hoja') }}</span>
          <ChoiceField
            v-model="filters.sheet"
            class="field-input sm:w-44"
            :options="sheetChoices"
            :freetext="false"
            allow-empty
            :placeholder="$t('Todas')"
          />
        </label>
        <label>
          <span class="field-label">{{ $t('Desde') }}</span>
          <DateField v-model="filters.from" class="field-input sm:w-40" />
        </label>
        <label>
          <span class="field-label">{{ $t('Hasta') }}</span>
          <DateField v-model="filters.to" class="field-input sm:w-40" />
        </label>
      </div>
      <button v-if="filtered" type="button" class="btn" :title="$t('Quitar los filtros')" @click="clearFilters">
        <X :size="15" /> {{ $t('Quitar') }}
      </button>
      <button class="btn-ghost" type="submit" :disabled="loading" :title="$t('Actualizar')">
        <RefreshCw :size="15" :class="{ 'animate-spin': loading }" />
      </button>
      <button
        v-if="session.isAdmin"
        type="button"
        class="btn-ghost"
        :title="$t('Volver a comprobar escrituras sin confirmar')"
        @click="recover"
      >
        <ListChecks :size="15" />
      </button>
    </form>

    <div class="min-h-0 flex-1 overflow-y-auto bg-stone-50">
      <div class="mx-auto max-w-4xl space-y-2 p-2 sm:p-3">
        <template v-if="pinned">
          <p class="hint px-1">{{ $t('Guardado enlazado (más antiguo que la lista):') }}</p>
          <HistoryCard
            :group="pinned"
            :detail="details.get(pinned.id) ?? null"
            :open="open.has(pinned.id)"
            :highlighted="highlighted === pinned.id"
            :can-edit="session.canEdit"
            @toggle="toggle(pinned)"
            @undo="(body, title) => review(pinned!, body, title)"
          />
          <p class="hint px-1 pt-2">{{ $t('Últimos guardados:') }}</p>
        </template>
        <p v-if="!groups.length && !loading" class="p-6 text-sm text-stone-500">
          {{ filtered ? $t('No hay guardados con estos filtros.') : $t('Todavía no hay guardados.') }}
        </p>
        <p v-if="!groups.length && loading" class="p-6 text-sm text-stone-500">{{ $t('Cargando…') }}</p>
        <HistoryCard
          v-for="group in groups"
          :key="group.id"
          :group="group"
          :detail="details.get(group.id) ?? null"
          :open="open.has(group.id)"
          :highlighted="highlighted === group.id"
          :can-edit="session.canEdit"
          :search="filters.text"
          @toggle="toggle(group)"
          @undo="(body, title) => review(group, body, title)"
        />
        <div v-if="next !== null" class="p-3 text-center">
          <button class="btn" :disabled="loading" @click="load({ more: true })">{{ $t('Cargar más') }}</button>
        </div>
      </div>
    </div>

    <UndoDialog v-if="undoing" v-model:reason="reason" :review="undoing" :busy="busy" @cancel="cancelUndo" @confirm="confirmUndo" />
  </div>
</template>
