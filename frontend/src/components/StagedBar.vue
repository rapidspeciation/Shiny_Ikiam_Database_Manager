<script setup lang="ts">
import { computed, onBeforeUnmount, ref } from 'vue'
import { AlertTriangle, ChevronDown, ChevronUp, CloudUpload, Loader2, Undo2, X } from 'lucide-vue-next'
import { api, ApiError, requestId } from '../lib/api'
import { formatSerial } from '../lib/dates'
import { errorText, notify } from '../lib/notice'
import { changeCount, stagedSummary, type StagedItem } from '../lib/staged'
import { t, tn } from '../lib/i18n'
import { useLive } from '../stores/live'
import { usePending } from '../stores/pending'
import { useSession } from '../stores/session'
import { useTables } from '../stores/tables'

/**
 * Emergidos and Clutches keep their saves in the app, for everyone, until
 * someone presses «Guardar en Google Sheets» (server/staged.mjs): every entry of
 * both tabs goes as one save (one recalculation of the workbook), and waits in
 * the app if Google is busy. This bar says how many wait and whose, lists them
 * (each can be undone), and saves them.
 */
const live = useLive()
const pending = usePending()
const session = useSession()
const tables = useTables()
const open = ref(false)
const confirming = ref(false)
const saving = ref(false)
/** Kept until the server answers for sure: a retry never writes twice. */
let flushId: string | null = null

const count = computed(() => live.stagedCount)
const sending = computed(() => live.sendingCount)
const people = computed(() => [...new Set(live.items.filter(i => i.status === 'staged').map(i => i.actorName))])
const refused = computed(() => live.items.filter(i => i.status === 'staged' && i.error).length)
const summary = computed(() => stagedSummary(live.items))
/** This device's changes of these tabs not kept in the app yet (typed a moment ago): they go first. */
const unsent = computed(
  () =>
    Object.values(pending.edits).filter(e => e.purpose === 'emergidos' || e.purpose === 'clutches' || e.id.startsWith('staged:')).length +
    pending.creates.filter(c => c.purpose === 'emergidos' || c.purpose === 'clutches').length,
)
const DATE = /date|_date$/i
const shown = (field: string, value: string) => (DATE.test(field) && /^\d{5}(\.\d+)?$/.test(value) ? formatSerial(Number(value)) : value)

async function undo(items: StagedItem[]) {
  // What was saved together (Emergidos: the butterflies and their clutch's count) is undone together.
  const ids = new Set(items.map(i => i.id))
  const entries = [...new Set(items.map(i => i.entryId))]
  const together = live.items.filter(i => entries.includes(i.entryId) && !ids.has(i.id))
  if (together.length) {
    const labels = [...new Set(together.map(i => i.label || i.sheet))].join(', ')
    if (!confirm(t('Se guardó junto con {labels}: se deshace todo junto. ¿Seguir?', { labels }))) return
  }
  try {
    if (together.length)
      for (const entry of entries) await api(`staged/entries/${encodeURIComponent(entry)}`, { method: 'DELETE', body: {} })
    else
      for (const item of items)
        await api(`staged/items/${encodeURIComponent(item.id)}`, { method: 'DELETE', body: {} })
    await live.refresh()
    await live.loadStaged()
    notify(tn(items.length, 'Cambio deshecho (no llegó a Google Sheets)', 'Cambios deshechos (no llegaron a Google Sheets)'))
  } catch (e) {
    notify(errorText(e), 'error')
  }
}

async function flush() {
  if (saving.value) return
  saving.value = true
  try {
    // What was typed a moment ago in these tabs is kept in the app first.
    if (unsent.value && pending.changeCount) await pending.save('').catch(() => {})
    flushId ??= requestId()
    const out = await api<{ status: string; outbox?: { status: string; error?: { message: string } }; result?: { records?: never[] } }>(
      'staged/flush',
      { method: 'POST', body: { requestId: flushId } },
    )
    flushId = null
    confirming.value = false
    await live.refresh()
    await live.loadStaged()
    if (out.status === 'empty') notify(t('No hay cambios por guardar'))
    else if (out.status === 'queued' || out.status === 'writing')
      notify(t('Google Sheets no responde: los cambios esperan en la app y se escribirán solos cuando responda'))
    else if (out.status === 'done') {
      const left = live.items.filter(i => i.status === 'staged' && i.error).length
      if (left) notify(tn(left, 'Guardado en Google Sheets; {n} fila necesita revisión (marcada en rojo)', 'Guardado en Google Sheets; {n} filas necesitan revisión (marcadas en rojo)'), 'error')
      else notify(t('Guardado en Google Sheets'), 'success')
      // The rows written: the tables follow at once.
      await Promise.all(['Insectary_data', 'Insectary_stocks'].filter(m => tables.tables[m]).map(m => tables.refresh(m).catch(() => {})))
    } else notify(out.outbox?.error?.message ? t(out.outbox.error.message) : t('No se guardó'), 'error')
  } catch (e) {
    if (!(e instanceof ApiError) || !['OFFLINE', 'SERVER_ERROR'].includes(e.code)) flushId = null
    notify(errorText(e), 'error')
  } finally {
    saving.value = false
  }
}
const onKey = (e: KeyboardEvent) => {
  if (e.key === 'Escape' && confirming.value && !saving.value) confirming.value = false
}
window.addEventListener('keydown', onKey)
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <div v-if="count || sending" class="border-b border-amber-300 bg-amber-50 text-sm text-amber-950">
    <div class="flex flex-wrap items-center gap-2 px-3 py-1.5 sm:px-4">
      <template v-if="count">
        <span class="font-medium">
          {{
            $tn(count, '{n} fila cambiada en la app, aún no en Google Sheets', '{n} filas cambiadas en la app, aún no en Google Sheets')
          }}
        </span>
        <span class="text-xs text-amber-900">{{ people.join(', ') }}</span>
        <span v-if="refused" class="flex items-center gap-1 text-xs font-medium text-red-800">
          <AlertTriangle :size="13" /> {{ $tn(refused, '{n} por revisar', '{n} por revisar') }}
        </span>
      </template>
      <span v-if="sending" class="flex items-center gap-1 text-xs">
        <Loader2 :size="13" class="animate-spin" />
        {{
          live.busy
            ? $tn(sending, '{n} esperando a que Google Sheets responda', '{n} esperando a que Google Sheets responda')
            : $tn(sending, 'Escribiendo {n} fila en Google Sheets…', 'Escribiendo {n} filas en Google Sheets…')
        }}
      </span>
      <div class="ml-auto flex items-center gap-2">
        <button v-if="count" class="btn h-9" :aria-expanded="open" @click="open = !open">
          <component :is="open ? ChevronUp : ChevronDown" :size="15" /> {{ open ? $t('Ocultar') : $t('Ver') }}
        </button>
        <button v-if="count && session.canEdit" class="btn-primary h-9" :disabled="saving" @click="confirming = true">
          <CloudUpload :size="15" /> {{ $tn(count, 'Guardar en Google Sheets ({n} fila)', 'Guardar en Google Sheets ({n} filas)') }}
        </button>
      </div>
    </div>
    <ul v-if="open && count" class="max-h-64 overflow-y-auto border-t border-amber-200 bg-white px-3 py-2 sm:px-4">
      <li v-for="group in summary" :key="group.sheet" class="mb-2">
        <p class="text-xs font-semibold text-stone-500">{{ group.sheet }}</p>
        <div v-for="row in group.rows" :key="row.items[0].rowId" class="flex flex-wrap items-baseline gap-x-2 border-b border-stone-100 py-1">
          <strong>{{ row.label || '—' }}</strong>
          <span v-if="row.isNew" class="rounded bg-amber-100 px-1 text-xs">{{ $t('nueva') }}</span>
          <span class="text-xs text-stone-700">
            {{ row.cells.map(c => `${c.field}: ${shown(c.field, c.value)}`).join(' · ') }}
          </span>
          <span class="text-xs text-stone-500">{{ row.who.join(', ') }}</span>
          <span v-if="row.error" class="w-full text-xs text-red-800"><AlertTriangle :size="12" class="inline" /> {{ $t(row.error) }}</span>
          <button v-if="session.canEdit" class="ml-auto text-xs text-stone-600 hover:underline" @click="undo(row.items)">
            <Undo2 :size="12" class="inline" /> {{ $t('Deshacer') }}
          </button>
        </div>
      </li>
    </ul>
    <!-- What will be written, before writing it. -->
    <div v-if="confirming" class="fixed inset-0 z-40 grid place-items-center bg-black/40 p-2" @click.self="!saving && (confirming = false)">
      <section class="flex max-h-[92vh] w-full max-w-2xl flex-col rounded-lg bg-white text-stone-900 shadow-xl">
        <header class="flex items-center border-b border-stone-200 px-4 py-3">
          <h2 class="flex-1 text-lg font-semibold">{{ $t('Guardar en Google Sheets') }}</h2>
          <button class="btn-ghost" :disabled="saving" @click="confirming = false"><X :size="20" /></button>
        </header>
        <div class="flex-1 overflow-y-auto px-4 py-3">
          <p class="mb-2 text-sm text-stone-600">
            {{
              $tn(
                changeCount(live.items),
                'Se escribe {n} fila de Emergidos, Clutches y Censo, de todo el equipo, en una sola vez. Si Google Sheets está ocupado, espera en la app y se escribe sola.',
                'Se escriben {n} filas de Emergidos, Clutches y Censo, de todo el equipo, en una sola vez. Si Google Sheets está ocupado, esperan en la app y se escriben solas.',
              )
            }}
          </p>
          <div v-for="group in summary" :key="group.sheet" class="mb-3">
            <p class="text-sm font-semibold">{{ group.sheet }} · {{ $tn(group.rows.length, '{n} fila', '{n} filas') }}</p>
            <p v-for="row in group.rows" :key="row.items[0].rowId" class="text-sm">
              <strong>{{ row.label || '—' }}</strong>
              <span v-if="row.isNew" class="text-xs text-amber-800"> ({{ $t('nueva') }})</span>:
              {{ row.cells.map(c => `${c.field} ${shown(c.field, c.value)}`).join(', ') }}
              <span class="text-xs text-stone-500">· {{ row.who.join(', ') }}</span>
            </p>
          </div>
        </div>
        <footer class="flex justify-end gap-2 border-t border-stone-200 px-4 py-3">
          <button class="btn" :disabled="saving" @click="confirming = false">{{ $t('Cancelar') }}</button>
          <button class="btn-primary" :disabled="saving" @click="flush">
            <Loader2 v-if="saving" :size="15" class="animate-spin" />
            <CloudUpload v-else :size="15" />
            {{ saving ? $t('Guardando…') : $t('Guardar en Google Sheets') }}
          </button>
        </footer>
      </section>
    </div>
  </div>
</template>
