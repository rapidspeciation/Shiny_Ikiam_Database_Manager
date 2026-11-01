<script setup lang="ts">
import { computed, onBeforeUnmount, ref } from 'vue'
import { Save, Eye, Undo2, AlertTriangle, CheckCircle2, Clock, Loader2, PencilLine } from 'lucide-vue-next'
import ReviewDialog from './ReviewDialog.vue'
import { usePending } from '../stores/pending'
import { errorText, notify } from '../lib/notice'
import { t, tn } from '../lib/i18n'
import { pendingPlaces } from '../lib/pendingPlaces'

/**
 * Pending changes and how they reach Google Sheets. With automatic saving (the
 * default) changes are written a moment after the last edit; otherwise this is
 * the "Subir cambios" step of the original app.
 */
const pending = usePending()
const reviewing = ref(false)
/** Pending cells that are not being saved, and why (the save bar shows the first reason). */
const issues = computed(() => Object.values(pending.issues))
const errorCount = computed(() => issues.value.length)
/** Which rows are unsaved, named in the bar (sheet · row · columns), all of them on hover. */
const placeText = (p: { module: string; label: string; row: number | null; fields: string[] }) =>
  [p.module, p.label, p.row === null ? t('fila nueva') : t('fila {n}', { n: p.row })].filter(Boolean).join(' · ') +
  (p.fields.length ? `: ${p.fields.join(', ')}` : '')
const places = computed(() => {
  const { shown, more } = pendingPlaces(Object.values(pending.edits), pending.creates)
  return shown.map(placeText).join('; ') + (more ? ` ${t('y {n} más', { n: more })}` : '')
})
const allPlaces = computed(() =>
  pendingPlaces(Object.values(pending.edits), pending.creates, Infinity).shown.map(placeText).join('\n'),
)
/** A walk's captures wait for Guardar (automatic saving leaves them). */
const waitingRows = computed(() => pending.creates.filter(c => c.manual).length)

async function save(reason = '') {
  try {
    const { saved, staged, queued } = await pending.save(reason)
    const left = errorCount.value
    if (!left) reviewing.value = false
    const why = issues.value[0]
    if (queued)
      notify(
        tn(queued, 'Google Sheets no responde: {n} cambio espera y se escribirá solo', 'Google Sheets no responde: {n} cambios esperan y se escribirán solos'),
      )
    if (staged && !saved && !left)
      notify(
        tn(staged, '{n} cambio guardado en la app; falta «Guardar en Google Sheets»', '{n} cambios guardados en la app; falta «Guardar en Google Sheets»'),
        'success',
      )
    else if (saved && !left)
      notify(tn(saved, '{n} cambio guardado en Google Sheets', '{n} cambios guardados en Google Sheets'), 'success')
    else if (saved)
      notify(
        tn(left, '{saved} guardados; {n} celda sin guardar: {reason}', '{saved} guardados; {n} celdas sin guardar: {reason}', {
          saved,
          reason: why,
        }),
        'error',
      )
    else if (left) {
      notify(tn(left, '{n} celda sin guardar: {reason}', '{n} celdas sin guardar: {reason}', { reason: why }), 'error')
      reviewing.value = true
    }
  } catch (e) {
    notify(errorText(e), 'error')
    reviewing.value = true
  }
}
const online = ref(navigator.onLine)
const onOffline = () => (online.value = false)
const onOnline = () => {
  online.value = true
  if (!pending.changeCount) return
  if (pending.autoSave) pending.runAutoSave()
  else notify(t('Conexión recuperada: ya puedes guardar los cambios pendientes'))
}
window.addEventListener('offline', onOffline)
window.addEventListener('online', onOnline)
// Escape closes the review, as dialogs do (not while a save is running).
const onKey = (e: KeyboardEvent) => {
  if (e.key === 'Escape' && reviewing.value && !pending.saving) reviewing.value = false
}
window.addEventListener('keydown', onKey)

// "Guardado" stays visible for a few seconds after an automatic save.
const now = ref(Date.now())
const tick = setInterval(() => (now.value = Date.now()), 1000)
onBeforeUnmount(() => {
  clearInterval(tick)
  window.removeEventListener('offline', onOffline)
  window.removeEventListener('online', onOnline)
  window.removeEventListener('keydown', onKey)
})
const justSaved = computed(() => !!pending.lastSaved && now.value - Date.parse(pending.lastSaved.at) < 4000)

function discard() {
  if (confirm(tn(pending.changeCount, '¿Descartar {n} cambio sin guardar?', '¿Descartar {n} cambios sin guardar?')))
    pending.discard()
}
</script>

<template>
  <!-- In the colour of the unsaved cells, with a strong edge: unlike the workbook's warnings above it (grey). -->
  <div
    v-if="pending.changeCount"
    class="flex flex-wrap items-center gap-x-2 gap-y-1 px-3 py-2 text-sm sm:px-4"
    :class="pending.autoSave && !errorCount ? 'border-t border-stone-200 bg-surface' : 'border-t-2 border-pending-edge bg-cell-pending'"
    data-save-bar
  >
    <span v-if="pending.autoSave && pending.saving" class="flex items-center gap-1.5 font-medium text-stone-700">
      <Loader2 :size="15" class="animate-spin" />
      {{ $tn(pending.changeCount, 'Guardando {n} cambio en Google Sheets…', 'Guardando {n} cambios en Google Sheets…') }}
    </span>
    <span v-else class="flex items-center gap-1.5 font-medium text-pending-text">
      <PencilLine :size="15" class="shrink-0" aria-hidden="true" />
      {{
        pending.autoSave && !pending.autoBlocked && errorCount < pending.changeCount
          ? $tn(pending.changeCount, '{n} cambio en {rows} por guardar', '{n} cambios en {rows} por guardar', {
              rows: $tn(pending.rowCount, '{n} fila', '{n} filas'),
            })
          : $tn(pending.changeCount, '{n} cambio en {rows} sin guardar', '{n} cambios en {rows} sin guardar', {
              rows: $tn(pending.rowCount, '{n} fila', '{n} filas'),
            })
      }}
    </span>
    <!-- The first places; all of them on hover, and a tap opens Revisar with the full list (a phone has no hover). -->
    <button
      v-if="!pending.saving"
      type="button"
      class="hit-target min-w-0 max-w-full truncate text-left text-xs text-pending-text hover:underline"
      :title="allPlaces"
      :aria-label="$t('Revisar: {places}', { places: allPlaces })"
      data-pending-places
      @click="reviewing = true"
    >
      {{ places }}
    </button>
    <button
      v-if="errorCount"
      type="button"
      class="flex min-w-0 max-w-full items-center gap-1 text-left font-medium text-red-800 hover:underline"
      :title="issues.join('\n')"
      @click="reviewing = true"
    >
      <AlertTriangle :size="15" class="shrink-0" /> {{ $tn(errorCount, '{n} celda sin guardar:', '{n} celdas sin guardar:') }}
      <span class="truncate font-normal">{{ issues[0] }}</span>
    </button>
    <span v-if="!online" class="rounded bg-inverse px-2 py-0.5 text-xs text-on-inverse">{{ $t('Sin conexión') }}</span>
    <!-- Sent, and kept by the server until Google answers (server/outbox.mjs): written on their own. -->
    <span
      v-if="pending.queuedCount"
      class="flex items-center gap-1 text-xs font-medium text-amber-900"
      :title="$t('Google Sheets no responde (recalcula la hoja); se escriben solos, en orden, cuando responda')"
    >
      <Clock :size="13" />
      {{ $tn(pending.queuedCount, '{n} esperando a Google Sheets', '{n} esperando a Google Sheets') }}
    </span>
    <span v-if="pending.autoSave && waitingRows" class="text-xs text-amber-900">
      {{
        $tn(
          waitingRows,
          '{n} fila del recorrido espera a que pulses Guardar',
          '{n} filas del recorrido esperan a que pulses Guardar',
        )
      }}
    </span>
    <span v-else-if="pending.autoSave && pending.autoBlocked" class="text-xs text-amber-900">{{ $t(pending.autoBlocked) }}</span>
    <span v-else-if="!pending.autoSave" class="hint hidden md:inline">{{
      $t('Se conservan en este dispositivo hasta que guardes.')
    }}</span>
    <div class="ml-auto flex flex-wrap items-center gap-2">
      <label
        class="flex items-center gap-1.5 text-xs text-stone-600"
        :title="$t('Escribir en Google Sheets poco después de cada cambio')"
      >
        <input
          type="checkbox"
          :checked="pending.autoSave"
          @change="pending.setAutoSave(($event.target as HTMLInputElement).checked)"
        />
        {{ $t('Guardar automáticamente') }}
      </label>
      <button class="btn" @click="discard"><Undo2 :size="15" /> {{ $t('Descartar') }}</button>
      <button class="btn" @click="reviewing = true"><Eye :size="15" /> {{ $t('Revisar') }}</button>
      <button class="btn-primary" :disabled="pending.saving" @click="save()">
        <Save :size="15" />
        {{ pending.saving ? $t('Guardando…') : pending.autoSave ? $t('Guardar ya') : $t('Guardar en la hoja') }}
      </button>
    </div>
  </div>
  <div
    v-else-if="justSaved"
    class="flex items-center gap-1.5 border-t border-stone-200 bg-surface px-3 py-1.5 text-xs text-brand-700 sm:px-4"
    role="status"
  >
    <CheckCircle2 :size="14" />
    <template v-if="pending.lastSaved!.where === 'app'">{{
      $tn(
        pending.lastSaved!.count,
        'Guardado en la app ({n} cambio), visible para todo el equipo · aún no en Google Sheets',
        'Guardado en la app ({n} cambios), visible para todo el equipo · aún no en Google Sheets',
      )
    }}</template>
    <template v-else>{{
      $tn(
        pending.lastSaved!.count,
        'Guardado en Google Sheets ({n} cambio) · se puede deshacer en Historial',
        'Guardado en Google Sheets ({n} cambios) · se puede deshacer en Historial',
      )
    }}</template>
  </div>
  <ReviewDialog v-if="reviewing" @close="reviewing = false" @save="save" />
</template>
