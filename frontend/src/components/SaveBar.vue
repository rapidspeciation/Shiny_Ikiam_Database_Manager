<script setup lang="ts">
import { computed, onBeforeUnmount, ref } from 'vue'
import { Save, Eye, Undo2, AlertTriangle, CheckCircle2, Loader2 } from 'lucide-vue-next'
import ReviewDialog from './ReviewDialog.vue'
import { usePending } from '../stores/pending'
import { errorText, notify } from '../lib/notice'

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
const cells = (n: number) => `${n} ${n === 1 ? 'celda' : 'celdas'}`

async function save(reason = '') {
  try {
    const { saved } = await pending.save(reason)
    const left = errorCount.value
    if (!left) reviewing.value = false
    if (saved && !left) notify(`${saved} ${saved === 1 ? 'cambio guardado' : 'cambios guardados'} en Google Sheets`, 'success')
    else if (saved) notify(`${saved} guardados; ${cells(left)} sin guardar: ${issues.value[0]}`, 'error')
    else if (left) {
      notify(`${cells(left)} sin guardar: ${issues.value[0]}`, 'error')
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
  else notify('Conexión recuperada: ya puedes guardar los cambios pendientes')
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
  if (confirm(`¿Descartar ${pending.changeCount} cambios sin guardar?`)) pending.discard()
}
</script>

<template>
  <div
    v-if="pending.changeCount"
    class="flex flex-wrap items-center gap-2 border-t px-3 py-2 text-sm sm:px-4"
    :class="pending.autoSave && !errorCount ? 'border-stone-200 bg-white' : 'border-amber-300 bg-amber-50'"
  >
    <span v-if="pending.autoSave && pending.saving" class="flex items-center gap-1.5 font-medium text-stone-700">
      <Loader2 :size="15" class="animate-spin" /> Guardando {{ pending.changeCount }}
      {{ pending.changeCount === 1 ? 'cambio' : 'cambios' }} en Google Sheets…
    </span>
    <span v-else class="font-medium text-amber-950">
      {{ pending.changeCount }} {{ pending.changeCount === 1 ? 'cambio' : 'cambios' }} en {{ pending.rowCount }}
      {{ pending.rowCount === 1 ? 'fila' : 'filas' }}
      {{ pending.autoSave && !pending.autoBlocked && errorCount < pending.changeCount ? 'por guardar' : 'sin guardar' }}
    </span>
    <button
      v-if="errorCount"
      type="button"
      class="flex min-w-0 max-w-full items-center gap-1 text-left font-medium text-red-800 hover:underline"
      :title="issues.join('\n')"
      @click="reviewing = true"
    >
      <AlertTriangle :size="15" class="shrink-0" /> {{ cells(errorCount) }} sin guardar:
      <span class="truncate font-normal">{{ issues[0] }}</span>
    </button>
    <span v-if="!online" class="rounded bg-stone-700 px-2 py-0.5 text-xs text-white">Sin conexión</span>
    <span v-if="pending.autoSave && pending.autoBlocked" class="text-xs text-amber-900">{{ pending.autoBlocked }}</span>
    <span v-else-if="!pending.autoSave" class="hint hidden md:inline">Se conservan en este dispositivo hasta que guardes.</span>
    <div class="ml-auto flex flex-wrap items-center gap-2">
      <label
        class="flex items-center gap-1.5 text-xs text-stone-600"
        title="Escribir en Google Sheets poco después de cada cambio"
      >
        <input
          type="checkbox"
          :checked="pending.autoSave"
          @change="pending.setAutoSave(($event.target as HTMLInputElement).checked)"
        />
        Guardar automáticamente
      </label>
      <button class="btn" @click="discard"><Undo2 :size="15" /> Descartar</button>
      <button class="btn" @click="reviewing = true"><Eye :size="15" /> Revisar</button>
      <button class="btn-primary" :disabled="pending.saving" @click="save()">
        <Save :size="15" /> {{ pending.saving ? 'Guardando…' : pending.autoSave ? 'Guardar ya' : 'Guardar en la hoja' }}
      </button>
    </div>
  </div>
  <div
    v-else-if="justSaved"
    class="flex items-center gap-1.5 border-t border-stone-200 bg-white px-3 py-1.5 text-xs text-brand-700 sm:px-4"
    role="status"
  >
    <CheckCircle2 :size="14" /> Guardado en Google Sheets ({{ pending.lastSaved!.count }}
    {{ pending.lastSaved!.count === 1 ? 'cambio' : 'cambios' }}) · se puede deshacer en Historial
  </div>
  <ReviewDialog v-if="reviewing" @close="reviewing = false" @save="save" />
</template>
