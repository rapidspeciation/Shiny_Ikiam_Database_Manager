<script setup lang="ts">
import { computed, ref } from 'vue'
import { Save, Eye, Undo2, AlertTriangle } from 'lucide-vue-next'
import ReviewDialog from './ReviewDialog.vue'
import { usePending } from '../stores/pending'
import { errorText, notify } from '../lib/notice'

/** Always visible while there are unsaved changes: the "Subir cambios" step. */
const pending = usePending()
const reviewing = ref(false)
const errorCount = computed(() => Object.keys(pending.errors).length)

async function save(reason = '') {
  try {
    const count = pending.changeCount
    await pending.save(reason)
    reviewing.value = false
    notify(`${count} ${count === 1 ? 'cambio guardado' : 'cambios guardados'} en Google Sheets`, 'success')
  } catch (e) {
    notify(errorText(e), 'error')
    reviewing.value = true
  }
}
const online = ref(navigator.onLine)
window.addEventListener('offline', () => (online.value = false))
window.addEventListener('online', () => {
  online.value = true
  if (pending.changeCount) notify('Conexión recuperada: ya puedes guardar los cambios pendientes')
})

function discard() {
  if (confirm(`¿Descartar ${pending.changeCount} cambios sin guardar?`)) pending.discard()
}
</script>

<template>
  <div
    v-if="pending.changeCount"
    class="flex flex-wrap items-center gap-2 border-t border-amber-300 bg-amber-50 px-3 py-2 text-sm sm:px-4"
  >
    <span class="font-medium text-amber-950">
      {{ pending.changeCount }} {{ pending.changeCount === 1 ? 'cambio' : 'cambios' }} en {{ pending.rowCount }}
      {{ pending.rowCount === 1 ? 'fila' : 'filas' }} sin guardar
    </span>
    <span v-if="errorCount" class="flex items-center gap-1 text-red-800">
      <AlertTriangle :size="15" /> {{ errorCount }} por revisar
    </span>
    <span v-if="!online" class="rounded bg-stone-700 px-2 py-0.5 text-xs text-white">Sin conexión</span>
    <span class="hint hidden md:inline">Se conservan en este dispositivo hasta que guardes.</span>
    <div class="ml-auto flex gap-2">
      <button class="btn" @click="discard"><Undo2 :size="15" /> Descartar</button>
      <button class="btn" @click="reviewing = true"><Eye :size="15" /> Revisar</button>
      <button class="btn-primary" :disabled="pending.saving" @click="save()">
        <Save :size="15" /> {{ pending.saving ? 'Guardando…' : 'Guardar en la hoja' }}
      </button>
    </div>
  </div>
  <ReviewDialog v-if="reviewing" @close="reviewing = false" @save="save" />
</template>
