<script setup lang="ts">
import { computed, onBeforeUnmount, ref } from 'vue'
import { X, Save, RotateCcw } from 'lucide-vue-next'
import { displayValue } from '../lib/cells'
import { usePending } from '../stores/pending'
import { useSession } from '../stores/session'
import { useTables } from '../stores/tables'

/** Lists every pending change as before → after, like "Subir cambios" did. */
const emit = defineEmits<{ close: []; save: [reason: string] }>()
const pending = usePending()
const session = useSession()
const tables = useTables()
const reason = ref('')

const fieldOf = (module: string, key: string) => session.module(module)?.fields.find(f => f.key === key)
const edits = computed(() =>
  Object.values(pending.edits).map(e => ({
    ...e,
    fields: Object.keys(e.values).map(field => ({
      field,
      before: displayValue(e.before[field], fieldOf(e.module, field)),
      after: displayValue(e.values[field], fieldOf(e.module, field)),
      error: pending.issues[`${e.id}:${field}`],
    })),
    rowError: pending.issues[`${e.id}:*`],
  })),
)

function revert(id: string, module: string, field: string) {
  const row = tables.row(module, id)
  const edit = pending.edits[id]
  if (!row || !edit) return
  pending.setCell(module, row, edit.label, field, row.values[field] ?? null)
  pending.touch()
}
function removeCreate(clientId: string) {
  pending.removeCreate(clientId)
  pending.touch()
}
function createErrors(clientId: string) {
  return Object.entries(pending.issues)
    .filter(([key]) => key.startsWith(`${clientId}:`))
    .map(([, message]) => message)
}
// Escape closes the dialog, as any dialog.
const onKey = (e: KeyboardEvent) => e.key === 'Escape' && emit('close')
window.addEventListener('keydown', onKey)
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <div class="fixed inset-0 z-40 grid place-items-center bg-black/40 p-2" @click.self="emit('close')">
    <section class="flex max-h-[92vh] w-full max-w-3xl flex-col rounded-lg bg-white shadow-xl">
      <header class="flex items-center border-b border-stone-200 px-4 py-3">
        <h2 class="flex-1 text-lg font-semibold">Revisar cambios antes de guardar</h2>
        <button class="btn-ghost" @click="emit('close')"><X :size="20" /></button>
      </header>
      <div class="flex-1 overflow-y-auto px-4 py-3">
        <p v-if="!pending.changeCount" class="text-stone-500">No hay cambios pendientes.</p>
        <div v-for="edit in edits" :key="edit.id" class="mb-3">
          <h3 class="text-sm font-semibold">
            {{ edit.label || '(sin ID)' }} <span class="font-normal text-stone-500">· {{ edit.module }} fila {{ edit.row }}</span>
          </h3>
          <p v-if="edit.rowError" class="text-sm text-red-700">{{ edit.rowError }}</p>
          <table class="mt-1 w-full text-sm">
            <tbody>
              <tr v-for="f in edit.fields" :key="f.field" class="border-t border-stone-100" :class="{ 'bg-red-50': f.error }">
                <td class="w-1/3 py-1 pr-2 text-stone-600">{{ f.field }}</td>
                <td class="py-1 pr-2 text-stone-500 line-through decoration-stone-300">{{ f.before || 'vacío' }}</td>
                <td class="py-1 pr-2 font-medium">{{ f.after || 'vacío' }}</td>
                <td class="w-8 text-right">
                  <button class="btn-ghost" title="Quitar este cambio" @click="revert(edit.id, edit.module, f.field)">
                    <RotateCcw :size="14" />
                  </button>
                </td>
              </tr>
              <tr v-for="f in edit.fields.filter(x => x.error)" :key="`${f.field}-error`">
                <td colspan="4" class="pb-1 text-xs text-red-700">{{ f.field }}: {{ f.error }}</td>
              </tr>
            </tbody>
          </table>
        </div>
        <div v-for="c in pending.creates" :key="c.clientId" class="mb-3">
          <h3 class="text-sm font-semibold">
            Fila nueva {{ c.label }} <span class="font-normal text-stone-500">· {{ c.module }}</span>
            <button class="ml-2 text-xs text-red-700 underline" @click="removeCreate(c.clientId)">quitar</button>
          </h3>
          <p v-for="m in createErrors(c.clientId)" :key="m" class="text-sm text-red-700">{{ m }}</p>
          <p class="text-sm text-stone-700">
            <span v-for="(value, key) in c.values" :key="key" class="mr-3 inline-block">
              <span class="text-stone-500">{{ key }}:</span> {{ displayValue(value, fieldOf(c.module, String(key))) }}
            </span>
          </p>
        </div>
      </div>
      <footer class="flex flex-wrap items-end gap-2 border-t border-stone-200 px-4 py-3">
        <label class="min-w-48 flex-1">
          <span class="field-label">Nota para el historial (opcional)</span>
          <input v-model="reason" class="field-input" placeholder="p. ej. ronda del lunes" />
        </label>
        <button class="btn-primary" :disabled="pending.saving || !pending.changeCount" @click="emit('save', reason)">
          <Save :size="15" /> {{ pending.saving ? 'Guardando…' : `Guardar ${pending.changeCount} cambios` }}
        </button>
      </footer>
    </section>
  </div>
</template>
