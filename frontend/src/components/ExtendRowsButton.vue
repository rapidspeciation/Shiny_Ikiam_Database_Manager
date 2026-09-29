<script setup lang="ts">
import { ref } from 'vue'
import { Rows3 } from 'lucide-vue-next'
import { api } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import { useSession } from '../stores/session'

/**
 * "Crear filas preasignadas": more empty rows at the end of a sheet, copies of
 * its last pre-made row (formulas, dropdowns, and in Insectary_data the next
 * Insectary IDs). Reviewers and admins only; others are told whom to ask.
 */
const props = withDefaults(defineProps<{ sheet: string; count?: number }>(), { count: 200 })
const emit = defineEmits<{ done: [] }>()
const session = useSession()
const busy = ref(false)

interface Extended {
  added: { from: number; to: number; count: number }
  completed: { from: number; to: number; columns: string[] } | null
  firstId: string | null
  lastId: string | null
  ok: boolean
  problems: string[]
  ownerColumns: string[]
}

async function extend() {
  const answer = prompt(`¿Cuántas filas preasignadas crear al final de ${props.sheet}? (máximo 500)`, String(props.count))
  if (answer === null) return
  const count = Number(answer)
  if (!Number.isInteger(count) || count < 1 || count > 500) return notify('Indica entre 1 y 500 filas', 'error')
  busy.value = true
  try {
    const r = await api<Extended>(`sheets/${encodeURIComponent(props.sheet)}/extend`, { method: 'POST', body: { count } })
    const ids = r.firstId ? ` (${r.firstId}–${r.lastId})` : ''
    const owner = r.ownerColumns.length
      ? ` Las columnas protegidas ${r.ownerColumns.join(', ')} debe completarlas el dueño de la hoja.`
      : ''
    const text = `Filas ${r.added.from}–${r.added.to} creadas en ${props.sheet}${ids}.${owner}`
    if (r.ok) notify(text, owner ? 'info' : 'success', owner ? 12000 : undefined)
    else notify(`${text} Revisa: ${r.problems.join('; ')}`, 'error', 15000)
    emit('done')
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <button v-if="session.isReviewer" class="btn" :disabled="busy" title="Copias de la última fila preasignada" @click="extend">
    <Rows3 :size="15" /> {{ busy ? 'Creando…' : 'Crear filas preasignadas' }}
  </button>
  <span v-else class="text-xs">Avisa a un administrador para que cree más.</span>
</template>
