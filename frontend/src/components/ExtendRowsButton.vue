<script setup lang="ts">
import { ref } from 'vue'
import { Rows3 } from 'lucide-vue-next'
import { api } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import { useSession } from '../stores/session'
import { t } from '../lib/i18n'

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
  const answer = prompt(
    t('¿Cuántas filas preasignadas crear al final de {sheet}? (máximo 500)', { sheet: props.sheet }),
    String(props.count),
  )
  if (answer === null) return
  const count = Number(answer)
  if (!Number.isInteger(count) || count < 1 || count > 500) return notify(t('Indica entre 1 y 500 filas'), 'error')
  busy.value = true
  try {
    const r = await api<Extended>(`sheets/${encodeURIComponent(props.sheet)}/extend`, { method: 'POST', body: { count } })
    const ids = r.firstId ? ` (${r.firstId}–${r.lastId})` : ''
    const owner = r.ownerColumns.length
      ? ` ${t('Las columnas protegidas {columns} debe completarlas el dueño de la hoja.', { columns: r.ownerColumns.join(', ') })}`
      : ''
    const text = `${t('Filas {from}–{to} creadas en {sheet}{ids}.', { from: r.added.from, to: r.added.to, sheet: props.sheet, ids })}${owner}`
    if (r.ok) notify(text, owner ? 'info' : 'success', owner ? 12000 : undefined)
    else notify(`${text} ${t('Revisa: {problems}', { problems: r.problems.join('; ') })}`, 'error', 15000)
    emit('done')
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <button
    v-if="session.isReviewer"
    class="btn"
    :disabled="busy"
    :title="$t('Copias de la última fila preasignada')"
    @click="extend"
  >
    <Rows3 :size="15" /> {{ busy ? $t('Creando…') : $t('Crear filas preasignadas') }}
  </button>
  <span v-else class="text-xs">{{ $t('Avisa a un administrador para que cree más.') }}</span>
</template>
