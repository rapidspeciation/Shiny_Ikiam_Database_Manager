<script setup lang="ts">
import { computed, ref } from 'vue'
import { Rows3 } from 'lucide-vue-next'
import { api } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import { useSession } from '../stores/session'
import { t, tm, type Msg } from '../lib/i18n'

/**
 * "Crear filas preasignadas": more empty rows at the end of a sheet, copies of
 * its last pre-made row (formulas, dropdowns). In Insectary_data the next
 * Insectary IDs go into the rows below the last ID, which hold the other
 * formulas already. Reviewers and admins only; others are told whom to ask.
 */
const props = withDefaults(defineProps<{ sheet: string; count?: number }>(), { count: 200 })
const emit = defineEmits<{ done: [] }>()
const session = useSession()
const busy = ref(false)
const byId = computed(() => props.sheet === 'Insectary_data')

interface Extended {
  /** Rows appended to the sheet (in Insectary_data only past its last row, else null). */
  added: { from: number; to: number; count: number } | null
  /** Insectary_data: the rows that got the next IDs. */
  filled: { from: number; to: number; count: number } | null
  completed: { from: number; to: number; columns: string[] } | null
  firstId: string | null
  lastId: string | null
  ok: boolean
  problems: string[]
  /** The problems' descriptors, for the interface language (server/messages.mjs). */
  problemsMsg?: Msg[]
  ownerColumns: string[]
}

async function extend() {
  const answer = prompt(
    byId.value
      ? t('¿Cuántos Insectary IDs más? Se escriben en las filas que siguen al último ID de {sheet} (máximo 500)', {
          sheet: props.sheet,
        })
      : t('¿Cuántas filas preasignadas crear al final de {sheet}? (máximo 500)', { sheet: props.sheet }),
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
    const made = r.filled
      ? t('Insectary IDs{ids} escritos en las filas {from}–{to} de {sheet}.', { from: r.filled.from, to: r.filled.to, sheet: props.sheet, ids })
      : t('Filas {from}–{to} creadas en {sheet}{ids}.', { from: r.added?.from, to: r.added?.to, sheet: props.sheet, ids })
    const text = `${made}${owner}`
    if (r.ok) notify(text, owner ? 'info' : 'success', owner ? 12000 : undefined)
    else
      notify(
        `${text} ${t('Revisa: {problems}', { problems: (r.problemsMsg?.map(tm) ?? r.problems).join('; ') })}`,
        'error',
        15000,
      )
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
    :title="byId ? $t('Los siguientes Insectary IDs, en las filas que siguen al último ID') : $t('Copias de la última fila preasignada')"
    @click="extend"
  >
    <Rows3 :size="15" /> {{ busy ? $t('Creando…') : $t('Crear filas preasignadas') }}
  </button>
  <span v-else class="text-xs">{{ $t('Avisa a un administrador para que cree más.') }}</span>
</template>
