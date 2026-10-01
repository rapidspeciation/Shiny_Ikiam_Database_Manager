<script setup lang="ts">
import { ArrowRight, Undo2, X } from 'lucide-vue-next'
import { displayValue } from '../../lib/cells'
import type { UndoPreviewItem } from '../../lib/types'
import type { UndoReview } from '../../composables/useUndo'
import { useSession } from '../../stores/session'

/**
 * The confirmation before an undo (see useUndo): every cell that goes back, as
 * now → after the undo, and the changes that cannot be undone (changed again
 * since, the row gone), in which case nothing is written. An optional reason;
 * «Deshacer en la hoja» writes it. Shared by the Historial and the tabs' own history.
 */
defineProps<{ review: UndoReview; busy: boolean }>()
const reason = defineModel<string>('reason', { default: '' })
const emit = defineEmits<{ cancel: []; confirm: [] }>()
const session = useSession()

/** Why a change cannot be undone (Spanish, the keys of lib/i18n.ts: shown through t()). */
const CONFLICT: Record<string, string> = {
  later_field_edit: 'se cambió otra vez después',
  missing_record: 'la fila ya no existe',
  value_or_chain_changed: 'ya no tiene el valor guardado',
}
const fieldOf = (sheet: string | null | undefined, key: string) =>
  sheet ? session.module(sheet)?.fields.find(f => f.key === key) : undefined
const empty = (value: UndoPreviewItem['before']) => value === null || value === undefined || value === ''
</script>

<template>
  <div class="fixed inset-0 z-[60] grid place-items-center bg-black/40 p-2" @click.self="!busy && emit('cancel')">
    <section class="flex max-h-[90vh] w-full max-w-2xl flex-col rounded-lg bg-white shadow-xl" role="dialog" :aria-label="review.title">
      <header class="flex items-start gap-2 border-b border-stone-200 px-4 py-3">
        <div class="min-w-0 flex-1">
          <h2 class="text-lg font-semibold">
            {{ $tn(review.preview.changes.length, 'Deshacer {n} cambio', 'Deshacer {n} cambios') }}
          </h2>
          <p class="hint break-words">{{ review.title }}</p>
        </div>
        <button class="btn-ghost h-11 w-11 justify-center" :aria-label="$t('Cerrar')" :disabled="busy" @click="emit('cancel')"><X :size="20" /></button>
      </header>
      <div class="flex-1 overflow-y-auto px-4 py-3 text-sm">
        <div v-if="review.preview.conflicts.length" class="mb-3 rounded bg-red-50 px-3 py-2 text-red-800">
          <p class="font-medium">
            {{
              $tn(
                review.preview.conflicts.length,
                '{n} cambio no se puede deshacer; no se escribe nada. Deshaz primero lo que se cambió después, elige otros cambios o corrígelo a mano.',
                '{n} cambios no se pueden deshacer; no se escribe nada. Deshaz primero lo que se cambió después, elige otros cambios o corrígelo a mano.',
              )
            }}
          </p>
          <ul class="mt-1 space-y-0.5">
            <li v-for="c in review.preview.conflicts" :key="c.recordId + c.field">
              <strong>{{ c.label || c.recordId }}</strong> · {{ c.field }}:
              {{ CONFLICT[c.reason || ''] ? $t(CONFLICT[c.reason || '']) : c.reason }}
              {{ $t('(ahora {value})', { value: c.before && typeof c.before === 'object' ? $t('fórmula {formula}', { formula: c.before.formula }) : displayValue(c.before, fieldOf(c.sheet, c.field)) || $t('vacío') }) }}
            </li>
          </ul>
        </div>
        <p class="hint mb-1">{{ $t('Cada celda vuelve al valor que tenía antes del guardado:') }}</p>
        <ul class="divide-y divide-stone-100">
          <li v-for="c in review.preview.changes" :key="c.recordId + c.field" class="py-1 sm:flex sm:items-start sm:gap-2">
            <span class="block shrink-0 sm:w-56">
              <strong>{{ c.label || c.recordId }}</strong>
              <span class="ml-1.5 font-mono text-xs text-stone-600">{{ c.field }}</span>
            </span>
            <span class="flex min-w-0 flex-wrap items-center gap-1">
              <span
                class="break-all"
                :class="empty(c.before) ? 'italic text-stone-400' : 'rounded bg-red-50 px-1 text-red-800 line-through decoration-red-300'"
                >{{
                  c.before && typeof c.before === 'object'
                    ? $t('fórmula {formula}', { formula: c.before.formula })
                    : displayValue(c.before, fieldOf(c.sheet, c.field)) || $t('vacío')
                }}</span
              >
              <ArrowRight :size="12" class="shrink-0 text-stone-400" />
              <span class="break-all" :class="empty(c.after) ? 'italic text-stone-400' : 'rounded bg-emerald-50 px-1 font-medium text-emerald-900'">{{
                c.after && typeof c.after === 'object'
                  ? $t('fórmula {formula}', { formula: c.after.formula })
                  : displayValue(c.after, fieldOf(c.sheet, c.field)) || $t('vacío')
              }}</span>
            </span>
          </li>
        </ul>
      </div>
      <footer class="flex flex-wrap items-end gap-2 border-t border-stone-200 px-4 py-3">
        <label class="min-w-48 flex-1">
          <span class="field-label">{{ $t('Motivo (opcional)') }}</span>
          <input v-model="reason" class="field-input" :placeholder="$t('p. ej. mariposas guardadas dos veces')" />
        </label>
        <button class="btn h-11" :disabled="busy" @click="emit('cancel')">{{ $t('Cancelar') }}</button>
        <button class="btn-primary h-11" :disabled="busy || !review.preview.eligible" @click="emit('confirm')">
          <Undo2 :size="15" /> {{ busy ? $t('Deshaciendo…') : $t('Deshacer en la hoja') }}
        </button>
      </footer>
    </section>
  </div>
</template>
