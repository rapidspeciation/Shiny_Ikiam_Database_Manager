<script setup lang="ts">
import { Check } from 'lucide-vue-next'

/**
 * Under one sheet's table of a proposal with rows of several sheets: its own
 * «Aplicar» (only that sheet's rows; the others stay pending, to check first),
 * or, once written, that it was applied.
 */
defineProps<{ sheet: string; rows: number; written: number; busy?: boolean }>()
const emit = defineEmits<{ apply: [] }>()
</script>

<template>
  <div v-if="rows || written" class="flex flex-wrap items-center gap-2 px-2 py-1.5" data-sheet-apply>
    <button
      v-if="rows"
      class="btn-primary bg-emerald-700 hover:bg-emerald-800"
      :disabled="busy"
      :title="$t('Escribe en la hoja solo las filas de {sheet}; las demás siguen pendientes', { sheet })"
      @click="emit('apply')"
    >
      <Check :size="15" /> {{ $tn(rows, 'Aplicar {n} fila', 'Aplicar {n} filas') }}
      <span class="font-normal opacity-80">· {{ sheet }}</span>
    </button>
    <span v-if="written" class="flex items-center gap-1 text-xs text-brand-700">
      <Check :size="13" /> {{ sheet }} · {{ $tn(written, '{n} fila ya escrita en la hoja', '{n} filas ya escritas en la hoja') }}
    </span>
  </div>
</template>
