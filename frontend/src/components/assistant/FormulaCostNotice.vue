<script setup lang="ts">
/**
 * Above a proposal that writes formulas: what they cost the Google Sheet's
 * recalculation (lib/formulaCost), one line per formula. Amber when a formula
 * is heavy (the server's thresholds, from the workbook audit): its line says
 * what would make it lighter.
 */
import { computed } from 'vue'
import { AlertTriangle, Calculator } from 'lucide-vue-next'
import { costLine, costTip, type FormulaCost } from '../../lib/formulaCost'

const props = defineProps<{ cost: FormulaCost[] }>()
const heavy = computed(() => props.cost.some(p => p.heavy))
</script>

<template>
  <div v-if="cost.length" class="formula-cost" :class="{ 'is-heavy': heavy }" role="status" data-test="formula-cost">
    <component :is="heavy ? AlertTriangle : Calculator" :size="12" class="mt-0.5 shrink-0" />
    <span class="min-w-0 flex-1">
      <span
        v-for="p in cost"
        :key="`${p.sheet}:${p.column}:${p.formula}`"
        class="block"
        :class="{ 'font-medium': p.heavy }"
        :title="p.formula"
      >
        <span class="font-mono">ƒx {{ p.column }}</span
        >: {{ costLine(p) }}
        <span v-if="costTip(p)" class="block pl-4 opacity-90">{{ $t('Más ligero:') }} {{ costTip(p) }}</span>
      </span>
    </span>
  </div>
</template>

<style scoped>
.formula-cost {
  display: flex;
  align-items: flex-start;
  gap: 4px;
  border-bottom: 1px solid #e2e8f0;
  background: #f8fafc;
  padding: 4px 8px;
  font-size: 11px;
  color: #475569;
}
.formula-cost.is-heavy {
  border-bottom-color: #fde68a;
  background: #fffbeb;
  color: #92400e;
}
</style>
