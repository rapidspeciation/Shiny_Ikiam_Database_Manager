<script setup lang="ts">
// The team's flash-frozen specimens of the candidate being compared (views/IdentifyView.vue), against the
// reference-genome target: 3 males and 3 females per subspecies (per species without subspecies) on each side of
// the Andes. The gap first; the detail on a tap. The inventory date is the snapshot's (it works offline).
import { computed } from 'vue'
import { stockGaps, TARGET, type AndesSide, type StockDetail } from '../../lib/identifyResult'
import { stockDate } from '../../lib/frozenStock'
import { t } from '../../lib/i18n'

const props = defineProps<{
  name: string
  detail: StockDetail | undefined
  recorded: { East: boolean; West: boolean } | undefined
  asOf: string
}>()
const gaps = computed(() => stockGaps(props.detail, props.recorded))
const sideName = (s: AndesSide) => (s === 'East' ? t('Este') : t('Oeste'))
const want = (m: number, f: number) =>
  [m ? t('{n} ♂', { n: m }) : '', f ? t('{n} ♀', { n: f }) : ''].filter(Boolean).join(t(' y '))
const complete = computed(() => {
  const sides = [...new Set(gaps.value.rows.map(r => r.side))]
  return sides.filter(s => !gaps.value.missing.some(m => m.side === s))
})
</script>

<template>
  <section class="rounded-lg border border-border bg-surface p-3">
    <h3 class="text-sm font-semibold">{{ $t('Congelados para genomas de referencia') }}</h3>
    <p class="text-xs text-fg-muted"><i>{{ name }}</i> · {{ stockDate(asOf) }}</p>
    <p v-if="!gaps.rows.length && !gaps.apart.length" class="mt-2 text-sm">{{ $t('Ningún congelado de esta especie.') }}</p>
    <ul v-else class="mt-2 space-y-0.5 text-sm">
      <li v-for="m in gaps.missing" :key="m.side" class="font-medium text-amber-900">
        {{ $t('Faltan en el {side}:', { side: sideName(m.side) }) }}
        <template v-for="(s, i) in m.short" :key="s.subspecies"
          ><template v-if="i">, </template><template v-if="s.subspecies"><i>{{ s.subspecies }}</i>{{ ' ' }}</template>{{ want(s.M, s.F) }}</template
        >
        <span v-if="m.unassigned" class="font-normal text-fg-muted">
          ({{ $tn(m.unassigned, 'otro sin subespecie anotada; puede contar', 'otros {n} sin subespecie anotada; pueden contar') }})</span
        >
      </li>
      <li v-for="s in complete" :key="s" class="text-success-text">{{ $t('{side}: completo', { side: sideName(s) }) }} ✓</li>
    </ul>
    <details v-if="gaps.rows.length || gaps.apart.length" class="mt-2 text-sm">
      <summary class="cursor-pointer text-brand-text">{{ $t('Detalle') }}</summary>
      <p class="mt-1 text-xs text-fg-muted">
        {{ gaps.bySubspecies ? $t('Meta: {n} ♂ y {n} ♀ por subespecie y lado de los Andes.', { n: TARGET }) : $t('Meta: {n} ♂ y {n} ♀ por lado de los Andes.', { n: TARGET }) }}
        {{ $t('La IA no distingue subespecies.') }}
      </p>
      <table class="mt-1 w-full text-left tabular-nums">
        <thead class="text-xs text-fg-muted">
          <tr>
            <th v-if="gaps.bySubspecies" class="py-1 font-medium">{{ $t('Subespecie') }}</th>
            <th class="py-1 font-medium">{{ $t('Lado') }}</th>
            <th class="py-1 text-right font-medium">♂</th>
            <th class="py-1 text-right font-medium">♀</th>
            <th class="py-1 text-right font-medium">{{ $t('Sin sexo') }}</th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="r in gaps.rows" :key="r.subspecies + r.side" class="border-t border-border">
            <td v-if="gaps.bySubspecies" class="py-1"><i>{{ r.subspecies || $t('(ninguna registrada)') }}</i></td>
            <td class="py-1">{{ sideName(r.side) }}</td>
            <td class="py-1 text-right" :class="r.missingM ? 'font-semibold text-amber-900' : ''">{{ r.M }}/{{ TARGET }}</td>
            <td class="py-1 text-right" :class="r.missingF ? 'font-semibold text-amber-900' : ''">{{ r.F }}/{{ TARGET }}</td>
            <td class="py-1 text-right text-fg-muted">{{ r.unknown }}</td>
          </tr>
          <tr v-for="a in gaps.apart" :key="a.label + (a.side ?? '')" class="border-t border-border text-fg-muted">
            <td :colspan="gaps.bySubspecies ? 2 : 1" class="py-1">
              {{ a.label === 'sin-lado' ? $t('Sin lado de los Andes anotado') : $t('{side}, sin subespecie anotada', { side: sideName(a.side!) }) }}
            </td>
            <td class="py-1 text-right">{{ a.M }}</td>
            <td class="py-1 text-right">{{ a.F }}</td>
            <td class="py-1 text-right">{{ a.unknown }}</td>
          </tr>
        </tbody>
      </table>
    </details>
  </section>
</template>
