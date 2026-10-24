<script setup lang="ts">
import { computed } from 'vue'
import { NotebookText } from 'lucide-vue-next'
import { t } from '../../lib/i18n'
import { recordLines, type RecordFacts } from '../../lib/photoCuration'

/**
 * A butterfly of the selected case, under the photos: what its envelope reads beside what the database
 * says (amber where they disagree), and its notes whole, the changes they record first: a CAM_ID or a
 * species changed after the photo makes the envelope out of date.
 */
const props = defineProps<{ record: RecordFacts & { named?: boolean }; second?: boolean; sheet?: string }>()
const lines = computed(() => recordLines(props.record, props.sheet))
const rows = computed(() => props.record.database ?? [])
const fieldWord = (f: string) => (f === 'species' ? t('la especie') : f === 'sex' ? t('el sexo') : 'CAM_ID')
</script>

<template>
  <div class="text-xs" :data-record="record.cam">
    <p class="mb-1 font-medium text-stone-800">
      <span class="font-mono">{{ record.cam }}</span>
      <span v-if="record.named" class="font-normal text-stone-500"> · {{ $t('el CAM leído en el sobre') }}</span>
      <span v-else-if="second" class="font-normal text-stone-500"> · {{ $t('el nombre del archivo') }}</span>
      <span v-for="r in rows" :key="r.sheet + r.row" class="font-normal text-stone-500"> · {{ r.sheet }} {{ $t('fila {n}', { n: r.row }) }}</span>
      <span v-if="!rows.length" class="font-normal text-amber-800"> · {{ $t('sin fila en la base de datos') }}</span>
    </p>
    <table v-if="lines.length" class="w-full border-collapse">
      <thead>
        <tr class="text-[10px] text-stone-500">
          <th class="w-24 py-0.5 pr-2 text-left font-normal" />
          <th class="py-0.5 pr-2 text-left font-normal">{{ $t('Sobre (OCR)') }}</th>
          <th class="py-0.5 text-left font-normal">{{ $t('Base de datos') }}</th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="l in lines" :key="l.label + l.envelope" class="border-t border-stone-100 align-top" :class="l.differ ? 'bg-amber-50' : ''">
          <td class="py-0.5 pr-2 text-stone-500">{{ l.label }}</td>
          <td class="py-0.5 pr-2" :class="l.differ ? 'text-amber-900' : 'text-stone-700'">{{ l.envelope || '—' }}</td>
          <td class="py-0.5" :class="l.differ ? 'font-medium text-amber-900' : 'text-stone-800'">{{ l.database || '—' }}</td>
        </tr>
      </tbody>
    </table>
    <template v-for="r in rows" :key="'n' + r.sheet + r.row">
      <div v-if="r.note" class="mt-1.5 rounded border border-sky-200 bg-sky-50 px-2 py-1 text-sky-950" data-note>
        <p v-for="n in (r.notes ?? []).filter(n => !n.checked)" :key="n.text" class="mb-0.5 font-medium text-amber-900">
          {{ $t('La nota cambia {field}: {from} → {to}.', { field: fieldWord(n.field), from: n.from ?? '?', to: n.to ?? '?' }) }}
        </p>
        <p class="flex gap-1"><NotebookText :size="12" class="mt-0.5 shrink-0 text-sky-700" /><span class="break-words whitespace-pre-line">{{ r.note }}</span></p>
      </div>
    </template>
  </div>
</template>
