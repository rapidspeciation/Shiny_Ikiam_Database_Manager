<script setup lang="ts">
import { computed } from 'vue'
import { NotebookText } from 'lucide-vue-next'
import { t } from '../../lib/i18n'
import type { CompareCell, CompareLine, RecordFacts } from '../../lib/photoCuration'

/**
 * A CAM ID case under its photos: what the photo shows (envelope and models) beside the database row of the CAM
 * read and of the file's CAM, green where the photo agrees with a row, amber where it does not; then the notes
 * of both, the changes they record first (a CAM_ID changed after the photo leaves the old one on the envelope).
 */
const props = defineProps<{ lines: CompareLine[]; records: RecordFacts[]; fileCam: string }>()
const target = computed(() => props.records[0])
// The file's CAM, also when the database has no row for it
const file = computed<RecordFacts>(() => props.records[1] ?? { cam: props.fileCam })
const rowsOf = (r?: RecordFacts) => r?.database ?? []
const tone = (c: CompareCell) => (c.same === true ? 'bg-emerald-50 text-emerald-900' : c.same === false ? 'bg-amber-50 font-medium text-amber-900' : 'text-stone-800')
const fieldWord = (f: string) => (f === 'species' ? t('la especie') : f === 'sex' ? t('el sexo') : 'CAM_ID')
</script>

<template>
  <div class="text-xs" data-compare>
    <table class="w-full border-collapse">
      <thead>
        <tr class="align-bottom text-[10px] text-stone-500">
          <th class="w-24 py-0.5 pr-2 text-left font-normal" />
          <th class="py-0.5 pr-2 text-left font-normal">{{ $t('Foto') }}</th>
          <th v-for="(r, i) in [target, file]" :key="i" class="py-0.5 pr-2 text-left font-normal">
            <span class="font-mono text-[11px] font-medium text-stone-800">{{ r?.cam }}</span>
            · {{ i === 0 ? $t('el CAM leído') : $t('el CAM del archivo') }}
            <template v-for="d in rowsOf(r)" :key="d.sheet + d.row"><br />{{ d.sheet }} {{ $t('fila {n}', { n: d.row }) }}</template>
            <template v-if="!rowsOf(r).length"><br /><span class="text-amber-800">{{ $t('sin fila en la base de datos') }}</span></template>
          </th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="l in lines" :key="l.label" class="border-t border-stone-100 align-top">
          <td class="py-0.5 pr-2 text-stone-500">{{ l.label }}</td>
          <td class="py-0.5 pr-2 text-stone-700">{{ l.photo || '—' }}</td>
          <td class="px-1 py-0.5" :class="tone(l.target)">{{ l.target.value || '—' }}</td>
          <td class="px-1 py-0.5" :class="tone(l.file)">{{ l.file.value || '—' }}</td>
        </tr>
      </tbody>
    </table>
    <template v-for="r in [target, file]" :key="'n' + r?.cam">
      <template v-for="d in rowsOf(r)" :key="'n' + d.sheet + d.row">
        <div v-if="d.note" class="mt-1.5 rounded border border-sky-200 bg-sky-50 px-2 py-1 text-sky-950" data-note>
          <p class="mb-0.5 text-[10px] text-sky-800">{{ $t('Notas de {cam}', { cam: r?.cam ?? '' }) }}</p>
          <p v-for="n in (d.notes ?? []).filter(n => !n.checked)" :key="n.text" class="mb-0.5 font-medium text-amber-900">
            {{ $t('La nota cambia {field}: {from} → {to}.', { field: fieldWord(n.field), from: n.from ?? '?', to: n.to ?? '?' }) }}
          </p>
          <p class="flex gap-1"><NotebookText :size="12" class="mt-0.5 shrink-0 text-sky-700" /><span class="break-words whitespace-pre-line">{{ d.note }}</span></p>
        </div>
      </template>
    </template>
  </div>
</template>
