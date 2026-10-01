<script setup lang="ts">
import ChoiceField from '../components/ChoiceField.vue'
import DateField from '../components/DateField.vue'
import { computed, ref } from 'vue'
import { History, Plus } from 'lucide-vue-next'
import SheetGrid from '../components/SheetGrid.vue'
import EntryModeToggle from '../components/EntryModeToggle.vue'
import ClutchesCards from '../components/clutches/ClutchesCards.vue'
import ClutchDayPanel from '../components/clutches/ClutchDayPanel.vue'
import { useEntryMode } from '../composables/useEntryMode'
import { useSheet } from '../composables/useSheet'
import { isBlank } from '../lib/cells'
import { nextClutch } from '../lib/clutches'
import { isoToSerial, todayIso } from '../lib/dates'
import { notify } from '../lib/notice'
import { persistentRef } from '../lib/persist'
import { orderColumns } from '../lib/rows'
import { usePending } from '../stores/pending'
import { t } from '../lib/i18n'

/**
 * "Clutches": new clutches (eggs laid) and their follow-up in Insectary_stocks.
 * Two ways to work (useEntryMode): cards for the round (components/clutches,
 * the default on every device), or the table, where hatching and pupation are
 * typed straight into the clutch's row; the clutches still in progress come
 * first. «Historial» shows the day's changes (to copy and to undo) over the table.
 */
const MODULE = 'Insectary_stocks'
const module = ref(MODULE)
const pending = usePending()
const { table, ready, options, creates, createFormulas, listColumn } = useSheet(module)
const { mode } = useEntryMode('clutches')
/** People's initials for the notes they add ("FCH - Franz Chandi"). */
const collectors = computed(() => listColumn('Abbr_name'))

const form = persistentRef('clutches:form', { species: '', date: todayIso(), eggs: null as number | null, place: 'Insectary' })
const number = ref('')
const showAll = ref(false)
const showHistory = ref(false)

const rows = computed(() => table.value?.rows.filter(r => r.observed) || [])
/** Clutch numbers are integers, sometimes with a batch suffix like "994(6)"; clutches added but not yet saved count too. */
const nextNumber = computed(() => nextClutch([...rows.value, ...creates.value].map(r => String(r.values['CLUTCH NUMBER'] ?? ''))))
const speciesList = computed(() => {
  const counts = new Map<string, number>()
  for (const r of rows.value.slice(-300))
    if (!isBlank(r.values.SPECIES)) counts.set(String(r.values.SPECIES), (counts.get(String(r.values.SPECIES)) || 0) + 1)
  return [...new Set([...[...counts].sort((a, b) => b[1] - a[1]).map(([s]) => s), ...(options.value.SPECIES || [])])]
})

function addClutch() {
  const clutch = (number.value || nextNumber.value).trim()
  if (!form.value.species) return notify(t('Elige la especie del clutch'))
  if (
    rows.value.some(r => String(r.values['CLUTCH NUMBER']) === clutch) ||
    creates.value.some(c => String(c.values['CLUTCH NUMBER']) === clutch)
  )
    return notify(t('El clutch {clutch} ya existe', { clutch }), 'error')
  const values: Record<string, string | number | null> = {
    'CLUTCH NUMBER': /^\d+$/.test(clutch) ? Number(clutch) : clutch,
    SPECIES: form.value.species,
    'DATE LAID': form.value.date ? isoToSerial(form.value.date) : null,
    'NUMBER OF EGGS': form.value.eggs,
    'INSECTARY OR LABORATORY': form.value.place,
  }
  for (const field of createFormulas.value) delete values[field]
  pending.addCreate(MODULE, clutch, values)
  pending.touch()
  number.value = ''
  form.value.eggs = null
  notify(t('Clutch {clutch} añadido', { clutch }))
}

/** In progress: laid in the last 60 days and not yet emerged. */
const inProgress = computed(() => {
  const since = isoToSerial(new Date(Date.now() - 60 * 864e5).toISOString().slice(0, 10))
  const recent = rows.value.filter(r => {
    const laid = r.values['DATE LAID']
    return typeof laid === 'number' ? laid >= since : false
  })
  return showAll.value ? rows.value.slice(-150) : recent.filter(r => isBlank(r.values['EMERGENCE DATE']))
})
const columns = computed(() =>
  table.value
    ? orderColumns(table.value.columns, [
        'CLUTCH NUMBER',
        'SPECIES',
        'DATE LAID',
        'NUMBER OF EGGS',
        'INSECTARY OR LABORATORY',
        'HATCHING DATE',
        'NUMBER OF LARVAE',
        'PUPA DATE',
        'NUMBER OF PUPA',
        'Earliest Emerge Date',
        'Number of Adults in Insectary_data',
        'NOTES',
      ])
    : [],
)
</script>

<template>
  <ClutchesCards
    v-if="mode === 'cards'"
    v-model:mode="mode"
    :table="table"
    :ready="ready"
    :options="options"
    :species="speciesList"
    :collectors="collectors"
    :create-formulas="createFormulas"
  />
  <div v-else class="flex h-full flex-col">
    <form class="toolbar" @submit.prevent="addClutch">
      <label>
        <span class="field-label">Clutch</span>
        <input v-model="number" class="field-input w-28" :placeholder="nextNumber" />
      </label>
      <label class="min-w-72">
        <span class="field-label">{{ $t('Especie') }}</span>
        <ChoiceField v-model="form.species" class="field-input" :options="speciesList" />
      </label>
      <label>
        <span class="field-label">{{ $t('Puesta') }}</span>
        <DateField v-model="form.date" class="field-input" />
      </label>
      <label>
        <span class="field-label">{{ $t('Huevos') }}</span>
        <input v-model.number="form.eggs" type="number" min="0" class="field-input w-24" />
      </label>
      <label>
        <span class="field-label">{{ $t('Dónde') }}</span>
        <ChoiceField v-model="form.place" class="field-input" :options="['Insectary', 'Laboratory']" :freetext="false" />
      </label>
      <button class="btn-primary"><Plus :size="15" /> {{ $t('Nuevo clutch') }}</button>
      <button type="button" class="btn ml-auto self-end" :title="$t('Historial de Clutches')" @click="showHistory = true">
        <History :size="15" /> {{ $t('Historial') }}
      </button>
      <EntryModeToggle v-model="mode" class="self-end" />
    </form>
    <p class="hint px-4 py-1">
      {{
        $t(
          'Eclosión y pupa: escribe la fecha y el número en la fila del clutch. Se muestran los clutches de los últimos 60 días que aún no emergen.',
        )
      }}
      <label class="ml-2"><input v-model="showAll" type="checkbox" /> {{ $t('ver los últimos 150') }}</label>
    </p>
    <div class="min-h-0 flex-1">
      <p v-if="!ready" class="p-6 text-stone-500">{{ $t('Cargando {sheet}…', { sheet: 'Insectary_stocks' }) }}</p>
      <SheetGrid
        v-else
        :module="MODULE"
        :rows="inProgress"
        :creates="creates"
        :columns="columns"
        :options="options"
        :frozen="['CLUTCH NUMBER']"
        :create-formulas="createFormulas"
        label-field="CLUTCH NUMBER"
        @notice="notify"
        @remove-create="
          id => {
            pending.removeCreate(id)
            pending.touch()
          }
        "
      />
    </div>
    <ClutchDayPanel v-if="showHistory" :collectors="collectors" @close="showHistory = false" />
  </div>
</template>
