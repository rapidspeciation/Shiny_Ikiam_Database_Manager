<script setup lang="ts">
import ChoiceField from '../components/ChoiceField.vue'
import DateField from '../components/DateField.vue'
import { computed, ref } from 'vue'
import { PenLine } from 'lucide-vue-next'
import IdPicker from '../components/IdPicker.vue'
import SheetGrid from '../components/SheetGrid.vue'
import { useSheet } from '../composables/useSheet'
import { isBlank } from '../lib/cells'
import { dayLabel, formatSerial, serialFromIso, todayIso } from '../lib/dates'
import { notify } from '../lib/notice'
import { persistentRef } from '../lib/persist'
import { fillIfBlank, orderColumns, rowsById } from '../lib/rows'
import { usePending } from '../stores/pending'
import { t } from '../lib/i18n'

/**
 * "Registrar Muertes": the IDs typed show their rows at once (to look at them),
 * and "Escribir" puts the death date and cause in those rows' empty cells.
 * The latest recorded deaths are a separate table below.
 */
const MODULE = 'Insectary_data'
const module = ref(MODULE)
const pending = usePending()
const { table, ready, options } = useSheet(module)

/**
 * What the team writes for a butterfly that was not preserved (Unknown,
 * Disappearance, Eaten…), as in every such row of 2026: no CAM, no tubes,
 * media NOT_COLLECTED.
 */
const NOT_PRESERVED: Record<string, string> = {
  Preserved_Dead_Alive: 'NA',
  CAM_ID: 'NA',
  Tube_1_id: 'NA',
  Tube_1_tissue: 'NA',
  T1_Preservation_medium: 'NOT_COLLECTED',
  Tube_2_id: 'NA',
  Tube_2_tissue: 'NA',
  T2_Preservation_medium: 'NOT_COLLECTED',
  Tube_3_id: 'NA',
  Tube_3_tissue: 'NA',
  Tube_4_id: 'NA',
  Tube_4_tissue: 'NA',
  Preservation_medium: 'NOT_COLLECTED',
  Preservation_date: 'NA',
  Location_body: 'NA',
}

const picked = persistentRef<string[]>('deaths:picked', [])
// Lists kept from when the IDs had to be loaded with a button.
const oldLoaded = persistentRef<string[]>('deaths:loaded', [])
if (oldLoaded.value.length) {
  if (!picked.value.length) picked.value = oldLoaded.value
  oldLoaded.value = []
}
// Deaths are usually entered the same day: today by default, with its weekday shown.
const date = ref(todayIso())
const cause = persistentRef('deaths:cause', '')
const notPreserved = persistentRef('deaths:not-preserved', true)
const recentCount = ref(30)

const ids = computed(() => {
  if (!table.value) return []
  const out: string[] = []
  for (const row of table.value.rows) if (row.observed && row.values.Insectary_ID) out.push(String(row.values.Insectary_ID))
  return [...new Set(out)].reverse()
})
const chosenRows = computed(() => (table.value ? rowsById(table.value.rows, 'Insectary_ID', picked.value) : []))
/** The latest recorded deaths, newest death date first, so the tab never opens empty. */
const recentDeaths = computed(() => {
  if (!table.value) return []
  const chosen = new Set(chosenRows.value.map(r => r.id))
  return table.value.rows
    .filter(r => r.observed && typeof r.values.Death_date === 'number' && !chosen.has(r.id))
    .sort((a, b) => (b.values.Death_date as number) - (a.values.Death_date as number) || b.row - a.row)
    .slice(0, recentCount.value)
})
const columns = computed(() =>
  table.value
    ? orderColumns(table.value.columns, [
        'Insectary_ID',
        'Death_date',
        'Death_cause',
        'Notes_Insectary_data',
        'CLUTCH NUMBER',
        'SPECIES',
        'Sex',
      ])
    : [],
)

const dateError = computed(() =>
  date.value && serialFromIso(date.value) === null ? t('Fecha no válida: el año debe estar entre 1990 y 2099') : '',
)

/** A butterfly already recorded dead is probably a mistyped ID (B9 of 2022 instead of B9D). */
function warn(id: string): string | null {
  const row = table.value ? rowsById(table.value.rows, 'Insectary_ID', [id])[0] : undefined
  const death = row?.values.Death_date
  return typeof death === 'number'
    ? t('{id} ya murió el {date} ({cause})', {
        id,
        date: formatSerial(death),
        cause: String(row!.values.Death_cause ?? t('sin causa')),
      })
    : null
}

/** Chosen rows whose date or cause is still empty: what "Escribir" would fill. */
const toWrite = computed(() =>
  chosenRows.value.filter(
    r => isBlank(pending.value(r, 'Death_date')) || (cause.value && isBlank(pending.value(r, 'Death_cause'))),
  ),
)

/** Writes the date and cause in the chosen rows' empty cells (as pending edits, to review and save). */
function write() {
  if (!table.value || !chosenRows.value.length) return notify(t('Escribe al menos un Insectary ID'))
  if (dateError.value) return notify(dateError.value, 'error')
  if (!date.value) return notify(t('Elige la fecha de muerte'), 'error')
  const serial = serialFromIso(date.value)
  let filled = 0
  for (const row of chosenRows.value) {
    const label = String(row.values.Insectary_ID)
    const set = (field: string, value: string | number) => {
      if (fillIfBlank(MODULE, row, label, field, value)) filled++
    }
    if (serial !== null) set('Death_date', serial)
    if (cause.value) set('Death_cause', cause.value)
    // Not preserved: only rows without a CAM or tube yet (a preserved one keeps its IDs).
    const why = pending.value(row, 'Death_cause')
    if (
      notPreserved.value &&
      !isBlank(why) &&
      why !== 'Killed_Preserved' &&
      isBlank(pending.value(row, 'CAM_ID')) &&
      isBlank(pending.value(row, 'Tube_1_id'))
    )
      for (const [field, value] of Object.entries(NOT_PRESERVED)) set(field, value)
  }
  pending.touch()
  notify(
    filled
      ? t('{n} celdas escritas; revisa y guarda', { n: filled })
      : t('Nada que escribir: esas filas ya tienen fecha y causa'),
  )
}
</script>

<template>
  <div class="flex h-full flex-col">
    <div class="toolbar">
      <IdPicker v-model="picked" :options="ids" :loading="!ready" :warn="warn" label="Insectary IDs" />
      <label>
        <span class="field-label">{{ $t('Fecha de muerte') }}</span>
        <DateField v-model="date" class="field-input" />
        <span v-if="dateError" class="block text-xs text-red-700">{{ dateError }}</span>
        <span v-else-if="date" class="block text-xs text-stone-600">{{ dayLabel(date) }}</span>
        <span v-else class="block text-xs text-amber-800">{{ $t('Elige la fecha') }}</span>
      </label>
      <label class="min-w-44">
        <span class="field-label">{{ $t('Causa por defecto') }}</span>
        <ChoiceField
          v-model="cause"
          class="field-input"
          :options="options.Death_cause || []"
          :placeholder="$t('p. ej. {example}', { example: 'Natural' })"
        />
      </label>
      <label
        class="flex max-w-64 items-center gap-2 pb-1.5 text-xs"
        :title="
          $t(
            'Para causas distintas de Killed_Preserved y filas sin CAM ni tubo: CAM, tubos, tejidos, Preservation_date, Location_body y Preserved_Dead_Alive en NA; medios en NOT_COLLECTED',
          )
        "
      >
        <input v-model="notPreserved" type="checkbox" /> {{ $t('Sin preservar: CAM y tubos NA, medios NOT_COLLECTED') }}
      </label>
      <button
        class="btn-primary"
        :disabled="!chosenRows.length"
        :title="
          chosenRows.length
            ? $t('Escribe la fecha y la causa en las celdas vacías de los {n} IDs elegidos', { n: chosenRows.length })
            : $t('Escribe primero los Insectary IDs')
        "
        @click="write"
      >
        <PenLine :size="15" /> {{ $t('Escribir fecha y causa')
        }}<template v-if="chosenRows.length"> ({{ chosenRows.length }})</template>
      </button>
    </div>
    <div class="flex min-h-0 flex-1 flex-col">
      <p v-if="!ready" class="p-6 text-stone-500">{{ $t('Cargando {sheet}…', { sheet: 'Insectary_data' }) }}</p>
      <template v-else>
        <section v-if="chosenRows.length" class="flex max-h-[45%] shrink-0 flex-col border-b-4 border-stone-200">
          <p class="hint px-4 py-1">
            <strong>{{ $t('IDs elegidos ({n})', { n: chosenRows.length }) }}</strong>
            <template v-if="toWrite.length">
              ·
              {{
                $t('{n} sin fecha o causa: «Escribir fecha y causa» las completa (solo celdas vacías o NA).', {
                  n: toWrite.length,
                })
              }}</template
            >
            <template v-else> · {{ $t('ya tienen fecha y causa.') }}</template>
          </p>
          <SheetGrid
            :module="MODULE"
            :rows="chosenRows"
            :columns="columns"
            :options="options"
            :frozen="['Insectary_ID']"
            :header-filters="false"
            :newest-first="false"
            :height="`${Math.min(chosenRows.length, 10) * 2.25 + 2.5}rem`"
            label-field="Insectary_ID"
            @notice="notify"
          />
        </section>
        <p class="hint px-4 py-1">
          <strong>{{ $t('Últimas {n} muertes registradas', { n: recentDeaths.length }) }}</strong>
          <button class="ml-1 underline" @click="recentCount += 30">{{ $t('ver más') }}</button>
        </p>
        <p v-if="!recentDeaths.length" class="p-6 text-stone-500">{{ $t('No hay muertes registradas.') }}</p>
        <div v-else class="min-h-0 flex-1">
          <SheetGrid
            :module="MODULE"
            :rows="recentDeaths"
            :columns="columns"
            :options="options"
            :frozen="['Insectary_ID']"
            :header-filters="false"
            :newest-first="false"
            label-field="Insectary_ID"
            @notice="notify"
          />
        </div>
      </template>
    </div>
  </div>
</template>
