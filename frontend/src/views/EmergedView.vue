<script setup lang="ts">
import TabHistoryButton from '../components/history/TabHistoryButton.vue'
import ChoiceField from '../components/ChoiceField.vue'
import DateField from '../components/DateField.vue'
import EntryModeToggle from '../components/EntryModeToggle.vue'
import EmergedCards from '../components/emerged/EmergedCards.vue'
import { computed, ref, watch } from 'vue'
import { LayoutGrid, Rows3, Plus } from 'lucide-vue-next'
import SheetGrid from '../components/SheetGrid.vue'
import InsectaryIdsWarning from '../components/InsectaryIdsWarning.vue'
import { heldIds, useEmergedState } from '../composables/useEmergedState'
import { useEntryMode } from '../composables/useEntryMode'
import { useSheet } from '../composables/useSheet'
import { isoToSerial } from '../lib/dates'
import { knownSpecies, siblingSpecies, stockOrigin, isHybrid, ADULT, CROSS_PURPOSE } from '../lib/emerged'
import { notify } from '../lib/notice'
import { orderColumns } from '../lib/rows'
import type { CellValue } from '../lib/types'
import { usePending } from '../stores/pending'
import { t } from '../lib/i18n'

/**
 * "Registrar Emergidos": new adults from a clutch go into the next unused
 * pre-filled rows of Insectary_data. SPECIES is a formula there that predicts
 * the species from the clutch; each butterfly shows that prediction and can be
 * changed to another subspecies of the same species when that is what emerged.
 * Two ways to work (useEntryMode): cards (components/emerged, the default on
 * every device: a card per butterfly, one Save that also counts the adults in
 * the clutch's row) or this table, where a batch of rows is prepared and typed
 * in the grid. Both share the day, the clutch and the free IDs (useEmergedState).
 */
const MODULE = 'Insectary_data'
const module = ref(MODULE)
const pending = usePending()
const { table, ready, stocks, options, creates, createFormulas, clutches, listColumn } = useSheet(module)
const { mode } = useEntryMode('emerged')
/** People's initials for the notes the cards add ("FCH - Franz Chandi"). */
const collectors = computed(() => listColumn('Abbr_name'))
const state = useEmergedState()
const clutch = state.clutch
const introDate = state.date
// Counts and the first ID are for one batch: kept from an earlier day they sent a
// batch to an old empty row (A0D, row 13243) with an extra "sin sexo" butterfly.
const females = ref(0)
const males = ref(0)
const unknown = ref(0)
const startId = ref('')
/** The cards not saved yet hold their IDs: the table never gives them to another row. */
const held = computed(() => new Set(heldIds(state.drafts.value)))
/** Free pre-made IDs: those after the last row used first (the suggestion), then earlier empty rows. */
const freeIds = computed(() => state.freeIds.value.filter(id => !held.value.has(id.toUpperCase())))
/** The same IDs in sheet order, which a batch follows from its first ID (H0B → H1B → H2B). */
const inOrder = computed(() => state.inOrder.value.filter(id => !held.value.has(id.toUpperCase())))
/** Sheet row of each free pre-made ID. */
const rowOf = state.rowOf
const idsLoaded = state.idsLoaded
const recentCount = ref(15)
watch(freeIds, ids => {
  if (!startId.value || !ids.includes(startId.value)) startId.value = ids[0] || ''
}, { immediate: true })
const loadFreeIds = () => state.loadFreeIds()

/** Species recorded for the clutch in Insectary_stocks (what the SPECIES formula will show). */
const species = computed(() => {
  const row = stocks.value?.rows.find(r => String(r.values['CLUTCH NUMBER']).trim() === clutch.value)
  return row ? knownSpecies(row.values.SPECIES) : ''
})

/** Other subspecies of the clutch's species: what may emerge instead (e.g. eurydice from a proceriformis clutch). */
const siblings = computed(() =>
  siblingSpecies(species.value, (table.value?.rows || []).map(r => String(r.values.SPECIES ?? ''))),
)
const gridOptions = computed(() => ({ ...options.value, SPECIES: siblings.value }))

/** Same rules as the Shiny app's apply_row_core_defaults, without the formula columns (the sheet fills those). */
function defaultsFor(speciesName: string): Record<string, CellValue> {
  const values: Record<string, CellValue> = { Wild_Reared: 'Reared', Stock_of_origin: stockOrigin(speciesName) }
  if (isHybrid(speciesName)) values.Research_purpose = CROSS_PURPOSE
  for (const field of createFormulas.value) if (field !== 'SPECIES') delete values[field]
  return values
}

function prepare(sexes: (string | null)[]) {
  if (!clutch.value) return notify(t('Elige el clutch'))
  if (!sexes.length) return notify(t('Indica cuántas hembras, machos o sin sexo emergieron'))
  if (!inOrder.value.length)
    return notify(t('No quedan filas preasignadas libres: crea más filas preasignadas en Insectary_data'), 'error')
  const start = inOrder.value.indexOf(startId.value.trim().toUpperCase())
  if (start < 0)
    return notify(
      t('{id} no es una fila preasignada libre de Insectary_data: elige uno de la lista', { id: startId.value || t('Ese ID') }),
    )
  const ids = inOrder.value.slice(start, start + sexes.length)
  if (ids.length < sexes.length)
    notify(
      t('Solo hay {n} filas preasignadas libres desde {id}: crea más filas preasignadas en Insectary_data', {
        n: ids.length,
        id: ids[0],
      }),
      'error',
    )
  ids.forEach((id, i) =>
    pending.addCreate(MODULE, id, {
      Insectary_ID: id,
      'CLUTCH NUMBER': clutch.value,
      // The clutch's prediction; change it only if another subspecies emerged.
      SPECIES: species.value || null,
      Sex: sexes[i],
      Intro2Insectary_date: introDate.value ? isoToSerial(introDate.value) : null,
      // An entry date: an adult (team rule, 5 Oct 2026).
      ...(introDate.value ? { LIFESTAGE: ADULT } : {}),
      ...defaultsFor(species.value),
    }),
  )
  startId.value = freeIds.value.find(id => !ids.includes(id)) || ''
  females.value = males.value = unknown.value = 0
  pending.touch()
  notify(
    t('{n} filas nuevas ({first}–{last}); revisa la subespecie si alguna es distinta', {
      n: ids.length,
      first: ids[0],
      last: ids.at(-1),
    }),
  )
}
/** The first ID chosen is an empty row earlier in the sheet, not the next one after the last used. */
const earlierRow = computed(() => {
  const id = startId.value.trim().toUpperCase()
  const next = freeIds.value[0]
  if (!id || !next || id === next || !rowOf.value.has(id)) return null
  return (rowOf.value.get(id) ?? 0) < (rowOf.value.get(next) ?? 0) ? { id, row: rowOf.value.get(id), next } : null
})
const batch = () => [
  ...Array(Math.max(0, females.value)).fill('female'),
  ...Array(Math.max(0, males.value)).fill('male'),
  ...Array(Math.max(0, unknown.value)).fill(null),
]

const columns = computed(() =>
  table.value
    ? orderColumns(table.value.columns, [
        'Insectary_ID',
        'CLUTCH NUMBER',
        'Sex',
        'Intro2Insectary_date',
        'SPECIES',
        'Wild_Reared',
        'Stock_of_origin',
        'LIFESTAGE',
        'Research_purpose',
        'Pedigree',
        'Notes_Insectary_data',
      ])
    : [],
)
/**
 * Below the new rows: the butterflies already recorded from the chosen clutch
 * (wherever they are in the sheet, so a saved batch never drops out of sight
 * and is not entered twice) and the latest rows.
 */
const ofClutch = computed(() =>
  clutch.value && table.value
    ? table.value.rows.filter(r => r.observed && String(r.values['CLUTCH NUMBER'] ?? '') === clutch.value)
    : [],
)
const recent = computed(() => {
  if (!table.value) return []
  const latest = table.value.rows.filter(r => r.observed).slice(-recentCount.value)
  return [...new Set([...latest, ...ofClutch.value])].sort((a, b) => a.row - b.row)
})
</script>

<template>
  <EmergedCards
    v-if="mode === 'cards'"
    v-model:mode="mode"
    :table="table"
    :stocks="stocks"
    :ready="ready"
    :options="options"
    :collectors="collectors"
    :create-formulas="createFormulas"
  />
  <div v-else class="flex h-full flex-col">
    <div class="toolbar">
      <label class="min-w-40">
        <span class="field-label">CLUTCH NUMBER</span>
        <ChoiceField
          v-model="clutch"
          class="field-input"
          :options="clutches"
          :placeholder="$t('p. ej. {example}', { example: '994(6)' })"
        />
      </label>
      <label>
        <span class="field-label">{{ $t('Hembras') }}</span>
        <input v-model.number="females" type="number" min="0" max="100" class="field-input w-20" />
      </label>
      <label>
        <span class="field-label">{{ $t('Machos') }}</span>
        <input v-model.number="males" type="number" min="0" max="100" class="field-input w-20" />
      </label>
      <label>
        <span class="field-label">{{ $t('Sin sexo') }}</span>
        <input v-model.number="unknown" type="number" min="0" max="100" class="field-input w-20" />
      </label>
      <label>
        <span class="field-label">{{ $t('Insectary ID inicial') }}</span>
        <!-- Any free pre-made row can start the batch (earlier empty rows too); type to search. -->
        <ChoiceField
          v-model="startId"
          class="field-input w-32 uppercase"
          :options="freeIds"
          :placeholder="freeIds.length ? '' : $t('no quedan')"
          :title="
            freeIds.length
              ? $t('{n} filas preasignadas libres', { n: freeIds.length })
              : $t('Crea más filas preasignadas en Insectary_data')
          "
          @focus="($event.target as HTMLInputElement).select()"
        />
      </label>
      <label>
        <span class="field-label">{{ $t('Intro a insectario') }}</span>
        <DateField v-model="introDate" class="field-input" />
      </label>
      <div class="flex gap-2">
        <button class="btn-primary" @click="prepare(batch())">
          <Rows3 :size="15" /> {{ batch().length ? $t('Preparar {n} filas', { n: batch().length }) : $t('Preparar filas') }}
        </button>
        <button class="btn" @click="prepare([null])"><Plus :size="15" /> {{ $t('Añadir una') }}</button>
      </div>
      <TabHistoryButton class="ml-auto self-end" purpose="emergidos" :title="$t('Historial de Emergidos')" />
      <EntryModeToggle v-model="mode" class="self-end" />
    </div>
    <!-- Cards not saved yet hold their IDs; they are saved from the cards (with the clutch's count). -->
    <p v-if="state.drafts.value.length" class="mx-4 mt-2 flex flex-wrap items-center gap-2 rounded-md border border-amber-300 bg-amber-50 px-3 py-2 text-sm text-amber-950">
      {{
        $tn(state.drafts.value.length, '{n} emergido en tarjetas sin guardar ({ids}).', '{n} emergidos en tarjetas sin guardar ({ids}).', {
          ids: heldIds(state.drafts.value).join(', '),
        })
      }}
      <button class="btn h-9" @click="mode = 'cards'"><LayoutGrid :size="15" /> {{ $t('Ver las tarjetas') }}</button>
    </p>
    <InsectaryIdsWarning class="mx-4 mt-2" :revision="table?.revision" @extended="loadFreeIds" />
    <p class="hint px-4 py-1">
      <template v-if="clutch"
        >{{ $t('Especie del clutch:') }} <strong>{{ species || $t('no encontrada') }}</strong
        >. {{ $t('Si una mariposa emergió de otra subespecie, cámbiala en su fila')
        }}<template v-if="siblings.length > 1">
          ({{
            siblings
              .map(s => s.split(' ').slice(2).join(' '))
              .filter(Boolean)
              .join(', ')
          }})</template
        >.
      </template>
      <strong v-if="idsLoaded && !freeIds.length" class="text-amber-800">{{
        $t('No quedan filas preasignadas libres: crea más filas preasignadas en Insectary_data.')
      }}</strong>
      <strong v-if="earlierRow" class="text-amber-800">{{
        $t('{id} es una fila vacía más arriba en la hoja (fila {row}), no la siguiente ({next}).', {
          id: earlierRow.id,
          row: earlierRow.row,
          next: earlierRow.next,
        })
      }}</strong>
      {{
        ofClutch.length
          ? $t(
              'Las filas nuevas usan las filas preasignadas; escribe cada ID en las alas. Debajo se muestran los {n} ya registrados del clutch {clutch} y los últimos {recent} registros.',
              { n: ofClutch.length, clutch, recent: recentCount },
            )
          : $t(
              'Las filas nuevas usan las filas preasignadas; escribe cada ID en las alas. Debajo se muestran los últimos {recent} registros.',
              { recent: recentCount },
            )
      }}
      <button class="underline" @click="recentCount += 15">{{ $t('ver más') }}</button>
    </p>
    <div class="min-h-0 flex-1">
      <p v-if="!ready" class="p-6 text-stone-500">{{ $t('Cargando {sheet}…', { sheet: 'Insectary_data' }) }}</p>
      <SheetGrid
        v-else
        :module="MODULE"
        :rows="recent"
        :creates="creates"
        :columns="columns"
        :options="gridOptions"
        :frozen="['Insectary_ID']"
        :locked-fields="['Collection_location']"
        :create-formulas="createFormulas.filter(f => f !== 'Insectary_ID' && f !== 'SPECIES')"
        label-field="Insectary_ID"
        @notice="notify"
        @remove-create="
          id => {
            pending.removeCreate(id)
            pending.touch()
            loadFreeIds()
          }
        "
      />
    </div>
  </div>
</template>
