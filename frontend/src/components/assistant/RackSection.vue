<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { AlertTriangle, Check, QrCode, RotateCcw } from 'lucide-vue-next'
import { api } from '../../lib/api'
import { errorText } from '../../lib/notice'
import { COLS, FIELD_LABELS, ROWS, cellCounts, conflictText, problemText, rackCells, splitTube, type CellState, type DepositRack, type RackCell } from '../../lib/rackDeposit'
import type { Proposal, ProposalChange } from '../../lib/proposals'
import { t, tn } from '../../lib/i18n'

/**
 * One rack of a deposit (lib/rackDeposit), as the LBI's deposit sheet (Anexo1) lays it out: the freezer,
 * shelf, rack code, rack row and box on the left, the 8 × 12 grid of tube IDs on the right (row A at the top,
 * column 1 at the left, as seen from above), so the person checks it against the rack in hand. Each well
 * coloured by what the proposal does there: a new row of the tube-locations sheet, its rack or well changed,
 * already so, empty, or still without a tube. The corner the rack's code sits beside is assumed (A12 on the
 * team's racks) until the person confirms it or turns the rack. A click on a well shows it below the grid;
 * a well without a tube takes one (its side label) or is said empty. The fields and the wells are saved at
 * once (POST chat/proposals/<id>/rack): the grid and the rows follow.
 */
const props = defineProps<{
  proposalId: string
  rack: DepositRack
  /** Its number in the deposit (from 1): the server's name for it. */
  number: number
  freezer: string
  changes: ProposalChange[]
  editable: boolean
}>()
const emit = defineEmits<{ replace: [proposal: Proposal]; notice: [message: string] }>()

const cells = computed(() => rackCells(props.rack, props.changes))
const counts = computed(() => cellCounts(cells.value))
const busy = ref(false)
const selected = ref<string | null>(null)
const typed = ref('')
watch(selected, () => (typed.value = ''))
const chosen = computed<RackCell | null>(() => (selected.value ? (cells.value.get(selected.value) ?? null) : null))

/** A change of this rack (its fields, wells, corner, a conflict); `deposit`: of the whole deposit instead. */
async function save(body: Record<string, unknown>, deposit = false) {
  if (!props.editable || busy.value) return
  busy.value = true
  try {
    const out = await api<{ proposal: Proposal }>(`chat/proposals/${props.proposalId}/rack`, {
      method: 'POST',
      body: deposit ? body : { racks: [{ rack: String(props.number), ...body }] },
    })
    emit('replace', out.proposal)
  } catch (e) {
    emit('notice', errorText(e))
  } finally {
    busy.value = false
  }
}
const FIELDS = ['estanteria', 'fila_de_rack', 'caja'] as const
/** Anexo1's order: freezer, shelf, rack, rack row, box. */
const FIELD_ROWS = ['estanteria', 'rack', 'fila_de_rack', 'caja'] as const
const fieldValue = (f: (typeof FIELDS)[number]) => props.rack[f] ?? ''
function onField(f: (typeof FIELDS)[number], event: Event) {
  const value = (event.target as HTMLInputElement).value.trim()
  if (value !== fieldValue(f)) save({ [f]: value })
}
function onRackId(event: Event) {
  const value = (event.target as HTMLInputElement).value.trim().toUpperCase()
  if (value && value !== props.rack.rack_id) save({ rack_id: value })
}
const TUBE = /^[A-Z]{2}\d{6,10}$/
const typedOk = computed(() => TUBE.test(typed.value.trim().toUpperCase()))
function fill() {
  if (!selected.value || !typedOk.value) return
  save({ fill: { [selected.value]: typed.value.trim().toUpperCase() } })
}
const other = computed(() => props.rack.code_options?.find(w => w !== props.rack.code_beside) ?? null)

const STATE: Record<CellState, { label: string; tip: string }> = {
  new: { label: 'nuevo', tip: 'Fila nueva en la hoja de ubicaciones: el tubo, el rack y el pocillo' },
  moved: { label: 'cambia', tip: 'La hoja lo tiene en otro rack o pocillo: se cambia al de las fotos' },
  same: { label: 'igual', tip: 'La hoja ya lo tiene aquí: no cambia' },
  check: { label: 'revisar', tip: 'El tubo está en dos filas de la hoja: no se cambia, revísalo' },
  empty: { label: 'vacío', tip: 'Pocillo vacío (dicho por la persona)' },
  missing: { label: 'falta', tip: 'Ninguna foto leyó este tubo: escribe su número (etiqueta lateral) o márcalo vacío' },
  left: { label: 'quitado', tip: 'Su fila se quitó de la propuesta: no se escribe' },
  off: { label: 'otro sitio', tip: 'Su fila en la tabla lo pone en otro rack o pocillo que las fotos: revísala' },
  conflict: {
    label: 'conflicto',
    tip: 'Una fila mueve aquí un tubo, o lo saca de aquí, pero el pocillo ya tiene otro: nada se sobrescribe; resuélvelo aquí',
  },
}
const legend = computed(() => (Object.keys(STATE) as CellState[]).filter(s => counts.value[s] > 0 || ['new', 'moved', 'same', 'missing'].includes(s)))
const occupied = computed(() => Object.keys(props.rack.sheet_here ?? {}).length)
const placeText = (p: { rack: string | null; well: string | null }) => `${p.rack ?? '—'} ${p.well ?? '—'}`
const codeCorner = computed(() => props.rack.code_beside)
const cellTitle = (c: RackCell) =>
  [
    `${c.well}${c.tube ? ` · ${c.tube}` : ''}`,
    t(STATE[c.state].tip),
    ...c.was.map(w => t('En la hoja: {rack} {well}', { rack: w.rack ?? '—', well: w.well ?? '—' })),
    c.rowPlace ? t('La fila de la tabla dice: {place}', { place: placeText(c.rowPlace) }) : '',
    c.sheetHere ? t('La hoja tiene aquí otro tubo: {tube}; revisa la fila de ese tubo', { tube: c.sheetHere }) : '',
    c.needsRows ? t('Sin fila en ninguna hoja del libro: necesita su fila') : '',
    c.byHand ? t('Escrito a mano (etiqueta lateral)') : '',
    c.disputed ? t('Las fotos no coinciden en este pocillo') : '',
    c.conflict ? conflictText(c.conflict) : '',
  ]
    .filter(Boolean)
    .join('\n')
const wasText = (c: RackCell) => c.was.map(w => `${w.rack ?? '—'} ${w.well ?? '—'}`).join(' · ')
</script>

<template>
  <section class="rack" :data-rack="rack.reading" :aria-label="$t('Rack {code}', { code: rack.rack_id ?? '?' })">
    <header class="rack-head">
      <h3 class="flex items-center gap-1.5 text-sm font-semibold text-stone-900">
        <span class="font-mono">{{ rack.rack_id ?? $t('Rack sin código') }}</span>
      </h3>
      <span class="text-[11px] text-stone-500">{{
        $tn(rack.tubes_read ?? 0, '{n} tubo leído de las fotos', '{n} tubos leídos de las fotos')
      }}</span>
      <!-- Which corner the code sits beside: assumed (the team's racks) until confirmed. -->
      <span
        v-if="rack.code_beside"
        class="corner"
        :class="rack.assumed ? 'is-assumed' : 'is-confirmed'"
        :title="
          rack.assumed
            ? $t('Supuesto: en los racks del equipo el código está junto a A12. Míralo desde arriba y confírmalo')
            : $t('Confirmado por la persona')
        "
      >
        <QrCode :size="12" />
        {{ $t('Código junto a {well}', { well: rack.code_beside }) }}
        <template v-if="rack.assumed"> · {{ $t('supuesto') }}</template>
        <template v-else><Check :size="12" /></template>
        <template v-if="rack.checked_by_ids"> · {{ $t('la secuencia de números de los tubos lo apoya') }}</template>
      </span>
      <span v-if="editable && rack.assumed" class="flex items-center gap-1">
        <button type="button" class="rack-btn" :disabled="busy" @click="save({ confirmed: true })">
          <Check :size="12" /> {{ $t('Confirmar') }}
        </button>
        <button
          v-if="other"
          type="button"
          class="rack-btn"
          :disabled="busy"
          :title="$t('Gira el rack: los pocillos y las filas cambian')"
          @click="save({ code_beside: other })"
        >
          <RotateCcw :size="12" /> {{ $t('Está junto a {well}', { well: other }) }}
        </button>
      </span>
      <button
        v-else-if="editable && other"
        type="button"
        class="rack-btn ml-auto"
        :disabled="busy"
        :title="$t('Gira el rack: los pocillos y las filas cambian')"
        @click="save({ code_beside: other })"
      >
        <RotateCcw :size="12" /> {{ $t('Código junto a {well}', { well: other }) }}
      </button>
    </header>

    <div class="rack-body">
      <!-- Anexo1's left block: one value per column, for the whole rack. -->
      <table class="rack-fields" :aria-label="$t('Datos de la hoja de depósito')">
        <tbody>
          <tr>
            <th>{{ $t('Ultracongelador') }}</th>
            <td class="font-mono text-stone-700" :title="$t('Se cambia arriba, para todo el depósito')">{{ freezer }}</td>
          </tr>
          <tr v-for="f in FIELD_ROWS" :key="f">
            <th>{{ f === 'rack' ? $t('Rack') : $t(FIELD_LABELS[f]) }}</th>
            <td v-if="f === 'rack'">
              <!-- Read from the photo or typed: the person corrects it here (the rows' Rack_ID follows). -->
              <input
                :value="rack.rack_id ?? ''"
                class="font-mono"
                :class="{ 'is-unset': !rack.rack_id }"
                :placeholder="$t('código del rack')"
                :disabled="!editable || busy"
                :aria-label="$t('Código del rack')"
                @change="onRackId"
                @keydown.enter="($event.target as HTMLInputElement).blur()"
              />
            </td>
            <td v-else>
              <input
                :value="fieldValue(f)"
                :class="{ 'is-unset': !fieldValue(f) }"
                :placeholder="$t('falta')"
                :disabled="!editable || busy"
                :aria-label="$t(FIELD_LABELS[f])"
                @change="onField(f, $event)"
                @keydown.enter="($event.target as HTMLInputElement).blur()"
              />
            </td>
          </tr>
        </tbody>
      </table>

      <!-- The grid as seen from above: rows A..H, columns 1..12. -->
      <div class="rack-grid-wrap">
        <table class="rack-grid" :class="{ 'is-busy': busy }">
          <thead>
            <tr>
              <th />
              <th v-for="c in COLS" :key="c" scope="col">{{ c }}</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="r in ROWS" :key="r">
              <th scope="row">{{ r }}</th>
              <td v-for="c in COLS" :key="c">
                <button
                  type="button"
                  class="well"
                  :class="[
                    `is-${cells.get(`${r}${c}`)!.state}`,
                    {
                      'is-selected': selected === `${r}${c}`,
                      'has-code': codeCorner === `${r}${c}`,
                      'needs-rows': cells.get(`${r}${c}`)!.needsRows,
                      'sheet-here': !!cells.get(`${r}${c}`)!.sheetHere,
                      'by-hand': cells.get(`${r}${c}`)!.byHand,
                    },
                  ]"
                  :title="cellTitle(cells.get(`${r}${c}`)!)"
                  :aria-label="cellTitle(cells.get(`${r}${c}`)!)"
                  :aria-pressed="selected === `${r}${c}`"
                  @click="selected = selected === `${r}${c}` ? null : `${r}${c}`"
                >
                  <template v-if="cells.get(`${r}${c}`)!.tube">
                    <span class="well-series">{{ splitTube(cells.get(`${r}${c}`)!.tube)[0] }}</span>
                    <span class="well-digits">{{ splitTube(cells.get(`${r}${c}`)!.tube)[1] }}</span>
                  </template>
                  <span v-else class="well-word">{{ $t(STATE[cells.get(`${r}${c}`)!.state].label) }}</span>
                </button>
              </td>
            </tr>
          </tbody>
        </table>
      </div>
    </div>

    <!-- The legend with the counts. -->
    <p class="rack-legend">
      <span v-for="s in legend" :key="s" class="legend-item" :title="$t(STATE[s].tip)">
        <span class="swatch" :class="`is-${s}`" />{{ $t(STATE[s].label) }} {{ counts[s] }}
      </span>
      <span v-if="occupied" class="legend-item" :title="$t('La hoja tiene en ese pocillo de este rack otro tubo, que las fotos no muestran ahí')">
        <span class="swatch is-sheet-here" />{{ $t('la hoja tiene otro tubo aquí') }} {{ occupied }}
      </span>
      <span v-if="rack.need_rows.length" class="legend-item" :title="$t('Sin fila en ninguna hoja del libro: necesita su fila')">
        <span class="swatch dot" />{{ $t('sin fila en el libro') }} {{ rack.need_rows.length }}
      </span>
    </p>

    <!-- The well clicked: what it is, where the sheet had it, and for a well without a tube, its tube or «vacío». -->
    <div v-if="chosen" class="well-bar" role="status">
      <strong class="font-mono">{{ chosen.well }}</strong>
      <span v-if="chosen.tube" class="font-mono">{{ chosen.tube }}</span>
      <span class="text-stone-600">{{ $t(STATE[chosen.state].tip) }}</span>
      <span v-if="chosen.was.length" class="text-stone-600">· {{ $t('en la hoja') }}: <span class="font-mono">{{ wasText(chosen) }}</span></span>
      <span v-if="chosen.rowPlace" class="text-amber-900">· {{ $t('La fila de la tabla dice: {place}', { place: placeText(chosen.rowPlace) }) }}</span>
      <span v-if="chosen.sheetHere" class="text-amber-900">
        · {{ $t('La hoja tiene aquí otro tubo: {tube}; revisa la fila de ese tubo', { tube: chosen.sheetHere }) }}
      </span>
      <span v-if="chosen.needsRows" class="text-amber-900">· {{ $t('sin fila en ninguna hoja del libro') }}</span>
      <template v-if="editable && (chosen.state === 'missing' || chosen.byHand)">
        <input
          v-model="typed"
          class="well-input font-mono"
          :placeholder="$t('Nº de la etiqueta lateral')"
          :aria-label="$t('Número del tubo de {well}', { well: chosen.well })"
          @keydown.enter="fill"
        />
        <button type="button" class="rack-btn" :disabled="busy || !typedOk" @click="fill">{{ $t('Guardar') }}</button>
        <button v-if="chosen.state === 'missing'" type="button" class="rack-btn" :disabled="busy" @click="save({ empty: [chosen.well] })">
          {{ $t('Vacío') }}
        </button>
        <button v-if="chosen.byHand" type="button" class="rack-btn" :disabled="busy" @click="save({ fill: { [chosen.well]: '' } })">
          {{ $t('Quitar') }}
        </button>
      </template>
      <!-- A row moved a tube onto this well's tube (or from here onto another's): swap them, or keep the photos'. -->
      <template v-if="chosen.conflict">
        <span class="text-red-800">· {{ conflictText(chosen.conflict) }}</span>
        <template v-if="editable">
          <button type="button" class="rack-btn" :disabled="busy" @click="save({ resolve: [{ well: chosen.conflict.to, how: 'swap' }] })">
            {{ $t('Intercambiar: {tube} a {to}, {other} a {from}', { ...chosen.conflict }) }}
          </button>
          <button type="button" class="rack-btn" :disabled="busy" @click="save({ resolve: [{ well: chosen.conflict.to, how: 'keep' }] })">
            {{ $t('Dejar como en las fotos') }}
          </button>
        </template>
      </template>
      <button
        v-if="editable && chosen.state === 'left' && chosen.tube"
        type="button"
        class="rack-btn"
        :disabled="busy"
        @click="save({ restore: [chosen.tube] }, true)"
      >
        {{ $t('Volver a proponer su fila') }}
      </button>
      <button
        v-if="editable && chosen.state === 'empty'"
        type="button"
        class="rack-btn"
        :disabled="busy"
        @click="save({ not_empty: [chosen.well] })"
      >
        {{ $t('No está vacío') }}
      </button>
    </div>

    <!-- What the person should hear: tubes needing their rows, the sheet's tubes the photos do not show here, problems. -->
    <ul
      v-if="rack.need_rows.length || rack.elsewhere.length || rack.duplicates.length || rack.problems.length || rack.conflicts?.length"
      class="rack-notes"
    >
      <li v-for="c in rack.conflicts ?? []" :key="c.to" class="is-conflict">
        <AlertTriangle :size="12" /> {{ conflictText(c) }}
        <button type="button" class="rack-link" @click="selected = c.to">{{ $t('Ver') }}</button>
      </li>
      <li v-if="rack.missing.length" class="is-warn">
        <AlertTriangle :size="12" />
        {{
          $tn(
            rack.missing.length,
            '{n} pocillo sin tubo: haz clic en él para escribir su número o marcarlo vacío',
            '{n} pocillos sin tubo: haz clic en cada uno para escribir su número o marcarlo vacío',
          )
        }}
      </li>
      <li v-if="rack.need_rows.length">
        {{
          $tn(
            rack.need_rows.length,
            '{n} tubo no tiene fila en ninguna hoja del libro (Collection_data, Insectary_data…): necesita su fila; no se inventa',
            '{n} tubos no tienen fila en ninguna hoja del libro (Collection_data, Insectary_data…): necesitan su fila; no se inventan',
          )
        }}
      </li>
      <li v-if="rack.elsewhere.length">
        {{ $t('La hoja pone en este rack tubos que las fotos no muestran ahí') }}:
        <span class="font-mono">{{ rack.elsewhere.map(e => `${e.tube} (${e.well ?? '—'})`).join(', ') }}</span>
      </li>
      <li v-if="rack.duplicates.length">
        {{ $t('En dos filas de la hoja (no se cambian)') }}: <span class="font-mono">{{ rack.duplicates.join(', ') }}</span>
      </li>
      <li v-for="(p, i) in rack.problems" :key="i" class="is-warn"><AlertTriangle :size="12" /> {{ problemText(p) }}</li>
    </ul>
    <p v-else-if="rack.missing.length" class="rack-notes is-warn">
      <AlertTriangle :size="12" />
      {{
        $tn(
          rack.missing.length,
          '{n} pocillo sin tubo: haz clic en él para escribir su número o marcarlo vacío',
          '{n} pocillos sin tubo: haz clic en cada uno para escribir su número o marcarlo vacío',
        )
      }}
    </p>
  </section>
</template>

<style scoped>
.rack {
  container-type: inline-size;
  border-top: 1px solid var(--color-stone-200);
  padding: 0.5rem 0.5rem 0.25rem;
  scroll-margin-top: 0.5rem;
}
.rack-head {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 0.25rem 0.5rem;
  margin-bottom: 0.4rem;
}
.corner {
  display: inline-flex;
  align-items: center;
  gap: 0.25rem;
  border-radius: 0.25rem;
  padding: 0.05rem 0.4rem;
  font-size: 11px;
}
.corner.is-assumed {
  background: var(--color-amber-100);
  color: var(--color-amber-900);
}
.corner.is-confirmed {
  background: var(--color-emerald-50);
  color: var(--color-emerald-800);
}
.rack-btn {
  display: inline-flex;
  align-items: center;
  gap: 0.2rem;
  border: 1px solid var(--color-stone-300);
  border-radius: 0.25rem;
  background: white;
  padding: 0.1rem 0.45rem;
  font-size: 11px;
  color: var(--color-stone-700);
}
.rack-btn:hover:not(:disabled) {
  border-color: var(--color-emerald-600);
  color: var(--color-emerald-800);
}
.rack-btn:disabled {
  opacity: 0.5;
}
.rack-body {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}
@container (min-width: 780px) {
  .rack-body {
    flex-direction: row;
    align-items: flex-start;
  }
}
.rack-fields {
  align-self: flex-start;
  border-collapse: collapse;
  font-size: 12px;
  flex: none;
}
.rack-fields th,
.rack-fields td {
  border: 1px solid var(--color-stone-300);
  padding: 0.15rem 0.4rem;
  text-align: left;
}
.rack-fields th {
  background: var(--color-stone-100);
  font-weight: 600;
  color: var(--color-stone-700);
  white-space: nowrap;
}
.rack-fields input {
  width: 7.5rem;
  border: 0;
  background: transparent;
  padding: 0.1rem 0;
  font-size: 12px;
}
.rack-fields input:focus {
  outline: 2px solid var(--color-emerald-600);
  outline-offset: 1px;
}
.rack-fields input.is-unset {
  background: var(--color-amber-50);
  box-shadow: inset 0 -1px 0 var(--color-amber-400);
}
.rack-fields input.is-unset::placeholder {
  color: var(--color-amber-800);
  font-style: italic;
}
.rack-grid-wrap {
  min-width: 0;
  overflow-x: auto;
}
.rack-grid {
  border-collapse: collapse;
  font-size: 11px;
}
.rack-grid.is-busy {
  opacity: 0.6;
}
.rack-grid th {
  background: var(--color-stone-100);
  border: 1px solid var(--color-stone-300);
  padding: 0 0.3rem;
  font-weight: 600;
  color: var(--color-stone-700);
  text-align: center;
}
.rack-grid td {
  border: 1px solid var(--color-stone-300);
  padding: 0;
}
.well {
  position: relative;
  display: flex;
  min-width: 3.3rem;
  height: 2.1rem;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  line-height: 1.05;
  font-family: var(--font-mono, ui-monospace, monospace);
}
.well:focus-visible {
  outline: 2px solid var(--color-emerald-700);
  outline-offset: -2px;
}
.well-series {
  font-size: 9px;
  color: var(--color-stone-500);
}
.well-digits {
  font-size: 12px;
  font-weight: 600;
  color: var(--color-stone-900);
}
.well-word {
  font-family: var(--font-sans, inherit);
  font-size: 10px;
  font-style: italic;
}
.well.is-new {
  background: var(--color-emerald-50);
  box-shadow: inset 0 0 0 1px var(--color-emerald-300);
}
.well.is-moved {
  background: var(--color-amber-100);
  box-shadow: inset 0 0 0 1px var(--color-amber-400);
}
.well.is-same {
  background: white;
}
.well.is-check {
  background: var(--color-amber-50);
  box-shadow: inset 0 0 0 2px var(--color-amber-500);
}
.well.is-empty {
  background: repeating-linear-gradient(135deg, var(--color-stone-100) 0 4px, white 4px 8px);
  color: var(--color-stone-500);
}
.well.is-missing {
  background: var(--color-red-50);
  box-shadow: inset 0 0 0 1px var(--color-red-400);
  color: var(--color-red-800);
}
.well.is-left {
  background: var(--color-stone-100);
  color: var(--color-stone-400);
  text-decoration: line-through;
}
.well.is-off {
  background: var(--color-amber-50);
  box-shadow: inset 0 0 0 2px var(--color-red-500);
}
.well.is-conflict {
  background: var(--color-red-100);
  box-shadow: inset 0 0 0 2px var(--color-red-600);
}
.well.sheet-here {
  outline: 2px dashed var(--color-amber-600);
  outline-offset: -3px;
}
.well.is-selected {
  box-shadow: inset 0 0 0 2px var(--color-emerald-700);
}
.well.by-hand .well-digits {
  color: var(--color-sky-800);
}
.well.needs-rows::after {
  content: '';
  position: absolute;
  top: 2px;
  left: 2px;
  width: 4px;
  height: 4px;
  border-radius: 9999px;
  background: var(--color-stone-400);
}
.well.has-code::before {
  content: '▣';
  position: absolute;
  top: -1px;
  right: 1px;
  font-size: 9px;
  color: var(--color-stone-700);
}
.rack-legend {
  display: flex;
  flex-wrap: wrap;
  gap: 0.15rem 0.7rem;
  margin-top: 0.35rem;
  font-size: 11px;
  color: var(--color-stone-600);
}
.legend-item {
  display: inline-flex;
  align-items: center;
  gap: 0.25rem;
}
.swatch {
  display: inline-block;
  width: 0.7rem;
  height: 0.7rem;
  border: 1px solid var(--color-stone-300);
}
.swatch.is-new {
  background: var(--color-emerald-50);
  border-color: var(--color-emerald-300);
}
.swatch.is-moved {
  background: var(--color-amber-100);
  border-color: var(--color-amber-400);
}
.swatch.is-same {
  background: white;
}
.swatch.is-check {
  background: var(--color-amber-50);
  border: 2px solid var(--color-amber-500);
}
.swatch.is-empty {
  background: repeating-linear-gradient(135deg, var(--color-stone-200) 0 2px, white 2px 4px);
}
.swatch.is-missing {
  background: var(--color-red-50);
  border-color: var(--color-red-400);
}
.swatch.is-off {
  background: var(--color-amber-50);
  border: 2px solid var(--color-red-500);
}
.swatch.is-conflict {
  background: var(--color-red-100);
  border: 2px solid var(--color-red-600);
}
.swatch.is-sheet-here {
  border: 2px dashed var(--color-amber-600);
}
.swatch.is-left {
  background: var(--color-stone-100);
}
.swatch.dot {
  width: 0.35rem;
  height: 0.35rem;
  border: 0;
  border-radius: 9999px;
  background: var(--color-stone-400);
}
.well-bar {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 0.25rem 0.5rem;
  margin-top: 0.35rem;
  border: 1px solid var(--color-stone-200);
  border-radius: 0.25rem;
  background: var(--color-stone-50);
  padding: 0.3rem 0.5rem;
  font-size: 12px;
}
.well-input {
  width: 9rem;
  border: 1px solid var(--color-stone-300);
  border-radius: 0.25rem;
  padding: 0.1rem 0.35rem;
  font-size: 12px;
}
.rack-notes {
  margin-top: 0.35rem;
  font-size: 11px;
  color: var(--color-stone-700);
}
.rack-notes li {
  margin-top: 0.15rem;
}
.rack-notes .is-warn,
.rack-notes.is-warn {
  display: flex;
  align-items: center;
  gap: 0.25rem;
  color: var(--color-amber-900);
}
.rack-notes .is-conflict {
  display: flex;
  align-items: center;
  gap: 0.25rem;
  font-weight: 500;
  color: var(--color-red-800);
}
.rack-link {
  text-decoration: underline;
  color: inherit;
}
</style>
