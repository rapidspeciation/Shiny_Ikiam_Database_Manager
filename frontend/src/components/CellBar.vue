<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { Lock } from 'lucide-vue-next'
import { barKey, type CellBarInfo, type Direction } from '../lib/gridKit'

/**
 * The bar above a grid, like Google Sheets' formula bar: the selected cell
 * (column · row ID) and its whole text across the grid's width, wrapping onto
 * up to five lines (three on a phone), then scrolling. Where the cell can be
 * edited the text can be too: Enter saves and goes down, Tab right, Esc gives
 * up, leaving the bar saves; Shift+Enter or Alt+Enter break the line in a
 * notes column. The grid writes the text as if typed in the cell (`save`),
 * so it is checked, recorded and saved the same way. Lines under the text show
 * the sheet's or the assistant's value, or a sum's total (`notes`).
 */
const props = defineProps<{ info: CellBarInfo | null }>()
const emit = defineEmits<{
  /** Write `text` into the cell (then move as Enter or Tab would; null: the focus went elsewhere). */
  save: [target: CellBarInfo, text: string, move: Direction | 'here' | null]
  /** One of the other readings offered (`choices`), picked: written into the cell. */
  pick: [target: CellBarInfo, text: string]
  /** Back to the grid without a change. */
  back: [move: Direction | 'here']
}>()

const box = ref<HTMLTextAreaElement>()
const text = ref('')
/** The cell whose text is being changed in the bar: a click on another cell saves it here, not there. */
let editing: CellBarInfo | null = null
const touch = typeof window !== 'undefined' && window.matchMedia?.('(pointer: coarse)').matches
/** Line height and vertical padding of the text box (style.css). */
const LINE = 18
const PADDING = 6
const MAX_LINES = touch ? 3 : 5
const below = computed(() => props.info?.notes?.filter(n => n.kind !== 'total') ?? [])
const totals = computed(() => props.info?.notes?.filter(n => n.kind === 'total') ?? [])

watch(
  () => props.info,
  info => {
    // What is being typed stays; the cell's own text changing meanwhile (a save landing) is taken if nothing was typed.
    if (editing && text.value !== editing.text) return
    if (editing && info && (info.index !== editing.index || info.field !== editing.field)) return
    if (editing && info) editing = info
    text.value = info?.text ?? ''
    nextTick(grow)
  },
  { immediate: true },
)

/** As tall as its text, up to MAX_LINES lines, then it scrolls. */
function grow() {
  const el = box.value
  // Not while hidden (a tab kept in the background): measured then, it would change size on coming back,
  // and the grid under it would be redrawn for nothing.
  if (!el?.clientWidth) return
  // The box's height includes its border (box-sizing), its scrollHeight does not.
  const border = el.offsetHeight - el.clientHeight
  el.style.height = 'auto'
  el.style.height = `${Math.min(el.scrollHeight, MAX_LINES * LINE + PADDING) + border}px`
}

function onFocus() {
  editing = props.info?.editable ? props.info : null
}

function onKeydown(event: KeyboardEvent) {
  const target = editing ?? props.info
  const key = barKey(event, !!target?.editable && target.multiline)
  if (!key) return
  event.preventDefault()
  if (key.action === 'newline') {
    const el = box.value!
    el.setRangeText('\n', el.selectionStart, el.selectionEnd, 'end')
    text.value = el.value
    grow()
  } else if (key.action === 'cancel') {
    text.value = editing?.text ?? props.info?.text ?? ''
    editing = null
    nextTick(grow)
    emit('back', 'here')
  } else finish(key.move)
}

/** Saves what was typed (if anything) into the cell it was typed for. */
function finish(move: Direction | 'here' | null) {
  const target = editing
  editing = null
  if (target && text.value !== target.text) emit('save', target, text.value, move)
  else if (move) emit('back', move)
  // The grid may refuse or change the text (a date read day first): the bar shows the cell again.
  if (!move) text.value = props.info?.text ?? ''
}

// The bar is as wide as the grid: a new width can change how many lines its text takes.
let resize: ResizeObserver | null = null
onMounted(() => {
  if (!box.value) return
  resize = new ResizeObserver(() => grow())
  resize.observe(box.value)
})
onBeforeUnmount(() => resize?.disconnect())
</script>

<template>
  <div class="cell-bar" data-cell-bar>
    <span class="cell-bar-where" :title="info ? `${info.column} · ${info.row}` : ''">
      <template v-if="info"
        ><b>{{ info.column }}</b
        ><template v-if="info.row"> · {{ info.row }}</template></template
      >
    </span>
    <div class="min-w-0 flex-1">
      <textarea
        ref="box"
        v-model="text"
        rows="1"
        spellcheck="false"
        class="cell-bar-text"
        :class="{ 'is-readonly': info && !info.editable }"
        :readonly="!info?.editable"
        :disabled="!info"
        :placeholder="info ? '' : $t('Selecciona una celda para ver todo su texto')"
        :aria-label="info ? $t('Contenido de {column}', { column: info.column }) : $t('Contenido de la celda')"
        @input="grow"
        @focus="onFocus"
        @blur="finish(null)"
        @keydown="onKeydown"
      />
      <p v-if="below.length || info?.choices?.length" class="cell-bar-notes">
        <span v-for="(note, i) in below" :key="i" :class="note.kind ? `is-${note.kind}` : ''">
          <b v-if="note.label">{{ note.label }}:</b> {{ note.text }}
        </span>
        <!-- A doubtful cell's other readings: a click writes one into the cell (the grid keeps its selection). -->
        <span v-if="info?.choices?.length" class="cell-bar-choices">
          <b>{{ $t('Otras lecturas') }}:</b>
          <button
            v-for="(choice, i) in info.choices"
            :key="`c${i}`"
            type="button"
            class="cell-bar-choice"
            :disabled="!info.editable"
            :title="$t('Escribir {value} en la celda', { value: choice.label })"
            @mousedown.prevent
            @click="emit('pick', info, choice.text)"
          >
            {{ choice.label }}
          </button>
        </span>
      </p>
    </div>
    <!-- A sum's total beside it, as in the cell: the bar stays one line tall. -->
    <span v-for="(note, i) in totals" :key="`t${i}`" class="cell-bar-total">{{ note.text }}</span>
    <span v-if="info && !info.editable" class="cell-bar-lock" :title="info.readonly || $t('Solo lectura')">
      <Lock :size="12" />
    </span>
  </div>
</template>
