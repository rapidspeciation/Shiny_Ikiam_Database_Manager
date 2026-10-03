<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { ChevronDown, Lock } from 'lucide-vue-next'
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
 *
 * The bar keeps one height whatever cell is selected (one line of text, and
 * one of notes with `notesLine`): it grew with each cell's text, so the grid
 * under it moved as cells were clicked, and the second click of a double click
 * landed on another row. The rest shows over the grid while the bar has the
 * focus, or after a moment under the pointer (the ⌄ says there is more).
 */
const props = defineProps<{ info: CellBarInfo | null; notesLine?: boolean }>()
const emit = defineEmits<{
  /** Write `text` into the cell (then move as Enter or Tab would; null: the focus went elsewhere). */
  save: [target: CellBarInfo, text: string, move: Direction | 'here' | null]
  /** One of the other readings offered (`choices`), picked: written into the cell. */
  pick: [target: CellBarInfo, text: string]
  /** Back to the grid without a change. */
  back: [move: Direction | 'here']
}>()

/** A choice clicked: written into the cell, or (an unreadable cell's partial reading) put in the bar to complete. */
function choose(info: CellBarInfo, choice: string) {
  if (!info.choicesComplete) return emit('pick', info, choice)
  const el = box.value
  if (!el) return
  el.focus()
  editing = info
  text.value = choice
  nextTick(() => {
    grow()
    el.setSelectionRange(choice.length, choice.length)
  })
}

const box = ref<HTMLTextAreaElement>()
const text = ref('')
/** The cell whose text is being changed in the bar: a click on another cell saves it here, not there. */
let editing: CellBarInfo | null = null
const touch = typeof window !== 'undefined' && window.matchMedia?.('(pointer: coarse)').matches
/** Line height and vertical padding of the text box (style.css). */
const LINE = 18
const PADDING = 6
const MAX_LINES = touch ? 3 : 5
/** The bar's whole text and notes are shown, over the grid: it has the focus, or the pointer rests on it. */
const focused = ref(false)
const hovered = ref(false)
const open = computed(() => focused.value || hovered.value)
/** Some of the text or notes is cut off while the bar keeps its one line. */
const clipped = ref(false)
let hoverTimer: number | undefined
function onEnter() {
  window.clearTimeout(hoverTimer)
  hoverTimer = window.setTimeout(() => (hovered.value = clipped.value), 300)
}
function onLeave() {
  window.clearTimeout(hoverTimer)
  hovered.value = false
}
watch(open, () => nextTick(grow))
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

/** One line; open, as tall as its text up to MAX_LINES lines, then it scrolls. */
function grow() {
  const el = box.value
  // Not while hidden (a tab kept in the background): nothing to measure.
  if (!el?.clientWidth) return
  // The box's height includes its border (box-sizing), its scrollHeight does not.
  const border = el.offsetHeight - el.clientHeight
  el.style.height = 'auto'
  const full = el.scrollHeight
  el.style.height = `${Math.min(full, (open.value ? MAX_LINES : 1) * LINE + PADDING) + border}px`
  // (The text box sits over the grid, so measuring it no longer moves anything.)
  const line = root.value?.clientHeight ?? 0
  clipped.value = full > LINE + PADDING + 1 || (!!line && (body.value?.scrollHeight ?? 0) > line + 1)
}
const root = ref<HTMLElement>()
const body = ref<HTMLElement>()

function onFocus() {
  focused.value = true
  editing = props.info?.editable ? props.info : null
}
function onBlur() {
  focused.value = false
  finish(null)
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
onBeforeUnmount(() => {
  resize?.disconnect()
  window.clearTimeout(hoverTimer)
})
</script>

<template>
  <div
    ref="root"
    class="cell-bar"
    :class="{ 'has-notes-line': notesLine, 'is-open': open && (clipped || focused) }"
    data-cell-bar
    @mouseenter="onEnter"
    @mouseleave="onLeave"
  >
    <div ref="body" class="cell-bar-body">
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
          @blur="onBlur"
          @keydown="onKeydown"
        />
        <p v-if="below.length || info?.choices?.length" class="cell-bar-notes">
          <span v-for="(note, i) in below" :key="i" :class="note.kind ? `is-${note.kind}` : ''">
            <b v-if="note.label">{{ note.label }}:</b> {{ note.text }}
          </span>
          <!-- A doubtful cell's other readings: a click writes one into the cell (the grid keeps its selection).
               An unreadable cell's partial readings: a click puts one in the bar to complete. -->
          <span v-if="info?.choices?.length" class="cell-bar-choices" :class="{ 'is-complete': info.choicesComplete }">
            <b>{{ info.choicesLabel ?? $t('Otras lecturas') }}:</b>
            <button
              v-for="(choice, i) in info.choices"
              :key="`c${i}`"
              type="button"
              class="cell-bar-choice"
              :disabled="!info.editable"
              :title="
                info.choicesComplete
                  ? $t('Completar {value} en la barra', { value: choice.label })
                  : $t('Escribir {value} en la celda', { value: choice.label })
              "
              @mousedown.prevent
              @click="choose(info, choice.text)"
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
      <!-- More text or notes than the bar's line: a click shows all of it (as does resting the pointer on the bar). -->
      <button
        v-if="clipped && !open"
        type="button"
        class="cell-bar-more"
        :title="$t('Ver todo el texto')"
        @mousedown.prevent
        @click="box?.focus()"
      >
        <ChevronDown :size="14" />
      </button>
      <!-- The grid's own actions on the selected cell (the Buscador: its history). -->
      <slot name="actions" />
    </div>
  </div>
</template>
