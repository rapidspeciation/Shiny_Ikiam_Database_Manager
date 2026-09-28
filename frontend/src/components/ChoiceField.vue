<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, ref, useAttrs, useId, watch } from 'vue'
import { type Choice, type ChoiceOptions, commitText, filterChoices, labelOf, toChoices } from '../lib/choices'

/**
 * The app's one dropdown, like the grids' list: a text box with a ▾ and a
 * white list under it, filtered as you type (prefix matches first), with the
 * suggestion Enter or Tab takes marked as in the grid. It replaces the
 * browser's <datalist> popups and <select>s, which looked different in each
 * browser. `freetext` (default) keeps a new value typed, completed to the
 * first suggestion when one matches; without it the value must be an option
 * (typing only filters). On phones the ▾ opens the list alone, to tap from,
 * without the keyboard. Classes and attributes go to the text box.
 */
defineOptions({ inheritAttrs: false })
const props = withDefaults(
  defineProps<{
    options: ChoiceOptions
    placeholder?: string
    freetext?: boolean
    /** An empty entry ("—") at the top of the list. */
    allowEmpty?: boolean
  }>(),
  { placeholder: undefined, freetext: true, allowEmpty: false },
)
const model = defineModel<string>({ default: '' })
const attrs = useAttrs()

/** Rendered at most: a species list has ~10,000 names; typing narrows it. */
const LIMIT = 100
const EMPTY: Choice = { value: '', label: '—' }
const touchScreen = typeof window !== 'undefined' && window.matchMedia?.('(pointer: coarse)').matches

const id = useId()
const input = ref<HTMLInputElement>()
const root = ref<HTMLElement>()
const popup = ref<HTMLElement>()
const open = ref(false)
/** Opened with the ▾ on a phone: the list alone, the text box not focused (no keyboard). */
const tapOnly = ref(false)
/** Typed since the list opened (the list is filtered by the text only then). */
const edited = ref(false)
/** Moved through the list with ↑/↓. */
const navigated = ref(false)
const active = ref(-1)

// Built only when needed (the list is open or a value is looked up): options can be long.
const choices = computed(() => toChoices(props.options))
const lower = computed(() => choices.value.map(c => c.label.toLowerCase()))
const plain = computed(() => typeof props.options[0] !== 'object')
const shown = (value: string) => (plain.value ? value : labelOf(choices.value, value))

const text = ref(shown(model.value))
/** The value last stored (props follow only after the parent redraws). */
let committed = model.value
watch(model, value => {
  committed = value
  text.value = shown(value)
  edited.value = false
})
// Labels can arrive after the value (options loaded later).
watch(
  () => props.options,
  () => {
    if (!edited.value) text.value = shown(committed)
  },
)

const filtered = computed(() => filterChoices(choices.value, edited.value ? text.value : '', LIMIT, lower.value))
const entries = computed(() => {
  const items = filtered.value.items
  return props.allowEmpty && !(edited.value && text.value.trim()) ? [EMPTY, ...items] : items
})
const more = computed(() => filtered.value.total - filtered.value.items.length)
/** Free text with nothing matching: no list to show (the value is new). */
const visible = computed(() => open.value && (entries.value.length > 0 || !props.freetext))
const optionId = (i: number) => `${id}-o${i}`

/** Explicit widths (w-36) size the box; otherwise it fills its place as the other field-inputs do. */
const fixedWidth = () => /(^|\s)w-/.test(String(attrs.class ?? ''))

const popStyle = ref<Record<string, string>>({})
/**
 * Under the box, inside what is visible: on a phone the keyboard hides the
 * bottom of the page without resizing it, so the visual viewport is what
 * counts. Above the box when there is no room below.
 */
function place() {
  const el = input.value
  if (!el || !open.value) return
  const r = el.getBoundingClientRect()
  const view = window.visualViewport
  const top = view ? view.offsetTop : 0
  const bottom = view ? view.offsetTop + view.height : window.innerHeight
  const left = view ? view.offsetLeft : 0
  const right = view ? view.offsetLeft + view.width : window.innerWidth
  const below = bottom - r.bottom - 8
  const above = r.top - top - 8
  const up = below < 160 && above > below
  const room = Math.max(96, Math.min(256, up ? above : below))
  const width = Math.max(r.width, popup.value?.offsetWidth ?? 0)
  popStyle.value = {
    left: `${Math.max(left + 4, Math.min(r.left, right - width - 4))}px`,
    minWidth: `${r.width}px`,
    maxWidth: `${Math.min(448, right - left - 8)}px`,
    maxHeight: `${room}px`,
    ...(up ? { bottom: `${document.documentElement.clientHeight - r.top + 2}px` } : { top: `${r.bottom + 2}px` }),
  }
}

function onOutside(event: PointerEvent) {
  const target = event.target as Node
  if (root.value?.contains(target) || popup.value?.contains(target)) return
  commit()
}
function listen(on: boolean) {
  const view = window.visualViewport
  if (on) {
    document.addEventListener('pointerdown', onOutside, true)
    window.addEventListener('scroll', place, true)
    window.addEventListener('resize', place)
    view?.addEventListener('resize', place)
    view?.addEventListener('scroll', place)
  } else {
    document.removeEventListener('pointerdown', onOutside, true)
    window.removeEventListener('scroll', place, true)
    window.removeEventListener('resize', place)
    view?.removeEventListener('resize', place)
    view?.removeEventListener('scroll', place)
  }
}
watch(open, now => {
  listen(now)
  if (now) nextTick(() => (place(), nextTick(place), reveal()))
})
onBeforeUnmount(() => open.value && listen(false))

function show(tap = false) {
  if (!props.options.length && !props.allowEmpty) return
  tapOnly.value = tap
  if (open.value) return
  navigated.value = false
  // The current value is marked, so Enter keeps it.
  active.value = edited.value
    ? text.value.trim() && entries.value.length
      ? 0
      : -1
    : entries.value.findIndex(c => c.value === committed)
  open.value = true
}
function close() {
  open.value = false
  tapOnly.value = false
  navigated.value = false
  active.value = -1
}
function reveal() {
  nextTick(() => document.getElementById(optionId(active.value))?.scrollIntoView?.({ block: 'nearest' }))
}

function store(value: string) {
  text.value = shown(value)
  edited.value = false
  if (value !== committed) {
    committed = value
    model.value = value
    // A parent that did not take the value (it keeps its own) gets its own shown again.
    nextTick(() => {
      if (model.value !== committed) {
        committed = model.value
        text.value = shown(committed)
      }
    })
  }
}
/** Enter, Tab, leaving the box: the marked suggestion, else the typed text by the freetext rules, else back to the value. */
function commit() {
  const pick = (edited.value || navigated.value) && entries.value[active.value]
  if (pick) store(pick.value)
  else if (edited.value) {
    const value = commitText(text.value, choices.value, { freetext: props.freetext, allowEmpty: props.allowEmpty })
    store(value ?? committed)
  } else text.value = shown(committed)
  close()
}
function take(choice: Choice) {
  store(choice.value)
  close()
}

function onInput() {
  edited.value = true
  navigated.value = false
  if (!open.value) show()
  active.value = text.value.trim() && entries.value.length ? 0 : -1
  reveal()
}
function onFocus() {
  tapOnly.value = false
  // Select-like lists: typing replaces the value (it only filters).
  if (!props.freetext) input.value?.select()
  show()
}
function onKey(event: KeyboardEvent) {
  // Keys that finish composing an accented letter or a suggestion of the phone's keyboard.
  if (event.isComposing) return
  const n = entries.value.length
  if (event.key === 'ArrowDown' || event.key === 'ArrowUp') {
    event.preventDefault()
    if (!open.value) return show()
    if (!n) return
    active.value = event.key === 'ArrowDown' ? Math.min(active.value + 1, n - 1) : Math.max(active.value - 1, 0)
    navigated.value = true
    reveal()
  } else if (event.key === 'Enter') {
    // Taking a suggestion must not submit the form around the box.
    if (visible.value) event.preventDefault()
    commit()
  } else if (event.key === 'Tab') {
    if (open.value) commit()
  } else if (event.key === 'Escape' && open.value) {
    // Only the list closes (not a dialog around it); the value comes back.
    if (visible.value) {
      event.preventDefault()
      event.stopPropagation()
    }
    edited.value = false
    text.value = shown(committed)
    close()
  }
}
function onBlur() {
  // Still open with the ▾ list on a phone: the list is being tapped.
  if (tapOnly.value) return
  if (open.value || edited.value) commit()
}
/** ▾: toggles the list. On a phone, the list alone (the keyboard would cover it). */
function onArrow() {
  if (touchScreen) {
    if (open.value && tapOnly.value) return commit()
    if (document.activeElement === input.value) input.value?.blur()
    return show(true)
  }
  if (open.value) return commit()
  input.value?.focus()
  show()
}
</script>

<template>
  <span ref="root" class="relative" :class="fixedWidth() ? 'inline-flex max-w-full align-middle' : 'flex w-full'">
    <input
      ref="input"
      v-model="text"
      type="text"
      role="combobox"
      autocomplete="off"
      aria-autocomplete="list"
      :aria-expanded="visible"
      :aria-controls="`${id}-list`"
      :aria-activedescendant="visible && active >= 0 ? optionId(active) : undefined"
      :placeholder="placeholder ?? (allowEmpty ? '—' : undefined)"
      @input="onInput"
      @focus="onFocus"
      @click="open || show()"
      @keydown="onKey"
      @blur="onBlur"
      v-bind="$attrs"
      class="min-w-0 pr-7"
    />
    <button
      type="button"
      class="absolute inset-y-px right-px flex w-6 items-center justify-center rounded-r-md text-[15px] text-stone-700 hover:bg-brand-50 hover:text-brand-700"
      tabindex="-1"
      aria-label="Ver la lista"
      title="Ver la lista"
      @mousedown.prevent
      @click="onArrow"
    >
      ▾
    </button>
    <Teleport to="body">
      <div
        v-if="visible"
        :id="`${id}-list`"
        ref="popup"
        role="listbox"
        class="choice-list fixed z-[10000] overflow-y-auto overscroll-contain rounded-md border border-stone-300 bg-white py-1 text-sm shadow-lg"
        :style="popStyle"
        @mousedown.prevent
      >
        <template v-for="(o, i) in entries" :key="`${i}:${o.value}`">
          <div
            v-if="o.group && o.group !== entries[i - 1]?.group"
            role="presentation"
            class="px-2.5 pt-1.5 pb-0.5 text-[11px] font-semibold tracking-wide text-stone-500 uppercase"
          >
            {{ o.group }}
          </div>
          <div
            :id="optionId(i)"
            role="option"
            :aria-selected="i === active"
            class="flex cursor-pointer items-baseline gap-3 px-2.5 py-1 whitespace-nowrap text-stone-800 hover:bg-stone-100 pointer-coarse:py-2.5"
            :class="{ 'is-pick': i === active, 'text-stone-500': o === EMPTY }"
            @click="take(o)"
          >
            <span class="min-w-0 truncate">{{ o.label }}</span>
            <span v-if="o.hint" class="ml-auto text-xs text-stone-500">{{ o.hint }}</span>
          </div>
        </template>
        <div v-if="!entries.length" class="px-2.5 py-1 text-stone-500">Ninguna opción coincide</div>
        <div v-if="more > 0" class="border-t border-stone-200 px-2.5 pt-1 text-xs text-stone-500">
          {{ more }} más: escribe para filtrar
        </div>
      </div>
    </Teleport>
  </span>
</template>
