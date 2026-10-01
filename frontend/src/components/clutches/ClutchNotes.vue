<script setup lang="ts">
import { computed, nextTick, ref, watch } from 'vue'
import { MessageSquarePlus, PencilLine, Undo2 } from 'lucide-vue-next'
import ParentsPicker from './ParentsPicker.vue'
import SexBadge from '../SexBadge.vue'
import type { useParents } from '../../composables/useParents'
import { isBlank } from '../../lib/cells'
import { NOTE_PHRASES, appendNote, noteDay, noteParts, notesOf, parentsOf, withParents } from '../../lib/clutches'
import { formatSerial } from '../../lib/dates'
import type { CellValue } from '../../lib/types'
import { t } from '../../lib/i18n'

/**
 * A clutch's parents and NOTES, near the top of its editor. The parents live
 * in NOTES for now ("U8A♀ + C8B♂", older "F1 clutch parents J7A + P5A"): shown
 * as ♀ mother and ♂ father, and changing them rewrites that part of the note
 * in the standard form (or adds it as a new dated note). The notes are listed
 * one by one; a new one is dated and signed and goes after them (" | ", never
 * replacing them), and «Corregir las notas» edits the cell's whole text. Every
 * change is the editor's pending edit of NOTES (`set`).
 */
const props = defineProps<{
  /** The cell as the person sees it (unsaved edit included). */
  notes: CellValue
  /** The cell as saved, to undo the unsaved change. */
  saved: CellValue
  dirty: boolean
  editable: boolean
  initials: string
  today: number
  parents: ReturnType<typeof useParents>
  /** The clutch's species: its butterflies come first in the parents' lists. */
  species: string
  /** Changes when another clutch is opened: the boxes close. */
  clutchId: string
}>()
const emit = defineEmits<{ set: [value: CellValue]; parentsWritten: [] }>()

const list = computed(() => notesOf(props.notes).map(noteParts))
const written = computed(() => parentsOf(props.notes))
const prefix = computed(() => `${noteDay(props.today)} ${props.initials}:`)
const message = ref('')

// --- Parents
const pickingParents = ref(false)
const female = ref('')
const male = ref('')
function openParents() {
  female.value = written.value?.female ?? ''
  male.value = written.value?.male ?? ''
  message.value = ''
  pickingParents.value = true
}
/** NOTES as they will read with the parents chosen. */
const parentsPreview = computed(() => (female.value.trim() && male.value.trim() ? withParents(props.notes, female.value, male.value, props.today, props.initials) : ''))
function writeParents() {
  if (!female.value.trim() || !male.value.trim()) return (message.value = t('Escribe el ID de la hembra y del macho'))
  emit('set', parentsPreview.value)
  emit('parentsWritten')
  pickingParents.value = false
}
// The species and life of the parents shown need Insectary_data: asked for only when a clutch has parents.
watch(
  () => !!written.value,
  has => {
    if (has) props.parents.load()
  },
  { immediate: true },
)
function about(id: string): string {
  const p = props.parents.find(id)
  if (!p) return props.parents.loaded.value ? t('no está en Insectary_data') : ''
  const life = p.alive ? t('vive') : p.death !== null ? t('murió el {date}', { date: formatSerial(p.death) }) : t('murió')
  return [p.species, life].filter(Boolean).join(' · ')
}

// --- A new note, dated and signed
const adding = ref(false)
const noteText = ref('')
const noteBox = ref<HTMLTextAreaElement>()
function startNote() {
  adding.value = true
  correcting.value = false
  nextTick(() => noteBox.value?.focus())
}
function addPhrase(p: string) {
  noteText.value = noteText.value.trim() ? `${noteText.value.trim()}; ${p}` : p
}
function addNote() {
  const text = noteText.value.trim()
  if (!text) return
  emit('set', appendNote(props.notes, text, props.today, props.initials))
  noteText.value = ''
  adding.value = false
}

// --- The whole text, to correct what is written
const correcting = ref(false)
const allText = ref('')
function startCorrect() {
  allText.value = isBlank(props.notes) ? '' : String(props.notes)
  correcting.value = true
  adding.value = false
}
function applyCorrect() {
  const text = allText.value.trim()
  emit('set', text ? text : null)
  correcting.value = false
}
const undo = () => emit('set', props.saved)

watch(
  () => props.clutchId,
  () => {
    pickingParents.value = false
    adding.value = false
    correcting.value = false
    noteText.value = ''
    message.value = ''
  },
)
</script>

<template>
  <div>
    <!-- Parents, read from NOTES: ♀ mother and ♂ father. -->
    <section class="py-3" :aria-label="$t('Padres')">
      <div class="mb-1 flex items-center gap-2">
        <span class="field-label mb-0 flex-1">{{ $t('Padres (en NOTES)') }}</span>
        <button v-if="editable && !pickingParents" type="button" class="btn h-11 px-3" @click="openParents">
          <PencilLine :size="16" /> {{ written ? $t('Corregir padres') : $t('Escribir padres') }}
        </button>
      </div>
      <div v-if="!pickingParents" class="grid grid-cols-2 gap-2">
        <button
          v-for="p in [
            { sex: 'female', name: $t('Madre'), id: written?.female ?? '' },
            { sex: 'male', name: $t('Padre'), id: written?.male ?? '' },
          ]"
          :key="p.sex"
          type="button"
          class="flex min-h-14 min-w-0 flex-col items-start rounded-lg border border-stone-200 bg-stone-50 px-2.5 py-1.5 text-left disabled:opacity-100"
          :disabled="!editable"
          @click="openParents"
        >
          <span class="flex items-center gap-1.5 text-xs text-stone-600"><SexBadge :sex="p.sex" /> {{ p.name }}</span>
          <span v-if="p.id" class="text-lg leading-tight font-semibold tabular-nums">{{ p.id }}</span>
          <span v-else class="text-sm text-stone-500 italic">{{ $t('no está en las notas') }}</span>
          <span v-if="p.id && about(p.id)" class="w-full truncate text-xs text-stone-500">{{ about(p.id) }}</span>
        </button>
      </div>
      <div v-else class="rounded-lg border border-stone-200 p-2">
        <ParentsPicker v-model:female="female" v-model:male="male" :parents="parents" :species="species" />
        <p v-if="parentsPreview" class="mt-2 text-xs text-stone-600">
          {{ written ? $t('NOTES quedará así:') : $t('Se añade a NOTES:') }}
          <span class="mt-0.5 block rounded bg-amber-50 px-2 py-1 text-sm break-words text-stone-800">{{ parentsPreview }}</span>
        </p>
        <p v-if="message" class="mt-1 text-sm text-red-800" role="alert">{{ message }}</p>
        <div class="mt-2 flex gap-2">
          <button type="button" class="btn h-11 flex-1" @click="pickingParents = false">{{ $t('Cancelar') }}</button>
          <button type="button" class="btn-primary h-11 flex-1" :disabled="!female.trim() || !male.trim()" @click="writeParents">{{ $t('Escribir en NOTES') }}</button>
        </div>
      </div>
    </section>

    <!-- NOTES: each note on its line; a new one after them; the whole text to correct. -->
    <section class="border-t border-stone-100 py-3" :aria-label="'NOTES'">
      <div class="mb-1 flex items-center gap-2">
        <span class="field-label mb-0 flex-1">NOTES</span>
        <button v-if="dirty && editable" type="button" class="flex h-11 items-center gap-1 px-1 text-sm text-stone-600 underline" @click="undo">
          <Undo2 :size="15" /> {{ $t('Deshacer') }}
        </button>
      </div>
      <ul v-if="list.length" class="space-y-1">
        <li
          v-for="(n, i) in list"
          :key="i"
          class="rounded-md px-2 py-1 text-sm break-words"
          :class="dirty && i === list.length - 1 ? 'bg-amber-50' : 'bg-stone-50'"
        >
          <span v-if="n.head" class="mr-1 text-xs font-medium whitespace-nowrap text-stone-500">{{ n.head }}</span>{{ n.text }}
        </li>
      </ul>
      <p v-else class="text-sm text-stone-500">{{ $t('Sin notas') }}</p>
      <p v-if="dirty" class="mt-1 text-xs text-amber-900">{{ $t('Sin guardar: se guarda con el resto del clutch.') }}</p>

      <template v-if="editable">
        <div v-if="!adding && !correcting" class="mt-2 grid grid-cols-2 gap-2">
          <button type="button" class="btn h-11" @click="startNote"><MessageSquarePlus :size="16" /> {{ $t('Añadir nota') }}</button>
          <button type="button" class="btn h-11" :disabled="!list.length" @click="startCorrect"><PencilLine :size="16" /> {{ $t('Corregir las notas') }}</button>
        </div>

        <div v-if="adding" class="mt-2">
          <div class="flex flex-wrap gap-1.5">
            <button v-for="p in NOTE_PHRASES" :key="p" type="button" class="min-h-10 rounded-full border border-stone-300 bg-white px-3 text-sm active:bg-stone-100" @click="addPhrase(p)">
              {{ p }}
            </button>
          </div>
          <label class="mt-2 block">
            <span class="sr-only">{{ $t('Nota nueva') }}</span>
            <textarea
              ref="noteBox"
              v-model="noteText"
              class="field-input min-h-20 text-base"
              rows="2"
              :placeholder="$t('Nota nueva, en inglés (p. ej. 3 larvae dead)')"
              enterkeyhint="done"
            />
          </label>
          <p class="mt-1 text-xs text-stone-500">{{ $t('Se añade después de las otras como «{prefix} …»', { prefix }) }}</p>
          <div class="mt-1 flex gap-2">
            <button type="button" class="btn h-11 flex-1" @click="(adding = false), (noteText = '')">{{ $t('Cancelar') }}</button>
            <button type="button" class="btn-primary h-11 flex-1" :disabled="!noteText.trim()" @click="addNote">{{ $t('Añadir nota') }}</button>
          </div>
        </div>

        <div v-if="correcting" class="mt-2">
          <label class="block">
            <span class="field-label">{{ $t('Todo el texto de NOTES') }}</span>
            <textarea v-model="allText" class="field-input min-h-28 text-base" rows="4" />
          </label>
          <p class="mt-1 text-xs text-stone-500">{{ $t('Para corregir lo escrito: reemplaza todo el texto de la celda. Las notas van separadas por « | ».') }}</p>
          <div class="mt-1 flex gap-2">
            <button type="button" class="btn h-11 flex-1" @click="correcting = false">{{ $t('Cancelar') }}</button>
            <button
              type="button"
              class="btn-primary h-11 flex-1"
              :disabled="allText.trim() === (isBlank(notes) ? '' : String(notes).trim())"
              @click="applyCorrect"
            >
              {{ $t('Usar este texto') }}
            </button>
          </div>
        </div>
      </template>
    </section>
  </div>
</template>
