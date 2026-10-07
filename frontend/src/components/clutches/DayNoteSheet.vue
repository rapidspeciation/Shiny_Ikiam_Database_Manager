<script setup lang="ts">
import { nextTick, onBeforeUnmount, onMounted, ref } from 'vue'
import { Check, Loader2 } from 'lucide-vue-next'
import PanelHead from './PanelHead.vue'
import { dayLabel } from '../../lib/dates'

/**
 * A clutch's note of the day: one per clutch and day, as long as needed,
 * written by anyone (the last to change it is shown). It stays in the app:
 * the sheet's NOTES keeps the short dated lines the events write. Inline in
 * the stage's area (no overlay): «Listo» or Ctrl+Enter keeps it, Esc closes.
 */
const props = defineProps<{
  clutch: string
  day: string
  text: string
  by: string | null
  saving?: boolean
}>()
const emit = defineEmits<{ close: []; save: [text: string] }>()
const draft = ref(props.text)
const box = ref<HTMLTextAreaElement>()
onMounted(() => nextTick(() => box.value?.focus()))
const onKey = (e: KeyboardEvent) => {
  if (e.key === 'Escape') emit('close')
  // Ctrl/⌘+Enter keeps the note (Enter alone is a new line).
  else if (e.key === 'Enter' && (e.ctrlKey || e.metaKey) && draft.value.trim() !== props.text.trim()) emit('save', draft.value)
}
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <section class="mt-2 rounded-lg border border-stone-300 bg-white p-2.5" role="region" :aria-label="$t('Nota de hoy del clutch {clutch}', { clutch })">
      <PanelHead :title="$t('Nota de hoy del clutch {clutch}', { clutch })" @close="emit('close')" />
      <p class="text-sm text-stone-600">{{ dayLabel(day) }} · {{ $t('solo en la app (no va a Google Sheets)') }}<template v-if="by"> · {{ $t('última vez: {who}', { who: by }) }}</template></p>
      <textarea ref="box" v-model="draft" class="field-input mt-2 min-h-32 text-base" rows="5" :placeholder="$t('Lo que viste hoy: plantas, larvas, dudas…')" />
      <div class="mt-2 flex gap-2">
        <button type="button" class="btn h-12 flex-1" @click="emit('close')">{{ $t('Cancelar') }}</button>
        <button type="button" class="btn-primary h-12 flex-[2]" :disabled="saving || draft.trim() === text.trim()" @click="emit('save', draft)">
          <Loader2 v-if="saving" :size="18" class="animate-spin" /><Check v-else :size="18" /> {{ $t('Listo') }}
        </button>
      </div>
  </section>
</template>
