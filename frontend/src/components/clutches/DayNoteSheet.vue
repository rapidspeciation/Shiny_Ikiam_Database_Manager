<script setup lang="ts">
import { nextTick, onBeforeUnmount, onMounted, ref } from 'vue'
import { Check, Loader2, X } from 'lucide-vue-next'
import { dayLabel } from '../../lib/dates'

/**
 * A clutch's note of the day: one per clutch and day, as long as needed,
 * written by anyone (the last to change it is shown). It stays in the app:
 * the sheet's NOTES keeps the short dated lines the events write.
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
const onKey = (e: KeyboardEvent) => e.key === 'Escape' && emit('close')
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <div class="fixed inset-0 z-50 flex items-end justify-center bg-black/30 sm:items-center" @click.self="emit('close')">
    <section
      class="max-h-full w-full overflow-y-auto rounded-t-2xl bg-white p-4 pb-[calc(1rem+env(safe-area-inset-bottom))] shadow-xl sm:max-w-lg sm:rounded-2xl"
      role="dialog"
      :aria-label="$t('Nota de hoy del clutch {clutch}', { clutch })"
    >
      <header class="flex items-center gap-2">
        <h2 class="min-w-0 flex-1 text-lg font-semibold">{{ $t('Nota de hoy del clutch {clutch}', { clutch }) }}</h2>
        <button class="btn-ghost h-11 w-11 justify-center" :aria-label="$t('Cerrar')" @click="emit('close')"><X :size="22" /></button>
      </header>
      <p class="text-sm text-stone-600">{{ dayLabel(day) }} · {{ $t('solo en la app (no va a Google Sheets)') }}<template v-if="by"> · {{ $t('última vez: {who}', { who: by }) }}</template></p>
      <textarea ref="box" v-model="draft" class="field-input mt-2 min-h-40 text-base" rows="6" :placeholder="$t('Lo que viste hoy: plantas, larvas, dudas…')" />
      <div class="mt-2 flex gap-2">
        <button type="button" class="btn h-12 flex-1" @click="emit('close')">{{ $t('Cancelar') }}</button>
        <button type="button" class="btn-primary h-12 flex-[2]" :disabled="saving || draft.trim() === text.trim()" @click="emit('save', draft)">
          <Loader2 v-if="saving" :size="18" class="animate-spin" /><Check v-else :size="18" /> {{ $t('Guardar nota') }}
        </button>
      </div>
    </section>
  </div>
</template>
