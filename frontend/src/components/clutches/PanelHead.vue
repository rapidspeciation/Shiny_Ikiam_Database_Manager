<script setup lang="ts">
import { X } from 'lucide-vue-next'

/** The title of an inline area of a clutch's stage (what is being done) with its ✕ (Esc does the same). */
defineProps<{ title: string }>()
const emit = defineEmits<{ close: [] }>()
</script>

<template>
  <div class="-mt-1 mb-1.5 flex items-center gap-2">
    <h3 class="min-w-0 flex-1 text-sm font-semibold text-stone-900">{{ title }}</h3>
    <button type="button" class="-mr-1 grid h-9 w-9 shrink-0 place-items-center rounded-md text-stone-500 active:bg-stone-100" :aria-label="$t('Cerrar')" :title="$t('Cerrar (Esc)')" @click="emit('close')">
      <X :size="18" />
    </button>
  </div>
</template>
