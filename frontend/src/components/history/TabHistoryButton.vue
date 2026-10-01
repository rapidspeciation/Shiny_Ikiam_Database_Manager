<script setup lang="ts">
import { ref } from 'vue'
import { History } from 'lucide-vue-next'
import TabHistory from './TabHistory.vue'

/** «Historial» in a data-entry tab's toolbar: opens that tab's saves of the day, to undo one (TabHistory). */
defineProps<{ purpose: string; title: string }>()
const open = ref(false)
</script>

<template>
  <div class="flex items-end">
    <button type="button" class="btn" :title="title" @click="open = true"><History :size="15" /> {{ $t('Historial') }}</button>
    <TabHistory v-if="open" :title="title" :purpose="purpose" @close="open = false" />
  </div>
</template>
