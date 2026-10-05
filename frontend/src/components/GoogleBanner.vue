<script setup lang="ts">
import { computed } from 'vue'
import { Clock, Loader2 } from 'lucide-vue-next'
import { googleNotice } from '../lib/staged'
import { useLive } from '../stores/live'

/**
 * Everyone sees it at once: Google Sheets is not answering normally (the
 * workbook recalculates for minutes after edits, server/workbook-health.mjs),
 * and how many saves wait in the app to be written when it answers
 * (server/outbox.mjs). Nothing is blocked: people keep working.
 */
const live = useLive()
const notice = computed(() => googleNotice(live.workbook.state, live.waiting))
</script>

<template>
  <div
    v-if="notice"
    role="status"
    class="flex items-center gap-2 border-b px-3 py-1.5 text-sm sm:px-4"
    :class="notice.kind === 'writing' ? 'border-stone-200 bg-stone-50 text-stone-700' : 'border-amber-300 bg-amber-50 text-amber-950'"
  >
    <Loader2 v-if="notice.kind === 'writing'" :size="15" class="shrink-0 animate-spin" />
    <Clock v-else :size="15" class="shrink-0" />
    <span>{{ notice.text }}</span>
    <span v-if="live.simulated" class="ml-auto rounded bg-amber-200 px-1.5 text-xs">{{ $t('simulado (laboratorio)') }}</span>
  </div>
</template>
