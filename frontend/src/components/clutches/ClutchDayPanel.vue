<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted } from 'vue'
import { History, X } from 'lucide-vue-next'
import TodayChanges from './TodayChanges.vue'
import { useClutchDay } from '../../composables/useClutchDay'
import { initialsOf } from '../../lib/rows'
import { useSession } from '../../stores/session'

/**
 * The Clutches history (TodayChanges: the day's changes to copy into the
 * notebook and to undo) as a panel over the table, for the table mode; the
 * cards show it as their «Hoy» list.
 */
const props = defineProps<{ collectors: string[] }>()
const emit = defineEmits<{ close: [] }>()
const session = useSession()
const day = useClutchDay()
const initials = computed(() => initialsOf(session.user?.displayName || '', props.collectors, session.user?.username || ''))
const onKey = (e: KeyboardEvent) => e.key === 'Escape' && emit('close')
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <div class="fixed inset-0 z-50 flex justify-end bg-black/30" @click.self="emit('close')">
    <section class="flex h-full w-full flex-col bg-stone-50 shadow-xl sm:max-w-xl" role="dialog" :aria-label="$t('Historial de Clutches')">
      <header class="flex items-center gap-2 border-b border-stone-200 bg-white px-3 py-1.5">
        <History :size="20" class="shrink-0 text-stone-600" />
        <h2 class="min-w-0 flex-1 truncate text-lg font-semibold">{{ $t('Historial de Clutches') }}</h2>
        <button class="btn-ghost h-11 w-11 justify-center" :aria-label="$t('Cerrar')" @click="emit('close')"><X :size="22" /></button>
      </header>
      <div class="min-h-0 flex-1 overflow-y-auto">
        <TodayChanges :day="day" :initials="initials" />
      </div>
    </section>
  </div>
</template>
